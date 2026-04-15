concurrency.env <- new.env()
concurrency.env$workers <- 100L

#' Concurrency of range requests
#'
#' Get or set the maximum number of concurrent HTTP range requests that can be performed per minute in \code{\link{downloadDatabaseRanges}}.
#' Setting this to a smaller number avoids excessive load on the server.
#'
#' @param concurrency Integer containing the maximum number of concurrent requests per minute. 
#'
#' @return If \code{concurrency=NULL}, the maximum number of concurrent requests is returned.
#'
#' If \code{concurrency} is provided, it is set to the maximum number of concurrent requests, and the previous maximum is returned.
#'
#' @author Aaron Lun
#' @examples
#' rangeConcurrency()
#' old <- rangeConcurrency(5)
#' rangeConcurrency()
#' rangeConcurrency(old)
#' rangeConcurrency()
#'
#' @seealso
#' \code{\link[httr2]{req_throttle}}, for the logic behind the requests-per-minute limit.
#'
#' @export
rangeConcurrency <- function(concurrency = NULL) {
    previous <- concurrency.env$workers
    if (is.null(concurrency)) {
        previous
    } else {
        concurrency.env$workers <- concurrency
        invisible(previous)
    }
}

#' Fetch byte ranges from a Gesel database file
#'
#' Download any number of byte ranges from a Gesel database file.
#'
#' @inheritParams downloadDatabaseFile
#' @param start Integer vector containing the zero-indexed closed start of each byte range to extract from the file.
#' This may be of zero length.
#' @param end Integer vector containing the zero-indexed open end of each byte range to extract from the file.
#' This should have the same length as \code{start} such that the \code{i}-th range is defined as \code{[start[i], end[i])}.
#' All ranges supplied in a single call to this function should be non-overlapping.
#' @param multipart Boolean indicating whether the server at \code{url} supports multi-part range requests.
#' @param concurrency Integer specifying the maximum number of concurrent range requests per minute.
#' Ignored if \code{multipart=TRUE}.
#'
#' @return Character vector of length equal to \code{length(start)}, containing the contents of the requested byte ranges.
#'
#' @author Aaron Lun
#' @examples
#' downloadDatabaseRanges("9606_set2gene.tsv", 0L, 100L)
#' downloadDatabaseRanges("9606_set2gene.tsv", c(10, 100, 1000), c(20, 150, 1100))
#'
#' @export
#' @import httr2
#' @aliases downloadMultipartRanges
downloadDatabaseRanges <- function(name, start, end, url = databaseUrl(), multipart = FALSE, concurrency = rangeConcurrency()) {
    url <- paste0(url, "/", name)
    if (multipart) {
        return(downloadMultipartRanges(url, start, end))
    }

    output <- character(length(start))
    keep <- which(start < end)
    start <- start[keep]
    end <- end[keep]
    if (length(keep) == 0L) {
        return(output)
    }

    reqs <- vector("list", length(start))
    for (i in seq_along(start)) {
        req <- request(url)
        req <- req_headers(req, Range=paste0("bytes=", start[i], "-", end[i] - 1L)) # byte ranges are closed intervals, not half-open.
        req <- req_throttle(req, capacity = concurrency)
        reqs[[i]] <- req
    }

    resps <- req_perform_parallel(reqs, progress=FALSE)
    for (i in seq_along(resps)) {
        # Process raw bytes to avoid issues with multi-byte characters.
        payload <- resp_body_raw(resps[[i]])
        output[keep[i]] <- rawToChar(head(payload, end[i] - start[i]))
    }

    output
}

#' @export
#' @import httr2
downloadMultipartRanges <- function(url, start, end) {
    output <- character(length(start))
    keep <- which(start < end)
    if (length(keep) == 0L) {
        return(output)
    }

    start <- start[keep]
    end <- end[keep]
    if (is.unsorted(start)) {
        o <- order(start)
        start <- start[o]
        end <- end[o]
        keep <- keep[o]
    }

    req <- request(url)
    ranges <- paste(sprintf("%s-%s", start, end - 1L), collapse=", ") # byte ranges are closed intervals, not half-open.
    req <- req_headers(req, Range=paste0("bytes=", ranges))
    resp <- req_perform(req)

    if (length(start) == 1L) {
        # Process raw bytes to avoid issues with multi-byte characters.
        content <- resp_body_raw(resp)
        output[keep] <- rawToChar(head(content, end - start))
        return(output)
    }

    ct <- resp_header(resp, "Content-Type")
    prefix <- "multipart/byteranges; boundary="
    if (!startsWith(ct, prefix)) {
        stop("unexpected content type from multi-part range request")
    }
    boundary <- substring(ct, nchar(prefix) + 1L, nchar(ct))

    output[keep] <- extract_multipart_strings(resp_body_raw(resp), boundary)
    output
}

extract_multipart_strings <- function(body, boundary, start, end) {
    parsed <- parse_multipart_ranges(body, boundary)

    part.starts <- part.ends <- integer(length(parsed))
    for (i in seq_along(parsed)) {
        current <- parsed[[i]]
        attrs <- attributes(current)
        attr.names <- tolower(names(attrs))
        if (!("content-range" %in% attr.names)) {
            stop("expected a 'Content-Range' header for each part of a multipart response")
        }

        content.range <- attrs[[match("content-range", attr.names)]] 
        if (!startsWith(content.range, "bytes ")) {
            stop("expected the 'Content-Range' header to start with 'bytes '")
        }

        content.range <- substr(content.range, 6, nchar(content.range))
        current.start <- as.integer(sub("-.*", "", content.range))
        if (is.na(current.start)) {
            stop("failed to extract start of the content range");
        }
        part.starts[i] <- current.start

        current.end <- as.integer(sub("/.*", "", sub(".*-", "", content.range)))
        if (is.na(current.end)) {
            stop("failed to extract end of the content range");
        }
        part.ends[i] <- current.end
    }

    chosen <- findInterval(start, part.starts)
    if (any(chosen == 0) || any(end > part.ends[chosen] + 1)) { # remember, content-range has a closed end, while our 'end' is open.
        stop("multipart response does not contain the requested byte ranges")
    }

    offset <- part.starts[chosen]
    s <- start - offset + 1L
    e <- end - offset

    collected <- character(length(start))
    for (i in seq_along(collected)) {
        collected[i] <- rawToChar(parsed[[chosen[i]]][s[i]:e[i]])
    }

    collected
}
