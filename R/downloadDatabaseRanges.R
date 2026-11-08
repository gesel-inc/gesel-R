concurrency.env <- new.env()
concurrency.env$workers <- 10L

#' Concurrency of range requests
#'
#' Get or set the maximum number of concurrent HTTP range requests that can be performed per second in \code{\link{downloadDatabaseRanges}}.
#' Setting this to a smaller number avoids excessive load on the server.
#'
#' @param concurrency Integer containing the maximum number of concurrent requests per second. 
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
#' \code{\link[httr2]{req_throttle}}, for the logic behind the requests-per-second limit.
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
#' Download any number of byte ranges from a Gesel database file using (possibly multi-part) HTTP range requests. 
#'
#' @inheritParams downloadDatabaseFile
#' @param start Integer vector of range start positions, see \dQuote{Fetching byte ranges} in \code{?\link{newConfig}} for details.
#' @param end Integer vector of range end positions, see \dQuote{Fetching byte ranges} in \code{?\link{newConfig}} for details.
#' @param multipart Boolean indicating whether the server at \code{url} supports multi-part range requests.
#' @param concurrency Integer specifying the maximum number of concurrent range requests per second.
#' Ignored if \code{multipart=TRUE}.
#'
#' @return List of raw vectors, see \dQuote{Fetching byte ranges} in \code{?\link{newConfig}} for details.
#'
#' @author Aaron Lun
#' @examples
#' downloadDatabaseRanges("9606_set2gene.tsv", 0L, 100L)
#' downloadDatabaseRanges("9606_set2gene.tsv", c(10, 100, 1000), c(20, 150, 1100))
#'
#' @seealso
#' \code{\link{readDatabaseRanges}}, for a local counterpart to this function that reads byte ranges from the filesystem.
#'
#' @export
#' @import httr2
#' @aliases downloadMultipartRanges
downloadDatabaseRanges <- function(name, start, end, url = databaseUrl(), multipart = FALSE, concurrency = rangeConcurrency()) {
    url <- paste0(url, "/", name)
    if (multipart) {
        return(downloadMultipartRanges(url, start, end))
    }

    stopifnot(all(start < end))
    reqs <- vector("list", length(start))
    for (i in seq_along(start)) {
        req <- request(url)
        req <- req_headers(req, Range=paste0("bytes=", start[i], "-", end[i] - 1L)) # byte ranges are closed intervals, not half-open.
        req <- req_throttle(req, capacity = concurrency, fill_time_s = 1)
        reqs[[i]] <- req
    }

    output <- vector("list", length(start))
    start <- integer(length(start))
    end <- integer(length(end))

    resps <- req_perform_parallel(reqs, progress=FALSE)
    for (i in seq_along(resps)) {
        curresp <- resps[[i]]

        # Avoid downloading the entire file if the server doesn't understand range requests and returns a 200.
        if (resp_status(curresp) != 206) {
            stop("expected a partial content response from a range request");
        }

        range <- resp_header(curresp, "content-range")
        range.loc <- extract_range_location(range)

        start[i] <- range.loc[["start"]]
        end[i] <- range.loc[["end"]]
        output[[i]] <- resp_body_raw(curresp) # Process raw bytes to avoid issues with multi-byte characters.
    }

    attr(output, "retrieved.start") <- start
    attr(output, "retrieved.end") <- end
    output
}

#' @export
#' @import httr2
downloadMultipartRanges <- function(url, start, end) {
    if (length(start) == 0L) {
        empty <- list()
        attr(empty, "retrieved.start") <- integer(0)
        attr(empty, "retrieved.end") <- integer(0)
        return(empty)
    }

    req <- request(url)
    ranges <- paste(sprintf("%s-%s", start, end - 1L), collapse=", ") # byte ranges are closed intervals, not half-open.
    req <- req_headers(req, Range=paste0("bytes=", ranges))
    resp <- req_perform(req)

    # Avoid downloading the entire file if the server doesn't understand range requests and returns a 200.
    if (resp_status(resp) != 206) {
        stop("expected a partial content response from a range request");
    }

    if (length(start) == 1L) {
        # Process raw bytes to avoid issues with multi-byte characters.
        range <- resp_header(resp, "content-range")
        range.loc <- extract_range_location(range)
        single <- list(resp_body_raw(resp))
        attr(single, "retrieved.start") <- unname(range.loc[1])
        attr(single, "retrieved.end") <- unname(range.loc[2])
        return(single)
    }

    ct <- resp_header(resp, "Content-Type")
    prefix <- "multipart/byteranges; boundary="
    if (!startsWith(ct, prefix)) {
        stop("unexpected content type from multi-part range request")
    }
    boundary <- substring(ct, nchar(prefix) + 1L, nchar(ct))
    extract_multipart_strings(resp_body_raw(resp), boundary)
}

extract_range_location <- function(content.range) {
    if (!startsWith(content.range, "bytes ")) {
        stop("expected the 'Content-Range' header to start with 'bytes '")
    }

    content.range <- substr(content.range, 6, nchar(content.range))
    current.start <- as.integer(sub("-.*", "", content.range))
    if (is.na(current.start)) {
        stop("failed to extract start of the content range");
    }

    current.end <- as.integer(sub("/.*", "", sub(".*-", "", content.range)))
    if (is.na(current.end)) {
        stop("failed to extract end of the content range");
    }

    c(
        start = current.start,
        end = current.end + 1L # convert to an open end for consistency. 
    )
}

extract_multipart_strings <- function(body, boundary) {
    parsed <- parse_multipart_ranges(body, boundary)

    part.start <- part.end <- integer(length(parsed))
    for (i in seq_along(parsed)) {
        current <- parsed[[i]]
        attrs <- attributes(current)
        attr.names <- tolower(names(attrs))
        if (!("content-range" %in% attr.names)) {
            stop("expected a 'Content-Range' header for each part of a multipart response")
        }
        content.range <- attrs[[match("content-range", attr.names)]] 
        current.range <- extract_range_location(content.range)
        part.start[i] <- current.range[["start"]]
        part.end[i] <- current.range[["end"]]
        attributes(current) <- NULL
        parsed[[i]] <- current
    }

    attributes(parsed) <- list(
        retrieved.start = part.start,
        retrieved.end = part.end
    )
    parsed
}
