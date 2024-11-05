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
#' @param multipart Logical scalar indicating whether the server at \code{url} supports multi-part range requests.
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
downloadDatabaseRanges <- function(name, start, end, url = databaseUrl(), multipart = FALSE) {
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
        reqs[[i]] <- req_headers(req, Range=paste0("bytes=", start[i], "-", end[i] - 1L)) # byte ranges are closed intervals, not half-open.
    }

    resps <- req_perform_parallel(reqs, progress=FALSE)
    for (i in seq_along(resps)) {
        payload <- resp_body_string(resps[[i]])
        output[keep[i]] <- substr(payload, 1L, end[i] - start[i])
    }

    output
}

#' @export
#' @import httr2
downloadMultipartRanges <- function(url, start, end) {
    output <- character(length(start))
    keep <- which(start < end)
    start <- start[keep]
    end <- end[keep]
    if (length(keep) == 0L) {
        return(output)
    }

    req <- request(url)
    o <- order(start)
    ranges <- paste(sprintf("%s-%s", start[o], end[o] - 1L), collapse=", ") # byte ranges are closed intervals, not half-open.
    req <- req_headers(req, Range=paste0("bytes=", ranges))
    resp <- req_perform(req)

    if (length(start) == 1L) {
        content <- resp_body_string(resp)
        output[keep] <- substr(content, 1L, end - start)
        return(output)
    }

    ct <- resp_header(resp, "Content-Type")
    prefix <- "multipart/byteranges; boundary="
    if (!startsWith(ct, prefix)) {
        stop("unexpected content type from multi-part range request")
    }
    boundary <- substring(ct, nchar(prefix) + 1L, nchar(ct))

    parsed <- parse_multipart_response(resp_body_string(resp), boundary)
    chosen <- findInterval(start, parsed$start)
    offset <- parsed$start
    output[keep] <- substr(parsed$content[chosen], start - offset + 1L, end - offset)
    output
}

# WARNING: this part only works when the contents of body are fully string-able;
# otherwise, if it's binary data, we really should be working with them as raw data.
parse_multipart_response <- function(body, boundary) {
    position <- 1L

    rx <- paste0("--", boundary)
    nrx <- nchar(rx)
    pos.boundaries <- gregexpr(rx, body)[[1]]

    starts <- integer(0) 
    contents <- character(0)

    for (i in seq_along(pos.boundaries)) {
        p <- pos.boundaries[i]
        if (i != 1L) {
            if (substr(body, p - 2L, p - 1L) != "\r\n") {
                stop("each non-first multipart boundary should be preceded by a CRLF")
            }
        }

        bound.end <- p + nrx
        next2 <- substr(body, bound.end, bound.end + 1)
        if (next2 == "--") {
            if (i != length(pos.boundaries)) {
                stop("unexpected location for the terminating multipart boundary")
            }
            break
        } else if (next2 != "\r\n") {
            stop("expected a CRLF at each multipart boundary")
        }

        subbody <- substr(body, bound.end + 2L, pos.boundaries[i + 1] - 3L)
        empty <- regexpr("\r\n\r\n", subbody)[[1]]
        if (empty == -1) {
            stop("could not find an empty line to separate headers in a multipart response")
        }
        contents <- c(contents, substr(subbody, empty + 4L, nchar(subbody)))

        part.headers <- substr(subbody, 1L, empty - 1L)
        components <- strsplit(part.headers, "\r\n", fixed=TRUE)[[1]]
        range.idx <- grep("^Content-Range: ", components)
        if (length(range.idx) != 1L) {
            stop("expected a single content range header at each multipart boundary")
        }

        range.info <- components[range.idx]
        curstart <- as.integer(gsub("^Content-Range: bytes ([0-9]+)-[0-9]+/[0-9]+$", "\\1", range.info))
        if (is.na(curstart)) {
            stop("invalid format for the content range header at each multipart boundary")
        }
        starts <- c(starts, curstart)
    }

    list(start=starts, content=contents)
}
