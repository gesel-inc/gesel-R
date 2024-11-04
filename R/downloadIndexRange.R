#' Download a byte range from a Gesel index file
#'
#' Download a byte range from a file in the Gesel index.
#'
#' @inheritParams downloadIndexFile
#' @param start Integer vector containing the zero-indexed closed start of each byte range to extract from the file.
#' @param end Integer vector containing the zero-indexed open end of each byte range to extract from the file.
#' This should have the same length as \code{start} such that the \code{i}-th range is defined as \code{[start[i], end[i])}.
#' @param num.workers Integer scalar specifying the number of workers to use for concurrent requests with multiple ranges.
#'
#' @return Character vector of length equal to \code{length(start)}, containing the contents of the byte ranges.
#'
#' @author Aaron Lun
#' @examples
#' downloadIndexRange("9606_set2gene.tsv", 0L, 100L)
#' downloadIndexRange("9606_set2gene.tsv", c(10, 100, 1000), c(20, 150, 1100))
#'
#' @export
#' @importFrom parallel stopCluster makeCluster parLapplyLB
downloadIndexRange <- function(name, start, end, url = indexUrl(), num.workers = 4L) {
    url <- paste0(url, "/", name)
    intervals <- mapply(c, start, end, SIMPLIFY=FALSE)
    if (num.workers == 1L) {
        return(vapply(X=intervals, FUN=range_request, url=url, FUN.VALUE=""))
    }

    if (num.workers != downloadIndexRange.env$num.workers) {
        if (!is.null(downloadIndexRange.env$cluster)) {
            stopCluster(downloadIndexRange.env$cluster)
        } 
        downloadIndexRange.env$cluster <- makeCluster(num.workers)
        downloadIndexRange.env$num.workers <- num.workers
    }

    out <- parLapplyLB(downloadIndexRange.env$cluster, X=intervals, fun=range_request, url=url)
    as.character(unlist(out))
}

#' @import httr2
range_request <- function(interval, url) {
    req <- request(url)
    req <- req_headers(req, Range=paste0("bytes=", interval[1], "-", interval[2] - 1L)) # byte ranges are closed intervals, not half-open.
    res <- req_perform(req)
    payload <- resp_body_string(res)
    substr(payload, 1L, diff(interval))
}

downloadIndexRange.env <- new.env()
downloadIndexRange.env$cluster <- NULL
downloadIndexRange.env$num.workers <- 0L
