#' Fetch byte ranges from a Gesel database file
#'
#' Download any number of byte ranges from a Gesel database file.
#'
#' @inheritParams downloadDatabaseFile
#' @param start Integer vector containing the zero-indexed closed start of each byte range to extract from the file.
#' @param end Integer vector containing the zero-indexed open end of each byte range to extract from the file.
#' This should have the same length as \code{start} such that the \code{i}-th range is defined as \code{[start[i], end[i])}.
#' @param num.workers Integer scalar specifying the number of workers to use for concurrent requests with multiple ranges.
#'
#' @return Character vector of length equal to \code{length(start)}, containing the contents of the requested byte ranges.
#'
#' @author Aaron Lun
#' @examples
#' downloadDatabaseRanges("9606_set2gene.tsv", 0L, 100L)
#' downloadDatabaseRanges("9606_set2gene.tsv", c(10, 100, 1000), c(20, 150, 1100))
#'
#' @export
#' @importFrom parallel stopCluster makeCluster parLapplyLB
downloadDatabaseRanges <- function(name, start, end, url = databaseUrl(), num.workers = 4L) {
    url <- paste0(url, "/", name)
    intervals <- mapply(c, start, end, SIMPLIFY=FALSE)

    if (num.workers == 1L) {
        return(vapply(X=intervals, FUN=range_request, url=url, FUN.VALUE=""))
    }

    if (num.workers != downloadDatabaseRanges.env$num.workers) {
        if (!is.null(downloadDatabaseRanges.env$cluster)) {
            stopCluster(downloadDatabaseRanges.env$cluster)
        } 

        limit.cores <- Sys.getenv("_R_CHECK_LIMIT_CORES_", NA)
        actual.workers <- num.workers
        if (!is.na(limit.cores) && !identical(tolower(limit.cores), "false")) {
            actual.workers <- 2L
        }

        downloadDatabaseRanges.env$cluster <- makeCluster(actual.workers)
        downloadDatabaseRanges.env$num.workers <- num.workers

        left.open <- Sys.getenv("_R_CHECK_CONNECTIONS_LEFT_OPEN_", NA)
        if (!is.na(left.open) && !identical(tolower(left.open), "false")) {
            on.exit({
                stopCluster(downloadDatabaseRanges.env$cluster)
                downloadDatabaseRanges.env$cluster <- NULL
                downloadDatabaseRanges.env$num.workers <- 0L
            }, add=TRUE, after=FALSE)
        }
    }

    out <- parLapplyLB(downloadDatabaseRanges.env$cluster, X=intervals, fun=range_request, url=url)
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

downloadDatabaseRanges.env <- new.env()
downloadDatabaseRanges.env$cluster <- NULL
downloadDatabaseRanges.env$num.workers <- 0L
