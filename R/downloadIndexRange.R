#' Download a byte range from a Gesel index file
#'
#' Download a byte range from a file in the Gesel index.
#'
#' @inheritParams downloadIndexFile
#' @param range Integer vector of length 2 containing the zero-indexed half-open byte range to extract from the file,
#' i.e., \code{c(a, b)} should extract bytes from \eqn{[a, b)}.
#'
#' @return String containing the contents of the specified byte range.
#'
#' @author Aaron Lun
#' @examples
#' downloadIndexRange("9606_set2gene.tsv", c(0L, 100L))
#'
#' @export
#' @import httr2
downloadIndexRange <- function(name, range, url = indexUrl()) {
    req <- request(paste0(url, "/", name))
    req <- req_headers(req, Range=paste0("bytes=", range[1], "-", range[2] - 1L)) # byte ranges are closed intervals, not half-open.
    res <- req_perform(req)
    payload <- resp_body_string(res)
    substr(payload, 1L, diff(range) + 1L)
}
