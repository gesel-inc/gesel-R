#' Default fetch for Gesel index files
#'
#' Default fetch function for files in the Gesel index.
#'
#' @param name String containing the name of the file.
#' @param url String containing the base URL to the Gesel indices.
#' For \code{indexUrl}, this can be \code{NULL} to set it back to the default.
#' @param cache String specifying the path to a cache directory.
#' If \code{NULL}, a cache location is automatically chosen.
#' @param overwrite Logical scalar indicating whether any cached file should be overwritten.
#'
#' @return \code{downloadIndexFile} returns a string containing a path to the downloaded file.
#'
#' If \code{url} is missing, \code{indexUrl} returns a string containing the URL to the Gesel indices.
#' If \code{url} is provided, it instead stores \code{url} as the URL to the indices, and the previous value of \code{url} is invisibly returned.
#' 
#' @details
#' The default index URL is set to the GitHub releases at \url{https://github.com/LTLA/gesel-feedstock}.
#' This can be altered by setting the \code{GESEL_INDEX_URL} environment variable.
#'
#' On first use of a cached file in an R session, \code{downloadIndexFile} will automatically check for updates at \code{url}. 
#' If the file on the remote has been updated, the new copy will be downloaded to the cache.
#' 
#' @author Aaron Lun
#'
#' @examples
#' # Download file.
#' downloadIndexFile("9606_collections.tsv.gz")
#' 
#' # Altering the default index URL.
#' indexUrl()
#' old <- indexUrl("https://foo")
#' indexUrl()
#' indexUrl(old)
#' 
#' @export
downloadIndexFile <- function(name, url = indexUrl(), cache = NULL, overwrite = FALSE) {
    download_file(cache, paste0(url, "/", name), overwrite)
}

#' @export
#' @rdname downloadIndexFile
indexUrl <- function(url) {
    if (missing(url)) {
        url <- downloadIndexUrl.env$url
        if (is.null(url)) {
            url <- Sys.getenv("GESEL_INDEX_URL", "https://github.com/LTLA/gesel-feedstock/releases/download/indices-v0.2.1")
        }
        url
    } else {
        previous <- downloadIndexUrl.env$url
        downloadIndexUrl.env$url <- url
        invisible(NULL)
    }
}

downloadIndexUrl.env <- new.env()
downloadIndexUrl.env$url <- NULL
