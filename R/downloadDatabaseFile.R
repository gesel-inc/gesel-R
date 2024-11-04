#' Download Gesel database files
#'
#' Default function to download Gesel database files. 
#'
#' @param name String containing the name of the file.
#' This usually has the species identifier as a prefix.
#' @param url String containing the base URL to the Gesel database files.
#' For \code{databaseUrl}, this can be \code{NULL} to set it back to the default.
#' @param cache String specifying the path to a cache directory.
#' If \code{NULL}, a cache location is automatically chosen.
#' @param overwrite Logical scalar indicating whether any cached file should be overwritten.
#'
#' @return \code{downloadDatabaseFile} returns a string containing a path to the downloaded file.
#'
#' For \code{databaseUrl}, if \code{url} is missing, the function returns a string containing the URL to the Gesel database.
#' If \code{url} is provided, it instead stores \code{url} as the URL to the database, and the previous value of \code{url} is invisibly returned.
#' 
#' @details
#' The default database URL is set to the GitHub releases at \url{https://github.com/LTLA/gesel-feedstock}.
#' This can be altered by setting the \code{GESEL_DATABASE_URL} environment variable.
#'
#' On first use of a cached file in an R session, \code{downloadDatabaseFile} will automatically check for updates at \code{url}. 
#' If the file on the remote has been updated, the new copy will be downloaded to the cache.
#' 
#' @author Aaron Lun
#'
#' @examples
#' # Download file.
#' downloadDatabaseFile("9606_collections.tsv.gz")
#' 
#' # Altering the default database URL.
#' databaseUrl()
#' old <- databaseUrl("https://foo")
#' databaseUrl()
#' databaseUrl(old)
#' 
#' @export
downloadDatabaseFile <- function(name, url = databaseUrl(), cache = NULL, overwrite = FALSE) {
    download_file(cache, paste0(url, "/", name), overwrite)
}

#' @export
#' @rdname downloadDatabaseFile
databaseUrl <- function(url) {
    if (missing(url)) {
        url <- downloadDatabaseUrl.env$url
        if (is.null(url)) {
            url <- Sys.getenv("GESEL_DATABASE_URL", "https://github.com/LTLA/gesel-feedstock/releases/download/indices-v0.2.1")
        }
        url
    } else {
        previous <- downloadDatabaseUrl.env$url
        downloadDatabaseUrl.env$url <- url
        invisible(NULL)
    }
}

downloadDatabaseUrl.env <- new.env()
downloadDatabaseUrl.env$url <- NULL
