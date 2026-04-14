#' Download Gesel database files
#'
#' Default function to download Gesel database files. 
#'
#' @param name String containing the name of the file.
#' This usually has the species identifier as a prefix.
#' @param url String containing the base URL to the Gesel database files.
#' @param cache String specifying the path to a cache directory.
#' @param overwrite Boolean indicating whether any cached file should be overwritten.
#'
#' @return \code{downloadDatabaseFile} returns a string containing a path to the downloaded file.
#'
#' For \code{databaseUrl}, if \code{url=NULL}, the function returns a string containing the URL to the Gesel database.
#' If \code{url} is provided, it instead stores \code{url} as the URL to the database, and the previous value of \code{url} is invisibly returned.
#' 
#' @details
#' The database URL defaults to the GitHub releases at \url{https://github.com/LTLA/gesel-feedstock}.
#' This can be altered by setting the \code{GESEL_DATABASE_URL} environment variable prior to the first call to this function.
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
downloadDatabaseFile <- function(name, url = databaseUrl(), cache = cacheDirectory(), overwrite = FALSE) {
    download_file(cache, paste0(url, "/", name), overwrite)
}

#' @export
#' @rdname downloadDatabaseFile
databaseUrl <- function(url = NULL) {
    previous <- downloadDatabaseUrl.env$url
    if (is.null(previous)) {
        previous <- Sys.getenv("GESEL_DATABASE_URL", "https://github.com/LTLA/gesel-feedstock/releases/download/indices-v0.2.1")
        downloadDatabaseUrl.env$url <- previous
    }
    if (is.null(url)) {
        previous 
    } else {
        downloadDatabaseUrl.env$url <- url
        invisible(previous)
    }
}

downloadDatabaseUrl.env <- new.env()
downloadDatabaseUrl.env$url <- NULL
