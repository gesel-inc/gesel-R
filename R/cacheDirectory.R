cacheDirectory.env <- new.env()
cacheDirectory.env$path <- NULL

#' Gesel cache directory
#'
#' Get or set the location of the Gesel cache directory,
#' used as the default in functions like \code{\link{downloadGeneFile}} and \code{\link{downloadDatabaseFile}}.
#'
#' @param cache String specifying the path to a cache directory.
#'
#' @return If \code{cache=NULL}, the path to the current cache directory is returned.
#'
#' If \code{cache} is provided, it is set as the path to the cache directory, and the previous location is invisibly returned.
#'
#' @details
#' The cache directory defaults to a location in the user cache directory, as chosen by the \pkg{rappdirs} package.
#' Users can modify this choice by setting the \code{GESEL_CACHE_DIRECTORY} environment variable before the first call to this function.
#'
#' @author Aaron Lun
#' @examples
#' cacheDirectory()
#' 
#' old <- cacheDirectory("/tmp/foo/bar")
#' cacheDirectory()
#'
#' cacheDirectory(old)
#' cacheDirectory()
#'
#' @export
#' @importFrom rappdirs user_cache_dir
cacheDirectory <- function(cache = NULL) {
    previous <- cacheDirectory.env$path
    if (is.null(previous)) {
        previous <- Sys.getenv("GESEL_CACHE_DIRECTORY", user_cache_dir("gesel"))
        cacheDirectory.env$path <- previous
    }
    if (is.null(cache)) {
        previous
    } else {
        cacheDirectory.env$path <- cache
        invisible(previous)
    }
}
