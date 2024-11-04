#' Flush the in-memory cache
#'
#' Flush the in-memory cache for \pkg{gesel} data structures in the current R session.
#'
#' @return The in-memory cache is cleared.
#' \code{NULL} is invisibly returned.
#'
#' @details
#' The \pkg{gesel} package makes caches the data structures in the current R session to avoid unnecessary requests to the filesystem and remote server.
#' On rare occasion, these cached data structures may be out of date when the Gesel database files change.
#' In such cases, the cache can be flushed to ensure that the various \pkg{gesel} functions operate on the latest version of the database.
#'
#' @author Aaron Lun
#' @examples
#' flushMemoryCache()
#'
#' @export
flushMemoryCache <- function() {
    objs <- ls(mem.cache)
    remove(list=objs, envir=mem.cache, inherits=FALSE)
}

mem.cache <- new.env()

get_cache <- function(context, species) {
    if (exists(context, envir=mem.cache, inherits=FALSE)) {
        cached <- get(context, envir=mem.cache, inherits=FALSE)
        cached[[species]]
    } else {
        NULL
    }
}

set_cache <- function(context, species, value) {
    cached <- get_cache(context, species)
    if (is.null(cached)) {
        cached <- list()
    }
    cached[[species]] <- value
    assign(context, cached, envir=mem.cache, inherits=FALSE)
}
