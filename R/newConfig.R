#' Create a new configuration object
#'
#' Create a new configuration object to specify how the Gesel database should be queried.
#' This can be used by applications to point to a different Gesel database from the default.
#'
#' @param fetch.gene Function that accepts the name of a Gesel gene annotation file and returns an absolute path to the file.
#' If \code{NULL}, it defaults to \code{\link{downloadGeneFile}}.
#' @param fetch.gene.args Named list of arguments to pass to \code{fetch.gene}.
#' @param gene.version String containing the version of the Gesel gene annotation file specification used by all gene annotation instances.
#' If \code{NULL}, this is automatically determined by \code{\link{fetchGeneVersion}}.
#' This can also be set explicitly to skip a query and save some time.
#' @param fetch.file Function that accepts the name of a Gesel database file and returns an absolute path to the file.
#' If \code{NULL}, it defaults to \code{\link{downloadDatabaseFile}}.
#' @param fetch.file.args Named list of arguments to pass to \code{fetch.file}.
#' @param fetch.ranges Function that downloads byte ranges, see \dQuote{Fetching byte ranges} below.
#' If \code{NULL}, it defaults to \code{\link{downloadDatabaseRanges}}.
#' @param fetch.ranges.args Named list of arguments to pass to \code{fetch.ranges}.
#' @param consolidate.block.size Integer specifying the block size for consolidated requests.
#' If \code{NULL}, it defaults to \code{\link{consolidateBlockSize}}.
#'
#' @return A list containing Gesel configuration settings.
#'
#' @details
#' The configuration list returned by \code{newConfig} can be passed to each \pkg{gesel} function to alter its behavior in a consistent manner.
#' For example, we can override \code{fetch.file} to retrieve database files from a shared filesystem instead of performing a HTTP request.
#'
#' The configuration list also contains a cache of data structures that can be populated by \pkg{gesel} functions.
#' This avoids unnecessary fetch requests upon repeated calls to the same function.
#' If the cache becomes stale or too large, it can be cleared by calling \code{\link{flushMemoryCache}}.
#'
#' If no configuration list is supplied to \pkg{gesel} functions, the default configuration is used.
#' The default is created by calling \code{newConfig} without any arguments.
#'
#' @section Fetching byte ranges
#' The \code{fetch.ranges} function should accept three arguments:
#' \itemize{
#' \item \code{fname}, the name of a Gesel database file.
#' \item \code{start}, an integer vector containing the zero-indexed closed start of each byte range to extract from the file.
#' This may be of zero length.
#' \item \code{end}, an integer vector containing the zero-indexed open end of each byte range to extract from the file.
#' This should have the same length as \code{start} such that the \code{i}-th byte range is defined as \code{[start[i], end[i])}.
#' The set of byte ranges should be non-empty, non-overlapping, and sorted by increasing start/end position.
#' }
#' It should return a list of raw vectors containing any number of \dQuote{retrieved} byte ranges.
#' The \code{retrieved.start} and \code{retrieved.end} attributes of this list are integer vectors of the same length as the list,
#' containing the zero-indexed closed start and open end of each retrieved byte range, respectively.
#' All retrieved byte ranges are non-empty but may be overlapping and in any order.
#'
#' Each \dQuote{requested} byte range (i.e., defined by \code{start} and \code{end}) will be enclosed within at least one retrieved byte range.
#' Note that the retrieved byte ranges may not have a 1-to-1 mapping to the requested byte ranges,
#' e.g., if the server decides that adjacent requested ranges can be merged into a single output range.
#'
#' @author Aaron Lun
#' @examples
#' config <- newConfig()
#'  
#' @export
newConfig <- function(
    fetch.gene = NULL,
    fetch.gene.args = list(),
    gene.version = NULL,
    fetch.file = NULL,
    fetch.file.args = list(),
    fetch.ranges = NULL,
    fetch.ranges.args = list(),
    consolidate.block.size = NULL
) {
    list(
        cache = new.env(),
        fetch.gene = fetch.gene,
        fetch.gene.args = fetch.gene.args,
        gene.version = gene.version,
        fetch.file = fetch.file,
        fetch.file.args = fetch.file.args,
        fetch.ranges = fetch.ranges,
        fetch.ranges.args = fetch.ranges.args,
        consolidate.block.size = consolidate.block.size
    )
}

mem.cache <- new.env()
mem.cache$default <- NULL

get_config <- function(config) {
    if (is.null(config)) {
        config <- mem.cache$default 
        if (is.null(config)) {
            config <- newConfig()
            mem.cache$default <- config
        }
    }
    config
}

fetch_gene <- function(config, ...) {
    fetch.gene <- config$fetch.gene
    if (is.null(fetch.gene)) {
        fetch.gene <- downloadGeneFile
    }
    do.call(fetch.gene, c(list(...), config$fetch.gene.args))
}

fetch_file <- function(config, ...) {
    fetch.file <- config$fetch.file
    if (is.null(fetch.file)) {
        fetch.file <- downloadDatabaseFile 
    }
    do.call(fetch.file, c(list(...), config$fetch.file.args))
}

fetch_ranges <- function(config, ...) {
    fetch.ranges <- config$fetch.ranges
    if (is.null(fetch.ranges)) {
        fetch.ranges <- downloadDatabaseRanges
    }
    do.call(fetch.ranges, c(list(...), config$fetch.ranges.args))
}

consolidate_block_size <- function(config) {
    consolidate.block.size <- config$consolidate.block.size
    if (is.null(consolidate.block.size)) {
        consolidate.block.size <- consolidateBlockSize()
    }
    consolidate.block.size
}

#' Flush the in-memory cache
#'
#' Flush the in-memory cache for \pkg{gesel} data structures in the current R session.
#'
#' @param config A configuration list.
#' If \code{NULL}, the default configuration is used.
#'
#' @return The in-memory cache in \code{config} is cleared.
#' \code{NULL} is invisibly returned.
#'
#' @details
#' By default, the \pkg{gesel} package caches the data structures in the current R session to avoid unnecessary requests to the filesystem and remote server.
#' On rare occasion, these cached data structures may be out of date when the Gesel database files change.
#' In such cases, the cache can be flushed to ensure that the various \pkg{gesel} functions operate on the latest version of the database.
#'
#' @author Aaron Lun
#' @examples
#' flushMemoryCache()
#'
#' @export
flushMemoryCache <- function(config = NULL) {
    config <- get_config(config)
    objs <- ls(config$cache)
    remove(list=objs, envir=config$cache, inherits=FALSE)
    invisible(NULL)
}

get_cache <- function(config, context, species) {
    cache <- config$cache
    if (exists(context, envir=cache, inherits=FALSE)) {
        cached <- get(context, envir=cache, inherits=FALSE)
        cached[[species]]
    } else {
        NULL
    }
}

set_cache <- function(config, context, species, value) {
    cached <- get_cache(config, context, species)
    if (is.null(cached)) {
        cached <- list()
    }
    cached[[species]] <- value
    assign(context, cached, envir=config$cache, inherits=FALSE)
}
