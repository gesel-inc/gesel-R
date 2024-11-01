#' Fetch a single collection
#' 
#' Fetch the details of a single collection from the Gesel index.
#' This is more efficient than \code{\link{fetchAllCollections}} when only a single collection is of interest.
#'
#' @inheritParams fetchGenesForSet
#' @param collection Integer identifying the collection of interest, as an index into the list of all collections for this species 
#' (i.e., the return value of \code{\link{fetchAllCollections}}).
#' @param use.preloaded Logical scalar indicating whether to use the preloaded value from a previous call to \code{\link{fetchAllCollections}}.
#'
#' @return Named list containing:
#' \itemize{
#' \item \code{title}, string containing the title for the collection.
#' \item \code{description}, string containing a description for the collection.
#' \item \code{maintainer}, string containing the identity of the collection's maintainer.
#' \item \code{source}, string containing the source of origin of the collection.
#' \item \code{start}, integer scalar specifying the identity of the first gene set in this collection,
#' as an index into the return value of \code{\link{fetchAllSets}}.
#' \item \code{size}, integer scalar specifying the number of gene sets in the collection.
#' }
#'
#' @author Aaron Lun
#' @examples
#' fetchSingleCollection("9606", 1)
#'
#' @export
#' @importFrom utils head
fetchSingleCollection <- function(species, collection, fetch.file = NULL, fetch.file.args = list(), fetch.range = NULL, fetch.range.args = list(), use.preloaded = TRUE) {
    if (use.preloaded) {
        candidate <- fetchAllCollections.env$result[[species]]
        if (!is.null(candidate)) {
            return(candidate[[collection]])
        }
    }

    fname <- paste0(species, "_collections.tsv")
    range.info <- get_single_collection_ranges(species, fname, fetch.file=fetch.file, fetch.file.args=fetch.file.args)

    if (is.null(fetch.range)) {
        fetch.range <- downloadIndexRange
    }
    interval <- range.info$ranges[collection + 0:1] - 0:1 # skip the newline.
    deets <- do.call(fetch.range, c(list(fname, interval), fetch.range.args))
    split <- strsplit(deets, "\t")[[1]]

    list(
        title = split[1],
        description = split[2],
        maintainer = split[4],
        `source` = split[5],
        start = range.info$starts[collection],
        size = range.info$sizes[collection]
    )
}
    
fetchSingleCollection.env <- new.env()
fetchSingleCollection.env$result <- list()

get_single_collection_ranges <- function(species, fname, fetch.file, fetch.file.args) {
    range.info <- fetchSingleCollection.env$result[[species]]
    if (is.null(range.info)) {
        range.info <- retrieve_ranges_with_sizes(fname, fetch=fetch.file, fetch.args=fetch.file.args)
        range.info$starts <- c(0L, cumsum(head(range.info$sizes, -1L))) + 1L
        fetchSingleCollection.env$result[[species]] <- range.info
    }
    range.info
}

#' Size of collections
#'
#' Quickly get the size of the collections in the Gesel index.
#' This is more efficient than \code{\link{fetchAllCollections}} when only the sizes are of interest.
#'
#' @inheritParams fetchSingleCollection
#'
#' @return Integer vector containing the size of each collection (i.e., the number of gene sets).
#'
#' @author Aaron Lun
#' @examples
#' head(fetchCollectionSizes("9606"))
#'
#' @export
fetchCollectionSizes <- function(species, fetch.file = NULL, fetch.file.args = list(), use.preloaded = TRUE) {
    if (use.preloaded) {
        candidate <- fetchAllCollections.env$result[[species]]
        if (!is.null(candidate)) {
            return(nrow(candidate))
        }
    }

    fname <- paste0(species, "_collections.tsv")
    range.info <- get_single_collection_ranges(species, fname, fetch.file=fetch.file, fetch.file.args=fetch.file.args)
    range.info$sizes
}
