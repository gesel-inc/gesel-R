#' Fetch some collections
#' 
#' Fetch the details of some gene set collections from the Gesel database.
#' This can be more efficient than \code{\link{fetchAllCollections}} when only a few collections are of interest.
#'
#' @inheritParams fetchGenesForSomeSets
#' @param collections Integer vector containing collection indices.
#' Each entry refers to a row of the data frame returned by \code{\link{fetchAllCollections}}).
#' @param use.preloaded Logical scalar indicating whether to use the preloaded value from a previous call to \code{\link{fetchAllCollections}}.
#'
#' @return Data frame with the same columns as the return value of \code{\link{fetchAllCollections}},
#' where each row corresponds to an entry of \code{collections}.
#'
#' @author Aaron Lun
#' @examples
#' fetchSomeCollections("9606", 1)
#'
#' @export
#' @importFrom utils head
fetchSomeCollections <- function(species, collections, fetch.file = NULL, fetch.file.args = list(), fetch.range = NULL, fetch.range.args = list(), use.preloaded = TRUE) {
    if (use.preloaded) {
        candidate <- fetchAllCollections.env$result[[species]]
        if (!is.null(candidate)) {
            return(candidate[collections,])
        }
    }

    fname <- paste0(species, "_collections.tsv")
    range.info <- get_single_collection_ranges(species, fname, fetch.file=fetch.file, fetch.file.args=fetch.file.args)
    intervals <- range.info$ranges

    if (is.null(fetch.range)) {
        fetch.range <- downloadIndexRange
    }
    starts <- intervals[collections]
    ends <- intervals[collections + 1L] - 1L # remove the newline.
    deets <- do.call(fetch.range, c(list(name=fname, start=starts, end=ends), fetch.range.args))
    split <- strsplit(deets, "\t")

    data.frame(
        title = vapply(split, function(x) x[1], ""),
        description = vapply(split, function(x) x[2], ""),
        maintainer = vapply(split, function(x) x[4], ""),
        `source` = vapply(split, function(x) x[5], ""),
        start = range.info$starts[collections],
        size = range.info$sizes[collections]
    )
}
    
fetchSomeCollections.env <- new.env()
fetchSomeCollections.env$result <- list()

get_single_collection_ranges <- function(species, fname, fetch.file, fetch.file.args) {
    range.info <- fetchSomeCollections.env$result[[species]]
    if (is.null(range.info)) {
        range.info <- retrieve_ranges_with_sizes(fname, fetch=fetch.file, fetch.args=fetch.file.args)
        range.info$starts <- c(0L, cumsum(head(range.info$sizes, -1L))) + 1L
        fetchSomeCollections.env$result[[species]] <- range.info
    }
    range.info
}

#' Size of collections
#'
#' Quickly get the sizes of all gene set collections in the Gesel database.
#' This is more efficient than \code{\link{fetchAllCollections}} when only the sizes are of interest.
#'
#' @inheritParams fetchSomeCollections
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
