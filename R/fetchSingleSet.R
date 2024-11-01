#' Fetch a single gene set 
#' 
#' Fetch the details of a single gene set from the Gesel index.
#' This is more efficient than calling \code{\link{fetchAllSets}} when only one set is of interest.
#'
#' @inheritParams fetchGenesForSet
#' @param set Integer identifying the set of interest, as an index into the list of all sets for this species 
#' (i.e., the return value of \code{\link{fetchAllSets}}).
#' @param use.preloaded Logical scalar indicating whether to use the preloaded value from a previous call to \code{\link{fetchAllSets}}.
#'
#' @return Named list containing:
#' \itemize{
#' \item \code{name}, string containing the name of the gene set.
#' \item \code{description}, string containing a description for the gene set.
#' \item \code{size}, integer scalar specifying the number of genes in this gene set.
#' \item \code{collection}, integer scalar containing the index of the collection containing this gene set.
#' This index refers to a row of the output of \code{\link{fetchAllCollections}}.
#' \item \code{number}, integer scalar containing the index of the gene set inside the specified collection.
#' }
#'
#' @author Aaron Lun
#' @examples
#' fetchSingleSet("9606", 1)
#'
#' @export
#' @importFrom utils head
fetchSingleSet <- function(species, set, fetch.file = NULL, fetch.file.args = list(), fetch.range = NULL, fetch.range.args = list(), use.preloaded = TRUE) {
    if (use.preloaded) {
        candidate <- fetchAllSets.env$result[[species]]
        if (!is.null(candidate)) {
            return(candidate[[set]])
        }
    }

    fname <- paste0(species, "_sets.tsv")
    range.info <- get_single_set_ranges(species, fname, fetch.file=fetch.file, fetch.file.args=fetch.file.args, use.preloaded=use.preloaded)

    if (is.null(fetch.range)) {
        fetch.range <- downloadIndexRange
    }
    interval <- range.info$ranges[set + 0:1] - 0:1 # skip the newline.
    deets <- do.call(fetch.range, c(list(fname, interval), fetch.range.args))
    split <- strsplit(deets, "\t")[[1]]

    list(
        name = split[1],
        description = split[2],
        size = range.info$sizes[set],
        collection = range.info$collections[set],
        number = range.info$numbers[set]
    )
}
    
fetchSingleSet.env <- new.env()
fetchSingleSet.env$result <- list()

get_single_set_ranges <- function(species, fname, fetch.file, fetch.file.args, use.preloaded) {
    range.info <- fetchSingleSet.env$result[[species]]
    if (is.null(range.info)) {
        range.info <- retrieve_ranges_with_sizes(fname, fetch=fetch.file, fetch.args=fetch.file.args)
        csizes <- fetchCollectionSizes(species, fetch.file=fetch.file, fetch.file.args=fetch.file.args, use.preloaded=use.preloaded)
        range.info$collections <- rep(seq_along(csizes), csizes)
        range.info$numbers <- sequence(csizes)
        fetchSingleSet.env$result[[species]] <- range.info
    }
    range.info
}

#' Size of gene sets
#'
#' Quickly get the size of the sets in the Gesel index.
#' This is more efficient than \code{\link{fetchAllSets}} when only the sizes are of interest.
#'
#' @inheritParams fetchSingleSet
#'
#' @return Integer vector containing the size of each set (i.e., the number of gene sets).
#'
#' @author Aaron Lun
#' @examples
#' head(fetchSetSizes("9606"))
#'
#' @export
fetchSetSizes <- function(species, fetch.file = NULL, fetch.file.args = list(), use.preloaded = TRUE) {
    if (use.preloaded) {
        candidate <- fetchAllSets.env$result[[species]]
        if (!is.null(candidate)) {
            return(nrow(candidate))
        }
    }

    fname <- paste0(species, "_sets.tsv")
    range.info <- get_single_set_ranges(species, fname, fetch.file=fetch.file, fetch.file.args=fetch.file.args, use.preloaded=use.preloaded)
    range.info$sizes
}
