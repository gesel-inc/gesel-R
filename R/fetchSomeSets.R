#' Fetch some sets
#' 
#' Fetch the details of some gene sets from the Gesel database.
#' This can be more efficient than calling \code{\link{fetchAllSets}} when only a few sets are of interest.
#'
#' @inheritParams fetchGenesForSomeSets
#' @param sets Integer vector of set indices, where each set index refers to a row in the data frame returned by \code{\link{fetchAllSets}}.
#' @param use.preloaded Logical scalar indicating whether to use the preloaded value from a previous call to \code{\link{fetchAllSets}}.
#'
#' @return Data frame with the same columns as the return value of \code{\link{fetchAllSets}},
#' where each row corresponds to an entry of \code{sets}.
#'
#' @author Aaron Lun
#' @examples
#' fetchSomeSets("9606", 1)
#'
#' @export
#' @importFrom utils head
fetchSomeSets <- function(species, sets, fetch.file = downloadDatabaseFile, fetch.file.args = list(), fetch.range = downloadDatabaseRanges, fetch.range.args = list(), use.preloaded = TRUE) {
    if (use.preloaded) {
        candidate <- fetchAllSets.env$result[[species]]
        if (!is.null(candidate)) {
            output <- candidate[sets,]
            rownames(output) <- NULL
            return(output)
        }
    }

    fname <- paste0(species, "_sets.tsv")
    range.info <- get_single_set_ranges(species, fname, fetch.file=fetch.file, fetch.file.args=fetch.file.args, use.preloaded=use.preloaded)
    intervals <- range.info$ranges

    starts <- intervals[sets]
    ends <- intervals[sets + 1L] - 1L # remove the newline.
    deets <- do.call(fetch.range, c(list(name=fname, start=starts, end=ends), fetch.range.args))
    split <- strsplit(deets, "\t")

    data.frame(
        name = vapply(split, function(x) x[1], ""),
        description = vapply(split, function(x) x[2], ""),
        size = range.info$sizes[sets],
        collection = range.info$collections[sets],
        number = range.info$numbers[sets]
    )
}
    
fetchSomeSets.env <- new.env()
fetchSomeSets.env$result <- list()

get_single_set_ranges <- function(species, fname, fetch.file, fetch.file.args, use.preloaded) {
    range.info <- fetchSomeSets.env$result[[species]]
    if (is.null(range.info)) {
        range.info <- retrieve_ranges_with_sizes(fname, fetch=fetch.file, fetch.args=fetch.file.args)
        csizes <- fetchCollectionSizes(species, fetch.file=fetch.file, fetch.file.args=fetch.file.args, use.preloaded=use.preloaded)
        range.info$collections <- rep(seq_along(csizes), csizes)
        range.info$numbers <- sequence(csizes)
        fetchSomeSets.env$result[[species]] <- range.info
    }
    range.info
}

#' Size of gene sets
#'
#' Quickly get the size of the sets in the Gesel database.
#' This is more efficient than \code{\link{fetchAllSets}} when only the sizes are of interest.
#'
#' @inheritParams fetchSomeSets
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
