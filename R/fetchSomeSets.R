#' Fetch some sets
#' 
#' Fetch the details of some gene sets from the Gesel database.
#' This can be more efficient than calling \code{\link{fetchAllSets}} when only a few sets are of interest.
#'
#' @inheritParams fetchGenesForSomeSets
#' @param sets Integer vector of set indices, where each set index refers to a row in the data frame returned by \code{\link{fetchAllSets}}.
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
fetchSomeSets <- function(species, sets, fetch.file = downloadDatabaseFile, fetch.file.args = list(), fetch.range = downloadDatabaseRanges, fetch.range.args = list()) {
    candidate <- get_cache("fetchAllSets", species)
    if (!is.null(candidate)) {
        output <- candidate[sets,]
        rownames(output) <- NULL
        return(output)
    }

    fname <- paste0(species, "_sets.tsv")
    raw.cached <- get_single_set_ranges(species, fname, fetch.file=fetch.file, fetch.file.args=fetch.file.args)
    cached <- raw.cached$cached
    modified <- raw.cached$modified

    prior.sets <- cached$prior$sets
    prior.details <- cached$prior$details

    needed <- setdiff(sets, prior.sets)
    if (length(needed)) {
        intervals <- cached$intervals
        starts <- intervals[needed]
        ends <- intervals[needed + 1L] - 1L # remove the newline.
        deets <- do.call(fetch.range, c(list(name=fname, start=starts, end=ends), fetch.range.args))

        prior.sets <- c(prior.sets, needed)
        split <- strsplit(deets, "\t")
        extra.df <- data.frame(
            name = vapply(split, function(x) x[1], ""),
            description = vapply(split, function(x) x[2], "")
        )
        prior.details <- rbind(prior.details, extra.df)
        modified <- TRUE
    }

    if (modified) {
        cached$prior$sets <- prior.sets
        cached$prior$details <- prior.details
        set_cache("fetchSomeSets", species, cached)
    }

    output <- prior.details[match(sets, prior.sets),]
    output$size <- cached$size[sets]
    output$collection <- cached$collections[sets]
    output$number <- cached$numbers[sets]
    rownames(output) <- NULL
    output
}
    
get_single_set_ranges <- function(species, fname, fetch.file, fetch.file.args) { 
    cached <- get_cache("fetchSomeSets", species)
    if (!is.null(cached)) {
        return(list(cached=cached, modified=FALSE))
    }

    range.info <- retrieve_ranges_with_sizes(fname, fetch=fetch.file, fetch.args=fetch.file.args)
    csizes <- fetchCollectionSizes(species, fetch.file=fetch.file, fetch.file.args=fetch.file.args)

    cached <- list(
        intervals = range.info$ranges,
        collections = rep(seq_along(csizes), csizes),
        numbers = sequence(csizes),
        sizes = range.info$sizes,
        prior = list(
            sets=integer(0),
            details=data.frame(
                name=character(0),
                description=character(0)
            )
        )
    )

    list(cached=cached, modified=TRUE)
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
fetchSetSizes <- function(species, fetch.file = NULL, fetch.file.args = list()) {
    candidate <- get_cache("fetchAllSets", species)
    if (!is.null(candidate)) {
        return(candidate$size)
    }

    fname <- paste0(species, "_sets.tsv")
    raw.cached <- get_single_set_ranges(species, fname, fetch.file=fetch.file, fetch.file.args=fetch.file.args)

    cached <- raw.cached$cached
    if (raw.cached$modified) {
        set_cache("fetchSomeSets", species, cached)
    }

    cached$sizes
}
