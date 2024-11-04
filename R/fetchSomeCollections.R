#' Fetch some collections
#' 
#' Fetch the details of some gene set collections from the Gesel database.
#' This can be more efficient than \code{\link{fetchAllCollections}} when only a few collections are of interest.
#'
#' @inheritParams fetchGenesForSomeSets
#' @param collections Integer vector containing collection indices.
#' Each entry refers to a row of the data frame returned by \code{\link{fetchAllCollections}}).
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
fetchSomeCollections <- function(species, collections, fetch.file = downloadDatabaseFile, fetch.file.args = list(), fetch.range = downloadDatabaseRanges, fetch.range.args = list()) {
    candidate <- get_cache("fetchAllCollections", species)
    if (!is.null(candidate)) {
        output <- candidate[collections,]
        rownames(output) <- NULL
        return(output)
    }

    fname <- paste0(species, "_collections.tsv")
    raw.cached <- get_single_collection_ranges(species, fname, fetch.file=fetch.file, fetch.file.args=fetch.file.args)
    cached <- raw.cached$cached
    modified <- raw.cached$modified

    prior.collections <- cached$prior$collections
    prior.details <- cached$prior$details

    needed <- setdiff(collections, prior.collections)
    if (length(needed)) {
        intervals <- cached$intervals
        starts <- intervals[collections]
        ends <- intervals[collections + 1L] - 1L # remove the newline.
        deets <- do.call(fetch.range, c(list(name=fname, start=starts, end=ends), fetch.range.args))

        prior.collections <- c(prior.collections, needed)
        split <- strsplit(deets, "\t")
        extra.df <- data.frame(
            title = vapply(split, function(x) x[1], ""),
            description = vapply(split, function(x) x[2], ""),
            maintainer = vapply(split, function(x) x[4], ""),
            `source` = vapply(split, function(x) x[5], "")
        )
        prior.details <- rbind(prior.details, extra.df)
        modified <- TRUE
    }

    if (modified) {
        cached$prior$collections <- prior.collections
        cached$prior$details <- prior.details
        set_cache("fetchSomeCollections", species, cached)
    }

    output <- prior.details[match(collections, prior.collections),]
    output$start <- cached$starts[collections]
    output$size <- cached$sizes[collections]
    rownames(output) <- NULL
    output
}
    
get_single_collection_ranges <- function(species, fname, fetch.file, fetch.file.args) {
    cached <- get_cache("fetchSomeCollections", species)
    if (!is.null(cached)) {
        return(list(cached=cached, modified=FALSE))
    }

    range.info <- retrieve_ranges_with_sizes(fname, fetch=fetch.file, fetch.args=fetch.file.args)

    cached <- list(
        intervals = range.info$ranges,
        starts = c(0L, cumsum(head(range.info$sizes, -1L))) + 1L,
        sizes = range.info$sizes,
        prior = list(
            collections=integer(0),
            details=data.frame(
                title=character(0),
                description=character(0),
                maintainer=character(0),
                `source`=character(0)
            )
        )
    )

    list(cached=cached, modified=TRUE)
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
fetchCollectionSizes <- function(species, fetch.file = NULL, fetch.file.args = list()) { 
    candidate <- get_cache("fetchAllCollections", species)
    if (!is.null(candidate)) {
        return(candidate$size)
    }

    fname <- paste0(species, "_collections.tsv")
    raw.cached <- get_single_collection_ranges(species, fname, fetch.file=fetch.file, fetch.file.args=fetch.file.args)

    cached <- raw.cached$cached
    if (raw.cached$modified) {
        set_cache("fetchSomeCollections", species, cached)
    }

    cached$sizes
}
