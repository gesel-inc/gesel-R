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
#' @details
#' Every time this function is called, information from the requested \code{collections} will be added to an in-memory cache.
#' Subsequent calls to this function will re-use as many of the cached collections as possible before making new requests to the Gesel database.
#'
#' If \code{\link{fetchAllCollections}} was previously called, its cached data will be used by \code{fetchSomeCollections} to avoid extra requests to the database.
#' If \code{collections} is large, it may be more efficient to call \code{\link{fetchAllCollections}} to prepare the cache before calling this function.
#'
#' @author Aaron Lun
#' @examples
#' fetchSomeCollections("9606", 1:5)
#'
#' @export
#' @importFrom utils head
fetchSomeCollections <- function(species, collections, config = NULL) {
    config <- get_config(config)
    candidate <- get_cache(config, "fetchAllCollections", species)
    if (!is.null(candidate)) {
        output <- candidate[collections,]
        rownames(output) <- NULL
        return(output)
    }

    fname <- paste0(species, "_collections.tsv")
    raw.cached <- get_single_collection_ranges(config, species, fname)
    cached <- raw.cached$cached
    modified <- raw.cached$modified

    prior.collections <- cached$prior$collections
    prior.details <- cached$prior$details

    needed <- setdiff(collections, prior.collections)
    if (length(needed)) {
        consolidated <- consolidateRanges(cached$intervals, needed, max.unused = consolidate_max_unused(config))
        consolidated.parts <- fetch_ranges(config, fname, consolidated$start, consolidated$end)
        newly.obtained <- setdiff(consolidated$requested, prior.collections)

        refined.parts <- refine_ranges(
            consolidated.parts,
            consolidated$start,
            consolidated$end,
            cached$intervals[newly.obtained],
            cached$intervals[newly.obtained + 1L] - 1L # omit the trailing newline.
        )

        lines <- vapply(refined.parts, rawToChar, FUN.VALUE="")
        split <- strsplit(lines, "\t")
        extra.df <- data.frame(
            title = vapply(split, function(x) x[1], ""),
            description = vapply(split, function(x) x[2], ""),
            maintainer = vapply(split, function(x) x[4], ""),
            `source` = vapply(split, function(x) x[5], "")
        )

        prior.collections <- c(prior.collections, newly.obtained)
        prior.details <- rbind(prior.details, extra.df)
        modified <- TRUE
    }

    if (modified) {
        cached$prior$collections <- prior.collections
        cached$prior$details <- prior.details
        set_cache(config, "fetchSomeCollections", species, cached)
    }

    output <- prior.details[match(collections, prior.collections),]
    output$start <- cached$starts[collections]
    output$size <- cached$sizes[collections]
    rownames(output) <- NULL
    output
}

get_single_collection_ranges <- function(config, species, fname) {
    config <- get_config(config)
    cached <- get_cache(config, "fetchSomeCollections", species)
    if (!is.null(cached)) {
        return(list(cached=cached, modified=FALSE))
    }

    range.info <- retrieve_ranges_with_sizes(config, fname)

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
#' @inheritParams fetchAllCollections 
#'
#' @return Integer vector containing the size of each collection (i.e., the number of gene sets).
#'
#' @author Aaron Lun
#' @examples
#' head(fetchCollectionSizes("9606"))
#'
#' @export
fetchCollectionSizes <- function(species, config = NULL) {
    config <- get_config(config)
    candidate <- get_cache(config, "fetchAllCollections", species)
    if (!is.null(candidate)) {
        return(candidate$size)
    }

    fname <- paste0(species, "_collections.tsv")
    raw.cached <- get_single_collection_ranges(config, species, fname)

    cached <- raw.cached$cached
    if (raw.cached$modified) {
        set_cache(config, "fetchSomeCollections", species, cached)
    }

    cached$sizes
}
