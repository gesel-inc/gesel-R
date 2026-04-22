#' Fetch genes for some sets
#'
#' Fetch the gene membership of some sets in the Gesel database.
#' This can be more efficient than \code{\link{fetchGenesForAllSets}} if only a few sets are of interest.
#' 
#' @param species String containing the NCBI taxonomy ID of the species of interest.
#' @param sets Integer vector containing set indices.
#' Each set index refers to a row in the data frame returned by \code{\link{fetchAllSets}}.
#' @param config Configuration list, typically created by \code{\link{newConfig}}.
#' If \code{NULL}, the default configuration is used.
#'
#' @return List of integer vectors.
#' Each vector corresponds to a set in \code{sets} and contains the identities of its member genes.
#' Each gene is defined by its gene index, which refers to a row of the data frame returned by \code{\link{fetchAllGenes}}.
#'
#' @details
#' Every time this function is called, information from the requested \code{sets} will be added to an in-memory cache.
#' Subsequent calls to this function will re-use as many of the cached sets as possible before making new requests to the Gesel database.
#'
#' If \code{\link{fetchGenesForAllSets}} was previously called, its cached data will be directly used by \code{fetchGenesForSomeSets} to avoid performing extra requests to the database.
#' If \code{sets} is large, it may be more efficient to call \code{\link{fetchGenesForAllSets}} to prepare the cache before calling this function.
#'
#' @author Aaron Lun
#' @examples
#' first.set <- fetchGenesForSomeSets("9606", 1:5)
#' str(first.set)
#' 
#' # Genes in the first set:
#' gene.symbols <- fetchAllGenes("9606")$symbol
#' head(gene.symbols[first.set[[1]]])
#'
#' # Identities of the requested sets.
#' set.info <- fetchAllSets("9606")[1:5,]
#' set.info
#' 
#' @export
fetchGenesForSomeSets <- function(species, sets, config = NULL) {
    config <- get_config(config)
    candidate <- get_cache(config, "fetchGenesForAllSets", species)
    if (!is.null(candidate)) {
        return(candidate[sets])
    }

    fname <- paste0(species, "_set2gene.tsv")
    cached <- get_cache(config, "fetchGenesForSomeSets", species)
    modified <- FALSE

    if (is.null(cached)) {
        intervals <- retrieve_ranges(config, fname)
        cached <- list(
            intervals = intervals,
            blocked = ranges_to_blocks(intervals, block.size = consolidate_block_size(config)),
            prior = list(set = integer(0), genes = list())
        )
        modified <- TRUE
    }

    prior.set <- cached$prior$set
    prior.genes <- cached$prior$genes

    needed <- setdiff(sets, prior.set)
    if (length(needed)) {
        consolidated <- consolidate_ranges(cached$intervals, cached$blocked, needed)
        consolidated.parts <- fetch_ranges(config, fname, consolidated$start, consolidated$end)
        newly.obtained <- setdiff(consolidated$requested, prior.set)

        refined.parts <- refine_ranges(
            consolidated.parts,
            consolidated$start,
            consolidated$end,
            cached$intervals[newly.obtained],
            cached$intervals[newly.obtained + 1L] - 1L # omit the trailing newline.
        )

        prior.set <- c(prior.set, newly.obtained)
        prior.genes <- c(prior.genes, decode_indices_from_raw(refined.parts))
        modified <- TRUE
    }

    if (modified) {
        cached$prior$set <- prior.set
        cached$prior$genes <- prior.genes
        set_cache(config, "fetchGenesForSomeSets", species, cached)
    }

    m <- match(sets, prior.set)
    prior.genes[m]
}
