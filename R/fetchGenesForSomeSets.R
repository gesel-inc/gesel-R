#' Fetch genes for some sets
#'
#' Fetch genes for some sets in the Gesel database.
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
#' @author Aaron Lun
#' @examples
#' first.set <- fetchGenesForSomeSets("9606", 1)
#' str(first.set)
#' 
#' # Genes in the first set:
#' gene.symbols <- fetchAllGenes("9606")$symbol
#' head(gene.symbols[first.set[[1]]])
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
        cached <- list(intervals = intervals, prior = list(set = integer(0), genes = list()))
        modified <- TRUE
    }

    prior.set <- cached$prior$set
    prior.genes <- cached$prior$genes

    needed <- sort(setdiff(sets, prior.set))
    if (length(needed)) {
        intervals <- cached$intervals
        deets <- fetch_range(config, fname, intervals[needed], intervals[needed + 1L])
        prior.set <- c(prior.set, needed)
        prior.genes <- c(prior.genes, decode_indices(deets))
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
