#' Fetch sets for some genes
#'
#' Fetch the identities of sets that contain some genes in the Gesel database.
#' This can be more efficient than \code{\link{fetchSetsForAllGenes}} if only a few genes are of interest.
#' 
#' @param genes Integer vector containing gene indices.
#' Each gene index refers to a row of the data frame returned by \code{\link{fetchAllGenes}}).
#' @inheritParams fetchGenesForSomeSets
#'
#' @return List of integer vectors.
#' Each vector corresponds to a gene in \code{genes} and contains the identities of the sets containing that gene.
#' Each set is defined by its set index, which refers to a row of the data frame returned by \code{\link{fetchAllSets}}.
#'
#' @details
#' Every time this function is called, information from the requested \code{genes} will be added to an in-memory cache.
#' Subsequent calls to this function will re-use as many of the cached genes as possible before making new requests to the Gesel database.
#'
#' If \code{\link{fetchSetsForAllGenes}} is called, its cached data will be directly used by \code{fetchSomeSets} to avoid extra requests to the database.
#' If \code{genes} is large, it may be more efficient to call \code{\link{fetchSetsForAllGenes}} to prepare the cache before calling this function.
#' 
#' @author Aaron Lun
#' @examples
#' first.gene <- fetchSetsForSomeGenes("9606", 1:5)
#' str(first.gene)
#' 
#' # Sets containing the first gene.
#' all.set.info <- fetchAllSets("9606")
#' head(all.set.info[first.gene[[1]],])
#'
#' # Identities of the requested genes.
#' fetchAllGenes("9606")[1:5,]
#' 
#' @export
fetchSetsForSomeGenes <- function(species, genes, config = NULL) {
    config <- get_config(config)
    candidate <- get_cache(config, "fetchSetsForAllGenes", species)
    if (!is.null(candidate)) {
        return(candidate[genes])
    }

    fname <- paste0(species, "_gene2set.tsv")
    raw.cached <- get_sets_for_some_genes_ranges(config, species, fname)
    cached <- raw.cached$cached
    modified <- raw.cached$modified

    prior.gene <- cached$prior$gene
    prior.sets <- cached$prior$sets

    needed <- setdiff(genes, prior.gene)
    if (length(needed)) {
        consolidated <- consolidateRanges(cached$intervals, needed, max.gap = consolidate_max_gap(config))
        consolidated.parts <- fetch_ranges(config, fname, consolidated$start, consolidated$end)
        newly.obtained <- setdiff(consolidated$requested, prior.gene)

        refined.parts <- refine_ranges(
            consolidated.parts,
            consolidated$start,
            consolidated$end,
            cached$intervals[newly.obtained],
            cached$intervals[newly.obtained + 1L] - 1L # omit the trailing newline.
        )

        prior.gene <- c(prior.gene, newly.obtained)
        prior.sets <- c(prior.sets, decode_indices_from_raw(refined.parts))
        modified <- TRUE
    }

    if (modified) {
        cached$prior$gene <- prior.gene
        cached$prior$sets <- prior.sets
        set_cache(config, "fetchSetsForSomeGenes", species, cached)
    }

    m <- match(genes, prior.gene)
    prior.sets[m]
}

get_sets_for_some_genes_ranges <- function(config, species, fname) {
    cached <- get_cache(config, "fetchSetsForSomeGenes", species)
    if (!is.null(cached)) {
        return(list(cached=cached, modified=FALSE))
    }

    intervals <- retrieve_ranges(config, fname)
    cached <- list(intervals = intervals, prior = list(gene = integer(0), sets = list()))
    return(list(cached=cached, modified=TRUE))
}

#' Effective number of genes
#'
#' Count the number of genes in the Gesel database that belong to at least one set.
#'
#' @inheritParams fetchAllCollections
#'
#' @details
#' The return value should be used as the total number of balls when performing a hypergeometric test for gene set enrichment
#' (see \code{\link{phyper}}), instead of \code{nrow(fetchAllGenes(species))}.
#' This ensures that uninteresting genes like pseudo-genes or predicted genes are ignored during the calculation.
#' Otherwise, unknown genes would inappropriately increase the number of balls and understate the enrichment p-values. 
#'
#' @return Integer specifying the number of genes in Gesel that belong to at least one set.
#'
#' @author Aaron Lun
#'
#' @export
effectiveNumberOfGenes <- function(species, config = NULL) {
    config <- get_config(config)
    candidate <- get_cache(config, "fetchSetsForAllGenes", species)
    if (!is.null(candidate)) {
        return(sum(lengths(candidate) > 0L))
    }

    fname <- paste0(species, "_gene2set.tsv")
    raw.cached <- get_sets_for_some_genes_ranges(config, species, fname)
    cached <- raw.cached$cached
    if (raw.cached$modified) {
        set_cache(config, "fetchSetsForSomeGenes", species, cached)
    }

    sum(diff(cached$intervals) > 1L) # for the newline character.
}
