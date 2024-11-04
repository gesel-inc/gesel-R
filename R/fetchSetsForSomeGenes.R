#' Fetch sets for some genes
#'
#' Fetch all sets that contain some genes in the Gesel database.
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
#' @author Aaron Lun
#' @examples
#' first.gene <- fetchSetsForSomeGenes("9606", 1)
#' str(first.gene)
#' 
#' # Sets containing the first gene
#' all.set.info <- fetchAllSets("9606")
#' head(all.set.info[first.gene[[1]],])
#' 
#' @export
fetchSetsForSomeGenes <- function(species, genes, fetch.file = downloadDatabaseFile, fetch.file.args = list(), fetch.range = downloadDatabaseRanges, fetch.range.args = list()) {
    candidate <- get_cache("fetchSetsForAllGenes", species)
    if (!is.null(candidate)) {
        return(candidate[genes])
    }

    fname <- paste0(species, "_gene2set.tsv")
    cached <- get_cache("fetchSetsForSomeGenes", species)
    modified <- FALSE

    if (is.null(cached)) {
        intervals <- retrieve_ranges(fname, fetch=fetch.file, fetch.args=fetch.file.args)
        cached <- list(intervals = intervals, prior = list(gene = integer(0), sets = list()))
        modified <- TRUE
    }

    prior.gene <- cached$prior$gene
    prior.sets <- cached$prior$sets

    needed <- sort(setdiff(genes, prior.gene))
    if (length(needed)) {
        intervals <- cached$intervals
        deets <- do.call(fetch.range, c(list(name=fname, start=intervals[needed], end=intervals[needed + 1L]), fetch.range.args))
        prior.gene <- c(prior.gene, needed)
        prior.sets <- c(prior.sets, decode_indices(deets))
        modified <- TRUE
    }

    if (modified) {
        cached$prior$gene <- prior.gene
        cached$prior$sets <- prior.sets
        set_cache("fetchSetsForSomeGenes", species, cached)
    }

    m <- match(genes, prior.gene)
    prior.sets[m]
}
