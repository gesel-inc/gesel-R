#' Fetch genes for some sets
#'
#' Fetch genes for some sets in the Gesel database.
#' This can be more efficient than \code{\link{fetchGenesForAllSets}} if only a few sets are of interest.
#' 
#' @param species String containing the NCBI taxonomy ID of the species of interest.
#' @param sets Integer vector containing set indices.
#' Each set index refers to a row in the data frame returned by \code{\link{fetchAllSets}}.
#' @param fetch.file Function that accepts the name of the file in the Gesel database and returns an absolute path to the file.
#' @param fetch.file.args Named list of arguments to pass to \code{fetch.file}.
#' @param fetch.range Function that accepts at least two arguments - 
#' the name of the file in the Gesel database, and an integer vector of length 2 containing the zero-indexed half-open byte range to extract from the file
#' (see \code{\link{downloadDatabaseRanges}} for details).
#' It should return a string containing the contents of the specified byte range.
#' @param fetch.range.args Named list of arguments to pass to \code{fetch.file}.
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
fetchGenesForSomeSets <- function(species, sets, fetch.file = downloadDatabaseFile, fetch.file.args = list(), fetch.range = downloadDatabaseRanges, fetch.range.args = list()) {
    candidate <- get_cache("fetchGenesForAllSets", species)
    if (!is.null(candidate)) {
        return(candidate[sets])
    }

    fname <- paste0(species, "_set2gene.tsv")
    cached <- get_cache("fetchGenesForSomeSets", species)
    modified <- FALSE

    if (is.null(cached)) {
        intervals <- retrieve_ranges(fname, fetch=fetch.file, fetch.args=fetch.file.args)
        cached <- list(intervals = intervals, prior = list(set = integer(0), genes = list()))
        modified <- TRUE
    }

    prior.set <- cached$prior$set
    prior.genes <- cached$prior$genes

    needed <- sort(setdiff(sets, prior.set))
    if (length(needed)) {
        intervals <- cached$intervals
        deets <- do.call(fetch.range, c(list(name=fname, start=intervals[needed], end=intervals[needed + 1L]), fetch.range.args))
        prior.set <- c(prior.set, needed)
        prior.genes <- c(prior.genes, decode_indices(deets))
        modified <- TRUE
    }

    if (modified) {
        cached$prior$set <- prior.set
        cached$prior$genes <- prior.genes
        set_cache("fetchGenesForSomeSets", species, cached)
    }

    m <- match(sets, prior.set)
    prior.genes[m]
}
