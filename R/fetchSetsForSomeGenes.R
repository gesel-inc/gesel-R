#' Fetch sets for some genes
#'
#' Fetch all sets that contain some genes in the Gesel index.
#' This can be more efficient than \code{\link{fetchSetsForAllGenes}} if only a few genes are of interest.
#' 
#' @param species String containing the NCBI taxonomy ID of the species of interest.
#' @param genes Integer vector containing gene indices.
#' Each gene index refers to an entry of the lists returned by \code{\link{fetchAllGenes}}).
#' @inheritParams fetchGenesForSomeSets
#' @param use.preloaded Logical scalar indicating whether to use the preloaded value from a previous call to \code{\link{fetchSetsForAllGenes}}.
#'
#' @return List of integer vectors.
#' Each vector corresponds to a gene in \code{genes} and contains the identities of the sets containing that gene.
#' Each set is defined by its set index, which refers to a row of \code{\link{fetchAllSets}}.
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
fetchSetsForSomeGenes <- function(species, genes, fetch.file = NULL, fetch.file.args = list(), fetch.range = NULL, fetch.range.args = list(), use.preloaded = TRUE) {
    if (use.preloaded) {
        candidate <- fetchSetsForAllGenes.env$result[[species]]
        if (!is.null(candidate)) {
            return(candidate[genes])
        }
    }

    fname <- paste0(species, "_gene2set.tsv")

    intervals <- fetchSetsForSomeGenes.env$result[[species]]
    if (is.null(intervals)) {
        intervals <- retrieve_ranges(fname, fetch=fetch.file, fetch.args=fetch.file.args)
        fetchSetsForSomeGenes.env$result[[species]] <- intervals
    }

    if (is.null(fetch.range)) {
        fetch.range <- downloadIndexRange
    }
    deets <- do.call(fetch.range, c(list(name=fname, start=intervals[genes], end=intervals[genes + 1L]), fetch.range.args))
    decode_indices(deets)
}

fetchSetsForSomeGenes.env <- new.env()
fetchSetsForSomeGenes.env$result <- list()
