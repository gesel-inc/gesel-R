#' Fetch sets for a single gene 
#'
#' Fetch all sets that contain a single gene in the Gesel index.
#' 
#' @param species String containing the NCBI taxonomy ID of the species of interest.
#' @param gene Integer identifying the gene of interest, as an index into the list of all genes for this species 
#' (i.e., one of the lists returned by \code{\link{fetchAllGenes}}).
#' @inheritParams fetchGenesForSet
#' @param use.preloaded Logical scalar indicating whether to use the preloaded value from a previous call to \code{\link{fetchSetsForAllGenes}}.
#'
#' @return Integer vector containing the identities of the sets that contain \code{gene},
#' where each integer is an index into \code{\link{fetchAllSets}}.
#'
#' @author Aaron Lun
#' @examples
#' first.gene <- fetchSetsForGene("9606", 1)
#' 
#' # Sets containing the first gene
#' head(fetchAllSets("9606")[first.gene,])
#' 
#' @export
fetchSetsForGene <- function(species, gene, fetch.file = NULL, fetch.file.args = list(), fetch.range = NULL, fetch.range.args = list(), use.preloaded = TRUE) {
    if (use.preloaded) {
        candidate <- fetchSetsForAllGenes.env$result[[species]]
        if (!is.null(candidate)) {
            return(candidate[[gene]])
        }
    }

    fname <- paste0(species, "_gene2set.tsv")

    intervals <- fetchSetsForGene.env$result[[species]]
    if (is.null(intervals)) {
        intervals <- retrieve_ranges(fname, fetch=fetch.file, fetch.args=fetch.file.args)
        fetchSetsForGene.env$result[[species]] <- intervals
    }

    if (is.null(fetch.range)) {
        fetch.range <- downloadIndexRange
    }
    deets <- do.call(fetch.range, c(list(fname, intervals[gene + 0:1]), fetch.range.args))
    cumsum(as.integer(strsplit(deets, "\t")[[1]])) + 1L
}

fetchSetsForGene.env <- new.env()
fetchSetsForGene.env$result <- list()
