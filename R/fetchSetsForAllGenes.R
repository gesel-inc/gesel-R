#' Fetch sets for all genes
#'
#' Fetch the identities of the sets that contain each gene in the Gesel index.
#'
#' @param species String containing the NCBI taxonomy ID of the species of interest.
#' @inheritParams fetchAllCollections
#'
#' @return List of integer vectors.
#' Each vector corresponds to a gene in the same order as \code{\link{fetchAllGenes}}.
#' Each vector contains the identities of the sets that contain that gene, where each integer is an index into \code{\link{fetchAllSets}}.
#'
#' @author Aaron Lun
#' @examples
#' all.genes <- fetchSetsForAllGenes("9606")
#' length(all.genes)
#'
#' # Sets containing the first gene:
#' head(fetchAllSets("9606")[all.genes[[1]],])
#'
#' # Details about the first gene:
#' fetchAllGenes("9606")$symbol[1]
#' 
#' @export
fetchSetsForAllGenes <- function(species, fetch = NULL, fetch.args = list(), use.preloaded = TRUE) {
    if (use.preloaded) {
        candidate <- fetchSetsForAllGenes.env$result[[species]]
        if (!is.null(candidate)) {
            return(candidate)
        }
    }
    if (is.null(fetch)) {
        fetch <- downloadIndexFile 
    }

    fname <- paste0(species, "_gene2set.tsv.gz")
    path <- do.call(fetch, c(list(fname), fetch.args))
    raw <- decompress_lines(path)
    output <- strsplit(raw, "\t")
    for (i in seq_along(output)) {
        output[[i]] <- cumsum(as.integer(output[[i]])) + 1L
    }

    fetchSetsForAllGenes.env$result[[species]] <- output
    output
}

fetchSetsForAllGenes.env <- new.env()
fetchSetsForAllGenes.env$result <- list()
