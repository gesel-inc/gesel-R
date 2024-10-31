#' Fetch genes for all sets
#'
#' Fetch the identities for genes in all sets in the \pkg{gesel} index.
#'
#' @param species String containing the NCBI taxonomy ID of the species of interest.
#' @inheritParams fetchAllCollections
#'
#' @return List of integer vectors.
#' Each vector corresponds to a gene set in the same order as \code{\link{fetchAllSets}}.
#' Each vector contains the identities of the genes in that set, where each integer is an index into \code{\link{fetchAllGenes}}.
#'
#' @author Aaron Lun
#' @examples
#' all.sets <- fetchGenesForAllSets("9606")
#' length(all.sets)
#'
#' # Genes in the first set:
#' fetchAllGenes("9606")$symbol[all.sets[[1]]]
#'
#' # Details about the first set:
#' fetchAllSets("9606")[1,]
#' 
#' @export
fetchGenesForAllSets <- function(species, fetch = NULL, fetch.args = list(), use.preloaded = TRUE) {
    if (use.preloaded) {
        candidate <- fetchGenesForAllSets.env$result[[species]]
        if (!is.null(candidate)) {
            return(candidate)
        }
    }
    if (is.null(fetch)) {
        fetch <- downloadIndexFile 
    }

    fname <- paste0(species, "_set2gene.tsv.gz")
    path <- do.call(fetch, c(list(fname), fetch.args))
    raw <- decompress_lines(path)
    output <- strsplit(raw, "\t")
    for (i in seq_along(output)) {
        output[[i]] <- cumsum(as.integer(output[[i]])) + 1L
    }

    fetchGenesForAllSets.env$result[[species]] <- output
    output
}

fetchGenesForAllSets.env <- new.env()
fetchGenesForAllSets.env$result <- list()
