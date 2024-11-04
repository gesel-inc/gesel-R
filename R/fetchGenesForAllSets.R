#' Fetch genes for all sets
#'
#' Fetch the identities for genes in all sets in the Gesel database.
#'
#' @inheritParams fetchAllCollections
#'
#' @return List of integer vectors.
#' Each vector represents a gene set, corresponding to the rows of the data frame returned by \code{\link{fetchAllSets}}.
#' Each vector contains the identities of the genes in that set, 
#' where each integer is a gene index that refers to a row of the data frame returned by \code{\link{fetchAllGenes}}.
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
fetchGenesForAllSets <- function(species, fetch = downloadDatabaseFile, fetch.args = list()) {
    candidate <- get_cache("fetchGenesForAllSets", species)
    if (!is.null(candidate)) {
        return(candidate)
    }

    fname <- paste0(species, "_set2gene.tsv.gz")
    path <- do.call(fetch, c(list(fname), fetch.args))
    raw <- decompress_lines(path)
    output <- decode_indices(raw)

    set_cache("fetchGenesForAllSets", species, output)
    output
}
