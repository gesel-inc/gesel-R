#' Fetch sets for all genes
#'
#' Fetch the identities of the sets that contain each gene in the Gesel database.
#'
#' @inheritParams fetchAllCollections
#'
#' @return List of integer vectors.
#' Each vector corresponds to a gene in the same order as \code{\link{fetchAllGenes}}.
#' Each vector contains the identities of the sets that contain that gene, 
#' where each integer is a set index that refers to a row of the data frame returned by \code{\link{fetchAllSets}}.
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
fetchSetsForAllGenes <- function(species, config = NULL) {
    config <- get_config(config)
    candidate <- get_cache(config, "fetchSetsForAllGenes", species)
    if (!is.null(candidate)) {
        return(candidate)
    }

    fname <- paste0(species, "_gene2set.tsv.gz")
    path <- fetch_file(config, fname)
    raw <- decompress_lines(path)
    output <- decode_indices(raw)

    set_cache(config, "fetchSetsForAllGenes", species, output)
    output
}
