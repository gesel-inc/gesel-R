#' Fetch genes for all sets
#'
#' Fetch the gene membership of all sets in the Gesel database.
#'
#' @inheritParams fetchAllCollections
#'
#' @return List of integer vectors.
#' Each vector represents a gene set, corresponding to the rows of the data frame returned by \code{\link{fetchAllSets}}.
#' Each vector contains the identities of the genes in that set, 
#' where each integer is a gene index that refers to a row of the data frame returned by \code{\link{fetchAllGenes}}.
#'
#' @details
#' If this function is called once, the returned list will be cached in memory and re-used in subsequent calls to this function.
#' The cached data will also be used to speed up calls to \code{\link{fetchGenesForSomeSets}}.
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
#' @seealso
#' \code{\link{renameGenesInSets}}, to easily convert the gene indices to the usual identifiers (symbols, Ensembl, etc.).
#' 
#' @export
fetchGenesForAllSets <- function(species, config = NULL) {
    config <- get_config(config)
    candidate <- get_cache(config, "fetchGenesForAllSets", species)
    if (!is.null(candidate)) {
        return(candidate)
    }

    fname <- paste0(species, "_set2gene.tsv.gz")
    path <- fetch_file(config, fname)
    raw <- decompress_lines(path)
    output <- decode_indices(raw)

    set_cache(config, "fetchGenesForAllSets", species, output)
    set_cache(config, "fetchGenesForSomeSets", species, NULL) # flush this cache as it won't be used once the full data is loaded.
    output
}
