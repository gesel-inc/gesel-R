#' Fetch available gene identfier types 
#' 
#' Fetch the list of available gene identifier types for this species.
#'
#' @param species String specifying the taxonomy ID of the species of interest.
#' @param config Configuration list, typically created by \code{\link{newConfig}}.
#' If \code{NULL}, the default configuration is used.
#'
#' @return Character vector of the available types.
#'
#' @details
#' In older versions (0.1.0) of the Gesel gene annotation file specification, an explicit listing of types was not generated,
#' so this function will just assume that \code{"ensembl"}, \code{"entrez"} and \code{"symbol"} are available.
#'
#' @author Aaron Lun
#' @examples
#' fetchGeneTypes("9606")
#' fetchGeneTypes("10090")
#' 
#' @export
fetchGeneTypes <- function(species, config = NULL) {
    config <- get_config(config)
    if (fetchGeneVersion(species, config = config) == "0.1.0") {
        return(c("ensembl", "entrez", "symbol"))
    }

    cached <- get_cache(config, "fetchGeneTypes", species)
    if (!is.null(cached)) {
        return(cached)
    }

    manifest <- fetch_gene(config, paste0(species, "_gene-types.tsv"))
    types <- readLines(manifest)
    set_cache(config, "fetchGeneTypes", species, types)
    types
}
