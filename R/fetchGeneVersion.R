#' Fetch version of the gene annotation files
#'
#' Get the version of the Gesel gene annotation file specification used by the species of interest.
#'
#' @param species String specifying the taxonomy ID of the species of interest.
#' @param config Configuration list, typically created by \code{\link{newConfig}}.
#' If \code{NULL}, the default configuration is used.
#'
#' @return String containing the version of the Gesel gene annotation file specification.
#'
#' @author Aaron Lun
#' @examples
#' fetchGeneVersion("9606")
#' 
#' @export
fetchGeneVersion <- function(species, config = NULL) {
    config <- get_config(config)
    v <- config$gene.version
    if (!is.null(v)) {
        return(v)
    }

    v <- get_cache(config, "fetchGeneVersion", species)
    if (!is.null(v)) {
        return(v)
    }

    v <- tryCatch({
        payload <- fetch_gene(config, paste0(species, "_gene-version.tsv"))
        suppressWarnings(readLines(payload))
    }, error = function(e) {
        "0.1.0"
    })
    
    set_cache(config, "fetchGeneVersion", species, v)
    v
}
