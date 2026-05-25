#' Create mapping of gene identifiers
#'
#' Create a mapping of gene identifiers (Ensembl, symbol, etc.) to their Gesel gene indices.
#' 
#' @inheritParams fetchAllGenes
#' @param type String specifying the type of gene identifier.
#' This can be any type listed in \code{\link{fetchGeneTypes}}. 
#' @param ignore.case Boolean indicating whether case should be ignored.
#' @param config Configuration list, typically created by \code{\link{newConfig}}.
#' If \code{NULL}, the default configuration is used.
#' 
#' @return Named list of integer vectors.
#' Each name is an identifier of the specified \code{type},
#' and each vector contains the identities of genes associated with that identifier (after ignoring case, if \code{ignore.case=TRUE}).
#' Specifically, each gene's identity is represented as a row index into the data frame returned by \code{\link{fetchAllGenes}}.
#'
#' @author Aaron Lun
#' @examples
#' mapping <- createGeneIdentifierMap("9606", type="symbol")
#'
#' # Taking it for a spin:
#' found <- mapping[["SNAP25"]]
#' fetchAllGenes("9606")$symbol[found]
#'
#' @seealso
#' \code{\link{searchGenes}}, which uses the mapping when searching for genes. 
#' 
#' @aliases mapGenesByName
#' @export
createGeneIdentifierMap <- function(species, type, ignore.case = FALSE, config = NULL) {
    if (ignore.case) {
        store.name <- "lower"
    } else {
        store.name <- "cased"
    }

    config <- get_config(config)
    cached <- get_cache(config, "mapGenesByName", species)
    sfound <- cached[[store.name]]
    modified <- FALSE
    if (is.null(sfound)) {
        sfound <- list()
        modified <- TRUE
    }

    tfound <- sfound[[type]]
    if (is.null(tfound)) {
        tfound <- list()
        modified <- TRUE

        genes <- fetchAllGenes(species, types=type, config=config)[[type]]
        all.ids <- unlist(genes)
        if (ignore.case) {
            all.ids <- tolower(all.ids)
        }
        all.idx <- rep(seq_along(genes), lengths(genes))

        tfound <- split(all.idx, all.ids)
        tfound <- lapply(tfound, unique)
    }

    if (modified) {
        sfound[[type]] <- tfound
        cached[[store.name]] <- sfound
        set_cache(config, "mapGenesByName", species, cached)
    }

    tfound
}

# For back-compatibility.
#' @export
mapGenesByName <- function(...) {
    createGeneIdentifierMap(...)
}
