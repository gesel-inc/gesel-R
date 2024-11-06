#' Map gene names to indices
#'
#' Create a mapping of gene names (Ensembl, symbol, etc.) to their gene indices.
#' 
#' @inheritParams fetchAllGenes
#' @param type String specifying the type of name.
#' This is typically one of \code{"symbol"}, \code{"entrez"}, and \code{"ensembl"},
#' @param ignore.case Logical scalar indicating whether case should be ignored.
#' @param config Configuration list, typically created by \code{\link{newConfig}}.
#' If \code{NULL}, the default configuration is used.
#' 
#' @return Named list of integer vectors.
#' Each name corresponds to an name of the specified \code{type},
#' and each vector contains the genes associated with that name (after ignoring case, if \code{ignore.case=TRUE}).
#' Vector entries should be interpreted as indices into any of the lists returned by \code{\link{fetchAllGenes}}.
#'
#' @author Aaron Lun
#' @examples
#' mapping <- mapGenesByName("9606", type="symbol")
#'
#' # Taking it for a spin:
#' found <- mapping[["SNAP25"]]
#' fetchAllGenes("9606")$symbol[found]
#' 
#' @export
mapGenesByName <- function(species, type, ignore.case = FALSE, config = NULL) {
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
