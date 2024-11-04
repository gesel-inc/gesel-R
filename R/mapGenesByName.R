#' Map gene names to indices
#'
#' Create a mapping of gene names (Ensembl, symbol, etc.) to their gene indices.
#' 
#' @inheritParams fetchAllGenes
#' @param type String specifying the type of name.
#' This is typically one of \code{"symbol"}, \code{"entrez"}, and \code{"ensembl"},
#' @param ignore.case Logical scalar indicating whether case should be ignored.
#' @param more.args Named list of further arguments to pass to \code{\link{fetchAllGenes}}.
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
mapGenesByName <- function(species, type, ignore.case = FALSE, more.args=list()) {
    if (ignore.case) {
        store.name <- "lower"
    } else {
        store.name <- "cased"
    }

    cached <- get_cache("mapGenesByName", species)
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

        genes <- do.call(fetchAllGenes, c(list(species, types=type), more.args))[[type]]
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
        set_cache("mapGenesByName", species, cached)
    }

    tfound
}
