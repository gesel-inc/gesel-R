#' Fetch all genes
#'
#' Fetch names for all genes.
#'
#' @param species String specifying the taxonomy ID of the species of interest.
#' @param types Character vector specifying the gene ID types to returnc.
#' This is typically one or more of \code{"symbol"}, \code{"entrez"}, and \code{"ensembl"},
#' defaulting to all of them.
#' @param fetch Function that accepts the name of the file in the Gesel gene descriptions and returns an absolute path to the file.
#' If \code{NULL}, it defaults to \code{\link{downloadGeneFile}}.
#' @param fetch.args Named list of arguments to pass to \code{fetch}.
#' @param use.preloaded Logical scalar indicating whether to use the preloaded value from a previous call to this function.
#'
#' @return List of length equal to \code{types}.
#' Each element is another list of length equal to the number of genes,
#' containing character vectors with the names of those genes.
#'
#' @author Aaron Lun
#' @examples
#' out <- fetchAllGenes("9606")
#' names(out)
#' head(out$symbol)
#' 
#' @export
fetchAllGenes <- function(species, types = NULL, fetch = NULL, fetch.args = list(), use.preloaded = TRUE) {
    if (is.null(types)) {
        types <- c("symbol", "entrez", "ensembl")
    }
    if (is.null(fetch)) {
        fetch <- downloadGeneFile
    }

    output <- list()
    for (t in types) {
        if (use.preloaded) {
            candidate <- fetchAllGenes.env$result[[species]][[t]]
            if (!is.null(candidate)) {
                output[[t]] <- candidate
                next
            }
        }

        path <- do.call(fetch, c(list(paste0(species, "_", t, ".tsv.gz")), fetch.args))
        raw <- decompress_lines(path)
        processed <- strsplit(raw, "\t")
        for (i in seq_along(processed)) {
            current <- processed[[i]]
            if (length(current) == 1L && current == "") {
                processed[[i]] <- character()
            }
        }

        output[[t]] <- processed
        if (is.null(fetchAllGenes.env$result[[species]])) {
            fetchAllGenes.env$result[[species]] <- list()
        }
        fetchAllGenes.env$result[[species]][[t]] <- processed
    }

    output
}

fetchAllGenes.env <- new.env()
fetchAllGenes.env$result <- list()
