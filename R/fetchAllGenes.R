#' Fetch all genes
#'
#' Fetch names for all genes.
#'
#' @param species String specifying the taxonomy ID of the species of interest.
#' @param types Character vector specifying the gene ID types to returnc.
#' This is typically one or more of \code{"symbol"}, \code{"entrez"}, and \code{"ensembl"},
#' defaulting to all of them.
#' @param cache String specifying the path to a cache directory.
#' If \code{NULL}, a cache location is automatically chosen.
#' @param overwrite Logical scalar indicating whether any cached file should be overwritten.
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
fetchAllGenes <- function(species, types = NULL, cache = NULL, overwrite = FALSE) {
    if (is.null(types)) {
        types <- c("symbol", "entrez", "ensembl")
    }

    output <- list()
    for (t in types) {
        if (!overwrite) {
            candidate <- fetchAllGenes.env$result[[species]][[t]]
            if (!is.null(candidate)) {
                output[[t]] <- candidate
                next
            }
        }

        path <- download_file(cache, paste0(default_gene_url, "/", species, "_", t, ".tsv.gz"), overwrite=overwrite)
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
