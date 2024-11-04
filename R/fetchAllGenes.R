#' Fetch all genes
#'
#' Fetch names for all genes.
#'
#' @param species String specifying the taxonomy ID of the species of interest.
#' @param types Character vector specifying the types of gene names to return.
#' This is typically one or more of \code{"symbol"}, \code{"entrez"}, and \code{"ensembl"},
#' defaulting to all of them.
#' @param fetch Function that accepts the name of the file in the Gesel gene descriptions and returns an absolute path to the file.
#' If \code{NULL}, it defaults to \code{\link{downloadGeneFile}}.
#' @param fetch.args Named list of arguments to pass to \code{fetch}.
#'
#' @return Data frame where each row represents a gene.
#' Each column corresponds to one of the \code{types} and is a list of character vectors.
#' Each vector in the column contains the names of the specified type for each gene.
#'
#' @author Aaron Lun
#' @examples
#' out <- fetchAllGenes("9606")
#' head(out)
#' head(out$symbol)
#' 
#' @export
fetchAllGenes <- function(species, types = NULL, fetch = downloadGeneFile, fetch.args = list()) {
    if (is.null(types)) {
        types <- c("symbol", "entrez", "ensembl")
    }

    cached <- get_cache("fetchAllGenes", species)
    modified <- FALSE
    if (is.null(cached)) {
        cached <- list()
    }

    output <- list()
    for (t in types) {
        candidate <- cached[[t]]
        if (!is.null(candidate)) {
            output[[t]] <- candidate
            next
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
        cached[[t]] <- processed
        modified <- TRUE
    }

    if (modified) {
        set_cache("fetchAllGenes", species, cached)
    }

    do.call(data.frame, lapply(output, I))
}
