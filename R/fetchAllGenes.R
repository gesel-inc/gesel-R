#' Fetch all genes
#'
#' Fetch names for all genes.
#'
#' @param species String specifying the taxonomy ID of the species of interest.
#' @param types Character vector specifying the types of gene names to return.
#' This is typically one or more of \code{"symbol"}, \code{"entrez"}, and \code{"ensembl"},
#' defaulting to all of them.
#' @param config Configuration list, typically created by \code{\link{newConfig}}.
#' If \code{NULL}, the default configuration is used.
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
fetchAllGenes <- function(species, types = NULL, config = NULL) {
    if (is.null(types)) {
        types <- c("symbol", "entrez", "ensembl")
    }

    config <- get_config(config)
    cached <- get_cache(config, "fetchAllGenes", species)
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

        path <- fetch_gene(config, paste0(species, "_", t, ".tsv.gz"))
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
        set_cache(config, "fetchAllGenes", species, cached)
    }

    do.call(data.frame, lapply(output, I))
}
