#' Fetch all gene sets
#'
#' Fetch information about all gene sets in the \pkg{gesel} index.
#'
#' @param species String containing the NCBI taxonomy ID of the species of interest.
#' @param cache String containing the path to a cache directory.
#' If \code{NULL}, a cache location is automatically chosen.
#' @param overwrite Logical scalar indicating whether any cached file should be overwritten.
#' 
#' @return Data frame of gene set information.
#' Each row represents a gene set and contains:
#' \itemize{
#' \item \code{name}, string containing the name of the gene set.
#' \item \code{description}, string containing a description for the gene set.
#' \item \code{size}, integer scalar specifying the number of genes in this gene set.
#' \item \code{collection}, integer scalar containing the index of the collection containing this gene set.
#' This index refers to a row of the output of \code{\link{fetchAllCollections}}.
#' \item \code{number}, integer scalar containing the index of the gene set inside the specified collection.
#' }
#'
#' @author Aaron Lun
#' @examples
#' out <- fetchAllSets("9606")
#' head(out)
#'
#' @export
fetchAllSets <- function(species, cache = NULL, overwrite = FALSE) {
    if (!overwrite) {
        candidate <- fetchAllSets.env$result[[species]]
        if (!is.null(candidate)) {
            return(candidate)
        }
    }

    fname <- paste0(species, "_sets.tsv.gz")
    path <- download_file(cache, paste0(default_index_url, "/", fname), overwrite=overwrite)
    raw <- decompress_lines(path)
    details <- strsplit(raw, "\t")

    names <- desc <- size <- character(length(details))
    for (i in seq_along(details)) {
        current <- details[[i]]
        names[i] <- current[1]
        desc[i] <- current[2]
        size[i] <- current[3]
    }

    info <- fetchAllCollections(species, cache=cache, overwrite=overwrite)
    output <- data.frame(
        name=names,
        description=desc,
        size=as.integer(size),
        collection=rep(seq_len(nrow(info)), info$size),
        number=sequence(info$size)
    )

    fetchAllSets.env$result[[species]] <- output
    output
}

fetchAllSets.env <- new.env()
fetchAllSets.env$results <- list()
