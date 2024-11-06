#' Fetch all gene sets
#'
#' Fetch information about all gene sets in the Gesel database.
#'
#' @inheritParams fetchAllCollections
#' 
#' @return Data frame of gene set information.
#' Each row represents a gene set and contains:
#' \itemize{
#' \item \code{name}, string containing the name of the gene set.
#' \item \code{description}, string containing a description for the gene set.
#' \item \code{size}, integer scalar specifying the number of genes in this gene set.
#' \item \code{collection}, integer scalar containing the collection index of the collection that contains this gene set.
#' The collection index refers to a row of the data frame returned by \code{\link{fetchAllCollections}}.
#' \item \code{number}, integer scalar containing the position of the gene set inside the specified collection.
#' The set index of the current gene set is defined by adding \code{number - 1} to the collection's \code{start}. 
#' }
#'
#' @author Aaron Lun
#' @examples
#' out <- fetchAllSets("9606")
#' head(out)
#'
#' @export
fetchAllSets <- function(species, config = NULL) {
    config <- get_config(config)
    candidate <- get_cache(config, "fetchAllSets", species)
    if (!is.null(candidate)) {
        return(candidate)
    }

    fname <- paste0(species, "_sets.tsv.gz")
    path <- fetch_file(config, fname)
    raw <- decompress_lines(path)
    details <- strsplit(raw, "\t")

    names <- desc <- size <- character(length(details))
    for (i in seq_along(details)) {
        current <- details[[i]]
        names[i] <- current[1]
        desc[i] <- current[2]
        size[i] <- current[3]
    }

    info <- fetchAllCollections(species, config=config)
    output <- data.frame(
        name=names,
        description=desc,
        size=as.integer(size),
        collection=rep(seq_len(nrow(info)), info$size),
        number=sequence(info$size)
    )

    set_cache(config, "fetchAllSets", species, output)
    output
}
