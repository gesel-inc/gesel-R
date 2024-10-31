#' Fetch all gene sets
#'
#' Fetch information about all gene sets in the \pkg{gesel} index.
#'
#' @inheritParams fetchAllCollections
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
fetchAllSets <- function(species, fetch = NULL, fetch.args = list(), use.preloaded = TRUE) {
    if (use.preloaded) {
        candidate <- fetchAllSets.env$result[[species]]
        if (!is.null(candidate)) {
            return(candidate)
        }
    }

    if (is.null(fetch)) {
        fetch <- downloadIndexFile
    }

    fname <- paste0(species, "_sets.tsv.gz")
    path <- do.call(fetch, c(list(fname), fetch.args))
    raw <- decompress_lines(path)
    details <- strsplit(raw, "\t")

    names <- desc <- size <- character(length(details))
    for (i in seq_along(details)) {
        current <- details[[i]]
        names[i] <- current[1]
        desc[i] <- current[2]
        size[i] <- current[3]
    }

    info <- fetchAllCollections(species, fetch=fetch, fetch.args=fetch.args, use.preloaded=use.preloaded)
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
