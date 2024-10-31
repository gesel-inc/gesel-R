#' Fetch all gene set collections
#'
#' Fetch information about all gene set collections in the Gesel index.
#' 
#' @param species String containing the NCBI taxonomy ID of the species of interest.
#' @param fetch Function that accepts the name of the file in the Gesel index and returns an absolute path to the file.
#' If \code{NULL}, it defaults to \code{\link{downloadIndexFile}}.
#' @param fetch.args Named list of arguments to pass to \code{fetch}.
#' @param use.preloaded Logical scalar indicating whether to use the preloaded value from a previous call to this function.
#'
#' @return Data frame of gene set collection information.
#' Each row represents a collection and contains:
#' \itemize{
#' \item \code{title}, string containing the title for the collection.
#' \item \code{description}, string containing a description for the collection.
#' \item \code{maintainer}, string containing the identity of the collection's maintainer.
#' \item \code{source}, string containing the source of origin of the collection.
#' \item \code{size}, integer scalar specifying the number of gene sets in the collection.
#' }
#' 
#' @author Aaron Lun
#' @examples
#' out <- fetchAllCollections("9606")
#' head(out)
#'
#' @export
#' @importFrom utils head
fetchAllCollections <- function(species, fetch = NULL, fetch.args = list(), use.preloaded = TRUE) {
    if (use.preloaded) {
        candidate <- fetchAllCollections.env$result[[species]]
        if (!is.null(candidate)) {
            return(candidate)
        }
    }

    if (is.null(fetch)) {
        fetch <- downloadIndexFile
    }

    fname <- paste0(species, "_collections.tsv.gz")
    path <- do.call(fetch, c(list(fname), fetch.args))
    raw <- decompress_lines(path)
    details <- strsplit(raw, "\t")

    title <- desc <- maintainer <- src <- size <- character(length(details))
    for (i in seq_along(details)) {
        current <- details[[i]]
        title[i] <- current[1]
        desc[i] <- current[2]
        maintainer[i] <- current[4]
        src[i] <- current[5]
        size[i] <- current[6]
    }

    size <- as.integer(size)
    output <- data.frame(
        title=title,
        description=desc,
        maintainer=maintainer,
        `source`=src,
        start=cumsum(c(1L, head(size, -1L))),
        size=size
    )

    fetchAllCollections.env$result[[species]] <- output
    output
}

fetchAllCollections.env <- new.env()
fetchAllCollections.env$result <- list()
