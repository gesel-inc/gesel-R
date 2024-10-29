#' Fetch all gene set collections
#'
#' Fetch information about all gene set collections in the \pkg{gesel} index.
#' 
#' @param species String containing the NCBI taxonomy ID of the species of interest.
#' @param cache String containing the path to a cache directory.
#' If \code{NULL}, a cache location is automatically chosen.
#' @param overwrite Logical scalar indicating whether any cached file should be overwritten.
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
fetchAllCollections <- function(species, cache = NULL, overwrite = FALSE) {
    if (!overwrite) {
        candidate <- fetchAllCollections.env$result[[species]]
        if (!is.null(candidate)) {
            return(candidate)
        }
    }

    fname <- paste0(species, "_collections.tsv.gz")
    path <- download_file(cache, paste0(default_index_url, "/", fname), overwrite=overwrite)
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
