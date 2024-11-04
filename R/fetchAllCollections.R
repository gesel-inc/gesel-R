#' Fetch all gene set collections
#'
#' Fetch information about all gene set collections in the Gesel database.
#' 
#' @param species String containing the NCBI taxonomy ID of the species of interest.
#' @param fetch Function that accepts the name of a Gesel database file and returns an absolute path to that file.
#' @param fetch.args Named list of arguments to pass to \code{fetch}.
#'
#' @return Data frame of gene set collection information.
#' Each row represents a collection and contains:
#' \itemize{
#' \item \code{title}, string containing the title for the collection.
#' \item \code{description}, string containing a description for the collection.
#' \item \code{maintainer}, string containing the identity of the collection's maintainer.
#' \item \code{source}, string containing the source of origin of the collection.
#' \item \code{start}, integer containing the set index for the first gene set in this collection.
#' The set index refers to a row in the data frame returned by \code{\link{fetchAllSets}}.
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
fetchAllCollections <- function(species, fetch = downloadDatabaseFile, fetch.args = list()) {
    candidate <- get_cache("fetchAllCollections", species)
    if (!is.null(candidate)) {
        return(candidate)
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

    set_cache("fetchAllCollections", species, output)
    output
}
