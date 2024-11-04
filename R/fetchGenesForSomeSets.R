#' Fetch genes for some sets
#'
#' Fetch genes for some sets in the Gesel index.
#' This can be more efficient than \code{\link{fetchGenesForAllSets}} if only a few sets are of interest.
#' 
#' @param species String containing the NCBI taxonomy ID of the species of interest.
#' @param sets Integer vector containing set indices.
#' Each set index refers to a row in the data frame returned by \code{\link{fetchAllSets}}.
#' @param fetch.file Function that accepts the name of the file in the Gesel index and returns an absolute path to the file.
#' If \code{NULL}, it defaults to \code{\link{downloadIndexFile}}.
#' @param fetch.file.args Named list of arguments to pass to \code{fetch.file}.
#' @param fetch.range Function that accepts at least two arguments - 
#' the name of the file in the Gesel index, and an integer vector of length 2 containing the zero-indexed half-open byte range to extract from the file
#' (see \code{\link{downloadIndexRange}} for details).
#' It should return a string containing the contents of the specified byte range.
#' If \code{NULL}, it defaults to \code{\link{downloadIndexRange}}.
#' @param fetch.range.args Named list of arguments to pass to \code{fetch.file}.
#' @param use.preloaded Logical scalar indicating whether to use the preloaded value from a previous call to \code{\link{fetchGenesForAllSets}}.
#'
#' @return List of integer vectors.
#' Each vector corresponds to a set in \code{sets} and contains the identities of its member genes.
#' Each gene is defined by its gene index, which refers to an entry of the lists returned by \code{\link{fetchAllGenes}}.
#'
#' @author Aaron Lun
#' @examples
#' first.set <- fetchGenesForSomeSets("9606", 1)
#' str(first.set)
#' 
#' # Genes in the first set:
#' gene.symbols <- fetchAllGenes("9606")$symbol
#' head(gene.symbols[first.set[[1]]])
#' 
#' @export
fetchGenesForSomeSets <- function(species, sets, fetch.file = NULL, fetch.file.args = list(), fetch.range = NULL, fetch.range.args = list(), use.preloaded = TRUE) {
    if (use.preloaded) {
        candidate <- fetchGenesForAllSets.env$result[[species]]
        if (!is.null(candidate)) {
            return(candidate[sets])
        }
    }

    fname <- paste0(species, "_set2gene.tsv")

    intervals <- fetchGenesForSomeSets.env$result[[species]]
    if (is.null(intervals)) {
        intervals <- retrieve_ranges(fname, fetch=fetch.file, fetch.args=fetch.file.args)
        fetchGenesForSomeSets.env$result[[species]] <- intervals
    }

    if (is.null(fetch.range)) {
        fetch.range <- downloadIndexRange
    }
    deets <- do.call(fetch.range, c(list(name=fname, start=intervals[sets], end=intervals[sets + 1L]), fetch.range.args))
    decode_indices(deets)
}

fetchGenesForSomeSets.env <- new.env()
fetchGenesForSomeSets.env$result <- list()
