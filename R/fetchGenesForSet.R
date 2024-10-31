#' Fetch genes for a single set
#'
#' Fetch genes for a single set in the Gesel index.
#' 
#' @param species String containing the NCBI taxonomy ID of the species of interest.
#' @param set Integer identifying the set of interest, as an index into the list of all sets for this species 
#' (i.e., the return value of \code{\link{fetchAllSets}}).
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
#' @return Integer vector containing the identities of the genes in \code{set},
#' where each integer is an index into \code{\link{fetchAllGenes}}.
#'
#' @author Aaron Lun
#' @examples
#' first.set <- fetchGenesForSet("9606", 1)
#' 
#' # Genes in the first set:
#' head(fetchAllGenes("9606")$symbol[first.set])
#' 
#' @export
fetchGenesForSet <- function(species, set, fetch.file = NULL, fetch.file.args = list(), fetch.range = NULL, fetch.range.args = list(), use.preloaded = TRUE) {
    if (use.preloaded) {
        candidate <- fetchGenesForAllSets.env$result[[species]]
        if (!is.null(candidate)) {
            return(candidate[[set]])
        }
    }

    fname <- paste0(species, "_set2gene.tsv")

    intervals <- fetchGenesForSet.env$result[[species]]
    if (is.null(intervals)) {
        intervals <- retrieve_ranges(fname, fetch=fetch.file, fetch.args=fetch.file.args)
        fetchGenesForSet.env$result[[species]] <- intervals
    }

    if (is.null(fetch.range)) {
        fetch.range <- downloadIndexRange
    }
    deets <- do.call(fetch.range, c(list(fname, intervals[set + 0:1]), fetch.range.args))
    cumsum(as.integer(strsplit(deets, "\t")[[1]])) + 1L
}

fetchGenesForSet.env <- new.env()
fetchGenesForSet.env$result <- list()
