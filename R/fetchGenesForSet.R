#' Fetch genes for a single set
#'
#' Fetch genes for a single set in the \pkg{gesel} index.
#' 
#' @param species String containing the NCBI taxonomy ID of the species of interest.
#' @param set Integer identifying the set of interest, as an index into the list of all sets for this species 
#' (i.e., the return value of \code{\link{fetchGenesForAllSets}}).
#' @param cache String containign the path to a cache directory.
#' If \code{NULL}, a cache location is automatically chosen.
#' @param overwrite Logical scalar indicating whether any cached file should be overwritten.
#' @param force.remote Logical scalar indicating whether to force a remote query.
#'
#' @return Integer vector containing the identities of the genes in that set, 
#' where each integer is an index into \code{\link{fetchAllGenes}}.
#'
#' @author Aaron Lun
#' @examples
#' first.set <- fetchGenesForSet("9606", 1)
#' 
#' # Genes in the first set:
#' fetchAllGenes("9606")$symbol[first.set]
#' 
#' @export
fetchGenesForSet <- function(species, set, cache = NULL, overwrite = FALSE, force.remote = FALSE) {
    if (!force.remote) {
        candidate <- fetchGenesForAllSets.env$result[[species]]
        if (!is.null(candidate)) {
            return(candidate[[set]])
        }
    }

    url <- paste0(default_index_url, "/", species, "_set2gene.tsv")

    intervals <- fetchGenesForSet.env$result[[species]]
    if (is.null(intervals)) {
        intervals <- retrieve_ranges(cache, url, overwrite)
        fetchGenesForSet.env$result[[species]] <- intervals
    }

    deets <- range_request(url, intervals[set + 0:1])
    cumsum(as.integer(strsplit(deets, "\t")[[1]])) + 1L
}

fetchGenesForSet.env <- new.env()
fetchGenesForSet.env$result <- list()
