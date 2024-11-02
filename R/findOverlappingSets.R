#' Find sets overlapping a list of genes
#'
#' Find all sets overlapping any gene in a user-supplied list, and return the number of overlaps per set.
#'
#' @inheritParams fetchAllSets
#' @param genes Integer vector representing a list of genes.
#' Each entry is an index into one of the lists returned by \code{\link{fetchAllGenes}}.
#' @param num.workers Integer scalar specifying the number of workers to use for parallelizing calls to \code{\link{fetchSetsForGene}}.
#' @param more.args Further arguments to pass to \code{\link{fetchSetsPerGene}}.
#'
#' @return A list containing:
#' \itemize{
#' \item \code{overlap}, a data frame of the overlapping sets.
#' Each row represents a set and has the columns \code{set} and \code{count}.
#' The former contains the identity of the set as an index into \code{\link{fetchAllSets}}.
#' The latter contains the number of overlaps with \code{genes} for that set.
#' \item \code{present}, an integer scalar containing the number of genes in \code{genes} that are present in at least one set in the Gesel index for \code{species}.
#' This can be used to remove genes outside of the universe prior to testing for enrichment.
#' }
#'
#' @author Aaron Lun
#' @examples
#' overlaps <- findOverlappingSets("9606", 1:10)
#' head(overlaps$overlap)
#' 
#' @export
#' @importFrom parallel parLapply makeCluster stopCluster
findOverlappingSets <- function(species, genes, num.workers = 4L, more.args = list()) {
    if (num.workers == 1L) {
        largs <- c(list(X=genes, FUN=fetchSetsForGene, species=species), more.args)
        info <- do.call(lapply, largs)
    } else {
        if (num.workers != findOverlappingSets.env$num.workers) {
            if (!is.null(findOverlappingSets.env$cluster)) {
                stopCluster(findOverlappingSets.env$cluster)
            } 
            findOverlappingSets.env$cluster <- makeCluster(num.workers)
            findOverlappingSets.env$num.workers <- num.workers
        }
        largs <- c(list(cl=findOverlappingSets.env$cluster, X=genes, fun=fetchSetsForGene, species=species), more.args)
        info <- do.call(parLapply, largs)
    }

    tab <- table(unlist(info))
    o <- order(tab, decreasing=TRUE)

    list(
        overlap=data.frame(
            set=as.integer(names(tab))[o],
            count=as.integer(tab)[o]
        ),
        present=sum(lengths(info) > 0)
    )
}

findOverlappingSets.env <- new.env()
findOverlappingSets.env$cluster <- NULL
findOverlappingSets.env$num.workers <- 0L
