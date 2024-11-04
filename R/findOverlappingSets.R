#' Find sets overlapping a list of genes
#'
#' Find all sets overlapping any gene in a user-supplied list, and return the number of overlaps per set.
#'
#' @inheritParams fetchAllSets
#' @param genes Integer vector containing gene indices.
#' Each gene index refers to a row of the data frame returned by \code{\link{fetchAllGenes}}.
#' @param more.args Further arguments to pass to \code{\link{fetchSetsForSomeGenes}}.
#'
#' @return A list containing:
#' \itemize{
#' \item \code{overlap}, a data frame of the overlapping sets.
#' Each row represents a set and has the integer columns \code{set} and \code{count}.
#' The former is a set index that refers to a row of the data frame returned by \code{\link{fetchAllSets}}.
#' The latter specifies the number of overlaps with \code{genes} for that set.
#' \item \code{present}, an integer scalar containing the number of genes in \code{genes} that are present in at least one set in the Gesel database for \code{species}.
#' This can be used to remove genes outside of the universe prior to testing for enrichment.
#' }
#'
#' @author Aaron Lun
#' @examples
#' overlaps <- findOverlappingSets("9606", 1:10)
#' head(overlaps$overlap)
#' 
#' @export
findOverlappingSets <- function(species, genes, more.args = list()) {
    info <- do.call(fetchSetsForSomeGenes, c(list(species=species, genes=genes), more.args))
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
