#' Find sets overlapping a list of genes
#'
#' Find all sets overlapping any gene in a user-supplied list, and return the number of overlaps per set.
#'
#' @inheritParams fetchAllSets
#' @param genes Integer vector containing gene indices.
#' Each gene index refers to a row of the data frame returned by \code{\link{fetchAllGenes}}.
#' @param counts.only Logical scalar indicating whether to only report the number of overlapping genes for each set.
#' @param more.args Further arguments to pass to \code{\link{fetchSetsForSomeGenes}}.
#'
#' @return A list containing:
#' \itemize{
#' \item \code{overlap}, a data frame of the overlapping sets.
#' Each row represents a set that is identified by the set index in the \code{set} column.
#' (This set index refers to a row of the data frame returned by \code{\link{fetchAllSets}}.)
#' It also has:
#' \itemize{
#' \item The \code{count} column, if \code{counts.only=TRUE}.
#' This specifies the number of overlaps between the genes in the set and those in \code{genes}.
#' \item The \code{genes} column, if \code{counts.only=FALSE}.
#' This is a list that contains the entries of \code{genes} that overlap with those in the set.
#' }
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
findOverlappingSets <- function(species, genes, counts.only = TRUE, more.args = list()) {
    info <- do.call(fetchSetsForSomeGenes, c(list(species=species, genes=genes), more.args))

    if (counts.only) {
        tab <- table(unlist(info))
        o <- order(tab, decreasing=TRUE)
        overlap <- data.frame(
            set=as.integer(names(tab))[o],
            count=as.integer(tab)[o]
        )

    } else {
        set.ids <- unlist(info)
        gene.ids <- rep(genes, lengths(info))
        by.set <- split(gene.ids, set.ids)
        o <- order(lengths(by.set), decreasing=TRUE)
        by.set <- by.set[o]
        overlap <- data.frame(
            set=as.integer(names(by.set)),
            genes=I(unname(by.set))
        )
    }

    output <- list(overlap=overlap, present=sum(lengths(info) > 0))
    output
}
