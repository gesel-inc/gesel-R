#' Find sets overlapping a list of genes
#'
#' Find all sets overlapping any gene in a user-supplied list, and return the number of overlaps per set.
#'
#' @inheritParams fetchAllSets
#' @param genes Integer vector containing gene indices.
#' Each gene index refers to a row of the data frame returned by \code{\link{fetchAllGenes}}.
#' @param counts.only Logical scalar indicating whether to only report the number of overlapping genes for each set.
#' @param config Configuration list, typically created by \code{\link{newConfig}}.
#' If \code{NULL}, the default configuration is used.
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
#' }
#'
#' @details
#' The \code{present} number should be used as the number of draws when performing a hypergeomtric test for gene set enrichment
#' (see \code{\link{phyper}}), instead of \code{length(genes)}.
#' It ensures that genes outside of the Gesel universe are ignored, e.g., due to user error, different genome versions.
#' Otherwise, unknown genes would inappropriately increase the number of draws and inflate the enrichment p-value.
#'
#' @author Aaron Lun
#' @examples
#' overlaps <- findOverlappingSets("9606", 1:10)
#' head(overlaps$overlap)
#' 
#' @export
findOverlappingSets <- function(species, genes, counts.only = TRUE, config = NULL) {
    info <- fetchSetsForSomeGenes(species=species, genes=genes, config=config)

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
