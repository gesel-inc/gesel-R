#' Find sets overlapping a list of genes
#'
#' Find all sets overlapping any gene in a user-supplied list, and return the number of overlaps per set.
#'
#' @inheritParams fetchAllSets
#' @param genes Integer vector containing gene indices.
#' Each gene index refers to a row of the data frame returned by \code{\link{fetchAllGenes}}.
#' @param counts.only Boolean indicating whether to only report the number of overlapping genes for each set.
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
#' The row order is arbitrary.
#' \item \code{present}, an integer indicating the number of genes in \code{genes} that are present in at least one set in the Gesel database for \code{species}.
#' }
#'
#' @details
#' \code{present} should be used as the number of draws when performing a hypergeomtric test for gene set enrichment (see \code{\link{phyper}}), instead of \code{length(genes)}.
#' This ensures that genes outside of the Gesel universe are ignored, e.g., due to user error or different genome versions.
#' Otherwise, unknown genes would inappropriately increase the number of draws and inflate the enrichment p-value.
#'
#' @author Aaron Lun
#' @examples
#' overlaps <- findOverlappingSets("9606", 1:10)
#' head(overlaps$overlap)
#' 
#' # More details on the overlapping sets.
#' all.sets <- fetchAllSets("9606")
#' all.sets[head(overlaps$overlap$set),]
#'
#' # Computing an enrichment p-value. We take the upper tail after
#' # subtracting 1 to ensure that the probability mass of the observed
#' # number of overlapping genes is included in the p-value.
#' set.size <- all.sets$size[overlaps$overlap$set]
#' universe <- effectiveNumberOfGenes("9606")
#' p <- phyper(
#'     q = overlaps$overlap$count - 1,
#'     m = set.size,
#'     n = universe - set.size,
#'     k = overlaps$present,
#'     lower.tail=FALSE
#' )
#'
#' # For multiple testing correction, it is necessary to consider all sets
#' # in the database, as these were implicitly considered during the search
#' # though only a subset of them are reported by findOverlappingSets.
#' fdr <- p.adjust(p, method="BH", n=nrow(all.sets))
#' summary(fdr <= 0.05)
#'
#' @export
findOverlappingSets <- function(species, genes, counts.only = TRUE, config = NULL) {
    info <- fetchSetsForSomeGenes(species=species, genes=genes, config=config)

    if (counts.only) {
        tab <- table(unlist(info))
        overlap <- data.frame(
            set=as.integer(names(tab)),
            count=as.integer(tab)
        )

    } else {
        set.ids <- unlist(info)
        gene.ids <- rep(genes, lengths(info))
        by.set <- split(gene.ids, set.ids)
        overlap <- data.frame(
            set=as.integer(names(by.set)),
            genes=I(unname(by.set))
        )
    }

    output <- list(overlap=overlap, present=sum(lengths(info) > 0))
    output
}
