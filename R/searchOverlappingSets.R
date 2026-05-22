#' Search for overlapping gene sets
#'
#' Search for gene sets that overlap with genes in a user-supplied list.
#'
#' @inheritParams fetchAllSets
#' @param genes Integer vector containing gene indices.
#' Each gene index refers to a row of the data frame returned by \code{\link{fetchAllGenes}}.
#' @param counts.only Boolean indicating whether to only report the number of overlapping genes for each set.
#' @param test.enrichment Boolean indicating whether to compute a hypergeometric p-value for enrichment of \code{genes} in each set.
#' @param config Configuration list, typically created by \code{\link{newConfig}}.
#' If \code{NULL}, the default configuration is used.
#'
#' @return A list containing a \code{overlap} data frame and a \code{present} integer.
#'
#' In the \code{overlap} data frame, each row represents a set that overlaps with \code{genes}.
#' The data frame contains the following columns:
#' \itemize{
#' \item \code{set}, an integer column containing the set index.
#' This refers to a row of the data frame returned by \code{\link{fetchAllSets}}.
#' \item \code{count}, an integer column containing the number of overlaps between the genes in the set and those in \code{genes}.
#' \item \code{genes}, a nested list that contains the entries of \code{genes} that overlap with those in the set.
#' Only reported if \code{counts.only = FALSE}.
#' \item \code{size} column, an integer column containing the size of each set.
#' Only reported if \code{test.enrichment = TRUE}, as it is a by-product of the p-value calculation.
#' \item \code{pvalue} column, a numeric column containing the hypergeometric p-value for overrepresentation of \code{genes} in the set.
#' Only reported if \code{test.enrichment = TRUE}.
#' }
#' The row order is arbitrary.
#'
#' \code{present} specifying the number of genes in \code{genes} that are present in at least one set in the Gesel database for \code{species}.
#' \code{present} can be used as the number of draws when performing a hypergeometric test for gene set enrichment, instead of \code{length(genes)} (see Details).
#' This ensures that we do not consider genes that are not present in any gene sets in Gesel,
#' e.g., due to changes in annotation across genome versions or because they are pseudogenes or predicted genes.
#' Otherwise, unknown genes would inappropriately increase the number of draws and inflate the enrichment p-value.
#'
#' @author Aaron Lun
#' @examples
#' out <- searchOverlappingSets("9606", 1:10)
#' overlaps <- out$overlap
#' head(overlaps)
#' 
#' # More details on the overlapping sets.
#' all.sets <- fetchAllSets("9606")
#' all.sets[head(overlaps$set),]
#'
#' # Computing the enrichment p-value manually. We take the upper tail after
#' # subtracting 1 to ensure that the probability mass of the observed
#' # number of overlapping genes is included in the p-value.
#' set.size <- all.sets$size[overlaps$set]
#' universe <- effectiveNumberOfGenes("9606")
#' p <- phyper(
#'     q = overlaps$count - 1,
#'     m = set.size,
#'     n = universe - set.size,
#'     k = out$present,
#'     lower.tail=FALSE
#' )
#' stopifnot(identical(p, overlaps$pvalue))
#'
#' # For multiple testing correction, it is necessary to consider all sets
#' # in the database, as these were implicitly considered during the search
#' # though only a subset of them are reported by searchOverlappingSets.
#' fdr <- p.adjust(p, method="BH", n=nrow(all.sets))
#' summary(fdr <= 0.05)
#'
#' @aliases findOverlappingSets
#' @export
#' @importFrom stats phyper
searchOverlappingSets <- function(species, genes, counts.only = TRUE, test.enrichment = TRUE, config = NULL) {
    info <- fetchSetsForSomeGenes(species = species, genes = genes, config = config)

    if (counts.only) {
        tab <- table(unlist(info))
        overlap <- data.frame(
            set = as.integer(names(tab)),
            count = as.integer(tab)
        )

    } else {
        set.ids <- unlist(info)
        gene.ids <- rep(as.integer(genes), lengths(info))
        by.set <- split(gene.ids, set.ids)
        overlap <- data.frame(
            set = as.integer(names(by.set)),
            count = lengths(by.set)
        )
        overlap$genes <- unname(by.set)
    }

    present <- sum(lengths(info) > 0)
    if (test.enrichment) {
        universe <- effectiveNumberOfGenes(species, config = config)
        set.sizes <- fetchSetSizes(species, config = config)[overlap$set]
        overlap$size <- set.sizes
        overlap$pvalue <- phyper(
            q = overlap$count - 1, # subtract 1 to include the probability mass of the observed number of overlaps.
            m = set.sizes,
            n = universe - set.sizes,
            k = present,
            lower.tail = FALSE
        )
    }

    list(overlap = overlap, present = present)
}

# Provided for back-compatibility.
#' @export
findOverlappingSets <- function(...) {
    searchOverlappingSets(..., test.enrichment = FALSE)
}
