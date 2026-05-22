#' Query gene sets
#'
#' Query gene sets based on overlaps with genes of interest or matches to keyboards in their names/descriptions.
#'
#' @inheritParams fetchAllGenes
#' @param genes Character vector of gene identifiers of any supported type.
#' These are typically Ensembl/Entrez identifiers or gene symbols.
#' If not \code{NULL}, this function will search for gene sets that overlap any of the supplied genes.
#' @param text String containing one or more keywords to search on, see the \code{query=} argument in \code{\link{searchSetText}}.
#' If not \code{NULL}, this function will search for gene sets that contain the (tokenized) keywords in their names or descriptions.
#' @param counts.only Boolean indicating whether to return a list of the overlapping genes in each set.
#' Only used if \code{genes} is provided.
#'
#' @return A data frame containing one row per set that matches the query conditions.
#' This contains the following columns:
#' \itemize{
#' \item \code{name}, string containing the name of the gene set.
#' \item \code{description}, string containing a description of the gene set.
#' \item \code{size}, integer specifying the number of genes in this gene set.
#' \item \code{collection}, the name of the collection that contains this gene set.
#' \item \code{set}, integer specifying the Gesel set index that can be used in other \pkg{gesel} functions, e.g., \code{\link{fetchGenesForSomeSets}}.
#' }
#' If \code{genes} is provided, the data frame will additionally contain:
#' \itemize{
#' \item \code{count}, an integer column containing the number of overlaps between the genes in the set and those in \code{genes}.
#' \item \code{genes}, a nested list where each entry is a character vector containing the genes in \code{genes} that are present in each set.
#' Only reported if \code{counts.only = FALSE}.
#' \item \code{pvalue} column, a numeric column containing the hypergeometric p-value for overrepresentation of \code{genes} in the set.
#' Rows will be sorted by this column if it is present.
#' }
#'
#' @details
#' This is a user-friendly wrapper for quick and convenient searching of the Gesel database.
#' Developers may prefer to use the lower-level \pkg{gesel} functions for more customization and flexibility.
#'
#' @author Aaron Lun
#' @examples
#' out <- querySets(
#'     species = "9606",
#'     genes = c("tead1", "tead2", "tead3", "tead4"),
#'     text = "transcription"
#' )
#' head(out)
#'
#' out2 <- querySets(
#'     species = "9606",
#'     genes = c("SNAP25", "neurod4", "neurod6"),
#'     text = "neuro*",
#'     counts.only = FALSE
#' )
#' head(out2)
#'
#' @seealso
#' \code{\link{searchGenes}}, to convert gene identifiers into internal Gesel indices.
#'
#' \code{\link{searchOverlappingSets}}, to find the sets that overlap the genes of interest.
#'
#' \code{\link{searchSetText}}, to find sets based on keywords in their names/descriptions.
#'
#' \code{\link{fetchSomeSets}}, to get the details for each set. 
#'
#' \code{\link{fetchSomeCollections}}, to get the details for each collection. 
#' 
#' @export
querySets <- function(species, genes = NULL, text = NULL, counts.only = TRUE, config = NULL) {
    if (!is.null(genes)) {
        mapped <- searchGenes(species, genes, config = config)
        output <- searchOverlappingSets(species, genes = unique(unlist(mapped)), counts.only = counts.only, config = config)$overlap
        output$size <- NULL # this will be added by fetchSomeSets, no need for it here.
        output <- output[order(output$pvalue),]

        if (!counts.only) {
            # Converting it back to the user-provided identifiers.
            # This is more intuitive than using some arbitrarily chosen identifier from the gene files.
            revmap <- rep(genes, lengths(mapped))
            all.mapped <- unlist(mapped)
            output$genes <- relist(revmap[match(unlist(output$genes), all.mapped)], output$genes)
        }

    } else {
        output <- NULL
    }

    if (!is.null(text)) {
        tout <- searchSetText(species, text, config = config)
        if (!is.null(output)) {
            output <- output[output$set %in% tout,,drop=FALSE]
        } else {
            output <- data.frame(set = tout)
        }
    }

    if (is.null(output)) {
        stop("at least one of 'genes' or 'text' must be provided")
    }

    details <- fetchSomeSets(species, output$set, config = config)
    output <- cbind(details, output)
    output$collection <- fetchSomeCollections(species, output$collection, config = config)$title
    rownames(output) <- NULL
    output$number <- NULL

    output
}
