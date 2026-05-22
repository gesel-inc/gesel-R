#' Load all gene sets for a species
#'
#' Load information about all gene sets for a species.
#' This includes its gene membership as well as details like its name and description.
#'
#' @inheritParams fetchGenesForAllSets
#' @param type String specifying the type of gene identifier to use in all sets.
#' This is typically one of \code{"symbol"}, \code{"entrez"}, and \code{"ensembl"},
#' @param config Configuration list, typically created by \code{\link{newConfig}}.
#' If \code{NULL}, the default configuration is used.
#' @param as.compressed Boolean indicating whether to return a \link[IRanges]{CompressedCharacterList}.
#'
#' @return If \code{as.compressed = FALSE}, a list is returned containing:
#' \itemize{
#' \item \code{sets}, a list of length equal to the total number of sets for \code{species}.
#' Each element is a character vector that corresponds to a gene set and contains the genes in that set.
#' Each gene is represented by zero, one or more identifiers of the specified \code{type} (see \code{\link{renameGenesInSets}} for details).
#' The positional index of each set in \code{sets} can be used as the Gesel set index in other \pkg{gesel} functions like \code{\link{fetchGenesForSomeSets}}.
#' \item \code{details}, a data frame where each row corresponds to a gene set in \code{sets}.
#' The data frame contains the following columns:
#' \itemize{
#' \item \code{name}, string containing the name of the gene set.
#' \item \code{description}, string containing a description of the gene set.
#' \item \code{size}, integer specifying the number of genes in this gene set.
#' Note that this may not equal \code{lengths(sets)} if any gene does not have exactly one identifier of the specified \code{type}. 
#' \item \code{collection}, the name of the collection that contains this gene set.
#' }
#' The row index of each set in \code{details} can be used as the Gesel set index in other \pkg{gesel} functions like \code{\link{fetchGenesForSomeSets}}.
#' }
#'
#' If \code{as.compressed = TRUE}, a \link[IRanges]{CompressedCharacterList} of length equal to the total number of sets for \code{species} is returned.
#' Each element is a character vector that contains the identifiers of genes in that set, as described above for \code{sets}.
#' The \code{\link[S4Vectors]{mcols}} contains more details about each set as described above for \code{details}.
#'
#' @author Aaron Lun
#'
#' @seealso
#' \code{\link{fetchGenesForAllSets}}, to obtain the internal Gesel gene indices for all sets.
#'
#' \code{\link{renameGenesInSets}}, to convert Gesel gene indices to identifiers.
#'
#' \code{\link{fetchAllSets}}, to obtain information about all sets.
#' 
#' @examples
#' everything <- loadAllSets("7227", "symbol")
#' head(everything$sets)
#' head(everything$details)
#'
#' everything2 <- loadAllSets("7227", "symbol", as.compressed = TRUE)
#' everything2
#' S4Vectors::mcols(everything2)
#' 
#' @export
loadAllSets <- function(species, type, config = NULL, as.compressed = FALSE) {
    all.sets <- fetchGenesForAllSets(species, config = config)
    all.sets <- renameGenesInSets(species, all.sets, type = type, config = config)

    details <- fetchAllSets(species, config = config)
    details$collection <- fetchAllCollections(species, config = config)$title[details$collection]
    details$number <- NULL

    if (as.compressed) {
        asNamespace("IRanges")
        output <- as(all.sets, "CompressedCharacterList")
        S4Vectors::mcols(output) <- details
        output
    } else {
        list(sets = all.sets, details = details)
    }
}
