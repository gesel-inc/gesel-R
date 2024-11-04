#' Search for genes
#'
#' Search for genes by converting gene identifiers to gene indices.
#' 
#' @inheritParams fetchAllGenes
#' @param genes Character vector of gene names of any type specified in \code{types}.
#' @param ignore.case Logical scalar indicating whether case should be ignored.
#' @param more.args Named list of further arguments to pass to \code{\link{fetchAllGenes}}.
#'
#' @return List of length equal to \code{genes}.
#' Each entry is an integer vector of gene indices that refer to rows of the data frame returned by \code{\link{fetchAllGenes}};
#' these rows represent the genes that match to the corresponding entry of \code{genes}.
#'
#' @author Aaron Lun
#' @examples
#' searchGenes("9606", c("SNAP25", "NEUROD6", "ENSG00000139618"))
#' 
#' @export
searchGenes <- function(species, genes, types = NULL, ignore.case = TRUE, more.args = list()) {
    if (is.null(types)) {
        types <- c("entrez", "ensembl", "symbol")
    }

    if (ignore.case) {
        genes <- tolower(genes)
    }

    output <- vector("list", length(genes))
    for (t in types) {
        mappings <- mapGenesByName(species, t, ignore.case=ignore.case, more.args=more.args)
        m <- match(genes, names(mappings))
        keep <- which(!is.na(m))
        output[keep] <- mapply(c, output[keep], mappings[m[keep]], SIMPLIFY=FALSE)
    }

    output
}
