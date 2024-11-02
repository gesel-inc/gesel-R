#' Search for genes
#'
#' Search for genes by converting gene identifiers to gene indices.
#' 
#' @inheritParams fetchAllGenes
#' @param genes Character vector of gene identifiers of any type specified in \code{types}.
#'
#' @return List of length equal to \code{genes}.
#' Each entry is an integer vector of gene indices, specifying the genes in \code{\link{fetchAllGenes}} that match to the corresponding entry of \code{genes}.
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
        mappings <- mapGenesByIdentifier(species, t, ignore.case=ignore.case, more.args=more.args)
        m <- match(genes, names(mappings))
        keep <- which(!is.na(m))
        output[keep] <- mapply(c, output[keep], mappings[m[keep]], SIMPLIFY=FALSE)
    }

    output
}
