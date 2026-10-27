#' Validate Gesel gene annotation files
#'
#' Validate Gesel gene annotation files against the specification at \url{https://github.com/gesel-inc/gesel-spec}.
#'
#' @param species String specifying the species in the form of its NCBI taxonomy ID.
#' @param path String containing the path to a directory containing the gene annotation files.
#' @param types Character vector specifying the types of gene identifiers to validate, e.g., \code{"symbol"}, \code{"entrez"}, or \code{"ensembl"},
#' If \code{NULL}, all detected files for \code{species} in \code{path} are checked.
#'
#' @return The number of genes, to be used as \code{num.genes} in \code{\link{validateDatabaseFiles}}.
#' Invalid formatting of the files will cause an error to be raised.
#'
#' @author Aaron Lun
#'
#' @examples
#' example(prepareGeneFiles, echo=FALSE)
#' validateGeneFiles(tmp, "1234")
#' 
#' @export
#' @importFrom Rcpp sourceCpp
#' @useDynLib gesel
validateGeneFiles <- function(path, species, types = NULL) {
    validate_gene_files(file.path(path, paste0(species, "_")), types)
}
