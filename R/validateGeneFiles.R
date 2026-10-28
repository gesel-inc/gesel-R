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
#' genes <- list(
#'     ensembl = list("ENSG1", c("ENSG2", "ENSG3"), character(0), "ENSG4"),
#'     entrez = list("1", character(0), c("2", "3", "4"), c("5", "6")),
#'     foobar = list("malat1", "neat1", "Gm1234", "LINC0000001")
#' )
#'
#' tmp <- tempfile()
#' dir.create(tmp)
#' prepareGeneFiles("1234", genes, tmp)
#'
#' validateGeneFiles(tmp, "1234")
#' 
#' @export
#' @importFrom Rcpp sourceCpp
#' @useDynLib gesel
validateGeneFiles <- function(path, species, types = NULL) {
    validate_gene_files(file.path(path, paste0(species, "_")), types)
}
