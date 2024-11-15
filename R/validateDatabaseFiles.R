#' Validate Gesel database files
#'
#' Validate Gesel database and gene mapping files against the specification at \url{https://github.com/gesel-inc/gesel-spec}.
#'
#' @param species String specifying the species in the form of its NCBI taxonomy ID.
#' @param path String containing the path to a directory containing the database files or gene mapping files, for \code{validateDatabaseFiles} and \code{validateGeneFiles} respectively.
#' @param num.genes Integer scalar specifying the total number of genes available for this species.
#' @param types Character vector specifying the types of gene names to validate, e.g.,\code{"symbol"}, \code{"entrez"}, or \code{"ensembl"},
#' If \code{NULL}, all detected files for \code{species} in \code{path} are checked.
#'
#' @return \code{validateDatabaseFiles} returns \code{NULL} invisibly.
#'
#' \code{validateGeneFiles} returns the number of genes, to be used as \code{num.genes}.
#'
#' In both functions, invalid formatting will cause an error to be raised.
#'
#' @author Aaron Lun
#'
#' @examples
#' example(prepareDatabaseFiles, echo=FALSE)
#' validateDatabaseFiles(output, "9606", num.genes)
#' 
#' @export
#' @importFrom Rcpp sourceCpp
#' @useDynLib gesel
validateDatabaseFiles <- function(path, species, num.genes) {
    validate_database_files(file.path(path, paste0(species, "_")), num.genes)
    invisible(NULL)
}

#' @export
#' @rdname validateDatabaseFiles
validateGeneFiles <- function(path, species, types=NULL) {
    validate_gene_files(file.path(path, paste0(species, "_")), types)
}
