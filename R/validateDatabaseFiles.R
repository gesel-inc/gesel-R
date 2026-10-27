#' Validate Gesel database files
#'
#' Validate Gesel database files against the specification at \url{https://github.com/gesel-inc/gesel-spec}.
#'
#' @param species String specifying the species in the form of its NCBI taxonomy ID.
#' @param path String containing the path to a directory containing the database files. 
#' @param num.genes Integer specifying the total number of genes available for this species.
#' This can be obtained as the return value of \code{\link{validateGeneFiles}}.
#'
#' @return \code{NULL}, invisibly.
#' Invalid formatting of the files will cause an error to be raised.
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
