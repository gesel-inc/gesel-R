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
#' # Mocking up some information.
#' collections <- data.frame(
#'     title=c("FOO", "BAR"),
#'     description=c("I am a foo", "I am a bar"),
#'     maintainer=c("Aaron", "Aaron"),
#'     source=c("https://foo", "https://bar")
#' )
#'
#' set.info <- list(
#'     data.frame(
#'         name=sprintf("FOO_%i", seq_len(20)),
#'         description=sprintf("this is FOO %i", seq_len(20))
#'     ),
#'     data.frame(
#'         name=sprintf("BAR_%i", seq_len(50)),
#'         description=sprintf("this is BAR %i", seq_len(50))
#'     )
#' )
#'
#' # Mocking up the gene sets.
#' num.genes <- 10000
#' set.membership <- list(
#'     lapply(seq_len(nrow(set.info[[1]])), function(i) {
#'         sample(num.genes, sample(500, 1))
#'     }),
#'     lapply(seq_len(nrow(set.info[[2]])), function(i) {
#'         sample(num.genes, sample(200, 1))
#'     })
#' )
#'
#' # Now making the database files.
#' output <- tempfile()
#' dir.create(output)
#' prepareDatabaseFiles(
#'     "1234",
#'     collections, 
#'     set.info, 
#'     set.membership,
#'     num.genes,
#'     output
#' )
#'
#' # Finally, validating them.
#' validateDatabaseFiles(output, "1234", num.genes)
#' 
#' @export
#' @importFrom Rcpp sourceCpp
#' @useDynLib gesel
validateDatabaseFiles <- function(path, species, num.genes) {
    validate_database_files(file.path(path, paste0(species, "_")), num.genes)
    invisible(NULL)
}
