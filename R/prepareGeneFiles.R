#' Prepare the Gesel gene files
#'
#' Prepare Gesel gene files containing the gene identifiers.
#'
#' @param genes Named list of lists.
#' Each inner list corresponds to an identifier type (e.g., Ensembl) and is named accordingly.
#' Each inner list should be of length equal to the total number of genes.
#' Each entry of the inner list corresponds to a gene and should be a character vector containing identifiers of the specified type for that gene.
#' An entry may be an empty character vector is no identifiers are available for a gene.
#'
#' Alternatively, a data frame where each row corresponds to a gene and each column is a nested list of identifiers of a particular type,
#' see the output of \code{\link{fetchAllGenes}} for details.
#' @param species String specifying the species in the form of its NCBI taxonomy ID.
#' @param path String containing the path to a directory in which to create the gene files.
#' @param validate Boolean indicating whether to run \code{\link{validateGeneFiles}} on the newly created files.
#' @param version String specifying the version of the Gesel gene file specification to use for saving \code{genes}.
#' 
#' @return Several files are produced inside \code{path} with the \code{<species>_} prefix.
#' \code{NULL} is invisibly returned.
#'
#' @seealso
#' \code{\link{prepareDatabaseFiles}}, to create Gesel database files containing the gene set information.
#'
#' @author Aaron Lun
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
#' list.files(tmp)
#' 
#' @export
prepareGeneFiles <- function(species, genes, path = ".", validate = TRUE, version = c("0.2.0", "0.1.0")) {
    version <- match.arg(version)
    if (version == "0.1.0") {
        sep <- "_"
        to.validate <- names(genes) # validation can't be trusted to auto-determine types if we mix it with the database files.
    } else {
        writeLines(names(genes), con = file.path(path, paste0(species, "_gene-types.tsv")))
        writeLines(version, con = file.path(path, paste0(species, "_gene-version.tsv")))
        sep <- "_gene-type-"
        to.validate <- NULL # validation will automatically determine types from the manifest.
    }

    genes <- as.list(genes)
    for (i in names(genes)) {
        fname <- file.path(path, paste0(species, sep, i, ".tsv.gz"))
        dump <- vapply(genes[[i]], paste, collapse="\t", FUN.VALUE="")
        handle <- gzfile(fname, open="wb")
        writeLines(dump, con=handle)
        close(handle)
    }

    if (validate) {
        validateGeneFiles(path, species, types = to.validate)
    }

    invisible(NULL)
}
