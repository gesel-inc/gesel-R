#' Fetch Gesel gene description files 
#'
#' Default function to download Gesel gene description files. 
#'
#' @param name String containing the name of the file, typically of the form \code{<species>_<type>.tsv.gz}, e.g., \code{"9606_symbol.tsv.gz"}.
#' @param url String containing the base URL to the Gesel gene descriptions. 
#' @param cache String specifying the path to a cache directory.
#' @param overwrite Boolean indicating whether any cached file should be overwritten.
#'
#' @return \code{downloadGeneFile} returns a string containing a local path to the downloaded file.
#'
#' For \code{geneUrl}, if \code{url=NULL}, the function returns a string containing the URL to the Gesel gene descriptions.
#' If \code{url} is provided, it instead stores \code{url} as the URL to the indices, and the previous value of \code{url} is invisibly returned.
#' 
#' @details
#' The gene URL defaults to the GitHub releases at \url{https://github.com/LTLA/gesel-feedstock}.
#' This can be altered by setting the \code{GESEL_GENE_URL} environment variable prior to the first call to this function.
#'
#' @author Aaron Lun
#'
#' @examples
#' # Download file.
#' downloadGeneFile("9606_symbol.tsv.gz")
#' 
#' # Altering the default gene URL.
#' geneUrl()
#' old <- geneUrl("https://foo")
#' geneUrl()
#' geneUrl(old)
#' 
#' @export
downloadGeneFile <- function(name, url = geneUrl(), cache = cacheDirectory(), overwrite = FALSE) {
    download_file(cache, paste0(url, "/", name), overwrite)
}

#' @export
#' @rdname downloadGeneFile
geneUrl <- function(url = NULL) {
    previous <- downloadGeneUrl.env$url
    if (is.null(previous)) {
        previous <- Sys.getenv("GESEL_GENE_URL", "https://github.com/LTLA/gesel-feedstock/releases/download/genes-v1.0.0")
        downloadGeneUrl.env$url <- previous
    }
    if (is.null(url)) {
        previous 
    } else {
        downloadGeneUrl.env$url <- url
        invisible(previous)
    }
}

downloadGeneUrl.env <- new.env()
downloadGeneUrl.env$url <- NULL
