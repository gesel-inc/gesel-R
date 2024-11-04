#' Fetch Gesel gene description files 
#'
#' Default function to download Gesel gene description files. 
#'
#' @param name String containing the name of the file, typically of the form \code{<species>_<type>.tsv.gz}, e.g., \code{"9606_symbol.tsv.gz"}.
#' @param url String containing the base URL to the Gesel gene descriptions. 
#' For \code{geneUrl}, this can be \code{NULL} to set it back to the default.
#' @param cache String specifying the path to a cache directory.
#' If \code{NULL}, a cache location is automatically chosen.
#' @param overwrite Logical scalar indicating whether any cached file should be overwritten.
#'
#' @return \code{downloadGeneFile} returns a string containing a local path to the downloaded file.
#'
#' For \code{geneUrl}, if \code{url} is missing, the function returns a string containing the URL to the Gesel gene descriptions.
#' If \code{url} is provided, it instead stores \code{url} as the URL to the indices, and the previous value of \code{url} is invisibly returned.
#' 
#' @details
#' The default gene URL is set to the GitHub releases at \url{https://github.com/LTLA/gesel-feedstock}.
#' This can be altered by setting the \code{GESEL_GENE_URL} environment variable.
#' 
#' On first use of a cached file in an R session, \code{downloadGeneFile} will automatically check for updates at \code{url}. 
#' If the file on the remote has been updated, the new copy will be downloaded to the cache.
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
downloadGeneFile <- function(name, url = geneUrl(), cache = NULL, overwrite = FALSE) {
    download_file(cache, paste0(url, "/", name), overwrite)
}

#' @export
#' @rdname downloadGeneFile
geneUrl <- function(url) {
    if (missing(url)) {
        url <- downloadGeneUrl.env$url
        if (is.null(url)) {
            url <- Sys.getenv("GESEL_GENE_URL", "https://github.com/LTLA/gesel-feedstock/releases/download/genes-v1.0.0")
        }
        url
    } else {
        previous <- downloadGeneUrl.env$url
        downloadGeneUrl.env$url <- url
        invisible(NULL)
    }
}

downloadGeneUrl.env <- new.env()
downloadGeneUrl.env$url <- NULL
