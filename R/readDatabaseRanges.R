#' Read byte ranges from a Gesel database file
#'
#' Read any number of byte ranges from a Gesel database file on the local filesystem.
#'
#' @param dir String containing the path to a directory containing all Gesel database files.
#' @inheritParams downloadDatabaseRanges
#'
#' @return List of length equal to \code{length(start)}.
#' Each entry is a raw vector representing the contents of the corresponding byte range.
#'
#' @examples
#' path <- downloadDatabaseFile("9606_set2gene.tsv")
#' readDatabaseRanges(dirname(path), basename(path), 0L, 100L)
#' readDatabaseRanges(dirname(path), basename(path), c(10, 100, 1000), c(20, 150, 1100))
#'
#' @seealso
#' \code{\link{downloadDatabaseRanges}}, for a remote counterpart to this function that requests the byte ranges from a server.
#' 
#' @export
readDatabaseRanges <- function(dir, name, start, end) {
    handle <- file(file.path(dir, name), open="rb")
    on.exit(close(handle))

    o <- order(start)
    output <- rep(list(raw()), length(start))
    for (i in o) {
        seek(handle, where=start[i]) # where= seems to be zero-based in terms of its position.
        output[[i]] <- readBin(handle, what=raw(), n=end[i] - start[i])
    }

    output
}
