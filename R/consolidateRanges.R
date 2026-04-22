consolidate.env <- new.env()
consolidate.env$block.size <- 10000L

#' Block size for consolidation
#'
#' Get or set the block size for consolidating HTTP range requests.
#'
#' @param block.size Integer specifying the block size in bytes.
#' Larger sizes reduce the number of requests at teh cost of increasing the size of each request.
#'
#' @return
#' If \code{block.size=NULL}, the current block size is returned.
#'
#' If \code{block.size} is provided, it is used to set the block size, and the previous value is returned invisibly. 
#'
#' @details
#' Each file is split up into blocks of size approximately equal to \code{consolidateBlockSize()}.
#' When performing a range request, all ranges in the same block will be retrieved. 
#' This consolidates near-adjacent ranges into a single request, reducing the number of requests at the cost of increasing the size of each request.
#'
#' All ranges associated with a block will be cached in memory, even those that were not directly requested.
#' Subsequent function calls can then quickly retrieve ranges from this cache instead of making a new HTTP request. 
#' Downloading and caching the entire block also ensures that the same bytes will never be requested from the server twice in the same session.
#'
#' @author Aaron Lun
#' @examples
#' consolidateBlockSize()
#' old <- consolidateBlockSize(500)
#' consolidateBlockSize()
#' consolidateBlockSize(old)
#'
#' @export
consolidateBlockSize <- function(block.size = NULL) {
    previous <- consolidate.env$block.size
    if (is.null(block.size)) {
        previous
    } else {
        consolidate.env$block.size <- block.size
        invisible(previous)
    }
}

#' @importFrom utils head tail
ranges_to_blocks <- function(boundaries, block.size) {
    range.starts <- head(boundaries, -1)
    mid <- range.starts + (tail(boundaries, -1) - range.starts) * 0.5 # use midpoints to place blocks.
    ids <- floor(mid / block.size)
    ids <- as.integer(factor(ids))

    by.id <- split(seq_along(ids), ids)
    by.id <- unname(by.id)
    first.in.block <- vapply(by.id, head, n=1, FUN.VALUE=as(0, typeof(boundaries)))
    block.boundaries <- c(boundaries[first.in.block], tail(boundaries, 1))

    list(
        bounds = block.boundaries,
        to.block = ids,
        to.range = by.id
    )
}

consolidate_ranges <- function(boundaries, block.info, needed) {
    requested.blocks <- block.info$to.block[needed]
    requested.blocks <- sort(unique(requested.blocks))
    if (length(requested.blocks) == 0L) {
        return(list(
            start = integer(0),
            end = integer(0),
            requested = integer(0)
        ))
    }

    is.run.start <- diff(requested.blocks) > 1
    run.start <- requested.blocks[c(TRUE, is.run.start)]
    run.end <- requested.blocks[c(is.run.start, TRUE)]

    list(
        start = block.info$bounds[run.start],
        end = block.info$bounds[run.end + 1L],
        requested = unlist(block.info$to.range[requested.blocks])
    )
}
