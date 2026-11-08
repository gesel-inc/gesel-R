consolidate.env <- new.env()
consolidate.env$block.size <- 10000L

#' Block size for consolidation
#'
#' Get or set the block size for consolidating HTTP range requests.
#'
#' @param block.size Integer specifying the block size in bytes.
#' Larger sizes reduce the number of requests at the cost of increasing the size of each request.
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
#' Setting the block size to a value of 1 indicates that no consolidation is to be performed, i.e., each range is requested as-is.
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

#' @importFrom utils head
define_blocks <- function(range.boundaries, block.size) {
    range.mid <- head(range.boundaries, -1) + diff(range.boundaries) / 2 # use midpoints to allocate ranges into blocks.
    block.ids <- floor(range.mid / block.size)
    block.ids <- as.integer(factor(block.ids)) # force all IDs to be consecutive. 

    if (length(block.ids)) {
        first.range.in.block <- c(1L, which(diff(block.ids) != 0L) + 1L)
        block.boundaries <- range.boundaries[c(first.range.in.block, length(range.boundaries))]
    } else {
        block.boundaries <- integer(0)
    }

    ranges.per.block <- split(seq_along(block.ids), block.ids)
    ranges.per.block <- unname(ranges.per.block)

    list(
        block.boundaries = block.boundaries,
        range.to.block = block.ids,
        block.to.range = ranges.per.block
    )
}

consolidate_ranges_by_block <- function(block.info, needed.ranges) {
    requested.blocks <- block.info$range.to.block[needed.ranges]
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
        consolidated.start = block.info$block.boundaries[run.start],
        consolidated.end = block.info$block.boundaries[run.end + 1L],
        requested.ranges = unlist(block.info$block.to.range[requested.blocks])
    )
}

fetch_consolidated_ranges <- function(config, fname, range.boundaries, block.info, needed.ranges) {
    consolidated <- consolidate_ranges_by_block(block.info, needed.ranges)
    consolidated.parts <- fetch_ranges(config, fname, consolidated$consolidated.start, consolidated$consolidated.end)

    refined <- refine_ranges(
        consolidated.parts,
        consolidated$consolidated.start,
        consolidated$consolidated.end,
        range.boundaries[consolidated$requested.ranges],
        range.boundaries[consolidated$requested.ranges + 1L] - 1L # omit the newline.
    )

    list(requested = consolidated$requested.ranges, contents = refined)
}
