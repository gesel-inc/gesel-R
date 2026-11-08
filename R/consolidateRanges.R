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

    consolidated.start <- block.info$block.boundaries[run.start]
    consolidated.end <- block.info$block.boundaries[run.end + 1L]
    # There's at least one range in each block, and at least one newline in each range, so a block can't be empty.
    stopifnot(all(consolidated.start != consolidated.end))

    list(
        consolidated.start = consolidated.start,
        consolidated.end = consolidated.end,
        requested.ranges = unlist(block.info$block.to.range[requested.blocks])
    )
}

extract_ranges_from_consolidated_output <- function(consolidated.output, requested.start, requested.end) {
    consolidated.output.starts <- attr(consolidated.output, "retrieved.start")
    consolidated.output.ends <- attr(consolidated.output, "retrieved.end")
    stopifnot(length(consolidated.output) == length(consolidated.output.starts))
    stopifnot(length(consolidated.output) == length(consolidated.output.ends))
    stopifnot(all(lengths(consolidated.output) == consolidated.output.ends - consolidated.output.starts))

    # extract_sorted_byte_ranges() only cares that the start positions are sorted.
    # nested intervals with tied starts are just ignored.
    # So, there's no need to sort by both start and end positions.
    if (is.unsorted(consolidated.output.starts)) {
        o <- order(consolidated.output.starts)
        consolidated.output <- consolidated.output[o]
        consolidated.output.starts <- consolidated.output.starts[o]
        consolidated.output.ends <- consolidated.output.ends[o]
    }

    stopifnot(!is.unsorted(requested.start))
    extract_sorted_byte_ranges(consolidated.output, consolidated.output.starts, consolidated.output.ends, requested.start, requested.end)
}

fetch_consolidated_ranges <- function(config, fname, range.boundaries, block.info, needed.ranges) {
    consolidated <- consolidate_ranges_by_block(block.info, needed.ranges)
    consolidated.output <- fetch_ranges(config, fname, consolidated$consolidated.start, consolidated$consolidated.end)

    # For back-compatibility.
    if (is.null(attr(consolidated.output, "retrieved.start"))) {
        attr(consolidated.output, "retrieved.start") <- consolidated$consolidated.start
        attr(consolidated.output, "retrieved.end") <- consolidated$consolidated.end
    }

    requested.ranges <- consolidated$requested.ranges
    requested.start <- range.boundaries[requested.ranges]
    requested.end <- range.boundaries[requested.ranges + 1L] - 1L # omit the newline.
    refined <- extract_ranges_from_consolidated_output(consolidated.output, requested.start, requested.end)
    list(requested = requested.ranges, contents = refined)
}
