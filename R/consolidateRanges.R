consolidate.env <- new.env()
consolidate.env$max.gap <- 10000L

#' Maximum gap for consolidation
#'
#' Get or set the maximum gap between ranges of interest.
#' This determines how aggressively \code{\link{consolidateRanges}} will merge near-adjacent ranges together.
#'
#' @param max.gap Integer specifying the maximum size of the gap, in bytes.
#'
#' @return
#' If \code{max.gap=NULL}, the current maximum gap is returned.
#'
#' If \code{max.gap} is provided, it is used to set the maximum gap, and the previous maximum is returned invisibly. 
#'
#' @author Aaron Lun
#' @examples
#' consolidateMaxGap()
#' old <- consolidateMaxGap(500)
#' consolidateMaxGap()
#' consolidateMaxGap(old)
#'
#' @export
consolidateMaxGap <- function(max.gap = NULL) {
    previous <- consolidate.env$max.gap
    if (is.null(max.gap)) {
        previous
    } else {
        consolidate.env$max.gap <- max.gap
        invisible(previous)
    }
}

#' Consolidate near-adjacent ranges
#'
#' Consolidate near-adjacent byte ranges in HTTP range requests.
#' This reduces the number of individual requests at the cost of having larger requests.
#'
#' @param boundaries Integer vector containing the zero-indexed boundaries of the byte ranges.
#' This should have length equal to the number of ranges plus 1, where the \code{i}-th range is defined as \code{[boundaries[i], boundaries[i+1])}.
#' @param needed Integer vector specifying the ranges of interest.
#' Each entry should be an index into \code{boundaries}. 
#' @param max.gap Number between 0 and 1 inclusive, specifying the maximum gap between the ranges of interest.
#' Larger values will produce a smaller number of larger ranges.
#'
#' @return List containing:
#' \itemize{
#' \item \code{start}, an integer vector containing the starts of the consolidated ranges.
#' \item \code{end}, an integer vector of length equal to \code{start}.
#' This contains the ends of the consolidated ranges.
#' \item \code{requested}, an integer vector of the individual ranges that will be requested after consolidation.
#' This will be a superset of \code{needed}, where a range is listed here if and only if it is enclosed within one of the consolidated ranges.
#' }
#'
#' @author Aaron Lun
#' @examples
#' boundaries <- c(0, 10, 30, 100, 200, 210) # five ranges
#' consolidateRanges(boundaries, c(2, 3, 4))
#' consolidateRanges(boundaries, c(1, 3, 5))
#' consolidateRanges(boundaries, c(1, 5))
#' consolidateRanges(boundaries, c(1, 5), max.gap=100)
#'
#' @export
#' @importFrom utils head tail
consolidateRanges <- function(boundaries, needed, max.gap = consolidateMaxGap()) {
    needed <- sort(needed)
    starts <- boundaries[needed]
    ends <- boundaries[needed + 1]

    gaps <- tail(starts, -1) - head(ends, -1)
    gaps.to.keep <- which(gaps > max.gap)
    gap.left <- needed[gaps.to.keep]
    gap.right <- needed[gaps.to.keep + 1]

    o2 <- order(gap.left)
    run.start <- c(head(needed, 1), gap.right[o2]) # RHS of the gap is the start of the run.
    run.end <- c(gap.left[o2], tail(needed, 1))
    
    output.needed <- integer(0)
    output.starts <- integer(0)
    output.ends <- integer(0)

    for (i in seq_along(run.start)) {
        rstart <- run.start[i]
        rend <- run.end[i]
        output.starts <- c(output.starts, boundaries[rstart])
        output.ends <- c(output.ends, boundaries[rend + 1])
        output.needed <- c(output.needed, seq(rstart, rend)) # rstart <= rend is guaranteed due to the nature of the gap.
    }

    list(
        start = output.starts,
        end = output.ends,
        requested = output.needed
    )
}
