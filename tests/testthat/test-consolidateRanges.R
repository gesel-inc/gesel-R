# library(testthat); library(gesel); source("setup.R"); source("test-consolidateRanges.R")

test_that("consolidateBlockSize works as expected", {
    old <- consolidateBlockSize(123)
    expect_identical(consolidateBlockSize(), 123)

    consolidateBlockSize(old)
    expect_identical(consolidateBlockSize(), old)
})

cr <- function(...) {
    out <- gesel:::consolidate_ranges_by_block(...)
    names(out) <- c("start", "end", "requested")
    out
}

r2b <- gesel:::define_blocks

test_that("consolidateRanges works for simple examples", {
    boundaries <- c(0, 10, 30, 100, 200, 210) # five ranges

    expect_identical(cr(r2b(boundaries, block.size=1), 2:4), list(start=10, end=200, requested=2:4))
    expect_identical(cr(r2b(boundaries, block.size=10), 2:4), list(start=10, end=200, requested=2:4))
    expect_identical(cr(r2b(boundaries, block.size=100), 2:4), list(start=0, end=200, requested=1:4))
    expect_identical(cr(r2b(boundaries, block.size=120), 2:4), list(start=0, end=210, requested=1:5))

    expect_identical(cr(r2b(boundaries, block.size=1), c(1, 3, 5)), list(start=c(0, 30, 200), end=c(10, 100, 210), requested=c(1L,3L,5L)))
    expect_identical(cr(r2b(boundaries, block.size=100), c(1, 3, 5)), list(start=c(0, 200), end=c(100, 210), requested=c(1L,2L,3L,5L)))
    expect_identical(cr(r2b(boundaries, block.size=1000), c(1, 3, 5)), list(start=0, end=210, requested=1:5))

    expect_identical(cr(r2b(boundaries, block.size=1), c(1, 5)), list(start=c(0, 200), end=c(10, 210), requested=c(1L,5L)))
    expect_identical(cr(r2b(boundaries, block.size=100), c(1, 5)), list(start=c(0, 200), end=c(100, 210), requested=c(1:3,5L)))
    expect_identical(cr(r2b(boundaries, block.size=200), c(1, 5)), list(start=0, end=210, requested=1:5))

    # Handles empty sets of ranges/blocks.
    expect_identical(cr(r2b(0:5 * 10, 10), integer(0)), list(start=integer(0), end=integer(0), requested=integer(0)))
    expect_identical(cr(r2b(integer(0), 10), integer(0)), list(start=integer(0), end=integer(0), requested=integer(0)))
})

test_that("consolidateRanges works in harder cases", {
    set.seed(2122)
    ntotal <- 100
    max.size <- 50
    range.sizes <- sample(max.size, ntotal, replace=TRUE)
    boundaries <- cumsum(c(0L, range.sizes))
    mid.pts <- (tail(boundaries, -1) - head(boundaries, -1)) * 0.5

    for (bs in c(0, 5, 10, 20, 50, 100)) {
        blocked <- r2b(boundaries, bs)

        for (n in 1:50) {
            needed <- sample(ntotal, n)
            out <- cr(blocked, needed)
            expect_true(all(needed %in% out$requested))

            # Check consistency between requested and boundaries.
            observed <- expected <- logical(tail(boundaries, 1))
            for (i in out$requested) {
                observed[(boundaries[i] + 1L):boundaries[i+1]] <- TRUE
            }
            for (i in seq_along(out$start)) {
                expected[(out$start[i] + 1L):out$end[i]] <- TRUE
            }
            expect_identical(observed, expected)

            # Check that the same blocks are retrieved.
            expect_identical(
                sort(unique(floor(mid.pts[needed] / bs))),
                sort(unique(floor(mid.pts[out$requested] / bs)))
            )
        }
    }
})

test_that("extracting the ranges works as expected (simple)", {
    parts <- list(as.raw(1:10), as.raw(11:30), as.raw(31:80), as.raw(81:100))
    attr(parts, "retrieved.start") <- c(101, 211, 331, 581)
    attr(parts, "retrieved.end") <- c(110, 230, 380, 600) + 1L

    expect_identical(
        gesel:::extract_ranges_from_consolidated_output(
            parts,
            c(102, 105, 215, 360, 590),
            c(105, 110, 228, 377, 595) + 1L
        ),
        list(
            parts[[1]][2:5],
            parts[[1]][5:10],
            parts[[2]][5:18],
            parts[[3]][30:47],
            parts[[4]][10:15]
        )
    )

    # Any ordering of nested requested ranges. 
    expect_identical(
        gesel:::extract_ranges_from_consolidated_output(
            parts,
            c(102, 105, 105, 215, 360, 360, 590),
            c(105, 108, 110, 228, 377, 370, 595) + 1L
        ),
        list(
            parts[[1]][2:5],
            parts[[1]][5:8],
            parts[[1]][5:10],
            parts[[2]][5:18],
            parts[[3]][30:47],
            parts[[3]][30:40],
            parts[[4]][10:15]
        )
    )
})

test_that("extracting the ranges works as expected (unsorted)", {
    parts <- list(as.raw(1:10), as.raw(11:30), as.raw(31:80), as.raw(81:100))
    attr(parts, "retrieved.start") <- c(201, 111, 331, 81)
    attr(parts, "retrieved.end") <- c(210, 130, 380, 100) + 1L

    expect_identical(
        gesel:::extract_ranges_from_consolidated_output(
            parts,
            c(90, 112, 125, 201, 360),
            c(99, 115, 130, 205, 377) + 1L
        ),
        list(
            parts[[4]][10:19],
            parts[[2]][2:5],
            parts[[2]][15:20],
            parts[[1]][1:5],
            parts[[3]][30:47]
        )
    )
})

test_that("extracting the ranges works as expected (overlapping)", {
    parts <- list(
        as.raw(1:10),
        as.raw(11:30),
        as.raw(21:50), # partial overlap with preceding range.
        as.raw(31:80), # partial overlap with preceding range.
        as.raw(31:60), # nested with tied start, but sorts after its parent.
        as.raw(51:70),
        as.raw(81:90), # nested with tied start, but sorts before its parent.
        as.raw(81:100)
    )
    attr(parts, "retrieved.start") <- c(1, 11, 21, 31, 31, 51, 81, 81)
    attr(parts, "retrieved.end") <- c(10, 30, 50, 80, 60, 70, 90, 100) + 1L

    # Also adding some nested requested ranges.
    expect_identical(
        gesel:::extract_ranges_from_consolidated_output(
            parts,
            c(1, 12, 23, 25, 31, 31, 45, 55, 55, 83, 91, 96),
            c(5, 18, 30, 50, 40, 35, 60, 60, 70, 88, 100, 99) + 1L
        ),
        list(
            as.raw(1:5),
            as.raw(12:18),
            as.raw(23:30),
            as.raw(25:50),
            as.raw(31:40),
            as.raw(31:35),
            as.raw(45:60),
            as.raw(55:60),
            as.raw(55:70),
            as.raw(83:88),
            as.raw(91:100),
            as.raw(96:99)
        )
    )
})

test_that("extracting the ranges works as expected (extreme)", {
    for (num in c(10, 100, 1000)) {
        starts <- sample(1000, num, replace=TRUE) - 1L
        ends <- starts + sample(50, num, replace=TRUE)
        full <- sample(as.raw(1:120), max(ends), replace=TRUE)

        parts <- vector("list", num)
        for (i in seq_len(num)) {
            parts[[i]] <- full[(starts[i] + 1L):ends[i]]
        }
        attr(parts, "retrieved.start") <- starts
        attr(parts, "retrieved.end") <- ends

        rnum <- num * 2
        truth <- vector("list", rnum)
        req.starts <- req.ends <- integer(rnum)
        for (req in seq_len(rnum)) {
            chosen <- sample(num, 1)
            req.start <- (sample(ends[chosen] - starts[chosen], 1) - 1L) + starts[chosen] # -1 so that length is at least 1. 
            req.end <- sample(ends[chosen] - req.start, 1) + req.start
            req.starts[req] <- req.start
            req.ends[req] <- req.end
            truth[[req]] <- full[(req.start + 1L):req.end]
        }

        o <- order(req.starts)
        req.starts <- req.starts[o]
        req.ends <- req.ends[o]
        truth <- truth[o]

        expect_identical(gesel:::extract_ranges_from_consolidated_output(parts, req.starts, req.ends), truth)
    }
})

test_that("extracting the ranges works as expected (fail)", {
    parts <- list(as.raw(1:10), as.raw(41:70), as.raw(81:100))
    attr(parts, "retrieved.start") <- c(1, 41, 81)
    attr(parts, "retrieved.end") <- c(10, 70, 100) + 1L

    expect_error(gesel:::extract_ranges_from_consolidated_output(parts, 20, 30), "failed")
    expect_error(gesel:::extract_ranges_from_consolidated_output(parts, 11, 50), "failed") # partial ovelap at the front of a range.
    expect_error(gesel:::extract_ranges_from_consolidated_output(parts, 50, 81), "failed") # partial overlap behind a range.
    expect_error(gesel:::extract_ranges_from_consolidated_output(parts, 20, 75), "failed") # encloses a range.
})
