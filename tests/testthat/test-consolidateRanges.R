# library(testthat); library(gesel); source("setup.R"); source("test-consolidateRanges.R")

test_that("consolidateBlockSize works as expected", {
    old <- consolidateBlockSize(123)
    expect_identical(consolidateBlockSize(), 123)

    consolidateBlockSize(old)
    expect_identical(consolidateBlockSize(), old)
})

cr <- gesel:::consolidate_ranges
r2b <- gesel:::ranges_to_blocks

test_that("consolidateRanges works for simple examples", {
    boundaries <- c(0, 10, 30, 100, 200, 210) # five ranges

    expect_identical(cr(boundaries, r2b(boundaries, block.size=1), 2:4), list(start=10, end=200, requested=2:4))
    expect_identical(cr(boundaries, r2b(boundaries, block.size=10), 2:4), list(start=10, end=200, requested=2:4))
    expect_identical(cr(boundaries, r2b(boundaries, block.size=100), 2:4), list(start=0, end=200, requested=1:4))
    expect_identical(cr(boundaries, r2b(boundaries, block.size=120), 2:4), list(start=0, end=210, requested=1:5))

    expect_identical(cr(boundaries, r2b(boundaries, block.size=1), c(1, 3, 5)), list(start=c(0, 30, 200), end=c(10, 100, 210), requested=c(1L,3L,5L)))
    expect_identical(cr(boundaries, r2b(boundaries, block.size=100), c(1, 3, 5)), list(start=c(0, 200), end=c(100, 210), requested=c(1L,2L,3L,5L)))
    expect_identical(cr(boundaries, r2b(boundaries, block.size=1000), c(1, 3, 5)), list(start=0, end=210, requested=1:5))

    expect_identical(cr(boundaries, r2b(boundaries, block.size=1), c(1, 5)), list(start=c(0, 200), end=c(10, 210), requested=c(1L,5L)))
    expect_identical(cr(boundaries, r2b(boundaries, block.size=100), c(1, 5)), list(start=c(0, 200), end=c(100, 210), requested=c(1:3,5L)))
    expect_identical(cr(boundaries, r2b(boundaries, block.size=200), c(1, 5)), list(start=0, end=210, requested=1:5))
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
            out <- cr(boundaries, blocked, needed)
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

test_that("consolidateRanges works in edge cases", {
    boundaries <- 0:5 * 10
    expect_identical(cr(boundaries, r2b(boundaries, 10), integer(0)), list(start=integer(0), end=integer(0), requested=integer(0)))

    boundaries <- rep(0, 10)
    expect_identical(cr(boundaries, r2b(boundaries, 10), c(1, 5, 9)), list(start=0, end=0, requested=1:9))

    boundaries <- c(rep(0, 5), rep(5, 4))
    expect_identical(cr(boundaries, r2b(boundaries, 1), c(2, 4, 6, 8)), list(start=c(0, 5), end=c(0, 5), requested=c(1:4, 6:8)))
})
