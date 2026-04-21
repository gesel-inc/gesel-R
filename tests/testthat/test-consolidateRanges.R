# library(testthat); library(gesel); source("setup.R"); source("test-consolidateRanges.R")

test_that("consolidateMaxGap works as expected", {
    old <- consolidateMaxGap(0.1)
    expect_identical(consolidateMaxGap(), 0.1)

    consolidateMaxGap(old)
    expect_identical(consolidateMaxGap(), old)
})

test_that("consolidateRanges works for simple examples", {
    boundaries <- c(0, 10, 30, 100, 200, 210) # five ranges

    expect_identical(consolidateRanges(boundaries, 2:4), list(start=10, end=200, requested=2:4))
    expect_identical(consolidateRanges(boundaries, 2:4, max.gap=0), list(start=10, end=200, requested=2:4)) # consecutive so it doesn't matter.
    expect_identical(consolidateRanges(boundaries, 2:4, max.gap=Inf), list(start=10, end=200, requested=2:4)) # consecutive so it doesn't matter.

    expect_identical(consolidateRanges(boundaries, c(1, 3, 5)), list(start=0, end=210, requested=1:5))
    expect_identical(consolidateRanges(boundaries, c(1, 3, 5), max.gap=0), list(start=c(0, 30, 200), end=c(10, 100, 210), requested=c(1L,3L,5L)))
    expect_identical(consolidateRanges(boundaries, c(1, 3, 5), max.gap=50), list(start=c(0, 200), end=c(100, 210), requested=c(1L,2L,3L,5L)))
    expect_identical(consolidateRanges(boundaries, c(1, 3, 5), max.gap=Inf), list(start=0, end=210, requested=1:5))

    expect_identical(consolidateRanges(boundaries, c(1, 5), max.gap=0), list(start=c(0, 200), end=c(10, 210), requested=c(1L,5L)))
    expect_identical(consolidateRanges(boundaries, c(1, 5), max.gap=189), list(start=c(0, 200), end=c(10, 210), requested=c(1L,5L)))
    expect_identical(consolidateRanges(boundaries, c(1, 5), max.gap=190), list(start=0, end=210, requested=1:5))
    expect_identical(consolidateRanges(boundaries, c(1, 5), max.gap=Inf), list(start=0, end=210, requested=1:5))
})

test_that("consolidateRanges works in harder cases", {
    set.seed(2122)
    ntotal <- 100
    max.size <- 50
    range.sizes <- sample(max.size, ntotal, replace=TRUE)
    boundaries <- cumsum(c(0L, range.sizes))

    for (n in 1:50) {
        needed <- sample(ntotal, n)
        sorted <- sort(needed)

        for (mg in c(0, 5, 10, 20, 50, 100)) {
            out <- consolidateRanges(boundaries, needed, max.gap=mg)
            expect_true(all(needed %in% out$requested))
            expect_identical(range(needed), range(out$requested))

            observed <- expected <- logical(tail(boundaries, 1) - 1L)
            for (i in out$requested) {
                observed[boundaries[i]:(boundaries[i+1] - 1L)] <- TRUE
            }
            for (i in seq_along(out$start)) {
                expected[out$start[i]:(out$end[i] - 1L)] <- TRUE
            }
            expect_identical(observed, expected)

            closest <- findInterval(out$requested, sorted)
            expect_true(all(boundaries[out$requested] - boundaries[sorted[closest] + 1] <= mg))
            expect_true(all(boundaries[out$requested] >= boundaries[sorted[closest]])) # sanity check.
        }
    }
})

test_that("consolidateRanges works in edge cases", {
    boundaries <- 1:5 * 10
    expect_identical(consolidateRanges(boundaries, integer(0)), list(start=integer(0), end=integer(0), requested=integer(0)))

    boundaries <- rep(0, 10)
    expect_identical(consolidateRanges(boundaries, c(1, 5, 9)), list(start=0, end=0, requested=1:9))

    boundaries <- c(rep(0, 5), rep(5, 4))
    expect_identical(consolidateRanges(boundaries, c(2, 4, 6, 8), max.gap=1), list(start=c(0, 5), end=c(0, 5), requested=c(2:4, 6:8)))
})
