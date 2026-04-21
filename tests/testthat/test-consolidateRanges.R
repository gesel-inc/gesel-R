# library(testthat); library(gesel); source("setup.R"); source("test-consolidateRanges.R")

test_that("consolidateMaxUnused works as expected", {
    old <- consolidateMaxUnused(0.1)
    expect_identical(consolidateMaxUnused(), 0.1)

    consolidateMaxUnused(old)
    expect_identical(consolidateMaxUnused(), old)
})

test_that("consolidateRanges works for simple examples", {
    boundaries <- c(0, 10, 30, 100, 200, 210) # five ranges

    expect_identical(consolidateRanges(boundaries, 2:4), list(start=10, end=200, requested=2:4))
    expect_identical(consolidateRanges(boundaries, 2:4, max.unused=0), list(start=10, end=200, requested=2:4)) # consecutive so it doesn't matter.
    expect_identical(consolidateRanges(boundaries, 2:4, max.unused=1), list(start=10, end=200, requested=2:4)) # consecutive so it doesn't matter.

    expect_identical(consolidateRanges(boundaries, c(1, 3, 5)), list(start=0, end=210, requested=1:5))
    expect_identical(consolidateRanges(boundaries, c(1, 3, 5), max.unused=0), list(start=c(0, 30, 200), end=c(10, 100, 210), requested=c(1L,3L,5L)))
    expect_identical(consolidateRanges(boundaries, c(1, 3, 5), max.unused=0.5), list(start=c(0, 200), end=c(100, 210), requested=c(1L,2L,3L,5L)))
    expect_identical(consolidateRanges(boundaries, c(1, 3, 5), max.unused=1), list(start=0, end=210, requested=1:5))

    expect_identical(consolidateRanges(boundaries, c(1, 5)), list(start=c(0, 200), end=c(10, 210), requested=c(1L,5L)))
    expect_identical(consolidateRanges(boundaries, c(1, 5), max.unused=0.99), list(start=0, end=210, requested=1:5))
})

test_that("consolidateRanges works in harder cases", {
    set.seed(2122)
    ntotal <- 100
    max.size <- 50
    range.sizes <- sample(max.size, ntotal, replace=TRUE)
    boundaries <- cumsum(c(0L, range.sizes))

    for (n in 1:50) {
        needed <- sample(ntotal, n)
        for (mu in c(0, 0.1, 0.2, 0.5, 1)) {
            out <- consolidateRanges(boundaries, needed, max.unused=mu)
            expect_true(all(needed %in% out$requested))

            required <- sum(range.sizes[needed])
            obtained <- sum(out$end - out$start)
            expect_gte(required / obtained, 1-mu)

            observed <- logical(tail(boundaries, 1))
            for (i in out$requested) {
                observed[boundaries[i]:boundaries[i+1]] <- TRUE
            }

            expected <- logical(tail(boundaries, 1))
            for (i in seq_along(out$start)) {
                expected[out$start[i]:out$end[i]] <- TRUE
            }

            expect_identical(observed, expected)
        }
    }
})

test_that("consolidateRanges works in edge cases", {
    boundaries <- 1:5 * 10
    expect_identical(consolidateRanges(boundaries, integer(0)), list(start=integer(0), end=integer(0), requested=integer(0)))

    boundaries <- rep(0, 10)
    expect_identical(consolidateRanges(boundaries, c(1, 5, 9)), list(start=0, end=0, requested=1:9))

    boundaries <- c(rep(0, 5), rep(5, 4))
    expect_identical(consolidateRanges(boundaries, c(2, 4, 6, 8)), list(start=c(0, 5), end=c(0, 5), requested=c(2:4, 6:8)))
})
