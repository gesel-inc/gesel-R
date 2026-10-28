# library(testthat); library(gesel); source("setup.R"); source("test-readDatabaseRanges.R")

test_that("readDatabaseRanges works correctly", {
    tmp <- tempfile()
    dir.create(tmp)

    dpath <- file.path(tmp, "foo.txt")
    contents <- "abcdefghijklmnopqrstuvwxyz"
    write(file=dpath, contents)

    # Handles out-of-order starts.
    starts <- c(0, 20, 10, 5)
    ends <- c(6, 23, 15, 14)
    observed <- readDatabaseRanges(tmp, "foo.txt", starts, ends)
    expect_identical(vapply(observed, rawToChar, ""), substring(contents, starts + 1, ends))

    # Handles empty or preceding ends.
    starts <- c(2, 20, 10, 5)
    ends <- c(6, 19, 15, 5)
    observed <- readDatabaseRanges(tmp, "foo.txt", starts, ends)
    expect_identical(vapply(observed, rawToChar, ""), substring(contents, starts + 1, pmax(starts, ends)))
})
