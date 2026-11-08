# library(testthat); library(gesel); source("setup.R"); source("test-readDatabaseRanges.R")

test_that("readDatabaseRanges works correctly", {
    tmp <- tempfile()
    dir.create(tmp)

    dpath <- file.path(tmp, "foo.txt")
    contents <- "abcdefghijklmnopqrstuvwxyz"
    write(file=dpath, contents)

    starts <- c(0, 7, 10, 20)
    ends <- c(6, 10, 15, 24)
    observed <- readDatabaseRanges(tmp, "foo.txt", starts, ends)
    expect_identical(vapply(observed, rawToChar, ""), substring(contents, starts + 1, ends))
    expect_identical(attr(observed, 'retrieved.start'), starts)
    expect_identical(attr(observed, 'retrieved.end'), ends)

    empty <- readDatabaseRanges(tmp, "foo.txt", integer(), integer())
    expect_identical(length(empty), 0L)
    expect_identical(attr(empty, 'retrieved.start'), integer())
    expect_identical(attr(empty, 'retrieved.end'), integer())
})
