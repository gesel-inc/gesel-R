# library(testthat); library(gesel); source("test-cacheDirectory.R")

test_that("cacheDirectory works correctly", {
    old <- cacheDirectory()
    cacheDirectory("/tmp/foo/bar")
    expect_identical(cacheDirectory(), "/tmp/foo/bar")
    cacheDirectory(old)
    expect_identical(cacheDirectory(), old)
})
