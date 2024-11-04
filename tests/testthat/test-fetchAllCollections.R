# library(testthat); library(gesel); source("setup.R"); source("test-fetchAllCollections.R")

test_that("fetchAllCollections matches our local ref", {
    payload <- fetchAllCollections("1111", fetch=getDatabaseFile, use.preloaded=FALSE)
    expect_identical(payload, ref.collections)
})

test_that("fetchAllCollections yields a sensible remote ref", {
    test <- fetchAllCollections("9606", use.preloaded=FALSE)
    expect_false(anyNA(test$title))
    expect_false(anyNA(test$description))
    expect_false(anyNA(test$maintainer))
    expect_false(anyNA(test$source))
    expect_identical(diff(test$start), head(test$size, -1L))

    preloaded <- fetchAllCollections("9606")
    expect_identical(test, preloaded)
})
