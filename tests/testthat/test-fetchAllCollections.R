# library(testthat); library(gesel); source("setup.R"); source("test-fetchAllCollections.R")

test_that("fetchAllCollections matches our local ref", {
    flushMemoryCache(test.config)

    payload <- fetchAllCollections("1111", config=test.config)
    expect_identical(payload, ref.collections)

    preloaded <- fetchAllCollections("1111", config=test.config)
    expect_identical(payload, preloaded)
})

test_that("fetchAllCollections yields a sensible remote ref", {
    flushMemoryCache()

    test <- fetchAllCollections("9606")
    expect_gt(nrow(test), 0)

    expect_false(anyNA(test$title))
    expect_false(anyNA(test$description))
    expect_false(anyNA(test$maintainer))
    expect_false(anyNA(test$source))
    expect_identical(diff(test$start), head(test$size, -1L))

    preloaded <- fetchAllCollections("9606")
    expect_identical(test, preloaded)
})
