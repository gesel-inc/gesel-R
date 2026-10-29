# library(testthat); library(gesel); source("setup.R"); source("test-fetchAllCollections.R")

test_that("fetchAllCollections matches our local ref", {
    flushMemoryCache(test.config)

    expected <- ref.collections
    expected$start <- cumsum(c(1L, head(lengths(ref.set.membership), -1L)))
    expected$size <- lengths(ref.set.membership)

    payload <- fetchAllCollections("1111", config=test.config)
    expect_identical(payload, expected)

    # Fetch from cache.
    preloaded <- fetchAllCollections("1111", config=test.config)
    expect_identical(preloaded, expected)
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

    # Fetch from cache.
    preloaded <- fetchAllCollections("9606")
    expect_identical(test, preloaded)
})
