# library(testthat); library(gesel); source("setup.R"); source("test-fetchSomeCollections.R")

test_that("fetchSomeCollections matches our local ref", {
    everything <- fetchAllCollections("1111", config=test.config)
    flushMemoryCache()

    payload <- fetchSomeCollections("1111", 1, config=test.config)
    expect_identical(everything[1,], payload)

    payload <- fetchSomeCollections("1111", 2, config=test.config)
    expected <- everything[2,]
    rownames(expected) <- NULL
    expect_identical(expected, payload)

    chosen <- seq_len(nrow(everything))
    payload <- fetchSomeCollections("1111", chosen, config=test.config)
    expect_identical(everything, payload)

    # Works for sizes.
    sizes <- fetchCollectionSizes("1111", config=test.config)
    expect_identical(sizes, everything$size)

    # Works with pre-loaded.
    everything <- fetchAllCollections("1111", config=test.config)
    preloaded <- fetchSomeCollections("1111", chosen, config=test.config)
    expect_identical(everything, preloaded)

    sizes <- fetchCollectionSizes("1111", config=test.config)
    expect_identical(sizes, everything$size)
})

test_that("fetchSomeCollections yields a sensible remote ref", {
    everything <- fetchAllCollections("9606")
    flushMemoryCache()

    set.seed(99)
    chosen <- sample(nrow(everything), 5L) 
    chosen <- union(chosen, c(1L, nrow(everything)))

    test <- fetchSomeCollections("9606", chosen)
    expected <- everything[chosen,]
    rownames(expected) <- NULL
    expect_identical(expected, test)

    expect_identical(fetchCollectionSizes("9606"), everything$size)

    # Works with pre-loading.
    preloaded <- fetchSomeCollections("9606", chosen)
    expect_identical(test, preloaded)
})
