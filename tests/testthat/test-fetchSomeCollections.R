# library(testthat); library(gesel); source("setup.R"); source("test-fetchSomeCollections.R")

test_that("fetchSomeCollections matches our local ref", {
    everything <- fetchAllCollections("1111", config=test.config)
    flushMemoryCache(test.config)

    payload <- fetchSomeCollections("1111", 1, config=test.config)
    expect_identical(everything[1,], payload)

    payload <- fetchSomeCollections("1111", 2, config=test.config)
    expected <- everything[2,]
    rownames(expected) <- NULL
    expect_identical(expected, payload)

    # Works with partial caching.
    chosen <- seq_len(nrow(everything))
    payload <- fetchSomeCollections("1111", chosen, config=test.config)
    expect_identical(everything, payload)

    # Works with full caching
    payload <- fetchSomeCollections("1111", 3:2, config=test.config)
    expected <- everything[3:2,]
    rownames(expected) <- NULL
    expect_identical(expected, payload)

    # Works for sizes.
    sizes <- fetchCollectionSizes("1111", config=test.config)
    expect_identical(sizes, everything$size)

    # Works with pre-loaded.
    everything <- fetchAllCollections("1111", config=test.config)
    preloaded <- fetchSomeCollections("1111", c(3,1,2), config=test.config)
    expected <- everything[c(3,1,2),]
    rownames(expected) <- NULL
    expect_identical(expected, preloaded)

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

    # Works with partial caching.
    preloaded <- fetchSomeCollections("9606", chosen)
    expect_identical(test, preloaded)

    extras <- head(setdiff(seq_len(nrow(everything)), chosen), 10)
    reloaded.plus <- fetchSomeCollections("9606", c(chosen, extras))
    plus <- fetchSomeCollections("9606", extras) 
    expect_identical(reloaded.plus, rbind(test, plus))

    # Works with pre-loading.
    invisible(fetchAllCollections("9606"))
    preloaded <- fetchSomeCollections("9606", chosen)
    expect_identical(test, preloaded)
    expect_identical(fetchCollectionSizes("9606"), everything$size)
})
