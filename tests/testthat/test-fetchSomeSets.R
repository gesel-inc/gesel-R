# library(testthat); library(gesel); source("setup.R"); source("test-fetchSomeSets.R")

test_that("fetchSomeSets matches our local ref", {
    everything <- fetchAllSets("1111", config=test.config)
    flushMemoryCache(test.config)

    payload <- fetchSomeSets("1111", 10:20, config=test.config)
    expected <- everything[10:20,]
    rownames(expected) <- NULL
    expect_identical(expected, payload)

    payload <- fetchSomeSets("1111", 40:60, config=test.config)
    expected <- everything[40:60,]
    rownames(expected) <- NULL
    expect_identical(expected, payload)

    # Works with partial caching.
    chosen <- seq_len(nrow(everything))
    payload <- fetchSomeSets("1111", chosen, config=test.config)
    expect_identical(everything, payload)

    # Works with full caching.
    payload <- fetchSomeSets("1111", 30:15, config=test.config)
    expected <- everything[30:15,]
    rownames(expected) <- NULL
    expect_identical(expected, payload)

    # Works for sizes.
    sizes <- fetchSetSizes("1111", config=test.config)
    expect_identical(sizes, everything$size)

    # Works with pre-loaded.
    everything <- fetchAllSets("1111", config=test.config)
    preloaded <- fetchSomeSets("1111", 10:50, config=test.config)
    expected <- everything[10:50,]
    rownames(expected) <- NULL
    expect_identical(expected, preloaded)

    sizes <- fetchSetSizes("1111", config=test.config)
    expect_identical(sizes, everything$size)
})

test_that("fetchSomeSets yields a sensible remote ref", {
    everything <- fetchAllSets("9606")
    flushMemoryCache()

    set.seed(99)
    chosen <- sample(nrow(everything), 20L) 
    chosen <- union(chosen, c(1L, nrow(everything)))

    test <- fetchSomeSets("9606", chosen)
    expected <- everything[chosen,]
    rownames(expected) <- NULL
    expect_identical(expected, test)

    expect_identical(fetchSetSizes("9606"), everything$size)

    # Works with full caching.
    preloaded <- fetchSomeSets("9606", chosen)
    expect_identical(test, preloaded)

    # Works with partial caching.
    extras <- sample(setdiff(seq_len(nrow(everything)), chosen), 10) # using a random sample to hopefully hit some different blocks.
    reloaded.plus <- fetchSomeSets("9606", c(chosen, extras))
    expected <- everything[c(chosen, extras),]
    rownames(expected) <- NULL
    expect_identical(reloaded.plus, expected)

    # Works with pre-loading.
    invisible(fetchAllSets("9606"))
    preloaded <- fetchSomeSets("9606", chosen)
    expect_identical(test, preloaded)
})
