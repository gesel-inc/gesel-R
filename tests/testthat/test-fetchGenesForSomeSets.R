# library(testthat); library(gesel); source("setup.R"); source("test-fetchGenesForSomeSets.R")

test_that("fetchGenesForSomeSets matches our local ref", {
    everything <- fetchGenesForAllSets("1111", config=test.config)
    flushMemoryCache()

    sub <- seq(from=2, to=length(everything), by=3)
    payload <- fetchGenesForSomeSets("1111", sub, config=test.config) 
    expect_identical(everything[sub], payload)

    sub <- seq(from=1, to=length(everything), by=3)
    payload <- fetchGenesForSomeSets("1111", sub, config=test.config)
    expect_identical(everything[sub], payload)

    # Works with partial caching.
    together <- seq_along(everything)
    payload <- fetchGenesForSomeSets("1111", together, config=test.config)
    expect_identical(everything, payload)

    # Works with full caching.
    sub <- seq(from=3, to=length(everything), by=3)
    payload <- fetchGenesForSomeSets("1111", sub, config=test.config)
    expect_identical(everything[sub], payload)

    # Works with pre-loading.
    everything <- fetchGenesForAllSets("1111", config=test.config)
    preloaded <- fetchGenesForSomeSets("1111", together, config=test.config)
    expect_identical(everything, preloaded)
})

test_that("fetchGenesForSomeSets yields a sensible remote ref", {
    everything <- fetchGenesForAllSets("9606")
    flushMemoryCache()

    set.seed(99)
    chosen <- sample(length(everything), 20L) 
    chosen <- union(chosen, c(1L, length(everything)))

    test <- fetchGenesForSomeSets("9606", chosen)
    expect_identical(everything[chosen], test)

    # Works with full caching.
    preloaded <- fetchGenesForSomeSets("9606", chosen)
    expect_identical(test, preloaded)

    # Works with partial caching.
    extras <- head(setdiff(seq_along(everything), chosen), 10)
    reloaded.plus <- fetchGenesForSomeSets("9606", c(chosen, extras))
    plus <- fetchGenesForSomeSets("9606", extras)
    expect_identical(reloaded.plus, c(test, plus))

    # Works with pre-loading.
    invisible(fetchGenesForAllSets("9606"))
    preloaded <- fetchGenesForSomeSets("9606", chosen)
    expect_identical(test, preloaded)
})
