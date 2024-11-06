# library(testthat); library(gesel); source("setup.R"); source("test-fetchGenesForSomeSets.R")

test_that("fetchGenesForSomeSets matches our local ref", {
    everything <- fetchGenesForAllSets("1111", config=test.config)
    flushMemoryCache()

    evens <- seq(from=2, to=length(everything), by=2)
    payload <- fetchGenesForSomeSets("1111", evens, config=test.config) 
    expect_identical(everything[evens], payload)

    odds <- seq(from=1, to=length(everything), by=2)
    payload <- fetchGenesForSomeSets("1111", odds, config=test.config)
    expect_identical(everything[odds], payload)

    together <- seq_along(everything)
    payload <- fetchGenesForSomeSets("1111", together, config=test.config)
    expect_identical(everything, payload)

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

    # Works with pre-loading.
    preloaded <- fetchGenesForSomeSets("9606", chosen)
    expect_identical(test, preloaded)
})
