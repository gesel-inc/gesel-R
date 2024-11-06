# library(testthat); library(gesel); source("setup.R"); source("test-fetchSetsForSomeGenes.R")

test_that("fetchSetsForSomeGenes matches our local ref", {
    everything <- fetchSetsForAllGenes("1111", config=test.config)
    flushMemoryCache()

    evens <- seq(from=2, to=length(everything), by=2)
    payload <- fetchSetsForSomeGenes("1111", evens, config=test.config)
    expect_identical(everything[evens], payload)

    odds <- seq(from=1, to=length(everything), by=2)
    payload <- fetchSetsForSomeGenes("1111", odds, config=test.config)
    expect_identical(everything[odds], payload)

    together <- seq_along(everything)
    payload <- fetchSetsForSomeGenes("1111", together, config=test.config)
    expect_identical(everything, payload)

    # Works with pre-loading.
    everything <- fetchSetsForAllGenes("1111", config=test.config)
    preloaded <- fetchSetsForSomeGenes("1111", together, config=test.config)
    expect_identical(everything, preloaded)
})

test_that("effectiveNumberOfGenes works a local ref", {
    everything <- fetchSetsForAllGenes("1111", config=test.config)
    flushMemoryCache()

    num <- effectiveNumberOfGenes("1111", config=test.config)
    expect_identical(num, sum(lengths(everything) > 0L))

    # Works with pre-loaded.
    everything <- fetchSetsForAllGenes("1111", config=test.config)
    tnum <- effectiveNumberOfGenes("1111", config=test.config)
    expect_identical(num, tnum)
})

test_that("fetchSetsForSomeGenes yields a sensible remote ref", {
    everything <- fetchSetsForAllGenes("9606")
    flushMemoryCache()

    set.seed(99)
    chosen <- sample(length(everything), 20L) 
    chosen <- union(chosen, c(1L, length(everything)))

    test <- fetchSetsForSomeGenes("9606", chosen)
    expect_identical(everything[chosen], test)

    expected.genes <- sum(lengths(everything) > 0L)
    expect_identical(effectiveNumberOfGenes("9606"), expected.genes)

    # Works with pre-loading.
    preloaded <- fetchSetsForSomeGenes("9606", chosen)
    expect_identical(test, preloaded)

    expect_identical(effectiveNumberOfGenes("9606"), expected.genes)
})
