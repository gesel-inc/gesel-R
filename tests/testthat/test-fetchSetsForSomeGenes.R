# library(testthat); library(gesel); source("setup.R"); source("test-fetchSetsForSomeGenes.R")

test_that("fetchSetsForSomeGenes matches our local ref", {
    everything <- fetchSetsForAllGenes("1111", fetch=getDatabaseFile)
    flushMemoryCache()

    evens <- seq(from=2, to=length(everything), by=2)
    payload <- fetchSetsForSomeGenes("1111", evens, fetch.file=getDatabaseFile, fetch.range=getDatabaseRanges)
    expect_identical(everything[evens], payload)

    odds <- seq(from=1, to=length(everything), by=2)
    payload <- fetchSetsForSomeGenes("1111", odds, fetch.file=getDatabaseFile, fetch.range=getDatabaseRanges)
    expect_identical(everything[odds], payload)

    together <- seq_along(everything)
    payload <- fetchSetsForSomeGenes("1111", together, fetch.file=getDatabaseFile, fetch.range=getDatabaseRanges)
    expect_identical(everything, payload)

    # Works with pre-loading.
    everything <- fetchSetsForAllGenes("1111", fetch=getDatabaseFile)
    preloaded <- fetchSetsForSomeGenes("1111", together, fetch.file=getDatabaseFile, fetch.range=getDatabaseRanges)
    expect_identical(everything, preloaded)
})

test_that("fetchSetsForSomeGenes yields a sensible remote ref", {
    everything <- fetchSetsForAllGenes("9606")
    flushMemoryCache()

    set.seed(99)
    chosen <- sample(length(everything), 20L) 
    chosen <- union(chosen, c(1L, length(everything)))

    test <- fetchSetsForSomeGenes("9606", chosen)
    expect_identical(everything[chosen], test)

    # Works with pre-loading.
    preloaded <- fetchSetsForSomeGenes("9606", chosen)
    expect_identical(test, preloaded)
})
