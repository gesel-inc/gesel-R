# library(testthat); library(gesel); source("setup.R"); source("test-fetchSetsForSomeGenes.R")

test_that("fetchSetsForSomeGenes matches our local ref", {
    everything <- fetchSetsForAllGenes("1111", fetch=getDatabaseFile)
    chosen <- seq_along(everything)

    payload <- fetchSetsForSomeGenes("1111", chosen, fetch.file=getDatabaseFile, fetch.range=getDatabaseRanges, use.preloaded=FALSE)
    expect_identical(everything, payload)

    preloaded <- fetchSetsForSomeGenes("1111", chosen, fetch.file=getDatabaseFile, fetch.range=getDatabaseRanges)
    expect_identical(everything, preloaded)
})

test_that("fetchSetsForSomeGenes yields a sensible remote ref", {
    everything <- fetchSetsForAllGenes("9606")

    set.seed(99)
    chosen <- sample(length(everything), 20L) 
    chosen <- union(chosen, c(1L, length(everything)))

    test <- fetchSetsForSomeGenes("9606", chosen, use.preloaded=FALSE)
    expect_identical(everything[chosen], test)

    preloaded <- fetchSetsForSomeGenes("9606", chosen)
    expect_identical(test, preloaded)
})
