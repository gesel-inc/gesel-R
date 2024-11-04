# library(testthat); library(gesel); source("setup.R"); source("test-fetchGenesForSomeSets.R")

test_that("fetchGenesForSomeSets matches our local ref", {
    everything <- fetchGenesForAllSets("1111", fetch=getDatabaseFile)
    chosen <- seq_along(everything)

    payload <- fetchGenesForSomeSets("1111", chosen, fetch.file=getDatabaseFile, fetch.range=getDatabaseRanges, use.preloaded=FALSE)
    expect_identical(everything, payload)

    preloaded <- fetchGenesForSomeSets("1111", chosen, fetch.file=getDatabaseFile, fetch.range=getDatabaseRanges)
    expect_identical(everything, preloaded)
})

test_that("fetchGenesForSomeSets yields a sensible remote ref", {
    everything <- fetchGenesForAllSets("9606")

    set.seed(99)
    chosen <- sample(length(everything), 20L) 
    chosen <- union(chosen, c(1L, length(everything)))

    test <- fetchGenesForSomeSets("9606", chosen, use.preloaded=FALSE)
    expect_identical(everything[chosen], test)

    preloaded <- fetchGenesForSomeSets("9606", chosen)
    expect_identical(test, preloaded)
})
