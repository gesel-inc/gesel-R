# library(testthat); library(gesel); source("setup.R"); source("test-fetchSomeSets.R")

test_that("fetchSomeSets matches our local ref", {
    everything <- fetchAllSets("1111", fetch=getDatabaseFile)
    chosen <- seq_len(nrow(everything))

    payload <- fetchSomeSets("1111", chosen, fetch.file=getDatabaseFile, fetch.range=getDatabaseRanges, use.preloaded=FALSE)
    expect_identical(everything, payload)

    preloaded <- fetchSomeSets("1111", chosen, fetch.file=getDatabaseFile, fetch.range=getDatabaseRanges)
    expect_identical(payload, preloaded)
})

test_that("fetchSomeSets yields a sensible remote ref", {
    everything <- fetchAllSets("9606")

    set.seed(99)
    chosen <- sample(nrow(everything), 20L) 
    chosen <- union(chosen, c(1L, nrow(everything)))

    test <- fetchSomeSets("9606", chosen, use.preloaded=FALSE)
    expected <- everything[chosen,]
    rownames(expected) <- NULL
    expect_identical(expected, test)

    preloaded <- fetchSomeSets("9606", chosen)
    expect_identical(test, preloaded)
})
