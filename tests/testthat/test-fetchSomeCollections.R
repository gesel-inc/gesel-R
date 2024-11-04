# library(testthat); library(gesel); source("setup.R"); source("test-fetchSomeCollections.R")

test_that("fetchSomeCollections matches our local ref", {
    everything <- fetchAllCollections("1111", fetch=getDatabaseFile)
    chosen <- seq_len(nrow(everything))

    payload <- fetchSomeCollections("1111", chosen, fetch.file=getDatabaseFile, fetch.range=getDatabaseRanges, use.preloaded=FALSE)
    expect_identical(everything, payload)

    preloaded <- fetchSomeCollections("1111", chosen, fetch.file=getDatabaseFile, fetch.range=getDatabaseRanges)
    expect_identical(everything, preloaded)
})

test_that("fetchSomeCollections yields a sensible remote ref", {
    everything <- fetchAllCollections("9606")

    set.seed(99)
    chosen <- sample(nrow(everything), 5L) 
    chosen <- union(chosen, c(1L, nrow(everything)))

    test <- fetchSomeCollections("9606", chosen, use.preloaded=FALSE)
    expected <- everything[chosen,]
    rownames(expected) <- NULL
    expect_identical(expected, test)

    preloaded <- fetchSomeCollections("9606", chosen)
    expect_identical(test, preloaded)
})
