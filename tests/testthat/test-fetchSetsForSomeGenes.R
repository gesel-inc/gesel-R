# library(testthat); library(gesel); source("setup.R"); source("test-fetchSetsForSomeGenes.R")

test_that("fetchSetsForSomeGenes matches our local ref", {
    everything <- fetchSetsForAllGenes("1111", config=test.config)
    flushMemoryCache()

    sub <- seq(from=2, to=length(everything), by=3)
    payload <- fetchSetsForSomeGenes("1111", sub, config=test.config)
    expect_identical(everything[sub], payload)

    sub <- seq(from=1, to=length(everything), by=3)
    payload <- fetchSetsForSomeGenes("1111", sub, config=test.config)
    expect_identical(everything[sub], payload)

    # Works with partial caching.
    together <- seq_along(everything)
    payload <- fetchSetsForSomeGenes("1111", together, config=test.config)
    expect_identical(everything, payload)

    # Works with full caching.
    sub <- seq(from=3, to=length(everything), by=3)
    payload <- fetchSetsForSomeGenes("1111", sub, config=test.config)
    expect_identical(everything[sub], payload)

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

    # Works with partial caching.
    preloaded <- fetchSetsForSomeGenes("9606", chosen)
    expect_identical(test, preloaded)

    extras <- head(setdiff(seq_along(everything), chosen), 10)
    reloaded.plus <- fetchSetsForSomeGenes("9606", c(chosen, extras))
    plus <- fetchSetsForSomeGenes("9606", extras)
    expect_identical(reloaded.plus, c(test, plus))

    # Works with pre-loading.
    invisible(fetchSetsForAllGenes("9606"))
    preloaded <- fetchSetsForSomeGenes("9606", chosen)
    expect_identical(test, preloaded)
    expect_identical(effectiveNumberOfGenes("9606"), expected.genes)
})
