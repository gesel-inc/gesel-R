# library(testthat); library(gesel); source("setup.R"); source("test-fetchGenesForAllSets.R")

flushMemoryCache()

test_that("fetchGenesForAllSets matches our local ref", {
    payload <- fetchGenesForAllSets("1111", fetch=getDatabaseFile)
    expect_identical(payload, lapply(ref.set.membership, function(x) sort(unique(x))))

    preloaded <- fetchGenesForAllSets("1111", fetch=getDatabaseFile)
    expect_identical(payload, preloaded)
})

test_that("fetchGenesForAllSets yields a sensible remote ref", {
    test <- fetchGenesForAllSets("9606")
    expect_true(any(lengths(test) > 0L))

    set.idx <- unlist(test)
    expect_true(all(set.idx > 0L))
    expect_true(all(set.idx <= nrow(fetchAllGenes("9606", type="ensembl"))))

    expect_false(any(vapply(test, is.unsorted, FALSE)))
    expect_true(all(vapply(test, anyDuplicated, 0L) == 0L))

    preloaded <- fetchGenesForAllSets("9606")
    expect_identical(test, preloaded)
})
