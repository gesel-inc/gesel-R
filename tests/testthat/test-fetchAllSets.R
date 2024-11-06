# library(testthat); library(gesel); source("setup.R"); source("test-fetchAllSets.R")

flushMemoryCache()

test_that("fetchAllSets matches our local ref", {
    payload <- fetchAllSets("1111", config=test.config)
    expect_identical(payload[,c("name", "description", "collection", "size")], ref.set.info)

    preloaded <- fetchAllSets("1111", config=test.config)
    expect_identical(payload, preloaded)
})

test_that("fetchAllSets yields a sensible remote ref", {
    test <- fetchAllSets("9606")
    expect_false(anyNA(test$name))
    expect_false(anyNA(test$description))
    expect_true(all(test$size > 0))

    coll.info <- fetchAllCollections("9606")
    expect_true(!is.unsorted(test$collection))
    expect_identical(tabulate(test$collection, nbins=nrow(coll.info)), coll.info$size)
    expect_identical(test$number, sequence(coll.info$size))

    preloaded <- fetchAllSets("9606")
    expect_identical(test, preloaded)
})
