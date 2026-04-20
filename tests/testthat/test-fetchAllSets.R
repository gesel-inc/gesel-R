# library(testthat); library(gesel); source("setup.R"); source("test-fetchAllSets.R")

flushMemoryCache()

test_that("fetchAllSets matches our local ref", {
    full.set.info <- do.call(rbind, ref.set.info)
    full.set.info$size <- unlist(lapply(ref.set.membership, lengths))
    full.set.info$collection <- rep(seq_along(ref.set.info), vapply(ref.set.info, nrow, 0L)) 
    full.set.info$number <- unlist(lapply(ref.set.info, function(y) seq_len(nrow(y))))

    payload <- fetchAllSets("1111", config=test.config)
    expect_identical(payload, full.set.info)

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
    expect_identical(test$number + coll.info$start[test$collection] - 1L, seq_len(nrow(test)))

    preloaded <- fetchAllSets("9606")
    expect_identical(test, preloaded)
})
