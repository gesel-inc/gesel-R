# library(testthat); library(gesel); source("setup.R"); source("test-fetchSetsForAllGenes.R")

flushMemoryCache()

test_that("fetchSetsForAllGenes matches our local ref", {
    gene.ids <- unlist(ref.set.membership)
    set.ids <- rep(seq_along(ref.set.membership), lengths(ref.set.membership))
    mapping <- split(set.ids, factor(gene.ids, seq_len(ref.num.genes)))
    mapping <- unname(lapply(mapping, function(x) sort(unique(x))))

    payload <- fetchSetsForAllGenes("1111", fetch=getDatabaseFile)
    expect_identical(payload, mapping)
})

test_that("fetchSetsForAllGenes yields a sensible remote ref", {
    test <- fetchSetsForAllGenes("9606")
    expect_true(any(lengths(test) > 0L))

    set.idx <- unlist(test)
    expect_true(all(set.idx > 0L))
    expect_true(all(set.idx <= nrow(fetchAllSets("9606"))))

    expect_false(any(vapply(test, is.unsorted, FALSE)))
    expect_true(all(vapply(test, anyDuplicated, 0L) == 0L))

    preloaded <- fetchSetsForAllGenes("9606")
    expect_identical(test, preloaded)
})
