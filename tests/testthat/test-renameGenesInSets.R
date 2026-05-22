# library(testthat); library(gesel); source("setup.R"); source("test-renameGenesInSets.R")

test_that("renameGenesInSets works locally", {
    all.sets <- fetchGenesForAllSets("1111", config=test.config)
    renamed <- renameGenesInSets("1111", all.sets, "foo", config=test.config)

    expected <- all.sets
    for (i in seq_along(expected)) {
        expected[[i]] <- unique(as.character(unlist(ref.genes$foo[expected[[i]]])))
    }

    expect_identical(renamed, expected)
    expect_true(all(vapply(renamed, anyDuplicated, 0L) == 0L))
})

test_that("renameGenesInSets works remotely", {
    all.genes <- fetchAllGenes("10090", type="symbol")

    # Check that it works properly if there's empty sets.
    made.up <- list(1:10, integer(0), 2:50 * 2, integer(0), 50:100 * 10, integer(0))
    renamed <- renameGenesInSets("10090", made.up, "symbol")

    expected <- made.up
    for (i in seq_along(expected)) {
        expected[[i]] <- unique(as.character(unlist(all.genes$symbol[expected[[i]]])))
    }

    expect_identical(renamed, expected)
    expect_true(all(vapply(renamed, anyDuplicated, 0L) == 0L))
})
