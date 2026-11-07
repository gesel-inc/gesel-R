# library(testthat); library(gesel); source("setup.R"); source("test-renameGenesInSets.R")

test_that("renameGenesInSets works locally", {
    all.sets <- fetchGenesForAllSets("1111", config=test.config)
    renamed <- renameGenesInSets("1111", all.sets, "alpha", config=test.config)

    expected <- all.sets
    for (i in seq_along(expected)) {
        expected[[i]] <- unique(as.character(unlist(ref.genes$alpha[expected[[i]]])))
    }
    expect_identical(renamed, expected)

    # Removes all duplicates correctly.
    all.sets2 <- all.sets
    for (i in seq_along(all.sets)) {
        all.sets2[[i]] <- c(all.sets2[[i]], sample(all.sets2[[i]], 100, replace=TRUE))
    }
    renamed2 <- renameGenesInSets("1111", all.sets2, "alpha", config=test.config)
    expect_identical(renamed, renamed2)

    # Handles empty sets.
    all.sets2 <- c(list(integer(0)), all.sets, list(integer(0)))
    renamed2 <- renameGenesInSets("1111", all.sets2, "alpha", config=test.config)
    expect_identical(renamed2, c(list(character(0)), renamed, list(character(0))))
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
})
