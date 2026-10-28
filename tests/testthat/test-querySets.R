# library(testthat); library(gesel); source("setup.R"); source("test-querySets.R")

test_that("querySets works in basic local tests", {
    genes <- c(sprintf("CHARLIE_%s", 1:50), sprintf("alpha_%s", 51:100), sprintf("Bravo_%s", 101:200))

    out <- querySets("1111", genes, types = c("CHARLIE", "alpha", "Bravo"), config = test.config)
    all.sets <- fetchAllSets("1111", config = test.config)
    expect_identical(all.sets$name[out$set], out$name)
    all.coll <- fetchAllCollections("1111", config = test.config)
    expect_identical(all.coll$title[all.sets$collection[out$set]], out$collection)
    expect_type(out$pvalue, "double")

    # Works if the full names are requested.
    loaded <- fetchGenesForSomeSets("1111", out$set, config = test.config)
    loaded.alpha <- renameGenesInSets("1111", loaded, "alpha", config = test.config)
    loaded.Bravo <- renameGenesInSets("1111", loaded, "Bravo", config = test.config)
    loaded.CHARLIE <- renameGenesInSets("1111", loaded, "CHARLIE", config = test.config)

    xout <- querySets("1111", genes, types = c("CHARLIE", "alpha", "Bravo"), counts.only = FALSE, config = test.config)
    for (i in seq_along(loaded)) {
        expected <- sort(intersect(c(loaded.alpha[[i]], loaded.Bravo[[i]], loaded.CHARLIE[[i]]), genes))
        expect_identical(expected, sort(xout$genes[[i]]))
    }

    # Intersects properly with descriptions.
    fout <- querySets("1111", genes, types = c("CHARLIE", "alpha", "Bravo"), text = "FOO", config = test.config)
    expect_true(all(fout$set %in% out$set))
    expect_true(all(grepl("FOO", fout$name) | grepl("FOO", fout$description)))
    expect_type(fout$pvalue, "double")

    # Handles descriptions by themselves.
    tout <- querySets("1111", text = "FOO", config = test.config)
    expect_true(all(grepl("FOO", tout$name) | grepl("FOO", tout$description)))
    expect_null(tout$pvalue)

    expect_error(querySets("1111", config = test.config), "must be provided")
})

test_that("querySets works remotely", {
    out <- querySets(
        species = "9606",
        genes = c("tead1", "tead2", "tead3", "tead4"),
        text = "transcription"
    )
    expect_gte(nrow(out), 0L)
    expect_true(all(grepl("transcription", out$name, ignore.case=TRUE) | grepl("transcription", out$description, ignore.case=TRUE)))

    chosen2 <- c("SNAP25", "neurod4", "neurod6")
    out2 <- querySets(
        species = "9606",
        genes = chosen2,
        text = "neuro*",
        counts.only = FALSE
    )
    expect_gte(nrow(out2), 0L)
    expect_true(all(grepl("neuro", out2$name, ignore.case=TRUE) | grepl("neuro", out2$description, ignore.case=TRUE)))
    expect_true(all(unlist(out2$genes) %in% chosen2))
})
