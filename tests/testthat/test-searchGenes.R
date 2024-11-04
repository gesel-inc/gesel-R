# library(testthat); library(gesel); source("setup.R"); source("test-searchGenes.R")

test_that("searchGenes works for the local ref", {
    chosen <- c(
        unlist(ref.genes$foo)[1],
        unlist(ref.genes$bar)[1],
        unlist(ref.genes$WHEE)[1]
    )

    out <- searchGenes(species, chosen, types=ref.gene.types, more.args=list(fetch=getGeneFile))
    expect_identical(length(out), length(chosen))
    expect_true(all(lengths(out) > 0L))

    for (i in out[[1]]) {
        expect_true(chosen[1] %in% ref.genes$foo[[i]])
    }
    for (i in out[[2]]) {
        expect_true(chosen[2] %in% ref.genes$bar[[i]])
    }
    for (i in out[[3]]) {
        expect_true(chosen[3] %in% ref.genes$WHEE[[i]])
    }

    # Works after ignoring case.
    lout <- searchGenes(species, tolower(chosen), types=ref.gene.types, ignore.case=TRUE, more.args=list(fetch=getGeneFile))
    for (i in lout[[3]]) {
        expect_true(chosen[3] %in% ref.genes$WHEE[[i]])
    }
})

test_that("searchGenes works for the remote", {
    chosen <- c("SNAP25", "neurod6", "ENSG00000139618", "10023")
    out <- searchGenes("9606", chosen)
    expect_identical(length(out), length(chosen))
    expect_true(all(lengths(out) > 0L))

    all.genes <- fetchAllGenes("9606")
    for (i in out[[1]]) {
        expect_true("SNAP25" %in% all.genes$symbol[[i]])
    }
    for (i in out[[2]]) {
        expect_true("NEUROD6" %in% all.genes$symbol[[i]])
    }
    for (i in out[[3]]) {
        expect_true("ENSG00000139618" %in% all.genes$ensembl[[i]])
    }
    for (i in out[[4]]) {
        expect_true("10023" %in% all.genes$entrez[[i]])
    }
})
