# library(testthat); library(gesel); source("setup.R"); source("test-fetchAllGenes.R")

test_that("fetchAllGenes works for the local ref", {
    roundtrip <- fetchAllGenes(species, types=ref.gene.types, fetch=getGeneFile)
    expect_identical(unclass(roundtrip$foo), ref.genes$foo)
    expect_identical(unclass(roundtrip$bar), ref.genes$bar)
    expect_identical(unclass(roundtrip$whee), ref.genes$whee)

    preloaded <- fetchAllGenes(species, types=ref.gene.types, fetch=getGeneFile)
    expect_identical(roundtrip, preloaded)
})

test_that("fetchAllGenes behaves for the remote", {
    roundtrip <- fetchAllGenes("9606", use.preloaded=TRUE)
    expect_gt(nrow(roundtrip), 0)

    expect_true(any(lengths(roundtrip$symbol) > 0L))
    expect_true(any(lengths(roundtrip$ensembl) > 0L))
    expect_true(any(lengths(roundtrip$entrez) > 0L))

    expect_true(all(grepl("^(ENSG[0-9]+|LRG_[0-9]+)$", unlist(roundtrip$ensembl))))
    expect_true(all(grepl("^[0-9]+$", unlist(roundtrip$entrez))))

    preloaded <- fetchAllGenes("9606")
    expect_identical(roundtrip, preloaded)
})
