# library(testthat); library(gesel); source("setup.R"); source("test-fetchAllGenes.R")

test_that("fetchAllGenes works for the local ref", {
    flushMemoryCache(test.config)

    roundtrip <- fetchAllGenes(species, config=test.config)
    expect_identical(colnames(roundtrip), ref.gene.types)
    expect_identical(unclass(roundtrip$foo), ref.genes$foo)
    expect_identical(unclass(roundtrip$bar), ref.genes$bar)
    expect_identical(unclass(roundtrip$whee), ref.genes$whee)

    sub <- fetchAllGenes(species, types = "bar", config=test.config)
    expect_identical(colnames(sub), "bar")
    expect_identical(unclass(roundtrip$bar), ref.genes$bar)

    preloaded <- fetchAllGenes(species, config=test.config)
    expect_identical(roundtrip, preloaded)
})

test_that("fetchAllGenes works for v1", {
    flushMemoryCache(test.config.v1)

    roundtrip <- fetchAllGenes(species, types = ref.gene.types, config=test.config.v1)
    expect_identical(colnames(roundtrip), ref.gene.types)
    expect_identical(unclass(roundtrip$foo), ref.genes$foo)
    expect_identical(unclass(roundtrip$bar), ref.genes$bar)
    expect_identical(unclass(roundtrip$whee), ref.genes$whee)
})

test_that("fetchAllGenes behaves for the remote", {
    flushMemoryCache()

    roundtrip <- fetchAllGenes("9606") 
    expect_gt(nrow(roundtrip), 0)

    expect_true(any(lengths(roundtrip$symbol) > 0L))
    expect_true(any(lengths(roundtrip$ensembl) > 0L))
    expect_true(any(lengths(roundtrip$entrez) > 0L))

    expect_true(all(grepl("^(ENSG[0-9]+|LRG_[0-9]+)$", unlist(roundtrip$ensembl))))
    expect_true(all(grepl("^[0-9]+$", unlist(roundtrip$entrez))))

    sub <- fetchAllGenes("9606", types = c("symbol", "entrez")) 
    expect_identical(sub, roundtrip[,c("symbol", "entrez")])

    preloaded <- fetchAllGenes("9606")
    expect_identical(roundtrip, preloaded)
})
