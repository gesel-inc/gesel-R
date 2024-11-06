# library(testthat); library(gesel); source("setup.R"); source("test-mapGenesByName.R")

flushMemoryCache()

test_that("mapGenesByName works for the local ref", {
    mapping <- mapGenesByName(species, type="foo", config=test.config) 

    set.seed(9)
    current.names <- unlist(ref.genes$foo)
    current.idx <- rep(seq_along(ref.genes$foo), lengths(ref.genes$foo))
    chosen <- sample(unique(current.names), 10)
    for (x in chosen) {
        current <- current.idx[current.names == x]
        expect_identical(sort(mapping[[x]]), sort(current))
    }

    # Works when ignoring the case.
    lmapping <- mapGenesByName(species, type="WHEE", ignore.case=TRUE, config=test.config)

    current.names <- unlist(ref.genes$WHEE)
    current.idx <- rep(seq_along(ref.genes$WHEE), lengths(ref.genes$WHEE))
    chosen <- tolower(sample(unique(current.names), 10))
    for (x in chosen) {
        current <- current.idx[tolower(current.names) == x]
        expect_identical(sort(lmapping[[x]]), sort(current))
    }
})

test_that("mapGenesByName works for the remote ref", {
    genes <- fetchAllGenes("9606")

    mapping <- mapGenesByName("9606", type="symbol") 
    for (f in mapping[["SNAP25"]]) {
        expect_true("SNAP25" %in% genes$symbol[[f]])
    }

    emapping <- mapGenesByName("9606", type="ensembl") 
    for (f in mapping[["ENSG00000000003"]]) {
        expect_true("ENSG00000000003" %in% genes$ensembl[[f]])
    }

    # Works when ignoring the case.
    lmapping <- mapGenesByName("9606", type="symbol", ignore.case=TRUE) 
    for (f in lmapping[["neurod6"]]) {
        expect_true("NEUROD6" %in% genes$symbol[[f]])
    }
})
