# library(testthat); library(gesel); source("setup.R"); source("test-createGeneIdentifierMap.R")

test_that("createGeneIdentifierMap works for the local ref", {
    flushMemoryCache(test.config)

    mapping <- createGeneIdentifierMap(species, type="Bravo", config=test.config) 
    expect_true(all(startsWith(names(mapping),"Bravo_")))

    current.names <- unlist(ref.genes$Bravo)
    unique.current.names <- unique(current.names)
    expect_identical(sort(names(mapping)), sort(unique.current.names))

    current.idx <- rep(seq_along(ref.genes$Bravo), lengths(ref.genes$Bravo))
    for (x in sample(unique.current.names, 20)) { # test a few examples.
        current <- current.idx[current.names == x]
        expect_identical(sort(mapping[[x]]), sort(current))
    }

    # Works when ignoring the case.
    lmapping <- createGeneIdentifierMap(species, type="CHARLIE", ignore.case=TRUE, config=test.config)
    expect_true(all(startsWith(names(lmapping), "charlie_")))

    current.names <- tolower(unlist(ref.genes$CHARLIE))
    unique.current.names <- unique(current.names)
    expect_identical(sort(names(lmapping)), sort(unique.current.names))

    current.idx <- rep(seq_along(ref.genes$CHARLIE), lengths(ref.genes$CHARLIE))
    for (x in sample(unique.current.names, 20)) { # test a few examples.
        current <- current.idx[current.names == x]
        expect_identical(sort(lmapping[[x]]), sort(current))
    }
})

test_that("createGeneIdentifierMap works for the remote ref", {
    flushMemoryCache()

    genes <- fetchAllGenes("9606")

    mapping <- createGeneIdentifierMap("9606", type="symbol") 
    for (f in mapping[["SNAP25"]]) {
        expect_true("SNAP25" %in% genes$symbol[[f]])
    }

    emapping <- createGeneIdentifierMap("9606", type="ensembl") 
    for (f in mapping[["ENSG00000000003"]]) {
        expect_true("ENSG00000000003" %in% genes$ensembl[[f]])
    }

    # Works when ignoring the case.
    lmapping <- createGeneIdentifierMap("9606", type="symbol", ignore.case=TRUE) 
    for (f in lmapping[["neurod6"]]) {
        expect_true("NEUROD6" %in% genes$symbol[[f]])
    }

    # Fetches it from the cache.
    reloaded <- createGeneIdentifierMap("9606", type="symbol") 
    expect_identical(mapping, reloaded)
})
