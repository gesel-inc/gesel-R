# library(testthat); library(gesel); source("setup.R"); source("test-fetchGeneTypes.R")

test_that("fetchGeneTypes works locally", {
    flushMemoryCache(test.config)
    expect_identical(fetchGeneTypes("1111", config = test.config), ref.gene.types)
    expect_identical(fetchGeneTypes("1111", config = test.config), ref.gene.types) # works again from cache.

    flushMemoryCache(test.config.v1)
    expect_identical(fetchGeneTypes("1111", config = test.config.v1), c("ensembl", "entrez", "symbol"))
})

test_that("fetchGeneTypes works remotely", {
    flushMemoryCache()
    expect_identical(fetchGeneTypes("9606"), c("ensembl", "entrez", "symbol")) 
    expect_identical(fetchGeneTypes("9606"), c("ensembl", "entrez", "symbol")) # works again from cache.
})
