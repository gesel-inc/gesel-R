# library(testthat); library(gesel); source("setup.R"); source("test-fetchGeneVersion.R")

test_that("fetchGeneVersion works locally", {
    flushMemoryCache(test.config)
    expect_identical(fetchGeneVersion("1111", config = test.config), "0.2.0")
    expect_identical(fetchGeneVersion("1111", config = test.config), "0.2.0") # pulls from cache.

    flushMemoryCache(test.config.v1)
    expect_identical(fetchGeneVersion("1111", config = test.config.v1), "0.1.0")
    expect_identical(fetchGeneVersion("1111", config = test.config.v1), "0.1.0") # pulls from cache.

    # Also just works if you fetch it directly.
    flushMemoryCache(test.config.v1)
    test.config.v1$gene.version <- "0.1.0"
    expect_identical(fetchGeneVersion("1111", config = test.config.v1), "0.1.0")
})

test_that("fetchGeneVersion works remotely", {
    raw.config <- newConfig()
    raw.config$gene.version <- NULL
    expect_identical(fetchGeneVersion("9606", config = raw.config), "0.1.0")
    expect_identical(fetchGeneVersion("9606", config = raw.config), "0.1.0") # pulls from cache.
})

