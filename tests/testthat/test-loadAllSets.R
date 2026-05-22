# library(testthat); library(gesel); source("setup.R"); source("test-loadAllSets.R")

test_that("loadAllSets works locally", {
    loaded <- loadAllSets("1111", "foo", config=test.config)
    expect_identical(length(loaded$sets), nrow(loaded$details))
    expect_true(all(startsWith(unlist(loaded$sets), "foo_")))

    loaded.comp <- loadAllSets("1111", "foo", config=test.config, as.compressed = TRUE)
    expect_s4_class(loaded.comp, "CompressedCharacterList")
    expect_identical(loaded$sets, as.list(loaded.comp))
    expect_identical(loaded$details, as.data.frame(S4Vectors::mcols(loaded.comp)))
})

test_that("loadAllSets works for a remote", {
    loaded <- loadAllSets("7227", "symbol")
    expect_identical(length(loaded$sets), nrow(loaded$details))

    loaded.comp <- loadAllSets("7227", "symbol", as.compressed = TRUE)
    expect_identical(loaded$sets, as.list(loaded.comp))
    expect_identical(loaded$details, as.data.frame(S4Vectors::mcols(loaded.comp)))
})
