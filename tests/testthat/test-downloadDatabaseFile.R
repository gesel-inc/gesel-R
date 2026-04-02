# library(testthat); library(gesel); source("test-downloadDatabaseFile.R")

test_that("downloadDatabaseFile works correctly", {
    tmp <- tempfile()
    out <- downloadDatabaseFile("9606_collections.tsv.gz", cache=tmp)
    expect_true(file.exists(out))
    expect_identical(dirname(out), tmp)

    cat(character(0), file=out)
    expect_identical(downloadDatabaseFile("9606_collections.tsv.gz", cache=tmp), out) # re-uses the cache.
    expect_equal(file.size(out), 0) # doesn't change the cached value.

    expect_identical(downloadDatabaseFile("9606_collections.tsv.gz", cache=tmp, overwrite=TRUE), out) # re-uses the cache.
    expect_gt(file.size(out), 0) # overwrites the broken cache value.
})

test_that("databaseUrl works correctly", {
    url <- databaseUrl()
    expect_identical(length(url), 1L)
    expect_type(url, "character")

    old <- databaseUrl("https://foo.bar")
    expect_identical(databaseUrl(), "https://foo.bar")

    databaseUrl(old)
    expect_identical(databaseUrl(), url)
})
