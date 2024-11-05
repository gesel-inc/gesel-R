# library(testthat); library(gesel); source("test-downloadDatabaseRanges.R")

test_that("downloadDatabaseRanges works on edge cases", {
    expect_identical(downloadDatabaseRanges("whee", integer(0), integer(0)), character(0))
    expect_identical(downloadDatabaseRanges("whee", c(10L, 20L), c(1L, 20L)), character(2))

    # Ignores empty or impossible ranges.
    observed <- downloadDatabaseRanges("9606_set2gene.tsv", c(10L, 10L, 20L, 30L), c(1L, 20L, 20L, 50L))
    ref <- downloadDatabaseRanges("9606_set2gene.tsv", c(10L, 30L), c(20L, 50L))
    expect_identical(observed[c(2, 4)], ref)
    expect_identical(observed[c(1, 3)], character(2))
})

test_that("downloadMultipartRanges works on edge cases", {
    expect_identical(downloadMultipartRanges("whee", integer(0), integer(0)), character(0))
    expect_identical(downloadMultipartRanges("whee", c(10L, 20L), c(1L, 20L)), character(2))

    obs <- downloadMultipartRanges(paste0(databaseUrl(), "/9606_set2gene.tsv"), 10L, 20L)
    expect_identical(obs, downloadDatabaseRanges("9606_set2gene.tsv", 10L, 20L))

    obs <- downloadMultipartRanges(paste0(databaseUrl(), "/9606_set2gene.tsv"), c(1L, 10L, 10L), c(1L, 20L, 1L))
    expect_identical(obs, downloadDatabaseRanges("9606_set2gene.tsv", c(1L, 10L, 10L), c(1L, 20L, 1L)))
})

test_that("multibyte parsing works as expected", {
    payload1 <- "asdasdasdasd"
    payload2 <- "hottle\r\nwhee\nblah\r\n"
    ex <- c(
        "--foo", 
        "Content-Range: bytes 10-17/100",
        "Content-Type: text/plain",
        "",
        payload1,
        "--foo",
        "Content-Type: text/plain",
        "Content-Range: bytes 20-30/100",
        "",
        payload2,
        "--foo--"
    )

    out <- gesel:::parse_multipart_response(paste(ex, collapse="\r\n"), "foo")
    expect_identical(out$start, c(10L, 20L))
    expect_identical(out$content, c(payload1, payload2))
})
