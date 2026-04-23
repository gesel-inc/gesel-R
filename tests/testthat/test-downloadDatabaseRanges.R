# library(testthat); library(gesel); source("test-downloadDatabaseRanges.R")

test_that("downloadDatabaseRanges works correctly", {
    observed <- downloadDatabaseRanges("9606_set2gene.tsv", c(10L, 1L, 30L), c(15L, 8L, 40L)) # zero-based index with open end.
    path <- downloadDatabaseFile("9606_set2gene.tsv.gz")
    handle <- gzfile(path, open="rb")
    on.exit(close(handle))
    loaded <- readBin(handle, n=100, what=raw())
    expect_identical(observed, list(loaded[11:15], loaded[2:8], loaded[31:40])) # convert to 1-based index with closed end.
})

test_that("downloadDatabaseRanges handles edge cases", {
    # Ignores empty or impossible ranges.
    expect_identical(downloadDatabaseRanges("whee", integer(0), integer(0)), list())
    expect_identical(downloadDatabaseRanges("whee", c(10L, 20L), c(1L, 20L)), list(raw(), raw()))

    observed <- downloadDatabaseRanges("9606_set2gene.tsv", c(10L, 10L, 20L, 30L), c(1L, 20L, 20L, 50L))
    ref <- downloadDatabaseRanges("9606_set2gene.tsv", c(10L, 30L), c(20L, 50L))
    expect_identical(observed[c(2, 4)], ref)
    expect_identical(observed[c(1, 3)], list(raw(), raw()))
})

test_that("rangeConcurrency works as expected", {
    old <- rangeConcurrency(5L)
    expect_identical(rangeConcurrency(), 5L)

    rangeConcurrency(old)
    expect_identical(rangeConcurrency(), old)
})

test_that("downloadMultipartRanges works on edge cases", {
    expect_identical(downloadMultipartRanges("whee", integer(0), integer(0)), list())
    expect_identical(downloadMultipartRanges("whee", c(10L, 20L), c(1L, 20L)), list(raw(), raw()))

    obs <- downloadMultipartRanges(paste0(databaseUrl(), "/9606_set2gene.tsv"), 10L, 20L)
    expect_identical(obs, downloadDatabaseRanges("9606_set2gene.tsv", 10L, 20L))

    # Mixing a single valid range with invalid ranges.
    obs <- downloadMultipartRanges(paste0(databaseUrl(), "/9606_set2gene.tsv"), c(1L, 10L, 30L), c(1L, 20L, 25L))
    expect_identical(obs, c(list(raw()), downloadDatabaseRanges("9606_set2gene.tsv", 10L, 20L), list(raw())))

    # Doing it again, making sure that sorting is handled correctly.
    obs <- downloadMultipartRanges(paste0(databaseUrl(), "/9606_set2gene.tsv"), c(50L, 5L, 10L, 200L), c(50L, 1L, 30L, 100L))
    expect_identical(obs, c(list(raw(), raw()), downloadDatabaseRanges("9606_set2gene.tsv", 10L, 30L), list(raw())))
})

strip_attrs <- function(x) {
    attributes(x) <- list()
    x
}

test_that("multibyte parsing works as expected (simple)", {
    payload1 <- "asdasdasdasd"
    payload2 <- "FOO\nwhee stuff\nBAR"
    ex <- c(
        "--foo", 
        paste0("Content-Range: bytes 10-", 10 + nchar(payload1) - 1, "/100"),
        "Content-Type: text/plain",
        "",
        payload1,
        "--foo",
        "Content-Type: text/plain",
        paste0("Content-Range: bytes 50-", 50 + nchar(payload2) - 1, "/100"),
        "",
        payload2,
        "--foo--"
    )

    out <- gesel:::parse_multipart_ranges(charToRaw(paste(ex, collapse="\r\n")), "foo")
    expect_identical(strip_attrs(out[[1]]), charToRaw(payload1))
    expect_identical(attr(out[[1]], "Content-Range"), "bytes 10-21/100")
    expect_identical(attr(out[[1]], "Content-Type"), "text/plain")
    expect_identical(strip_attrs(out[[2]]), charToRaw(payload2))
    expect_identical(attr(out[[2]], "Content-Range"), "bytes 50-67/100")
    expect_identical(attr(out[[2]], "Content-Type"), "text/plain")
})

test_that("multibyte parsing works for harder cases", {
    payload1 <- "asdasd🙃🙃🙃asd" # slap in unicode.
    payload2 <- "hottle\r\nwhee\nblah\r\n" # sprinkle in some CR, LFs.
    payload3 <- "Content-Type: asdasd\r\nContent-Range: 942835723948" # Weird header-like part body.

    ex <- paste(c(
        "wheestuff", # ignore stuff preceding the first boundary.
        "--foo", # No headers at all.
        "",
        payload1,
        "--foo    ", # slap in some whitespace.
        paste0("Content-Range: bytes 30-", 30 + nchar(payload2, "bytes") - 1, "/100"),
        "",
        payload2,
        "--foo",
        "Content-Type: text/plain",
        "",
        payload3,
        "--foo--",
        "more" # ignore stuff following the terminating boundary.
    ), collapse="\r\n")

    out <- gesel:::parse_multipart_ranges(charToRaw(paste(ex, collapse="\r\n")), "foo")
    expect_identical(strip_attrs(out[[1]]), charToRaw(payload1))
    expect_identical(strip_attrs(out[[2]]), charToRaw(payload2))
    expect_true(startsWith(attr(out[[2]], "Content-Range"), "bytes 30-"))
    expect_identical(strip_attrs(out[[3]]), charToRaw(payload3))
    expect_identical(attr(out[[3]], "Content-Type"), "text/plain")
})

test_that("multibyte parsing throws errors correctly", {
    expect_error(gesel:::parse_multipart_ranges(raw(), "foo"), "premature")
    expect_error(gesel:::parse_multipart_ranges(charToRaw("--foo"), "foo"), "premature")
    expect_error(gesel:::parse_multipart_ranges(charToRaw("--foo\nyay"), "foo"), "be followed by a CRLF")
    expect_error(gesel:::parse_multipart_ranges(charToRaw("--foo\r\n"), "foo"), "premature")
    expect_error(gesel:::parse_multipart_ranges(charToRaw("--foo\r\n\r"), "foo"), "premature")
    expect_error(gesel:::parse_multipart_ranges(charToRaw("--foo\r\n\r2"), "foo"), "marked by an empty line")
    expect_error(gesel:::parse_multipart_ranges(charToRaw("--foo\r\nContent-Type"), "foo"), "premature")
    expect_error(gesel:::parse_multipart_ranges(charToRaw("--foo\r\nContent-Type:foo"), "foo"), "followed by a space")
    expect_error(gesel:::parse_multipart_ranges(charToRaw("--foo\r\nContent-Type: foo"), "foo"), "premature")
    expect_error(gesel:::parse_multipart_ranges(charToRaw("--foo\r\nContent-Type: foo\r2"), "foo"), "followed by a CRLF")
    expect_error(gesel:::parse_multipart_ranges(charToRaw("--foo\r\nContent-Type: foo\r\n"), "foo"), "premature")
})

test_that("extracting the strings works correctly", {
    payload1 <- "genki dashitte"
    payload2 <- "watashi ga soba ni irukara"

    ex <- paste(c(
        "--foo", 
        paste0("Content-Range: bytes 10-", 10 + nchar(payload1) - 1, "/100"),
        "Content-Type: text/plain",
        "",
        payload1,
        "--foo",
        "Content-Type: text/plain",
        paste0("content-range: bytes 100-", 100 + nchar(payload2) - 1, "/100"), # use lower case header name for some variety.
        "",
        payload2,
        "--foo--"
    ), collapse="\r\n")

    expect_identical(
        gesel:::extract_multipart_strings(charToRaw(ex), "foo", c(100, 10, 102, 14, 10), c(110, 20, 105, 17, 10 + nchar(payload1))),
        list(
            charToRaw(substr(payload2, 1, 10)),
            charToRaw(substr(payload1, 1, 10)),
            charToRaw(substr(payload2, 3, 5)),
            charToRaw(substr(payload1, 5, 7)),
            charToRaw(payload1)
        )
    )

    # Still works if the response is unsorted.
    payload3 <- "koi mo yume mo hana mo niji mo kaze mo"
    ex.uns <- paste(c(
        "--foo", 
        paste0("Content-Range: bytes 200-", 200 + nchar(payload1) - 1, "/100"),
        "Content-Type: text/plain",
        "",
        payload1,
        "--foo",
        paste0("content-range: bytes 100-", 100 + nchar(payload2) - 1, "/100"), # use lower case header name for some variety.
        "",
        payload2,
        "--foo",
        "Content-Type: text/plain",
        paste0("content-range: bytes 140-", 140 + nchar(payload3) - 1, "/100"), # use lower case header name for some variety.
        "",
        payload3,
        "--foo--"
    ), collapse="\r\n")

    expect_identical(
        gesel:::extract_multipart_strings(charToRaw(ex.uns), "foo", c(100, 205, 102, 144, 140), c(110, 210, 105, 147, 140 + nchar(payload3))),
        list(
            charToRaw(substr(payload2, 1, 10)),
            charToRaw(substr(payload1, 6, 10)),
            charToRaw(substr(payload2, 3, 5)),
            charToRaw(substr(payload3, 5, 7)),
            charToRaw(payload3)
        )
    )
})

test_that("refining the ranges works as expected", {
    parts <- list(as.raw(1:10), as.raw(11:30), as.raw(31:80), as.raw(81:100))
    parts.start <- c(101, 211, 331, 581)
    parts.end <- c(110, 230, 380, 600) + 1L

    expect_identical(
        gesel:::refine_ranges(
            parts,
            parts.start,
            parts.end,
            c(105, 590, 215, 102, 360),
            c(110, 595, 228, 105, 377) + 1L
        ),
        list(
            parts[[1]][5:10],
            parts[[4]][10:15],
            parts[[2]][5:18],
            parts[[1]][2:5],
            parts[[3]][30:47]
        )
    )

    # Handles empty or length-1 ranges gracefully. 
    expect_identical(
        gesel:::refine_ranges(
            parts,
            parts.start,
            parts.end,
            c(105, 107, 215, 102, 360),
            c(110, 106, 215, 101, 377) + 1L
        ),
        list(
            parts[[1]][5:10],
            raw(),
            parts[[2]][5],
            raw(),
            parts[[3]][30:47]
        )
    )

    expect_error(gesel:::refine_ranges(parts, parts.start, parts.end, 90, 100), "does not contain") # start is before the requested part's start.
    expect_error(gesel:::refine_ranges(parts, parts.start, parts.end, 101, 112), "does not contain") # end is after the requested part's end.
    expect_error(gesel:::refine_ranges(parts, parts.start, parts.end, 701, 800), "does not contain") # start is after the requested part's end.
})
