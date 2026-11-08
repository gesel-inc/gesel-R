# library(testthat); library(gesel); source("test-downloadDatabaseRanges.R")

test_that("downloadDatabaseRanges works correctly", {
    starts <- c(1L, 10L, 30L)
    ends <- c(8L, 15L, 40L)
    output <- downloadDatabaseRanges("9606_set2gene.tsv", starts, ends) # zero-based index with open end.

    out.starts <- attr(output, "retrieved.start")
    expect_identical(length(out.starts), 3L)
    expect_true(all(starts <= out.starts))
    out.ends <- attr(output, "retrieved.end")
    expect_identical(length(out.ends), 3L)
    expect_true(all(ends >= out.ends))

    path <- downloadDatabaseFile("9606_set2gene.tsv.gz")
    handle <- gzfile(path, open="rb")
    on.exit(close(handle))
    loaded <- readBin(handle, n=100, what=raw())

    observed <- vector("list", length(starts))
    for (i in seq_along(starts)) {
        offset <- out.starts[i]
        observed[[i]] <- output[[i]][(starts[i] - offset + 1):(ends[i] - offset)]
    }
    expect_identical(observed, list(loaded[2:8], loaded[11:15], loaded[31:40])) # convert to 1-based index with closed end.

    # Ignores empty ranges.
    empty <- downloadDatabaseRanges("whee", integer(0), integer(0))
    expect_identical(length(empty), 0L)
    expect_identical(attr(empty, "retrieved.start"), integer())
    expect_identical(attr(empty, "retrieved.end"), integer())
})

test_that("rangeConcurrency works as expected", {
    old <- rangeConcurrency(5L)
    expect_identical(rangeConcurrency(), 5L)

    rangeConcurrency(old)
    expect_identical(rangeConcurrency(), old)
})

test_that("downloadMultipartRanges works on edge cases", {
    empty <- downloadMultipartRanges("whee", integer(0), integer(0))
    expect_identical(length(empty), 0L)
    expect_identical(attr(empty, "retrieved.start"), integer())
    expect_identical(attr(empty, "retrieved.end"), integer())

    obs <- downloadMultipartRanges(paste0(databaseUrl(), "/9606_set2gene.tsv"), 10L, 20L)
    expect_identical(obs, downloadDatabaseRanges("9606_set2gene.tsv", 10L, 20L))
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
        paste0("Content-Range: bytes 10-", 10 + nchar(payload1) - 1, "/1000"),
        "Content-Type: text/plain",
        "",
        payload1,
        "--foo",
        "Content-Type: text/plain",
        paste0("content-range: bytes 100-", 100 + nchar(payload2) - 1, "/1000"), # use lower case header name for some variety.
        "",
        payload2,
        "--foo--"
    ), collapse="\r\n")

    output <- gesel:::extract_multipart_strings(charToRaw(ex), "foo")
    expect_identical(length(output), 2L)
    expect_identical(output[[1]], charToRaw(payload1))
    expect_identical(output[[2]], charToRaw(payload2))
    expect_identical(attr(output, "retrieved.start"), c(10L, 100L))
    expect_identical(attr(output, "retrieved.end"), c(10L + nchar(payload1), 100L + nchar(payload2)))

    # Still works if the response is unsorted.
    payload3 <- "koi mo yume mo hana mo niji mo kaze mo"
    ex.uns <- paste(c(
        "--foo", 
        paste0("Content-Range: bytes 200-", 200 + nchar(payload1) - 1, "/1000"),
        "Content-Type: text/plain",
        "",
        payload1,
        "--foo",
        paste0("content-range: bytes 100-", 100 + nchar(payload2) - 1, "/1000"), # use lower case header name for some variety.
        "",
        payload2,
        "--foo",
        "Content-Type: text/plain",
        paste0("content-range: bytes 140-", 140 + nchar(payload3) - 1, "/1000"), # use lower case header name for some variety.
        "",
        payload3,
        "--foo--"
    ), collapse="\r\n")

    output.uns <- gesel:::extract_multipart_strings(charToRaw(ex.uns), "foo")
    expect_identical(length(output.uns), 3L)
    expect_identical(output.uns[[1]], charToRaw(payload1))
    expect_identical(output.uns[[2]], charToRaw(payload2))
    expect_identical(output.uns[[3]], charToRaw(payload3))
    expect_identical(attr(output.uns, "retrieved.start"), c(200L, 100L, 140L))
    expect_identical(attr(output.uns, "retrieved.end"), c(200L + nchar(payload1), 100L + nchar(payload2), 140L + nchar(payload3)))
})
