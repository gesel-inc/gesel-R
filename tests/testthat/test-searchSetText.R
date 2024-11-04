# library(testthat); library(gesel); source("setup.R"); source("test-searchSetText.R")

test_that("basic search works for the local ref", {
    out <- searchSetText(species, "foo 1", fetch.file=getDatabaseFile, fetch.range=getDatabaseRanges)
    expect_identical(out, grep("foo 1$", ref.set.info$description, ignore.case=TRUE))

    # Name or description only.
    nout <- searchSetText(species, "this", use.description=FALSE, fetch.file=getDatabaseFile, fetch.range=getDatabaseRanges)
    expect_identical(nout, integer(0))

    nout <- searchSetText(species, "this", use.name=FALSE, fetch.file=getDatabaseFile, fetch.range=getDatabaseRanges)
    expect_identical(nout, grep("this", ref.set.info$description))

    # Throwing in some wildcards.
    pout <- searchSetText(species, "foo 1*", fetch.file=getDatabaseFile, fetch.range=getDatabaseRanges)
    expect_identical(pout, grep("foo 1", ref.set.info$description, ignore.case=TRUE))

    pout <- searchSetText(species, "foo ?", fetch.file=getDatabaseFile, fetch.range=getDatabaseRanges)
    expect_identical(pout, grep("foo .$", ref.set.info$description, ignore.case=TRUE))
})

test_that("basic search works for the remote ref", {
    sets <- fetchAllSets("9606")

    cout <- searchSetText("9606", "cancer")
    expect_true(all(grepl("cancer", sets$name[cout], ignore.case=TRUE) | grepl("cancer", sets$description[cout], ignore.case=TRUE)))

    iout <- searchSetText("9606", "innate immun*")
    expect_true(all(
        (grepl("innate", sets$name[iout], ignore.case=TRUE) & grepl("immun.*", sets$name[iout], ignore.case=TRUE)) |
        (grepl("innate", sets$description[iout], ignore.case=TRUE) & grepl("immun.*", sets$description[iout], ignore.case=TRUE))
    ))

    aout <- searchSetText("9606", "adaptive immun*")
    expect_true(all(
        (grepl("adaptive", sets$name[aout], ignore.case=TRUE) & grepl("immun.*", sets$name[aout], ignore.case=TRUE)) |
        (grepl("adaptive", sets$description[aout], ignore.case=TRUE) & grepl("immun.*", sets$description[aout], ignore.case=TRUE))
    ))
})
