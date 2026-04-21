# library(testthat); library(gesel); source("setup.R"); source("test-searchSetText.R")

test_that("basic search works for the local ref", {
    full.set.info <- fetchAllSets(species, config=test.config)
    flushMemoryCache(test.config)

    out <- searchSetText(species, "foo 1", config=test.config)
    expect_identical(out, grep("foo 1$", full.set.info$description, ignore.case=TRUE))

    # Name or description only.
    nout <- searchSetText(species, "this", use.description=FALSE, config=test.config)
    expect_identical(nout, integer(0))

    nout <- searchSetText(species, "this", use.name=FALSE, config=test.config)
    expect_identical(nout, grep("this", full.set.info$description))

    # Throwing in some wildcards.
    pout <- searchSetText(species, "foo 1*", config=test.config)
    expect_identical(pout, grep("foo 1", full.set.info$description, ignore.case=TRUE))

    pout <- searchSetText(species, "foo ?", config=test.config)
    expect_identical(pout, grep("foo .$", full.set.info$description, ignore.case=TRUE))
})

test_that("basic search works for the remote ref", {
    sets <- fetchAllSets("9606")
    flushMemoryCache()

    cout <- searchSetText("9606", "cancer")
    expect_true(all(grepl("cancer", sets$name[cout], ignore.case=TRUE) | grepl("cancer", sets$description[cout], ignore.case=TRUE)))

    # Multiple words with a prefixed wildcard.
    iout <- searchSetText("9606", "innate immun*")
    expect_true(
        all(
            (grepl("innate", sets$name[iout], ignore.case=TRUE) | grepl("innate", sets$description[iout], ignore.case=TRUE)) &
            (grepl("immun.*", sets$name[iout], ignore.case=TRUE) | grepl("immun.*", sets$description[iout], ignore.case=TRUE))
        )
    )

    aout <- searchSetText("9606", "adaptive immun*")
    expect_true(
        all(
            (grepl("adaptive", sets$name[aout], ignore.case=TRUE) | grepl("adaptive*", sets$description[aout], ignore.case=TRUE)) &
            (grepl("immun.*", sets$name[aout], ignore.case=TRUE) | grepl("immun.*", sets$description[aout], ignore.case=TRUE))
        )
    )

    # Trying with only the name or description (also tests the cache).
    iout.noname <- searchSetText("9606", "immun*", use.name=FALSE)
    expect_true(all(grepl("immun.*", sets$description[iout.noname], ignore.case=TRUE)))

    iout.nodesc <- searchSetText("9606", "immun*", use.description=FALSE)
    expect_true(all(grepl("immun.*", sets$name[iout.nodesc], ignore.case=TRUE)))

    iout.none <- searchSetText("9606", "immun*", use.description=FALSE, use.name=FALSE)
    expect_identical(length(iout.none), 0L)

    # Non-prefixed wildcard.
    nout <- searchSetText("9606", "*nucleic*")
    expect_true(all(grepl(".*nucleic.*", sets$name[nout], ignore.case=TRUE) | grepl(".*nucleic.*", sets$description[nout], ignore.case=TRUE)))

    # Trying a rare question mark.
    tout <- searchSetText("9606", "?typical")
    expect_true(all(grepl(".typical", sets$name[tout], ignore.case=TRUE) | grepl(".typical", sets$description[tout], ignore.case=TRUE)))
})
