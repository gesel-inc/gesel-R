# library(gesel); library(testthat); source("setup.R"); source("test-prepareDatabaseFiles.R")

test_that("prepareDatabaseFiles handles non-unique and unsorted values correctly", {
    set.membership2 <- ref.set.membership
    for (i in seq_along(set.membership2)) {
        current <- set.membership2[[i]]
        for (j in seq_along(current)) {
            current[[j]] <- c(current[[j]], head(current[[j]]))
        }
        set.membership2[[i]] <- current
    }

    copy.dir <- tempfile()
    dir.create(copy.dir)
    gesel::prepareDatabaseFiles(
        "9606",
        ref.collections, 
        ref.set.info, 
        set.membership2,
        ref.num.genes,
        copy.dir 
    )

    copy.config <- gesel::newConfig(
        fetch.gene = function(name) file.path(gene.dir, name),
        fetch.file = function(name) file.path(copy.dir, name),
        fetch.ranges = function(name, starts, ends) getDatabaseRanges(copy.dir, name, starts, ends)
    )

    expect_identical(fetchAllCollections("9606", config=copy.config), fetchAllCollections(species, config=test.config))
    expect_identical(fetchAllSets("9606", config=copy.config), fetchAllSets(species, config=test.config))
    expect_identical(fetchGenesForAllSets("9606", config=copy.config), fetchGenesForAllSets(species, config=test.config))
    expect_identical(fetchSetsForAllGenes("9606", config=copy.config), fetchSetsForAllGenes(species, config=test.config))
})

test_that("prepareDatabaseFiles emits errors correctly", {
    copy.dir <- tempfile()
    dir.create(copy.dir)
    expect_error(gesel::prepareDatabaseFiles(
        "9606",
        ref.collections, 
        ref.set.info[1], 
        ref.set.membership,
        ref.num.genes,
        copy.dir 
    ), "should equal number of rows")

    expect_error(gesel::prepareDatabaseFiles(
        "9606",
        ref.collections, 
        ref.set.info, 
        ref.set.membership[1],
        ref.num.genes,
        copy.dir 
    ), "should equal number of rows")

    sub.membership <- ref.set.membership
    sub.membership[[2]] <- sub.membership[[2]][1:5]
    expect_error(gesel::prepareDatabaseFiles(
        "9606",
        ref.collections, 
        ref.set.info, 
        sub.membership, 
        ref.num.genes,
        copy.dir 
    ), "should be the same")

    sub.membership <- ref.set.membership
    sub.membership[[3]][[1]][1] <- 1e6
    expect_error(gesel::prepareDatabaseFiles(
        "9606",
        ref.collections, 
        ref.set.info, 
        sub.membership, 
        ref.num.genes,
        copy.dir 
    ), "out-of-bounds")
})
