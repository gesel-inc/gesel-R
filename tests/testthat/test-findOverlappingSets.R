# library(testthat); library(gesel); source("setup.R"); source("test-findOverlappingSets.R")

test_that("findOverlappingSets works for the local ref", {
    genes <- sample(ref.num.genes, 100)
    ref <- lapply(ref.set.membership, intersect, y=genes)
    keep <- which(lengths(ref) > 0)

    overlaps <- findOverlappingSets(species, genes, counts.only=FALSE, config=test.config)
    expect_identical(sort(overlaps$overlap$set), sort(keep))
    expect_identical(lapply(ref[overlaps$overlap$set], sort), lapply(overlaps$overlap$genes, sort))

    counts <- findOverlappingSets(species, genes, config=test.config)
    expect_identical(counts$overlap, data.frame(set=overlaps$overlap$set, count=lengths(overlaps$overlap$genes)))
})

test_that("findOverlappingSets works for the remote", {
    chosen <- 1:10 * 10
    everything <- fetchGenesForAllSets("9606")
    ref <- lapply(everything, intersect, y=chosen)
    keep <- which(lengths(ref) > 0)

    overlaps <- findOverlappingSets("9606", chosen, counts.only=FALSE)
    expect_identical(sort(overlaps$overlap$set), sort(keep))
    expect_identical(lapply(ref[overlaps$overlap$set], sort), lapply(overlaps$overlap$genes, sort))

    counts <- findOverlappingSets("9606", chosen)
    expect_identical(counts$overlap, data.frame(set=overlaps$overlap$set, count=lengths(overlaps$overlap$genes)))
})

