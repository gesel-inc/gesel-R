# library(testthat); library(gesel); source("setup.R"); source("test-searchOverlappingSets.R")

test_that("searchOverlappingSets works for the local ref", {
    full.set.membership <- unlist(ref.set.membership, recursive=FALSE)
    genes <- sample(ref.num.genes, 100)
    ref <- lapply(full.set.membership, intersect, y=genes)
    keep <- which(lengths(ref) > 0)

    overlaps <- searchOverlappingSets(species, genes, counts.only=FALSE, config=test.config)
    expect_identical(sort(overlaps$set), sort(keep))
    expect_identical(lapply(ref[overlaps$set], sort), lapply(overlaps$which, sort))

    expect_identical(lengths(full.set.membership[overlaps$set]), overlaps$size)
    expect_gte(min(overlaps$pvalue), 0)
    expect_lte(max(overlaps$pvalue), 1)
    expect_gte(attr(overlaps, "present"), 0)

    counts <- searchOverlappingSets(species, genes, config = test.config)
    expect_identical(counts$set, overlaps$set)
    expect_identical(counts$count, lengths(overlaps$which))

    # Automatically removes duplicates from the genes.
    duplicated.c <- searchOverlappingSets(species, c(genes, genes), config=test.config)
    expect_identical(counts, duplicated.c)
    duplicated.o <- searchOverlappingSets(species, c(genes, genes), counts.only = FALSE, config=test.config)
    expect_identical(overlaps, duplicated.o)

    # Works without enrichment. 
    no.p <- searchOverlappingSets(species, genes, test.enrichment=FALSE, config=test.config)
    expect_identical(no.p, data.frame(set = counts$set, count = counts$count))
})

test_that("searchOverlappingSets works for the remote", {
    chosen <- 1:10 * 10L
    everything <- fetchGenesForAllSets("9606")
    ref <- lapply(everything, intersect, y=chosen)
    keep <- which(lengths(ref) > 0)

    overlaps <- searchOverlappingSets("9606", chosen, counts.only=FALSE)
    expect_identical(sort(overlaps$set), sort(keep))
    expect_identical(lapply(ref[overlaps$set], sort), lapply(overlaps$which, sort))

    counts <- searchOverlappingSets("9606", chosen)
    expect_identical(counts$set, overlaps$set)
    expect_identical(counts$count, lengths(overlaps$which))
})

