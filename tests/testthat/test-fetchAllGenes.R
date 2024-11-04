# library(testthat); library(gesel); source("test-fetchAllGenes.R")

set.seed(999999)

gtmp <- tempfile()
dir.create(gtmp)
species <- "2222"

ref.genes <- list()
num.genes <- 500
types <- c("foo", "bar", "whee")

for (i in 1:3) {
    type <- types[i]
    gene.ids <- sprintf("%s_%4d", type, seq_len(9999))

    genes.plus <- num.genes * (1 + i/5) # generating a 1:many mapping of genes to names.
    chosen <- sample(gene.ids, genes.plus, replace=TRUE)
    ids <- sample(num.genes, genes.plus, replace=TRUE)

    current <- unname(split(chosen, factor(ids, seq_len(num.genes))))
    ref.genes[[type]] <- current

    gpath <- file.path(gtmp, paste0(species, "_", type, ".tsv.gz"))
    handle <- gzfile(gpath, open="wb")
    writeLines(vapply(current, paste, collapse="\t", FUN.VALUE=""), con=handle)
    close(handle)
}

test_that("fetchAllGenes works for the local ref", {
    roundtrip <- fetchAllGenes(species, types=types, fetch=function(x) file.path(gtmp, x), use.preloaded=FALSE)
    expect_identical(unclass(roundtrip$foo), ref.genes$foo)
    expect_identical(unclass(roundtrip$bar), ref.genes$bar)
    expect_identical(unclass(roundtrip$whee), ref.genes$whee)
})

test_that("fetchAllGenes behaves for the remote", {
    roundtrip <- fetchAllGenes("9606", use.preloaded=TRUE)
    expect_gt(nrow(roundtrip), 0)

    expect_true(any(lengths(roundtrip$symbol) > 0L))
    expect_true(any(lengths(roundtrip$ensembl) > 0L))
    expect_true(any(lengths(roundtrip$entrez) > 0L))

    expect_true(all(grepl("^(ENSG[0-9]+|LRG_[0-9]+)$", unlist(roundtrip$ensembl))))
    expect_true(all(grepl("^[0-9]+$", unlist(roundtrip$entrez))))

    preloaded <- fetchAllGenes("9606")
    expect_identical(roundtrip, preloaded)
})
