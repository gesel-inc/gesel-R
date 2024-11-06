set.seed(9999)
species <- "1111"

# Mocking up some information.
ref.collections <- data.frame(
    title=c("FOO", "BAR"),
    description=c("I am a foo", "I am a bar"),
    maintainer=c("Aaron", "Aaron"),
    source=c("https://foo", "https://bar"),
    start=c(1L, 21L),
    size=c(20L, 50L)
)

ref.set.info <- data.frame(
    name=c(
        sprintf("FOO_%i", seq_len(20)),
        sprintf("BAR_%i", seq_len(50))
    ),
    description=c(
        sprintf("this is FOO %i", seq_len(20)),
        sprintf("this is BAR %i", seq_len(50))
    ),
    collection=rep(1:2, c(20L, 50L))
)

# Mocking up the gene sets.
ref.num.genes <- 10000
ref.set.membership <- split(
    sample(ref.num.genes - 10L, 5000, replace=TRUE) + 5L, # first and last 5 genes don't get involved.
    factor(
        sample(nrow(ref.set.info), 5000, replace=TRUE),
        seq_len(nrow(ref.set.info))
    )
)
ref.set.membership[1:4 * 10] <- rep(list(integer(0)), 4L) # spiking in a few empty sets, for good measure.
ref.set.membership <- unname(ref.set.membership)
ref.set.info$size <- lengths(ref.set.membership)

# Now making the database files.
ref.dir <- tempfile()
dir.create(ref.dir)
prepareDatabaseFiles(
    species,
    ref.collections, 
    ref.set.info, 
    ref.set.membership,
    ref.num.genes,
    ref.dir 
)

getDatabaseFile <- function(name) {
    file.path(ref.dir, name)
}

getDatabaseRanges <- function(name, starts, ends) {
    handle <- file(file.path(ref.dir, name), open="rb")
    on.exit(close(handle))

    o <- order(starts)
    output <- character(length(o))
    for (i in o) {
        seek(handle, where=starts[i]) # where= seems to be zero-based in terms of its position.
        output[i] <- rawToChar(readBin(handle, what=raw(), n=ends[i] - starts[i]))
    }

    output
}

gene.dir <- tempfile()
dir.create(gene.dir)

ref.genes <- list()
num.genes <- 500
ref.gene.types <- c("foo", "bar", "WHEE")

for (i in 1:3) {
    type <- ref.gene.types[i]
    gene.ids <- sprintf("%s_%4d", type, seq_len(9999))

    genes.plus <- num.genes * (1 + i/5) # generating a 1:many mapping of genes to names.
    chosen <- sample(gene.ids, genes.plus, replace=TRUE)
    ids <- sample(num.genes, genes.plus, replace=TRUE)

    current <- unname(split(chosen, factor(ids, seq_len(num.genes))))
    ref.genes[[type]] <- current

    gpath <- file.path(gene.dir, paste0(species, "_", type, ".tsv.gz"))
    handle <- gzfile(gpath, open="wb")
    writeLines(vapply(current, paste, collapse="\t", FUN.VALUE=""), con=handle)
    close(handle)
}

getGeneFile <- function(name) {
    file.path(gene.dir, name)
}

test.config <- gesel::newConfig(
    fetch.gene = getGeneFile,
    fetch.file = getDatabaseFile,
    fetch.range = getDatabaseRanges
)
