set.seed(9999)
species <- "1111"

# Mocking up some information.
ref.collections <- data.frame(
    title=c("FOO", "BAR", "WHE"),
    description=c("I am a foo", "I am a bar", "I am a whe"),
    maintainer=c("Aaron", "Aaron", "Aaron"),
    source=c("https://foo", "https://bar", "https://whe"),
    start=c(1L, 21L, 71L),
    size=c(20L, 50L, 10L)
)

ref.set.info <- list(
    data.frame(
        name=sprintf("FOO_%i", seq_len(20)),
        description=sprintf("this is FOO %i", seq_len(20))
    ),
    data.frame(
        name=sprintf("BAR_%i", seq_len(50)),
        description=sprintf("this is BAR %i", seq_len(50))
    ),
    data.frame(
        name=sprintf("WHE_%i", seq_len(10)),
        description=sprintf("this is WHE %i", seq_len(10))
    )
)

# Mocking up the gene sets.
ref.num.genes <- 10000
ref.set.membership <- list(
    lapply(seq_len(nrow(ref.set.info[[1]])), function(y) sample(ref.num.genes, sample(200, 1))),
    lapply(seq_len(nrow(ref.set.info[[2]])), function(y) sample(ref.num.genes, sample(500, 1))),
    lapply(seq_len(nrow(ref.set.info[[3]])), function(y) sample(ref.num.genes, sample(100, 1)))
)

# Now making the database files.
ref.dir <- tempfile()
dir.create(ref.dir)
gesel::prepareDatabaseFiles(
    species,
    ref.collections, 
    ref.set.info, 
    ref.set.membership,
    ref.num.genes,
    ref.dir 
)

getDatabaseRanges <- function(dir, name, starts, ends) {
    handle <- file(file.path(dir, name), open="rb")
    on.exit(close(handle))

    o <- order(starts)
    output <- rep(list(raw()), length(starts))
    for (i in o) {
        seek(handle, where=starts[i]) # where= seems to be zero-based in terms of its position.
        output[[i]] <- readBin(handle, what=raw(), n=ends[i] - starts[i])
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

test.config <- gesel::newConfig(
    fetch.gene = function(name) file.path(gene.dir, name),
    fetch.file = function(name) file.path(ref.dir, name),
    fetch.ranges = function(name, starts, ends) getDatabaseRanges(ref.dir, name, starts, ends)
)
