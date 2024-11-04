set.seed(9999)

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
    "1111",
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
        seek(handle, where=starts[i])
        output[i] <- rawToChar(readBin(handle, what=raw(), n=ends[i] - starts[i]))
    }

    output
}
