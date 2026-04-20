#' Prepare the Gesel database
#'
#' Prepare Gesel database files from various pieces of gene set information.
#'
#' @param species String specifying the species in the form of its NCBI taxonomy ID.
#' @param path String containing the path to a directory in which to create the database files.
#' @param collections Data frame of information about each gene set collection, where each row corresponds to a collection.
#' This data frame should contain the \code{title}, \code{description}, \code{source} and \code{maintainer} columns as described in \code{?\link{fetchAllCollections}}.
#' @param set.info List of data frames of length equal to \code{nrow(collections)}.
#' Each data frame corresponds to a collection where each row corresponds to a gene set.
#' Each data frame should have the \code{name} and \code{description} columns as described in \code{?\link{fetchAllSets}}.
#' @param set.membership List of list of integer vectors.
#' Each inner list corresponds to a collection and each vector corresponds to a gene set in that collection.
#' Each vector contains the identities of its constituent genes, as row indices into the data frame returned by \code{\link{fetchAllGenes}}.
#' All gene indices should be positive and no greater than \code{num.genes}.
#' (Unsorted and duplicate entries are allowed.)
#' @param num.genes Integer specifying the total number of genes available for this species.
#' @param validate Boolean indicating whether to run \code{\link{validateDatabaseFiles}} on the newly created files.
#'
#' @return Several files are produced at \code{path} with the \code{<species>_} prefix.
#' These can be made available for download with \code{\link{downloadDatabaseFile}}.
#'
#' @author Aaron Lun
#' @examples
#' # Mocking up some information.
#' collections <- data.frame(
#'     title=c("FOO", "BAR"),
#'     description=c("I am a foo", "I am a bar"),
#'     maintainer=c("Aaron", "Aaron"),
#'     source=c("https://foo", "https://bar")
#' )
#'
#' set.info <- list(
#'     data.frame(
#'         name=sprintf("FOO_%i", seq_len(20)),
#'         description=sprintf("this is FOO %i", seq_len(20))
#'     ),
#'     data.frame(
#'         name=sprintf("BAR_%i", seq_len(50)),
#'         description=sprintf("this is BAR %i", seq_len(50))
#'     )
#' )
#'
#' # Mocking up the gene sets.
#' num.genes <- 10000
#' set.membership <- list(
#'     lapply(seq_len(nrow(set.info[[1]])), function(i) {
#'         sample(num.genes, sample(500, 1))
#'     }),
#'     lapply(seq_len(nrow(set.info[[2]])), function(i) {
#'         sample(num.genes, sample(200, 1))
#'     })
#' )
#'
#' # Now making the database files.
#' output <- tempfile()
#' dir.create(output)
#' prepareDatabaseFiles(
#'     "9606",
#'     collections, 
#'     set.info, 
#'     set.membership,
#'     num.genes,
#'     output
#' )
#'
#' # We can then read directly from them:
#' config <- newConfig(fetch.file=function(x) file.path(output, x))
#' head(fetchAllSets("9606", config))
#'
#' @export
prepareDatabaseFiles <- function(species, collections, set.info, set.membership, num.genes, path = ".", validate = TRUE) {
    if (nrow(collections) != length(set.info)) {
        stop("length of 'set.info' should equal number of rows in 'collections'")
    }
    if (nrow(collections) != length(set.membership)) {
        stop("length of 'set.membership' should equal number of rows in 'collections'")
    }

    num.sets <- unname(vapply(set.info, nrow, 0L))
    if (!identical(num.sets, unname(lengths(set.membership)))) {
        stop("number of sets in each collection of 'set.info' and 'set.membership' should be the same")
    }

    prefix <- file.path(path, paste0(species, "_"))

    save_data_frame_with_sizes(
        data.frame(
            title=collections$title,
            description=collections$description,
            species=species,
            maintainer=collections$maintainer,
            `source`=collections$source
        ), 
        paste0(prefix, "collections.tsv"),
        size=num.sets
    )

    set.info <- do.call(rbind, set.info)
    set.membership <- do.call(c, set.membership)
    set.membership <- lapply(set.membership, function(set) sort(unique(set)))

    save_data_frame_with_sizes(
        data.frame(
            name=set.info$name,
            description=set.info$description
        ), 
        paste0(prefix, "sets.tsv"),
        size=lengths(set.membership)
    )

    save_integer_list(set.membership, paste0(prefix, "set2gene.tsv"))

    reversed <- rep(seq_along(set.membership), lengths(set.membership))
    f <- factor(unlist(set.membership), levels=seq_len(num.genes))
    if (anyNA(f)) {
        stop("detected out-of-bounds gene indices in 'set.membership'")
    }
    save_integer_list(split(reversed, f), paste0(prefix, "gene2set.tsv"))

    # Build a search index for the descriptions and names.
    by.dtoken <- tokenize(set.info$description)
    by.ntoken <- tokenize(set.info$name)
    save_integer_list(by.ntoken, paste0(prefix, "tokens-names.tsv"), include.names=TRUE)
    save_integer_list(by.dtoken, paste0(prefix, "tokens-descriptions.tsv"), include.names=TRUE)

    if (validate) {
        validateDatabaseFiles(path, species, num.genes)
    }

    invisible(NULL)
}

#' @importFrom utils write.table
save_integer_list <- function(x, prefix, include.names = FALSE) {
    lines <- character(length(x))
    for (i in seq_along(x)) {
        z <- x[[i]]
        if (length(z)) {
            # Assume we're already sorted and unique, then get to 0-based indexing with delta encoding.
            z <- c(z[1] - 1L, diff(z))
            lines[i] <- paste(z, collapse="\t")
        }
    }
    write(lines, file=prefix)

    strlen <- nchar(lines, type="bytes") # deal with UTF-8 chars.
    handle <- gzfile(paste0(prefix, ".ranges.gz"), open="wb")
    if (!include.names || is.null(names(x))) {
        write(strlen, file=handle, ncolumns=1)
    } else {
        write.table(data.frame(X=names(x), Y=strlen), col.names=FALSE, row.names=FALSE, quote=FALSE, file=handle, sep="\t")
    }
    close(handle)

    # Saving a zipped copy for use in full client-side analysis.
    handle <- gzfile(paste0(prefix, ".gz"), open="wb")
    write(lines, file=handle)
    close(handle)
}

save_data_frame_with_sizes <- function(x, prefix, size) {
    x <- lapply(as.list(x), function(x) gsub("\t|\n", " ", x))
    lines <- do.call(paste, c(x, list(sep="\t")))
    write(lines, file=prefix)

    nc <- nchar(lines, type="bytes") # deal with UTF-8 chars.
    handle <- gzfile(paste0(prefix, ".ranges.gz"), open="wb")
    write.table(data.frame(X=nc, Y=size), file=handle, row.names=FALSE, quote=FALSE, col.names=FALSE, sep="\t")
    close(handle)

    collected <- paste(lines, size, sep="\t")
    handle <- gzfile(paste0(prefix, ".gz"), open="wb")
    write(collected, file=handle, ncolumns=1)
    close(handle)
}

tokenize <- function(x) {
    out <- gsub("[^a-zA-Z0-9-]", " ", tolower(x))
    tokens <- strsplit(out, "\\s+")
    ids <- rep(seq_along(tokens), lengths(tokens))
    by.token <- split(ids, unlist(tokens))
    by.token <- by.token[!(names(by.token) %in% "")]
    lapply(by.token, unique)
}
