#' Prepare the Gesel database
#'
#' Prepare Gesel database files from various pieces of gene set information.
#'
#' @param species String specifying the species in the form of its NCBI taxonomy ID.
#' @param path String containing the path to a directory in which to create the database files.
#' @param collections Data frame of information about each gene set collection, where each row corresponds to a collection.
#' This data frame should contain the same columns as that returned by \code{\link{fetchAllCollections}}.
#' @param set.info Data frame of information about each gene set, where each row corresponds to a set. 
#' This data frame should contain the same columns as that returned by \code{\link{fetchAllSets}}.
#' @param set.membership List of integer vectors, where each vector corresponds to a gene set and contains the indices of its constituent genes.
#' All gene indices should be positive and no greater than \code{num.genes}.
#' @param num.genes Integer scalar specifying the total number of genes available for this species.
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
#'     source=c("https://foo", "https://bar"),
#'     start=c(1L, 21L),
#'     size=c(20L, 50L)
#' )
#'
#' set.info <- data.frame(
#'     name=c(
#'         sprintf("FOO_%i", seq_len(20)),
#'         sprintf("BAR_%i", seq_len(50))
#'     ),
#'     description=c(
#'         sprintf("this is FOO %i", seq_len(20)),
#'         sprintf("this is BAR %i", seq_len(50))
#'     ),
#'     collection=rep(1:2, c(20L, 50L))
#' )
#'
#' # Mocking up the gene sets.
#' num.genes <- 10000
#' set.membership <- split(
#'     sample(num.genes, 5000, replace=TRUE),
#'     factor(
#'         sample(nrow(set.info), 5000, replace=TRUE),
#'         seq_len(nrow(set.info))
#'     )
#' )
#' set.info$size <- lengths(set.membership)
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
prepareDatabaseFiles <- function(species, collections, set.info, set.membership, num.genes, path = ".") {
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
        size=collections$size
    )

    if (!identical(rep(seq_len(nrow(collections)), collections$size), set.info$collection)) {
        stop("'collections$size' is not consistent with 'set.info$collection'")
    }

    save_data_frame_with_sizes(
        data.frame(
            name=set.info$name,
            description=set.info$description
        ), 
        paste0(prefix, "sets.tsv"),
        size=set.info$size
    )

    if (!identical(set.info$size, unname(lengths(set.membership)))) {
        stop("'set.info$size' is not consistent with 'lengths(set.membership)'")
    }
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

    invisible(NULL)
}

#' @importFrom utils write.table
save_integer_list <- function(x, prefix, include.names = FALSE) {
    lines <- character(length(x))
    for (i in seq_along(x)) {
        z <- x[[i]]
        if (length(z)) {
            z <- sort(unique(z)) # convert to diffs to reduce integer size
            z <- c(z[1] - 1L, diff(z)) # get to 0-based indexing with delta encoding.
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
    by.token[!(names(by.token) %in% c("", "-"))]
}
