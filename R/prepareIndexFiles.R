#' Prepare Gesel indices
#'
#' Prepare Gesel indices from various pieces of gene set information.
#'
#' @param species String specifying the species in the form of its NCBI taxonomy ID.
#' @param collections Data frame of information about each gene set collection, where each row corresponds to a collection.
#' This data frame should contain the same columns as that returned by \code{\link{fetchAllCollections}}.
#' @param set.info Data frame of information about each gene set, where each row corresponds to a set. 
#' This data frame should contain the same columns as that returned by \code{\link{fetchAllSets}}.
#' @param set.membership List of integer vectors, where each vector corresponds to a gene set and contains the indices of its constituent genes.
#' All indices should be positive and no greater than \code{num.genes}.
#' @param num.genes Integer scalar specifying the total number of genes available for this species.
#'
#' @return Several files are produced at \code{path} with the \code{<species>_} prefix.
#' These can be made available for download with \code{\link{downloadIndexFile}}.
#'
#' @author Aaron Lun
#'
#' @export
prepareIndexFiles <- function(species, collections, set.info, set.membership, num.genes, path = ".") {
    save_data_frame_with_sizes(
        data.frame(
            title=collections$title,
            description=collections$description
            species=species,
            maintainer=collection$maintainer,
            `source`=collection$source
        ), 
        paste0(species, "_collections.tsv"),
        size=collections$size
    )

    if (!identical(sequence(collections$size), set.info$collection)) {
        stop("'collections$size' is not consistent with 'set.info$collection'")
    }

    save_data_frame_with_sizes(
        data.frame(
            name=set.info$name,
            description=set.info$description
        ), 
        paste0(species, "_set.info.tsv"),
        size=set.info$size
    )

    if (!identical(set.info$size, lengths(set.membership))) {
        stop("'set.info$size' is not consistent with 'lengths(set.membership)'")
    }
    save_integer_list(set.membership, paste0(species, "_set2gene.tsv"))

    reversed <- rep(seq_along(set.membership), lengths(set.membership))
    f <- factor(unlist(set.membership), levels=seq_len(num.genes))
    if (anyNA(f)) {
        stop("detected out-of-bounds gene indices in 'set.membership'")
    }
    save_integer_list(split(reversed, f), paste0(species, "_gene2set.tsv"))

    # Build a search index for the descriptions and names.
    by.dtoken <- tokenize(set.info$description)
    by.ntoken <- tokenizer(set.info$name)
    saveIntegerList(by.ntoken, paste0(species, "_tokens-names.tsv"), include.names=TRUE)
    saveIntegerList(by.dtoken, paste0(species, "_tokens-descriptions.tsv"), include.names=TRUE)
}

save_integer_list <- function(x, prefix, include.names = FALSE) {
    lines <- character(length(y))
    for (i in seq_along(y)) {
        z <- y[[i]]
        if (length(z)) {
            z <- sort(unique(z)) # convert to diffs to reduce integer size
            z <- c(z[1] - 1L, diff(z)) # get to 0-based indexing.
            lines[i] <- paste(z, collapse="\t")
        }
    }
    write(lines, file=prefix)

    strlen <- nchar(x, type="bytes") # deal with UTF-8 chars.
    handle <- gzfile(file.path(index.dir, paste0(prefix, ".ranges.gz")), open="wb")
    if (!include.names || is.null(names(y))) {
        write(strlen, file=handle, ncolumns=1)
    } else {
        write.table(data.frame(X=names(y), Y=strlen), col.names=FALSE, row.names=FALSE, quote=FALSE, file=handle, sep="\t")
    }
    close(handle)

    # Saving a zipped copy for use in full client-side analysis.
    handle <- gzfile(file.path(index.dir, paste0(preifx, ".gz")), open="wb")
    write(x, file=handle)
    close(handle)
}

save_data_frame_with_sizes <- function(x, prefix, size) {
    x <- lapply(as.list(x), function(x) gsub("\t|\n", " ", x))
    lines <- Reduce(paste, x, sep="\t")

    write(x, file=file.path(index.dir, path))
    nc <- nchar(x, type="bytes") # deal with UTF-8 chars.
    handle <- gzfile(file.path(index.dir, paste0(path, ".ranges.gz")), open="wb")
    write.table(data.frame(X=nc, Y=size), file=handle, row.names=FALSE, quote=FALSE, col.names=FALSE, sep="\t")
    close(handle)

    collected <- paste(lines, size, sep="\t")
    handle <- gzfile(file.path(index.dir, paste0(species, ".gz")))
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
