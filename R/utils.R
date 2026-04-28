#' @import httr2
handle_error <- function(req) {
    req_error(req, body = function(res) {
        ct <- resp_content_type(res)
        if (ct == "text/plain") {
            resp_body_string(res)
        } else {
            character()
        }
    })
}

#' @import methods
#' @import httr2 
#' @importFrom utils URLencode
download_file <- function(cache, url, overwrite) {
    dir.create(cache, recursive=TRUE, showWarnings=FALSE)
    target <- file.path(cache, URLencode(url, reserved=TRUE))

    if (overwrite || !file.exists(target)) {
        # Saving to a temporary file and renaming it on success,
        # so we don't fail with a partially downloaded file in the cache.
        tempf <- tempfile(tmpdir=cache)
        on.exit(unlink(tempf), add=TRUE, after=FALSE)

        req <- request(url)
        req <- handle_error(req)
        res <- req_perform(req, path=tempf)

        file.rename(tempf, target) # this should be more or less atomic, so no need for locks.
    }

    target
}

decompress_lines <- function(path) {
    handle <- gzfile(path, "rb")
    on.exit(close(handle))
    readLines(handle)
}

compute_ranges <- function(bytes) {
    c(0L, cumsum(as.integer(bytes) + 1L)) # +1 for the newline.
}

retrieve_ranges <- function(config, name) {
    path <- fetch_file(config, paste0(name, ".ranges.gz"))
    contents <- decompress_lines(path)
    compute_ranges(contents)
}

retrieve_ranges_with_sizes <- function(config, name) {
    path <- fetch_file(config, paste0(name, ".ranges.gz"))
    contents <- decompress_lines(path)
    parsed <- strsplit(contents, "\t")
    bytes <- vapply(parsed, function(x) x[1], "")
    extra <- vapply(parsed, function(x) x[2], "")
    list(ranges=compute_ranges(bytes), sizes=as.integer(extra))
}

retrieve_ranges_with_names <- function(config, name) {
    path <- fetch_file(config, paste0(name, ".ranges.gz"))
    contents <- decompress_lines(path)
    parsed <- strsplit(contents, "\t")
    names <- vapply(parsed, function(x) x[1], "")
    bytes <- vapply(parsed, function(x) x[2], "")
    list(names=names, ranges=compute_ranges(bytes))
}

decode_indices_from_raw <- function(contents) {
    lines <- vapply(contents, rawToChar, FUN.VALUE="")
    decode_indices(lines)
}

decode_indices <- function(lines) {
    output <- rep(list(integer()), length(lines))

    keep <- which(nchar(lines) != 0L)
    parsed <- strsplit(lines[keep], "\t", fixed=TRUE)
    output[keep] <- lapply(parsed, function(x) {
        out <- as.integer(x)
        if (anyNA(out)) {
            # This is just a last-ditch defence against incorrect range requests.
            # We'll assume that the number of genes/sets is less than .Machine$integer.max,
            # so we won't bother to consider NAs generated from integer overflow.
            stop("invalid cast to number when decoding indices")
        }
        cumsum(out) + 1L
    })

    output
}
