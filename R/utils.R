#' @import httr2
handle_error <- function(req) {
    req_error(req, body = function(res) {
        if (ct == "text/plain") {
            resp_body_string(res)
        } else {
            NULL
        }
    })
}

#' @import httr2
parse_remote_last_modified <- function(res) {
    remote_mod <- resp_header(res, "last-modified")

    if (is.null(remote_mod)) {
        warning("failed to find 'last-modified' header from the SewerRat API")
        return(NULL)
    }

    remote_mod <- as.POSIXct(remote_mod, format="%a, %d %b %Y %H:%M:%S", tz="GMT")
    if (is.na(remote_mod)) {
        warning("invalid 'last-modified' header from the SewerRat API")
        return(NULL)
    }

    return(remote_mod) 
}

#' @import httr2 
#' @importFrom rappdirs user_cache_dir
#' @importFrom utils URLencode
download_file <- function(cache, url, overwrite) {
    if (is.null(cache)) {
        cache <- user_cache_dir("gesel")
        dir.create(cache, recursive=TRUE, showWarnings=FALSE)
    }
    target <- file.path(cache, URLencode(url, reserved=TRUE))

    if (!file.exists(target)) {
        overwrite <- TRUE
    } else if (!overwrite) {
        req <- request(url)
        req <- req_method(req, "HEAD")
        req <- handle_error(req)
        res <- try(req_perform(req), silent=TRUE)
        if (!is(res, "try-error")) { # don't throw an error if there is no internet.
            remote_mod <- parse_remote_last_modified(res)
            last_mod <- file.info(target)$mtime
            if (!is.null(remote_mod) && remote_mod > last_mod) {
                overwrite <- TRUE
            }
        }
    }

    if (overwrite) {
        # Saving to a temporary file and renaming it on success,
        # so we don't fail with a partially downloaded file in the cache.
        tempf <- tempfile(tmpdir=cache)
        on.exit(unlink(tempf), add=TRUE, after=FALSE)

        req <- request(url)
        req <- handle_error(req)
        res <- req_perform(req, path=tempf)

        # The key part here is to set the modification time correctly,
        # so that any updating mechanisms work correctly.
        mod <- parse_remote_last_modified(res)
        if (!is.null(mod)) {
            Sys.setFileTime(target, mod)
        }

        file.rename(tempf, target) # this should be more or less atomic, so no need for locks.
    }

    target
}

#' @importFrom utils head
decompress_lines <- function(path) {
    handle <- gzfile(path, "rb")
    on.exit(close(handle))
    lines <- readLines(handle)
    if (lines[length(lines) - 1] == "") {
        head(lines, -1L)
    } else {
        lines
    }
}

compute_ranges <- function(bytes) {
    c(0L, cumsum(as.integer(bytes) + 1L)) # +1 for the newline.
}

retrieve_ranges <- function(name, fetch, fetch.args) {
    if (is.null(fetch)) {
        fetch <- downloadIndexFile
    }
    path <- do.call(fetch, c(list(paste0(name, ".ranges.gz")), fetch.args))
    contents <- decompress_lines(path)
    compute_ranges(contents)
}

retrieve_ranges_with_sizes <- function(name, fetch, fetch.args) {
    if (is.null(fetch)) {
        fetch <- downloadIndexFile
    }
    path <- do.call(fetch, c(list(paste0(name, ".ranges.gz")), fetch.args))

    contents <- decompress_lines(path)
    parsed <- strsplit(contents, "\t")
    bytes <- vapply(parsed, function(x) x[1], "")
    extra <- vapply(parsed, function(x) x[2], "")

    list(ranges=compute_ranges(bytes), sizes=as.integer(extra))
}

retrieve_ranges_with_names <- function(name, fetch, fetch.args) {
    if (is.null(fetch)) {
        fetch <- downloadIndexFile
    }
    path <- do.call(fetch, c(list(paste0(name, ".ranges.gz")), fetch.args))

    contents <- decompress_lines(path)
    parsed <- strsplit(contents, "\t")
    names <- vapply(parsed, function(x) x[1], "")
    bytes <- vapply(parsed, function(x) x[2], "")

    list(names=names, ranges=compute_ranges(bytes))
}

decode_indices <- function(lines) {
    parsed <- strsplit(lines, "\t", fixed=TRUE)
    lapply(parsed, function(x) cumsum(as.integer(x)) + 1L)
}
