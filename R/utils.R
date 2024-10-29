map.env <- new.env()
map.env$full <- list()

default_index_url <- "https://github.com/LTLA/gesel-feedstock/releases/download/indices-v0.2.1"
default_gene_url <- "https://github.com/LTLA/gesel-feedstock/releases/download/genes-v1.0.0"

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
        last_mod <- file.info(target)$mtime
        if (last_mod < Sys.time()) {
            req <- request(url)
            req <- req_method(req, "HEAD")
            req <- handle_error(req)
            res <- req_perform(req)
            remote_mod <- parse_remote_last_modified(res)
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

retrieve_ranges <- function(cache, url, overwrite) {
    path <- download_file(cache, paste0(url, ".ranges.gz"), overwrite)
    contents <- decompress_lines(path);
    c(0L, cumsum(as.integer(contents)))
}

#' @import httr2
range_request <- function(url, range) {
    req <- request(url)
    req <- req_headers(req, Range=paste0("bytes=", range[1], "-", range[2] - 1L)) # ignore the newline at the end.
    res <- req_perform(req)
    payload <- resp_body_string(res)
    substr(payload, 1L, diff(range))
}
