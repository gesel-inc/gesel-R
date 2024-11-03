#' Search set text
#'
#' Search for sets based on their names and descriptions.
#'
#' @inheritParams fetchSetsForGene
#' @param query String containing one or more words to search on.
#' A set is only matched if it matches to all of the tokens in the query.
#' The \code{*} and \code{?} wildcards can be used to match to any or one character, respectively.
#' @param use.name Logical scalar indicating whether to search on the name of the set.
#' @param use.description Logical scalar indicating whether to search on the description of the set.
#'
#' @return Integer vector of set indices for the matching gene sets.
#' Each index refers to a row in the data frame returned by \code{\link{fetchAllSets}}.
#'
#' @author Aaron Lun
#' @examples
#' out <- searchSetText("9606", "cancer")
#' fetchSingleSet("9606", out[1])
#' 
#' out <- searchSetText("9606", "innate immun*")
#' fetchSingleSet("9606", out[1])
#' 
#' @export
searchSetText <- function(
    species,
    query,
    use.name = TRUE, 
    use.description = TRUE,
    fetch.file = NULL,
    fetch.file.args = list(),
    fetch.range = NULL,
    fetch.range.args = list()) 
{
    # Don't use tokenize() here, as we need to preserve ? and *.
    tokens <- gsub("[^a-zA-Z0-9?*-]", " ", tolower(query))
    tokens <- unique(unlist(strsplit(tokens, "\\s+")))
    tokens <- setdiff(tokens, c("", "-"))

    gathered <- vector("list", length(tokens))

    if (use.name) {
        for (i in seq_along(tokens)) {
            gathered[[i]] <- fetch_sets_by_token(
                species,
                tokens[i],
                "names", 
                fetch.file=fetch.file,
                fetch.file.args=fetch.file.args,
                fetch.range=fetch.range,
                fetch.range.args=fetch.range.args
            )
        }
    }

    if (use.description) {
        for (i in seq_along(tokens)) {
            found <- fetch_sets_by_token(
                species,
                tokens[i],
                "descriptions", 
                fetch.file=fetch.file,
                fetch.file.args=fetch.file.args,
                fetch.range=fetch.range,
                fetch.range.args=fetch.range.args
            )
            gathered[[i]] <- union(gathered[[i]], found)
        }
    }

    Reduce(intersect, gathered)
}

searchSetText.env <- new.env()
searchSetText.env$names <- list()
searchSetText.env$descriptions <- list()

fetch_sets_by_token <- function(species, token, type, fetch.file, fetch.file.args, fetch.range, fetch.range.args) {
    if (is.null(token)) {
        return(integer())
    }

    cached <- get(type, envir=searchSetText.env, inherits=FALSE)
    sfound <- cached[[species]]
    if (token %in% names(sfound$prior)) {
        return(sfound$prior[[token]])
    }

    fname <- sprintf("%s_tokens-%s.tsv", species, type)
    if (is.null(sfound)) {
        sraw <- retrieve_ranges_with_names(fname, fetch=fetch.file, fetch.args=fetch.file.args)
        sfound$ranges <- sraw$ranges
        sfound$order <- sraw$names
        sfound$prior <- list()
    }
    ordered <- sfound$order
    ranges <- sfound$ranges

    if (is.null(fetch.range)) {
        fetch.range <- downloadIndexRange
    }

    output <- integer()
    if (grepl("[*?]", token)) {
        regex <- token
        regex <- gsub("[*]", ".*", regex)
        regex <- gsub("[?]", ".", regex)
        regex <- paste0("^", regex)

        relevant <- grep(regex, ordered)
        if (length(relevant)) {
            is.uncached <- !(relevant %in% names(sfound$prior))

            if (any(is.uncached)) {
                uncached <- relevant[is.uncached]
                lines <- lapply(uncached, function(t) {
                    do.call(fetch.range, c(list(fname, ranges[t + 0:1]), fetch.range.args))
                })
                decoded <- decode_indices(unlist(lines))
                names(decoded) <- ordered[uncached]
                sfound$prior <- c(sfound$prior, decoded)
            }

            output <- unique(unlist(sfound$prior[ordered[relevant]]))
        }

    } else {
        t <- match(token, ordered)
        if (!is.na(t)) {
            text <- do.call(fetch.range, c(list(fname, ranges[t + 0:1]), fetch.range.args))
            output <- decode_indices(text)[[1]]
        }
    }

    sfound$prior[[token]] <- output
    cached[[species]] <- sfound
    assign(type, value=cached, envir=searchSetText.env)
    as.integer(output)
}
