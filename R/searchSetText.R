#' Search set text
#'
#' Search for sets based on their names and descriptions.
#'
#' @inheritParams fetchSetsForSomeGenes
#' @param query String containing one or more words to search on.
#' A set is only matched if it matches to all of the tokens in the query.
#' The \code{*} and \code{?} wildcards can be used to match to any or one character, respectively.
#' @param use.name Logical scalar indicating whether to search on the name of the set.
#' @param use.description Logical scalar indicating whether to search on the description of the set.
#'
#' @return Integer vector of set indices for the matching gene sets.
#' Each set index refers to a row in the data frame returned by \code{\link{fetchAllSets}}.
#'
#' @author Aaron Lun
#' @examples
#' out <- searchSetText("9606", "cancer")
#' fetchSomeSets("9606", out[1])
#' 
#' out <- searchSetText("9606", "innate immun*")
#' fetchSomeSets("9606", out[1])
#' 
#' @export
searchSetText <- function(
    species,
    query,
    use.name = TRUE, 
    use.description = TRUE,
    fetch.file = downloadDatabaseFile,
    fetch.file.args = list(),
    fetch.range = downloadDatabaseRanges,
    fetch.range.args = list()) 
{
    # Don't use tokenize() here, as we need to preserve ? and *.
    tokens <- gsub("[^a-zA-Z0-9?*-]", " ", tolower(query))
    tokens <- unique(unlist(strsplit(tokens, "\\s+")))
    tokens <- setdiff(tokens, c("", "-"))

    gathered.names <- vector("list", length(tokens))
    if (use.name) {
		gathered.names <- fetch_sets_by_token(
			species,
			tokens,
			"names", 
			fetch.file=fetch.file,
			fetch.file.args=fetch.file.args,
			fetch.range=fetch.range,
			fetch.range.args=fetch.range.args
		)
    }

    gathered.descriptions <- vector("list", length(tokens))
    if (use.description) {
		gathered.descriptions <- fetch_sets_by_token(
			species,
			tokens,
			"descriptions", 
			fetch.file=fetch.file,
			fetch.file.args=fetch.file.args,
			fetch.range=fetch.range,
			fetch.range.args=fetch.range.args
		)
    }

	gathered <- mapply(c, gathered.names, gathered.descriptions, SIMPLIFY=FALSE)
    as.integer(Reduce(intersect, gathered))
}

fetch_sets_by_token <- function(species, tokens, type, fetch.file, fetch.file.args, fetch.range, fetch.range.args) {
    cached <- get(type, envir=searchSetText.env, inherits=FALSE)
    sfound <- cached[[species]]

    fname <- sprintf("%s_tokens-%s.tsv", species, type)
    if (is.null(sfound)) {
        sraw <- retrieve_ranges_with_names(fname, fetch=fetch.file, fetch.args=fetch.file.args)
        sfound$ranges <- sraw$ranges
        sfound$names <- sraw$names
        sfound$prior <- list()
    }
    snames <- sfound$names
    prior <- sfound$prior

    # Finding the unique set of all tokens that haven't been resolved yet.
    to.request <- integer(0)
    partial.request <- list()
    for (needed.token in setdiff(tokens, names(prior))) {
        if (grepl("[*?]", needed.token)) {
            regex <- needed.token
            regex <- gsub("[*]", ".*", regex)
            regex <- gsub("[?]", ".", regex)
            regex <- paste0("^", regex, "$")

            relevant <- grep(regex, snames)
            relevant.names <- snames[relevant]
            partial.request[[needed.token]] <- relevant.names

            if (length(relevant)) {
                is.uncached <- !(relevant.names %in% names(prior))
                if (any(is.uncached)) {
                    to.request <- union(to.request, relevant[is.uncached])
                }
            }
        } else {
            m <- match(needed.token, snames)
            if (!is.na(m)) {
                to.request <- union(to.request, m)
            } else {
                prior[[needed.token]] <- integer(0)
            }
        }
    }

	# Making a parallelized set of to.request for anything that we're missing.
    if (length(to.request)) {
        if (is.null(fetch.range)) {
            fetch.range <- downloadIndexRange
        }
        ranges <- sfound$ranges
        starts <- ranges[to.request]
        ends <- ranges[to.request + 1L]
        deets <- do.call(fetch.range, c(list(name=fname, start=starts, end=ends), fetch.range.args))
        requested.indices <- decode_indices(deets)

        names(requested.indices) <- snames[to.request] 
        prior <- c(prior, requested.indices)
    }

	# Filling up the caches for subsequent queries.
    for (needed.token in names(partial.request)) {
        needed.actual.tokens <- partial.request[[needed.token]]
		prior.actual.tokens <- prior[needed.actual.tokens]
        prior[[needed.token]] <- unique(unlist(prior.actual.tokens))
    }

    sfound$prior <- prior
    cached[[species]] <- sfound
    assign(type, value=cached, envir=searchSetText.env)

    prior[tokens]
}

searchSetText.env <- new.env()
searchSetText.env$names <- list()
searchSetText.env$descriptions <- list()
