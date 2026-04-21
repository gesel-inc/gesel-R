#' Search set text
#'
#' Search for sets based on their names and descriptions.
#'
#' @inheritParams fetchSetsForSomeGenes
#' @param query String containing one or more words to search on.
#' A set is only matched if it matches to all of the tokens in the query.
#' The \code{*} and \code{?} wildcards can be used to match to any or one character, respectively.
#' @param use.name Boolean indicating whether to search on the name of the set.
#' @param use.description Boolean indicating whether to search on the description of the set.
#'
#' @return Integer vector of set indices for the matching gene sets.
#' Each set index refers to a row in the data frame returned by \code{\link{fetchAllSets}}.
#'
#' @author Aaron Lun
#' @examples
#' out <- searchSetText("9606", "cancer")
#' fetchSomeSets("9606", head(out))
#' 
#' out <- searchSetText("9606", "innate immun*")
#' fetchSomeSets("9606", head(out))
#' 
#' @export
searchSetText <- function(species, query, use.name = TRUE, use.description = TRUE, config = NULL) {
    # Don't use tokenize() here, as we need to preserve ? and *.
    tokens <- strsplit(tolower(query), "[^a-zA-Z0-9?*-]")
    tokens <- unique(unlist(tokens))
    tokens <- setdiff(tokens, c("", "-"))

    config <- get_config(config)

    gathered.names <- vector("list", length(tokens))
    if (use.name) {
        gathered.names <- fetch_sets_by_token(config, species, tokens, "names")
    }

    gathered.descriptions <- vector("list", length(tokens))
    if (use.description) {
        gathered.descriptions <- fetch_sets_by_token(config, species, tokens, "descriptions")
    }

    gathered <- mapply(union, gathered.names, gathered.descriptions, SIMPLIFY=FALSE)
    as.integer(Reduce(intersect, gathered))
}

fetch_sets_by_token <- function(config, species, tokens, type) {
    cached <- get_cache(config, "searchSetText", species)
    tfound <- cached[[type]]
    modified <- FALSE

    fname <- sprintf("%s_tokens-%s.tsv", species, type)
    if (is.null(tfound)) {
        sraw <- retrieve_ranges_with_names(config, fname)
        tfound$ranges <- sraw$ranges
        tfound$names <- sraw$names
        tfound$prior <- list()
        modified <- TRUE
    }
    tnames <- tfound$names
    prior <- tfound$prior

    # Finding the unique set of all tokens that haven't been resolved yet.
    to.request <- integer(0)
    partial.request <- list()
    for (needed.token in setdiff(tokens, names(prior))) {
        if (grepl("[*?]", needed.token)) {
            regex <- needed.token
            regex <- gsub("[*]", ".*", regex)
            regex <- gsub("[?]", ".", regex)
            regex <- paste0("^", regex, "$")

            relevant <- grep(regex, tnames)
            relevant.names <- tnames[relevant]
            partial.request[[needed.token]] <- relevant.names

            if (length(relevant)) {
                is.uncached <- !(relevant.names %in% names(prior))
                if (any(is.uncached)) {
                    to.request <- union(to.request, relevant[is.uncached])
                }
            }
        } else {
            m <- match(needed.token, tnames)
            if (!is.na(m)) {
                to.request <- union(to.request, m)
            } else {
                prior[[needed.token]] <- integer(0)
                modified <- TRUE
            }
        }
    }

    # Making a parallelized set of to.request for anything that we're missing.
    if (length(to.request)) {
        consolidated <- consolidateRanges(tfound$ranges, to.request, max.gap = consolidate_max_gap(config))
        consolidated.parts <- fetch_ranges(config, fname, consolidated$start, consolidated$end)
        newly.obtained <- consolidated$requested[!(tnames[consolidated$requested] %in% names(prior))]

        refined.parts <- refine_ranges(
            consolidated.parts,
            consolidated$start,
            consolidated$end,
            tfound$ranges[newly.obtained],
            tfound$ranges[newly.obtained + 1L] - 1L # omit the trailing newline.
        )

        requested.indices <- decode_indices_from_raw(refined.parts)
        names(requested.indices) <- tnames[newly.obtained] 
        prior <- c(prior, requested.indices)
        modified <- TRUE
    }

    # Filling up the caches for subsequent queries.
    for (needed.token in names(partial.request)) {
        needed.actual.tokens <- partial.request[[needed.token]]
        prior.actual.tokens <- prior[needed.actual.tokens]
        prior[[needed.token]] <- unique(unlist(prior.actual.tokens))
        modified <- TRUE
    }

    if (modified) {
        tfound$prior <- prior
        cached[[type]] <- tfound
        set_cache(config, "searchSetText", species, cached)
    }

    prior[tokens]
}
