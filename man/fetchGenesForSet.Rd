% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fetchGenesForSet.R
\name{fetchGenesForSet}
\alias{fetchGenesForSet}
\title{Fetch genes for a single set}
\usage{
fetchGenesForSet(
  species,
  set,
  fetch.file = NULL,
  fetch.file.args = list(),
  fetch.range = NULL,
  fetch.range.args = list(),
  use.preloaded = TRUE
)
}
\arguments{
\item{species}{String containing the NCBI taxonomy ID of the species of interest.}

\item{set}{Integer identifying the set of interest, as an index into the list of all sets for this species 
(i.e., the return value of \code{\link{fetchAllSets}}).}

\item{fetch.file}{Function that accepts the name of the file in the Gesel index and returns an absolute path to the file.
If \code{NULL}, it defaults to \code{\link{downloadIndexFile}}.}

\item{fetch.file.args}{Named list of arguments to pass to \code{fetch.file}.}

\item{fetch.range}{Function that accepts at least two arguments - 
the name of the file in the Gesel index, and an integer vector of length 2 containing the zero-indexed half-open byte range to extract from the file
(see \code{\link{downloadIndexRange}} for details).
It should return a string containing the contents of the specified byte range.
If \code{NULL}, it defaults to \code{\link{downloadIndexRange}}.}

\item{fetch.range.args}{Named list of arguments to pass to \code{fetch.file}.}

\item{use.preloaded}{Logical scalar indicating whether to use the preloaded value from a previous call to \code{\link{fetchGenesForAllSets}}.}
}
\value{
Integer vector containing the identities of the genes in \code{set},
where each integer is an index into \code{\link{fetchAllGenes}}.
}
\description{
Fetch genes for a single set in the Gesel index.
}
\examples{
first.set <- fetchGenesForSet("9606", 1)

# Genes in the first set:
head(fetchAllGenes("9606")$symbol[first.set])

}
\author{
Aaron Lun
}
