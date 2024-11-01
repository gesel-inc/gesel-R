% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fetchSingleCollection.R
\name{fetchSingleCollection}
\alias{fetchSingleCollection}
\title{Fetch a single collection}
\usage{
fetchSingleCollection(
  species,
  collection,
  fetch.file = NULL,
  fetch.file.args = list(),
  fetch.range = NULL,
  fetch.range.args = list(),
  use.preloaded = TRUE
)
}
\arguments{
\item{species}{String containing the NCBI taxonomy ID of the species of interest.}

\item{collection}{Integer identifying the collection of interest, as an index into the list of all collections for this species 
(i.e., the return value of \code{\link{fetchAllCollections}}).}

\item{fetch.file}{Function that accepts the name of the file in the Gesel index and returns an absolute path to the file.
If \code{NULL}, it defaults to \code{\link{downloadIndexFile}}.}

\item{fetch.file.args}{Named list of arguments to pass to \code{fetch.file}.}

\item{fetch.range}{Function that accepts at least two arguments - 
the name of the file in the Gesel index, and an integer vector of length 2 containing the zero-indexed half-open byte range to extract from the file
(see \code{\link{downloadIndexRange}} for details).
It should return a string containing the contents of the specified byte range.
If \code{NULL}, it defaults to \code{\link{downloadIndexRange}}.}

\item{fetch.range.args}{Named list of arguments to pass to \code{fetch.file}.}

\item{use.preloaded}{Logical scalar indicating whether to use the preloaded value from a previous call to \code{\link{fetchAllCollections}}.}
}
\value{
Named list containing:
\itemize{
\item \code{title}, string containing the title for the collection.
\item \code{description}, string containing a description for the collection.
\item \code{maintainer}, string containing the identity of the collection's maintainer.
\item \code{source}, string containing the source of origin of the collection.
\item \code{start}, integer scalar specifying the identity of the first gene set in this collection,
as an index into the return value of \code{\link{fetchAllSets}}.
\item \code{size}, integer scalar specifying the number of gene sets in the collection.
}
}
\description{
Fetch the details of a single collection from the Gesel index.
This is more efficient than \code{\link{fetchAllCollections}} when only a single collection is of interest.
}
\examples{
fetchSingleCollection("9606", 1)

}
\author{
Aaron Lun
}
