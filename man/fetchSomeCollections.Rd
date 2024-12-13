% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fetchSomeCollections.R
\name{fetchSomeCollections}
\alias{fetchSomeCollections}
\title{Fetch some collections}
\usage{
fetchSomeCollections(species, collections, config = NULL)
}
\arguments{
\item{species}{String containing the NCBI taxonomy ID of the species of interest.}

\item{collections}{Integer vector containing collection indices.
Each entry refers to a row of the data frame returned by \code{\link{fetchAllCollections}}).}

\item{config}{Configuration list, typically created by \code{\link{newConfig}}.
If \code{NULL}, the default configuration is used.}
}
\value{
Data frame with the same columns as the return value of \code{\link{fetchAllCollections}},
where each row corresponds to an entry of \code{collections}.
}
\description{
Fetch the details of some gene set collections from the Gesel database.
This can be more efficient than \code{\link{fetchAllCollections}} when only a few collections are of interest.
}
\examples{
fetchSomeCollections("9606", 1)

}
\author{
Aaron Lun
}
