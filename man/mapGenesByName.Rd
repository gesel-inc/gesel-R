% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mapGenesByName.R
\name{mapGenesByName}
\alias{mapGenesByName}
\title{Map gene names to indices}
\usage{
mapGenesByName(species, type, ignore.case = FALSE, config = NULL)
}
\arguments{
\item{species}{String specifying the taxonomy ID of the species of interest.}

\item{type}{String specifying the type of name.
This is typically one of \code{"symbol"}, \code{"entrez"}, and \code{"ensembl"},}

\item{ignore.case}{Logical scalar indicating whether case should be ignored.}

\item{config}{Configuration list, typically created by \code{\link{newConfig}}.
If \code{NULL}, the default configuration is used.}
}
\value{
Named list of integer vectors.
Each name corresponds to an name of the specified \code{type},
and each vector contains the genes associated with that name (after ignoring case, if \code{ignore.case=TRUE}).
Vector entries should be interpreted as indices into any of the lists returned by \code{\link{fetchAllGenes}}.
}
\description{
Create a mapping of gene names (Ensembl, symbol, etc.) to their gene indices.
}
\examples{
mapping <- mapGenesByName("9606", type="symbol")

# Taking it for a spin:
found <- mapping[["SNAP25"]]
fetchAllGenes("9606")$symbol[found]

}
\author{
Aaron Lun
}
