\name{rp.wrangle}

\alias{rp.wrangle}

\title{Collate and organise specific datasets}

\description{
Specific datasets are read and reorganised into a convenient form for analysis.
}

\usage{
  rp.wrangle(name)
  }

\arguments{
  \item{name}{a single character string giving the name of the dataset required.}
  }

\details{
Calling the function with no arguments, as \code{rp.wrangle()}, will print the names of the datasets available.

The \pkg{readxl}, \pkg{readr} and \pkg{dplyr} packages are used for some datasets.
}

\value{
The dataset required, returned as a dataframe or tibble. If the function is called with no arguments, the dataset names are returned in a dataframe along with appropriate comments.
}

\examples{
\dontrun{
  rp.wrangle()
  rp.wrangle("cofe_2019")
}}
