\name{rp.datalink}

\alias{rp.datalink}

\title{Links for datasets}

\description{
Links to specific remote datasets are provided. These can be read from the source locations or downloaded to a local directory for convenience.
}

\usage{
  rp.datalink(name, action = "retrieve filename")
  }

\arguments{
  \item{name}{When \code{action = "set local directory"}, this should be a single character string giving the location of the local directory to be used for downloading datasets. When \code{action = "download"}, this should be a character vector of dataset names for downloading to the local directory. The special case \code{name = "all"} will download all datasets. When \code{action = "retrieve filename"}, this should be a single character string giving the name of the dataset whose location is required.}
  \item{action}{a character string taking the values "set local directory", "download" or "retrieve filename". The actions performed by these settings are indicated in the description of the \code{name} argument above.}
  }

\details{
Calling the function with no arguments, as \code{rp.datalink()}, will print the dataset names, the file extensions and the remote link.

When \code{action = "retrieve filename"} but the file has not been downloaded to a local directory, the file is downloaded to a temporary file and the name of this temporary file is returned.

The specification of the local directory can be removed by setting the \code{name} argument to \code{NULL}.

The file associated with `name` can be a zip file, in which case the \code{unz} function can be used to locate the specific file of interest. The Examples below illustrate this. Some functions to read data require a specific path to the file of interest, rather than a connection. In this case, the \code{unzip} function can be used to unzip the files to a temporary (using \code{tempdir}) or other specified directory first.
}

\value{
  If the function is called with no arguments, the dataset names, the file extensions and the remote link re returned in a dataframe. If \code{action = "set local directory"}, the name of the local directory is returned. When \code{action = "download"}, the names of the local files created are returned. When \code{action = "retrieve filename", the location of the specified dataset is returned.}
  }

\examples{
\dontrun{
  rp.datalink()
  rp.datalink("~/Desktop/temp", "set local directory")
  rp.datalink("global_temperature")
  
  # Reading from a file within a zip file
  zfile   <- rp.datalink("children_services")
  dirname <- "Data and Code CSRR Strand 3 - August 2023 Update"
  path <- unz(zfile, paste(dirname, "Example_Indicator_CSR.csv", sep = "/"))
  if (!requireNamespace("readr", quietly = TRUE)) 
    indicator <- read_csv(path, skip = 6, col_names = c("LA", 2011:2021))
  }}
