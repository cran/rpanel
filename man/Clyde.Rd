\name{Clyde}

\alias{Clyde}

\title{Water quality in the River Clyde}

\description{
These data record the water quality, in terms of dissolved oxygen (\code{DO}) on a percentage scale, at a number of sampling stations (\code{Station}) on the River Clyde.  The date (\code{Day}, \\code{Month}, \code{Year}) is also available, along with the day of the year (\code{Doy} between 1 and 365) and an identified (\code{id}) of the survey on which each measurement was made.

The data are used in the \code{\link{rp.plot4d}} example script. 

The data were kindly provided by the Scottish Environment Protection Agency,
with the assistance of Dr. Brian Miller.
}

\references{
   rpanel: Simple interactive controls for R functions using the tcltk package.
      Journal of Statistical Software, 17, issue 9.
   }

\examples{
\dontrun{
  with(Clyde, {
    rp.plot4d(cbind(Doy, DO), Station, location.plot = FALSE)
    rp.plot4d(cbind(Station, DO), Doy, location.plot = FALSE)
  })
}}

\keyword{iplot}
\keyword{dynamic}

