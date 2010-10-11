\name{river}

\alias{river}

\title{Temperature and DO threshold in the River Clyde}

\description{
These data record the water temperature at a sampling station on the River Clyde,
together with an indicator of whether (1) or not (0) the concentration of
dissolved oxygen fell below the threshold of 5 percent.

The data are used in the \code{\link{rp.logistic}} example script. 

The data were kindly provided by the Scottish Environment Protection Agency,
with the assitance of Dr. Brian Miller.
}

\references{
   rpanel: Simple interactive controls for R functions using the tcltk package.
      Journal of Statistical Software, 17, issue 9.
   }

\examples{
if (interactive()) {
  data(river)
  attach(river)
  rp.logistic(Temperature, Low)
  }}

\keyword{iplot}
\keyword{dynamic}

