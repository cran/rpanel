\name{aircond}

\alias{aircond}

\title{Intervals between the failure of air-conditioning equipment in aircraft}

\description{
These data, reported by Proschan (1963, Technometrics 5, 375-383), refer to
the intervals, in service-hours, between failures of the air-conditioning
equipment in a Boeing 720 aircraft.  (Proschan reports data on 10 different
aircraft.  The data from only one of the aircraft is used here.  
Cox and Snell (1981, Applied Statistics: principles and examples,
Chapman and Hall, London) discuss the analysis of the data on all 10 aircraft.)

The dataset consists of a single vector of data.

The data are used in the rp.likelihood example script.

}

\references{
   rpanel: Simple interactive controls for R functions using the tcltk package.
      Journal of Statistical Software, 17, issue 9.
   }

\examples{
if (interactive()) {
   data(aircond)
   hist(aircond)
   rp.likelihood("dexp(data, theta)", aircond, 0.005, 0.03)
   rp.likelihood("dgamma(data, theta[1], theta[2])",
        aircond, c(0.3, 0.005), c(3, 0.06))
}}

\keyword{iplot}
\keyword{dynamic}

