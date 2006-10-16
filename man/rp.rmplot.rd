\name{rp.rmplot}

\alias{rp.rmplot}

\title{Interactive plotting of repeated measurement data }

\description{
  This function creates a panel which controls the display of
  data which have a repeated measurement structure across time.
  Groups, means and stadnard errors can be displayed.  Individual
  profiles can also be inspected. 
}

\usage{
rp.rmplot(y, id = NA, timept = NA, fac = NA, xlab = NA, ylab = NA)
}

\arguments{
  \item{y}{a vector or matrix of response data.  If y is a matrix, the rows should correspond to cases and the columns to the repeated measurements.}
  \item{id}{when y is a vector, id should contain the identifiers for the individual profiles.}
  \item{timept}{when y is a vector, timept should contain the time value associated with each measurement.}
  \item{fac}{factor.}
  \item{xlab}{x-axis label.}
  \item{ylab}{y-axis label.}
}

\details{
  This function is designed principally for repeated measurements over
  time, with common time points for each profile.  A set of radiobuttons
  allows all the individual profiles to be plotted, or summaries in the
  form of means and two standard errors.  A checkbox allows the data to
  be split inot groups identified by the variable fac.  When there are
  only two groups, a band can be displayed to indicate time points
  at which the distance between the observed means is more than two
  standard errors of the differences between the means.
}

\value{
  the name of the panel object.
}

\references{rpanel: Simple interactive controls for R functions using
the tcltk library
(http://www.stats.gla.ac.uk/~adrian/rpanel/)}

\examples{
data(luthor)

LH <- luthor[,2:16]
gp     <- factor(luthor[,1])
times  <- c(1:5,(5+(1:10)/2))
rp.rmplot(log(LH), fac = gp, timept = times)
}

\keyword{iplot}
\keyword{dynamic}
