\name{rp.power}

\alias{rp.power}

\title{Interactive power calculations for a two-sample t-test }

\description{
  This function creates a panel which allows the sample size,
  population means and common standard deviation to bet set.
  The correponding power curve is displayed in the graphics window.
}

\usage{
rp.power()
}

\arguments{
  None.
}

\details{
  The population parameters and sample size are controlled by
  doublebuttons.  The sample size refer to the common sample size
  of each of the two groups.  A checkbox allows plots of the
  population distributions also to be diplayed. }

\value{
  the panel object.
}

\references{rpanel: Simple interactive controls for R functions using
the tcltk library
(http://www.stats.gla.ac.uk/~adrian/research-reports/rpanel.ps)}

\examples{
  rp.power()
}

\keyword{iplot}
\keyword{dynamic}
