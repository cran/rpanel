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
  doublebuttons.  The sample size refer to the total sample size,
  assuming two groups of equal size.  A checkbox allows plots of the
  population distributions also to be diplayed.}

\value{
  the panel object.
}

\references{
   rpanel: Simple interactive controls for R functions using the tcltk package.
      Journal of Statistical Software, 17, issue 9.
   }

\examples{
if (interactive()) {
  rp.power()
  }}

\keyword{iplot}
\keyword{dynamic}
