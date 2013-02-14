\name{rp.power}

\alias{rp.power}

\title{Interactive power calculations for a two-sample t-test }

\description{
This function creates a panel which allows the sample size, population means and common standard deviation to be set.  The correponding power curve for a two-sample t-test is displayed in the graphics window.
}

\usage{
rp.power(panel = TRUE, panel.plot = TRUE, populations.showing = FALSE,
         ngrid = seq(10, 300), mu1 = 0, mu2 = 1,
         sigma = 1, n = 20, xgrid = seq(- 4, 5, length = 100),
         popdens.lim = 0.7, hscale = 1, vscale = hscale)
}

\arguments{
\item{panel}{a logical value determining whether an interactive panel is created.}
\item{panel.plot}{a logical value determining whether the plot is placed inside the panel.}
\item{populations.showing}{a logical value determining whether the populations are initially showing.}
\item{ngrid}{a vector which determines the grid a sample sizes used.}
\item{mu1,mu2}{the initial values of the means of the two populations.}
\item{sigma}{the initial value of the common standard deviation of the two populations.}
\item{n}{the initial value of the sample size.}
\item{xgrid}{the grid of values over which the populations are plotted.}
\item{popdens.lim}{the upper limit on the population density scale.}
\item{hscale, vscale}{scaling parameters for the size of the plot.}
}

\details{
The population parameters and sample size are controlled by doublebuttons.  The sample size refer to the total sample size, assuming two groups of equal size.  A checkbox allows plots of the population distributions also to be diplayed.}

\value{
  Nothing is returned.
}

\references{
   rpanel: Simple interactive controls for R functions using the tcltk package.
      Journal of Statistical Software, 17, issue 9.
   }

\examples{
\dontrun{
  rp.power()
}}

\keyword{iplot}
\keyword{dynamic}
