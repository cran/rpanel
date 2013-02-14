\name{rp.bubbleplot}

\alias{rp.bubbleplot}

\title{Animated scatterplot}

\description{
  This function produces a scatterplot of two variables, with the values of third and fourth variables represented by size and colour of the plotted points.  In addition, the scatterplot is animated over a fifth variable, such as time.
}

\usage{
  rp.bubbleplot(x, y, year, size, col, col.palette = topo.colors(20),
                     interpolate = FALSE, fill.in = FALSE, labels = rownames(x),
                     hscale = 1, vscale = hscale)
}

\arguments{
\item{x}{a matrix of values, whose columns correspond to time points, to be plotted on the horizontal axis.}
\item{y}{a matrix of values, whose columns correspond to time points, to be plotted on the vertical axis.}
\item{year}{a vector of values, usually years, over which the scatterplot will be animated.  The values in this vector correspond to the columns of \code{x} and \code{y}.}
\item{size}{a vector or matrix of values used to scale the sizes of the plotted points.}
\item{col}{a vector or matrix of values which will be translated into the colours of the plotted points.}
\item{col.palette}{the colour palette used to colour the points.}
\item{interpolate}{a logical variable controlling whether interpolation is used to create data for plotting at year values which do not correspond to an exact values of \code{year}.}
\item{fill.in}{a logical variable which controls whether gaps resulting from missing data are filled in with the largest previous value.}
\item{labels}{the labels of the plotted points, used to highlight individual points on the scatterplot.}
\item{hscale, vscale}{scaling parameters for the size of the plot when \code{panel.plot} is set to \code{TRUE}.}
}

\details{
  This plot mimics the plots made famous by Hans Rosling through the Gapminder project (see \url{www.gapminder.org}).  This type of plot is also available at the Google Public Data Explorer (\url{www.google.com/publicdata/directory}).  The aim of this function is to make this type of plot available directly from within R.  The controls provide a slider or button for animation, plus a list of country names for individual identification.
}

\value{
  Nothing is returned.
}

\references{
   rpanel: Simple interactive controls for R functions using the tcltk package.
      Journal of Statistical Software, 17, issue 9.
   }

\examples{
\dontrun{
   rp.bubbleplot(log(gdp), log(co2.emissions), 1960:2007, size = population, 
      col = life.expectancy, interpolate = TRUE)
}}

\keyword{iplot}
\keyword{dynamic}
