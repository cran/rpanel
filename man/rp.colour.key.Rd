\name{rp.colour.key}

\alias{rp.colour.key}

\title{Creates a colour key.}

\description{
A colour key is created using the specified colours (\code{cols}) and an axis defined by the specified breaks (\code{brks}).  This is usually an additional component of a panel which allows the colours on the main plot to be interpreted.  The function is used in that way in the function \code{rp.plot4d}.
}

\usage{
rp.colour.key(cols, brks, par.mar = c(5, 0, 4, 3) + 0.1, natural = TRUE, margin = FALSE)
}

\arguments{
\item{cols}{a vector of colours.}
\item{brks}{a vector of values which defines the positions on the axis between which each colour is placed.}
\item{par.mar}{a vector of four values which are passed to the \code{mar} argument of the \code{par} function to control the marginal space around the key.}
\item{natural}{a logical value which, when \code{TRUE}, causes the usual form of axis to be constructed from the values in \code{brks}.  When \code{natural} is \code{FALSE}, the values in \code{brks} are associated with a regularly spaced set of locations along the axis.}
\item{margin}{a logical value which determines whether a marginal plotting area is placed on the left of the key.  This can be useful in allowing relevant information to be plotted alongside the key, such as the confidence intervals in \code{rp.surface}.  Specifically, if \code{margin} is \code{FALSE}, the horizontal axis has range \code{c(0, 1)} while if \code{margin} is \code{TRUE} the the range is \code{c(-1, 1)}.  In both cases the key is plotted over the horizontal range \code{c(0, 1)}.}
}

\references{
   rpanel: Simple interactive controls for R functions using the tcltk package.
      Journal of Statistical Software, 17, issue 9.
   }

\examples{
\dontrun{
  key.plot <- function(panel) {
  	rp.colour.key(topo.colors(12), 0:12)
  	panel
  }
  panel <- rp.control()
  rp.tkrplot(panel, key, key.plot, hscale = 0.15)
}}

\keyword{iplot}
\keyword{dynamic}
