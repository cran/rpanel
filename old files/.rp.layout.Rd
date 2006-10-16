\name{.rp.layout}

\alias{.rp.layout}

\title{Choose the layout for widgets.}

\description{
  This sets the layout for widgets having added them to the window. This is an internal function and a public one is in development.
}

\usage{
.rp.layout(widget, pos, default=NULL)
}

\arguments{
  \item{widget}{The widget to apply the layout to. }
  \item{pos}{The position of the widget. Values include; NULL (take space available), "default_left_align", "left", "right", "top", "bottom" or as a vector with four values for x, y, width and height in that order.}
  \item{default}{Overrides pos should it be NULL.}
}

\value{
  None returned.
}

\references{rpanel: Simple interactive controls for R functions using 
the tcltk library 
(http://www.stats.gla.ac.uk/~adrian/research-reports/rpanel.ps)}

\seealso{\code{\link{rp.control}}

\keyword{iplot}
\keyword{dynamic}
