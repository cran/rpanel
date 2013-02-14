\name{rp.grid}

\alias{rp.grid}

\title{Define a subsidiary grid within an rpanel}

\description{A subsidiary grid is defined at a specified location within an rpanel.}

\usage{
rp.grid(panel, name=paste("grid", .nc(), sep=""), pos=NULL, background=NULL, 
  parentname=deparse(substitute(panel)), ...) 
}

\arguments{
  \item{panel}{the panel to which the grid should be attached.}
  \item{name}{a string defining the name of the grid. For use with \code{\link{rp.widget.dispose}}}
  \item{pos}{See the help information on "grid" mode in \code{\link{rp.pos}}, for more information. }
  \item{background}{a character variable defining a background colour.  (This is not the same as colours in \R, but simple colours are available.)}
  \item{parentname}{this specifies the widget inside which the grid should appear.}
  \item{...}{...}
}

\note{
The former argument \code{parent} has been discontinued in version 1.1, while the argument \code{bg} has been renamed \code{background} for consistency with the other functions.
}

\details{
The role of this function is to specify a subsidiary grid at a particular row and column position of the parent grid.  Nesting of grids within grids is permitted.  See the help information on "grid" mode in \code{\link{rp.pos}} for a description of the settings of the \code{pos} argument.
}

\references{
rpanel: Simple interactive controls for \R functions using the tcltk package. Journal of Statistical Software, 17, issue 9.
}

\examples{
\dontrun{
panel <- rp.control()
rp.grid(panel, pos=list(row=0, column=0, sticky="news"), background="red", name="g0")
rp.grid(panel, pos=list(row=1, column=1, sticky="news", width=100, height=100), background="navy", name="g1")
rp.grid(panel, pos=list(row=2, column=2, sticky="news", width=150, height=200), background="green", name="g2")
rp.button(panel, function(panel) { panel }, "press A", pos=list(row=1, column=1, sticky=""), parentname="g1")
rp.button(panel, function(panel) { panel }, "press B", pos=list(row=2, column=2, sticky="news"), parentname="g1")
rp.button(panel, function(panel) { panel }, "press C", pos=list("left",width=50, height=150), parentname="g2")
rp.grid(panel, pos=list(row=0, column=0, sticky="", width=10, height=10), background="yellow", parentname="g0")
}}

\keyword{iplot}
\keyword{dynamic}
