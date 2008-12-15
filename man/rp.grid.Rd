\name{rp.grid}

\alias{rp.grid}

\title{Define a subsidiary grid within an rpanel}

\description{A subsidiary grid is defined at a specified row and column location within an rpanel.}

\usage{rp.grid(panel, name, pos, bg = NULL, parent = window, \dots)}

\arguments{\item{panel}{the panel in which the slider appears. This may be passed as a panelname string or the panel object itself.}

\item{name}{a string defining the name of the grid.}

\item{pos}{a list containing components which define various features of the grid; see the help information on "grid" mode in \code{\link{rp.pos}}, for more information.  These components can also be specified as additional arguments in \dots.}

\item{bg}{a character variable defining a background colour.  (This is not the same as colours in \R, but simple colours are available.)}

\item{parent}{this specifies the widget inside which the grid should appear. In the current version of rpanel, it should not normally be used.}

\item{\dots}{any other arguments will be assumed to refer to components of \code{pos}; see the help information on "grid" mode in \code{\link{rp.pos}} for details.}
}

\details{The role of this function is to specify a subsidiary grid at a particular row and column position of the parent grid.  Nesting of grids within grids is permitted.  See the help information on "grid" mode in \code{\link{rp.pos}} for a description of the settings of the \code{pos} argument.
}

\value{If the parameter panel is the panelname string the same string is returned. If the panel object is used the altered panel is assigned to both the calling level and panel's environment level.}

\references{
rpanel: Simple interactive controls for \R functions using the tcltk package. Journal of Statistical Software, 17, issue 9.
}

\examples{
if (interactive()) {
   rp.pos("grid")
   }}

\keyword{iplot}
\keyword{dynamic}
