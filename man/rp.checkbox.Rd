\name{rp.checkbox}

\alias{rp.checkbox}

\title{Check Box widget for rpanel}

\description{
  Adds a checkbox to the panel, to control a logical variable.
}

\usage{
rp.checkbox(panel, var, action = I, parent = window, title = deparse(substitute(var)), initval = NULL, pos = NULL)
}

\arguments{
  \item{panel}{the panel in which the checkbox should appear. This 
  may be passed as a panelname string or the panel object itself.}
  \item{var}{the name of the variable within the panel that the checkbox should control.}
  \item{action}{the function to call whenever the checkbox is clicked.}
  \item{title}{the title of the checkbox. This defaults to the name of the variable var.}
  \item{parent}{this specifies the widget inside which the checkbox should appear. In the current version, it should not normally be used.}
  \item{initval}{the initial value for the variable var (optional). The initial value can also be specified in the call to \code{rp.control}.}
  \item{pos}{the layout instructions. Please see the \code{\link{rp.pos}} example and help for full details.}
}

\details{
  The function \code{action} should take one argument, which should be the panel to which the checkbox is attached. 
}

\value{
  If the parameter panel is the panelname string the same string is returned. If the 
  panel object is used the altered panel is assigned to both the calling level and global 
  level.
}

\section{Warning}{
  The \code{action} function should return the panel. 
  Without this assignment any widgets added or alterations made to panel parameters within 
  the \code{action} function will be lost.
}

\references{rpanel: Simple interactive controls for R functions using 
the tcltk package 
(http://www.stats.gla.ac.uk/~adrian/research-reports/rpanel.ps)}

\seealso{\code{\link{rp.radiogroup}},\code{\link{rp.control}}}

\examples{
oncheck <- function(panel) {
  if (panel$cb) cat("Box is checked\n")
  else cat("Not checked!\n")
  panel
}
panel <- rp.control()
rp.checkbox(panel, cb, action = oncheck)
}

\keyword{iplot}
\keyword{dynamic}
