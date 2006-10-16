\name{rp.textentry}

\alias{rp.textentry}

\title{Text entry box for a panel}

\description{
  This function adds a box allows text to be entered.
}

\usage{
rp.textentry(panel, var, action = I, title = deparse(substitute(var)), 
initval = NULL, parent = window, pos = NULL)
}

\arguments{
  \item{panel}{the panel in which the text entry box should appear. This 
  may be passed as a panelname string or the panel object itself.}
  \item{var}{the name of the variable which will be assigned the text entered into the box.}
  \item{action}{the function which is called when the text has been entered.}
  \item{title}{the label for the text entry box.}
  \item{parent}{this specifies the widget inside which the text entry widget should appear. In the current version, it should not normally be used.}
  \item{initval}{the initial value for var (optional). The initial value can also be specified in the call to \code{rp.control}.}
  \item{pos}{the layout instructions. Please see the \code{\link{rp.pos}} example and help for full details.}
}

\details{
  The function \code{action} should take one argument, which should be the panel to which the textentry is attached. 
}

\value{
  If the parameter panel is the panelname string the same string is returned. If the 
  panel object is used the altered panel is assigned to both the calling level and panel's environment 
  level.
}

\section{Warning}{
  The \code{action} function should return the panel. 
  Without this assignment any widgets added or alterations made to panel parameters within 
  the \code{action} function will be lost.
}

\references{rpanel: Simple interactive controls for R functions using 
the tcltk library 
(http://www.stats.gla.ac.uk/~adrian/rpanel/)}

\seealso{\code{\link{rp.control}}}

\examples{
density.draw <- function(panel) {
  h <- as.numeric(panel$h.text)
  plot(density(panel$x, bw = h))
  panel
}
panel <- rp.control(panelname = "thepanel", x = rnorm(50))
rp.textentry(panel, h.text, density.draw, "bandwidth", initval = "0.5")          
rp.do(panel, density.draw)
}

\keyword{iplot}
\keyword{dynamic}
