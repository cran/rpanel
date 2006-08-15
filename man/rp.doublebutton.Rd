\name{rp.doublebutton}

\alias{rp.doublebutton}

\title{Double-button widget for rpanel}

\description{
  Adds a widget with '+' and '-' buttons, to increment and decrement a variable.
}

\usage{
rp.doublebutton(panel, var, step, title = deparse(substitute(var)), action = I, 
initval = NULL, range = c(NA, NA), log = FALSE, repeatinterval = 100, 
repeatdelay = 100, parent = window, pos = NULL)
}

\arguments{
  \item{panel}{the panel in which the doublebutton should appear. This may be passed as a panelname string or the panel object itself.}
  \item{var}{the name of the variable within the panel that the doublebutton should control.}
  \item{step}{the value by which the variable "var" is incremented or decremented on pressing a button. When log is TRUE this is a factor instead.}
  \item{action}{the function which is called when a button is pressed.}
  \item{title}{the label for the doublebutton. This defaults to the name of var.}
  \item{parent}{this specifies the widget inside which the this widget should appear. In the current version, it should not normally be used.}
  \item{initval}{the initial value for var (optional). The initial value can also be specified in the call to \code{rp.control}.}
  \item{range}{a 2-element numeric vector containing lower and upper limits for var. Use NA for no limit (upper and/or lower).}
  \item{log}{a logical variable which determines whether the increment (step) is multiplicative or additive.}
  \item{repeatinterval}{the interval between auto-repeats (milliseconds) when the button is held down.}
  \item{repeatdelay}{the time after which the button starts to auto-repeat (milliseconds).}
  \item{pos}{the layout instructions. Please see the \code{\link{rp.pos}} example and help for full details.}
}

\details{
  \code{action} should be a function of one argument, which should be the panel.  The panel can then be manipulated, and data stored in the panel may be used/modified, then the (optionally modified) panel must be returned.
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
density.draw <- function(panel) {
  plot(density(panel$x, bw = panel$h))
  panel
}
panel <- rp.control(x = rnorm(50))
rp.doublebutton(panel, var = h, step = 0.05, 
  title = "Density estimate", action = density.draw,
  range = c(0.1, 5), initval=1)
}

\keyword{iplot}
\keyword{dynamic}
