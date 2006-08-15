\name{rp.slider}

\alias{rp.slider}

\title{Slider for an rpanel}

\description{
  Add a slider to the panel, to graphically control a numeric variable.}

\usage{
rp.slider(panel, var, from, to, action = I, title = deparse(substitute(var)), 
  log = FALSE, resolution = 0, initval = NULL, parent = window, pos = NULL)
}

\arguments{
  \item{panel}{the panel in which the slider appears. This 
  may be passed as a panelname string or the panel object itself.}
  \item{var}{the name of the variable that the slider controls.}
  \item{from}{the lower limit of the range of values to which the slider can be set.}
  \item{to}{the upper limit of the range of values to which the slider can be set.}
  \item{log}{a logical variable which controls whether the scale of the slider is logarithmic.}
  \item{resolution}{the resolution of the slider scale. If > 0, all values are rounded to an even multiple of this value. The default is 0.}
  \item{action}{the function which is called when the slider is moved.}
  \item{title}{the label of the slider.}
  \item{initval}{the initial value of var (optional). The initial value can also be specified in the call to \code{rp.control}.}
  \item{parent}{this specifies the widget inside which the slider should appear. In the current version of rpanel, it should not normally be used.}
  \item{pos}{the layout instructions. Please see the \code{\link{rp.pos}} example and help for full details.}
}

\details{
  The function \code{action} should take one argument, which should be the panel to which the slider is attached. 
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
the tcltk library 
(http://www.stats.gla.ac.uk/~adrian/research-reports/rpanel.ps)}

\seealso{\code{\link{rp.radiogroup}},\code{\link{rp.control}}}

\examples{
density.draw <- function(panel) {
  plot(density(panel$x, bw = panel$h))
  panel
}
panel <- rp.control(x = rnorm(50))
rp.slider(panel, h, 0.5, 5, log = TRUE, action = density.draw)
}

\keyword{iplot}
\keyword{dynamic}
