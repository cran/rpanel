\name{rp.button}

\alias{rp.button}

\title{Button widget for rpanel}

\description{
  This function adds a button to the panel. A nominated function is called when the button is pressed.
}

\usage{
rp.button(panel, action, title = deparse(substitute(action)), id = "", parent = window, repeatdelay = 0, repeatinterval = 0, pos = NULL)
}

\arguments{
  \item{panel}{the panel in which the button should appear. This 
  may be passed as a panelname string or the panel object itself.}
  \item{action}{the function executed when the button is pressed.}
  \item{title}{the text displayed on the button.}
  \item{id}{the name of the button. This is helpful in allowing possible modification and layout changes which have not been implemented and will be considered later.}
  \item{parent}{this specifies the widget inside which the button should appear. In the current version of rpanel, it should not normally be used.}
  \item{repeatinterval}{the interval between auto-repeats (milliseconds) when the button is held down.}
  \item{repeatdelay}{the time after which the button starts to auto-repeat (milliseconds).}
  \item{pos}{the layout instructions. Please see the \code{\link{rp.pos}} example and help for full details.}
}

\details{
  The function \code{action} should take one argument, which should be the panel. 
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

\seealso{\code{\link{rp.doublebutton}},\code{\link{rp.control}}}

\examples{
# This function will be called on pressing the button "Simulate".
boxp.sim <- function(panel) {
  boxplot(rnorm(50))
  panel
}
# Create an rpanel and add the button "Simulate" to it.
panel <- rp.control()
rp.button(panel, action = boxp.sim, title = "Simulate")
}

\keyword{iplot}
\keyword{dynamic}
