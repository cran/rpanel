\name{rp.button}

\alias{rp.button}

\title{Button control for rpanel}

\description{
  This function adds a button to the panel. A nominated function is called when the button is pressed.
}

\usage{
rp.button(panel, action, title=deparse(substitute(action)), repeatdelay=0, repeatinterval=0, quitbutton=FALSE, 
  pos=NULL, foreground=NULL, background=NULL, font=NULL, parentname=deparse(substitute(panel)), 
  name=paste("button", .nc(), sep=""), ...)
}

\arguments{
  \item{panel}{the panel in which the button should appear.}
  \item{action}{the function executed when the button is pressed.}
  \item{title}{the text displayed on the button.}
  \item{repeatinterval}{the interval between auto-repeats (milliseconds) when the button is held down.}
  \item{repeatdelay}{the time after which the button starts to auto-repeat (milliseconds).}
  \item{quitbutton}{this defaults to FALSE. Set to TRUE this creates a button which will close the window and escape from a rp.block call. Before the window is destroyed the action function will be called.}
  \item{pos}{the layout instructions. Please see the \code{\link{rp.pos}} example and help for full details.}
  \item{foreground}{this sets the colour of text e.g. "navy"}
  \item{background}{this sets the background colour of text e.g. "white"}
  \item{font}{this sets the text font e.g. "Arial"}
  \item{parentname}{this specifies the widget inside which the button should appear.}
  \item{name}{the name of the button.}
  \item{...}{...}
}

\note{
The arguments \code{id} and \code{parent} have been discontinued in version 1.1.
}

\details{
  The function \code{action} should take one argument, which should be the panel.
  See \code{\link{rp.grid}} for details of the grid layout system.
}

\section{Warning}{
  The \code{action} function should return the panel. 
  Without this assignment any widgets added or alterations made to panel parameters within 
  the \code{action} function will be lost.
}

\references{
   rpanel: Simple interactive controls for R functions using the tcltk package.
      Journal of Statistical Software, 17, issue 9.
   }

\seealso{\code{\link{rp.doublebutton}},\code{\link{rp.control}}}

\examples{
\dontrun{
   # This function will be called on pressing the button "Simulate".
   boxp.sim <- function(panel) {
     boxplot(rnorm(50))
     panel
     }
   # Create an rpanel and add the button "Simulate" to it.
   panel <- rp.control()
   rp.button(panel, action = boxp.sim, title = "Simulate")
}}

\keyword{iplot}
\keyword{dynamic}
