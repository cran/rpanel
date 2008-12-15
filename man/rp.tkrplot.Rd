\name{rp.tkrplot}

\alias{rp.tkrplot}

\alias{rp.tkrreplot}

\title{rpanel calls for tkrplot and tkrreplot}

\description{
  These functions call Luke Tierney's \code{tkrplot} and \code{tkrreplot} 
functions to allow R graphics to be displayed in a panel.
}

\usage{
rp.tkrplot(panel, name, plotfun, action = NA,  mousedrag = NA,
           mouseup = NA, hscale = 1, vscale = 1, parent = window, pos = NULL,
           foreground = NULL, margins = c(0, 0, 0, 0), ...)
rp.tkrreplot(panel, name)
}

\arguments{
  \item{panel}{the panel in which the plot should appear. This 
  may be passed as a panelname string or the panel object itself.}
  \item{name}{the name of the plot. This is subsequently used in tkrreplot to specify the plot to be redrawn.}
  \item{plotfun}{the function used to create the plot.}
  \item{action}{the function called when the plot is clicked.}
  \item{mousedrag}{the function called when the mouse is dragged.}
  \item{mouseup}{the function called when the mouse is released.}
  \item{parent}{this specifies the widget inside which the plot should appear. In the current version of rpanel, it should not normally be used.}
  \item{hscale}{horizontal scaling factor to control the width of the plot.}
  \item{vscale}{vertical scaling factor to control the height of the plot.}
  \item{pos}{the layout instructions. Please see the \code{\link{rp.pos}} example and help for full details.}
  \item{foreground}{the filename of a transparent gif file.  This will be overlaid on the tkrplot image.}
  \item{margins}{an integer vector of length 4 giving the margin sizes, in pixels and in the 
                 usual order, for the placing of the foreground image.}
  \item{\dots}{information for \code{pos} can be passed individually as additional arguments.}
}

\details{
  The function \code{action} should take one argument, which should be the panel to which the tkrplot is attached. 
  
    See \code{\link{rp.grid}} for details of the grid layout system.
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

\references{
   rpanel: Simple interactive controls for R functions using the tcltk package.
      Journal of Statistical Software, 17, issue 9.
   }

\seealso{\code{\link{rp.image}}}

\examples{
if (interactive()) {
   draw <- function(panel) {
      plot(1:20, (1:20)^panel$h)
      panel
      }
   
   redraw <- function(panel) {
      rp.tkrreplot(panel, tkrp)
      panel
      }

   rpplot <- rp.control(title = "Demonstration of rp.tkrplot", h = 1)
   rp.tkrplot(rpplot, tkrp, draw)
   rp.slider(rpplot, h, action = redraw, from = 0.05, to = 2.00, resolution = 0.05)
   }}

\keyword{iplot}
\keyword{dynamic}
