\name{rp.checkbox}

\alias{rp.checkbox}

\title{A checkbox control for rpanel}

\description{
  Adds one or more checkboxes to the panel, to control logical variables.
}

\usage{
rp.checkbox(panel, variable, action=I, labels=NULL, names=NULL, title=NULL, 
  initval=rep(FALSE, length(labels)), pos=NULL, doaction=FALSE, foreground=NULL,
  background=NULL, font=NULL, parentname=deparse(substitute(panel)),
  name=paste("checkbox", .nc(), sep=""), ...) 
}

\arguments{
  \item{panel}{the panel in which the checkbox(es) should appear.}
  \item{variable}{the name of the variable within the panel that the checkbox(es) should control.}
  \item{action}{the function to call whenever a checkbox is clicked.}
  \item{labels}{the labels of the checkboxes. The length of \code{labels} determines the number
               of checkboxes created.  This default value for \code{labels} is the name of 
               \code{variable}, and therefore a single checkbox.}
  \item{names}{the names attached to the elements of \code{variable}.  These provide a helpful
               means of referring to particular items in multiple checkboxes when defining
               the \code{action} function.  If names were not specified in the call to
               \code{rp.control} then \code{names} is set to \code{labels}.}
  \item{title}{the title of the checkbox group. This defaults to the name of the variable
               \code{variable}.}
  \item{initval}{the initial value for \code{variable} (optional). The initial value can also be 
               specified in the call to \code{rp.control}.}
  \item{pos}{the layout instructions. Please see the \code{\link{rp.pos}} example and 
               help for full details.}
  \item{doaction}{a logical variable which determines whether the action function is called
               when the widget is created.  The default is FALSE, so that the \code{rp.do}
               function should be called after all widgets have been created, to initialise
               the state of the panel display.}
  \item{foreground}{this sets the colour of text e.g. "navy"}
  \item{background}{this sets the background colour of text e.g. "white"}
  \item{font}{this sets the text font e.g. "Arial"}
  \item{parentname}{this specifies the widget inside which the checkbox(es) should appear.}
  \item{name}{the name of the checkbox.}
  \item{...}{...}
}

\details{
  The function \code{action} should take one argument, which should be the panel to which the checkbox is attached.
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

\seealso{\code{\link{rp.radiogroup}},\code{\link{rp.control}}}

\examples{
\dontrun{
   plot.hist <- function(panel) {
	 with(panel, {
		xlim <- range(c(x, mean(x) + c(-3, 3) * sd(x)))
	   if (panel$cbox[3])
	      clr <- "lightblue" else clr <- NULL
	   hist(x, freq = FALSE, col = clr, xlim = xlim)
	   if (panel$cbox[1]) {
	      xgrid <- seq(xlim[1], xlim[2], length = 50)
	      dgrid <- dnorm(xgrid, mean(x), sd(x))
	      lines(xgrid, dgrid, col = "red", lwd = 3)
	      }
	   if (panel$cbox[2])
	      box()
	   })
	 panel
	 }
   x <- rnorm(50)
   panel <- rp.control(x = x)
   rp.checkbox(panel, cbox, plot.hist, 
      labels = c("normal density", "box", "shading"), title = "Options")
   rp.do(panel, plot.hist)
}}

\keyword{iplot}
\keyword{dynamic}
