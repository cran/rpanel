\name{rp.checkbox}

\alias{rp.checkbox}

\title{Check Box widget for rpanel}

\description{
  Adds one or more checkboxes to the panel, to control logical variables.
}

\usage{
rp.checkbox(panel, var, action = I, labels = NA, names = labels, title = NA,
             initval = NA,  parent = window, pos = NULL, doaction = FALSE, ...)
}

\arguments{
  \item{panel}{the panel in which the checkbox(es) should appear. This 
               may be passed as a panelname string or the panel object itself.}
  \item{var}{the name of the variable within the panel that the checkbox(es) should control.}
  \item{action}{the function to call whenever a checkbox is clicked.}
  \item{labels}{the labels of the checkboxes. This defaults to the name of the variable \code{var}
               plus an identifying integer.}
  \item{names}{the names attached to the elements of \code{var}.  These provide a helpful
               means of referring to particular items in multiple checkboxes.}
  \item{title}{the title of the checkbox group. This defaults to the name of the variable \code{var}.
               If there is only one box, and \code{labels} is not specified, \code{title} is taken 
               as the label.}
  \item{initval}{the initial value for \code{var} (optional). The initial value can also be 
               specified in the call to \code{rp.control}.}
  \item{parent}{this specifies the widget inside which the checkbox should appear. 
               In the current version, it should not normally be used.}
  \item{pos}{the layout instructions. Please see the \code{\link{rp.pos}} example and 
               help for full details.}
  \item{doaction}{a logical variable which determines whether the action function is called
               when the widget is created.  The default is FALSE, so that the \code{rp.do}
               function should be called after all widgets have been created, to initialise
               the state of the panel display.}
  \item{\dots}{information for \code{pos} can be passed individually as additional arguments.}
}

\details{
  The function \code{action} should take one argument, which should be the panel to which the checkbox is attached.
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

\seealso{\code{\link{rp.radiogroup}},\code{\link{rp.control}}}

\examples{
if (interactive()) {
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
