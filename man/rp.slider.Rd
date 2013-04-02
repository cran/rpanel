\name{rp.slider}

\alias{rp.slider}
\alias{rp.slider.change}

\title{Slider for an rpanel}

\description{
  Add a slider (or slider group) to the panel, to graphically control a numeric variable.}

\usage{
rp.slider(panel, variable, from, to, action=I, labels=NULL, names=NULL, title=NULL,
  log=rep(FALSE, length(from)), showvalue=FALSE, showvaluewidth=4, resolution=0,
  initval=from, pos=NULL, 
  horizontal=TRUE, foreground=NULL, background=NULL, font=NULL, 
  parentname=deparse(substitute(panel)), name=paste("slider", .nc(), sep=""), ...) 
rp.slider.change(panel, name, value, i=1, do=TRUE)
}

\arguments{
  \item{panel}{the panel in which the slider appears.}
  \item{variable}{the name of the variable that the slider controls.}
  \item{from}{the lower limit of the range of values to which the slider can be 
    set.}
  \item{to}{the upper limit of the range of values to which the slider can be 
    set.}
  \item{action}{the function which is called when the slider is moved.}
  \item{labels}{displayed labels}
  \item{names}{the names of the elements of \code{variable}, for reference by action functions.}
  \item{title}{the label of the slider.}
  \item{log}{a logical variable which controls whether the scale of the slider 
    is logarithmic.}
  \item{showvalue}{a logical variable which determines whether the present value 
    of "var" is shown. This is forced to FALSE when log is TRUE.}
  \item{showvaluewidth}{the number of significant digits in the shown value}
  \item{resolution}{the resolution of the slider scale. If > 0, all values are 
    rounded to an even multiple of this value. The default is 0.}
  \item{initval}{the initial value of var (optional). The initial value can also 
    be specified in the call to \code{rp.control}.}
  \item{pos}{the layout instructions. Please see the \code{\link{rp.pos}} example 
    and help for full details.}
  \item{horizontal}{a logical variable determining whether the slider is 
    displayed horizontally (or vertically).}
  \item{foreground}{colour of the text}
  \item{background}{colour of the text background}
  \item{font}{font to be used}
  \item{parentname}{this specifies the widget inside which the slider should 
    appear.}
  \item{name}{name assigned to the slider, used for disposing of the widget} 
  \item{...}{...}
  \item{value}{new value for the slider}
  \item{i}{which slider to alter}
  \item{do}{whether to call the action event}
}

\note{
New for version 2.0 is support for multiple sliders in a group. See demo(rp.slider).
}

\details{
  The function \code{action} should take one argument, which should be the panel to which the slider is attached. 
  
    See \code{\link{rp.grid}} for details of the grid layout system.
}

\section{Warning}{
  The \code{action} function should return the panel. 
  Without this assignment any widgets added or alterations made to panel parameters within 
  the \code{action} function will be lost.\cr
\cr
Note that setting log=TRUE and showvalue=TRUE is not allowed. The slider value shown would be incorrect 
  (it wouldn't be the log value) and so showvalue is over-ridden and set to FALSE. A new widget rp.label is 
  under development which would be used in these circumstances.
}

\references{
   rpanel: Simple interactive controls for R functions using the tcltk package.
      Journal of Statistical Software, 17, issue 9.
   }

\seealso{\code{\link{rp.radiogroup}},\code{\link{rp.control}}}

\examples{
\dontrun{
density.draw <- function(panel) {
   plot(density(panel$x, bw = panel$h))
   panel
   }
panel <- rp.control(x = rnorm(50))
rp.slider(panel, h, 0.5, 5, log = TRUE, action = density.draw)
   
printer <- function(panel) {
  print(panel$h)
  panel
}
panel <- rp.control(x = rnorm(50), h=c(1,2,3))
rp.slider(panel, h, c(0.5,0.5,0.5), c(5,5,5),
  log = c(TRUE,TRUE,TRUE), action = printer,
  title=c('h','h1','h2'), initval=c(1,2,3))
    
# An example which changes the slider position through another widget
    
draw <- function(panel) {
  hist(panel$x)
  abline(v=panel$v, col="red", lty=2)
  panel
}

redraw <- function(panel) {
  rp.tkrreplot(panel, plot)
  panel
}

redraw1 <- function(panel) {
  rp.tkrreplot(panel, plot)
  rp.slider.change(panel, "slider", panel$v)
  panel
}

x <- rnorm(25)
panel <- rp.control(v = 0, x = x)
rp.tkrplot(panel, plot, draw, pos="right")
rp.slider(panel, v, min(x), max(x), redraw, name = "slider")
rp.doublebutton(panel, v, diff(range(x))/100, action=redraw1)

}}  

\keyword{iplot}
\keyword{dynamic}
