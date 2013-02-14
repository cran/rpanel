\name{rp.doublebutton}

\alias{rp.doublebutton}

\title{Double-button widget for rpanel}

\description{
  Adds a control with '+' and '-' buttons, to increment and decrement a variable.
}

\usage{
rp.doublebutton(panel, variable, step, title=deparse(substitute(variable)), action=I, initval=NULL, range=c(NA, NA), 
  log=FALSE, showvalue=FALSE, showvaluewidth=4, repeatinterval=100, repeatdelay=100, pos=NULL, foreground=NULL, 
  background=NULL, font=NULL, parentname=deparse(substitute(panel)), name=paste("doublebutton", .nc(), sep=""), ...) 
}

\arguments{
  \item{panel}{the panel in which the doublebutton should appear.}
  \item{variable}{the name of the variable within the panel that the doublebutton should control.}
  \item{step}{the value by which the variable "variable" is incremented or decremented on pressing a button. When log is TRUE this is a factor instead.}
  \item{title}{the label for the doublebutton. This defaults to the name of var.}
  \item{action}{the function which is called when a button is pressed.}
  \item{initval}{the initial value for var (optional). The initial value can also be specified in the call to \code{rp.control}.}
  \item{range}{a 2-element numeric vector containing lower and upper limits for var. Use NA for no limit (upper and/or lower).}
  \item{log}{a logical variable which determines whether the increment (step) is multiplicative or additive.}
  \item{showvalue}{a logical variable which determines whether the present value of "variable" is shown between the + and - buttons. This is forced to FALSE when log is TRUE.}
  \item{showvaluewidth}{defines the width of the shown value in characters.}
  \item{repeatinterval}{the interval between auto-repeats (milliseconds) when the button is held down.}
  \item{repeatdelay}{the time after which the button starts to auto-repeat (milliseconds).}
  \item{pos}{the layout instructions. Please see the \code{\link{rp.pos}} example and help for full details.}
  \item{foreground}{colour of the text}
  \item{background}{colour of the text background}
  \item{font}{font to be used}
  \item{parentname}{this specifies the widget inside which the doublebutton widget. }
  \item{name}{name assigned to the doublebutton; used for disposal etc}
  \item{...}{...}
}

\note{
The former arguments \code{parent} and \code{...} have been discontinued in version 1.1.  Note also that the argument \code{var} has been renamed \code{variable} to avoid reserved word issues.
}

\details{
  \code{action} should be a function of one argument, which should be the panel.  The panel can then be manipulated, and data stored in the panel may be used/modified, then the (optionally modified) panel must be returned.
  
See \code{\link{rp.grid}} for details of the grid layout system.
}

\value{
  If the argument \code{panel} is the panelname string, the same string is returned.  If the panel object is used, the altered panel is assigned to both the calling level and panel's environment level.
}

\section{Warning}{
  The \code{action} function should return the panel.  Without this assignment any widgets added or alterations made to panel parameters within the \code{action} function will be lost.

Note that setting \code{log=TRUE} and \code{showvalue=TRUE} is not allowed.
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
   rp.doublebutton(panel, var = h, step = 0.05, 
     title = "Density estimate", action = density.draw,
     range = c(0.1, 5), initval=1)
}}

\keyword{iplot}
\keyword{dynamic}
