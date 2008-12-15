\name{rp.textentry}

\alias{rp.textentry}

\title{Text entry boxes for a panel}

\description{
  This function adds one or more boxes which allow text to be entered.
}

\usage{
rp.textentry(panel, var, action = I, labels = NA, names = labels, title = NA, 
             initval = NA, parent = window, pos = NULL, ...)
}

\arguments{
  \item{panel}{the panel in which the text entry box(es) should appear. This 
               may be passed as a panelname string or the panel object itself.}
  \item{var}{the name of the variable which will be assigned the text entered into the box(es).}
  \item{action}{the function which is called when the text has been entered.}
  \item{labels}{a character string of labels for the text entry boxes.}
  \item{names}{the names attached to the elements of \code{var}.  These provide a helpful
               means of referring to particular items in multiple textentry boxes.}
  \item{title}{a title for the group of text entry boxes.  If there is only one box, and
               \code{labels} is not specified, \code{title} is taken as the label.}
  \item{initval}{the initial value(s) for var (optional). The initial value(s) can also be 
               specified in the call to \code{rp.control}.}
  \item{parent}{this specifies the widget inside which the text entry widget should appear. In the current  
               version, it should not normally be used.}
  \item{pos}{the layout instructions. Please see the \code{\link{rp.pos}} example and help for full 
               details.}
  \item{\dots}{information for \code{pos} can be passed individually as additional arguments.}
}

\details{
  The function \code{action} should take one argument, which should be the panel to which the 
  text entry box is attached. 
  
    See \code{\link{rp.grid}} for details of the grid layout system.
}

\value{
  If the parameter panel is the panelname string the same string is returned. If the 
  panel object is used the altered panel is assigned to both the calling level and 
  panel's environment level.
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

\seealso{\code{\link{rp.control}}}

\examples{
if (interactive()) {
   plotf <- function(panel) {
      with(panel, {
		 pars   <- as.numeric(pars)
         xgrid <- seq(0.1, max(c(pars[3], 5), na.rm = TRUE), length = 50)
         dgrid <- df(xgrid, pars[1], pars[2])
         plot(xgrid, dgrid, type = "l", col = "blue", lwd = 3)
         if (!is.na(pars[3])) {
            lines(rep(pars[3], 2), c(0, 0.95 * max(dgrid)), lty = 2, col = "red")
            text(pars[3], max(dgrid), as.character(pars[3]), col = "red")
            }
         })
      panel
      }

   panel <- rp.control(pars = c(5, 10, NA))
   rp.textentry(panel, pars, plotf, labels = c("df1", "df2", "observed"),
          initval = c(10, 5, 3))
   rp.do(panel, plotf)
   }}

\keyword{iplot}
\keyword{dynamic}
