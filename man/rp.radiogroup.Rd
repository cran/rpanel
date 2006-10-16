\name{rp.radiogroup}

\alias{rp.radiogroup}

\title{Radiobuttons for a panel}

\description{
  This function adds a set of radiobuttons to the panel. When a radiobutton is pressed, a variable is set and an action function is called.
}

\usage{
rp.radiogroup(panel, var, values, labels=values, initval = values[1], parent=window, 
pos=NULL, title=deparse(substitute(var)), action=I, ...)
}

\arguments{
  \item{panel}{the panel in which the radiobuttons should appear. This 
  may be passed as a panelname string or the panel object itself.}
  \item{var}{the name of the variable whose values are set by the buttons.}
  \item{values}{the values that \code{var} is allocated by the buttons.}
  \item{labels}{the labels for the radiobuttons.}
  \item{initval}{the initial value for the variable (optional). The initial value can also be specified in the call to \code{rp.control}.}
  \item{parent}{this specifies the widget inside which the radiobuttons should appear. In the current version of rpanel, it should not normally be used.}
  \item{pos}{the layout instructions. Please see the \code{\link{rp.pos}} example and help for full details.}
  \item{title}{the label for the group of radiobuttons.}
  \item{action}{the function which is called when a button is pressed.}
  \item{\dots}{any further parameters.}
}

\details{
  The function \code{action} should take one argument, which should be the panel to which the radiobuttons are attached. 
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

\references{rpanel: Simple interactive controls for R functions using 
the tcltk library 
(http://www.stats.gla.ac.uk/~adrian/rpanel/)}

\seealso{\code{\link{rp.checkbox}},\code{\link{rp.control}}}

\examples{
data.plotfn <- function(panel) {
  if (panel$plot.type == "histogram") 
    hist(panel$x)
  else 
    if (panel$plot.type == "boxplot")
      boxplot(panel$x)
    else 
      plot(density(panel$x))
  panel
}
panel <- rp.control(x = rnorm(50))
rp.radiogroup(panel, plot.type, 
       c("histogram", "boxplot", "density estimate"), 
       action = data.plotfn, title = "Plot type")     
}

\keyword{iplot}
\keyword{dynamic}
