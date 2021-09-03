\name{rp.radiogroup}

\alias{rp.radiogroup}

\title{Radiobuttons for a panel}

\description{
This function adds a set of radiobuttons to the panel. When a radiobutton is pressed, a variable is set and an action function is called.
}

\usage{
rp.radiogroup(panel, variable, vals, labels=NULL, initval=vals[1], pos=NULL, 
  title=deparse(substitute(variable)), 
  action=I, foreground=NULL, background=NULL, font=NULL, 
  parentname=deparse(substitute(panel)), name=paste("radiogroup", .nc(), sep=""), ...)
}

\arguments{
  \item{panel}{the panel in which the radiobuttons should appear.}
  \item{variable}{the name of the variable whose values are set by the buttons.}
  \item{vals}{the values attached to the labels for return from the action function. NOTE: for implementation.}
  \item{labels}{the labels for the radiobuttons.}
  \item{initval}{the initial value for the variable (optional). The initial value can also be specified in the call to \code{rp.control}.}
  \item{pos}{the layout instructions. Please see the \code{\link{rp.pos}} example and help for full details.}
  \item{title}{the label for the group of radiobuttons.}
  \item{action}{the function which is called when a button is pressed.}
  \item{foreground}{colour of the text}
  \item{background}{colour of the text background}
  \item{font}{font to be used}
  \item{parentname}{this specifies the widget inside which the radiogroup should 
    appear.}
  \item{name}{name assigned to the listbox, used for disposing of the widget}  
  \item{...}{...}
}

\details{
The function \code{action} should take one argument, which should be the panel to which the radiobuttons are attached. 
  
See \code{\link{rp.grid}} for details of the grid layout system.
}

\value{
If the argument \code{panel} is the panelname string, the same string is returned.  If the panel object is used, the altered panel is assigned to both the calling level and panel's environment 
level.
}

\section{Warning}{
The \code{action} function should return the panel.  Without this assignment any widgets added or alterations made to panel parameters within the \code{action} function will be lost.
}

\references{
   rpanel: Simple interactive controls for R functions using the tcltk package.
      Journal of Statistical Software, 17, issue 9.
   }

\seealso{\code{\link{rp.checkbox}}, \code{\link{rp.control}}}

\examples{
\dontrun{
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
}}

\keyword{iplot}
\keyword{dynamic}
