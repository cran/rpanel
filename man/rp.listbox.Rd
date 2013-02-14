\name{rp.listbox}

\alias{rp.listbox}

\title{Listbox for a panel}

\description{
This function adds a listbox to the panel. When an item is pressed, a variable is set and an action function is called.
}

\usage{
rp.listbox(panel, variable, vals, labels = vals, rows=length(labels), initval=vals[1], pos=NULL, 
  title=deparse(substitute(variable)), action=I, foreground=NULL, background=NULL, font=NULL,
  parentname=deparse(substitute(panel)), sleep = 0.01, name=paste("listbox", .nc(), sep=""), ...)
}

\arguments{
  \item{panel}{the panel in which the listbox should appear.}
  \item{variable}{the name of the variable whose value is set by the listbox.}
  \item{vals}{the values of \code{var} used by the listbox. NOTE: Not currently in use, intended to be. }
  \item{labels}{the labels for values of var offered by the listbox.}
  \item{rows}{the number of rows in the list. This defaults to the number of labels. If the number of labels is greater than the number of rows the listbox will be displayed with a scrollbar.}
  \item{initval}{the initial value of <var> (optional). The initial value can also be specified in the call to \code{rp.control}.}
  \item{pos}{the layout instructions. Please see the \code{\link{rp.pos}} example and help for full details.}
  \item{title}{the label for the listbox.}
  \item{action}{the function which is called when an item is chosen.}
  \item{foreground}{colour of the text}
  \item{background}{colour of the text background}
  \item{font}{font to be used}
  \item{parentname}{this specifies the widget inside which the listbox should 
    appear.}
  \item{sleep}{a length of time in seconds, passed to \code{Sys.sleep}, which can be used to overcome a technical problem in some computer systems.  If the listbox appears blank, then setting this parameter to a slightly value may fix the problem.}  
  \item{name}{name assigned to the listbox, used for disposing of the widget}  
  \item{...}{...}  
}

\note{
The former arguments \code{parent} and \code{...} have been discontinued in version 1.1.  Note also that the argument \code{var} has been renamed \code{variable} to avoid reserved word issues.
}

\details{
The function \code{action} should take one argument, which should be the panel to which the listbox is attached. 
  
See \code{\link{rp.grid}} for details of the grid layout system.
}

\value{
If the argument \code{panel} is the panelname string, the same string is returned.  If the panel object is used, the altered panel is assigned to both the calling level and panel's environment level.
}

\section{Warning}{
The \code{action} function should return the panel.  Without this assignment any widgets added or alterations made to panel parameters within the \code{action} function will be lost.
}

\references{
rpanel: Simple interactive controls for R functions using the tcltk package.  Journal of Statistical Software, 17, issue 9.
}

\seealso{\code{\link{rp.checkbox}},\code{\link{rp.control}}}

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
   rp.listbox(panel, plot.type,
       c("histogram", "boxplot", "density estimate"),
       action = data.plotfn, title = "Plot type") 
}}

\keyword{iplot}
\keyword{dynamic}
