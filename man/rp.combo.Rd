\name{rp.combo}

\alias{rp.combo}

\title{A `combo' for a panel}

\description{
  This function adds a `combobox' to the panel. When an item is pressed, a variable is set and an action function is called.
}

\usage{
rp.combo(panel, variable, prompt=NULL, vals, initval=vals[1], pos=NULL, action=I, foreground=NULL, 
  background=NULL, font=NULL, editable=FALSE, parentname=deparse(substitute(panel)), name=paste("combo", .nc(), sep=""), ...) 
}

\arguments{
  \item{panel}{the panel in which the combobox should appear.}
  \item{variable}{the name of the variable whose value is set by the combobox.}
  \item{prompt}{the label for the combobox.}
  \item{vals}{the values of \code{variable} used by the combo. }
  \item{initval}{the initial value of \code{variable} (optional). The initial value can also be specified in the call to \code{rp.control}.}
  \item{pos}{the layout instructions. Please see the \code{\link{rp.pos}} example and help for full details.}
  \item{action}{the function which is called when an item is chosen.}
  \item{foreground}{colour of the text}
  \item{background}{colour of the text background}
  \item{font}{font to be used}
  \item{editable}{whether the combobox can be edited or not.}
  \item{parentname}{this specifies the widget inside which the combobox should 
    appear.}
  \item{name}{name assigned to the combobox, used for disposing of the widget}  
  \item{...}{...}
}

\note{
Parameters \code{parent} and \code{...} have been discontinued in version 1.1.  Note that the argument previously named \code{var} has been renamed \code{variable} to avoid reserved 
word issues.
}

\details{
  The function \code{action} should take one argument, which should be the panel to which the combobox is attached. 
  
    See \code{\link{rp.grid}} for details of the grid layout system.
}

\value{
  If the parameter panel is the panelname string the same string is returned.  If the 
  panel object is used the altered panel is assigned to both the calling level and panel's 
  environment level.
}

\section{Warning}{
  The \code{action} function should return the panel.  Without this assignment any widgets added or alterations made to panel parameters within the \code{action} function will be lost.
}

\references{
   rpanel: Simple interactive controls for R functions using the tcltk package.
      Journal of Statistical Software, 17, issue 9.
   }

\seealso{\code{\link{rp.checkbox}},\code{\link{rp.control}}}

\examples{
\dontrun{
   callback <- function(panel) {
      print(panel$option)
   }
   panel <- rp.control()
   rp.combo(panel, option, "Pick an option:", 
            c("Option1","Option2","Other options"), action=callback)
}}

\keyword{iplot}
\keyword{dynamic}
