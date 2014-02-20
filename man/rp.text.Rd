\name{rp.text}

\alias{rp.text}
\alias{rp.text.change}

\title{Text boxes for a panel}

\description{
  This function adds one or more boxes which allow text to be entered.
}

\usage{
rp.text(panel, text, pos=NULL, action=I, foreground=NULL, background=NULL,
        font=NULL, width=NULL, parentname=deparse(substitute(panel)),
        name = paste("text", .nc(), sep=""), ...)   
rp.text.change(panel, name, text)
}

\arguments{
  \item{panel}{the panel on which the text should appear.}
  \item{text}{the text to be displayed}
  \item{pos}{the layout instructions. Please see the \code{\link{rp.pos}} example and 
               help for full details.}
  \item{action}{the function which is called when the text has been entered.}
  \item{foreground}{colour of the text}
  \item{background}{colour of the text background}
  \item{font}{font to be used}
  \item{width}{character width of the textboxes}  
  \item{parentname}{this specifies the widget inside which the text entry widget 
    should appear.}
  \item{name}{name assigned to the textentries; used for disposal etc}
  \item{...}{...}
}

\details{
The function \code{action} should take one argument, which should be the panel to which the text box is attached. 
  
See \code{\link{rp.grid}} for details of the grid layout system.
}

\section{Warning}{
The \code{action} function should return the panel.  Without this assignment any widgets added or alterations made to panel parameters within the \code{action} function will be lost.
}

\references{
   rpanel: Simple interactive controls for R functions using the tcltk package.
      Journal of Statistical Software, 17, issue 9.
   }

\seealso{\code{\link{rp.control}}}

\examples{
\dontrun{
panel <- rp.control(x=1)
callback <- function(panel)
{
  rp.text.change(panel, "t2", panel$x)
  panel$x = panel$x+1
  panel
}
rp.text(panel, "This is a test", name="t1")
rp.text(panel ,"And so is this", font="Arial", foreground="white",
        background="navy", action=callback, name="t2")
rp.text(panel,"Here is some more text, this time across several lines.\n
               Here is some more text, this time across several lines.\n
               Here is some more text, this time across several lines.", name="t3")
}}

\keyword{iplot}
\keyword{dynamic}
