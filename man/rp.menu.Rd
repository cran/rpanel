\name{rp.menu}

\alias{rp.menu}

\title{Top level menu for a panel}

\description{
This function adds a menu to the top of the panel window.  When a menu item is selected, a variable is set and an action function is called.
}

\usage{
rp.menu(panel, variable, labels, initval=NULL, action=I,
        foreground=NULL, background=NULL, font=NULL,
        name=paste("menu", .nc(), sep="")) 
}

\arguments{
  \item{panel}{the panel to which the menu should be attached should appear.}
  \item{variable}{the name of the variable whose value is set by the menu. (Renamed in 2.0 to \code{variable} from \code{var} as \code{var} is a reserved word.) }
  \item{labels}{the labels for the menu options. These values are returned through var. The menu is defined by a 
  list of lists of character strings. Each major menu heading should be the first item in the sub-lists with the 
  submenu items listed afterwards in the same list. Please see the example.}
  \item{initval}{the initial value of \code{variable} (optional). The initial value can also be specified in the call to \code{\link{rp.control}}.}
  \item{action}{the function which is called when a menu item is chosen.}
  \item{foreground}{this sets the colour of text e.g. "navy"}
  \item{background}{this sets the background colour of text e.g. "white"}
  \item{font}{this sets the text font e.g. "Arial"}
  \item{name}{the name of the widget - this is used by \code{\link{rp.widget.dispose}} }
}

\note{
The former argument \code{parent} has been discontinued in version 1.1.
}

\details{
The function \code{action} should take one argument, which should be the panel to which the listbox is attached.

The list for a menu consisting of "File" and "Edit" only would be defined as
\code{list(list("File"), list("Edit"))}.

The list for a menu consisting of "File" with subitem "Quit", and "Edit" with subitems "Copy", "Cut" and "Paste", would be defined as
\code{list(list("File", "Quit"), list("Edit", "Copy", "Cut", "Paste"))}.
}

\section{Warning}{
The \code{action} function should return the panel.  Without this assignment any widgets added or alterations made to panel parameters within the \code{action} function will be lost.

The action function must be defined before the \code{rp.menu} statement as it relies on the function already existing.
}

\references{
rpanel: Simple interactive controls for R functions using the tcltk package.  Journal of Statistical Software, 17, issue 9.
}

\seealso{\code{\link{rp.checkbox}},\code{\link{rp.control}}}

\examples{
\dontrun{
   a <- rp.control()
   # The action function has to come first so that it already exists for rp.menu, 
   # as it creates the callback functions on the fly it requires action to already 
   # be defined.
   domenu <- function(panel) {
      rp.messagebox(panel$menuchoice, title = "You chose")
      panel
      }
   rp.menu(a, menuchoice, labels=list(list("File","Quit"),
              list("Edit","Copy","Cut","Paste")), action=domenu)
}}

\keyword{iplot}
\keyword{dynamic}
