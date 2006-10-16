\name{rp.menu}

\alias{rp.menu}

\title{Top level menu for a panel}

\description{
  This function adds a menu to the top of the panel window. When a menu item is selected, a variable is set and an action function is called.
}

\usage{
rp.menu(panel, var, labels, initval = NULL, parent = window, action = I, ...)
}

\arguments{
  \item{panel}{the panel to which the menu should be attached should appear. This 
  may be passed as a panelname string or the panel object itself.}
  \item{var}{the name of the variable whose value is set by the menu.}
  \item{labels}{the labels for the menu options. These values are returned through var. The menu is defined by a 
  list of lists of character strings. Each major menu heading should be the first item in the sub-lists with the 
  submenu items listed afterwards in the same list. Please see the example.}
  \item{initval}{the initial value of <var> (optional). The initial value can also be specified in the call to \code{rp.control}.}
  \item{parent}{this specifies the widget inside which this widget should appear. In the current version, it should not normally be used.}
  \item{action}{the function which is called when a menu item is chosen.}
  \item{\dots}{any further parameters.}
}

\details{
  The function \code{action} should take one argument, which should be the panel to which the listbox is attached.\cr
\cr
  The list for a menu consisting of "File" and "Edit" only would be defined thus;\cr
  list(list("File"), list("Edit"))\cr
\cr
  The list for a menu consisting of "File" with subitem "Quit" and "Edit" with subitems "Copy", "Cut" and "Paste" 
  would be defined thus;\cr
  list(list("File", "Quit"), list("Edit", "Copy", "Cut", "Paste"))
}

\value{
  If the parameter panel is the panelname string the same string is returned. If the 
  panel object is used the altered panel is assigned to both the calling level and panel's 
  environment level.
}

\section{Warning}{
  The \code{action} function should return the panel. 
  Without this assignment any widgets added or alterations made to panel parameters within 
  the \code{action} function will be lost.\cr
\cr
  The action function must be defined before the rp.menu statement as it relies on the function already existing.
}

\references{rpanel: Simple interactive controls for R functions using 
the tcltk library 
(http://www.stats.gla.ac.uk/~adrian/rpanel/)}

\seealso{\code{\link{rp.checkbox}},\code{\link{rp.control}}}

\examples{
a <- rp.control()
# The action function has to come first so that it already exists for rp.menu - as it creates the callback
# functions on the fly it requires action to already be defined.
domenu <- function(panel)
{
 rp.messagebox(panel$menuchoice, title = "You chose")
 panel
}
rp.menu(a, menuchoice, labels=list(list("File","Quit"),list("Edit","Copy","Cut","Paste")), action=domenu)
}

\keyword{iplot}
\keyword{dynamic}
