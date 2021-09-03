\name{rp.notebook}

\alias{rp.notebook}
\alias{rp.notebook.raise}

\title{Define a notebook within an rpanel}

\description{A tabbed notebook, the location of which is defined by pos, is created within an rpanel. Further widgets, grids or 
even notebooks can then be placed within the notebook.
}

\usage{
rp.notebook(panel, tabs, tabnames=tabs, width = 600, height = 400, pos = NULL, 
  foreground = NULL, background = "lightgray", font = NULL, 
  parentname = deparse(substitute(panel)),
  name = paste("notebook", .nc(), sep = ""), ...)
rp.notebook.raise(panel, parentname, label)
}

\arguments{
  \item{panel}{the panel in which the notebook should appear.}
  \item{tabs}{this is a vector of the names to appear on the tabs}
  \item{tabnames}{this is a vector of the labels to be used internally - used by \code{rp.notebook.raise}}
  \item{width}{the width, in pixels, of the notebook}
  \item{height}{the height, in pixels, of the notebook}
  \item{pos}{the position of the notebook. see \code{\link{rp.pos}} }
  \item{foreground}{this sets the colour of text e.g. "navy"}
  \item{background}{this sets the background colour of text e.g. "white"}
  \item{font}{this sets the text font e.g. "Arial"}
  \item{parentname}{this specifies the widget inside which the notebook should appear.}
  \item{name}{the name of the widget - this is used by \code{\link{rp.widget.dispose}} }
  \item{label}{the name of the tab which is to be raised}
  \item{...}{...}
}

\details{The role of this function is to specify a notebook.  Nesting of notebooks is permitted. \code{rp.notebook.raise} is used to bring the contents of a particular tab to the foreground.

These functions make use of the BWidget extension to the Tcl/Tk system.  If Bwidget has not been installed on your system, download it from \url{https://sourceforge.net/projects/tcllib/files/BWidget/} and expand the compressed file into a folder.  On a Windows machine, this folder should then be copied into the folder containing the Tcl libraries that were installed as part of \R.  This may be in a location such as C:\\Program Files\\R\\R-4.0.2\\Tcl\\lib (with an obvious change to the version number of R being used).  On a Mac, the downloaded folder should be copied into the folder where the main Tcl package is located (note: not inside the Tcl folder but at the same level as the Tcl folder).  This may be in a location such as /usr/local/lib.
}

\references{
rpanel: Simple interactive controls for \R functions using the tcltk package. Journal of Statistical Software, 17, issue 9.
}

\examples{
\dontrun{
panel <- rp.control(title="Notebook example with two notebooks")
rp.notebook(panel, c("File", "Edit"), width=600, height=400,
            pos=list(row=0, column=0), background="lightgray",
            font="Arial", name="n1")
rp.notebook.raise(panel, "n1", "Edit")
rp.button(panel, function(panel){
	                rp.messagebox("Button pressed!"); panel },
	                "Test this", parentname="Edit")
rp.messagebox("A second tabbed notebook can be added to the same window.")
rp.notebook(panel, c("A tab 1", "A tab 2"), width=200, height=200,
            pos=list(row=1, column=1), background="Navy", foreground="White")
rp.messagebox("A tabbed notebook can be placed inside a tabbed notebook.")
rp.notebook(panel, c("Tab within tab", "Another tab"),
            width=200, height=100, parentname="File", name="n3")
rp.notebook.raise(panel, "n1", "File")
rp.notebook.raise(panel, "n3", "Another tab")
}}

\keyword{iplot}
\keyword{dynamic}
