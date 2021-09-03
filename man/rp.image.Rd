\name{rp.image}

\alias{rp.image}

\title{Placement of an image within a rpanel}

\description{
An image is placed inside a panel. When the image is clicked the action function is called with the x and y coordinates of the clicked position.
}

\usage{
rp.image(panel, filename, imagename, action=NA, mousedrag=NA, mouseup=NA, pos=NULL,
  parentname=deparse(substitute(panel)), ...) 
}

\arguments{
  \item{panel}{the panel in which the image should appear. This 
  may be passed as a panelname string or the panel object itself.}
  \item{filename}{the name of the file where the image is located.}
  \item{imagename}{name assigned to the image, used for disposing of the widget}  
  \item{action}{the function which is called when the image is clicked.}
  \item{mousedrag}{the function which is called when the mouse is dragged.}
  \item{mouseup}{the function which is called when the mouse is released.}
  \item{pos}{the layout instructions. Please see the \code{\link{rp.pos}} example and help for full details.}
  \item{parentname}{this specifies the widget inside which the image should 
    appear.}
  \item{...}{...}
}

\note{
The former arguments \code{parent} and \code{...} have been discontinued in version 1.1.  Note also that the argument \code{id} has been renamed \code{name} to be consistent with the rest of rpanel.
}

\details{
The function \code{action} should take three arguments, the panel and the coordinates \code{x} and \code{y} where the image was clicked. At present only GIF images are supported.
  
See \code{\link{rp.grid}} for details of the grid layout system.
}

\value{
If the argument \code{panel} is the panelname string, the same string is returned.  If the panel object is used, the altered panel is assigned to both the calling level and panel's environment level.
}

\section{Warning}{
The \code{action} function should return the panel.  Without this assignment any widgets added or alterations made to panel parameters within the \code{action} function will be lost.
}

\references{
   rpanel: Simple interactive controls for R functions using the tcltk package.
      Journal of Statistical Software, 17, issue 9.
   }

\examples{
\dontrun{
   gulls.click <- function(panel, x, y) {
     print(c(x, y))
     panel
     }
   panel <- rp.control()
   image.file <- file.path(system.file(package = "rpanel"), "images", "gulllmks.gif")
   rp.image(panel, image.file, gulls.image, action = gulls.click)
}}

\keyword{iplot}
\keyword{dynamic}
