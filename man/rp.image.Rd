\name{rp.image}

\alias{rp.image}

\title{Placement of an image within a rpanel}

\description{
  An image is placed inside a panel. When the image is clicked the action function is called with the x and y coordinates of the clicked position.
}

\usage{
rp.image(panel, filename, action = I, id = "", parent = window, pos = NULL)
}

\arguments{
  \item{panel}{the panel in which the image should appear. This 
  may be passed as a panelname string or the panel object itself.}
  \item{filename}{the name of the file where the image is located.}
  \item{action}{the function which is called when the image is clicked.}
  \item{id}{the name of the image.}
  \item{parent}{this specifies the widget inside which the image should appear. In the current version of rpanel, it should not normally be used.}
  \item{pos}{the layout instructions. Please see the \code{\link{rp.pos}} example and help for full details.}
}

\details{
  The function \code{action} should take three arguments, the panel and the coordinates x and y where the image was clicked. At present only GIF images are supported.
}

\value{
  If the parameter panel is the panelname string the same string is returned. If the 
  panel object is used the altered panel is assigned to both the calling level and global 
  level.
}

\section{Warning}{
  The \code{action} function should return the panel. 
  Without this assignment any widgets added or alterations made to panel parameters within 
  the \code{action} function will be lost.
}

\references{rpanel: Simple interactive controls for R functions using 
the tcltk package 
(http://www.stats.gla.ac.uk/~adrian/research-reports/rpanel.ps)}
\seealso{ \code{\link{rp.control}} }

\examples{
gulls.click <- function(panel, x, y) {
  print(c(x, y))
  panel
}
panel <- rp.control()
image.file <- file.path(system.file(package = "rpanel"), "images", "gulllmks.gif")
rp.image(panel, image.file, id = "gulls.image", action = gulls.click)
}

\keyword{iplot}
\keyword{dynamic}
