\name{rp.clearlines}

\alias{rp.clearlines}

\title{Remove lines from an rpanel image}

\description{
  This function removes line(s) from an rpanel image widget: \code{rp.clearlines} removes all the lines from an image while \code{rp.deleteline} deletes only a given line.
}

\usage{
rp.clearlines(panel, image)
}

\arguments{
  \item{panel}{the panel which contains the image. This 
  may be passed as a panelname string or the panel object itself.}
  \item{image}{the name of the image within the panel.}
}

\value{
  If the parameter panel is the panelname string the same string is returned. If the 
  panel object is used the altered panel is assigned to both the calling level and panel's environment 
  level.
}

\references{
   rpanel: Simple interactive controls for R functions using the tcltk package.
      Journal of Statistical Software, 17, issue 9.
   }

\seealso{\code{\link{rp.image}},\code{\link{rp.line}}}

\examples{
if (interactive()) {
   panel <- rp.control()
   image.file <- file.path(system.file(package = "rpanel"), "images", "gulllmks.gif")
   rp.image(panel, image.file, id = "gulls.image")
   rp.line(panel, gulls.image, 10, 10, 100, 100, color = "green")
   rp.line(panel, gulls.image, 100, 100, 100, 10, color = "blue")
   rp.clearlines(panel, gulls.image)
   }}

\keyword{iplot}
\keyword{dynamic}
