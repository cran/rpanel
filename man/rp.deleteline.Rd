\name{rp.deleteline}

\alias{rp.deleteline}

\title{Removes a line from an rpanel image}

\description{
  This removes a previously drawn line which was given an id in rp.line.
}

\usage{
rp.deleteline(panel, imagename, id)
}

\arguments{
  \item{panel}{the panel containing the image. This 
  may be passed as a panelname string or the panel object itself.}
  \item{imagename}{the image on which the line was drawn.}
  \item{id}{the identifier of the line to be deleted.}
}

\note{
In version 1.1, the former argument \code{image} has been renamed \code{name} to be consistent with the rest of \pkg{rpanel}.
}

\value{
  If the argument \code{panel} is the panelname string the same string is returned.  If the panel object is used, the altered panel is assigned to both the calling level and panel's environment level.
}

\references{
   rpanel: Simple interactive controls for R functions using the tcltk package.
      Journal of Statistical Software, 17, issue 9.
   }

\examples{
\dontrun{
   panel <- rp.control()
   image.file <- file.path(system.file(package = "rpanel"), "images", "gulllmks.gif")
   panel <- rp.image(panel, image.file, imagename="gulls.image")
   rp.line(panel, imagename=gulls.image, 10, 10, 100, 100, color = "green", id="first")
   rp.line(panel, imagename=gulls.image, 100, 100, 100, 10, color = "blue", id="second")
   rp.deleteline(panel, imagename=gulls.image, id="first")
}}

\keyword{iplot}
\keyword{dynamic}
