\name{rp.deleteline}

\alias{rp.deleteline}

\title{Removes a line from an rpanel image}

\description{
  This removes a previously drawn line which was given an id in rp.line.
}

\usage{
rp.deleteline(panel, image, id)
}

\arguments{
  \item{panel}{the panel containing the image. This 
  may be passed as a panelname string or the panel object itself.}
  \item{image}{the image on which the line was drawn.}
  \item{id}{the identifier of the line to be deleted.}
}

\details{
}

\value{
  If the parameter panel is the panelname string the same string is returned. If the 
  panel object is used the altered panel is assigned to both the calling level and global 
  level.
}

\references{rpanel: Simple interactive controls for R functions using 
the tcltk package 
(http://www.stats.gla.ac.uk/~adrian/research-reports/rpanel.ps)}
\seealso{\code{\link{rp.clearlines}},\code{\link{rp.line}},\code{\link{rp.image}}}

\examples{
panel <- rp.control()
image.file <- file.path(system.file(package = "rpanel"), "images", "gulllmks.gif")
rp.image(panel, image.file, id = "gulls.image")
rp.line(panel, gulls.image, 10, 10, 100, 100, color = "green", id="first")
rp.line(panel, gulls.image, 100, 100, 100, 10, color = "blue", id="second")
rp.deleteline(panel, gulls.image,"first") # deletes only the first line leaving the vertial blue one.
}
\keyword{iplot}
\keyword{dynamic}
