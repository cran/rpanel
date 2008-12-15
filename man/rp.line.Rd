\name{rp.line}

\alias{rp.line}

\title{Draws a line on an rpanel image}

\description{
  This draws a line connecting the pixel locations x1, y1 to x2, y2 on the specified image. The colour and width of the line can be controlled.
}

\usage{
rp.line(panel, image, x1, y1, x2, y2, ..., color = "black", width = 2, id = 'rpline')
}

\arguments{
  \item{panel}{the panel containing the image. This 
  may be passed as a panelname string or the panel object itself.}
  \item{image}{the image on which the line should be drawn.}
  \item{x1}{the horizontal first position of start of the line in pixel co-ordinates.}
  \item{y1}{the vertical first position of start of the line in pixel co-ordinates.}
  \item{x2}{the horizontal final position of end of the line in pixel co-ordinates.}
  \item{y2}{the vertical final position of end of the line in pixel co-ordinates.}
  \item{\dots}{any further parameters.}
  \item{color}{the colour of the line. The default is "black".}
  \item{width}{the width of the line. The default is 2.}
  \item{id}{the identifier of the line created.}
}

\details{
  The function \code{action} should take one argument, which should be the panel to which the line is attached. 
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

\seealso{\code{\link{rp.tkrplot}},\code{\link{rp.image}}}

\examples{
if (interactive()) {
   click.capture <- function(panel,x,y) {
     if (is.null(panel$x)) { 
       panel$x <- as.numeric(x)
       panel$y <- as.numeric(y) 
       } 
     else { 
       rp.line(panel, gulls.image, panel$x, panel$y, as.numeric(x), as.numeric(y), width=3, id = "current")
       panel$x <- as.numeric(x)
       panel$y <- as.numeric(y)
     }
     panel
   }
   gulls.panel <- rp.control()
   image.file <- file.path(system.file(package = "rpanel"), "images", "gulllmks.gif")
   rp.image(gulls.panel, image.file, id = "gulls.image", action = click.capture)
   }}

\keyword{iplot}
\keyword{dynamic}
