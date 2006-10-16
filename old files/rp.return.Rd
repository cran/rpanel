\name{rp.return}

\alias{rp.return}

\title{Returns panel to the global environment}

\description{
This function should be called in a user written callback function to return any alterations made 
to the panel within the function to the global environment where panels are stored. The panel is 
automatically sent to callback functions as panel for the callback function to use. Any widgets 
added to the function or alterations made to the panel's properties will be lost if the panel 
is not returned to the global environment.
}

\usage{
rp.return <- (panel)
}

\arguments{
  \item{panel}{this is the panel sent into the callback function as panel.}
}

\value{
  As a side effect this returns the panel.
}

\references{rpanel: Simple interactive controls for R functions using 
the tcltk library 
(http://www.stats.gla.ac.uk/~adrian/research-reports/rpanel.ps)}

\seealso{ \code{\link{rp.line}} }

\examples{
# rp.return is used to send the new line and the panel$x and panel$y back to the global environment
click.capture <- function(panel,x,y) {
  if (is.null(panel$x)) { 
    panel$x <- as.numeric(x)
    panel$y <- as.numeric(y) 
  } else { 
    rp.line(panel, gulls.image, panel$x, panel$y, as.numeric(x), as.numeric(y), width=3, id = "current")
    panel$x <- as.numeric(x)
    panel$y <- as.numeric(y)
  }
  rp.return(panel)
}
gulls.panel <- rp.control()
image.file <- file.path(system.file(package = "rpanel"), "data", "gulllmks.gif")
rp.image(gulls.panel, image.file, id = "gulls.image", action = click.capture)
}

\keyword{iplot}
\keyword{dynamic}
