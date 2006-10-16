\name{rp.panelname}

\alias{rp.panelname}

\title{Automatic generation of a panel name}

\description{
This creates the automatically assigned internal 'realname' parameter of a 
panel. 'realname' is the internally used name of the panel used by 
callback functions. In rpanel these will always have the form '.rpanelxxxxxxxx' 
where 'xxxxxxxx' is generated randomly whenever a panel is created.
}

\usage{
rp.panelname(new = TRUE)
}

\arguments{
  \item{new}{optional parameter. When set to FALSE this will return the most recently created panel name. } 
}

\value{
  A string of the form 'rpanelx', where x is an integer.
}

\section{Warning}{
Note: returning of the most recent panel may fail when running R on a Windows machine in DOS. A warning is contained within the function.
}

\references{rpanel: Simple interactive controls for R functions using 
the tcltk package 
(http://www.stats.gla.ac.uk/~adrian/rpanel/)}

\seealso{ \code{\link{rp.control}} }

\examples{
hist.draw <- function(panel) {
  hist(rnorm(50))
  panel
}
hist.sample <- function() {
  panel.name <- rp.panelname()
  rp.control(realname = panel.name)
  rp.button(panel.name, title = "Sample", action = hist.draw)
}
hist.sample()    
}

\keyword{iplot}
\keyword{dynamic}
