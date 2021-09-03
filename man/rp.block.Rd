\name{rp.block}

\alias{rp.block}

\title{Blocks use of the \R console until a panel is closed}

\description{
This function prevents the \R console from accepting further input waits until a panel is closed. The function has two uses.  The first is to keep \R active when an R script is run in batch mode. This prevents the \R session from terminating until the panel has been closed.  The second use is to block the user from further use of the command prompt.  There may be circumstances in which it is helpful to do this.
}

\usage{
rp.block(panel)
}

\arguments{
  \item{panel}{the panel whose closure will lead to termination of rp.block.}
}

\details{
\code{rp.block} should usually be the very last function executed in a script, to prevent 
termination until the panel has been closed.
}

\value{
Nothing is returned.
}

\references{
rpanel: Simple interactive controls for R functions using the tcltk package 
(\url{http://www.stats.gla.ac.uk/~adrian/rpanel/})}

\seealso{\code{\link{rp.control}} }

\examples{
\dontrun{
# This function will be called on pressing the button "Simulate".
boxp.sim <- function(panel) {
  boxplot(rnorm(50))
  panel
}
# Create an rpanel and add the button "Simulate" to it.
panel <- rp.control()
rp.button(panel, action = boxp.sim, title = "Simulate")
rp.block(panel)
}
}

\keyword{iplot}
\keyword{dynamic}
