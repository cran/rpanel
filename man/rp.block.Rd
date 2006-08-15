\name{rp.block}

\alias{rp.block}

\title{Blocks use of the R console until a panel is closed}

\description{
  This function prevents the R console from accepting further input waits until 
  a panel is closed. The function has two uses: The first is to keep R active 
  when an R script is run in batch mode. This prevents the R session from 
  terminating until the panel has been closed. The second use is to block the 
  user from further use of the command prompt. There may be circumstances in 
  which it is helpful to do this.
}

\usage{
rp.block(panel)
}

\arguments{
  \item{panel}{the panel whose closure will lead to termination of rp.block. This 
  may be passed as a panelname string or the panel object itself.}
}

\details{
rp.block should usually be the very last function executed in a script, to prevent 
termination until the panel has been closed.  It should not normally be used in interactive 
mode, except where one wishes to prevent use of the R command line whilst 
the panel is running.
}

\value{
  If the parameter panel is the panelname string the same string is returned. If the 
  panel object is used the altered panel is assigned to both the calling level and global 
  level.
}

\section{Warning}{
Note: This may stop the panel responding to button and other events in MacOS command line.
}

\references{rpanel: Simple interactive controls for R functions using 
the tcltk package 
(http://www.stats.gla.ac.uk/~adrian/research-reports/rpanel.ps)}

\seealso{ \code{\link{rp.control}} }

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
