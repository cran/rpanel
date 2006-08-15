\name{rp.panel}

\alias{rp.panel}

\title{Returns a panel}

\description{
Returns the most recently created panel or a named (by passing the name as a string parameter) panel.
}

\usage{
rp.panel <- (panelname = rp.panelname(new = FALSE))}

\arguments{
  \item{panelname}{optional string parameter. If set the panel of that name is returned, if not set the most recently created panel is returned. } 
}

\value{
  If panelname is set the panel of that name is returned, if not set the most recently created panel is returned.
}

\section{Warning}{
Note: returning of the most recent panel may fail when running R on a Windows machine in DOS. A warning is contained within the function.
}

\references{rpanel: Simple interactive controls for R functions using 
the tcltk package 
(http://www.stats.gla.ac.uk/~adrian/research-reports/rpanel.ps)}

\seealso{ \code{\link{rp.control}} }

\examples{
rp.control(realname = "newpanel") # create a panel - will be created in .GlobalEnv as "newpanel"
rp.control() # creates the panel, but does not return a handle to it - created as ".rpanel2"
panel2 <- rp.panel("newpanel") # pick up the first panel
\dontrun{panel <- rp.panel() # gets a handle to the latest panel created}
}

\keyword{iplot}
\keyword{dynamic}
