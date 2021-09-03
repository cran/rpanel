\name{rp.panel}

\alias{rp.panel}

\title{Returns a panel}

\description{
Returns a named (by passing the name as a string parameter) panel.
}

\usage{
rp.panel(panelname)
}

\arguments{
  \item{panelname}{optional string parameter. If set the panel of that name is returned, if not set the most recently created panel is returned. } 
}

\value{
If \code{panelname} is set, the panel of that name is returned.  If it is not set, the most recently created panel is returned.
}

\section{Warning}{
Note: returning of the most recent panel may fail when running \R on a Windows machine in DOS. A warning is contained within the function.
}

\references{
   rpanel: Simple interactive controls for R functions using the tcltk package.
      Journal of Statistical Software, 17, issue 9.
   }

\seealso{ \code{\link{rp.control}} }

\examples{
\dontrun{
   # create a panel - will be created in .rpenv as "newpanel"
   rp.control(panelname = "newpanel")
   # creates the panel, but does not return a handle to it - created as ".rpanel2"
   rp.control()
   # pick up the first panel
   panel2 <- rp.panel("newpanel")
}}

\keyword{iplot}
\keyword{dynamic}
