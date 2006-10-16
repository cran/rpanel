\name{rp.messagebox}

\alias{rp.messagebox}

\title{Displays a message}

\description{
  This function displays a message in a pop-up window.
}

\usage{
rp.messagebox(..., title="rpanel Message")
}

\arguments{
  \item{\dots}{parameters containing the message to be displayed.}
  \item{title}{the title for the message window.}
}

\details{
  The pop-up window remains displayed and no other action can be taken, until the 'ok' button is pressed. 
}

\value{
  None.
}

\references{rpanel: Simple interactive controls for R functions using 
the tcltk package 
(http://www.stats.gla.ac.uk/~adrian/rpanel/)}

\seealso{ \code{\link{rp.control}} }

\examples{
\dontrun{
# This is not run as RCMD CHECK will be suspended until OK is clicked ...
rp.messagebox("Click OK to continue.", title = "Test message")
}
}

\keyword{iplot}
\keyword{dynamic}
