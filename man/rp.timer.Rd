\name{rp.timer}

\alias{rp.timer}

\title{Creates a series of timed actions}

\description{
This creates an interval timer and allows the user to set the criteria to stop the timer.
}

\usage{
rp.timer(panel, microseconds, action, where)
}

\arguments{
  \item{panel}{the panel which has some relevant variables.}
  \item{microseconds}{time between each call of action.}
  \item{action}{function to be executed on each timer tick.}
  \item{where}{a function which should return true or false, taking parameter panel. When false the loop will stop.}
}

\details{
This allows the user to setup an interval timer and the function to be called at each 'tick'.

Care should be taken when writing code to anticipate interactions with the panel while activity controlled by a timer is underway, as these interactions may cause changes in the state of the panel.
}

\references{
   rpanel: Simple interactive controls for R functions using the tcltk package.
      Journal of Statistical Software, 17, issue 9.
   }

\seealso{\code{\link{rp.control}}}

\examples{
\dontrun{
  stopme <- function(panel) panel$count<=20
  callme <- function(panel) {
    print(panel$count)
    panel$count = panel$count+1
    panel
  } 
  panel <- rp.control(count=1)
  rp.timer(panel, 500, callme, stopme)
}}

\keyword{iplot}
\keyword{dynamic}
