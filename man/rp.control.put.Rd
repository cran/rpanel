\name{rp.control.put}

\alias{rp.control.put}

\title{Updates the panel environment with the current value of the panel list object.}

\description{
Sometimes an action function makes changes to the panel list object.  When the action function is completed, the panel environment is updated.  However, if there are other calls to action functions within the original action function, then the panel environment needs to be updated before these calls.  This function achieves that.
}

\usage{
rp.control.put(panelname, panel)
}

\arguments{
  \item{panelname}{the panelname of the relevant panel.  This is usually identified as \code{panel$panelname}.}
  \item{panel}{the relevant panel.}
}

\references{
   rpanel: Simple interactive controls for R functions using the tcltk package.
      Journal of Statistical Software, 17, issue 9.
   }

\seealso{\code{\link{rp.control}}}

\examples{
\dontrun{
  action1 <- function(panel) {
    panel$x <- rnorm(1)
    rp.control.put(panel$panelname, panel)
    rp.do(panel, action2)
    panel
  }
  action2 <- function(panel) {
  	print(panel$x)
  	panel
  }
  panel <- rp.control(x = 0)
  rp.button(panel, action1, "new x")
}}

\keyword{iplot}
\keyword{dynamic}
