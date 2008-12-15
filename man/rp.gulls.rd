\name{rp.gulls}

\alias{rp.gulls}

\title{STEPS module: the Birds and the Bees}

\description{
  The function launches a panel which contains an image of a
  herring gull.  With this bird, sex cannot easily be identified by
  visual inspection.  The user is invited to identify length measurements,
  defined by pairs of landmarks, which will enable males and females
  to be identified.
}

\usage{
rp.gulls(df.name = "gulls", panel.plot = TRUE, sleep = 0.5)
}

\arguments{
  \item{df.name}{a string defining the name of the dataframe to which collected measurements will be added.}
  \item{panel.plot}{whether to plot or not.}
  \item{sleep}{the duration in seconds of a pause while the necessary internal information is loaded
                into the panel.  See Details.}
}

\details{The panel contains an image with landmarks indicated by yellow dots.  When the user clicks two landmarks, a length measurement is indicated by a coloured line.  The `Collect data' button can be clicked to request that this measurement is collected, on a database of birds whose sex is known.  If the measurement is a valid and useful one, it is added to the named dataframe, which is available for inspection and analysis.  If the measurement is invalid or not useful, an appropriate message is given in a pop-up window.

On some machines the \code{R} and Tcl/Tk code can become out of step because of the time taken to initialise panel with the large amount of internal information required for plotting.  The \code{sleep} argument allows a pause for this to be completed before further \code{rpanel} instructions are executed.  If the \code{rpanel} window displays with very small size, try increasing the value of \code{sleep}.
}

\value{
  the name of the panel created.
}

\references{
   rpanel: Simple interactive controls for R functions using the tcltk package.
      Journal of Statistical Software, 17, issue 9.
   }

\examples{
if (interactive()) {
  rp.gulls()
  }}

\keyword{iplot}
\keyword{dynamic}
