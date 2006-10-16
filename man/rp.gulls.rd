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
rp.gulls(df.name = "gulls", plot.panel = TRUE)
}

\arguments{
  \item{df.name}{a string defining the name of the dataframe to which collected measurements will be added.}
  \item{plot.panel}{whether to plot or not.}
}

\details{
  The panel contains an image with landmarks indicated by yellow dots.
  When the user clicks two landmarks, a length measurement is indicated
  by a coloured line.  The `Collect data' button can be clicked to
  request that this measurement is collected, on a database of birds
  whose sex is known.  If the measurement is a valid and useful one,
  it is added to the named dataframe, which is available for inspection
  and analysis.  If the measurement is invalid or not useful, an
  appropriate message is given in a pop-up window.
}

\value{
  the name pf the panel created.
}

\section{Warning}{
Note: The parameters should not normally be used.
}

\references{rpanel: Simple interactive controls for R functions using
the tcltk package
(http://www.stats.gla.ac.uk/~adrian/rpanel/)}

\examples{
  rp.gulls()
}

\keyword{iplot}
\keyword{dynamic}
