\name{rp.gulls}

\alias{rp.gulls}

\title{STEPS module: the Birds and the Bees}

\description{
The function launches a panel which contains an image of a herring gull.  With this bird, sex cannot easily be identified by visual inspection.  The user is invited to identify length measurements, defined by pairs of landmarks, which will enable males and females to be identified.
}

\usage{
rp.gulls(df.name = "", panel.plot = TRUE)
}

\arguments{
  \item{df.name}{a string giving the filename where the dataframe containing the currently collected measurements will be stored using the \code{save} function.  If this string is the default value of "" then no file will be saved.}
  \item{panel.plot}{whether to plot or not.}
}

\details{
The panel contains an image with landmarks indicated by yellow dots.  When the user clicks two landmarks, a length measurement is indicated by a coloured line.  The `Collect data' button can be clicked to request that this measurement is collected, on a database of birds whose sex is known.  If the measurement is a valid and useful one, it is added to the named dataframe, which is immediately saved in the file \code{df.name} and is therefore available for inspection and analysis simply by \code{load}ing this file.  If the measurement is invalid or not useful, an appropriate message is given in a pop-up window.

Note that in versions of rpanel earlier than 1.1-1 the dataframe containing the collected data was previously forced into the global environment for immediate access.  This has been replaced by the use of a user-nominated file.
}

\value{
  the name of the panel created.
}

\references{
   rpanel: Simple interactive controls for R functions using the tcltk package.
      Journal of Statistical Software, 17, issue 9.
   }

\examples{
\dontrun{
  rp.gulls()
}}

\keyword{iplot}
\keyword{dynamic}
