\name{rp.tables}

\alias{rp.tables}

\title{Interactive statistical tables}

\description{
This function launches a panel which allows standard normal, t, chi-squared and F distributions to be plotted, with interactive control of parameters, tail probability and p-value calculations.
}

\usage{
  rp.tables(panel = TRUE, panel.plot = TRUE, hscale = NA, vscale = hscale,
            distribution = "normal", degf1 = 5, degf2 = 30,
            observed.value = " ", observed.value.showing = !is.na(observed.value),
            probability = 0.05, tail.probability, tail.direction, heading)
}

\arguments{
\item{panel}{a logical parameter which determines whether interactive controls are provided or a simple static plot is produced.}
\item{panel.plot}{a logical parameter which determines whether the plot is placed inside the panel (TRUE) or the standard graphics window (FALSE).  If the plot is to be placed inside the panel then the tkrplot package is required.}
\item{hscale, vscale}{horizontal and vertical scaling factors for the size of the plot when \code{panel.plot} is set to \code{TRUE}.  It can be useful to adjust these for projection on a screen, for example.  The default values are 1 on Unix platforms and 1.4 on Windows platforms.}
\item{distribution}{a character string which determines which distribution is to be plotted.  Current options are "normal" (default), "t", "chi-squared" and "F".}
\item{degf1, degf2}{The degrees of freedom used for the chi-squared (\code{degf1}) and F (\code{degf1}, \code{degf2}) distributions.}
\item{observed.value}{a numerical value, or a character string which will be converted by \code{as.numeric}, which identifies an observed value whose location within the distribution is of interest.}
\item{observed.value.showing}{a logical value which determines whether the observed value (if any) is displayed on the plot.}
\item{probability}{the value of the tail probability used when tail area is shaded.}
\item{tail.probability}{a character string which determines whether the tail area is drawn from the observed value (\code{"from observed value"}), using a fixed probability (\code{"fixed probability"}) or not shown (\code{"none"}).}
\item{tail.direction}{a character string which determines whether the lower (\code{"lower"}), upper (\code{"upper"}) or two-sided (\code{"two-sided"}) tail area is drawn.}
\item{heading}{a character string which will appear as a heading of the plot.  If this is missing, a heading based on the selected distribution will be created.}
}

\details{
The panel contains radiobuttons to select the standard normal, t, chi-squared or F distributions.  Doublebuttons are available to control the degrees of freedom.  An observed value can be added to the plot, with optional determination of the corresponding p-value.  Alternatively, shaded areas corresponding to tail probabilities of specified value can be displayed.
}

\value{
  Nothing is returned.
}

\references{
   rpanel: Simple interactive controls for R functions using the tcltk package.
      Journal of Statistical Software, 17, issue 9.
   }

\examples{
\dontrun{
  rp.tables()
}}

\keyword{iplot}
\keyword{dynamic}
