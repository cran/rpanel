\name{rp.tables}

\alias{rp.tables}

\title{Interactive statistical tables}

\description{
This function launches a panel which allows standard normal, t, chi-squared and F distributions to be plotted, with interactive control of parameters, tail probability and p-value calculations.
}

\usage{
  rp.tables(panel.plot = TRUE, hscale = NA, vscale = hscale)
}

\arguments{
  \item{panel.plot}{a logical parameter which determines whether the
                plot is placed inside the panel (TRUE) or the
                standard graphics window (FALSE).  If the plot is
                to be placed inside the panel then the tkrplot
                library is required.}
  \item{hscale, vscale}{horizontal and vertical scaling factors for the size of the plot
                when \code{panel.plot} is set to \code{TRUE}.
                It can be useful to adjust these for projection on a screen, for example.
                The default values are 1 on Unix platforms and 1.4 on Windows platforms.}
}

\details{
The panel contains radiobuttons to select the standard normal, t, chi-squared or F distributions.  Doublebutton are available to control the degrees of freedom.  An observed value can be added to the plot, with optional determination of the corresponding p-value.  Alternatively, shaded areas corresponding to tail probabilities of specified value can be displayed.
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
