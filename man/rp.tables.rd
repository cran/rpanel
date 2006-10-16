\name{rp.tables}

\alias{rp.tables}

\title{Interactive statistical tables}

\description{
  This function launches a panel which allows standard normal, t,
  chi-squared and F distributions to be plotted, with interactive
  control of parameters, tail probability and p-value calculations.
}

\usage{
  rp.tables(panel.plot = FALSE)
}

\arguments{
  \item{panel.plot}{a logical parameter which determines whether the
                plot is placed inside the panel (TRUE) or the
                standard graphics window (FALSE).  If the plot is
                to be placed inside the panel then the tkrplot
                library is required.}
}

\details{
  The panel contains radiobuttons to select the standard normal, t,
  chi-squared or F distributions.  Doublebutton are available to
  control the degrees of freedom.  An observed value can be added
  to the plot, with optional determination of the corresponding
  p-value.  Alternatively, shaded areas corresponding to tail
  probabilities of specified value can be displayed.
}

\value{
  the name of the panel object.
}

\references{rpanel: Simple interactive controls for R functions using
the tcltk library
(http://www.stats.gla.ac.uk/~adrian/rpanel/)}

\examples{
  rp.tables()
}

\keyword{iplot}
\keyword{dynamic}
