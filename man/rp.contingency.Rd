\name{rp.contingency}

\alias{rp.contingency}

\title{Visualisation of two-way contingency tables with uncertainties}

\description{
A two-way contingency table is displayed graphically and the uncertainties associated with a chi-squared test may be visualised.
}

\usage{
rp.contingency(x, uncertainty = 'none', proportion.scale = 'fixed', cols)
}

\arguments{
\item{x}{a two-way table or matrix of observed counts.}
\item{uncertainty}{a character value which controls whether uncertainties are displayed for each cell, in \code{'shading'} or \code{'violin'} style.}
\item{proportion.scale}{a character value specifying whether the proportion scale is \code{'fixed'} or \code{'free'}.}
\item{cols}{a named vector of characters specifying colours to be used in the plot. See \code{help(rp.colours)} for details.}
}

\details{
The table is displayed in a form which stratifies by columns. If it more appropriate to stratify by the rows of \code{x} then the function may be applied to \code{t(x)}.

The uncertainties associated with each cell are based on the standard errors associated with standardised residuals. See the help file for \code{chisq.test} for details.

Missing data are not allowed.
}

\examples{
\dontrun{
   x <- matrix(c(19, 41, 32, 28), ncol = 2,
            dimnames = list(c("non-smoker", "smoker"),
                            c("cases", "controls")))
   rp.contingency(x)
   rp.contingency(x, style = "aligned")
   rp.contingency(x, uncertainty = TRUE)
   rp.contingency(x, uncertainty = TRUE) + ggplot2::coord_flip()
}}

\keyword{iplot}
\keyword{dynamic}
