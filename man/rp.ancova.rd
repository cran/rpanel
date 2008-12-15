\name{rp.ancova}

\alias{rp.ancova}

\title{Interactive analysis of covariance}

\description{
  This function plots a response variable against a covariate, with
  different groups of data identified by colour and symbol.  It also
  creates a panel which controls the model which is fitted to the
  data and displayed on the plot.
}

\usage{
  rp.ancova(x, y, group, panel = TRUE, panel.plot = TRUE, model = "None",
       xlab = deparse(substitute(x)), ylab = deparse(substitute(y)), hscale = NA, vscale = hscale)
}

\arguments{
\item{x}{a vector of covariate values.}
\item{y}{a vector of response values.}
\item{group}{a vector of group indicators.}
\item{panel}{a logical variable which determines whether a panel is created to allow interactive control of the fitted models.}
\item{panel.plot}{a logical parameter which determines whether the plot is placed inside the panel (TRUE) or the standard graphics window (FALSE).  If the plot is to be placed inside the panel then the \code{tkrplot} library is required.}
\item{model}{a character variable defining the model to be fitted, if panel is set to FALSE.  The valid values are "None", "Single mean", "Single line", "Parallel lines", and "Different lines".}
\item{xlab}{a character variable used for the covariate axis label.}
\item{ylab}{a character variable used for the response axis label.}
\item{hscale, vscale}{scaling parameters for the size of the plot when \code{panel.plot} is set to \code{TRUE}.  The default values are 1 on Unix platforms and 1.4 on Windows platforms.}
}

\details{
  Static plots, for printing or other purposes can be created by setting
  the panel argument to FALSE and specifying the model of interest.
}

\value{
  If panel is TRUE, the name of the R panel object is returned.
  If panel is FALSE, nothing is returned.
}

\references{
   rpanel: Simple interactive controls for R functions using the tcltk package.
      Journal of Statistical Software, 17, issue 9.
   }

\examples{
if (interactive()) {
   data(gullweight)
   attach(gullweight)
   rp.ancova(hab, weight, month)
   }}

\keyword{iplot}
\keyword{dynamic}
