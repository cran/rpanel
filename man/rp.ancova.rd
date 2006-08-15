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
  rp.ancova(x, y, group, panel = TRUE, model = "None",
       xlab = deparse(substitute(x)), ylab = deparse(substitute(y)))
}

\arguments{
  \item{x}{a vector of covariate values.}
  \item{y}{a vector of response values.}
  \item{group}{a vector of group indicators.}
  \item{panel}{a logical variable which determines whether a panel is created to allow interactive control of the fitted models.}
  \item{model}{a character variable defining the model to be fitted, if panel is set to FALSE.  The valid values are "None", "Single mean", "Single line", "Parallel lines", and "Different lines".}
  \item{xlab}{a character variable used for the covariate axis label.}
  \item{ylab}{a character variable used for the response axis label.}
}

\details{
  Static plots, for printing or other purposes can be created by setting
  the panel argument to FALSE and specifying the model of interest.
}

\value{
  If panel is TRUE, the name of the R panel object is returned.
  If panel is FALSE, nothing is returned.
}

\references{rpanel: Simple interactive controls for R functions using
the tcltk package
(http://www.stats.gla.ac.uk/~adrian/research-reports/rpanel.ps)}

\examples{
  x <- runif(50)
  y <- x + rnorm(50, sd = 0.2)
  g <- rbinom(50, 1, 0.5)
  rp.ancova(x, y, g)
}

\keyword{iplot}
\keyword{dynamic}
