\name{rp.regression2}

\alias{rp.regression2}

\title{Interactive display of regression with two covariates}

\description{
  This function plots a response variable against two covariates and allows the 
  creation of a panel which controls the model which is fitted to the data and 
  displayed on the plot.  The plot is constructed with the aid of the rgl package 
  for three-dimensional display.
}

\usage{
rp.regression2(y, x1, x2, ylab  = NA, x1lab = NA, x2lab = NA, 
               panel = TRUE, model = "None", residuals.showing = FALSE,
               size = 3, col = "red")
}

\arguments{
   \item{y}{a vector of response values.}
   \item{x1}{a vector of covariate values.}
   \item{x2}{a vector of covariate values.}
   \item{ylab}{a character variable used for the response axis label.}
   \item{x1lab}{a character variable used for the first covariate axis label.}
   \item{x2lab}{a character variable used for the second covariate axis label.}
   \item{panel}{a logical variable which determines whether a panel is created to allow interactive control of the fitted models.}
   \item{model}{a character variable defining the model to be fitted, if panel is set to FALSE. The valid values are "None", the name of the first and second covariates and the combination of these names with an "&".}
   \item{residuals.showing}{a logical value determining whether the residuals are shown on the plot.}
   \item{size}{the size of the plotted points.}
   \item{col}{the colour of the plotted points.}
}

\details{
  Static plots, for printing or other purposes can be created by setting the panel 
  argument to FALSE and specifying the model of interest.
}

\value{
  If panel is TRUE, the name of the R panel object is returned. If panel is FALSE, 
  nothing is returned.
}

\references{rpanel: Simple interactive controls for R functions using
the tcltk library
(http://www.stats.gla.ac.uk/~adrian/rpanel/)}

\seealso{\code{\link{rp.plot3d}}}

\examples{
  data(CofE)
  attach(CofE)
  rp.regression2(Giving, Employ, Attend)
}

\keyword{iplot}
\keyword{dynamic}
