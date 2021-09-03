\name{rp.surface}

\alias{rp.surface}

\title{Interactive visualisation of a surface and its uncertainty}

\description{
  This function plots a surface and uses interactive interrogation by the mouse, or a sequence of animations, to indicate the uncertainty in the surface as an estimate of the true surface.
}

\usage{
      rp.surface(surface, covariance, x1grid, x2grid, x, y, Display = "persp",
                       hscale = 1, vscale = hscale, panel = TRUE,
                       Speed = 5, ntime = 10, ninterp = 50,
                       zlim = NULL, col.palette = topo.colors(100), coords = rep(NA, 2))
}

\arguments{
\item{surface}{a matrix of estimated surface values over a regular grid.}
\item{covariance}{the covariance matrix for the estimates in \code{surface}, corresponding to the estimates in vector form \code{c(surface)}.}
\item{x1grid, x2grid}{vectors defining the regular grids over each margin of \code{surface}.}
\item{x}{an optional two-column matrix of observed covariate values.}
\item{y}{an optional vector of response values.}
\item{Display}{a character value which determines the initial type of surface plot.  Options are "image" (the default) and "persp".}
\item{hscale, vscale}{scaling parameters for the size of the plot.}
\item{panel}{a logical variable which determines whether a panel is created to allow interactive control.}
\item{Speed}{this determines the initial value of the speed of animations by setting the value of the sleep time (in hundredths of a second, with an offset of 2) between displayed surfaces.}
\item{ntime}{the number of interpolated surfaces displayed between successive simulated surfaces, to control the smoothness of the animation.}
\item{ninterp}{the number of grid values in each dimension when constructing a surface for the "image" display option.  This is used because the input grid of \code{surface} may have quite low resolution which produces a rather chunky image display.  A finer grid is constructed if the \code{akima} package is available.}
\item{zlim}{a vector of length two which defines the range of plotting on the surface scale.  By default, \code{zlim} is determined by the range of \code{surface} plus and minus three standard deviations (available from \code{covar}).}
\item{col.palette}{the colour palette used to paint the surface.  The colours are determined simply by the height of the surface.}
\item{coords}{a vector of length two which defines the location where the uncertainty in the surface is examined, through the construction of a variability interval.  This applies when \code{panel = FALSE} and \code{Display = "image"}.}
}

\details{
  The interactive controls allow the surface to be plotted using \code{image} or \code{persp} displays, and with the display of uncertainty through mouse click and drag on the \code{image} plot or animation.
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
if (require(sm)) {
   with(trawl, {
      location  <- cbind(Longitude, Latitude)
      model     <- sm.regression(location, Score1, ngrid = 15, display = "none")
      longitude <- model$eval.points[ , 1]
      latitude  <- model$eval.points[ , 2]
      xgrid     <- as.matrix(expand.grid(longitude, latitude))
      S         <- sm.weight2(location, xgrid, model$h)
      covar     <- tcrossprod(S) * model$sigma^2
      rp.surface(model$estimate, covar, longitude, latitude, location, Score1)
   })
}}}
