\name{rp.plot4d}

\alias{rp.plot4d}
\alias{rp.spacetime}

\title{Animated scatterplot}

\description{
This function plots two covariates coloured by a response variable and animates this by a
third covariate.  In particular, it is useful for plotting spatiotemporal data.
}

\usage{
   rp.plot4d(x, z, y, model, group, subset, col.palette, col.breaks, col.labels,
                  hscale = 1, vscale = hscale, panel = TRUE,
                  x1lab, x2lab, zlab, ylab,
                  display = "image", Display = NULL,
                  background.plot = NULL, foreground.plot = NULL, 
                  z.window = "normal", z.window.pars = c(min(z), sd(z)/5),
                  coords = rep(NA, 2), radius = 0.05,  col.circle = "black",
                  lwd.circle = 1,
                  location.plot = TRUE, retain.location.plot = FALSE,
                  group.level, group.name,
                  eqscplot = FALSE, location.plot.type = "histogram")
 
   rp.spacetime(space, time, y, model, group, subset, col.palette, col.breaks, col.labels,
                  hscale = 1, vscale = hscale, panel = TRUE,
                  x1lab, x2lab, zlab, ylab,
                  display = "image", Display = NULL,
                  background.plot = NULL, foreground.plot = NULL,
                  time.window = "normal",
                  time.window.pars = c(min(time), sd(time)/5),
                  coords = rep(NA, 2), radius = 0.05,  col.circle = "black",
                  lwd.circle = 1,
                  location.plot = TRUE, retain.location.plot = FALSE,
                  group.level, group.name,
                  eqscplot = TRUE, location.plot.type = "histogram")
}

\arguments{
\item{x, space}{a two column matrix of covariates, in particular defining spatial locations.}
\item{z, time}{a vector of values, such as times, over which the scatterplot will be animated.}
\item{y}{a vector of response values which will be used to colour the plotted points.}
\item{model}{a list with components \code{x} (a two-column matrix), \code{z} (a vector) and \code{y} (an array) which defines the fitted values (\code{y}) over a
	regular grid of \code{x} and \code{z} values.  When \code{group} is not present \code{y} should be three-dimensional.  When \code{group} is present it should be four-dimensional, with the fourth dimension indexing the fitted values of the model at the different levels of \code{group}.}
\item{group}{an optional factor allowing plots to be created for each factor level.}
\item{subset}{a vector of logical values or indices which will be used to subset \code{x} (or \code{space}), \code{z} (or \code{time}), \code{y}, \code{group} before plotting.}
\item{col.palette, col.breaks, col.labels}{the colour palette used to colour the points,
	the break points on the scale which define the range associated with the each colour and
	the labels associated with the break points.  If \code{col.palette} is missing,
	\code{topo.colors(20)} will be used, or \code{topo.colors} with the number of colours
	set by the number of levels when \code{y} is a factor.  If \code{col.breaks} is missing
	then a regular grid over the range of the observed data is used.  If \code{col.labels}
	is specified then the colour key has a grid of equally spaced colour blocks labelled by
	\code{col.labels}; otherwise the scale is linear.  Setting \code{col.breaks} and
	\code{col.labels} differently can be useful if the data \code{y} are on a transformed
	scale but labels on the original scale are desired.}
\item{hscale, vscale}{scaling parameters for the size of the plot when \code{panel} is
	set to \code{TRUE}.  The default values are 1 on Unix platforms and 1.4 on Windows
	platforms.}
\item{panel}{a logical value determining whether an interactive plot with control panel
	is created.}
\item{x1lab,x2lab,zlab,ylab}{the axis labels of the variables}
\item{display}{a character string which determines whether an "image" or "persp" plot is displayed.}
\item{Display}{a logical vector which controls whether the points, and where present model and reference information, are displayed.}
\item{background.plot,foreground.plot}{function to add further graphical material, such as
	a map, onto the background or foreground of the plot.}
\item{z.window,time.window}{a character string which determines whether the window in
	\code{z} is initially "normal" or "uniform".  This can be changed in the interactive panel.}
\item{z.window.pars,time.window.pars}{a vector of length two which sets initial values for
	the location and width of the \code{z.window}.  These values can be changed in the
	interactive panel.}
\item{coords}{a vector of length two which defines the location of the window in the
	\code{x} space when the function is not used interactively (\code{panel = FALSE}).}
\item{radius}{the radius of the window in the \code{x} space when the function is not
	used interactively.}
\item{col.circle,lwd.circle}{the colour and line width of the circle used to define the
	window in the \code{x} space.}
\item{location.plot}{a logical value which determines whether the mouse can be used to
	interact with the \code{x} plot to create a plot of \code{y} against \code{z} for a
	nominated neighbourhood.}
\item{retain.location.plot}{a logical value which determines the initial state of the
	checkbox determining whether a plot of \code{y} against \code{z} for a nominated
	neighbourhood remains in place after the mouse has been released.}
\item{group.level}{the initial value of the \code{group} factor.  This defaults to the first level.}
\item{group.name}{an optional character value giving a name to the \code{group} variable.}
\item{eqscplot}{a logical value which determines whether the \code{x} plot is constructed
	by using the \code{eqscplot} function in the \pkg{MASS} package, so that the same
	distances on each axis represent the same changes in the corresponding axis variables.}
\item{location.plot.type}{a character variable controlling whether a histogram or a density
	estimate (using the \pkg{lattice} package) is produced when \code{y} is a factor or
	absent and a location plot is requested by clicking the mouse on the plot of \code{x}.}
}

\details{
The colour black should be avoided when using a normal window shape for \code{z}.  This is because hsv shading is used to indicate increasing distance from the current \code{z} location and black has an hsv representation with s component 0, which cannot therefore be reduced further.
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
# The quakes data

with(quakes, {
  rp.plot4d(cbind(long, lat), depth)
  rp.plot4d(cbind(long, lat), depth, mag)
})

# SO2 over Europe

with(SO2, {
  location <- cbind(longitude, latitude)

  if (require(mgcv) & require(maps)) {
     location1 <- location[,1]
     location2 <- location[,2]
     model <- gam(logSO2 ~ s(location1, location2, year))
     loc1  <- seq(min(location1), max(location1), length = 30)
     loc2  <- seq(min(location2), max(location2), length = 30)
     yr    <- seq(min(year), max(year), length = 30)
     newdata <- expand.grid(loc1, loc2, yr)
     names(newdata) <- c("location1", "location2", "year")
     model <- predict(model, newdata)
     model <- list(x = cbind(loc1, loc2), z = yr,
                   y = array(model, dim = rep(30, 3)))
     mapxy <- map('world', plot = FALSE,
                  xlim = range(longitude), ylim = range(latitude))
     rp.plot4d(location, year, logSO2, model,
                 col.palette = rev(heat.colors(20)),
                 foreground.plot = function() map(mapxy, add = TRUE))
  }
  else
    rp.plot4d(location, year, logSO2, col.palette = rev(heat.colors(20)))
})

# Dissolved Oxygen in the River Clyde

with(Clyde, {

  rp.plot4d(cbind(Doy, DO), Station, location.plot = FALSE)
  rp.plot4d(cbind(Station, DO), Doy, location.plot = FALSE)
  rp.plot4d(cbind(Station, Doy), Year, DO)

  # Highlight the data before and after a sewage treatment plant update in 1985
  ind     <- Year >= 80 & Year <= 89 & !(Year == 85)
  year    <- Year[ind] + Doy[ind] / 365
  station <- Station[ind]
  doy     <- Doy[ind]
  do      <- DO[ind]
  group   <- factor(c("after 1985", "before 1985")[1 + 
                  as.numeric(year < 85)])
  rp.plot4d(cbind(doy, do), station, group,
       col.palette = c("red", "green"), location.plot = FALSE)
})

}}

\keyword{iplot}
\keyword{dynamic}
