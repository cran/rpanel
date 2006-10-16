\name{rp.plot3d}

\alias{rp.plot3d}

\title{Interactive display of a plot of three variables}

\description{
  This function produces a scatterplot of three variables, using the rgl package 
  for three-dimensional display.
}

\usage{
  rp.plot3d(x, y, z, xlab  = NA, ylab = NA, zlab = NA, 
                   axes = TRUE, type = "p", size = 3, col = "red",
                   xlim = NA, ylim = NA, zlim = NA, ...)
}

\arguments{
   \item{x,y,z}{vectors of observed values.}
   \item{xlab}{a character variable used for the first axis label.}
   \item{ylab}{a character variable used for the second axis label.}
   \item{zlab}{a character variable used for the third axis label.}
   \item{axes}{a logical variable determining whether the axes are shown.}
   \item{type}{a character variable controlling the type of plotting. If the value is set to "n", the points are not plotted.}
   \item{size}{the size of the plotted points.}
   \item{col}{the colour of the plotted points.}
   \item{xlim}{the plotting range for the first variable.}
   \item{ylim}{the plotting range for the second variable.}
   \item{zlim}{the plotting range for the third variable.}
   \item{...}{other rgl parameters which control the appearance of the plotted points.}
}

\details{
  The plot is produced by appropriate calls to the rgl package.  This allows
  interactive control of the viewing position. Other objects may subsequently be added to the plot by using rgl functions and data which are centred and scaled by the returned values indicated below.
}

\value{
     A scaling function is returned to allow further objects
     to be added to the plot.  The function accepts x, y, z
     vector arguments and returns a list with x, y, z components
     defining the co-ordinates for plotting.  An illustration
     is given in the example below. 
}

\references{rpanel: Simple interactive controls for R functions using
the tcltk package
(http://www.stats.gla.ac.uk/~adrian/rpanel/)}

\seealso{\code{\link{rp.regression2}}}

\examples{
  x <- rnorm(50)
  y <- rnorm(50)
  z <- rnorm(50)
  scaling <- rp.plot3d(x, y, z, xlim = c(-3, 3))
# In addition you may add a line to the plot with these two lines;
#  a <- scaling(c(-3,3), c(0,0), c(0,0))
#  lines3d(a$x, a$y, a$z, col = "green", size = 2) 
}

\keyword{iplot}
\keyword{dynamic}
