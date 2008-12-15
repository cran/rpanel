\name{rp.geosim}

\alias{rp.geosim}

\title{Interactive visualisation of spatially correlated random fields}

\description{
Th is function allows Gaussian random fields to be simulated and visualised, using graphical controls for a variety of parameter settings.}

\usage{
rp.geosim(max.Range = 0.2, max.pSill = 1, max.Nugget = 1, max.Kappa = 10, 
                      min.ngrid = 10, max.ngrid = 25, hscale = NA, vscale = hscale,
                      col.palette = terrain.colors(40), sleep = 0.5)
}

\arguments{
  \item{max.Range, max.pSill, max.Nugget}{the maximum values of the range, sill and nugget
                    parameters.  These define the end-points of the corresponding slider scales.}
  \item{max.Kappa}{The maximum value of the kappa parameter in the Matern family of 
                    spatial covariance functions.}
  \item{min.ngrid, max.ngrid}{the minimum and maximum values of the grid size for sampling points.}
  \item{hscale, vscale}{horizontal and vertical scaling factors for the size of the plots.
                    It can be useful to adjust these for projection on a screen, for example.
                    The default values are 1 on Unix platforms and 1.4 on Windows platforms.}
  \item{col.palette}{the colour palette used to display the random fields.}
  \item{sleep}{the duration in seconds of a pause while the necessary internal information is loaded
                into the panel.  See Details.}
}

\details{
The aim of the tool is to allow the generation of repeated simulated fields without the distraction of re-executing code explicity.  This can help to gain an intuitive understanding of the nature of spatial data.  In particular, interactive control of parameters can help greatly in understanding the meaning and effects of parameter values.  Nugget effects can be added and sampled points displayed.  Two-diemsional contour plots are produced.  Three-dimensional plots are also produced if teh \code{rgl} package is available.

The use of the function is discussed in the paper referenced below.

On some machines the \code{R} and Tcl/Tk code can become out of step because of the time taken to initialise panel with the large amount of internal information required for plotting.  The \code{sleep} argument allows a pause for this to be completed before further \code{rpanel} instructions are executed.  If the \code{rpanel} window displays with very small size, try increasing the value of \code{sleep}.
}

\value{
  None.
}

\references{
   Adler, D. (2005).
   rgl: 3D Visualization Device System (OpenGL).
   http://CRAN.R-project.org.

   Diggle, P.J. and Ribiero, P.J. (2008).
   Model-based Geostatistics.
   Springer, New York.
   
   Bowman, A.W., Crawford, E., Alexander, G. Gibson and Bowman, R.W. (2007).   
   rpanel: Simple interactive controls for R functions using the tcltk package.
      Journal of Statistical Software, 17, issue 9.
      
   Bowman, A.W., Gibson, I., Scott, E.M. and Crawford, E. (2008).
   Interactive Teaching Tools for Spatial Sampling.
   Technical report, available from the rpanel web page at www.stats.gla.ac.uk/~rpanel.
   }

\seealso{\code{\link{rp.firth}},\code{\link{rp.mururoa}}}

\examples{
if (interactive()) {
   rp.geosim()
   }}

\keyword{iplot}
\keyword{dynamic}
