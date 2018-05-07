\name{rp.geosim}

\alias{rp.geosim}

\title{Interactive visualisation of spatially correlated random fields}

\description{
This function allows Gaussian random fields to be simulated and visualised, using graphical controls for a variety of parameter settings.}

\usage{
rp.geosim(max.Range = 0.5, max.pSill = 1, max.Nugget = 1, max.Kappa = 10, 
                      max.aniso.ratio = 5,
                      min.ngrid = 10, max.ngrid = 25, hscale = NA, vscale = hscale,
                      col.palette = terrain.colors(40))
}

\arguments{
  \item{max.Range, max.pSill, max.Nugget}{the maximum values of the range, sill and nugget
                    parameters.  These define the end-points of the corresponding slider scales.}
  \item{max.Kappa}{The maximum value of the kappa parameter in the Matern family of 
                    spatial covariance functions.}
  \item{max.aniso.ratio}{The maximum value of the anisotropy ratio parameter, which
                    controls the degree of anisotropy in the simulated field.}
  \item{min.ngrid, max.ngrid}{the minimum and maximum values of the grid size for sampling points.}
  \item{hscale, vscale}{horizontal and vertical scaling factors for the size of the plots.
                    It can be useful to adjust these for different screen resolutions or for 
                    projection in a lecture setting.  The default values are 1.2 on Unix platforms
                    and 1.4 on Windows platforms.}
  \item{col.palette}{the colour palette used to display the random fields.}
}

\details{
The aim of the tool is to allow the generation of repeated simulated fields without the distraction of re-executing code explicity.  This can help to gain an intuitive understanding of the nature of spatial data.  In particular, interactive control of parameters can help greatly in understanding the meaning and effects of parameter values.  Nugget effects can be added and sampled points displayed.  Two-diemsional contour plots are produced.  Three-dimensional plots are also produced if the \pkg{rgl} package is available.

The use of the function is discussed in the paper paper by Bowman et al. (2008) referenced below.

The \pkg{geoR} and \pkg{RandomFields} packages are used to generate the data.

Note that the Matern covariance function is parameterised in the form described by Handcock & Wallis (1994) which separates the effects of the shape and range parameters.
}

\value{
  Nothing is returned.
}

\references{
   Adler, D. (2005).
   rgl: 3D Visualization Device System (OpenGL).
   http://CRAN.R-project.org.

   Bowman, A.W., Crawford, E., Alexander, G. Gibson and Bowman, R.W. (2007).   
   rpanel: Simple interactive controls for R functions using the tcltk package.
   Journal of Statistical Software, 17, issue 9.
      
   Bowman, A.W., Gibson, I., Scott, E.M. and Crawford, E. (2008).
   Interactive Teaching Tools for Spatial Sampling.
   Journal of Statistical Software, 36, 13, 1--17.

   Diggle, P.J. and Ribiero, P.J. (2008).
   Model-based Geostatistics.
   Springer, New York.
   
   Handcock, M.S. and Wallis, J.R. (1994).
   An approach to statistical spatial-temporal modeling of meterological fields.
   Journal of the American Statistical Association, 89, 368-378.
   }

\seealso{\code{\link{rp.firth}}, \code{\link{rp.mururoa}}}

\examples{
\dontrun{
   rp.geosim()
}}

\keyword{iplot}
\keyword{dynamic}
