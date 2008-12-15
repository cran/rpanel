\name{rp.firth}

\alias{rp.firth}

\title{Geostatistical sampling and analysis simulation tool}

\description{
This function gives access to a sampling scenario which is based on the mapping radioactivity and the calculation of a radionuclide inventory within a water body.  (A `firth' is a Scottish term for a long, narrow indentation of the sea coast at the mouth of a river.)  Interest lies in nuclides which, on release into a water body, attach (absorb) to sediment in a manner which depends on the sediment particle size.   Cobalt-60 and caesium-137 are examples of nuclides which exhibit this behaviour.  In this sampling scenario, the map of sediment type is used to define regions of different particle size from which the sediment samples will be collected by grabs from a boat.  The presence of strata therefore has to be considered, as the different types of material on the sea bed may affect the mean values of the measurements taken.

The function displays a map and gives graphical control over a variety of sampling strategies.  Once the user has drawn a sample, some simple predictions over the whole firth can be produced.  The \code{geoR} package is used to construct these predictions.
}

\usage{
rp.firth(hscale = NA, col.palette = rev(heat.colors(40)), col.se = "blue", file = NA,
          parameters = NA, sleep = 0.5)
}

\arguments{
  \item{hscale}{a scaling parameter which expands (>1) or contracts (<1) the size of the plot
                within the panel.  This can be useful for projection onto a screen, for example.
                The vertical scale is set to the same value as the horizontal scale, to ensure
                that the plot is square.  The default values are 1 on Unix platforms and 1.4 on 
                Windows platforms.}
  \item{col.palette}{the colour palette used to display the predicted and true spatial surfaces.}
  \item{col.se}{the colour used to draw the standard error contours on the predicted surface.}
  \item{file}{the name of a file to which the sampled data will be written.}
  \item{parameters}{a list which can be used to change the parameters which control the simulated
              measurement data.}
  \item{sleep}{the duration in seconds of a pause while the necessary internal information is loaded
                into the panel.  See Details.}
}

\details{
The use of the function is discussed in the paper referenced below.

Once the data have been sampled, a data file may be saved for further analysis external to the \code{rp.firth} function, using the \code{file} argument.  A convenient way of saving to the current working directory, for example to a file named \code{firth.dmp}, is to set the \code{file} argument to \code{file.path(getwd(), "firth.dmp")}.  The \code{load} function can then be applied to the saved file to create an object called \code{mururoa.data}, which is a three-column matrix with the x and y locations in columns 1 and 2 and the observed values in column 3.

On some machines the \code{R} and Tcl/Tk code can become out of step because of the time taken to initialise panel with the large amount of internal information required for plotting.  The \code{sleep} argument allows a pause for this to be completed before further \code{rpanel} instructions are executed.  If the \code{rpanel} window displays with very small size, try increasing the value of \code{sleep}.
}

\value{Nothing.}

\references{
   Bowman, A.W., Crawford, E., Alexander, G. Gibson and Bowman, R.W. (2007).   
   rpanel: Simple interactive controls for R functions using the tcltk package.
      Journal of Statistical Software, 17, issue 9.
      
   Bowman, A.W., Gibson, I., Scott, E.M. and Crawford, E. (2008).
   Interactive Teaching Tools for Spatial Sampling.
   Technical report, available from the rpanel web page at www.stats.gla.ac.uk/~rpanel.
   }

\seealso{\code{\link{rp.mururoa}}, \code{\link{rp.mururoa}}}

\examples{
if (interactive()) {
  rp.firth()
  }}

\keyword{iplot}
\keyword{dynamic}
