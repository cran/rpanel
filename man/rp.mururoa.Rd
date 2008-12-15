\name{rp.mururoa}

\alias{rp.mururoa}

\title{Sampling in Mururoa Atoll}

\description{
This function is based on a real sampling study on the effects of nuclear experiments conducted between 1966 and 1996 in the South Pacific, at the atolls of Mururoa and Fangataufa, (Report by International Advisory Committee, IAEA, 1998).  As part of the assessment of subsequent radiological conditions, both terrestrial and aquatic samples were collected and assayed for activities due to strontium-90, caesium-137, plutonium and tritium.   The sampling scenario in the function is based on water sampling by boat in the Mururoa atoll.  A graphical control panel allows users to select sampling points.  Once the user has drawn a sample, some simple predictions over the whole atoll can be produced.
}

\usage{
rp.mururoa(hscale = NA, col.palette = rev(heat.colors(40)), col.se = "blue", file = NA,
    parameters = NA, sleep = 0.5)
}

\arguments{
  \item{hscale}{a scaling parameter which expands (>1) or contracts (<1) the size of the plot
                within the panel.  This can be useful for projection onto a screen, for example.
                The vertical scale is set to the same value as the horizontal scale, to ensure
                that the plot is square.  The default values are 1 on Unix platforms and 1.4 
                on Windows platforms.}
  \item{col.palette}{the colour palette used to display the predicted and true spatial surfaces.}
  \item{col.se}{the colour used to draw the standard error contours on the predicted surface.}
  \item{file}{the name of a file to which the sampled data will be written.}
  \item{parameters}{a list which can be used to change the parameters which control the simulated
              measurement data.}
  \item{sleep}{the duration in seconds of a pause while the necessary internal information is loaded
                into the panel.  See Details.}
}

\details{
The panel controls allow the user to experiment with random and systematic sampling, with further control of the alignment and patterns of points in the systematic case.  The number of points can also be selected.  When a sample is taken, simulated data are generated.  Some further controls allow predicted surfaces and standard errors to be displayed, using different types of trend functions.  The \code{geoR} package is used to construct these predictions.  The true simulated surface can also be displayed, to indicate the success of the predictions.

Once the data have been sampled, a data file may be saved for further analysis external to the \code{rp.mururoa} function, using the \code{file} argument.  A convenient way of saving to the current working directory, for example to a file named \code{mururoa.dmp}, is to set the \code{file} argument to \code{file.path(getwd(), "mururoa.dmp")}.  The \code{load} function can then be applied to the saved file to create an object called \code{mururoa.data}, which is a three-column matrix with the x and y locations in columns 1 and 2 and the observed values in column 3.

On some machines the \code{R} and Tcl/Tk code can become out of step because of the time taken to initialise panel with the large amount of internal information required for plotting.  The \code{sleep} argument allows a pause for this to be completed before further \code{rpanel} instructions are executed.  If the \code{rpanel} window displays with very small size, try increasing the value of \code{sleep}.
}

\value{
None.
}

\references{
   rpanel: Simple interactive controls for R functions using the tcltk package.
      Journal of Statistical Software, 17, issue 9.
   }

\seealso{\code{\link{rp.firth}}}

\examples{
if (interactive()) {
  rp.mururoa()
  }}

\keyword{iplot}
\keyword{dynamic}
