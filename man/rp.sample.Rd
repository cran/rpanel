\name{rp.sample}

\alias{rp.sample}

\title{Interactive exploration of sampling variation}

\description{
Graphical exploration of variation in samples and sample means. The primary use of the function is in interactive mode, using a variety of controls for the display. Normal or binomial distributions can be used.
}

\usage{
rp.sample(n, mu, sigma, distribution  = 'normal', shape = 0,
          panel = TRUE, nbins = 20, nbins.mean = 20,
          display, display.sample, display.mean, nsim = 50,
          show.out.of.range = TRUE, ggplot = TRUE,
          hscale = NA, vscale = hscale, pause = 0.01)
}

\arguments{
\item{n}{the size of the sample. If this is missing, it is set to 25.}
\item{mu}{the mean of the distribution from which samples are taken. If this is missing it is set to 5 for a normal distribution and 0.5 for a binomial distribution.}
\item{sigma}{the standard deviation of the normal distribution. If this is missing it is set to 0.4.}
\item{distribution}{a character value which determines whether a \code{'normal'} or \code{'binomial'} distribution is used.}
\item{shape}{the shape parameter of the skew-normal distribution. When this is set to the default value of 0, samples are generated from a normal distribution. Setting non-zero values for this parameter gives some skewness to the distribution from which the data are sampled.}
\item{panel}{a logical value which determines whether the function runs in interactive mode. See Details.}
\item{nbins}{an integer value which sets the number of bins used in the data histograms.}
\item{nbins.mean}{an integer value which sets the number of bins used in the histogram of the sample means.}
\item{display}{a logical value which determines the form of graphical display used initially or in non-interactive mode. Possible values are \code{'histogram'} (the default), \code{'density'} or \code{'violin'}.}
\item{display.sample}{a logical vector which controls options for graphical display of the data, used initially or in non-interactive mode. See Details.}
\item{display.mean}{a logical vector which controls options for graphical display of the sample means, used initially or in non-interactive mode. See Details.}
\item{nsim}{an integer value which the number of accumulated mean values which are plotted when the function runs in non-interactive mode. See Details.}
\item{show.out.of.range}{a logical value which controls whether observations lying beyond 3 standard deviations (for samples) or 3 standard errors (for sample means) are indicated. The scales of the plots are fixed at 3 standard deviations above and below the mean so that the axes are fixed for all samples.}
\item{ggplot}{a logical value which controls whether \code{ggplot} graphics are used. If this is set to \code{FALSE} or the \pkg{ggplot2} package is not available, functionality reverts to the standard graphics version available in \pkg{rpanel} 1.1-5 which has simpler functionality. The \code{ggplot} version of the function is recommended.}
\item{hscale, vscale}{scaling parameters for the size of the plot when \code{panel.plot} is set to \code{TRUE}.  The default values are 1.}
\item{pause}{a time delay, in seconds, for the insertion of components into the control panel. The speed of some computing systems can create a panel which does not expand in time to contain all its components. The \code{pause} argument adds a short delay to each component to avoid this.}
}

\details{
When \code{display} is set to \code{density} or \code{violin}, density estimates are constructed using a bandwidth which is optimal for a normal distribution. For small samples this provides a stable and conservative estimate which is not unduly influenced by features which may well simply be due sampling variation. As the sample size increases, the estimate will still converge to the true density function.

When the size of the sample is less than 10, a histogram or density estimate is not a very effective display. This also causes issues of scaling the vertical axis. So in this case individual points are displayed instead.

The visual effect of the animation is assisted by holding the axes constant.  This means that there may occasionally be observations outside the displayed horizontal range, or a histogram height which exceeds the displayed vertical range. This is denoted by a + symbol at the top of the relevant histogram bars. This issue can often be tackled by reducing the number of histogram bins.

When \code{display} is set to \code{'density'} or \code{'violin'}, individual points are plotted, with a random vertical position. This is suppressed when the number of points exceeds 5000.

The \code{display.sample} and \code{display.mean} arguments control the details of what is displayed initially and, more usefully, when the function operates in non-interactive mode. Each argument is a logical vector with named values. \code{display.sample} has the default setting \code{c(data = TRUE, population = FALSE, mean = FALSE, 'st.dev. scale' = FALSE)} while the default for \code{display.mean} is \code{c('sample mean' = FALSE, 'accumulate' = FALSE, 'se scale' = FALSE, 't-statistic' = FALSE, 'zoom' = FALSE, 'distribution' = FALSE)}. Any elements of these arguments which are not explicitly identified are set to the default values. In the case of binomial data the \code{'st.dev. scale'} setting is disabled as it is not a helpful addition to the plot.

The principal use of the function is in interactive mode, when \code{panel} is set to \code{TRUE}. If \code{panel} is set to \code{FALSE} then interactive mode is switched off. In this case, if the \code{ggplot2} package is available and the \code{ggplot} argument is set to \code{TRUE}, the function returns plots of a sample of data and of accumulated means as components \code{plotdata} and \code{plotmean} of the returned object. The number of accumulated means is set by the \code{nsim} argument.

If the \code{ggplot2} package is not available, standard graphics are used with simpler display options, along the lines of the function provided in version 1.1-5 of the package.

}

\value{
When the function operates in interactive mode, with \code{panel} set to \code{TRUE}, nothing is returned. When \code{panel} is set to \code{FALSE}, plots of a sample of data and of accumulated means are provided as components \code{sample} and \code{mean} of the returned object.
}

\references{
   rpanel: Simple interactive controls for R functions using the tcltk package.
      Journal of Statistical Software, 17, issue 9.
   }

\examples{
\dontrun{
   rp.sample()
}}

\keyword{iplot}
\keyword{dynamic}
