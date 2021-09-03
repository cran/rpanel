\name{rp.rmplot}

\alias{rp.rmplot}

\title{Interactive plotting of repeated measurement data}

\description{
This function creates a panel which controls the display of data which have a repeated measurement structure across time.  Groups, means and standard errors can be displayed.  Individual profiles can also be inspected. 
}

\usage{
rp.rmplot(y, id = NA, timept = NA, fac = NA, type = "all", 
                          xlab = NA, ylab = NA, xlabels = NA, add = FALSE,
                          lwd = 1, col = NA, lty = NA, panel = TRUE, 
                          panel.plot = TRUE, hscale = NA, vscale = hscale, ...)
}

\arguments{
\item{y}{a vector, matrix or dataframe of response data.  If y is a matrix or dataframe, the rows should correspond to cases and the columns to the repeated measurements.}
\item{id}{when y is a vector, id should contain the identifiers for the individual profiles.}
\item{timept}{when y is a vector, \code{timept} should contain the time value associated with each repeated measurement.  When y is a matrix or dataframe \code{timept} may identify the values associated with the repeated measurements (columns); in this case the default value is the sequence from 1 to the number of repeated measurements.}
\item{fac}{an optional factor to split the data into groups.}
\item{type}{when the function is not running in interactive panel mode, this character variable determines the type of plot produced.  It can be set to "all", "mean", "mean+bar" or "band".  The last option is applicable only when there are two groups of data.}
\item{xlab}{the x-axis label.}
\item{ylab}{the y-axis label.}
\item{xlabels}{labels for the repeated measurements, to be printed on the x-axis.}
\item{add}{a logical variable which determines whether the repeated measurements graph is added to an existing plot.  This is only appropriate when \code{panel = FALSE}.}
\item{lwd}{the width of the lines drawn for each repeated measurements profile.}
\item{col}{a vector of colours associated with each of the factor levels in \code{fac}.}
\item{lty}{a vector of linetypes associated with each of the factor levels in \code{fac}.}
\item{panel}{a logical variable controlling whether an interactive panel is created.}
\item{panel.plot}{a logical parameter which determines whether the plot is placed inside the panel (TRUE) or the standard graphics window (FALSE).  If the plot is to be placed inside the panel then the \code{tkrplot} library is required.}
\item{hscale, vscale}{scaling parameters for the size of the plot when \code{panel.plot} is set to \code{TRUE}.  The default values are 1 on Unix platforms and 1.4 on Windows platforms.}
\item{...}{further arguments which will be passed to the \code{plot} call
             in the construction of the graph.}
}

\details{
This function is designed principally for repeated measurements over time, with common time points for each profile.  A set of radiobuttons allows all the individual profiles to be plotted, or summaries in the form of means and two standard errors.  A checkbox allows the data to be split into groups identified by the variable \code{fac}.  When there are only two groups, a band can be displayed to indicate time points at which the distance between the observed means is more than two standard errors of the differences between the means.
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
   LH    <- luthor[,2:16]
   gp    <- factor(luthor[,1])
   times <- c(1:5,(5+(1:10)/2))
   rp.rmplot(log(LH), fac = gp, timept = times)
}}

\keyword{iplot}
\keyword{dynamic}
