\name{rp.normal}

\alias{rp.normal}

\title{Interactive fitting of a normal distribution}

\description{
This function plots a histogram of a sample of data and creates a panel which controls the mean and standard deviation of the normal distribution which is fitted to the data and displayed on the plot.
}

\usage{
  rp.normal(y, ylab = deparse(substitute(y)), panel.plot = TRUE, hscale = NA, vscale = hscale)
  }

\arguments{
  \item{y}{a vector of data.}
  \item{ylab}{a character variable used for the histogram axis label.}
  \item{panel.plot}{a logical parameter which determines whether the
                plot is placed inside the panel (TRUE) or the
                standard graphics window (FALSE).  If the plot is
                to be placed inside the panel then the tkrplot
                library is required.}
\item{hscale, vscale}{scaling parameters for the size of the plot when \code{panel.plot} is set to \code{TRUE}.  The default values are 1 on Unix platforms and 1.4 on Windows platforms.}
  }

\details{
The interactive controls allow a normal density curve to be added to the histogram, with doublebuttons used to control the values of the normal mean and standard deviation.  The fitted normal density based on the sample mean and standard deviation can also be displayed.
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
  y <- rnorm(50, mean = 10, sd = 0.5)
  rp.normal(y)
  }}

\keyword{iplot}
\keyword{dynamic}
