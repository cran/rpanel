\name{rp.likelihood}

\alias{rp.likelihood}

\title{Interactive inspection of one- or two-parameter likelihood surfaces}

\description{
This function plots a likelihood surface for a model with one or two parameters.  It also creates a panel which allows the maximum likelihood estimate, a confidence region and other objects of interest to be added to the plot.  For one-parameter models, the \code{tkrplot} package is required.  For two-parameter models the \code{rgl} package is required.
}

\usage{
rp.likelihood(loglik.fn, data, theta.low, theta.high, form = "log-likelihood", 
                  hscale = NA, vscale = hscale)
}

\arguments{
  \item{loglik.fn}{This should be either the name of a function, with arguments \code{theta} 
        and \code{data}, or R code, in text form, which evaluates the log-likelihood function.
        The latter form allows simple \code{R} expressions such as \code{sum(log(dexp(data, theta)))}
        or \code{sum(log(dgamma(data, theta[1], theta[2])))} to be used to
        define the log-likelihood.}
  \item{data}{an object which contains the data.  This will be referred to in
        \code{likelihood contributions}.}
  \item{theta.low}{a vector of length one or two which defines the lower limit(s) of
        the parameter values for initial plotting.}
  \item{theta.high}{a vector of length one or two which defines the upper limit(s) of
        the parameter values for initial plotting.}
  \item{form}{a text variable which determines whether the likelihood or log-likelihood
        function is to be plotted.  This applies only to one-parameter models.  With
        two-parameter models, only the log-likelihood is plotted.}
\item{hscale, vscale}{scaling parameters for the size of the plot when there is one covariate.  The default values are 1 on Unix platforms and 1.4 on Windows platforms.}
  }

\details{
  The interactive controls allow a variety of aspects of the plots to be altered.
  This is intended to allow students and lecturers to explore likelihood surfaces
  in a manner which promotes an intuitive understanding of the concepts involved.
  
  In the case of one parameter, the vertical axes of the (log-)likelihood plot can be
  clicked and grabbed to alter the plotting region interactively.  This can be useful,
  in particular, in identifying the maximum likelihood estimator graphically.
}

\value{
  Nothing is returned.
}

\references{
	rpanel: Statistical cartoons in \R. MSOR Connections, 7, 3-7.

   rpanel: Simple interactive controls for R functions using the tcltk package.
      Journal of Statistical Software, 17, issue 9.
   }

\examples{
\dontrun{
   rp.likelihood("sum(log(dexp(data, theta)))", aircond, 0.005, 0.03)
   rp.likelihood("sum(log(dgamma(data, theta[1], theta[2])))",
        aircond, c(0.3, 0.005), c(3, 0.06))
}}

\keyword{iplot}
\keyword{dynamic}
