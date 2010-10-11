\name{rp.logistic}

\alias{rp.logistic}

\title{Interactive display of logistic regression with a single covariate}

\description{
  The function \code{rp.logistic} plots a binary or binomial response variable 
  against a single covariates and creates a panel which controls the position of 
  a logistic curve and allows a logistic regression to be fitted to the data and 
  displayed on the plot.
  }

\usage{
   rp.logistic(x, y, xlab = NA, ylab = NA, panel.plot = TRUE, line.showing = TRUE,
               hscale = NA, vscale = hscale)
  }

\arguments{
\item{x}{a vector of covariate values.}
\item{y}{a vector of response values with two levels, or a two-column matrix whose first column is the number of `successes' and the second column is the number of `failures' at each covariate value.}
\item{xlab}{a character variable used for the covariate axis label.}
\item{ylab}{a character variable used for the response axis label.}
\item{panel.plot}{a logical variable which determines whether the plot is placed inside the control panel.}
\item{line.showing}{a logical value determining whether a regression line is shown on the plot when the function starts.}
\item{hscale, vscale}{horizontal and vertical scaling factors for the size of the plots.  It can be useful to adjust these for projection on a screen, for example.  The default values are 1 on Unix platforms and 1.4 on Windows platforms.}
}

\details{The control panel allows a logistic regression line to be drawn on the plot and the intercept and slope of the linear predictor altered interactively.  The fitted logistic regression can also be displayed.
  
If \code{y} is a vector of responses with two values, these are treated as a factor which is then converted to the (0,1) scale by \code{as.numeric}.
  
The values of the response variable can be `jittered'.
}

\value{Nothing is returned.}

\references{
   rpanel: Simple interactive controls for R functions using the tcltk package.
      Journal of Statistical Software, 17, issue 9.
   }

\seealso{\code{\link{rp.regression}}}

\examples{
if (interactive()) {
  data(river)
  attach(river)
  rp.logistic(Temperature, Low)
  }}

\keyword{iplot}
\keyword{dynamic}
