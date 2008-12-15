\name{rp.regression}

\alias{rp.regression}
\alias{rp.regression2}

\title{Interactive display of regression with one or two covariates}

\description{
  The function \code{rp.regression} plots a response variable against one or two covariates and 
  creates a panel which controls the model which is fitted to the data and 
  displayed on the plot.  In the case of two covariates, a three-dimensional 
  display is created.  The function \code{rp.regression2} is retained simply for compatibility
  with earlier releases of the package.
  }

\usage{
  rp.regression(x, y, ylab = NA, x1lab = NA, x2lab = NA, xlab = NA, 
                 panel = TRUE, panel.plot = TRUE, hscale = NA, vscale = hscale,
                 model = "None", line.showing = TRUE, residuals.showing = FALSE, size = 3, col = "red")
  rp.regression2(y, x1, x2, ylab  = NA, x1lab = NA, x2lab = NA, 
                 panel = TRUE, model = "None", residuals.showing = FALSE, size = 3, col = "red")
  }

\arguments{
   \item{x}{a vector or two column matrix of covariate values.}
   \item{y}{a vector of response values.}
   \item{x1, x2}{vectors of covariate values.}
   \item{ylab}{a character variable used for the response axis label.}
   \item{x1lab}{a character variable used for the first covariate axis label.}
   \item{x2lab}{a character variable used for the second covariate axis label.}
   \item{xlab}{a character variable used for the first covariate axis label.  This is provided
               for convenience as a more natural argument name when there is only one covariate.}
   \item{panel}{a logical variable which determines whether a panel is created to allow interactive 
                control of the fitted models.  This is relevant only to the case of two covariates.}
   \item{panel.plot}{a logical variable which determines whether the plot is placed inside the
                control panel.  This is relevant only to the case of one covariate.}
\item{hscale, vscale}{scaling parameters for the size of the plot when there is one covariate and \code{panel.plot} is set to \code{TRUE}.  The default values are 1 on Unix platforms and 1.4 on Windows platforms.}
   \item{model}{a character variable defining the model to be fitted when the function starts. 
               The valid values are "None", the name of the first and second covariates and the 
               combination of these names with an "&".  This is relevant only to the case of two 
               covariates.}
   \item{line.showing}{a logical value determining whether a regression line is shown on the plot
               when the function starts.  This is relevant only to the case of one covariates.}
   \item{residuals.showing}{a logical value determining whether the residuals are shown on the plot
               when the function starts.}
   \item{size}{the size of the plotted points.  This is relevant only to the case of two covariates.}
   \item{col}{the colour of the plotted points.  This is relevant only to the case of two covariates.}
}

\details{
  In the case of one covariate, the control panel allows a line to be drawn on the 
  plot and its intercept and slope altered interactively.  The residuals and the
  least squares fitted line can be displayed.  When the fitted line is displayed,
  the effects of moving individual points can be viewed by clicking and dragging.
  
  In the case of two covariates, the plot is constructed with the aid of the rgl package 
  for three-dimensional display, through the \code{rpanel} function \code{\link{rp.plot3d}}.
  This display can be rotated and linear models involving one, two or none of the 
  covariates can be displayed.  Residuals can also be superimposed.

  In the case of two covariates, static plots, for printing or other purposes can be 
  created by setting the panel argument to FALSE and specifying \code{model} and 
  \code{residuals.showing} as required.
  }

\value{
  Nothing is returned.
  }

\references{
   rpanel: Simple interactive controls for R functions using the tcltk package.
      Journal of Statistical Software, 17, issue 9.
   }

\seealso{\code{\link{rp.plot3d}}}

\examples{
if (interactive()) {
  data(CofE)
  attach(CofE)
  rp.regression(Employ, Giving)
  rp.regression(cbind(Employ, Attend), Giving)
  }}

\keyword{iplot}
\keyword{dynamic}
