\name{rp.regression}

\alias{rp.regression}
\alias{rp.regression2}

\title{Graphical display of regression effects (interactive with one or two covariates)}

\description{
When there are one or two covariates, specified through the \code{x} and \code{y} arguments, the function \code{rp.regression} creates a panel which controls the model which is fitted to the data and displayed on the plot.  In the case of two covariates, a three-dimensional display is created.  If a formula or a fitted linear model is passed through the \code{x} argument, then a graphical display of the regression effects is created, irrespective of the number of covariates.  The function \code{rp.regression2} is retained simply for compatibility with earlier releases of the package.
}

\usage{
  rp.regression(x, y, ylab = NA, x1lab = NA, x2lab = NA, xlab = NA,
                model = "None", line.showing = TRUE,
                residuals.showing = FALSE, size = 3,
                panel = TRUE, panel.plot = TRUE, hscale = NA,
                vscale = hscale, yrange, ci = TRUE,
                point.estimate = !ci, labels, subset, ngrid = 200, col)
  rp.regression2(y, x1, x2, ylab  = NA, x1lab = NA, x2lab = NA, 
                 panel = TRUE, model = "None", residuals.showing = FALSE,
                 size = 3, col = "red", ...)
  }

\arguments{
\item{x}{a vector or two column matrix of covariate values, or a formula, or a fitted linear model.}
\item{y}{a vector of response values.  This is not required if \code{x} is a formula or a fitted linear model.}

The following arguments refer to the case of one or two covariates, specified explicitly:
\item{x1, x2}{vectors of covariate values.}
\item{ylab}{a character variable used for the response axis label.}
\item{x1lab}{a character variable used for the first covariate axis label.}
\item{x2lab}{a character variable used for the second covariate axis label.}
\item{xlab}{a character variable used for the first covariate axis label.  This is provided for convenience as a more natural argument name when there is only one covariate.}
\item{model}{a character variable defining the model to be fitted initially (two covariates only). The valid values are "None", the name of the first and second covariates and the combination of these names combined with " and ".}
\item{line.showing}{a logical value determining whether a regression line is shown on the initial plot (one covariate only).}
\item{residuals.showing}{a logical value determining whether the residuals are shown on the initial plot.}
\item{size}{the size of the plotted points (two covariates only).}

The following arguments refer to the case where the \pkg{tcltk} package is used:
\item{panel}{a logical variable which determines whether a panel is created to allow interactive control of the fitted models (two covariates only).}
\item{panel.plot}{a logical variable which determines whether the plot is placed inside the control panel (one covariate only).}
\item{hscale, vscale}{scaling parameters for the size of the plot when \code{panel.plot} is set to \code{TRUE}.  The default values are 1.}

The following argument refers to the case where a regression model is specified through a formula or a call to the \code{lm} function:
\item{yrange}{a vector of length 2 giving the range of values for the change in the response when regression effects are plotted in a static display.}
\item{ci}{a logical variable which controls whether the graphical indication of uncertainty is placed around the estimate of each model coefficient (\code{ci = TRUE}) or around 0 (\code{ci = FALSE}).}
\item{point.estimate}{a logical variable which controls whether the point estimates of the regression effects are superimposed as vertical lines.}
\item{labels}{a character vector which gives the labels to be used for each model coefficient (in \code{subset, if this is specified}). The covariate range information will be added by the function. Labels must be provided for all terms, before the \code{subset} information is applied (see next argument). The \code{labels} vector can also be larger than the number of coefficients in \code{model} to allow a consistent set of positions when models of different size are viewed successively. (This is used by the \code{rp.ancova} function.)}
\item{subset}{an integer vector which specifies which model coefficients are included in the display. The vector refers to the coefficients as listed in the output of the \code{summary} function on the fitted model. If this is not specified then all model coefficients are included apart from the intercept, identified by the standard name '(Intercept)'.}
\item{ngrid}{an integer which specifies the number of grid points used in the creation of the uncertainty strips.}
\item{col}{the colour of the plotted points (two covariate case) or the point estimates of the regression effect (general case).}
\item{...}{additional arguments used when the function is called by \code{rp.lm}.}
}

\details{
In the case of one covariate, the control panel allows a line to be drawn on the plot and its intercept and slope altered interactively.  The residuals and the least squares fitted line can be displayed.  When the fitted line is displayed, the effects of moving individual points can be viewed by clicking and dragging.
  
In the case of two covariates, the plot is constructed with the aid of the rgl package for three-dimensional display, through the \code{rpanel} function \code{\link{rp.plot3d}}.  This display can be rotated and linear models involving one, two or none of the covariates can be displayed.  Residuals can also be superimposed.  Static plots, for printing or other purposes can be created by setting the panel argument to FALSE and specifying \code{model} and \code{residuals.showing} as required.

If \code{x} is a formula or a fitted linear model, then a static plot of the regression effects is created.  Each coefficient is scaled by the length of the range of corresponding covariate values, in order to display the regression effects in a manner which allows these to be compared.  Density strips are used to indicate the uncertainty involved.  The uncertainty strips have a common maximum intensity, as this makes it easier to assess the 'extremity' of values of interest for distributions which are very diffuse.
}

\value{
When \code{x} is a formula or a fitted linear model, the plot is returned as a \code{ggplot} object.  This allows details of the plot to be modified through further \code{ggplot} functions.  In other cases, the panel object is returned.
  }

\references{
   rpanel: Simple interactive controls for R functions using the tcltk package.
      Journal of Statistical Software, 17, issue 9.
   }

\seealso{\code{\link{rp.plot3d}}}

\examples{
\dontrun{
  with(CofE, {
    rp.regression(Employ, Giving)
    rp.regression(cbind(Employ, Attend), Giving)
    rp.regression(Giving ~ Employ + Elect + Attend)
    rp.regression(cbind(Employ, Attend), Giving, model = "Employ and Attend", panel = FALSE)
  })
}}

\keyword{iplot}
\keyword{dynamic}
