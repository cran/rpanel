\name{rp.coefficients}

\alias{rp.coefficients}

\title{Graphical display of the effects of the coefficients in a linear or generalised linear model}

\description{
The effects of the coefficients of a linear or generalised linear model are displayed in graphical form. The effects are made comparable by viewing the change in the mean value of the response as each predictor variable changes across its range.
}

\usage{
rp.coefficients(model, style = 'density', ci = TRUE, point.estimate = !ci,
                 se.scale = FALSE, marks, cols, ngrid)
}

\arguments{
\item{model}{a linear or generalised linear model produced by the \code{lm} or \code{glm} functions.}
\item{style}{a character variables controlling whether effect uncertainties are
         displayed as densities (\code{'density'}) or shaded density strips
         (\code{'shading.'})}
\item{ci}{a logical value controlling whether uncertainty is placed around the
         estimated effect in confidence interval style or around 0 in the format
         of a hypothesis test.}
\item{point.estimate}{a logical value controlling whether the estimated effect
         is displayed as a point estimate.}
\item{se.scale}{a logical value controlling whether a standard error scale is
         superimposed on the uncertainty display.}
\item{marks}{a vector of reference values to be highlighted on the standard
         error scale. If this is not supplied then the upper and lower 2.5\%
         quantiles of a t-distribution will be used in the case of linear models
         and +/-1.96 for generalised linear models.}
\item{cols}{a named character vector of colours to be used in the display. If this is
not specified then the colours defined by the \pkg{rpanel} package function \code{rp.colours} are used. The colours can be altered by setting the \code{cols} argument to a vector of colours, assigned according to the names of the vector.}
\item{ngrid}{the number of grid points used in the construction of the
         uncertainty display. If not supplied, this will be set to 200.}
}

\details{
The covariate range information is added to the labels by the function.

When coefficient labels are lengthy, or there are many coefficients in the model, some overlapping may occur. Options to deal with this are shown in the examples below.
}

\value{
A \code{ggplot} object is returned. This allows further annotation of the display.
}

\examples{
\dontrun{
   model <- lm(Giving ~ Employ + Elect + Attend + 1, data = CofE)
   rp.coefficients(model)
   
   # Change colours
   
   # Options for labelling
   plt <- rp.coefficients(model)
   plt + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))
   plt + ggplot2::scale_x_discrete(labels = scales::label_wrap(5))
   plt + ggplot2::coord_flip()
   }
}
