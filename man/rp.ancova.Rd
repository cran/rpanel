\name{rp.ancova}

\alias{rp.ancova}

\title{Interactive analysis of covariance}

\description{
This function plots a response variable against a covariate, with different groups of data identified by colour and symbol.  It also creates a panel which controls the model which is fitted to the data and displayed on the plot.

The function is superseded by \code{rp.lm} which is strongly recommended as it has a more flexible interface and improved plotting, using \code{ggplot}.
}

\usage{
  rp.ancova(x, y, group, panel = TRUE, panel.plot = TRUE, model = NA,
            xlab, ylab, glab, hscale = NA, vscale = hscale, style = 'ggplot')
}

\arguments{
\item{x}{a vector of covariate values.}
\item{y}{a vector of response values.}
\item{group}{a vector of group indicators.  If this is not already a factor it will be converted into one.}
\item{panel}{a logical variable which determines whether a panel is created to allow interactive control of the fitted models.}
\item{panel.plot}{a logical parameter which determines whether the plot is placed inside the panel (TRUE) or the standard graphics window (FALSE)}
\item{model}{a character variable which determines the model to be fitted. Valid values are \code{'None', 'Single line', 'Parallel lines', 'Different lines'}.}
\item{xlab}{a character variable used for the covariate axis label.}
\item{ylab}{a character variable used for the response axis label.}
\item{glab}{a character variable used for the group variable label.}
\item{hscale, vscale}{scaling parameters for the size of the plot when \code{panel.plot} is set to \code{TRUE}.  The default values are 1.}
\item{style}{a character variable used to determine whether the style of the plot. When this is set to \code{'ggplot'} the \code{rp.lm} function is called. For any other setting, a simpler display using standard graphics is created.}
}

\details{
The \code{rp.lm} function provides an alternative means of creating the display, based on model formulae.

The \code{'ggplot'} style is strongly recommended as in this case the \code{rp.lm} function is called. The older style of display has very limited functionality.

Static plots, for printing or other purposes can be created by setting the panel argument to FALSE and specifying the model of interest.
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
   with(gullweight, {
     rp.ancova(hab, weight, month)
   })
}}

\keyword{iplot}
\keyword{dynamic}
