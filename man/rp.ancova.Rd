\name{rp.ancova}

\alias{rp.ancova}

\title{Interactive analysis of covariance}

\description{
  This function plots a response variable against a covariate, with
  different groups of data identified by colour and symbol.  It also
  creates a panel which controls the model which is fitted to the
  data and displayed on the plot.
}

\usage{
  rp.ancova(x, y, group, panel = TRUE, panel.plot = TRUE, model = NA, model0 = NA,
       xlab, ylab, glab, hscale = NA, vscale = hscale, style = "new")
}

\arguments{
\item{x}{a vector of covariate values.}
\item{y}{a vector of response values.}
\item{group}{a vector of group indicators.  If this is not already a factor it will be converted into one.}
\item{panel}{a logical variable which determines whether a panel is created to allow interactive control of the fitted models.}
\item{panel.plot}{a logical parameter which determines whether the plot is placed inside the panel (TRUE) or the standard graphics window (FALSE).  If the plot is to be placed inside the panel then the \code{tkrplot} library is required.}
\item{model, model0}{logical vectors of length 4 defining the initial and comparison models to be fitted.  The four values determine whether each of the four terms intercept, \code{x}, \code{z} and \code{x:z} appear.  This is appropriate only for \code{style = "new"}.}
\item{xlab}{a character variable used for the covariate axis label.}
\item{ylab}{a character variable used for the response axis label.}
\item{glab}{a character variable used for the group variable label.}
\item{hscale, vscale}{scaling parameters for the size of the plot when \code{panel.plot} is set to \code{TRUE}.  The default values are 1 on Unix platforms and 1.4 on Windows platforms.}
\item{style}{a character variable used to determine whether the style of controls in earlier version of the \code{rpanel} package is to be used.  The value \code{"new"} activates the new style and any other value activates the old one.}
}

\details{
  Static plots, for printing or other purposes can be created by setting
  the panel argument to FALSE and specifying the model of interest.
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
