\name{rp.anova}

\alias{rp.anova}

\title{Interactive analysis of variance}

\description{
This function plots response data, separated by one or two factors. It also creates a panel which controls the models which can be fitted to the data and displayed on the plot.  A comparison model can also be selected and the results of an F-test are displayed graphically.

The function is superseded by \code{rp.lm} which is strongly recommended as it has a more flexible interface and improved plotting, using \code{ggplot}. The \code{rp.anova} function is likely to be dropped in a future release of the \code{rpanel} package.
}

\usage{
rp.anova(y, x, z, model = NA, model0 = NA, ylab = NA, xlab = NA, zlab = NA, title = NULL,
         lines = TRUE, panel = TRUE, panel.plot = TRUE, hscale = 1.3,
         vscale = hscale / 1.3)
}

\arguments{
\item{y}{a vector of response values.}
\item{x}{a factor which splits \code{y} into different groups.}
\item{z}{an optional second factor which splits \code{y} into a second set of groups.}
\item{model, model0}{logical vectors of length 2 or 4, for one or two factors respectively, defining the initial and comparison models to be fitted. For one factor, the two values determine whether each of the terms for the intercept and \code{x} appear.  For two factors, the four values determine whether each of the four terms intercept, \code{x}, \code{z} and \code{x:z} appear.}
\item{ylab}{a character name used for the response variable.}
\item{xlab}{a character name used for the first factor.}
\item{zlab}{a character variable used for the response axis label.}
\item{title}{a character variable supplying a title.  (This is used only in the case where \code{panel} is \code{FALSE}.)}
\item{lines}{a logical variable which determines whether lines are drawn to connect the estimated means for each group.  This can be helpful in highlighting the relative positions of the means across the groups.}
\item{panel}{a logical variable which determines whether a panel is created to allow interactive control of the fitted models.}
\item{panel.plot}{a logical parameter which determines whether the plot is placed inside the panel (TRUE) or the standard graphics window (FALSE).}
\item{hscale, vscale}{scaling parameters for the size of the plot when \code{panel.plot} is
	set to \code{TRUE}.}
}

\details{
The data are displayed as points superimposed on a density strip created by the 
\pkg{ggplot2} package.  Selected models are displayed through the fitted values for each
group.  When a valid comparison model is selected, its fitted values are displayed along
with a shaded regions expressing the contribution of the differences between the two sets
of fitted values to the F-statistic.  The F-test is displayed in graphical form with a density
strip to represent the F-distribution and a point to indicate the observed value of the
F-statistic.
  
Static plots, for printing or other purposes can be created by setting
the panel argument to FALSE and specifying the models of interest.
}

\value{
When the function is called in interactive mode, the \code{tcltk} panel object is returned, otherwise the \code{ggplot} object containing the requested plot is returned.
}

\references{
   rpanel: Simple interactive controls for R functions using the tcltk package.
      Journal of Statistical Software, 17, issue 9.
   }

\examples{
\dontrun{
   with(poisons, {
     rp.anova(1/stime, treatment, poison)
   })
}}

\keyword{iplot}
\keyword{dynamic}
