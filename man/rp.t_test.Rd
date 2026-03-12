\name{rp.t_test}

\alias{rp.t_test}

\title{Graphical display for inference in comparing the means of one or two samples}

\description{
The sample(s) of data are plotted along with a graphical display which
indicates the uncertainty in the location of the true means and in their
difference. This is done by plotting a shaded representation of a normal
distribution in 'violin plot' style. This can also be compared with a reference
mean, representing the null hypothesis in a t-test.
}

\usage{
rp.t_test(x, y = NULL, panel = TRUE, mu = NULL, data.display = 'density',
          display = c(distribution = FALSE, detail = FALSE),
          ruler.position = 'none', candidate, print = FALSE,
          cols, xlab, ylab, vlab, hscale = 1, vscale = hscale, ...)
}

\arguments{
\item{x}{a vector of data on a continuous scale or a formula with a response variable and a single predictor variable which is a factor with two levels.}
\item{y}{an optional second vector of data on a continuous scale, to allow two-sample or paired-sample analyses. Alternatively, \code{y} may be a character vector or a factor with two levels, defining the groups. If \code{x} is a formula then \code{y} is ignored.}
\item{panel}{ a logical value which determines whether the function runs in interactive mode.}
\item{mu}{a numerical value representing a reference mean of interest, for
            example a null hypothesis in a formal test. This is the same parameter
            as \code{mu} in \code{t.test} but its default value is \code{NULL}
            so that a reference value will be display only if requested.}
\item{data.display}{a character value which controls the form of the data and
            display. Valid options are \code{'histogram'},
            \code{'density'} or \code{'violin'}.}
\item{display}{a named logical vector which controls the form of uncertainty to
            be displayed. The named elements are \code{'distribution'} for display of the reference
            distribution and \code{'detail'} for the display of further details.}
\item{ruler.position}{a character value which determines whether a standard error scale is
            added to the plot and its location, \code{'candidate'} or \code{'sample mean'}.}
\item{candidate}{a numeric value controlling where the standard error ruler is located when \code{ruler.position} is set to \code{'candidate'}.}
\item{print}{a logical value which controls whether the results are printed on the console.}
\item{cols}{the colours to be used in constructing the display. The default values are set by the \code{rp.colours} function whose help file give the details.}
\item{xlab, ylab, vlab}{character values providing labels for the group(s) to be analysed and for the measurement scale (\code{vlab}).}
\item{hscale, vscale}{scaling parameters for the size of the plot when \code{panel} is set to \code{TRUE}.  The default values are 1.}
\item{...}{further named argument, which will be passed on to the \code{t.test}
            function. Useful arguments include \code{paired} and
            \code{var.equal} which both take logical values.}
}

\details{
The graphics are created through the \pkg{ggplot2} package so this needs to be installed.

The function aims to follow the general pattern of the \code{t.test} function and the arguments of \code{t.test} described in its help file can also be passed to \code{rp.t_test}.
}

\value{
A \code{ggplot} object is returned. This allows further alterations and annotations to be made.
}

\references{
   Bowman, A.W. (2019). Graphics for uncertainty (with discussion).
      Journal of the Royal Statistics Society, Series A,
      Statistics & Society, 182, 403-418.
   }

\examples{
\dontrun{
x <- rnorm(50)
rp.t_test(x)
rp.t_test(x, mu = 0)
rp.t_test(x, uncertainty = 'sample mean')
rp.t_test(x, uncertainty = 'reference')

g <- factor(rep(1:2, each = 25))
plt <- rp.t_test(x ~ g)
plt + ggplot2::ggtitle("Two-sample inference")
}}

\keyword{iplot}
\keyword{dynamic}
