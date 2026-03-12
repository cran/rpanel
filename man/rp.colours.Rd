\name{rp.colours}

\alias{rp.colours}
\alias{rp.colors}

\title{Specify the colours to be used in some \pkg{rpanel} functions.}

\description{
Returns a named vector specifying the colours to be used in some \pkg{rpanel} functions. Where colours for particular purposes are not specified default values are returned.
}

\usage{
rp.colours(cols)
}

\arguments{
\item{cols}{a named vector of colours.}

}

\details{
Some \pkg{rpanel} functions have a \code{cols} argument which can be used to specify the colours used when creating some aspects of the resulting visual display. The \code{rp.colours} function is used internally to set these colours, altered where appropriate by the elements of \code{cols}. Users need not employ this function, except to view the default colours.
}

\value{
A named vector of the colours.
}

\examples{
\dontrun{
# View the defauklt colours.
rp.colours()

# Alter particular colours
model <- lm(Giving ~ Attend + Employ, data = CofE)
rp.coefficients(model)
rp.coefficients(model, cols = c('estimate' = 'darkgreen'))
}}

\keyword{iplot}
\keyword{dynamic}
