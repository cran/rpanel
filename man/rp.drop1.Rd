\name{rp.drop1}

\alias{rp.drop1}

\title{Display in graphical form the results of dropping all single terms from a model.}

\description{
Plot the results, in terms of F-tests, of dropping all individual terms from a linear model. This is essentially a graphical display of the results from the function \code{drop1}. The hierarchy of the model formula is respected so that when interaction terms are present main effect terms are not considered.
}

\usage{
rp.drop1(model, subset.terms, p.reference = c(0.05, 0.01), cols)
}

\arguments{
\item{model}{a linear model produced by the \code{lm} function.}
\item{subset.terms}{a character vector containing the names of the subset of terms to be examined.}
\item{p.reference}{a numeric vector specifying the p-values for which the positions on the F-value scale will be marked.}
\item{cols}{a named vector of colours to be used in the construction of the plot. See \code{help(rp.colours)} for details. The relevant name for \code{rp.drop1} is \code{'reference'}.}
}

\details{
The \code{rp.colours} function is used internally to set these colours, altered where appropriate by the elements of \code{cols}. Users need not employ this function, except to view the default colours.

The \pkg{ggplot2} package is required by this function. If the \pkg{ggforce} is available, this is used to scale the heights of facet rows when there are multiple terms with different degrees of freedom.
}

\value{
A \code{ggplot} object containing the plot.
}

\examples{
\dontrun{
model <- lm(Giving ~ Attend + Employ, data = CofE)
rp.drop1(model)
}}

\keyword{iplot}
\keyword{dynamic}
