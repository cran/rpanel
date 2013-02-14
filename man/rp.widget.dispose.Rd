\name{rp.widget.dispose}

\alias{rp.widget.dispose}

\title{Removes a widget}

\description{
This will dispose/remove a widget from a panel.
}

\usage{
rp.widget.dispose(panel, name)
}

\arguments{
  \item{panel}{the panel on which the text should disappear.}
  \item{name}{the name assigned to the widget on creation.}
}

\details{
This will dispose of a widget and its memory usage.
}

\references{
   rpanel: Simple interactive controls for R functions using the tcltk package.
      Journal of Statistical Software, 17, issue 9.
   }

\seealso{\code{\link{rp.control}}}

\examples{
\dontrun{
 p1 <- rp.control()
 rp.button(p1, I, "press me", name="b1")
 rp.widget.dispose(p1, b1 )
}}

\keyword{iplot}
\keyword{dynamic}
