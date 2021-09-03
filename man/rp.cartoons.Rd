\name{rp.cartoons}

\alias{rp.cartoons}

\title{Access to a collection of rpanel illustrations}

\description{This function creates a panel with a menu which launches a variety of rpanel illustrations.  The function provides a template which can be amended by users to create tailored sets of illustrations.}

\usage{
  rp.cartoons(hscale = 1)
}

\arguments{
\item{hscale}{a scaling parameter for the size of the plot which will be passed to all relevant menu items.}
}

\value{Nothing.}

\references{
   rpanel: Simple interactive controls for R functions using the tcltk package.
      Journal of Statistical Software, 17, issue 9.
   }

\examples{
\dontrun{
   rp.cartoons()
}}

\keyword{iplot}
\keyword{dynamic}
