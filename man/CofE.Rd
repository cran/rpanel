\name{CofE}

\alias{CofE}

\title{Giving in the Church of England}

\description{
These data record the average annual giving in pounds per church
member in the dioceses of the Church of England in the early 1980's. Three potentially relevant covariates are also recorded for each diocese, namely the percentage of the population who are employed, the percentage of the population on the electoral roll of the church and
the percentage of the population who usually attend church.
Background details are available in Pickering (1985; Applied Economics 17, 619-32).

The data are used in the \code{\link{rp.regression}} example script. 
}

\references{
   rpanel: Simple interactive controls for R functions using the tcltk package.
      Journal of Statistical Software, 17, issue 9.
   }

\examples{
\dontrun{
  with(CofE, {
    rp.regression(cbind(Employ, Attend), Giving)
  })
}}

\keyword{iplot}
\keyword{dynamic}

