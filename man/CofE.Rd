\name{CofE}

\alias{CofE}

\title{Data for the rp.regression2 example script}

\description{
These data record the average annual giving in pounds per church
member in the dioceses of the Church of England in the early 1980's. Three potentially relevant covariates are also recorded for each diocese, namely the percentage of the population who are employed, the percentage of the population on the electoral roll of the church and
the percentage of the population who usually attend church.
Background details are available in Pickering (1985; Applied Economics 17, 619-32).

The data are used in the rp.regression2 example script. 
}

\references{rpanel: Simple interactive controls for R functions using
the tcltk library
(http://www.stats.gla.ac.uk/~adrian/rpanel/)}

\examples{
  data(CofE)
  attach(CofE)
  rp.regression2(Giving, Employ, Attend)
}

\keyword{iplot}
\keyword{dynamic}

