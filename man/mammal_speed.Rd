\name{mammal_speed}
\docType{data}
\alias{mammal_speed}

\title{Data on maximal running speed and body mass in terrestrial mammals}

\description{
In an investigation of the relationship between mass (kg) and maximal running speed (km/hr) in terrestrial mammals,  Garland (1983) collected information from published articles on these two variables for a large number of different species.  The measurements are not all recorded to the same level of accuracy since the results have been collated from the work of a number of different scientists.

The \code{mammal_speed} dataframe consists of four columns of data:
\itemize{
\item \code{Name}: the name of the mammal;
\item \code{Family}: the family of the mammal;
\item \code{Mass}: the characteristic mass of the mammal in kg;
\item \code{Speed}: the characteristic maximal running speed of the mammal in km/hr.
}

The \code{rodent} dataset in the \pkg{rpanel} package is a subset of these data.
}

\references{
Garland, T. (1983).  The relation between maximal running speed and body
mass in terrestrial animals.  Journal of the Zoological Society of
London, 199, 155-170.
}

\examples{
\dontrun{
if (require(ggplot2, quietly = TRUE)) {
  ggplot(mammal_speed, aes(log(Mass), log(Speed), col = Family)) + geom_point()
}
}}

\keyword{iplot}
\keyword{dynamic}

