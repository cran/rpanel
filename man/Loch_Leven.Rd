\name{Loch_Leven}
\docType{data}
\alias{Loch_Leven}

\title{Data on longevity and father's age, from the Hyde genealogy}

\description{
Loch Leven is situated in lowland Scotland in the Perth and Kinross area. It is the largest shallow lake in Great Britain with an area of 13.3km$^2$, mean depth 3.9m and a maximum depth 25.5m. Regular monitoring is carried out by the *Centre for Ecology & Hydrology* in Edinburgh.

One of the features of interest is the water quality and hence the relationship between *chlorophyll$_a$* (as an indicator of water quality) and *soluble reactive phosphorus* (a nutrient) is very important. The monthly means for chlorophyll$_a$ (`lchla`) and soluble reactive phosphorus (`srp`) are provided from January 1988 to December 2007, along with the `year` and `month` of each measurement.
The \code{Loch_Leven} dataframe consists of four variables:
\itemize{
\item \code{year}: the year of the measurement;
\item \code{month}: the month of the measurement, as an integer (1-12);
\item \code{chla}: the measurement of chlorophyll$_a$;
\item \code{srp}: the measurement of soluble reactive phosphorus.
}
}

\references{
   Ferguson, C.A., Bowman, A.W., Scott, E.M., Carvalho, L. (2009). Multivariate varying-coefficient models for an ecological system. Environmetrics 20.4, 460-476.
}

\examples{
\dontrun{
with(Loch_Leven, plot(year, log(chla)))
}}

\keyword{iplot}
\keyword{dynamic}

