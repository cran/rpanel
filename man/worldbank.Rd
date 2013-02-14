\name{worldbank}

\alias{co2.emissions}
\alias{gdp}
\alias{life.expectancy}
\alias{population}

\title{Data on CO2 emissions, GDP, life.expectancy and population for the countries of the world between 1960 and 2007}

\description{Loading this file makes the dataframes \code{co2.emissions}, \code{gdp}, \code{life.expectancy} and \code{population} available.  These contain the CO2 emissions, gross domenstic product, life expectancy and population data for each country of the world (rows indexed by rownames) for the years 1960-2007.

These data are provided by the World Bank through the database at
   \code{http://data.worldbank.org/data-catalog/world-development-indicators}
The data are also used use in the Google Public Data Explorer
   \code{http://www.google.com/publicdata/directory}
and by the Gapminder project
   \code{http://www.gapminder.org}
)

The data are used in the rp.bubbleplot example script.
}

\references{
   rpanel: Simple interactive controls for R functions using the tcltk package.
      Journal of Statistical Software, 17, issue 9.
   }

\examples{
\dontrun{
   rp.bubbleplot(log(gdp), log(co2.emissions), 1960:2007, size = population, 
      col = life.expectancy,
      interpolate = TRUE, hscale = 1.5, vscale = 1.5)
}}

\keyword{iplot}
\keyword{dynamic}
