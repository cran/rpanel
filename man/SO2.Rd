\name{SO2}

\alias{SO2}

\title{Sulphur dioxide measurements over Europe}

\description{
The data document values of SO2, on a log scale, from monitoring stations across Europe from 1990 to 2001. The data were collected through the 'European monitoring and evaluation programme' (EMEP) and they are available at \url{https://www.emep.int}. The data recorded here have been organised into a convenient form for analysis.

The data file consists of six variables:
   \code{site}: a site code for the monitoring station
   \code{longitude}: longitude of the monitoring station
   \code{latitude}: latitude of the monitoring station
   \code{year}: year of measurement
   \code{month}: month of measurement
   \code{logSO2}: SO2 measurement on a log scale
}

\references{
Spatiotemporal smoothing and sulphur dioxide trends over Europe
A. W. Bowman, M. Giannitrapani and E. M. Scott.
Applied Statistics, 58 (2009), 737--752
}

\examples{
\dontrun{
  Month     <- SO2$month + (SO2$year - 1990) * 12
  Year      <- SO2$year + (SO2$month - 0.5) / 12
  Location  <- cbind(SO2$longitude, SO2$latitude)
  back      <- I
  if (require(maps)) {
  	mapxy <- map('world', plot = FALSE,
              xlim = range(SO2$longitude), ylim = range(SO2$latitude))
      back  <- function() map(mapxy, add = TRUE)
  }
  rp.plot4d(Location, Year, SO2$logSO2, col.palette = rev(heat.colors(12)),
              background.plot = back)
}}

\keyword{iplot}
\keyword{dynamic}

