\name{luthor}

\alias{luthor}

\title{Data for the rp.rmplot example script}

\description{
These data, reported by Raz(1989, Biometrics 54, 851-71) refer to
an experiment which compared the concentrations of
leutinizing hormone (LH) in 16 suckled and 16 nonsuckled cows.
Measurements were made daily from day 1 through to day 4 postpartum,
and twice daily from day 5 through to day 10 postpartum.  The cows
were ovariectomised on day 5 postpartum.

The first column of the dataset defines the group (1 - non-suckled,
2 - suckled) while the remaining columns give the LH values at
the successive recording times.

The data are used in the \code{\link{rp.rmplot}} example script. 
}

\references{
   rpanel: Simple interactive controls for R functions using the tcltk package.
      Journal of Statistical Software, 17, issue 9.
   }

\examples{
if (interactive()) {
   data(luthor)
   LH <- luthor[,2:16]
   gp     <- factor(luthor[,1])
   times  <- c(1:5,(5+(1:10)/2))
   rp.rmplot(log(LH), fac = gp, timept = times)
   }}

\keyword{iplot}
\keyword{dynamic}

