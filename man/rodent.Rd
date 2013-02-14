\name{rodent}

\alias{rodent}

\title{The mass and speed of quadrupedal rodents}

\description{
In an investigation of the relationship between mass (kg) and speed (km/hr) 
in mammals,  Garland (1983) collected information from published articles
on these two variables for a large number of different species.  These
measurements are given below for a variety of four-footed rodents.
(The common names of the species are taken from Corbet & Hill (1986).)
Notice that the measurements are not all recorded to the same
level of accuracy since the results have been collated from the work
of a number of different scientists.

The data are used in \code{\link{rp.cartoons}}.
}

\references{
Bowman, A.W. & Robinson, D.R. (1990).  Introduction to Regression and
Analysis of Variance: a computer illustrated text.  Bristol: Adam Hilger.

Garland, T. (1983).  The relation between maximal running speed and body
mass in terrestrial animals.  Journal of the Zoological Society of
London, 199, 155-170.

Corbet, G.B. & Hill, J.E. (1986).  A World List of Mammalian Species.
2nd edition. London: British Museum, Natural History.

rpanel: Simple interactive controls for R functions using the tcltk package.
      Journal of Statistical Software, 17, issue 9.

}

\examples{
\dontrun{
   with(rodent, {
     rp.regression(log(Mass), log(Speed))
   })
}}

\keyword{iplot}
\keyword{dynamic}

