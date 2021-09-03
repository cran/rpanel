\name{poisons}

\alias{poisons}

\title{Survival times of animals subjected to different poisons and treatment}

\description{
These data record the survival times (in units of 10 hours) of animals in a
3 x 4 factorial experiment.  Four animals were allocated to each combination of
three poisons and four treatments, using a randomisation procedure.

The data are used in the \code{\link{rp.anova}} example script. 

The data were reported in the paper by Box and Cox (1964) referenced below.
}

\references{
   Box, GEP and Cox, DR (1964), 
   An analysis of transformations. Journal of the Royal Statistical Society Series B -
   Statistical Methodology, 26, 211-252.
   
   rpanel: Simple interactive controls for R functions using the tcltk package.
      Journal of Statistical Software, 17, issue 9.
   }

\examples{
\dontrun{
  with(poisons, {
    rp.anova(1/stime, treatment, poison)
  })
}}

\keyword{iplot}
\keyword{dynamic}
