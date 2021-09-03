\name{gullweight}

\alias{gullweight}

\title{The weights of herring gulls captured at different times of year}

\description{These data are part of a large sample collected  by Prof. P. Monaghan of the University of  Glasgow in a study of the weight changes in herring  gulls throughout the year.  Some birds were caught in June (coded as month  1)  and others in December (month 2).  Since weight  is dependent on the size of the bird this information is recorded in  the  form  of the head and bill length, \code{hab} (in  mm),  the distance from the back of the head to the tip of the bill.

The data are used in the \code{\link{rp.ancova}} example script.
}

\references{
   rpanel: Simple interactive controls for R functions using the tcltk package.
      Journal of Statistical Software, 17, issue 9.
   }

\examples{
\dontrun{
   with(gullweight, {
     rp.ancova(hab, weight, month)
   })
   }}

\keyword{iplot}
\keyword{dynamic}

