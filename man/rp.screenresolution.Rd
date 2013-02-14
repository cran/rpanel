\name{rp.screenresolution}

\alias{rp.screenresolution}

\title{Screen resolution}

\description{
  This returns the current screen resolution as a list with two components; width and height.
}

\usage{
rp.screenresolution()
}

\arguments{
	None
}

\details{
  One use of this function is to identify the size of the screen so that panels can be constructed top match this using pixel co-ordinates.  However, the grid layout system is likely to be a better option in general.  See \code{\link{rp.grid}} for details of this.
}

\references{
   rpanel: Simple interactive controls for R functions using the tcltk package.
      Journal of Statistical Software, 17, issue 9.
   }

\seealso{\code{\link{rp.grid}},\code{\link{rp.pos}}}

\keyword{iplot}
\keyword{dynamic}
