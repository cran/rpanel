\name{rp.var.put}

\alias{rp.var.put}

\title{Places an object in the rpanel environment, usually within a panel.}

\description{
The management of objects within the rpanel environment is usually handled `behind the scenes' but it can occasionally be useful to place an object there explicitly.
}

\usage{
rp.var.put(panelname, name, val, labels = NULL)
}

\arguments{
  \item{panelname}{the panelname of the relevant panel.  This is usually identified as \code{panel$panelname}.  If this argument is set to \code{NULL} then the object is not placed inside a panel.}
  \item{name}{the name of the variable.}
  \item{val}{the contents of the variable as a numeric or character vector.}
  \item{labels}{labels for \code{var}.}
}

\references{
   rpanel: Simple interactive controls for R functions using the tcltk package.
      Journal of Statistical Software, 17, issue 9.
   }

\seealso{\code{\link{rp.var.get}}}

\keyword{iplot}
\keyword{dynamic}
