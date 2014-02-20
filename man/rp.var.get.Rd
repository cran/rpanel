\name{rp.var.get}

\alias{rp.var.get}

\title{Retrieves an object from the rpanel envrionment, usually from a panel.}

\description{
The management of objects within the rpanel environment is usually handled `behind the scenes' but it can occasionally be useful to retrieve an object there explicitly.
}

\usage{
rp.var.get(panelname, name)
}

\arguments{
  \item{panelname}{the panelname of the relevant panel.  This is usually identified as \code{panel$panelname}.  If this argument is set to \code{NULL} then the object is not retrieved from a panel.}
  \item{name}{the name of the variable in character form.}
}

\references{
   rpanel: Simple interactive controls for R functions using the tcltk package.
      Journal of Statistical Software, 17, issue 9.
   }

\seealso{\code{\link{rp.var.get}}}

\keyword{iplot}
\keyword{dynamic}
