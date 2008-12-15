\name{rp.do}

\alias{rp.do}

\title{Runs a user written callback function}

\description{
Runs a user written callback function, passing a panel to it as a parameter. This should be used to get the rpanel 
into its intial state. For instance it is useful when using radiobuttons as these do not automatically call the action function when the controls are first created.
}

\usage{
rp.do(panel, action = I)
}

\arguments{
  \item{panel}{the panel to be passed as a parameter to the function. This 
  may be passed as a panelname string or the panel object itself.}
  \item{action}{the function to be executed.}
}

\value{
  If the parameter panel is the panelname string the same string is returned. If the 
  panel object is used the altered panel is assigned to both the calling level and panel's environment 
  level.
}

\references{
   rpanel: Simple interactive controls for R functions using the tcltk package.
      Journal of Statistical Software, 17, issue 9.
   }

\seealso{ \code{\link{rp.radiogroup}} }

\examples{
if (interactive()) {
   data.plotfn <- function(panel) {
     if (panel$plot.type == "histogram") 
       hist(panel$x)
     else 
       if (panel$plot.type == "boxplot")
         boxplot(panel$x)
       else 
         plot(density(panel$x))
     panel
   }
   panel <- rp.control(x = rnorm(50))
   rp.radiogroup(panel, plot.type, 
          c("histogram", "boxplot", "density estimate"), 
          action = data.plotfn, title = "Plot type")     
   rp.do(panel, data.plotfn)
   }}

\keyword{iplot}
\keyword{dynamic}
