\name{rp.do}

\alias{rp.do}

\title{Runs a user-written action function}

\description{
Runs a user-written action function, passing a panel to it as a parameter. This can be used to put the rpanel into its intial state.  For example, it is useful when using radiobuttons as these do not automatically call the action function when the controls are first created.
}

\usage{
rp.do(panel, action, x = NA, y = NA)
}

\arguments{
  \item{panel}{the panel to be passed as a parameter to the function.}
  \item{action}{the function to be executed.}
  \item{x,y}{additional arguments for mouse position on the plot, so that the action function can be called with these additional arguments if they are present.}
}

\value{
  If the argument \code{panel} is the panelname string the same string is returned. If the panel object is used, the altered panel is assigned to both the calling level and panel's environment level.
}

\references{
   rpanel: Simple interactive controls for R functions using the tcltk package.
      Journal of Statistical Software, 17, issue 9.
   }

\seealso{ \code{\link{rp.radiogroup}} }

\examples{
\dontrun{
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
          action = data.plotfn, title = "Plot type", initval="histogram")     
   rp.do(panel, data.plotfn)
}}

\keyword{iplot}
\keyword{dynamic}
