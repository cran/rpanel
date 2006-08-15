\name{rp.control}

\alias{rp.control}

\title{Create an rpanel}

\description{
  This function creates a panel window into which rpanel widgets can be placed.
  It also returns, and can set up variables within, the rpanel object.
}

\usage{
rp.control(title = "", size = NULL, panelname, realname, aschar = TRUE, ...)
}

\arguments{
  \item{title}{the title of the panel displayed in the banner.}
  \item{size}{a two-element numeric vector specifying width and height of the panel in pixels. If this argument is omitted the size of the panel will adapt to the subsequent addition of widgets.}
  \item{panelname}{the name of the panel at .GlobalEnv. If this is not assigned, then realname is taken.}
  \item{realname}{retained for backward compatibility. If this is not assigned, the realname is assigned automatically using rp.panelname.}
  \item{aschar}{if TRUE (default) the panel's name as a string is returned. If FALSE the panel list itself is returned.}
  \item{\dots}{additional arguments which are treated as variable initialisations and are stored within the returned rpanel object. For example inserting \code{x=3} creates a variable \code{x} in the rpanel object with the value 3.}
}

\value{
  Dependent on parameter ischar. If ischar is TRUE this is the string name of the panel if
  FALSE this is the list object which defines the panel.
}

\references{rpanel: Simple interactive controls for R functions using 
the tcltk package 
(http://www.stats.gla.ac.uk/~adrian/research-reports/rpanel.ps)}

\seealso{\code{\link{rp.button}},\code{\link{rp.slider}},\code{\link{rp.doublebutton}},\code{\link{rp.textentry}},\code{\link{rp.checkbox}},\code{\link{rp.radiogroup}}}

\examples{
hist.or.boxp <- function(panel) {
  if (panel$plot.type == "histogram")
    hist(panel$x)
  else
    boxplot(panel$x)
  panel
}
panel <- rp.control(x=rnorm(50))
rp.radiogroup(panel, plot.type, c("histogram", "boxplot"), title="Plot type", action = hist.or.boxp)  

# Try also
# panel <- rp.control(ischar = TRUE) # returns a string ".rpanel1" in panel
# panel <- rp.control(ischar = FALSE) # returns the panel list object itself
}

\keyword{iplot}
\keyword{dynamic}
