\name{rp.control}

\alias{rp.control}
\alias{rp.control.dispose}

\title{Create or dispose of an rpanel}

\description{
The function \code{rp.control} creates a panel window into which rpanel widgets can be placed.  It can also set up variables within the rpanel object.  The function \code{rp.control.dispose} disposes of an rpanel.}

\usage{
rp.control(title = "", size = c(100, 100), panelname, background, ...)
rp.control.dispose(panel)
}

\arguments{
  \item{title}{the title of the panel displayed in the banner.}
  \item{size}{a two-element numeric vector specifying width and height of the panel in pixels.  If this argument is omitted the size of the panel will adapt to the subsequent addition of widgets.}
  \item{panelname}{the name of the panel. It is usually not necessary to set this as it will be given a name automatically.}
  \item{background}{the background colour of the control e.g. "white". (New parameter with version 2.0.)}
  \item{\dots}{additional arguments which are treated as variable initialisations and are stored within the returned rpanel object. For example inserting \code{x=3} creates a variable \code{x} in the rpanel object with the value 3.  Note that the names of these additional arguments should not conflict with those of the main arguments of \code{rp.control}.}
  \item{panel}{the panel to be disposed of.  This represents the object and its parameters}
}

\note{
Previous arguments \code{realname} and \code{aschar} have been discontinued in version 1.1.
}

\details{
Objects passed into \code{rp.control} are then available to be used by action functions.
}

\value{
  The list object which defines the panel.
}

\references{
   rpanel: Simple interactive controls for R functions using the tcltk package.
      Journal of Statistical Software, 17, issue 9.
   }

\seealso{
\code{\link{rp.button}},
\code{\link{rp.checkbox}},
\code{\link{rp.combo}},
\code{\link{rp.doublebutton}},
\code{\link{rp.grid}},
\code{\link{rp.image}},
\code{\link{rp.listbox}},
\code{\link{rp.menu}},
\code{\link{rp.radiogroup}},
\code{\link{rp.slider}},
\code{\link{rp.text}},
\code{\link{rp.textentry}},
\code{\link{rp.tkrplot}},
\code{\link{rp.widget.dispose}}
}
\examples{
\dontrun{
   hist.or.boxp <- function(panel) {
     if (panel$plot.type == "histogram")
       hist(panel$x)
     else
       boxplot(panel$x)
     panel
   }
   panel <- rp.control(x=rnorm(50), panelname="panel")
   rp.radiogroup(panel, plot.type, c("histogram", "boxplot"),
                 title="Plot type", action = hist.or.boxp)  

   # Try also
   # panel <- rp.control()
   # rp.control.dispose(panel)
}}

\keyword{iplot}
\keyword{dynamic}
