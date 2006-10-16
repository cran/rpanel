\name{rp.pos}

\alias{rp.pos}

\title{Positioning widgets in an R-Panel}

\description{
  This function is really just here so you can read this documentation and view the example.
}

\usage{
rp.pos()
}

\details{
  The various widget-creation functions accept a parameter called "pos" which can be used to specify how the panel is laid out.  It has various modes of operation, outlined below:

  \item{default}{If the parameter is not specified, widgets are arranged in a column with the most recently added widget at the bottom}
  \item{'pack'}{If the parameter is \code{left}, \code{right}, \code{top} or \code{bottom} then the widget is put up against the left/right/top/bottom edge of the window.  If there is already a widget in that position, the new widget is placed "in" from that widget.  This uses Tk's "pack" layout manager}
  \item{'place'}{If the parameter is an array of four numeric values, they are interpreted as coordinates, going \code{c(x, y, width, height)} where all dimensions are in pixels.  \code{x} and \code{y} define the coordinates in from the left-hand side and down from the top respectively.  When using this mode of laying out objects, it usually helps to specify the size of the panel in \code{rp.control}}
  \item{'default\_left\_align'}{This is kept for backward compatibility, and is used internally by some scripts.  It uses the "pack" layout manager, but aligns objects to the left of their bounding boxes.  This makes doublebuttons and checkboxes look better in some situations.}
  
  The example should give you a better idea of how they all behave: you should resize the windows to see how they behave.
}

\value{
  None.
}

\section{Warning}{
    This may not work properly on the command line (as opposed to R-GUI) under Mac OS X due to the \code{rp.block} function.
}

\references{rpanel: Simple interactive controls for R functions using 
the tcltk package 
(http://www.stats.gla.ac.uk/~adrian/rpanel/)}

\seealso{\code{\link{rp.control}}}

\examples{
\dontrun{
# example function for "pos" documentation.  It can't be allowed to run because it calls block and messagebox.
rp.pos()
}
}

\keyword{iplot}
\keyword{dynamic}
