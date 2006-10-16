\name{rpanel-package}
\alias{rpanel-package}
\alias{rpanel}
\docType{package}
\title{
simple interactive controls for R functions using the tcltk package
}
\description{
rpanel provides a set of wrapper functions to build simple 
GUI controls for R functions.  Uses could include changing a parameter 
on a graph (and animating it) with a slider, or a "doublebutton", right 
up to more sophisticated mini-applications.}
\details{
\tabular{ll}{
Package: \tab rpanel\cr
Type: \tab Package\cr
Version: \tab 1.0-1\cr
Date: \tab 2006-06-12\cr
License: \tab GNU\cr
}
This package contains a number of functions (with help and examples) and several 
example scripts.\cr
\cr
Package functions\cr
\cr
rp.panelname: automatic generation of a panel name\cr
rp.control: create an rpanel\cr
rp.slider: add a slider to a panel, to graphically control a numeric variable\cr
rp.textentry: adds a box allows text to be entered\cr
rp.button: adds a button to the panel with a nominated function called on pressing\cr
rp.checkbox: adds a checkbox to the panel, to control a logical variable\cr
rp.radiogroup: adds a set of radiobuttons to the panel\cr
rp.listbox: adds a listbox to the panel\cr
rp.doublebutton: adds a widget with '+' and '-' buttons, to increment and decrement a variable\cr
rp.image: adds an image to the panel, the action function is called with coordinates on clicking\cr
rp.line: draws a line connecting the pixel locations x1, y1 to x2, y2 on the specified rp.image\cr
rp.deleteline: removes a line from an rp.image\cr
rp.clearlines: removes all lines from an rp.image\cr
rp.messagebox: displays a message in a pop-up window\cr
rp.tkrplot: calls Luke Tierney's \code{tkrplot} function to allow R graphics to be displayed in a panel\cr
rp.tkrreplot: calls Luke Tierney's \code{tkrreplot} functions to allow R graphics to be displayed in a panel.\cr
rp.block: blocks use of the R console until a panel is closed\cr
rp.panel: returns a named panel or the most recently created panel\cr
rp.do: executes a nominated user defined callback function\cr
\cr
Example scripts\cr
\cr
rp.gulls: STEPS module - the Birds and the Bees\cr
rp.ancova: Interactive analysis of covariance\cr
rp.power: Interactive power calculations for a two-sample t-test\cr
rp.rmplot: Interactive plotting of repeated measurement data\cr
rp.tables: Interactive statistical tables\cr
rp.regression2: Interactive display of regression with two covariates\cr
rp.plot3d: Interactive display of a plot of three variables\cr
}
\author{
A.W.Bowman, R.Bowman & E.Crawford

Maintainer: Ewan Crawford <ewan@stats.gla.ac.uk>
}
\references{rpanel: Simple interactive controls for R functions using 
the tcltk package 
(http://www.stats.gla.ac.uk/~adrian/rpanel/)}
\keyword{ package }
\seealso{\code{\link{rp.control}},\code{\link{rp.button}},\code{\link{rp.slider}},\code{\link{rp.doublebutton}},\code{\link{rp.textentry}},\code{\link{rp.checkbox}},\code{\link{rp.radiogroup}}}
\examples{
rp.gulls()
}
