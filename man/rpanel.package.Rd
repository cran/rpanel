\name{rpanel-package}

\alias{rpanel-package}
\alias{rpanel}

\docType{package}

\title{Simple interactive controls for R functions using the tcltk package}

\description{
\pkg{rpanel} provides a set of functions to build simple GUI controls for R functions.  Uses include changing a parameter on a graph (and animating it) with a slider, or a "doublebutton", 
up to more sophisticated mini-applications.  In addition to functions which create controls, a number of `cartoon' functions built on these controls are also available.}

\details{
\tabular{ll}{
Package: \tab rpanel\cr
Type: \tab Package\cr
Version: \tab 1.1-5\cr
Date: \tab 2021-09-02\cr
License: \tab GNU\cr
}

This package contains a number of functions (with help and examples) and several 
example scripts.\cr
\cr
Cartoon functions\cr
\cr
\code{\link{rp.gulls}}: An interactive problem-solving exercise on deciding the sex of a herring gull\cr
\code{\link{rp.ci}}: Confidence intervals\cr
\code{\link{rp.anova}}: Analysis of variance\cr
\code{\link{rp.ancova}}: Analysis of covariance\cr
\code{\link{rp.power}}: Power calculations for a two-sample t-test\cr
\code{\link{rp.normal}}: Fitting a normal distribution to a single sample\cr
\code{\link{rp.rmplot}}: Plotting of repeated measurement data\cr
\code{\link{rp.tables}}: Interactive statistical tables\cr
\code{\link{rp.regression}}: Regression with one or two covariates\cr
\code{\link{rp.plot3d}}: Interactive display of a plot of three variables\cr
\code{\link{rp.plot4d}}: Interactive display of a plot of four variables\cr
\code{\link{rp.spacetime}}: A version of \code{rp.plot4d} designed for space-time data\cr
\code{\link{rp.likelihood}}: Exploration of one and two parameter likelihood functions\cr
\code{\link{rp.logistic}}: Interactive display of logistic regression with a single covariate\cr
\code{\link{rp.cartoons}}: A menu-driven set of \code{rpanel} illustrations\cr
\code{\link{rp.geosim}}: Simulation of spatial processes\cr
\code{\link{rp.mururoa}}: Sampling in Mururoa Atoll\cr
\code{\link{rp.firth}}: Sampling in a firth\cr
\code{\link{rp.surface}}: Displaying the uncertainty in an estimate of a surface\cr
\cr
Functions to create individual controls\cr
\cr
\code{\link{rp.control}}: create an rpanel\cr
\code{\link{rp.slider}}: add a slider to a panel, to graphically control a numeric variable\cr
\code{\link{rp.textentry}}: adds a box allows text to be entered\cr
\code{\link{rp.button}}: adds a button to the panel with a nominated function called on pressing\cr
\code{\link{rp.checkbox}}: adds a checkbox to the panel, to control a logical variable\cr
\code{\link{rp.radiogroup}}: adds a set of radiobuttons to the panel\cr
\code{\link{rp.listbox}}: adds a listbox to the panel\cr
\code{\link{rp.combo}}: adds a combo box to the panel\cr
\code{\link{rp.doublebutton}}: adds a widget with '+' and '-' buttons, to increment and decrement a variable\cr
\code{\link{rp.menu}}: adds a menu to the panel\cr
\code{\link{rp.text}}: adds a text box to the panel\cr
\code{\link{rp.image}}: adds an image to the panel; the action function is called with coordinates on clicking\cr
\code{\link{rp.line}}: draws a line connecting the pixel locations x1, y1 to x2, y2 on the specified rp.image\cr
\code{\link{rp.deleteline}}: removes a line from an rp.image\cr
\code{\link{rp.clearlines}}: removes all lines from an rp.image\cr
\code{\link{rp.messagebox}}: displays a message in a pop-up window\cr
\code{\link{rp.tkrplot}}: calls Luke Tierney's \code{tkrplot} function to allow R graphics to be displayed in a panel\cr
\code{\link{rp.tkrreplot}}: calls Luke Tierney's \code{tkrreplot} functions to allow R graphics to be refreshed in a panel.\cr
\code{\link{rp.timer}}: executes an action function repeatedly until a condition is satisfied\cr
\code{\link{rp.block}}: blocks use of the R console until a panel is closed\cr
\code{\link{rp.panel}}: returns a named panel or the most recently created panel\cr
\code{\link{rp.var.put}}: place an object into the rpanel environment, usually within a panel\cr
\code{\link{rp.var.get}}: retrieve an object from the rpanel environment, usually from a panel\cr
\code{\link{rp.pos}}: a demonstration function for layout control\cr
\code{\link{rp.grid}}: a grid system for layout control\cr
\code{\link{rp.do}}: executes a nominated user defined callback function\cr
\code{\link{rp.colour.key}}: a colour key to associate with a plot\cr
\cr
Generally speaking these functions have a parameter, \code{name}, which is used to later delete or modify a widget.\cr
\cr
}
\author{
E. Crawford & A. Bowman

Maintainer: Adrian Bowman <adrian.bowman@glasgow.ac.uk>
}
\references{
   rpanel: Simple interactive controls for R functions using the tcltk package.
      Journal of Statistical Software, 17, issue 9.
   }
\keyword{package}
\seealso{\code{\link{rp.control}},\code{\link{rp.button}},\code{\link{rp.slider}},\code{\link{rp.doublebutton}},\code{\link{rp.textentry}},\code{\link{rp.checkbox}},\code{\link{rp.radiogroup}}}
\examples{
\dontrun{
   rp.gulls()
}}

\keyword{iplot}
\keyword{dynamic}
