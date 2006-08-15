# Moved to rp.slider.r and code tidied 12/07/2006 by EC.

rp.slider <- function(panel, var, from, to, action = I, title = deparse(substitute(var)),
  log = FALSE, resolution = 0, initval = NULL, parent = window, pos = NULL) {
# some preparations
  varname <- deparse(substitute(var))
  ischar <- is.character(panel)
  if (ischar) { panelname <- panel; panel <- .geval(panel) }
  else { panelname <- panel$intname; panelreturn <- deparse(substitute(panel)); .gassign(panel, panelname) }  
  
# create the property varname within the panel
  inittclvalue <- .rp.initialise(panelname, varname, initval, from)
# done after using 'from' as the default value; as the values are only logged on callback
  if (log) { to <- log(to); from <- log(from); inittclvalue <- log (inittclvalue) }
  
# this is the callback function
  f <- function(...) {
# get the value of the slider position
    x <- .geval(panelname, "$", varname, ".tcl")
    x <- as.numeric(tclvalue(x))
    if (log) { x <- exp(x) }
# set varname to x - note cannot use assign for this
    .geval(panelname, "$", varname, " <- ", deparse(x))
# call the action function
    panel <- action(.geval(panelname))
# has the panel been passed back?
    if (!is.null(panel$intname)) {      
# assign the returned value back to the .GlobalEnv - replaces rp.return
      .gassign(panel,panelname)
    }
    else {
# no intname? no panel! Stop and complain.
      stop("The panel was not passed back from the action function.")
    }
  }
  
# setup the slider value and set to initial value
  tclvariable <- .geval(panelname, "$", varname, ".tcl <- tclVar(", deparse(inittclvalue), ")")
# create the slider
  newslider <- tkscale(panel$window, from = from, to = to, showvalue = F, command = f, orient = "horizontal",
    label = title, resolution = resolution, variable = tclvariable)
  .rp.layout(newslider, pos)   #place the slider

  if (ischar) invisible(panelname) else assign(panelreturn, .geval(panelname), envir=parent.frame())
}
