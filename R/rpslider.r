# Moved to rp.slider.r and code tidied 12/07/2006 by EC.

rp.slider <- function(panel, var, from, to, action = I, title = deparse(substitute(var)),
  log = FALSE, showvalue = FALSE, resolution = 0, initval = NULL, parent = window, pos = NULL, horizontal = TRUE, ...) {
# some preparations
  varname <- deparse(substitute(var))
  ischar <- is.character(panel)
  if (ischar) { panelname <- panel; panel <- .geval(panel) }
  else { panelname <- panel$intname; panelreturn <- deparse(substitute(panel)); .gassign(panel, panelname) }
  
  pos = .newpos(pos, ...)

  if (.checklayout(pos))
# check the pos for unpaired variables etc  
  {

# Over-ride! This stops the incorrect 'unlogged' value being shown.
  if (showvalue && log) showvalue <- FALSE
  
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
# assign the returned value back to the .rpenv - replaces rp.return
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
  if ((!is.list(pos)) || (is.null(pos$grid))) {
    gd = panel$window
  }
  else {
    gd = .geval(panelname,"$",pos$grid)
  }

  if (horizontal==TRUE) {
    orient = "horizontal"
  }
  else {
    orient = "vertical"
  }

   if ((!is.list(pos)) || ((is.null(pos$width)) && (is.null(pos$height)))) {  
    newslider <- tkscale(gd, from = from, to = to, showvalue = showvalue, command = f, orient = orient,
      label = title, resolution = resolution, variable = tclvariable)
  } else {
    newslider <- tkscale(gd, from = from, to = to, showvalue = showvalue, command = f, orient = orient,
      label = title, resolution = resolution, variable = tclvariable, width=pos$width, height=pos$height)
  }
    
#place the slider
  if ((!is.list(pos)) || ((is.null(pos$row)) && (is.null(pos$column)))) {
    .rp.layout(newslider, pos)
  }
  else {
    if (is.null(pos$sticky)) { pos$sticky <- "w" }
    if (is.null(pos$rowspan)) { pos$rowspan = 1 }
    if (is.null(pos$columnspan)) { pos$columnspan = 1; }  
    tkgrid(newslider, row=pos$row, column=pos$column, sticky=pos$sticky, "in"=gd, rowspan=pos$rowspan, columnspan=pos$columnspan)    
  }
  }
  
  if (ischar) invisible(panelname) else assign(panelreturn, .geval(panelname), envir=parent.frame())
}
