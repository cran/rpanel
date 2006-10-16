# Moved to rpcheckbox.r and code tidied 18/07/2006 by EC.

rp.checkbox <-function(panel, var, action = I, parent = window, title = deparse(substitute(var)), 
  initval = NULL, pos = NULL){
# some preparations 
  varname <- deparse(substitute(var))
  ischar <- is.character(panel)
  if (ischar) { panelname <- panel; panel <- .geval(panel) }
  else { panelname <- panel$intname; panelreturn <- deparse(substitute(panel)); .gassign(panel, panelname) }  
  
# create the property varname within the panel
  inittclvalue <- .rp.initialise(panelname, varname, initval, FALSE)

# callback on pressing enter
  f <- function(...) {
    .geval(panelname, "$", varname, " <- as.logical(tclObj(", panelname, "$", varname, ".tcl))")
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

# setup the checkbox value and set to initial value
  tclvariable <- .geval(panelname, "$", varname, ".tcl <- tclVar(", deparse(inittclvalue), ")")
# create the checkbox
  newcheckbox <- tkcheckbutton(panel$window, command = f, text = title, variable = tclvariable)
  .rp.layout(newcheckbox, pos, "default_left_align")

  if (ischar) invisible(panelname) else assign(panelreturn, .geval(panelname), envir=parent.frame())
}
  
