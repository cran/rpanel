# Moved to rptextentry.r and code tidied 17/07/2006 by EC.

rp.textentry <- function(panel, var, action = I, title = deparse(substitute(var)), initval = NULL, 
  parent = window, pos = NULL){
# some preparations
  varname <- deparse(substitute(var))
  ischar <- is.character(panel)
  if (ischar) { panelname <- panel; panel <- .geval(panel) }
  else { panelname <- panel$intname; panelreturn <- deparse(substitute(panel)); .gassign(panel, panelname) }  
  
# create the property varname within the panel
  inittclvalue <- .rp.initialise(panelname, varname, initval)
  
# callback on pressing any key
  keypress <- function(...) {
    .geval(panelname, "$", varname, " <- tclvalue(", panelname, "$", varname, ".tcl)")
  }
# callback on pressing enter
  enterpress <- function(...) {
    .geval(panelname, "$", varname, " <- tclvalue(", panelname, "$", varname, ".tcl)")
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

# setup the text entry and set to initial value
  tclvariable <- .geval(panelname, "$", varname, ".tcl <- tclVar(", deparse(inittclvalue), ")")
# add the label, textbox and lay it all out
  frame <- tkframe(panel$window)
  .rp.layout(frame, pos) #lay it out
  label <- tklabel(frame, text = title)
  entry <- tkentry(frame, textvariable = tclvariable) #create the widget
  tkgrid(label,entry)
# bind the keypress and enter events to the textbox
  tkbind(entry, "<Key-Return>", enterpress) #run the function when enter is pressed
  tkbind(entry, "<KeyRelease>", keypress) #sync the text whenever user types

  if (ischar) invisible(panelname) else assign(panelreturn, .geval(panelname), envir=parent.frame())
}
