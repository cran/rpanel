# Moved to rpradiogroup.r and code tidied 18/07/2006 by EC.

rp.radiogroup <- function(panel, var, values, labels = values, initval = values[1],
  parent = window, pos = NULL, title = deparse(substitute(var)), action = I, ...) {
# some preparations 
  varname <- deparse(substitute(var))
  ischar <- is.character(panel)
  if (ischar) { panelname <- panel; panel <- .geval(panel) }
  else { panelname <- panel$intname; panelreturn <- deparse(substitute(panel)); .gassign(panel, panelname) }  
  
# create the property varname within the panel
  inittclvalue <- .rp.initialise(panelname, varname, initval = initval)

# create a frame to contain the radiogroup
  newradiogroup <- tkwidget(panel$window, "labelframe", text = title)
  .rp.layout(newradiogroup, pos)
  
# setup the slider value and set to initial value
  tclvariable <- .geval(panelname, "$", varname, ".tcl <- tclVar(", deparse(inittclvalue), ")")

# add a function to each radiobutton
  for (i in 1:length(values))
  {
    f <- function()
    {
      .geval(panelname, "$", varname, " <- as.character(tclvalue(", panelname, "$", varname, ".tcl))")
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
    rb <- tkradiobutton(newradiogroup, command = f, value = values[i])
# key stage of attaching the panel variable to the button
    tkconfigure(rb, variable = tclvariable)
    lb <- tklabel(newradiogroup, text = values[i])
    tkgrid(rb, lb, sticky = "w") 
  }

  if (ischar) invisible(panelname) else assign(panelreturn, .geval(panelname), envir=parent.frame())
}
