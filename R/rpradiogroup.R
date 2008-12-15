# Moved to rpradiogroup.r and code tidied 18/07/2006 by EC. Layout manager changed 12/2007 EC.

rp.radiogroup <- function(panel, var, values, labels = values, initval = values[1],
  parent = window, pos = NULL, title = deparse(substitute(var)), action = I, ...) {
# some preparations 
  varname <- deparse(substitute(var))
  ischar <- is.character(panel)
  if (ischar) { panelname <- panel; panel <- .geval(panel) }
  else { panelname <- panel$intname; panelreturn <- deparse(substitute(panel)); .gassign(panel, panelname) }  

  pos = .newpos(pos, ...)

# create the property varname within the panel
  inittclvalue <- .rp.initialise(panelname, varname, initval = initval)

  if (.checklayout(pos))
# check the pos for unpaired variables etc  
  {

# create a frame to contain the radiogroup
  newradiogroup <- tkwidget(panel$window, "labelframe", text = title, padx=2, pady=2, borderwidth=2)

  if ((!is.list(pos)) || ((is.null(pos$row)) && (is.null(pos$column)))) {
    .rp.layout(newradiogroup, pos)
  }
  else 
  {
    if (is.null(pos$grid)) {
      gd = panel$window
    }
    else {
      gd = .geval(panelname,"$",pos$grid)
    }
    if (is.null(pos$sticky)) { pos$sticky <- "w" }
    if (is.null(pos$rowspan)) { pos$rowspan = 1 }
    if (is.null(pos$columnspan)) { pos$columnspan = 1; }  
# Note - the in must be in quotes due to the fact that in is a reserved work in R. Daft this. 
    tkgrid(newradiogroup, row=pos$row, column=pos$column, sticky=pos$sticky, "in"=gd, rowspan=pos$rowspan, columnspan=pos$columnspan)    
  }

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
## lines marked ## are the version which leads to a mis-displaying of the options
##    rb <- tkradiobutton(newradiogroup, command = f, value = values[i])
# key stage of attaching the panel variable to the button
##    tkconfigure(rb, variable = tclvariable)
##    lb <- tklabel(newradiogroup, text = labels[i])
##    tkgrid(rb, lb)
##    tkgrid.configure(rb, sticky="w") 
##    tkgrid.configure(lb, sticky="w")
##    tkgrid.rowconfigure(newradiogroup, i, weight=1) #, minsize=30
   if ((!is.list(pos)) || ((is.null(pos$width)) && (is.null(pos$height)))) {
     rb <- tkradiobutton(newradiogroup, command = f, text = labels[i], value = values[i], variable = tclvariable)
   } else {
     rb <- tkradiobutton(newradiogroup, command = f, text = labels[i], value = values[i], variable = tclvariable, width=pos$width, height=pos$height)
   }
   tkgrid(rb, sticky = "w", row=i, column=0)
# commenting this out is what makes the radiobuttons appear together rather than irregularly spaced.
#   tkgrid.rowconfigure(newradiogroup, i, weight=1) #, minsize=30)
  }
  
  }

  if (ischar) invisible(panelname) else assign(panelreturn, .geval(panelname), envir=parent.frame())
}
