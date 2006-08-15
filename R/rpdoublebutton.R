# Moved to rpdoublebutton.r and code tidied 18/07/2006 by EC.

rp.doublebutton <- function(panel, var, step, title = deparse(substitute(var)), action = I, initval = NULL, 
  range = c(NA, NA), log = FALSE, repeatinterval = 100, repeatdelay = 100, parent = window, pos = NULL) {
# some preparations 
  varname <- deparse(substitute(var))
  ischar <- is.character(panel)
  if (ischar) { panelname <- panel; panel <- .geval(panel) }
  else { panelname <- panel$intname; panelreturn <- deparse(substitute(panel)); .gassign(panel, panelname) }  
  
# create the property varname within the panel
  inittclvalue <- .rp.initialise(panelname,varname,initval)  # formerly had ,1 at end
# create the frame to contain the buttons and labels
  frame <- tkframe(panel$window)
  .rp.layout(frame, pos, "default_left_align")
  label <- tklabel(frame, text = title)

# the button callback function
  changefun <- function(panelname, varname, op, action) {
    function(...) {
      newvalue <- .geval(panelname, "$", varname, " ", op, " ", as.character(step))      
      if (!is.na(range[1])) newvalue <- max(newvalue, range[1])
      if (!is.na(range[2])) newvalue <- min(newvalue, range[2])
      .geval(panelname, "$", varname, " <- ", as.character(newvalue))
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
  }

# create the two buttons' functions
  incfun <- changefun(panelname, varname, if (log) "*" else "+", action)
  decfun <- changefun(panelname, varname, if (log) "/" else "-", action)
# create the two buttons  
  incbutton <- tkbutton(frame, text = "+", command = incfun, repeatdelay = repeatdelay, repeatinterval = repeatinterval)
  decbutton <- tkbutton(frame, text = "-", command = decfun, repeatdelay = repeatdelay, repeatinterval = repeatinterval)
# add the buttons and label to the frame  
  tkgrid(decbutton,incbutton,label)

  if (ischar) invisible(panelname) else assign(panelreturn, .geval(panelname), envir=parent.frame())
}
