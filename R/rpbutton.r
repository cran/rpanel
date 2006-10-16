# Moved to rptextentry.r and code tidied 18/07/2006 by EC.

rp.button <- function(panel, action, title = deparse(substitute(action)), id = "", 
  parent = window, repeatdelay = 0, repeatinterval = 0, quitbutton = FALSE, pos = NULL) {
# some preparations
  ischar <- is.character(panel)
  if (ischar) { panelname <- panel; panel <- .geval(panel) }
  else { panelname <- panel$intname; panelreturn <- deparse(substitute(panel)); .gassign(panel, panelname) }  
  
# callback on pressing button
  f <- function(...) {
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
    if (quitbutton) {
      .geval("try(tkdestroy(", panelname, "$window))")
    }
  }

# create and layout the button
  newbutton <- tkbutton(panel$window, text = title, command = f, repeatdelay = repeatdelay,
    repeatinterval = repeatinterval)
  .rp.layout(newbutton,pos)
  
  if (ischar) invisible(panelname) else assign(panelreturn, .geval(panelname), envir=parent.frame())
}
  
