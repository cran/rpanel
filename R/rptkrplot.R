# Moved to rptkrplot.r and code tidied 19/07/2006 by EC.

rp.tkrplot <- function(panel, name, plotfun, action = I, parent = window, pos = NULL) {
# some preparations 
  if (require(tkrplot)) {  # This uses Luke Tierney's tkrplot package

  id <- deparse(substitute(name)) # this is the 'tag' for the plot used in tp.tkrreplot
  ischar <- is.character(panel)
  if (ischar) { panelname <- panel; panel <- .geval(panel) }
  else { panelname <- panel$intname; panelreturn <- deparse(substitute(panel)); .gassign(panel, panelname) }  

# add the function plotfun (which creates the plot in r) to the panel
  .passign(plotfun, panelname, paste(id, ".plotfun", sep=""))

# create the plot using plotfun and tkrplot
  theplot <- .geval(panelname, "$", id, " <- tkrplot(", panelname, "$window, function() ", panelname, "$", id, ".plotfun(", panelname, "))")
  .rp.layout(theplot,pos)

# callback function if action is defined. This will return the x & y coordinates of where clicked
  f <- function(x, y) {
# call the action function
    panel <- action(.geval(panelname), x, y)
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
  tkbind(theplot, "<Button 1>", f)
  tkconfigure(theplot, cursor = "hand2")

  if (ischar) invisible(panelname) else assign(panelreturn, .geval(panelname), envir=parent.frame())
  }
  else {
    warning("Package TkRplot is not installed.")
  }
}
