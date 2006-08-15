# Moved to rptkrreplot.r and code tidied 19/07/2006 by EC.

rp.tkrreplot<-function(panel, name) {
# some preparations 
  if (require(tkrplot)) {  # This uses Luke Tierney's tkrplot package

  id <- deparse(substitute(name))
  ischar <- is.character(panel)
  if (ischar) { panelname <- panel; panel <- .geval(panel) }
  else { panelname <- panel$intname; panelreturn <- deparse(substitute(panel)); .gassign(panel, panelname) }  
  
# replot the graph. plotfun support removed - we always use the functionalready defined in tp.tkplot
  tkrreplot(.geval(panelname, "$", id))

  if (ischar) invisible(panelname) else assign(panelreturn, .geval(panelname), envir=parent.frame())
  }
  else {
    warning("Package TkRplot is not installed.")
  }
}
