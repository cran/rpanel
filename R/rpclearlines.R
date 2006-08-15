# Moved to rpclearlines.r and code tidied 18/07/2006 by EC.

rp.clearlines <- function(panel, image) {
# some preparations 
  imagename <- deparse(substitute(image))
  ischar <- is.character(panel)
  if (ischar) { panelname <- panel; panel <- .geval(panel) }
  else { panelname <- panel$intname; panelreturn <- deparse(substitute(panel)); .gassign(panel, panelname) }  
  
# delete the lines
  .geval("tkdelete(", panelname, "$", imagename, ".canvas, 'withtag', 'rpline')")


  if (ischar) invisible(panelname) else assign(panelreturn, .geval(panelname), envir=parent.frame())
}
