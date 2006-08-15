# Moved to rpdeleteline.r and code tidied 18/07/2006 by EC.

rp.deleteline <- function(panel, image, id) {
# some preparations 
  imagename <- deparse(substitute(image))
  ischar <- is.character(panel)
  if (ischar) { panelname <- panel; panel <- .geval(panel) }
  else { panelname <- panel$intname; panelreturn <- deparse(substitute(panel)); .gassign(panel, panelname) }  
    
  .geval("tkdelete(", panelname, "$", imagename, ".canvas, 'withtag', ", panelname, "$", imagename, ".", id, ")")

  if (ischar) invisible(panelname) else assign(panelreturn, .geval(panelname), envir=parent.frame())
}
