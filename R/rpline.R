# Moved to rpline.r and code tidied 18/07/2006 by EC.

rp.line <- function(panel, image, x1, y1, x2, y2, ..., color = "black", width = 2, id = "rpline") {
# some preparations 
  imagename <- deparse(substitute(image))
  ischar <- is.character(panel)
  if (ischar) { panelname <- panel; panel <- .geval(panel) }
  else { panelname <- panel$intname; panelreturn <- deparse(substitute(panel)); .gassign(panel, panelname) }
    
  
# create the line and add to canvas with id
  line <- .geval(panelname, "$", imagename, ".", id, " <- tkcreate(", panelname, "$", imagename,
    ".canvas, 'line' ,", as.character(x1), ", ", as.character(y1), ", ", as.character(x2), ", ", 
    as.character(y2), ", fill = '", color, "', width = ", as.character(width), ")")    
  
# add a tag to the line
  .geval("tkaddtag(", panelname, "$", imagename, ".canvas, 'rpline', 'withtag', ", panelname, "$", imagename, ".", id, ")")
  
  if (ischar) invisible(panelname) else assign(panelreturn, .geval(panelname), envir=parent.frame())
}
