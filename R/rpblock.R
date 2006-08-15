# Moved to rpblock.r and code tidied 19/07/2006 by EC.

rp.block <- function(panel) {
# some preparations 
  ischar <- is.character(panel)
  if (ischar) { panelname <- panel; panel <- .geval(panel) }
  else { panelname <- panel$intname; panelreturn <- deparse(substitute(panel)); .gassign(panel, panelname) }  

  open <- .geval("exists('",panelname, "')")
  while(open){
    Sys.sleep(0.01)
    open <- .geval("exists('",panelname, "')")
  }
  invisible()
}
