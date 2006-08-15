# Moved to rpcontrol.r and code tidied 17/07/2006 by EC.

rp.control <- function(title = "", size = NULL, panelname, realname, aschar = TRUE, ...) {  
# Every time a panel is created .rpnumber is incremented. It may be used in the panelname if 
# rp.panelname is used.
  if (missing(panelname) && missing(realname)) { panelname <- rp.panelname() }
  else { if (!missing(realname) && missing(panelname)) { panelname <- realname } }
  
  panel <- list(...)
  
  if (!is.null(size)) { panel$window <- tktoplevel(height = size[2], width = size[1]) }
  else { panel$window <- tktoplevel() }
  
  tkwm.title(panel$window, title)

  tkbind(panel$window, "<Destroy>", function() {
# 'exists' is required as widgets within the panel will inherit this <destroy> function.
# Thus as the widgets are removed from the panel as it closes this is called multiple times.
# Destroy tcltk object.
    if (exists(paste(panelname,"$window",sep=""))) { .geval("try(tkdestroy(", panelname, "$window))") }
# Remove from r object from global environment.
    if (exists(panelname)) { .geval("try(rm(", panelname, "))") }
  })
  
  panel$intname <- panelname

# "output" the panel to the global environment.
  .gassign(panel, panelname)

  if (aschar) invisible(panelname) else assign(panelname, .geval(panelname), envir=parent.frame())
}
