# Moved to rpcontrol.r and code tidied 17/07/2006 by EC.
# size = NULL
rp.control <- function(title = "", size = c(100,100), panelname, realname, aschar = TRUE, ...) {  
# Every time a panel is created .rpnumber is incremented. It may be used in the panelname if 
# rp.panelname is used.
  if (missing(panelname) && missing(realname)) { panelname <- rp.panelname() }
  else { if (!missing(realname) && missing(panelname)) { panelname <- realname } }
  
  if (!is.null(size)) { 
    if (length(size) == 2) {
      wm <- tktoplevel(height = size[2], width = size[1]) 
    }
    else {
      wm <- tktoplevel()
      geo <- paste(size[1], "x", size[2], "+", size[3], "+", size[4], sep="")
      tkwm.geometry(wm, geo) 
    }  
  }
  else { wm <- tktoplevel() }
  
  tkwm.title(wm, title)

  tkbind(wm, "<Destroy>", function() {
# 'exists' is required as widgets within the panel will inherit this <destroy> function.
# Thus as the widgets are removed from the panel as it closes this is called multiple times.
    if (exists(paste(panelname,"$window",sep=""), envir=.rpenv)) { .geval("try(tkdestroy(", panelname, "$window))") }
# Remove from r object from panel's environment environment.
    if (exists(panelname, envir=.rpenv)) { .geval("try(rm(", panelname, "))") }
  })
  
  panel <- list(...)
  panel$window <- wm
  panel$intname <- panelname

# "output" the panel to the panel's environment environment.
  .gassign(panel, panelname)

  if (aschar) invisible(panelname) else assign(panelname, .geval(panelname), envir=parent.frame())
}
