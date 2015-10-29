w.button <- function(parent, action = I, title = deparse(substitute(action)), 
                     repeatdelay = 0, repeatinterval = 0, pos = NULL, 
                     foreground = NULL, background = NULL, font = NULL) {
   widget <- w.createwidget(parent, pos, background)
   widget$.type <- "button"
   f <- function(...) action()
   widget$.widget <- handshake(tkbutton, parent$.handle, text=title, command=f, 
                               repeatdelay=repeatdelay, repeatinterval=repeatinterval)
   w.appearancewidget(widget, font, foreground, background)
   invisible(widget)
}

rp.button <- function(panel, action, title=deparse(substitute(action)), 
                      repeatdelay = 0, repeatinterval = 0, quitbutton = FALSE, 
                      pos = NULL, foreground = NULL, background = NULL, font = NULL,
                      parentname = deparse(substitute(panel)), 
                      name = paste("button", .nc(), sep = ""), ...) {

  if (!exists(panel$panelname, .rpenv, inherits = FALSE)) {# if the panelname is not set then
    panelname = deparse(substitute(panel)) # the panel name should be the panel deparse subst'ed
# 13/03/2012 these lines are not commented out in previous version
#    panel <- rp.control.get(panelname, panel) # now get the panel
#    panel$panelname = panelname # now set the panelname properly
#    assign(panelname, panel, envir=.rpenv) # now send back the panel
  } 
  else { 
    panelname = panel$panelname 
# 13/03/2012 these lines are not commented out in previous version
#    panel <- rp.control.get(panelname, panel) # now get the panel
  }
  
  if (is.null(pos)) { if (length(list(...))) { pos <- list(...) } }

  f <- function()
  {
    panel <- rp.control.get(panelname)
    panel <- action(panel)
    rp.control.put(panelname, panel)
  }

  if (rp.widget.exists(panelname, parentname)) 
  { 
    parent <- rp.widget.get(panelname, parentname) 
  }
  else 
  { 
    parent <- panel 
  }
  if (is.list(pos)) { if (!is.null(pos$grid)) { parent <- rp.widget.get(panelname, pos$grid) } }

  widget <- w.button(parent, f, title, repeatdelay, repeatinterval, pos, foreground, background, font)
  rp.widget.put(panelname, name, widget)

  if (.rpenv$savepanel) { rp.control.put(panelname, panel) } # put the panel back into the environment
  
  
  invisible(panelname)
}
