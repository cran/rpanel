w.timer <- function(microseconds, action, where)
{
  dothis <- function()
  {
    action()
    w.timer(microseconds, action, where)
  }

  if (eval(parse(text=where), .rpenv)) { timer <- handshake(tcl, "after", microseconds, dothis) }
  else { timer <- NULL }

  invisible(timer)
}

rp.timer <- function(panel, microseconds, action, where) {

  if (!exists(panel$panelname, .rpenv, inherits = FALSE)) # if the panelname is not set then
  { 
    panelname = deparse(substitute(panel)) # the panel name should be the panel deparse subst'ed
# 13/03/2012 these lines are not commented out in previous version
#    panel <- rp.control.get(panelname, panel) # now get the panel
#    panel$panelname = panelname # now set the panelname properly
#    assign(panelname, panel, envir=.rpenv) # now send back the panel
  } 
  else 
  { 
    panelname = panel$panelname 
# 13/03/2012 these lines are not commented out in previous version
#    panel <- rp.control.get(panelname, panel) # now get the panel
  }

  dothis <- function()
  {
    panel <- rp.control.get(panelname)
    panel <- action(panel)
    rp.control.put(panelname, panel)
    
# **************************************************
#   need to return value back into various places  *
# **************************************************
    
    if (where(panel)) { timer <- handshake(tcl, "after", microseconds, dothis) }
    else { timer <- NULL }
  }

  if (where(panel)) { timer <- handshake(tcl, "after", microseconds, dothis) }
  else { timer <- NULL }

  invisible(timer)
}

rp.do <- function(panel, action, x = NA, y = NA) 
{
  if (!exists(panel$panelname, .rpenv, inherits = FALSE)) # if the panelname is not set then
  { 
    panelname = deparse(substitute(panel)) # the panel name should be the panel deparse subst'ed
# 13/03/2012 these lines are not commented out in previous version
#    panel <- rp.control.get(panelname, panel) # now get the panel
#    panel$panelname = panelname # now set the panelname properly
#    assign(panelname, panel, envir=.rpenv) # now send back the panel
  } 
  else 
  { 
    panelname = panel$panelname 
# 13/03/2012 these lines are not commented out in previous version
#    panel <- rp.control.get(panelname, panel) # now get the panel
  }
  panel <- rp.control.get(panelname)
  if (is.na(x) & is.na(y))
     panel <- action(panel)
  else
     panel <- action(panel, x, y)
  rp.control.put(panelname, panel)
  invisible(panelname)
}

rp.block <- function(panel) 
{
  if (!exists(panel$panelname, .rpenv, inherits = FALSE)) # if the panelname is not set then
  { 
    panelname = deparse(substitute(panel)) # the panel name should be the panel deparse subst'ed
  } 
  else 
  { 
    panelname = panel$panelname 
  }

  open <- eval(parse(text=paste("!is.null(",panelname, ")", sep="")), envir=.rpenv)
  while(open){
    Sys.sleep(0.01)
    open <- exists(panelname)
    if (open) {
      open <- exists(panelname, envir= .rpenv)
      if (open)
        open <- eval(parse(text=paste("!is.null(",panelname, ")", sep="")), envir=.rpenv)
    }
  }

  invisible()
}
