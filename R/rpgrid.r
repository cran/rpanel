# new grid control to assist layout

rp.grid <- function(panel, name, pos = NULL, bg = NULL, parent=window, ...) {

# some preparations
  ischar <- is.character(panel)
  if (ischar) { panelname <- panel; panel <- .geval(panel) }
  else { panelname <- panel$intname; panelreturn <- deparse(substitute(panel)); .gassign(panel, panelname) }  

  pos = .newpos(pos, ...)

#  frame name create into rpanel

  if (.checklayout(pos))
# check the pos for unpaired variables etc  
  {


  if ((!is.list(pos)) || (is.null(pos$grid)))
  {
    gd = paste(panelname, "$window", sep="")
  }
  else
  {
    gd = paste(panelname, "$", grid, sep="")
  }

  if ((!is.list(pos)) || (is.null(pos$width)))
  { 
    if (is.null(bg))
    {
      newframe <- .geval(panelname, "$", name, " <- tkframe(", gd, ")")
    }
    else
    {
      newframe <- .geval(panelname, "$", name, " <- tkframe(", gd, ", bg='",bg,"')")
    }
  }
  else 
  { 
    if (is.null(bg))
    {
      newframe <- .geval(panelname, "$", name, " <- tkframe(", gd, ", width=", as.character(pos$width), ", height=", as.character(pos$height),")")
    }
    else
    {
      newframe <- .geval(panelname, "$", name, " <- tkframe(", gd, ", bg='",bg,"', width=", as.character(pos$width), ", height=", as.character(pos$height),")")
    }
    # print(cat("width specified: width=", as.character(pos$width), ", height=", as.character(pos$height)))
  }

#  frame set pos and parent
  if ((!is.list(pos)) || (is.null(pos$row)))
  { 
    tkgrid(newframe, row=0, column=0, sticky="news")
    tkgrid.rowconfigure(panel$window,0,weight=1)
    tkgrid.columnconfigure(panel$window,0,weight=1)
    tkgrid.rowconfigure(newframe,0,weight=1)
    tkgrid.columnconfigure(newframe,0,weight=1)
  }
  else 
  { 
    if (is.null(pos$sticky)) { pos$sticky <- "w" }
    if (is.null(pos$rowspan)) { pos$rowspan = 1 }
    if (is.null(pos$columnspan)) { pos$columnspan = 1; }  
    tkgrid(newframe, row=pos$row, column=pos$column, rowspan=pos$rowspan, columnspan=pos$columnspan, sticky=pos$sticky)
  }
  }

  if (ischar) invisible(panelname) else assign(panelreturn, .geval(panelname), envir=parent.frame())
}
