# Added May 2008

rp.text <- function(panel, text, pos = NULL, ...) {
# some preparations
  ischar <- is.character(panel)
  if (ischar) { panelname <- panel; panel <- .geval(panel) }
  else { panelname <- panel$intname; panelreturn <- deparse(substitute(panel)); .gassign(panel, panelname) }  

  pos = .newpos(pos, ...)

  if (.checklayout(pos))
# check the pos for unpaired variables etc  
  {
  
# create and layout the button
  if ((!is.list(pos)) || ((is.null(pos$row)) && (is.null(pos$column)))) 
  {
    if ((!is.list(pos)) || ((is.null(pos$width)) && (is.null(pos$height)))) {  
      newtext <- tklabel(panel$window, text = text)
    } else {
      newtext <- tklabel(panel$window, text = text, width=pos$width, height=pos$height)
    }
    .rp.layout(newtext,pos)
  }
  else 
  {
    if (is.null(pos$grid)) {
      gd = panel$window
    }
    else {
      gd = .geval(panelname,"$",pos$grid)
    }
    if ((is.null(pos$width)) && (is.null(pos$height))) {  
      newtext <- tklabel(panel$window, text = text)
    } else {
      newtext <- tklabel(panel$window, text = text, width=pos$width, height=pos$height)
    }
    if (is.null(pos$sticky)) { pos$sticky <- "w" }
    if (is.null(pos$rowspan)) { pos$rowspan = 1 }
    if (is.null(pos$columnspan)) { pos$columnspan = 1; }  
# Note - the in must be in quotes due to the fact that in is a reserved work in R. Daft this.
    tkgrid(newtext, row=pos$row, column=pos$column, sticky=pos$sticky, "in"=gd, rowspan=pos$rowspan, columnspan=pos$columnspan)
  }
  
  }
  
  if (ischar) invisible(panelname) else assign(panelreturn, .geval(panelname), envir=parent.frame())
}
  
