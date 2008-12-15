# Moved to rpbutton.r and code tidied 18/07/2006 by EC.
# Grid support added 01/2008
# Pos Alterations 07/2008

rp.button <- function(panel, action, title = deparse(substitute(action)), id = "", parent = window, repeatdelay = 0, repeatinterval = 0, quitbutton = FALSE, pos = NULL, ...) {
# some preparations
  ischar <- is.character(panel)
  if (ischar) { panelname <- panel; panel <- .geval(panel) }
  else { panelname <- panel$intname; panelreturn <- deparse(substitute(panel)); .gassign(panel, panelname) }  

  pos = .newpos(pos, ...)

# callback on pressing button
  f <- function(...) {
# call the action function
    panel <- action(.geval(panelname))
# has the panel been passed back?
    if (!is.null(panel$intname)) {      
# assign the returned value back to the .rpenv - replaces rp.return
      .gassign(panel,panelname)
    }
    else {
# no intname? no panel! Stop and complain.
      stop("The panel was not passed back from the action function.")
    }
    if (quitbutton) {
      .geval("try(tkdestroy(", panelname, "$window))")
    }
  }

# Start to lay out

  if (.checklayout(pos))
# check the pos for unpaired variables etc  
  {

# create and layout the button
    if ((!is.list(pos)) || (is.null(pos$row)))
# this does not use "grid"  
    {
      newbutton <- tkbutton(panel$window, text = title, command = f, repeatdelay = repeatdelay, repeatinterval = repeatinterval)
      .rp.layout(newbutton,pos)
    } # not grid
    else 
# this uses "grid"  
    {
# get the grid
      if (is.null(pos$grid)) { gd = panel$window } else { gd = .geval(panelname,"$",pos$grid) }
      if ((is.null(pos$width)) && (is.null(pos$height))) {
# if the size is not specified
        newbutton <- tkbutton(panel$window, text = title, command = f, repeatdelay = repeatdelay, repeatinterval = repeatinterval)
      } else {
# if the size is not specified
        newbutton <- tkbutton(panel$window, text = title, command = f, repeatdelay = repeatdelay, repeatinterval = repeatinterval, width=pos$width, height=pos$height)
      }
      if (is.null(pos$sticky)) { pos$sticky <- "w" }
      if (is.null(pos$rowspan)) { pos$rowspan = 1 }
      if (is.null(pos$columnspan)) { pos$columnspan = 1 }
# Note - the "in" must be in quotes due to the fact that this is a reserved word in R. Very poor.
      tkgrid(newbutton, row=pos$row, column=pos$column, sticky=pos$sticky, "in"=gd, rowspan=pos$rowspan, columnspan=pos$columnspan)
    } # is grid

  } # end of if .checklayout(pos)
  
  if (ischar) invisible(panelname) else assign(panelreturn, .geval(panelname), envir=parent.frame())
}
