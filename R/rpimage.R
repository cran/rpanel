# Moved to rpimage.r and code tidied 18/07/2006 by EC.

rp.image<-function(panel, filename, action = NA, mousedrag = NA, mouseup = NA, id = "img", parent = window, pos = NULL, ...) {
# some preparations
 ischar <- is.character(panel)
 if (ischar) { panelname <- panel; panel <- .geval(panel) }
 else { panelname <- panel$intname; panelreturn <- deparse(substitute(panel)); .gassign(panel, panelname) }  # read in the file and create the image, create a canvas and place on the panel and then put the image on the canvas
 .geval(panelname, "$", id, ".image <- tkimage.create('photo', file = '", filename, "')")
 canvas <- .geval(panelname, "$", id, ".canvas <- tkcanvas(", panelname, "$window, width = tcl('image', 'width', ", panelname, "$", id, ".image), height = tcl('image', 'height', ", panelname, "$", id, ".image))")
 .geval(panelname, "$", id, ".imageincanvas <- tkcreate(", panelname, "$", id, ".canvas, 'image', 0, 0, image = ", panelname, "$", id, ".image, anchor = 'nw')")

  pos = .newpos(pos, ...)

  if (.checklayout(pos))
# check the pos for unpaired variables etc  
  {

  if ((!is.list(pos)) || ((is.null(pos$row)) && (is.null(pos$column))))
  {
   .rp.layout(canvas, pos)
  }
  else
  {
    if (is.null(pos$grid))
    {
      gd = panel$window
    }
    else
    {
      gd = .geval(panelname,"$",pos$grid)
    }
    if (is.null(pos$sticky)) { pos$sticky <- "w" }
    if (is.null(pos$rowspan)) { pos$rowspan = 1 }
    if (is.null(pos$columnspan)) { pos$columnspan = 1; }  
# Note - the in must be in quotes due to the fact that in is a reserved work in R. Daft this.
    tkgrid(canvas, row=pos$row, column=pos$column, sticky=pos$sticky, "in"=gd, rowspan=pos$rowspan, columnspan=pos$columnspan)  
  }

# callback function if action is defined. This will return the x & y coordinates of where clicked
  fdown <- function(x, y) {
# call the action function
    panel <- action(.geval(panelname), x, y)
# has the panel been passed back?
    if (!is.null(panel$intname)) {
# assign the returned value back to the .rpenv - replaces rp.return
      .gassign(panel,panelname)
    }
    else {
# no intname? no panel! Stop and complain.
      stop("The panel was not passed back from the action function.")
    }
  }
# callback function if mousedrag is defined. This will return the x & y coordinates of where clicked
  fdrag <- function(x, y) {
# call the action function
    panel <- mousedrag(.geval(panelname), x, y)
# has the panel been passed back?
    if (!is.null(panel$intname)) {
# assign the returned value back to the .rpenv - replaces rp.return
      .gassign(panel,panelname)
    }
    else {
# no intname? no panel! Stop and complain.
      stop("The panel was not passed back from the action function.")
    }
  }
# callback function if mouseup is defined. This will return the x & y coordinates of where clicked
  fup <- function(x, y) {
# call the action function
    panel <- mouseup(.geval(panelname), x, y)
# has the panel been passed back?
    if (!is.null(panel$intname)) {
# assign the returned value back to the .rpenv - replaces rp.return
      .gassign(panel,panelname)
    }
    else {
# no intname? no panel! Stop and complain.
      stop("The panel was not passed back from the action function.")
    }
  }
  if (is.function(action)) { tkbind(canvas, "<Button-1>", fdown) }
  if (is.function(mousedrag)) { tkbind(canvas, "<B1-Motion>", fdrag) }
  if (is.function(mouseup)) { tkbind(canvas, "<ButtonRelease-1>", fup) }

 tkconfigure(canvas, cursor="hand2") # make the cursor a hand as it passes over the canvas
 
 }
 
 if (ischar) invisible(panelname) else assign(panelreturn, .geval(panelname), envir=parent.frame())
}
