# Moved to rpimage.r and code tidied 18/07/2006 by EC.

rp.image<-function(panel, filename, action = I, id = "img", parent = window, pos = NULL) {
# some preparations 
  ischar <- is.character(panel)
  if (ischar) { panelname <- panel; panel <- .geval(panel) }
  else { panelname <- panel$intname; panelreturn <- deparse(substitute(panel)); .gassign(panel, panelname) }  
  
# read in the file and create the image, create a canvas and place on the panel and then put the image on the canvas
  .geval(panelname, "$", id, ".image <- tkimage.create('photo', file = '", filename, "')")
  canvas <- .geval(panelname, "$", id, ".canvas <- tkcanvas(", panelname, "$window, width = tcl('image', 'width', ", panelname, "$", id, ".image), height = tcl('image', 'height', ", panelname, "$", id, ".image))")
  .geval(panelname, "$", id, ".imageincanvas <- tkcreate(", panelname, "$", id, ".canvas, 'image', 0, 0, image = ", panelname, "$", id, ".image, anchor = 'nw')")
  .rp.layout(canvas, pos)
  
  f <- function(x, y) {
# call the action function
      panel <- action(.geval(panelname), as.numeric(x), as.numeric(y))
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
  tkbind(canvas, "<Button 1>", f) # bind the function to the canvas
  tkconfigure(canvas, cursor="hand2") # make the cursor a hand as it passes over the canvas
  
  if (ischar) invisible(panelname) else assign(panelreturn, .geval(panelname), envir=parent.frame())
}
