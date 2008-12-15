.rptkrplot <- function (parent, fun, hscale=1, vscale=1,
foreground=NULL, margins=c(0,0,0,0))
{
   image <- paste("Rplot", .make.tkindex(), sep = "")
   .my.tkdev(hscale, vscale)
   if (!is.null(foreground))
   {
     foreground <- tkimage.create('photo', file = foreground)
     w <- as.numeric(tcl("image","width", foreground))
     h <- as.numeric(tcl("image","height", foreground))
     pw <- par("pin")[1]
     par(pin=c(pw,(h/w)*pw))
   }


   try(fun())


   .Tcl(paste("image create Rplot", image))

   if (is.null(foreground))
   {
     w <- as.numeric(tcl("image","width", image))
     h <- as.numeric(tcl("image","height", image))
   }

   lab <- tkcanvas(parent, width=w+margins[1]+margins[3], height=h+margins[2]+margins[4])
#    tkcreate(lab, 'image', w, h, image=image, anchor="nw")

   if (!is.null(foreground))
   {
     tkcreate(lab, 'image', margins[1], margins[2], image=foreground, anchor="nw")
     lab$margins <- margins
   }

   tkbind(lab, "<Destroy>", function() .Tcl(paste("image delete", image)))
   lab$image <- image
   lab$fun <- fun
   lab$hscale <- hscale
   lab$vscale <- vscale
   lab$foreground <- foreground
   lab$w <- w
   lab$h <- h
   lab
}

.rp.coords <- function(plot, x, y, parplt, parusr)
{
 xClick <- x
 yClick <- y
 require(tcltk)
 width  <- as.numeric(tclvalue(tkwinfo("reqwidth",plot)))
 height <- as.numeric(tclvalue(tkwinfo("reqheight",plot)))

 xMin <- parplt[1] * width
 xMax <- parplt[2] * width
 yMin <- parplt[3] * height
 yMax <- parplt[4] * height

# print(xMin)
# print(xMax)
# print(yMin)
# print(yMax)

 rangeX <- parusr[2] - parusr[1]
 rangeY <- parusr[4] - parusr[3]

 xClick <- as.numeric(xClick)+0.5
 yClick <- as.numeric(yClick)+0.5
 yClick <- height - yClick

 xPlotCoord <- parusr[1]+(xClick-xMin)*rangeX/(xMax-xMin)
 yPlotCoord <- parusr[3]+(yClick-yMin)*rangeY/(yMax-yMin)

 c(xPlotCoord, yPlotCoord, width, height, xClick, yClick)
}

# Moved to rptkrplot.r and code tidied 19/07/2006 by EC.
# Scaling added 19/02/2007
# Altered to use foregrounds /01/2008

rp.tkrplot <- function(panel, name, plotfun, action = NA, mousedrag = NA, mouseup = NA, hscale = 1, vscale = 1, parent = window, pos=NULL, foreground=NULL, margins=c(0,0,0,0), ...)
{
# some preparations
 if (require(tkrplot)) {  # This uses Luke Tierney's tkrplot package

 id <- deparse(substitute(name)) # this is the 'tag' for the plot used in tp.tkrreplot
 ischar <- is.character(panel)
 if (ischar) { panelname <- panel; panel <- .geval(panel) }
 else { panelname <- panel$intname; panelreturn <- deparse(substitute(panel)); .gassign(panel, panelname) }

  pos = .newpos(pos, ...)

 # print("1")

 if (.checklayout(pos))
# check the pos for unpaired variables etc  
 {

 # print("2")

# add the function plotfun (which creates the plot in r) to the panel
 .passign(plotfun, panelname, paste(id, ".plotfun", sep=""))

# get grid
 if ((!is.list(pos)) || (is.null(pos$grid))) {
   gd = panel$window
 }
 else {
   gd = .geval(panelname,"$",pos$grid)
 }

 # print("3")

# create the plot using plotfun and tkrplot
 if (is.null(foreground))
 {
 
   # print("3A")

# this uses the old function

# print(cat(panelname, "$", id, " <- tkrplot(", panelname, "$window, function() {", panelname, "$", id, ".plotfun(", panelname, "), hscale=", hscale, ", vscale=", vscale,"); ", panelname, "$", id, ".parplt <- par('plt'); ", panelname, "$", id, ".parusr <- par('usr'); assign('",panelname,"', ",panelname,", envir = .rpenv); }", sep=""))

#################################
# theplot <- .geval(panelname, "$", id, " <- tkrplot(", panelname, "$window, function() { temppanel <- ", panelname, "$", id, ".plotfun(", panelname, "); temppanel$", id, ".parplt <- par('plt'); temppanel$", id, ".parusr <- par('usr'); assign('",panelname,"', temppanel, envir = .rpenv); }, hscale=",deparse(hscale),", vscale=",deparse(vscale),")")
# theplot <- .geval(panelname, "$", id, " <- tkrplot(", panelname, "$window, function() ", panelname, "$", id, ".plotfun(", panelname, "), hscale=", hscale, ", vscale=", vscale,");")
# theplot <- .geval(panelname, "$", id, " <- tkrplot(", panelname, "$window, function() { temppanel <- ", panelname, "$", id, ".plotfun(", panelname, "); temppanel$", id, ".parplt <- par('plt'); temppanel$", id, ".parusr <- par('usr'); assign('",panelname,"', temppanel, envir = .rpenv); }, hscale=",deparse(hscale),", vscale=",deparse(vscale),")")
#################################

  theplot <- .geval(panelname, "$", id, " <- tkrplot(", panelname, "$window, function() {", panelname, "$", id, ".plotfun(", panelname, "); ", panelname, "$", id, ".parplt <- par('plt'); ", panelname, "$", id, ".parusr <- par('usr'); assign('",panelname,"', ",panelname,", envir = .rpenv); }, hscale=", hscale, ", vscale=", vscale,");")

 } else {
# this uses the new function which allows foregrounds
 theplot <- .geval(panelname, "$", id, " <- .rptkrplot(", panelname, "$window, function() { temppanel <- ", panelname, "$", id, ".plotfun(", panelname, "); temppanel$", id, ".parplt <- par('plt'); temppanel$", id, ".parusr <- par('usr'); assign('",panelname,"', temppanel, envir = .rpenv); }, hscale=",deparse(hscale),", vscale=",deparse(vscale),", foreground='", foreground, "', margins=c(",deparse(margins[1]),",",deparse(margins[2]),",",deparse(margins[3]),",",deparse(margins[4]),"))")
 }

 # print("4")

# this is needed to pick up the results of any changes to panel made in the function which drew the plot
# these changes are likely to be things such as picking up the par of the plot
 panel <- .geval(panelname)

 if ((!is.list(pos)) || ((is.null(pos$row)) && (is.null(pos$column)))) {
   .rp.layout(theplot,pos)
 }
 else {
   if (is.null(pos$sticky)) { pos$sticky <- "w" }
   if (is.null(pos$rowspan)) { pos$rowspan = 1 }
   if (is.null(pos$columnspan)) { pos$columnspan = 1; }  
   tkgrid(theplot, row=pos$row, column=pos$column, sticky=pos$sticky, "in"=gd, rowspan=pos$rowspan, columnspan=pos$columnspan);
 }

 # print("5")

# callback function if action is defined. This will return the x & y coordinates of where clicked
 fdown <- function(x, y) {
# call the action function
   coords  <-  .rp.coords(.geval(panelname, "$", id), x, y, .geval(panelname, "$", id, ".parplt"), .geval(panelname, "$", id, ".parusr"))
#   panel <- action(.geval(panelname), coords[1], coords[2], x, y)
   panel <- action(.geval(panelname), coords[1], coords[2])
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

 # print("6")

# callback function if mousedrag is defined. This will return the x & y coordinates of where clicked
 fdrag <- function(x, y) {
# call the action function
   coords  <-  .rp.coords(.geval(panelname, "$", id), x, y, .geval(panelname, "$", id, ".parplt"), .geval(panelname, "$", id, ".parusr"))
#   panel <- mousedrag(.geval(panelname), coords[1], coords[2], x, y)
   panel <- mousedrag(.geval(panelname), coords[1], coords[2])
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

 # print("7")

# callback function if mouseup is defined. This will return the x & y coordinates of where clicked
 fup <- function(x, y) {
# call the action function
   coords  <-  .rp.coords(.geval(panelname, "$", id), x, y, .geval(panelname, "$", id, ".parplt"), .geval(panelname, "$", id, ".parusr"))
#   panel <- mouseup(.geval(panelname), coords[1], coords[2], x, y)
   panel <- mouseup(.geval(panelname), coords[1], coords[2])
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

 # print("8")


## For Wayne - add left and right clicks

 if (is.function(action)) { tkbind(theplot, "<Button-1>", fdown) }
 if (is.function(mousedrag)) { tkbind(theplot, "<B1-Motion>", fdrag) }
 if (is.function(mouseup)) { tkbind(theplot, "<ButtonRelease-1>", fup) }
 tkconfigure(theplot, cursor = "hand2")

 if (ischar) invisible(panelname) else assign(panelreturn,
.geval(panelname), envir=parent.frame())
 }
 else {
   warning("Package TkRplot is not installed.")
 }
 
 }
 
}

