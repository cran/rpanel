rp.tkrplot <- function(panel, name, plotfun, action = NA, mousedrag = NA, mouseup = NA,
                       hscale = 1, vscale = 1, pos = NULL, foreground = NULL, background = NULL, 
                       margins=c(0, 0, 0, 0), parentname = deparse(substitute(panel)),
                       mar = par()$mar, ...) {

  panelname <- panel$panelname
  tst <- try(is.character(name), silent = TRUE)
  if (inherits(tst, 'try-error'))
     name <- deparse(substitute(name))
  else
     if (!tst) name <- deparse(substitute(name))

  if (is.null(pos) && length(list(...)) > 0) pos <- list(...)
  
  pf <- function() {
  	 panel <- rp.control.get(panelname)
  	 panel <- plotfun(panel)
  	 rp.control.put(panelname, panel)
  }

  if (is.function(action))
     fa <- function(x, y) { 
        panel <- rp.control.get(panelname)
        panel <- action(panel, x ,y)
        rp.control.put(panelname, panel)
     }
  else
     fa <- NA
  if (is.function(mousedrag))
     fd <- function(x, y) { 
        panel <- rp.control.get(panelname)
        panel <- mousedrag(panel, x, y)
        rp.control.put(panelname, panel)
     }
  else
     fd <- NA
  if (is.function(mouseup))
     fu <- function(x, y) { 
        panel <- rp.control.get(panelname); 
        panel <- mouseup(panel, x ,y)
        rp.control.put(panelname, panel)
     }
  else
     fu <- NA

  if (rp.widget.exists(panelname, parentname))
     parent <- rp.widget.get(panelname, parentname) 
  else
     parent <- panel
  if (is.list(pos) && !is.null(pos$grid))
     parent <- rp.widget.get(panelname, pos$grid)
  
  widget <- w.tkrplot(parent, w.plotfun = pf, action = fa, mousedrag = fd, mouseup = fu,
                      hscale, vscale, pos, foreground, background, margins, name, mar)
  rp.widget.put(panelname, name, widget)
  
  panel <- rp.control.get(panelname)
  rp.control.put(panelname, panel)
  invisible(panelname)
}

.w.tkrplot <- function(parent, fun, hscale = 1, vscale = 1, foreground = NULL, background = NULL, 
                        margins=c(0, 0, 0, 0)) {
                        	
   foreground <- handshake(tkimage.create, 'photo', file = foreground)
   w <- as.numeric(handshake(tcl, "image", "width", foreground))
   h <- as.numeric(handshake(tcl, "image", "height", foreground))
   pw <- par("pin")[1]
   par(pin = c(pw, (h / w) * pw))
   
   fl <- tempfile(pattern = "tkrplotawb", fileext = ".png")
   on.exit(unlink(fl))
   png(filename = fl, width = 480 * hscale, height = 480 * vscale)
   image <- try(fun())
   # assign(paste(name, ".plt", sep = ""), par('plt'), envir = .rpenv)
   # assign(paste(name, ".usr", sep = ""), par('usr'), envir = .rpenv)
   if (inherits(image, "try-error")) {
      txt <- if (grepl("figure margins", image)) 'hscale may be too small.'
      else ''
      stop(txt)
   }
   dev.off()
   
   image <- handshake(tkimage.create, 'photo', file = fl)
   # w.widget <- handshake(tkcanvas, parent,
   #                       width  = handshake(tcl, 'image', 'width',  image),
   #                       height = handshake(tcl, 'image', 'height', image))
   # imageincanvas <- handshake(tkcreate, w.widget, 'image', 0, 0, image = image, anchor = 'nw')
   
  margins <- as.numeric(margins)
  lab <- handshake(tkcanvas, parent, width=w+margins[1]+margins[3], height=h+margins[2]+margins[4])
  handshake(tkcreate, lab, 'image', margins[1], margins[2], image=foreground, anchor="nw")
  lab$margins <- margins
  w.setbackground(lab, background)
  	
  handshake(tkbind, lab, "<Destroy>", function() handshake(.Tcl, paste("image delete", image)))

  lab$image      <- image
  lab$fun        <- fun
  lab$hscale     <- hscale
  lab$vscale     <- vscale
  lab$foreground <- foreground
  lab$w          <- w
  lab$h          <- h
  lab
}

.w.coords <- function(plot, x, y, parplt, parusr) {
  # convert the x,y clicked co-ordinates into values from the plot itself
  xClick <- x
  yClick <- y
  width  <- as.numeric(handshake(tclvalue, handshake(tkwinfo, "reqwidth",  plot)))
  height <- as.numeric(handshake(tclvalue, handshake(tkwinfo, "reqheight", plot)))
  xMin   <- parplt[1] * width
  xMax   <- parplt[2] * width
  yMin   <- parplt[3] * height
  yMax   <- parplt[4] * height
  rangeX <- parusr[2] - parusr[1]
  rangeY <- parusr[4] - parusr[3]
  xClick <- as.numeric(xClick) + 0.5
  yClick <- as.numeric(yClick) + 0.5
  yClick <- height - yClick
  xPlotCoord <- parusr[1] + (xClick - xMin) * rangeX / (xMax - xMin)
  yPlotCoord <- parusr[3] + (yClick - yMin) * rangeY / (yMax - yMin)
  c(xPlotCoord, yPlotCoord, width, height, xClick, yClick)
}

w.tkrplot <- function(parent, w.plotfun, action = NA, mousedrag = NA, mouseup = NA,
                      hscale = 1, vscale = 1, pos = NULL, foreground = NULL, background = NULL,
                      margins = c(0, 0, 0, 0), name = paste("plot", .nc(), sep = ""), mar) {
    widget <- w.createwidget(parent, pos, NULL, tkrplottype = TRUE)
    widget$.type <- "tkrplot"
    plotter <- function() {
       par(mar = mar)
       w.plotfun()
       assign(paste(name, ".plt", sep = ""), par('plt'), envir = .rpenv)
       assign(paste(name, ".usr", sep = ""), par('usr'), envir = .rpenv)
    }
    if (is.null(foreground)) {
       # widget$.widget <- handshake(tkrplot::tkrplot, parent$.handle, plotter,
       #                             hscale = hscale, vscale = vscale)
       widget$.widget <- handshake(tkrplot.awb, parent$.handle, w.plotfun,
                                   name = name, hscale = hscale, vscale = vscale)
       w.setbackground(widget$.widget, background)
    }
    else {
       widget$.widget <- .w.tkrplot(parent$.handle, plotter, hscale = hscale, vscale = vscale, 
                             foreground = foreground, background = background,
                             margins = c(deparse(margins[1]), deparse(margins[2]),
                                         deparse(margins[3]),deparse(margins[4])))
    }  
    
    fdown <- function(x, y) {
       coords <- .w.coords(widget$.widget, x, y,
                           eval(parse(text = paste(name, ".plt", sep = "")), envir = .rpenv), 
                           eval(parse(text = paste(name, ".usr", sep = "")), envir = .rpenv)) 
       action(coords[1], coords[2]) 
    }
    fdrag <- function(x, y) {
       coords <- .w.coords(widget$.widget, x, y,
                           eval(parse(text = paste(name, ".plt", sep = "")), envir = .rpenv), 
                           eval(parse(text = paste(name, ".usr", sep = "")), envir = .rpenv))
       mousedrag(coords[1], coords[2])
    }
    fup <- function(x, y) {
       coords <- .w.coords(widget$.widget, x, y,
                           eval(parse(text = paste(name, ".plt", sep = "")), envir = .rpenv), 
                           eval(parse(text = paste(name, ".usr", sep = "")), envir = .rpenv))
       mouseup(coords[1], coords[2])
    }

    if (is.function(action))    handshake(tkbind, widget$.widget, "<Button-1>", fdown)
    if (is.function(mousedrag)) handshake(tkbind, widget$.widget, "<B1-Motion>", fdrag)
    if (is.function(mouseup))   handshake(tkbind, widget$.widget, "<ButtonRelease-1>", fup)
    handshake(tkconfigure, widget$.widget, cursor = "hand2")

    w.appearancewidget(widget, NULL, NULL, NULL)

  widget
}

tkrplot.awb <- function (parent, fun, name, hscale = 1, vscale = 1)  {
   fl <- tempfile(pattern = "tkrplotawb", fileext = ".png")
   on.exit(unlink(fl))
   png(filename = fl, width = 480 * hscale, height = 480 * vscale)
       # type = getVariable("tkRplotR_pngType"), 
   image <- try(fun(), silent = FALSE)
   assign(paste(name, ".plt", sep = ""), par('plt'), envir = .rpenv)
   assign(paste(name, ".usr", sep = ""), par('usr'), envir = .rpenv)
   if (inherits(image, "try-error")) {
      txt <- if (grepl("figure margins", image)) 'hscale may be too small.'
             else ''
      stop(txt)
   }
   dev.off()
   
   image <- handshake(tkimage.create, 'photo', file = fl)
   w.widget <- handshake(tkcanvas, parent,
                         width  = handshake(tcl, 'image', 'width',  image),
                         height = handshake(tcl, 'image', 'height', image))
   imageincanvas <- handshake(tkcreate, w.widget, 'image', 0, 0, image = image, anchor = 'nw')
   lab <- tklabel(parent, image = image)
   tkbind(lab, "<Destroy>", function() .Tcl(paste("image delete", image)))
   lab$image  <- image
   lab$fun    <- fun
   lab$hscale <- hscale
   lab$vscale <- vscale
   lab
}

rp.tkrreplot <- function(panel, name) {
  # if (is.na(charmatch("window", panel$panelname))) # if the panelname is not set then
  if (!exists(panel$panelname, .rpenv, inherits = FALSE)) # if the panelname is not set then
     panelname <- deparse(substitute(panel)) # the panel name should be the panel deparse subst'ed
  else 
     panelname <- panel$panelname
  tst <- try(is.character(name), silent = TRUE)
  if (inherits(tst, 'try-error'))
     name <- deparse(substitute(name))
  else
     if (!tst) name <- deparse(substitute(name))
  img  <- rp.widget.get(panelname, name)
  handshake(tkrreplot.awb, img$.widget)
  invisible(panelname)
}

tkrreplot.awb <- function(lab, fun = lab$fun, hscale = lab$hscale, vscale = lab$vscale) {
     fl <- tempfile(pattern = "tkrplotawb", fileext = ".png")
     on.exit(unlink(fl))
     png(fl, width = 480 * hscale, height = 480 * vscale)
         # type = getVariable("tkRplotR_pngType"), 
     err <- try(fun())
     # assign(paste(name, ".plt", sep = ""), par('plt'), envir = .rpenv)
     # assign(paste(name, ".usr", sep = ""), par('usr'), envir = .rpenv)
     if (inherits(err, "try-error")) stop()
     dev.off()
     handshake(tkimage.create, 'photo', lab$image, file = fl)
}
