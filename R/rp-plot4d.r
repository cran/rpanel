#     Plots of two covariates coloured by a response variable and
#     animated by a third covariate.

rp.plot4d <- function(x, z, y, model, group, subset,
                  col.palette, col.breaks, col.labels,
                  hscale = 1, vscale = hscale, panel = TRUE,
                  x1lab, x2lab, zlab, ylab,
						display = "image", Display = NULL,
                  background.plot = NULL, foreground.plot = NULL,
                  z.window = "normal", z.window.pars = c(min(z), sd(z)/5),
                  coords = rep(NA, 2), radius = 0.05, col.circle = "black", lwd.circle = 1,
                  location.plot = TRUE, retain.location.plot = FALSE,
                  group.level, group.name,
                  eqscplot = FALSE, location.plot.type = "histogram") {

   if (eqscplot & !requireNamespace("MASS", quietly = TRUE)) {
      cat("eqscplot requires the MASS package which is not available.\n")
      eqscplot <- FALSE
   }

   draw.plot <- function(panel) {
      with(panel, {

      	 z0  <- z.window.pars["location"]
      	 zsd <- z.window.pars["width"]

      	 if (display == "image") {
      	   par(mar = c(3, 3, 1, 1) + 0.1, mgp = c(1.5, 0.2, 0), tcl = -0.2)
            if (eqscplot)
               MASS::eqscplot(x, type = "n", xlab = x1lab, ylab = x2lab)
            else {
               # plot(x, type = "n", xlab = x1lab, ylab = x2lab)
               plot(x[ , 1], x[ , 2], type = "n", axes = FALSE, xlab = x1lab, ylab = x2lab)
               usr <- par("usr")
               rect(usr[1], usr[3], usr[2], usr[4], col = grey(0.9), border = NA)
               grid(col = "white", lty = 1)
               axis(1, lwd = 0, lwd.ticks = 2, col = grey(0.6), col.ticks = grey(0.6),
                       col.axis = grey(0.6), cex.axis = 0.8)
               axis(2, lwd = 0, lwd.ticks = 2, col = grey(0.6), col.ticks = grey(0.6),
                       col.axis = grey(0.6), cex.axis = 0.8)
            }
      	 }

      	 if (is.list(model) & (Display["model"] | ("reference" %in% names(model)))) {
            if (length(dim(model$y)) == 4) {
         	     model$y <- model$y[ , , , which(group.level == levels(group))]
         	     if ("reference" %in% names(model))
         	        model$reference <- model$reference[ , , , which(group.level == levels(group))]
            }
         	mt     <- dim(model$y)[3]
            m.ind  <- (z0 - min(model$z)) / (diff(range(model$z)) / (mt - 1))
            m.low  <- min(1 + floor(m.ind), mt)
            m.high <- min(1 + m.low, mt)
            m.p    <- 1 + m.ind - m.low
            if (m.low >= 1 & m.high <= mt) {
               my     <- (1 - m.p) * model$y[ , , m.low] + m.p * model$y[ , , m.high]
               my.ind <- !is.na(my)
               grdx <- model$x[ , 1]
               grdy <- model$x[ , 2]
   	           dfrm <- data.frame(x = rep(grdx, length(grdy)), y = rep(grdy, each = length(grdx)), z = c(my))
   	           if ("reference" %in% names(model)) {
                  mr     <- (1 - m.p) * model$reference[ , , m.low] + m.p * model$reference[ , , m.high]
                  mr.ind <- !is.na(mr)
                  dfrm$r <- c(mr)
               }
   	         if (display == "image" & requireNamespace("interp", quietly = TRUE)) {
   	            sbst   <- apply(as.matrix(dfrm), 1, function(x) !any(is.na(x)))
   	            dfrm   <- dfrm[sbst, ]
   	            grdx   <- seq(min(dfrm$x), max(dfrm$x), length = 200)
   	            grdy   <- seq(min(dfrm$y), max(dfrm$y), length = 200)
   	            intrpz <- interp::interp(dfrm$x, dfrm$y, dfrm$z, grdx, grdy)
  	               if ("reference" %in% names(model)) {
  	                 intrpr <- interp::interp(dfrm$x, dfrm$y, dfrm$r, grdx, grdy)
  	               }
  	               dfrm <- list(x = grdx, y = grdy, z = intrpz$z)
  	               if ("reference" %in% names(model)) dfrm$r <- intrpr$z
  	           }
  	           else {
  	              dfrm   <- as.list(dfrm)
  	              dfrm$z <- matrix(dfrm$z, nrow = dim(model$y)[1])
  	           }
   	           if (is.list(model) & Display["model"]) {
   	              all.y <- model$y
   	              if (!missing.y) all.y <- c(all.y, y)
                  brks[is.infinite(brks) & (brks > 0)] <- max(all.y, na.rm = TRUE) + 1
                  brks[is.infinite(brks) & (brks < 0)] <- min(all.y, na.rm = TRUE) - 1
                  # image(mx[ , 1], mx[ , 2], my, breaks = brks, col = col.palette, add = TRUE)
                  if (display == "image") {
  	                image(grdx, grdy, dfrm$z, breaks = brks, col = col.palette, add = TRUE)
                  }
  	              else if (display == "persp") {
  	                 ngrid <- dim(dfrm$z)[1]
  	                 clr   <- array(c(dfrm$z[-ngrid, -ngrid], dfrm$z[    -1, -ngrid],
                                      dfrm$z[-ngrid,     -1], dfrm$z[    -1,     -1]),
                                      dim = c(ngrid - 1, ngrid - 1, 4))
                     clr   <- apply(clr, 1:2, function(x)
                                    if (length(which(is.na(x))) > 1) NA else mean(x, na.rm = TRUE))
                     clr   <- col.palette[cut(c(clr), brks, labels = FALSE)]
  	                 persp(grdx, grdy, dfrm$z, col = clr, ticktype = "detailed", d = 10,
     	                     xlab = x1lab, ylab = x2lab, zlab = ylab, theta = theta, phi = phi, zlim = range(brks))
  	              }
   	           }
               if (is.list(model) && (("reference" %in% names(model)) && Display["reference"])) {
               	  lvls <- pretty(c(2, max(c(2, dfrm$r), na.rm = TRUE)))
               	  mmx <- max(c(2, dfrm$r), na.rm = TRUE)
               	  if (mmx >= 2) {
               	     lvls <- if (trunc(mmx) > 5) pretty(c(2, trunc(mmx))) else 2:trunc(mmx)
               	     # contour(mx[ , 1], mx[ , 2], mr, add = TRUE, col = "blue", levels = lvls, lty = 1)
               	     if (display == "image")
               	        contour(grdx, grdy, matrix(dfrm$r, ncol = length(grdx)),
               	                add = TRUE, col = "blue", levels = lvls, lty = 1)
               	  }
               	  lvls <- pretty(c(-2, min(c(-2, mr), na.rm = TRUE)))
               	  mmn <- min(c(-2, dfrm$r), na.rm = TRUE)
               	  if (mmn <= -2) {
               	     lvls <- if (trunc(mmn) < -5) pretty(c(-2, trunc(mmn))) else (-2):trunc(mmn)
               	     # contour(mx[ , 1], mx[ , 2], mr, add = TRUE, col = "blue", levels = lvls, lty = 2)
               	     if (display == "image")
               	        contour(grdx, grdy, matrix(dfrm$r, , ncol = length(grdx)),
               	                add = TRUE, col = "blue", levels = lvls, lty = 2)
               	  }
               }
            }
         }

         if (is.function(background.plot)) background.plot()

      	 if ((display == "image") & ((!is.list(model) | (is.list(model) & !Display["model"] & Display["points"])))) {
      	 	  if (is.list(model)) z.window <- "uniform"
            zsd1  <- if (zsd >= 1.49 * sdz) 4 * sdz else zsd
            alpha <- exp(-0.5 * (z - z0)^2 / zsd1^2)
            ord   <- order(alpha)
            # if (z.window == "normal")
               # clr <- hsv(clr[1, ], clr[2, ] * alpha, clr[3, ])
            # else if (z.window == "uniform") {
               # clr <- hsv(clr[1, ], clr[2, ], clr[3, ])
               # ord <- ord[abs(z[ord] - z0) < 2 * zsd1]
            # }
            if (z.window == "normal") {
               if ((.Platform$OS.type == "unix") & all(col.palette == "black")) {
            		  clr <- grey(0.9 * (1 - alpha))
            		  clr[alpha < 0.05] <- NA
               }
               else {
                  clr <- rbind(sapply(clr, col2rgb) / 255, alpha)
                  clr <- apply(clr, 2, function(x) rgb(x[1], x[2], x[3], alpha = x[4]))
               }
            }
            else if (z.window == "uniform")
               ord <- ord[abs(z[ord] - z0) < 2 * zsd1]
            xord <- x[ord, ]
            cord <- clr[ord]
            if (nlevels(group) > 1) {
               xord <- xord[group[ord] == group.level, ]
               cord <- cord[group[ord] == group.level]
            }
            if (is.list(model)) points(xord[ , 1], xord[ , 2])
            points(xord[ , 1], xord[ , 2], pch = 16, col = cord)
            if (!is.null(foreground.plot) && is.character(foreground.plot) && foreground.plot == "regression") {
            	 xy   <- xord[ , 2]
            	 xx   <- xord[ , 1]
            	 xg   <- seq(min(xx), max(xx), length = 50)
            	 smth <- loess(xy ~ xx, weights = alpha[ord])
            	 prd  <- predict(smth, data.frame(xx = xg))
            	 lines(xg, prd, col = "blue", lwd = 2)
            }
      	 }

      	 if ((display == "image") & is.function(foreground.plot)) foreground.plot()

         if (all(!is.na(coords))) {
         	dr1          <- diff(range(panel$x[ , 1]))
   	        dr2          <- diff(range(panel$x[ , 2]))
   	        if (eqscplot) {
   	           dr1 <- max(dr1, dr2)
   	           dr2 <- max(dr1, dr2)
   	        }
            lines(coords[1] + circle[ , 1] * radius * dr1,
                  coords[2] + circle[ , 2] * radius * dr2, col = col.circle, lwd = lwd.circle)
         }
      })
      panel
   }

   draw.key <- function(panel) {
   	  if (panel$missing.y) return(panel)
      if (is.factor(panel$y)) {
      	 par(mar = c(3, 0, 1, 0) + 0.1)
      	 plot(0:1, type = "n", axes = FALSE, xlab = "", ylab = "")
      	 for (i in 1:length(levels(panel$y)))
      	    text(1, 1 - i * 1.5 * strheight("A"), levels(panel$y)[i],
      	         col = panel$col.palette[i], pos = 4, offset = 0)
      	 # legend("topleft", levels(panel$y), # col = panel$col.palette,
         #    text.col = panel$col.palette)
      }
      else {
         rp.colour.key(panel$col.palette, panel$col.labels, par.mar = c(3, 1, 1, 1.5) + 0.1,
             natural = panel$natural)
         mtext(ylab, side = 2, line = 0.1, font = 1)
      }
      panel
   }

   draw.band <- function(panel) {
      with(panel, {
      	 z0      <- z.window.pars["location"]
      	 zsd     <- z.window.pars["width"]
      	 z0      <- z.window.pars[1]
      	 zsd     <- z.window.pars[2]
         # par(mar = c(0, 3, 2, 1) + 0.1, mgp = c(1, 0.2, 0), tcl = -0.2)
         # plot(range(z), c(0, 1), type = "n", axes = FALSE, xlab = "", ylab = "",
         #       xaxs = "i", yaxs = "i")
         par(mar = c(0, 3, 2, 1) + 0.1, mgp = c(1.5, 0.2, 0), tcl = -0.2)
         plot(range(z), c(0, 1), type = "n", axes = FALSE, yaxs = "i", xlab = " ", ylab = " ")
         zsd1  <- if (zsd >= 1.49 * sdz) 4 * sdz else zsd
         if (is.list(model)) z.window <- "uniform"
         if (z.window == "normal") {
            nrect <- 100
            zvec  <- seq(par()$usr[1], par()$usr[2], length = nrect + 1)
            zmid  <- (zvec[-nrect + 1] + zvec[-1]) / 2
            alpha <- exp(-0.5 * (zmid - z0)^2 / zsd1^2)
            clr   <- rgb2hsv(col2rgb("lightblue"))
            clr   <- hsv(rep(clr[1, ], nrect), clr[2, ] * alpha, rep(clr[3, ], nrect))
            rect(zvec[-(nrect + 1)], 0, zvec[-1], 1, col = clr, border = NA)
         }
         else if (z.window == "uniform") {
         	usr <- par("usr")
            rect(usr[1], 0, usr[2], 1, col = grey(0.9), border = NA)
            rect(z0 - 2 * zsd1, 0, z0 + 2 * zsd1, 1, col = "lightblue",   border = NA)
            lines(rep(z0, 2), c(0, 1), col = grey(0.9))
         }
         axis(3, font.main = 1,
              col = grey(0.6), col.ticks = grey(0.6), col.axis = grey(0.6), cex.axis = 0.8)
         mtext(zlab, line = 1, font = 1)
         box()
      })
      panel
   }

   draw.location <- function(panel) {
      with(panel, {
         if (missing.y | is.factor(y)) {
            par(mar = c(3, 3, 1, 1) + 0.1, mgp = c(1.5, 0.2, 0), tcl = -0.2)
            if (missing.y) y <- factor(rep(1, length(z)))
            if (length(z[locind]) > 0) {
               gpind <- group[locind]
               ind   <- (gpind == group.level)
               if (nlevels(y) > 1 & requireNamespace("lattice", quietly = TRUE)) {
                  if (location.plot.type == "histogram")
                     print(lattice::histogram( ~ z[locind][ind] | y[locind][ind],
                                  xlab = zlab, xlim = range(z),
                                  layout = c(1, nlevels(y)), type = "count"))
                  else
                     print(lattice::densityplot( ~ z[locind][ind] | y[locind][ind],
                                  xlab = zlab, xlim = range(z),
                                  layout = c(1, nlevels(y)), type = "count"))
               }
               else {
               	  if (location.plot.type == "histogram") {
                     hist(z[locind][ind], main = "", xlab = zlab, xlim = range(z))
                     box()
                  }
                  else
                     print(lattice::densityplot( ~ z[locind][ind],
                            xlab = zlab, xlim = range(z), type = "count"))
              }
            }
            else {
               plot(z, y, type = "n", axes = FALSE, xlab = zlab, ylab = "")
               axis(1)
               box()
            }
         }
         else {
            # par(mar = c(3, 0.2, 1, 1) + 0.1, mgp = c(1.5, 0.2, 0), tcl = -0.2)
            # plot(z, y, type = "n", axes = FALSE, xlab = zlab, ylab = "")
            # axis(2, labels = FALSE)
            # axis(1)
            # box()
            par(mar = c(3, 0.2, 1, 1) + 0.1, mgp = c(1.5, 0.2, 0), tcl = -0.2)
            plot(range(z), range(col.labels), type = "n", axes = FALSE, xlab = zlab, ylab = ylab)
            usr <- par("usr")
            rect(usr[1], usr[3], usr[2], usr[4], col = grey(0.9), border = NA)
            grid(col = "white", lty = 1)
            axis(1, lwd = 0, lwd.ticks = 2, col = grey(0.6), col.ticks = grey(0.6),
                    col.axis = grey(0.6), cex.axis = 0.8)
            axis(2, lwd = 0, lwd.ticks = 2, col = grey(0.6), col.ticks = grey(0.6),
                    col.axis = grey(0.6), cex.axis = 0.8, labels = FALSE)
            gpind <- group[locind]
            ind   <- (gpind == group.level)
            points(z[locind][ind], y[locind][ind], pch = 16)
         }
      })
      # if (all(!is.na(panel$coords)))
      #    rp.text.change(panel, "textpane",
      #       paste("\n              location: (",
      #             signif(panel$coords[1], 5), ", ",
      #             signif(panel$coords[2], 5), ")", sep = ""))
      #       #       ")        radius: ", signif(panel$radius, 5), sep = ""))
      # else
      #    rp.text.change(panel, "textpane", "\n\n")
      panel
   }

   click <- function(panel, x, y) {
   	  dr1          <- diff(range(panel$x[ , 1]))
   	  dr2          <- diff(range(panel$x[ , 2]))
   	  if (eqscplot) {
   	     dr1 <- max(dr1, dr2)
   	     dr2 <- max(dr1, dr2)
   	  }
      d.pts        <- ((panel$x[ , 1] - x) / dr1)^2 + ((panel$x[ , 2] - y) / dr2)^2
      panel$locind <- which(d.pts <= panel$radius^2)
      panel$coords <- c(x, y)
      panel$zsdold <- panel$z.window.pars["width"]
      panel$z.window.pars["width"] <- panel$sdz * 4
      rp.control.put(panel$panelname, panel)
      rp.tkrreplot(panel, plot)
      rp.tkrreplot(panel, band)
      if (panel$location.plot.showing)
         rp.tkrreplot(panel, location)
      else {
         # rp.text(panel, "\n\n", name = "textpane", grid = "plots",
         #    row = 0, column = 1 + as.numeric(!panel$missing.y),
         #    sticky = "news", background = "white", fontsize = 12)
         rp.tkrplot(panel, location, draw.location, grid = "plots",
            row = 1, column = 1 + as.numeric(!panel$missing.y),
            hscale = panel$hscale, vscale = panel$vscale, background = "white")
      }
      panel$location.plot.showing <- TRUE
      panel
   }

   drag <- function(panel, x, y) {
   	  dr1          <- diff(range(panel$x[ , 1]))
   	  dr2          <- diff(range(panel$x[ , 2]))
   	  if (eqscplot) {
   	     dr1 <- max(dr1, dr2)
   	     dr2 <- max(dr1, dr2)
   	  }
      d.pts        <- ((panel$x[ , 1] - x) / dr1)^2 + ((panel$x[ , 2] - y) / dr2)^2
      panel$locind <- which(d.pts <= panel$radius^2)
      panel$coords <- c(x, y)
      rp.control.put(panel$panelname, panel)
      rp.tkrreplot(panel, plot)
      rp.tkrreplot(panel, location)
      panel
   }

   release <- function(panel, x, y) {
      if (!panel$retain.location.plot) {
         panel$z.window.pars["width"] <- panel$zsdold
         panel$locind <- integer(0)
         panel$coords <- rep(NA, 2)
         rp.control.put(panel$panelname, panel)
         rp.widget.dispose(panel, "location")
         rp.tkrreplot(panel, plot)
         # rp.tkrreplot(panel, location)
         rp.tkrreplot(panel, band)
         # rp.widget.dispose(panel, textpane)
         panel$location.plot.showing <- FALSE
      }
      panel
   }

   plot.3d <- function(panel) {
      with(panel, {
         rp.plot3d(x[ , 1], x[ , 2], z, col = colour, xlab = x1lab, ylab = x2lab, zlab = zlab)
      })
      panel
   }

   redraw4d <- function(panel) {
   	  if (panel$location.plot.showing) {
   	     if (panel$retain.location.plot)
            rp.do(panel, click, panel$coords[1], panel$coords[2])
         else {
            rp.do(panel, release, panel$coords[1], panel$coords[2])
            panel$location.plot.showing <- FALSE
            panel$coords <- rep(NA, 2)
         }
   	  }
   	  else {
         rp.tkrreplot(panel, plot)
         rp.tkrreplot(panel, band)
      }
      panel
   }

   missing.y <- missing(y)
   if (missing(model)) model <- NULL
   xlab <- deparse(substitute(x))
   if (missing(x1lab)) {
   	if (!is.null(colnames(x)[1]))
   		x1lab <- colnames(x)[1]
   	else
   		x1lab <- paste(xlab, "1", sep = "-")
   }
   if (missing(x2lab)) {
   	if (!is.null(colnames(x)[2]))
   		x2lab <- colnames(x)[2]
   	else
   		x2lab <- paste(xlab, "2", sep = "-")
   }
   if (missing(zlab)) zlab <- deparse(substitute(z))
   if ((.Platform$OS.type == "unix") & panel) {
   	  if (missing.y & !is.list(model))  col.palette <- "black"
   	  if (!missing.y) z.window    <- "uniform"
   }

   if (!missing.y) {
   	  if (missing(ylab)) ylab <- deparse(substitute(y))
   }
   else {
   	  y <- factor(rep(1, length(z)))
   	  ylab <- ""
   }
   if (missing(group))
   	  group <- factor(rep(1, length(y)))
   else
   	  if (!is.factor(group)) stop("group is not a factor.")
   if (missing(group.level)) group.level <- levels(group)[1]
   if (missing(group.name))  group.name <- deparse(substitute(group))

   if (!missing(subset)) {
   	  x     <- x[subset, ]
   	  z     <- z[subset]
   	  y     <- y[subset]
   	  group <- group[subset]
   }

   panel.flag <- panel

   if (is.data.frame(x)) x <- as.matrix(x)
   if (!is.matrix(x) && ncol(x) == 2) stop("x should be a two-column matrix")
   w   <- cbind(x, z)
   ind <- apply(w, 1, function(x) any(is.na(x)))
   x   <- x[!ind, ]
   z   <- z[!ind]
   if (!missing.y) {
      y <- y[!ind]
      if (!all(is.na(y))) {
         w   <- cbind(x, z, y)
         ind <- apply(w, 1, function(x) any(is.na(x)))
         x   <- x[!ind, ]
         z   <- z[!ind]
         y   <- y[!ind]
      }
   }

   brks    <- NA
   natural <- NA
   missing.col.labels <- missing(col.labels)
   if (missing.col.labels) col.labels <- NA
   key <- 0.25
   if (missing.y & !is.list(model)) {
      ind         <- rep(1, length(z))
      if (!(all(col.palette == "black"))) col.palette <- "blue"
   }
   else if (is.factor(y) & !missing.y) {
      if (missing(col.palette) || all(is.na(col.palette)))
          col.palette <- topo.colors(nlevels(y))
      ind <- as.numeric(y)
   }
   else {
      if (missing(col.palette) || all(is.na(col.palette)))
          col.palette <- topo.colors(20)
      if (!missing(col.breaks)) {
          if (length(col.breaks) != length(col.palette) + 1)
             stop("the length of col.breaks should be length(col.palette) + 1.")
      	  brks <- col.breaks
      }
      else {
      	 all.y <- if (is.list(model)) model$y else numeric(0)
      	 if (!missing.y) all.y <- c(all.y, y)
         rng   <- range(all.y, na.rm = TRUE)
         del   <- 0.04 * diff(rng)
         brks  <- seq(rng[1] - del, rng[2] + del, length = length(col.palette) + 1)
         # brks <- seq(rng[1], rng[2], length = length(col.palette) + 1)
      }
      natural <- missing.col.labels
      if (natural) col.labels <- brks
      if (!missing.y) {
         ind  <- if (all(is.na(y))) rep(1, length(y)) else cut(y, brks, labels = FALSE)
      }
      key     <- 0.15
   }
   colour <- if (!missing.y) col.palette[ind] else rep("black", length(y))
   clr    <- col2rgb(colour)
   clr    <- rgb2hsv(clr)
   clr    <- colour
   # rad    <- mean(apply(x, 2, function(w) diff(range(w)))) / 20
   theta  <- seq(0, 2 * pi, length = 50)
   circle <- matrix(c(cos(theta), sin(theta)), ncol = 2)
   n      <- length(z)

   if (all(is.null(Display))) {
      Display <- if (!all(is.na(y))) c("points" = TRUE) else NULL
      if (!is.null(model)) {
         Display <- c(Display, "model" = TRUE)
         if ("reference" %in% names(model)) Display <- c(Display, "reference" = TRUE)
      }
   }
   else
      names(Display) <- c("points", "model", "reference")[1:length(Display)]

   if (!is.null(model)) {
      if (!is.list(model)) {
         cat("model is not a list and will not be used.\n")
         model <- NULL
      }
      else if (nlevels(group) == 1 & length(dim(model$y)) != 3) {
         cat("model$y is not a three-dimensional array and will not be used.\n")
         model <- NULL
      }
      else if (nlevels(group) >  1 & length(dim(model$y)) != 4) {
         cat("model$y is not a four-dimensional array and will not be used.\n")
         model <- NULL
      }
   }

   if (all(is.na(y))) location.plot <- FALSE

   names(z.window.pars) <- c("location", "width")

   if (panel.flag) {
      panel <- rp.control(x = x, y = y, z = z, missing.y = missing.y,
                  x1lab = x1lab, x2lab = x2lab, ylab = ylab, zlab = zlab,
                  model = model, brks = brks,
                  col.palette = col.palette, col.labels = col.labels, natural = natural,
                  coords = rep(NA, 2), radius = radius, circle = circle,
                  col.circle = col.circle, lwd.circle = lwd.circle, n = n, sdz = sd(z),
                  z.window = z.window, z.window.pars = z.window.pars,
                  colour = colour, clr = clr, hscale = hscale, vscale = vscale,
                  location.plot.showing = FALSE,
                  retain.location.plot = retain.location.plot,
                  group = group, group.level = group.level, group.name = group.name,
                  eqscplot = eqscplot, locind = integer(0),
                  background.plot = background.plot, foreground.plot = foreground.plot,
                  Display = Display, display = display, theta = -30, phi = 40,
                  panel.plot = TRUE)
      rp.grid(panel, "controls", row = 0, column = 0, sticky = "n", background = "grey")
      rp.grid(panel, "plots",    row = 0, column = 1, background = "white")
      rp.tkrplot(panel, band, draw.band,
                hscale = hscale, vscale = 0.12 * vscale,
                grid = "plots", row = 0, column = 0, background = "white")
      if (location.plot)
         rp.tkrplot(panel, plot, draw.plot, click, drag, release,
                hscale = hscale, vscale = vscale,
                grid = "plots", row = 1, column = 0, background = "white")
      else
         rp.tkrplot(panel, plot, draw.plot,
                hscale = hscale, vscale = vscale,
                grid = "plots", row = 1, column = 0, background = "white")
      if (!missing.y)
         rp.tkrplot(panel, key,  draw.key, hscale = key * hscale, vscale = vscale,
                grid = "plots", row = 1, column = 1, background = "white")
      rp.slider(panel, z.window.pars, c(min(z), sd(z) / 20), c(max(z), sd(z) * 1.5), redraw4d,
                labels = c("centre", "width"),
                title = paste(zlab, "window"),
                grid = "controls", row = 0, column = 0)
      if (is.null(model) & (.Platform$OS.type != "unix"))
         rp.radiogroup(panel, z.window, c("normal", "uniform"), action = redraw4d,
                grid = "controls", row = 1, column = 0, title = paste(zlab, "window shape"))
      if (location.plot) {
         rp.slider(panel, radius, 0.05 / 5,  0.05 * 5, redraw4d,
                title = paste(xlab, "window radius"),
                grid = "controls", row = 2, column = 0)
         rp.checkbox(panel, retain.location.plot, redraw4d, "Retain location plot",
                grid = "controls", row = 3, column = 0)
         if (is.factor(y) & requireNamespace("lattice", quietly = TRUE)) {
            rp.radiogroup(panel, location.plot.type, c("histogram", "density"), action = redraw4d,
                grid = "controls", row = 4, column = 0, title = "location plot type")
         }
      }
      if (!is.null(model)) {
      	 disp <- if (!all(is.na(y))) "points" else NULL
      	 disp <- c(disp, "model")
      	 if ("reference" %in% names(model)) disp <- c(disp, "reference")
      	 disp <- names(Display)
      	 if (length(disp) > 1)
            rp.checkbox(panel, Display, redraw4d, disp,
                grid = "controls", row = 5, column = 0, title = "location plot type")
          rp.radiogroup(panel, display, c("image", "persp"), action = redraw4d,
      						grid = "controls", row = 6, column = 0, title = "Display type")
          rp.slider(panel, theta, -180, 180, redraw4d, title = "persp left/right", grid = "controls", row = 8, column = 0)
          rp.slider(panel, phi,      0,  90, redraw4d, title = "persp up/down", grid = "controls", row = 9, column = 0, )
      }
      if (nlevels(group) > 1) {
         rp.radiogroup(panel, group.level, levels(group), action = redraw4d,
                grid = "controls", row = 6, column = 0, title = group.name)
      }
      if (requireNamespace("rgl", quietly = TRUE) & !all(is.na(y))) {
         rp.button(panel, plot.3d, "3D plot", grid = "controls", row = 10, column = 0)
      }
   }
   else {
      panel <- list(x = x, y = y, z = z, missing.y = missing.y,
                  x1lab = x1lab, x2lab = x2lab, ylab = ylab, zlab = zlab,
                  model = model, brks = brks, natural = natural,
                  col.palette = col.palette, brks = brks, col.labels = col.labels,
                  coords = coords, radius = radius, circle = circle,
                  col.circel = col.circle, lwd.circle = lwd.circle, n = n, sdz = sd(z),
                  colour = colour, clr = clr, eqscplot = eqscplot,
                  z.window = z.window, z.window.pars = z.window.pars,
                  group = group, group.level = group.level, group.name = group.name,
                  Display = Display, display = display, theta = -30, phi = 40,
                  background.plot = background.plot, foreground.plot = foreground.plot,
                  panel.plot = FALSE)
      if (all(!is.na(coords))) {
   	     dr1          <- diff(range(panel$x[ , 1]))
   	     dr2          <- diff(range(panel$x[ , 2]))
   	     if (eqscplot) {
   	        dr1 <- max(dr1, dr2)
   	        dr2 <- max(dr1, dr2)
   	     }
         d.pts        <- ((panel$x[ , 1] - panel$coords[1]) / dr1)^2 +
                         ((panel$x[ , 2] - panel$coords[2]) / dr2)^2
         panel$locind <- which(d.pts <= panel$radius^2)
         panel$coords <- c(panel$coords[1], panel$coords[2])
         panel$zsdold <- panel$z.window.pars["width"]
         panel$z.window.pars["width"] <- panel$sdz * 4
         layout(matrix(c(2, 3, 5, 4, 6, 1), ncol = 3), widths = c(8, 1, 8), heights = c(1, 8))
         draw.location(panel)
      }
      else
        layout(matrix(c(1, 2, 4, 3), ncol = 2), widths = c(8, 1), heights = c(1, 8))
      draw.band(panel)
      draw.plot(panel)
      draw.key(panel)
      layout(1)
   }

   invisible()
}

   rp.spacetime <- function(space, time, y, model, group, subset,
                  col.palette, col.breaks, col.labels,
                  hscale = 1, vscale = hscale, panel = TRUE,
                  x1lab, x2lab, zlab, ylab,
						display = "image", Display = NULL,
						background.plot = NULL, foreground.plot = NULL,
                  time.window = "normal",
                  time.window.pars = c(min(time), sd(time)/5),
                  coords = rep(NA, 2), radius = 0.05, col.circle = "black", lwd.circle = 1,
                  location.plot = TRUE, retain.location.plot = FALSE,
                  group.level, group.name,
                  eqscplot = TRUE, location.plot.type = "histogram") {

   xlab <- deparse(substitute(space))
   if (!is.null(colnames(space)[1]))
      x1lab <- colnames(space)[1]
   else
      x1lab <- paste(xlab, "1", sep = "-")
   if (!is.null(colnames(space)[2]))
      x2lab <- colnames(space)[2]
   else
      x2lab <- paste(xlab, "2", sep = "-")
   if (missing(zlab)) zlab <- deparse(substitute(time))
   missing.y <- missing(y)
   if (!missing.y) {
      if (missing(ylab)) ylab <- deparse(substitute(y))
   }
   else {
   	  y        <- jitter(rep(1, length(time)))
   	  ylab     <- ""
   }
   if (missing(model))   model   <- NULL
   if (missing(Display)) Display <- NULL

      rp.plot4d(space, time, y, model, group, subset,
                  col.palette = col.palette, col.breaks = col.breaks, col.labels = col.labels,
                  hscale = hscale, vscale = vscale, panel = panel,
                  x1lab = x1lab, x2lab = x2lab, zlab = zlab, ylab = ylab,
   					display = display, Display = Display,
                  background.plot = background.plot, foreground.plot = foreground.plot,
                  z.window = time.window, z.window.pars = time.window.pars,
                  coords = coords, radius = radius, location.plot = location.plot,
                  retain.location.plot = retain.location.plot, eqscplot = eqscplot,
                  group.level = group.level, group.name = group.name,
                  location.plot.type = location.plot.type)
}
