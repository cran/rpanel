#     rpanel function for simple analysis of variance

rp.anova <- function(y, x, z, model = NA, model0 = NA,
                     ylab = NA, xlab = NA, zlab = NA, title = NULL, lines = TRUE,
                     panel = TRUE, panel.plot = TRUE, hscale = 1.3, vscale = hscale / 1.3) {

   # The denstrip package has direct references to lattice, so lattice needs to be loaded.
   # An alternative is to use the code at the end of this file which copies the denstrip functions.
   if (!requireNamespace("lattice")) stop("the lattice package is not available.")
   if (!requireNamespace("denstrip", quietly = TRUE)) stop("the denstrip package is not available.")
   if (!requireNamespace("colorspace", quietly = TRUE)) stop("the colorspace package is not available.")
  
   type <- if (missing(z)) "One-way" else "Two-way"

   if (is.na(ylab)) ylab <- deparse(substitute(y))
   if (is.na(xlab)) xlab <- deparse(substitute(x))
   zlab <- if (type == "One-way") "" else deparse(substitute(z))
   xterm <- xlab
   zterm <- zlab

   if (type == c("One-way")) z <- rep(1, length(x))
   x <- as.factor(x)
   z <- as.factor(z)

   ind <- !is.na(x) & !is.na(y) & !is.na(z) 
   x <- x[ind]
   y <- y[ind]
   z <- z[ind]
   if (length(x) < 5) stop("too few non-missing data.")
 
   graphics <- "strip plot"
   jitter.x <- jitter(as.numeric(x), factor = 0.5, amount = NULL)
   
   if (is.na(hscale)) {
      if (.Platform$OS.type == "unix") hscale <- 1
      else                             hscale <- 1.4
      }
   if (is.na(vscale)) 
      vscale <- hscale

   rp.anova.draw <- function(panel) {
   	
      if (panel$type == "Two-way") {
         panel$model  <- c(panel$model11, panel$model12, panel$model13, panel$model14)
         panel$model0 <- c(panel$model01, panel$model02, panel$model03, panel$model04)
      }
      else {
         panel$model  <- c(panel$model11, panel$model12)
         panel$model0 <- c(panel$model01, panel$model02)
      }
   	  panel$model.check  <- any(panel$model)
   	  panel$model0.check <- any(panel$model0)
      if (!panel$model[1] & any(panel$model[-1])) {
         rp.messagebox("The overall mean must be included if other terms are present.")
         panel$model.check <- FALSE
      }
      if (panel$type == "Two-way" & panel$model[4] & !all(panel$model[2:3])) {
         rp.messagebox("The main effects must be included if the interaction term is present.")
         panel$model.check <- FALSE
      }
      if (any(panel$model) & !panel$model0[1] & any(panel$model0[-1])) {
         rp.messagebox("The overall mean must be included if other terms are present",
                       "in the new model.")
         panel$model0.check <- FALSE
      }
      if (any(panel$model) & panel$type == "Two-way" & panel$model0[4] & !all(panel$model0[2:3])) {
         rp.messagebox("The main effects must be included if the interaction term",
                       "is present in the new model.")
         panel$model0.check <- FALSE
      }
      
      form <- "y ~ 1"
      trms <- panel$term.names[panel$model[-1]]
      if (length(trms) > 0)
         for (i in 1:length(trms))
            form <- paste(form, trms[i], sep = " + ")
      mdl <- lm(as.formula(form), na.action = na.exclude)
      panel$df1 <- mdl$df.residual
      panel$sigma <- summary(mdl)$sigma
            
      form <- "y ~ 1"
      trms <- panel$term.names[panel$model0[-1]]
      if (length(trms) > 0)
         for (i in 1:length(trms))
            form <- paste(form, trms[i], sep = " + ")
      mdl0 <- lm(as.formula(form), na.action = na.exclude)
      panel$df0 <- mdl0$df.residual
            
      rss1        <- sum(mdl$residuals^2)
      rss0        <- sum(mdl0$residuals^2)
      panel$fstat <- (abs(rss0 - rss1) / abs(panel$df1 - panel$df0)) / panel$sigma^2

      with(panel, {
      	
         form <- if (type == "Two-way") "y ~ x | z" else "y ~ x"
         form <- as.formula(form)
         ngps <- nrow(unique(data.frame(x, z)))
         if (graphics != "boxplot") {
            clr  <- colorspace::rainbow_hcl(3)
         	dfrm <- data.frame(x, y, z, jitter.x)
           	plt  <- ggplot2::ggplot(dfrm, ggplot2::aes(y, x)) + ggplot2::xlab(ylab) + ggplot2::ylab(xlab) +
         	          ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
         	                panel.grid.minor = ggplot2::element_blank(),
         	                panel.background = ggplot2::element_rect(fill = "grey90")) +
         	  	     ggplot2::ggtitle(ttl)
            if (!(model0.check & model.check) | all(model == model0)) {
               plt <- plt + ggplot2::stat_density(ggplot2::aes(fill = ..density..), geom = "tile",
                            height = 0.7, position = "identity", show.legend = FALSE) +
		         ggplot2::scale_fill_gradient(low = "grey90", high = clr[3])
            }
            else if (model.check & !all(model == model0)) {
               mn    <- tapply(fitted(mdl0), list(x, z), mean)
               se    <- tapply(fitted(mdl0), list(x, z), length)
               se    <- summary(mdl)$sigma * sqrt(abs(df0 - df1)) / sqrt(se * ngps)
               dfrm1 <- data.frame(y = c(mn), x = rep(rownames(mn), ncol(mn)),
                                   z = rep(colnames(mn), each = nrow(mn)))
               ngrid <- 200
               xgrid <- seq(min(y), max(y), length = ngrid)
               dfrm1 <- data.frame(xgrid = rep(xgrid, each = nrow(dfrm1)),
                                   x = rep(dfrm1$x, ngrid), z = rep(dfrm1$z, ngrid))
               dfrm1$dgrid <- dnorm(dfrm1$xgrid, mn[cbind(dfrm1$x, dfrm1$z)],
                                    se[cbind(dfrm1$x, dfrm1$z)])
               plt <- plt + ggplot2::geom_tile(ggplot2::aes(x = xgrid, y = x, fill = dgrid),
                                      height = 0.8, data = dfrm1,
                                      show.legend = FALSE) +
                  	 ggplot2::scale_fill_gradient(low = "grey90", high = clr[2])
            }
        	   if (model.check) {
               plt <- plt + ggplot2::stat_summary(ggplot2::aes(x = fitted(mdl)), width = 0,
                               fun.y = "mean", size = 1,
          	     				    fun.ymin = function(x) mean(x) - 0.45,
          	     				    fun.ymax = function(x) mean(x) + 0.45,
          						    geom = "crossbar", col = clr[1])
        	   }
          	# plt <- plt + ggplot2::geom_jitter(width = 0, height = 0.1, col = clr[3])
          	plt <- plt + ggplot2::geom_point(ggplot2::aes(x = y, y = jitter.x), col = clr[3])
        	   plt <- plt + ggplot2::coord_flip()
         	if (type == "Two-way") plt <- plt + ggplot2::facet_grid(. ~ z)
            print(plt)
         }
         else if (graphics == "boxplot") {
            print(lattice::bwplot(form, groups = x, layout = c(length(levels(z)), 1),
               ylab = ylab, xlab = xlab,
               panel = function(x, y, subscripts, groups) {
       	          # panel.grid(-1, 0)
                  lattice::panel.bwplot(x, y, col = groups[subscripts], horizontal = FALSE)
       	          if (any(model) & model.check) {
       	             fv <- unique(cbind(groups[subscripts], fitted(mdl)[subscripts]))
       	             ind <- apply(fv, 1, function(x) !any(is.na(x)))
       	             fv <- fv[ind, ]
       	             fv <- fv[order(fv[ , 1]), ]
       	             lattice::panel.lines(fv[ , 1], fv[ , 2], lwd = 2, col = "grey")
       	             lattice::panel.segments(fv[ , 1] - 0.4, fv[ , 2], fv[ , 1] + 0.4, fv[ , 2],
       	                            lwd = 2, col = "red")
       	          }
               }
            ))
         }
      })
      panel
      }
      
   rp.anova.fplot <- function(panel) {
      with(panel, {
         if (model.check & model0.check & any(model) & any(model0) & !all(model == model0)) {
         	clr  <- paste("grey", round(seq(100, 0, length = 101)), sep = "")
         	pct  <- qf(0.99, abs(df0 - df1), min(df0, df1))
       	    xlim <- max(pct * 1.5, fstat * 1.1)
       	    grd  <- seq(0, xlim, length = 100)
       	    del  <- diff(grd)[1] / 2
       	    grd  <- grd[-1] - del
       	    ind  <- cut(df(grd, abs(df0 - df1), min(df0, df1)), length(clr), labels = FALSE)
       	    par(mar = c(1, 1, 1, 1), oma = rep(0, 4), tcl = -0.2, xaxs = "i", mgp = c(1, 0, 0))
       	    plot(c(0, xlim), c(0, 1), type = "n", xlab = "", ylab = "", axes = FALSE)
       	    axis(1, cex.axis = 0.7)
       	    lines(par()$usr[1:2], rep(par()$usr[3], 2))
       	    rect(grd - del, 0.05, grd + del, 1, col = clr[ind], border = clr[ind])
       	    points(fstat, 0.525, col = "red", pch = 16)
       	    title(paste("p-value:", round(1 - pf(fstat, abs(df0 - df1), min(df0, df1)), 3)),
       	        cex.main = 0.8, font.main = 1)
         }
         else {
       	    par(mar = c(0, 0, 0, 0) + 0.1, bg = bgdcol)
            plot(c(0, 1), c(0, 1), type = "n", xlab = "", ylab = "", axes = FALSE)
         }
      })
      panel
   }
      
   rp.anova.redraw <- function(panel) {
      rp.tkrreplot(panel, plot)
      rp.tkrreplot(panel, fplot)
      panel
   }

   if (panel.plot & !requireNamespace("tkrplot", quietly = TRUE)) {
      warning("the tkrplot package is not available so panel.plot has been set to FALSE.")
      panel.plot <- FALSE
   }

   model.options <- c("overall mean")
   if (type == "One-way") {
      model.options <- c(model.options, xterm)
      term.names    <- c("x")
   }
   if (type == "Two-way") {
      model.options <- c(model.options, zterm, xterm, paste(xterm, ":", zterm))
      term.names    <- c("z", "x", "z:x")
   }
   init.model  <- model
   init.model0 <- model0
   if (any(is.na(init.model)))  init.model  <- rep(FALSE, 4)
   if (any(is.na(init.model0))) init.model0 <- rep(FALSE, 4)
   names(init.model) <- model.options
   bgdcol <- "grey85"

   if (panel) {
   	
      panel <- rp.control(paste(type, "anova"), 
                    x = x, y = y, z = z, type = type, xlab = xlab, ylab = ylab, ttl = NULL,
                    xterm = xterm, zterm = zterm, term.names = term.names, jitter.x = jitter.x, 
                    graphics = "strip plot", lines = lines,
                    model11 = init.model[1],  model12 = init.model[2],  model13 = init.model[3], 
                    model14 = init.model[4],  model01 = init.model0[1], model02 = init.model0[2],
                    model03 = init.model0[3], model04 = init.model0[4],
                    model.check = TRUE, model0.check = TRUE, bgdcol = bgdcol)
      
      rp.grid(panel, "controls", row = 1, column = 0, background = bgdcol)
      rp.grid(panel, "models",   grid = "controls", row = 0, column = 0, background = bgdcol)
      rp.grid(panel, "fplot",    grid = "controls", row = 0, column = 1, background = bgdcol)

      if (panel.plot) {
         rp.grid(panel, "dataplot", row = 0, column = 0, background = "white")
         rp.tkrplot(panel, plot,  rp.anova.draw,  hscale = hscale, vscale = vscale, 
                   grid = "dataplot", row = 0, column = 0, background = "white")
      	 rp.text(panel, "", grid = "fplot", row = 0, column = 0, background = bgdcol)
      	 rp.text(panel, "", grid = "fplot", row = 1, column = 0, background = bgdcol)
         rp.tkrplot(panel, fplot, rp.anova.fplot, hscale = hscale * 0.7, vscale = vscale * 0.2, 
                   grid = "fplot", row = 2, column = 0, background = bgdcol)
      	 rp.text(panel, "", grid = "fplot", row = 3, column = 0, background = bgdcol)
         action.fn <- rp.anova.redraw
      }
      else
         action.fn <- rp.anova.draw

      rp.text(panel, "        Model", grid = "models", row = 0, column = 1, background = bgdcol)
      rp.text(panel,       "current", grid = "models", row = 1, column = 0, background = bgdcol)
      rp.text(panel,           "new", grid = "models", row = 1, column = 2, background = bgdcol)
      rp.checkbox(panel, model11, action.fn, labels = "", initval = init.model[1],
            grid = "models", row = 2, column = 0, background = bgdcol)
      rp.checkbox(panel, model12, action.fn, labels = "", initval = init.model[2],
            grid = "models", row = 3, column = 0, background = bgdcol)
      for (i in 1:length(model.options)) rp.text(panel, model.options[i], 
            grid = "models", row = i + 1, column = 1, background = bgdcol)
      rp.checkbox(panel, model01, action.fn, labels = "", initval = init.model0[1],
            grid = "models", row = 2, column = 2, background = bgdcol)
      rp.checkbox(panel, model02, action.fn, labels = "", initval = init.model0[2],
            grid = "models", row = 3, column = 2, background = bgdcol)

      if (type == "Two-way") {
         rp.checkbox(panel, model13, action.fn, labels = "", initval = init.model[3],
               grid = "models", row = 4, column = 0, background = bgdcol)
         rp.checkbox(panel, model14, action.fn, labels = "", initval = init.model[4],
               grid = "models", row = 5, column = 0, background = bgdcol)
         rp.checkbox(panel, model03, action.fn, labels = "", initval = init.model0[3],
               grid = "models", row = 4, column = 2, background = bgdcol)
         rp.checkbox(panel, model04, action.fn, labels = "", initval = init.model0[4],
               grid = "models", row = 5, column = 2, background = bgdcol)
      }
      rp.do(panel, action.fn)
   }
   else {
      panel <- list(x = x, y = y, z = z, type = type, xlab = xlab, ylab = ylab, ttl = title,
                    xterm = xterm, zterm = zterm, term.names = term.names, jitter.x = jitter.x, 
                    graphics = "strip plot", lines = lines,
                    model11 = init.model[1],  model12 = init.model[2],  model13 = init.model[3], 
                    model14 = init.model[4],  model01 = init.model0[1], model02 = init.model0[2],
                    model03 = init.model0[3], model04 = init.model0[4],
                    model.check = TRUE, model0.check = TRUE, bgdcol = bgdcol)
      rp.anova.draw(panel)
   }
      
   invisible()
   
}

# This is Chris Jackson's function, copied here because of the requireNamespace issue

# rp.denstrip <- function (x, dens, at, width, horiz = TRUE, colmax, colmin = "white", 
    # scale = 1, gamma = 1, ticks = NULL, tlen = 1.5, twd, tcol, 
    # mticks = NULL, mlen = 1.5, mwd, mcol, lattice = FALSE, ...) 
# {
    # if (!is.numeric(x)) 
        # stop("'x' must be numeric")
    # if (missing(dens)) {
        # de <- density(x, ...)
        # x <- de$x
        # dens <- de$y
    # }
    # else {
        # if (!is.numeric(dens)) 
            # stop("'dens' must be numeric")
        # if (length(dens) != length(x)) 
            # stop("Lengths of 'dens' and 'x' must be the same")
        # dens <- dens[order(x)]
        # x <- sort(x)
    # }
    # if (lattice) {
        # rect.fn <- lattice::panel.rect
        # seg.fn <- lattice::panel.segments
        # default.width <- diff(lattice::current.panel.limits()[[if (horiz) 
            # "ylim"
        # else "xlim"]])/30
        # default.colmax <- lattice::trellis.par.get("add.line")$col
        # default.twd <- lattice::trellis.par.get("add.line")$lwd
        # default.mwd <- lattice::trellis.par.get("add.line")$lwd * 2
    # }
    # else {
        # rect.fn <- rect
        # seg.fn <- segments
        # default.width <- diff(par("usr")[if (horiz) 
            # 3:4
        # else 1:2])/30
        # default.colmax <- par("fg")
        # default.twd <- par("lwd")
        # default.mwd <- par("lwd") * 2
    # }
    # if (missing(width)) 
        # width <- default.width
    # if (missing(colmax)) 
        # colmax <- default.colmax
    # if (missing(twd)) 
        # twd <- default.twd
    # if (missing(mwd)) 
        # mwd <- default.mwd
    # if (missing(tcol)) 
        # tcol <- colmax
    # if (missing(mcol)) 
        # mcol <- colmax
    # dens <- dens/max(dens) * scale
    # n <- length(x)
    # rgbmax <- col2rgb(colmax, alpha = TRUE)
    # rgbmin <- if (colmin == "transparent") 
        # c(col2rgb(colmax, alpha = FALSE), 0)
    # else col2rgb(colmin, alpha = TRUE)
    # if (gamma <= 0) 
        # stop("gamma must be greater than 0")
    # p <- dens[1:(n - 1)]^gamma
    # if (colmin == "transparent") 
        # cols <- rgb(p * rgbmax[1] + (1 - p) * rgbmin[1], p * 
            # rgbmax[2] + (1 - p) * rgbmin[2], p * rgbmax[3] + 
            # (1 - p) * rgbmin[3], alpha = p * rgbmax[4] + (1 - 
            # p) * rgbmin[4], maxColorValue = 255)
    # else cols <- rgb(p * rgbmax[1] + (1 - p) * rgbmin[1], p * 
        # rgbmax[2] + (1 - p) * rgbmin[2], p * rgbmax[3] + (1 - 
        # p) * rgbmin[3], alpha = rgbmax[4], maxColorValue = 255)
    # first.col <- c(TRUE, cols[2:(n - 1)] != cols[1:(n - 2)])
    # next.col <- c(first.col, TRUE)
    # next.col[1] <- FALSE
    # if (horiz) {
        # xleft <- x[-n][first.col]
        # xright = x[next.col]
        # ybottom <- at - width/2
        # ytop <- at + width/2
    # }
    # else {
        # xleft <- at - width/2
        # xright <- at + width/2
        # ybottom <- x[-n][first.col]
        # ytop <- x[next.col]
    # }
    # rect.fn(xleft = xleft, ybottom = ybottom, xright = xright, 
        # ytop = ytop, border = NA, col = cols[first.col])
    # if (!is.null(ticks)) {
        # if (horiz) {
            # tx0 <- tx1 <- ticks
            # ty0 <- at - width * tlen/2
            # ty1 <- at + width * tlen/2
        # }
        # else {
            # tx0 <- at - width * tlen/2
            # tx1 <- at + width * tlen/2
            # ty0 <- ty1 <- ticks
        # }
        # seg.fn(tx0, ty0, tx1, ty1, lwd = twd, col = tcol)
    # }
    # if (!is.null(mticks)) {
        # if (horiz) {
            # tmx0 <- tmx1 <- mticks
            # tmy0 <- at - width * mlen/2
            # tmy1 <- at + width * mlen/2
        # }
        # else {
            # tmx0 <- at - width * mlen/2
            # tmx1 <- at + width * mlen/2
            # tmy0 <- tmy1 <- mticks
        # }
        # seg.fn(tmx0, tmy0, tmx1, tmy1, lwd = mwd, col = mcol)
    # }
    # invisible()
# }

# rp.panel.denstrip <- function (...) 
# {
    # rp.denstrip(..., lattice = TRUE)
# }
