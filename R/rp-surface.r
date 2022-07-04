rp.surface <- function(surface, covariance, x1grid, x2grid, x, y, Display = "persp",
                       hscale = 1, vscale = hscale, panel = TRUE,
                       Speed = 5, ntime = 10, ninterp = 50,
                       zlim = NULL, col.palette = topo.colors(100), coords = rep(NA, 2)) {

   if (!requireNamespace("tkrplot", quietly = TRUE)) stop("the tkrplot package is not available.")
   if (!requireNamespace("interp", quietly = TRUE))   stop("the interp package is not available.")
   # if (!requireNamespace("lattice", quietly = TRUE)) stop("the lattice package is not available.")
   # if (!requireNamespace("rgl", quietly = TRUE))     stop("the rgl package is not available.")

   x1lab <- deparse(substitute(x1grid))
   x2lab <- deparse(substitute(x2grid))
   ylab  <- deparse(substitute(surface))

   #   This needs to allow different grid lengths in different directions.
   n1grid  <- length(x1grid)
   n2grid  <- length(x2grid)
   xgrid   <- as.matrix(expand.grid(x1grid, x2grid))
   eig     <- eigen(covariance)
   e.vals  <- pmax(eig$values, 0)
   e.vecs  <- eig$vectors
   se.fit  <- sqrt(diag(covariance))
   surface <- c(surface)

   if (is.null(zlim))
      zlim <- range(surface - 3 * se.fit, surface + 3 * se.fit, na.rm = TRUE)
   brks <- seq(zlim[1], zlim[2], length = length(col.palette) + 1)

   col.fn <- function(pnl) {
      rp.colour.key(pnl$col.palette, brks, par.mar = c(3, 0, 1, 1.5) + 0.1, margin = TRUE)
      if (all(!is.na(pnl$coords))) lines(rep(-0.5, 2), pnl$cipos, lwd = 3)
      pnl
   }

   #   Set up a finer grid and mask for the image plots
   xg1    <- seq(min(x1grid), max(x1grid), length = ninterp)
   xg2    <- seq(min(x2grid), max(x2grid), length = ninterp)
   xg     <- as.matrix(expand.grid(xg1, xg2))
   ind    <- apply(xg, 1, function(x) which.min((xgrid[ , 1] - x[1])^2 + (xgrid[ , 2] - x[2])^2))
   mask   <- as.numeric(!is.na(surface[ind]))
   mask[mask == 0] <- NA
   mask   <- matrix(mask, ncol = ninterp)
   ind.na <- !is.na(surface)

   rp.surface.draw <- function(pnl) {

      surf <- pnl$surface
      if (pnl$animation) surf <- surf + pnl$e.isurf

      if (pnl$Display == "image") {
         surf <- interp::interp(pnl$xgrid[pnl$ind.na, 1], pnl$xgrid[pnl$ind.na, 2], surf[pnl$ind.na],
                        pnl$xg1, pnl$xg2)$z * pnl$mask
         par(mar = c(3, 3, 1, 0) + 0.1, mgp = c(2, 0.5, 0), tcl = -0.2)
         image(pnl$xg1, pnl$xg2, surf, zlim = pnl$zlim, col = pnl$col.palette,
               xlab = pnl$x1lab, ylab = pnl$x2lab)
      }
         # print(contourplot(c(surf) ~ xgrid[,1]*xgrid[,2], region = TRUE))
         # filled.contour(x1grid, x2grid, surf, zlim = zlim, color.palette = heat.colors)
      else if (pnl$Display == "persp") {
      	 surf <- matrix(surf, ncol = pnl$n2grid)
      	 ng1  <- 1:(pnl$n1grid - 1)
      	 ng2  <- 1:(pnl$n2grid - 1)
      	 clr  <- array(c(surf[ng1, ng2], surf[ng1 + 1, ng2], surf[ng1, ng2 + 1],
      	          surf[ng2 + 1, ng2 + 1]), dim = c(pnl$n1grid - 1, pnl$n2grid - 1, 4))
      	 ind  <- apply(clr, 1:2, function(x) if (length(which(is.na(x))) > 1) NA else 1)
      	 clr  <- apply(clr, 1:2, function(x) mean(x, na.rm = TRUE)) * ind
          clr  <- cut(c(clr), pnl$brks, labels = FALSE, include.lowest = TRUE)
          clr  <- pnl$col.palette[clr]
          par(mar = c(3, 3, 1, 0.5) + 0.1, mgp = c(1.5, 0.2, 0), tcl = -0.2)
          persp(pnl$x1grid, pnl$x2grid, surf,
                zlim = pnl$zlim, theta = pnl$theta, phi = pnl$phi, ticktype = "detailed",
                col = clr, d = 4, xlab = pnl$x1lab, ylab = pnl$x2lab, zlab = pnl$ylab)
      }

      # Draw the confidence interval against the colour key
      if (all(!is.na(pnl$coords)) & pnl$Display == "image" & !pnl$animation) {
         # ind    <- !is.na(pnl$surface)
         # pred1  <- interp::interp(pnl$xgrid[ind, 1], pnl$xgrid[ind, 2], pnl$surface[ind],
                          # pnl$coords[1], pnl$coords[2])$z
         # se1    <- interp::interp(pnl$xgrid[ind, 1], pnl$xgrid[ind, 2], pnl$se.fit[ind],
                          # pnl$coords[1], pnl$coords[2])$z

         se.fit <- interp::interp(pnl$xgrid[pnl$ind.na, 1], pnl$xgrid[pnl$ind.na, 2], pnl$se.fit[pnl$ind.na],
                          pnl$xg1, pnl$xg2)$z * pnl$mask
         ind    <- !is.na(c(surf))
         xg     <- as.matrix(expand.grid(pnl$xg1, pnl$xg2))
         pred1  <- interp::interp(xg[ind, 1], xg[ind, 2], c(surf)[ind],
                          pnl$coords[1], pnl$coords[2])$z
         se1    <- interp::interp(xg[ind, 1], xg[ind, 2], c(se.fit)[ind],
                          pnl$coords[1], pnl$coords[2])$z

         # Is pretty ok here?
         # col.range <- range(pretty(range(pred1), 20))
         ci <- c(pred1 - 2 * se1, pred1 + 2 * se1)
         # cipos <- c((ci[1] - col.range[1]) / diff(col.range),
                    # (ci[2] - col.range[1]) / diff(col.range))
         # cipos <- min(x2grid) + cipos * diff(range(x2grid))
         pnl$cipos <- ci
         if (pnl$panel.ind)
            rp.control.put(pnl$panelname, pnl)
         else
            points(coords[1], coords[2], pch = "+", cex = 2)
      }

#      if (animation == "none") {
#         scaling <- rp.plot3d(x[,1], y, x[,2], ylim = range(y, e.sim),
#            xlab = "Longitude", ylab = "Score1", zlab = "Latitude")
#         save(scaling, file = "scaling.dmp")
#         xgrid <- cbind(x1grid, x2grid)
#         sm.surface3d(xgrid, e.sim[ , , 1],
#                    scaling = scaling, zlim = range(e.sim))
#      }

      if (pnl$animation == "rgl") {
         scaling <- rp.plot3d(x[,1], y, x[,2], ylim = range(y, e.sim))
         # load("scaling.dmp")
         xgrid <- cbind(pnl$x1grid, pnl$x2grid)
#         sm.surface3d(xgrid, e.sim[ , , 1],
#                    scaling = scaling, zlim = range(e.sim))
         for (i in 2:nsim) {
            for (wt in seq(0, 1, length = 15)) {
               rgl::par3d(skipRedraw = TRUE)
               rgl::pop3d()
               rgl::pop3d()
               sm::sm.surface3d(pnl$xgrid,
                    (1 - wt) * e.sim[ , , i - 1] + wt * e.sim[ , , i],
                    scaling = scaling, zlim = range(e.sim))
               rgl::par3d(skipRedraw = FALSE)
               # Sys.sleep(0.1)
            }
         }
      }

      pnl
   }

   rp.surface.redraw <- function(pnl) {
      rp.tkrreplot(pnl, plot)
      pnl
   }

   rp.key.redraw <- function(pnl) {
      rp.tkrreplot(pnl, key)
      pnl
   }

   animate <- function(pnl) {
      pnl$animation  <- !pnl$animation
      if (pnl$animation) {
         pnl$e.sim      <- pnl$e.vecs %*% diag(sqrt(pnl$e.vals)) %*%
                                     rnorm(length(pnl$surface))
         pnl$e.sim.old  <- rep(0, length(pnl$surface))
         pnl$isurf      <- 1
   	     rp.control.put(pnl$panelname, pnl)
   	     rp.timer(pnl, 1, animation.call, function(pnl) pnl$animation)
         pnl$e.isurf    <- rep(0, length(pnl$surface))
  	     rp.control.put(pnl$panelname, pnl)
         rp.tkrreplot(pnl, plot)
      }
      pnl
   }

   animation.call <- function(pnl) {
   	  Sys.sleep(0.01 + pnl$Speed / 100)
   	  if (pnl$isurf == ntime + 1) {
         pnl$e.sim.old <- pnl$e.sim
         pnl$e.sim     <- pnl$e.vecs %*% diag(sqrt(pnl$e.vals)) %*%
                                  rnorm(length(pnl$surface))
   	  	 pnl$isurf     <- 1
   	  }
   	  wt          <- pnl$isurf / ntime
   	  wt1         <- sqrt(wt^2 + (1 - wt)^2)
      pnl$e.isurf <- pnl$e.sim.old * (1 - wt) / wt1 + pnl$e.sim * wt / wt1
      pnl$isurf   <- pnl$isurf + 1
   	  rp.control.put(pnl$panelname, pnl)
      rp.tkrreplot(pnl, plot)
        # Code to create figures from an animation
        # pdf(paste("figures/surface-", pnl$npdf, ".pdf", sep = ""))
        # rp.surface.draw(pnl)
        # dev.off()
        # print(pnl$npdf)
        # pnl$npdf <- pnl$npdf + 1
      pnl
   }

   mouse <- function(pnl, x, y) {
   	  pnl$coords <- c(x, y)
   	  rp.control.put(pnl$panelname, pnl)
   	  rp.tkrreplot(pnl, plot)
      rp.tkrreplot(pnl, key)
      pnl$npdf <- pnl$npdf + 1
      pnl
   }

   release <- function(pnl, x, y) {
   	  pnl$coords <- c(NA, NA)
   	  rp.control.put(pnl$panelname, pnl)
   	  rp.tkrreplot(pnl, plot)
      rp.tkrreplot(pnl, key)
      pnl
   }

   if (panel) {
      pnl <- rp.control(x = x, y = y, surface = c(surface), se.fit = se.fit,
                          e.vecs = e.vecs, e.vals = e.vals,
                          x1grid = x1grid, x2grid = x2grid, xgrid = xgrid,
                          n1grid = n1grid, n2grid = n2grid,
                          zlim = zlim, coords = rep(NA, 2), theta = -30, phi = 40,
                          Display = Display, animation = FALSE, npdf = 1,
                          Speed = Speed, ntime = ntime, ninterp = ninterp,
                          xg1 = xg1, xg2 = xg2, mask = mask, ind.na = ind.na,
                          x1lab = x1lab, x2lab = x2lab, ylab = ylab,
                          brks = brks, col.palette = col.palette, col.fn = col.fn,
                          panel.ind = TRUE)
      rp.grid(pnl, "controls", row = 0, column = 0, sticky = "n")
      rp.grid(pnl, "plot",     row = 0, column = 1, background = "white")
      rp.grid(pnl, "key",      row = 0, column = 2, background = "white")
      rp.tkrplot(pnl, plot, rp.surface.draw, mouse, mouse, release,
                 hscale = hscale, vscale = vscale,
                 grid = "plot", row = 0, column = 0, background = "white")
      rp.tkrplot(pnl, key, col.fn, hscale = 0.15 * hscale, vscale = vscale,
                 grid = "key", row = 0, column = 0, background = "white")
      rp.radiogroup(pnl, Display, c("persp", "image"), action = rp.surface.redraw,
                 grid = "controls", row = 0, column = 0)
      rp.slider(pnl, theta, -180, 180, rp.surface.redraw, labels = "persp angle 1",
                 grid = "controls", row = 1, column = 0)
      rp.slider(pnl, phi,      0,  90, rp.surface.redraw, labels = "persp angle 2",
                 grid = "controls", row = 2, column = 0)
      rp.button(pnl, animate, "Animate: on/off",
                 grid = "controls", row = 3, column = 0)
      rp.doublebutton(pnl, Speed, 0.95, log = TRUE, action = rp.surface.redraw,
                 grid = "controls", row = 5, column = 0)
   }
   else {
      pnl <- list(x = x, y = y, surface = c(surface), se.fit = se.fit,
                          e.vecs = e.vecs, e.vals = e.vals,
                          x1grid = x1grid, x2grid = x2grid, xgrid = xgrid,
                          n1grid = n1grid, n2grid = n2grid,
                          zlim = zlim, coords = coords, theta = -30, phi = 40,
                          Display = Display, animation = FALSE, npdf = 1,
                          Speed = Speed, ntime = ntime, ninterp = ninterp,
                          xg1 = xg1, xg2 = xg2, mask = mask, ind.na = ind.na,
                          x1lab = x1lab, x2lab = x2lab, ylab = ylab,
                          brks = brks, col.palette = col.palette, col.fn = col.fn,
                          panel.ind = FALSE)
      layout(matrix(c(1, 2), ncol = 2), widths = c(7, 1))
      pnl <- rp.surface.draw(pnl)
      # ind   <- !is.na(surface)
      # pred1 <- interp(xgrid[ind, 1], xgrid[ind, 2], surface[ind], coords[1], coords[2])$z
      # se1   <- interp(xgrid[ind, 1], xgrid[ind, 2], se.fit[ind],  coords[1], coords[2])$z
      # col.range <- range(pretty(range(pred1), 20))
      # ci <- c(pred1 - 2 * se1, pred1 + 2 * se1)
      # cipos <- c((ci[1] - col.range[1]) / diff(col.range),
                 # (ci[2] - col.range[1]) / diff(col.range))
      # pnl$cipos <- ci
      col.fn(pnl)
      layout(1)
   }

   invisible()
}
