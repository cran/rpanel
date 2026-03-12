#       regression2: regression with two covariates

rp.regression2 <- function (y, x1, x2, ylab = NA, x1lab = NA, x2lab = NA, panel = TRUE,
    model = "None", residuals.showing = FALSE, size = 3, col = "red", ...) {

   hscale <- 1
   vscale <- 1
   extra.args <- list(...)
   models     <- extra.args$models
   panel.ttl  <- extra.args$title
   display    <- extra.args$display
   if (!is.null(display) & !('formula' %in% class(display)))
      stop('display must be a formula.')

   rp.rotate <- function(panel) {
      with(panel, {
         if (phi < -90) phi <- -90
         if (phi >  90) phi <-  90
         rgl::view3d(theta = theta, phi = phi, fov = fov)
      })
      panel
   }

   rp.regression2.model <- function(panel, x, y) {


     # if (panel$interactive) {
     # if (missing(x))
     #   panel$model = 'None'
     # else {
     #   d.nodes <- (panel$model.nodes$x - x)^2 + (panel$model.nodes$y - y)^2
     #   comp1   <- panel$model.nodes$comparison1
     #   comp2   <- panel$model.nodes$comparison2
     #   if (!is.null(comp1)) {
     #      x.comps <- (panel$model.nodes$x[comp1] + panel$model.nodes$x[comp2]) / 2
     #      y.comps <- (panel$model.nodes$y[comp1] + panel$model.nodes$y[comp2]) / 2
     #      d.comps <- (x.comps - x)^2 + (y.comps - y)^2
     #      d.nodes <- c(d.nodes, d.comps)
     #   }
     #   hpt <- which.min(d.nodes)
     #   n.nodes <- length(panel$model.nodes$x)
     #   panel$comparison <- NULL
     #   if (hpt > n.nodes) {
     #      hpt <- hpt - n.nodes
     #      panel$highlighted.node <- c(comp1[hpt], comp2[hpt])
     #      panel$comparison <- hpt
     #   }
     #   else
     #      panel$highlighted.node <- hpt
     #   panel$model <- switch(panel$highlighted.node[1],
     #                         'No effects', xlab, zlab, paste(xlab, 'and', zlab))
     #   rp.control.put(panel$panelname, panel)
     #   rp.tkrreplot(panel, modelnodes)
     #   rp.tkrreplot(panel, fplot)
     # }
     # }
     with(panel, {
          if (current.model != "None") {
             rgl::pop3d()
             if (residuals.showing) rgl::pop3d()
          }
          if (model != "None") {
             a <- scaling(xgrid, smat[,, model], zgrid)
             rgl::surface3d(x = a$x, z = a$z, y = a$y, alpha = 0.5)
             if (residuals.showing) {
                a <- scaling(c(t(cbind(x, x))), c(t(cbind(y, fv[ , model]))),
                             c(t(cbind(z, z))))
                rgl::segments3d(a$x, a$y, a$z, col = "green")
             }
          }
       })
   panel$current.model <- panel$model
   panel
   }

   rp.regression2.residuals <- function(panel) {
      with(panel, {
         if (model != "None") {
            if (residuals.showing) {
               a <- scaling(c(t(cbind(x, x))), c(t(cbind(y, fv[ , model]))),
                            c(t(cbind(z, z))))
               rgl::segments3d(a$x, a$y, a$z, col = "green")
            }
            else rgl::pop3d()
        }
      })
      panel
   } 
   
    if (requireNamespace("rgl", quietly = TRUE)) {

        if (is.na(ylab))  ylab  <- deparse(substitute(y))
        if (is.na(x1lab)) x1lab <- deparse(substitute(x1))
        if (is.na(x2lab)) x2lab <- deparse(substitute(x2))
        x      <- x1
        z      <- x2
        xlab   <- x1lab
        zlab   <- x2lab
        ngrid  <- 20
        ind <- !is.na(x + y + z)
        if (length(col) == length(x)) {
           ind <- (ind & (!is.na(col)))
           clr <- col[ind]
           }
        else 
           clr <- col
        if (!all(ind)) {
           x <- x[ind]
           y <- y[ind]
           z <- z[ind]
           warning("missing data removed.")
           }
        xlo <- min(x) - 0.05 * diff(range(x))
        xhi <- max(x) + 0.05 * diff(range(x))
        ylo <- min(y) - 0.05 * diff(range(y))
        yhi <- max(y) + 0.05 * diff(range(y))
        zlo <- min(z) - 0.05 * diff(range(z))
        zhi <- max(z) + 0.05 * diff(range(z))
        xgrid <- seq(xlo, xhi, length = ngrid)
        zgrid <- seq(zlo, zhi, length = ngrid)
        smatx <- matrix(rep(xgrid, ngrid), ncol = ngrid)
        smatz <- t(matrix(rep(zgrid, ngrid), ncol = ngrid))
        smat <- array(c(mean(y, na.rm = TRUE) + 0 * smatx,
                        coef(lm(y ~ x))[1] + coef(lm(y ~ x))[2] * smatx,
                        coef(lm(y ~ z))[1] + coef(lm(y ~ z))[2] * smatz,
                        coef(lm(y ~ x + z))[1] + coef(lm(y ~ x + z))[2] * smatx +
                                coef(lm(y ~ x + z))[3] * smatz),
                      dim = c(ngrid, ngrid, 4))
        fv <- matrix(c(fitted(lm(y ~ 1)), fitted(lm(y ~ x)),
                       fitted(lm(y ~ z)), fitted(lm(y ~ x + z))), ncol = 4)
        both <- paste(xlab, "and", zlab)
        dimnames(smat) <- list(NULL, NULL, c("No effects", xlab, zlab, both))
        dimnames(fv) <- list(NULL, c("No effects", xlab, zlab, both))
        ylo <- min(ylo, smat)
        yhi <- max(yhi, smat)
        ylim <- c(ylo, yhi)
        scaling <- rp.plot3d(x, y, z, xlab = x1lab, ylab = ylab,
                             zlab = x2lab, ylim = ylim, col = clr)

        if (panel) {
           bgdcol      <- "grey85"
           model.nodes <- data.frame(x = c(0.5, 0.25, 0.75, 0.5),
                                     y = 0.85 - c(0, 1, 1, 2) * 0.35,
                                     label = paste(ylab, '~',
                                                   c('1', xlab, zlab,
                                                     paste(xlab, zlab, sep = ' + '))),
                                     comparison1 = c(2, 3, 4, 4),
                                     comparison2 = c(1, 1, 2, 3))
           # panel.name <- rpanel::rp.panelname()
            spin.panel <- rp.control(panel.ttl, x = x, y = y, z = z,
                xlab = xlab, ylab = ylab, zlab = zlab,
                theta = -30, phi = 30,
                model.display = 'terms', bgdcol = "grey85",
                models = models, interactive = TRUE,
                # realname = panel.name,
                xgrid = xgrid, zgrid = zgrid, scaling = scaling,
                fov = 1, current.model = "None", smat = smat,
                fv = fv, model = model, residuals.showing = residuals.showing,
                highlighted.node = NA, model.nodes = model.nodes)
            # rp.tkrplot(spin.panel, modelnodes, rp.lmsmall.modelnodes,
            #            action = rp.regression2.model,
            #            vscale = 0.5, background = "white")
            # rp.doublebutton(spin.panel, theta, -1, title = "Theta", action = rp.rotate)
            # rp.doublebutton(spin.panel, phi,   -1, title = "Phi",   action = rp.rotate)
            rp.radiogroup(spin.panel, model,
                c("None", "No effects", xlab, zlab, paste(xlab, "and", zlab)),
                title = "Model", action = rp.regression2.model)
            rp.checkbox(spin.panel, residuals.showing, rp.regression2.residuals, "Show residuals")
            # rp.tkrplot(spin.panel, fplot, rp.lmsmall.effectsplot,
            #            vscale = vscale * 0.5, 
            #            background = bgdcol)
            rp.do(spin.panel, rp.regression2.model)
            # invisible(list(panel.name = panel.name))
        }
        else {
            if (missing(model)) {
               if (is.null(display))
                  model <- paste(xlab, "and", zlab)
               else {
                  display <- as.character(display)[-1]
                  if (display == '1') model <- 'No effects'
                  else model <- display
               }
            }
            rp.regression2.model(list(x = x, y = y, z = z, xlab = xlab,
                ylab = ylab, zlab = zlab, theta = -30, phi = 30,
                xgrid = xgrid, zgrid = zgrid, scaling = scaling,
                fov = 1, current.model = "None", model = model,
                interactive = FALSE,
                smat = smat, fv = fv, residuals.showing = residuals.showing))
        }
    }
    else {
        stop("regression2 requires the rgl package.")
    }
   
   invisible(spin.panel)
}
