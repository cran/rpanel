#       regression2: rpanel function for regression
#                    with two covariates.


rp.plot3d <- function (x, y, z, xlab = NA, ylab = NA, zlab = NA, axes = TRUE,
    type = "p", size = 3, col = "red", xlim = NA, ylim = NA,
    zlim = NA, ...)
{
    if (require(rgl)) {
        if (is.na(xlab))
            xlab <- deparse(substitute(x))
        if (is.na(ylab))
            ylab <- deparse(substitute(y))
        if (is.na(zlab))
            zlab <- deparse(substitute(z))
        xrange <- xlim
        yrange <- ylim
        zrange <- zlim
        ind <- !is.na(x + y + z)
        if (length(col) == length(x)) 
           ind <- (ind & (!is.na(col)))
        if (!all(ind)) cat("Warning: missing data removed. \n")
        if (any(is.na(xlim))) {
            xrange[1] <- min(x[ind]) - 0.05 * diff(range(x[ind]))
            xrange[2] <- max(x[ind]) + 0.05 * diff(range(x[ind]))
        }
        if (any(is.na(ylim))) {
            yrange[1] <- min(y[ind]) - 0.05 * diff(range(y[ind]))
            yrange[2] <- max(y[ind]) + 0.05 * diff(range(y[ind]))
        }
        if (any(is.na(zlim))) {
            zrange[1] <- min(z[ind]) - 0.05 * diff(range(z[ind]))
            zrange[2] <- max(z[ind]) + 0.05 * diff(range(z[ind]))
        }
        xscale <- pretty(xrange)
        yscale <- pretty(yrange)
        zscale <- pretty(zrange)
        xscale <- xscale[xscale >= xrange[1] & xscale <= xrange[2]]
        yscale <- yscale[yscale >= yrange[1] & yscale <= yrange[2]]
        zscale <- zscale[zscale >= zrange[1] & zscale <= zrange[2]]
        xadj1 <- mean(xrange)
        yadj1 <- mean(yrange)
        zadj1 <- mean(zrange)
        xadj2 <- diff(xrange)/2
        yadj2 <- diff(yrange)/2
        zadj2 <- diff(zrange)/2
        x.orig <- x
        y.orig <- y
        z.orig <- z
        x <- (x - xadj1)/xadj2
        y <- (y - yadj1)/yadj2
        z <- (z - zadj1)/zadj2
        xscale.adj <- (xscale - xadj1)/xadj2
        yscale.adj <- (yscale - yadj1)/yadj2
        zscale.adj <- (zscale - zadj1)/zadj2
        rx <- c(-1, 1)
        ry <- c(-1, 1)
        rz <- c(-1, 1)
        rgl.open()
        rgl.bg(col = c("white", "black"))
        if (!(type == "n")) {
            ind1 <- ((x.orig >= xrange[1]) & (x.orig <= xrange[2]) &
                (y.orig >= yrange[1]) & (y.orig <= yrange[2]) &
                (z.orig >= zrange[1]) & (z.orig <= zrange[2]))
            ind <- (ind1 & ind)
            if (length(col) == length(x.orig)) clr <- col[ind]
               else clr <- col
            rgl.points(x[ind], y[ind], z[ind], size = size, col = clr)
        }
        rgl.viewpoint(-30, 30, fov = 1)
        if (axes) {
            rgl.lines(rx[c(1, 2, 2, 2, 2, 1, 1, 1)], ry[rep(1,
                8)], rz[c(1, 1, 1, 2, 2, 2, 2, 1)], col = "black")
            rgl.lines(rx[c(1, 2, 2, 2, 2, 1, 1, 1)], ry[rep(2,
                8)], rz[c(1, 1, 1, 2, 2, 2, 2, 1)], col = "black")
            for (i in 1:2) for (j in 1:2) rgl.lines(rx[c(i, i)],
                ry[c(1, 2)], rz[c(j, j)], col = "black")
            rgl.texts(mean(rx), min(rx), min(rx), "")
            delta <- 0.1
            nyticks <- length(yscale)
            if (nyticks/2 - floor(nyticks/2) > 0)
                ypos <- 1/(nyticks - 1)
            else ypos <- 0
            rgl.texts(c(0, -1 - 2 * delta, -1 - 2 * delta), c(-1 -
                2 * delta, ypos, -1 - 2 * delta), c(1 + 2 * delta,
                -1 - 2 * delta, 0), c(xlab, ylab, zlab), adj = c(0.5,
                0.5, 1), col = "blue")
            rgl.texts((xscale - xadj1)/xadj2, -1 - delta, 1 +
                delta, as.character(xscale), col = "black")
            rgl.texts(-1 - delta, (yscale - yadj1)/yadj2, -1 -
                delta, as.character(yscale), col = "black")
            rgl.texts(-1 - delta, -1 - delta, (zscale - zadj1)/zadj2,
                as.character(zscale), col = "black")
            scaling <- function(x, y, z) {
                list(x = x, y = y, z = z)
            }
            rgl.segments(xscale.adj, -1, 1, xscale.adj, -1 -
                delta/4, 1 + delta/4, scaling = scaling, col = "black")
            rgl.segments(-1, yscale.adj, -1, -1 - delta/4, yscale.adj,
                -1 - delta/4, scaling = scaling, col = "black")
            rgl.segments(-1, -1, zscale.adj, -1 - delta/4, -1 -
                delta/4, zscale.adj, scaling = scaling, col = "black")
        }
        scaling <- function(x, y, z) {
            xx <- (x - xadj1)/xadj2
            yy <- (y - yadj1)/yadj2
            zz <- (z - zadj1)/zadj2
            list(x = xx, y = yy, z = zz)
        }
        invisible(scaling)
    }
    else {
        warning("Package RGL is not installed.")
    }
}

rp.rotate <- function(panel) {
   with(panel, {
      if (phi < -90) phi <- -90
      if (phi >  90) phi <-  90
      rgl.viewpoint(theta = theta, phi = phi, fov = fov)
      })
   panel
   }

rp.regression2.model <- function(panel) {
   with(panel, {
      if (current.model != "None") {
         rgl.pop()
         if (residuals.showing) rgl.pop()
         }
      if (model != "None") {
         a <- scaling(xgrid, smat[,, model], zgrid)
         rgl.surface(a$x, a$z, a$y, alpha = 0.5)
         if (residuals.showing)
            rgl.segments(x, fv[, model], z, x, y, z, scaling, col = "green")
         }
      })
   panel$current.model <- panel$model
   panel
   }

rp.regression2.residuals <- function(panel) {
   with(panel, {
      if (model != "None") {
         if (residuals.showing)
            rgl.segments(x, fv[, model], z, x, y, z, scaling, col = "green")
         else rgl.pop()
         }
      })
   panel
   } 
   
rgl.segments <- function(x0, y0, z0, x1, y1, z1, scaling, ...) {
         a <- scaling(c(rbind(x0, x1)), c(rbind(y0, y1)), c(rbind(z0, z1)))
         rgl.lines(a$x, a$y, a$z, ...)
         } 
         
rp.regression2 <- function (y, x1, x2, ylab = NA, x1lab = NA, x2lab = NA, panel = TRUE,
    model = "None", residuals.showing = FALSE, size = 3, col = "red")
{
    if (require(rgl)) {

        if (is.na(x1lab))
            x1lab <- deparse(substitute(x1))
        if (is.na(x2lab))
            x2lab <- deparse(substitute(x2))
        if (is.na(ylab))
            ylab <- deparse(substitute(y))
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
           cat("Warning: missing data removed. \n")
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
        smat <- array(c(mean(y, na.rm = TRUE) + 0 * smatx, coef(lm(y ~
            x))[1] + coef(lm(y ~ x))[2] * smatx, coef(lm(y ~
            z))[1] + coef(lm(y ~ z))[2] * smatz, coef(lm(y ~
            x + z))[1] + coef(lm(y ~ x + z))[2] * smatx + coef(lm(y ~
            x + z))[3] * smatz), dim = c(ngrid, ngrid, 4))
        fv <- matrix(c(fitted(lm(y ~ 1)), fitted(lm(y ~ x)),
            fitted(lm(y ~ z)), fitted(lm(y ~ x + z))), ncol = 4)
        both <- paste(xlab, "and", zlab)
        dimnames(smat) <- list(NULL, NULL, c("No effects", xlab,
            zlab, both))
        dimnames(fv) <- list(NULL, c("No effects", xlab, zlab,
            both))
        ylo <- min(ylo, smat)
        yhi <- max(yhi, smat)
        ylim <- c(ylo, yhi)
        scaling <- rp.plot3d(x, y, z, xlab = x1lab, ylab = ylab,
            zlab = x2lab, ylim = ylim, col = clr)

        if (panel) {
            panel.name <- rp.panelname()
            spin.panel <- rp.control("Spin plot", x = x, y = y,
                z = z, xlab = xlab, ylab = ylab, zlab = zlab,
                theta = -30, phi = 30, realname = panel.name,
                xgrid = xgrid, zgrid = zgrid, scaling = scaling,
                fov = 1, current.model = "None", smat = smat,
                fv = fv, model = model, residuals.showing = residuals.showing)
            spin.panel <- rp.doublebutton(spin.panel, theta,
                -1, title = "Theta", action = rp.rotate)
            spin.panel <- rp.doublebutton(spin.panel, phi, -1,
                title = "Phi", action = rp.rotate)
            spin.panel <- rp.radiogroup(spin.panel, model, c("None",
                "No effects", xlab, zlab, paste(xlab, "and",
                  zlab)), title = "Model", action = rp.regression2.model)
            spin.panel <- rp.checkbox(spin.panel, residuals.showing,
                title = "Show residuals", action = rp.regression2.residuals)
            invisible(list(panel.name = panel.name))
        }
        else {
            rp.regression2.model(list(x = x, y = y, z = z, xlab = xlab,
                ylab = ylab, zlab = zlab, theta = -30, phi = 30,
                xgrid = xgrid, zgrid = zgrid, scaling = scaling,
                fov = 1, current.model = "None", model = model,
                smat = smat, fv = fv, residuals.showing = residuals.showing))
            invisible()
        }
    }
    else {
        stop("regression2 will not run without package RGL installed as this plots in 3D.")
    }
}
