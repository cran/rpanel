#     A general function for linear models

rp.lm <- function(x, ylab, xlab, zlab,
                  panel = TRUE, panel.plot = TRUE,
                  style = 'ggplot', plot.nodes = FALSE,
                  uncertainty.display = 'density', inference = 'coefficients',
                  ci = TRUE, cols, display.model, comparison.model,
                  residuals.showing, linewidth = 1,
                  hscale = 1, vscale = hscale, ...) {
   
   if (!requireNamespace('ggplot2', quietly = TRUE)) stop('this function requires the ggplot2 package.')
   
   static <- !panel
   missing.display.model    <- missing(display.model)
   missing.comparison.model <- missing(comparison.model)
   if (missing.display.model & panel & missing.comparison.model) {
      display.model <- NULL
      missing.display.model <- FALSE
   }
   if (missing.comparison.model) comparison.model <- NULL
   if (missing(residuals.showing)) residuals.showing <- FALSE
   if (!requireNamespace('ggplot2', quietly = TRUE)) style <- 'old'
   # Other possibilities
   # refcol        <- '#E495A5'
   # notchcol      <- 'black' # or make it the bgdcol
   # library(RColorBrewer)
   # display.brewer.pal(3, 'Pastel1')
   # brewer.pal(3, 'Pastel1')
   # [1] "#FBB4AE" "#B3CDE3" "#CCEBC5"
   # > brewer.pal(3, 'Pastel2')
   # [1] "#B3E2CD" "#FDCDAC" "#CBD5E8"
   # Alison's suggestions
   # clr           <- c(est    = '#9BD5FF', estline   = '#0093FF',
   #                    ref    = '#FFB56B', refline   = '#FF7F00')
   # Pastel2 choices
   # clr           <- c(est    = '#B3E2CD', estline   = '#B3E2CD',
   #                    ref    = '#FDCDAC', refline   = '#FDCDAC',
   # Pastel1 choices
   current.model <- 'None'
   scaling       <- NULL
   smat          <- NULL
   fv            <- NULL
   xgrid         <- NULL
   zgrid         <- NULL
   clrs          <- if (missing(cols)) rp.colours() else rp.colours(cols)
   
   # Deal with formula or model inputs
   class.x <- class(x)
   if ('formula' %in% class.x)
      model <- lm(x, ...)
   else if ('lm' %in% class.x)
      model <- x
   else
      stop('x is not a formula or a linear model object.')
   if (!('model' %in% names(model))) {
      model <- update(model, model = TRUE)
      message("model refitted with 'model = TRUE'.")
   }

   # Extract required information from the model
   mf           <- model$model
   response.ind <- attr(model$terms, 'response')
   var.types    <- attr(model$terms, 'dataClasses')
   indchar      <- which(var.types == 'character')
   if (length(indchar) > 0)
      stop('character variables detected - perhaps factors were intended.')
   numeric.ind  <- which(var.types == 'numeric')
   numeric.ind  <- numeric.ind[numeric.ind != response.ind]
   factor.ind   <- which(var.types == 'factor')
   contr        <- attributes(model$qr$qr)$contrasts
   trms         <- attr(model$terms, 'term.labels')
   
   if (length(trms) == 0) stop('at least one predictor variable is required.')
   if (length(numeric.ind) + length(factor.ind) > 2) {
      if (inference == 'coefficients') return(rp.coefficients(model))
      if (inference == 'terms') return(drop1(model))
   }

   # Extract the raw data
   y <- mf[ , response.ind]
   x <- mf[ , trms[1]]
   z <- if (length(trms) > 1) mf[ , trms[2]] else rep(NA, length(x))
   jitter.x <- jitter(as.numeric(x), factor = 0.5, amount = NULL)
   
   # Set up variable labels
   yterm <- names(var.types)[response.ind]
   xterm <- trms[1]
   zterm <- if (length(trms) > 1) trms[2] else NA
   if (missing(ylab)) ylab <- yterm
   if (missing(xlab)) xlab <- xterm
   if (missing(zlab)) zlab <- zterm
   # if (length(numeric.ind) == 2 & length(factor.ind) == 0 & length(xlab) == 1)
   #    stop('xlab is of length 1 but length 2 is needed.')

   # Identify type of model
   if (length(numeric.ind) == 1 & length(factor.ind) == 0) type <- 'regression.one'
   if (length(numeric.ind) == 2 & length(factor.ind) == 0) type <- 'regression.two'
   if (length(numeric.ind) == 1 & length(factor.ind) == 1) type <- 'ancova'
   if (length(numeric.ind) == 0 & length(factor.ind) == 1) type <- 'one.way'
   if (length(numeric.ind) == 0 & length(factor.ind) == 2) type <- 'two.way'
   ttl <- switch(type, regression.one = 'Simple regression',
                       regression.two = 'Regression with two covariates',
                               ancova = 'Analysis of covariance',
                              one.way = 'One-way analysis of variance',
                              two.way = 'Two-way analysis of variance')

   # Export to older functions if required
   if (type == 'regression.one')
      return(rp.regression(x, y, panel = panel, xlab = xlab, ylab = ylab))
   if (style != 'ggplot') {
      if (type == 'ancova') {
         if (factor.ind == 2) {
            temp <- z
            z    <- x
            x    <- temp
         }
         return(rp.ancova.old(x = x, y = y, group = z, panel = panel, panel.plot = panel.plot,
                       xlab = xlab, ylab = ylab, hscale = hscale, vscale = vscale))
      }
      if (type == 'regression.two') {
         return(rp.regression2(y, x, z,
                               ylab = ylab, x1lab = xlab, x2lab = zlab, model = "None",
                               residuals.showing = FALSE))
      }
   }
   
   # Set up the model nodes
   bgdcol      <- "grey85"
   model.nodes <- data.frame(x = c(0.5, 0.25, 0.75, 0.5, 0.5),
                             y = 0.9 - c(0, 1, 1, 2, 3) * 0.25,
                             label = paste(yterm, '~',
                                           c('1', xterm, zterm,
                                             paste(xterm, zterm, sep = ' + '),
                                             paste(xterm, zterm,
                                                   paste(xterm, zterm, sep = ':'),
                                                   sep = ' + '))),
                             comparison1 = c(2, 3, 4, 4, 5),
                             comparison2 = c(1, 1, 2, 3, 4))
   if (type == 'one.way')
      model.nodes <- data.frame(x = c(0.5, 0.5),
                                y = 0.8 - c(0, 1) * 0.6,
                                label = paste(yterm, '~', c('1', xterm)),
                                comparison1 = 2, comparison2 = 1)
   if (type == 'regression.two') {
      model.nodes   <- model.nodes[1:4, ]
      model.nodes$y <- 0.85 - c(0, 1, 1, 2) * 0.35
      # Set up the 3D plot
      ngrid  <- 20
      xlo   <- min(x) - 0.05 * diff(range(x))
      xhi   <- max(x) + 0.05 * diff(range(x))
      ylo   <- min(y) - 0.05 * diff(range(y))
      yhi   <- max(y) + 0.05 * diff(range(y))
      zlo   <- min(z) - 0.05 * diff(range(z))
      zhi   <- max(z) + 0.05 * diff(range(z))
      xgrid <- seq(xlo, xhi, length = ngrid)
      zgrid <- seq(zlo, zhi, length = ngrid)
      smatx <- matrix(rep(xgrid, ngrid), ncol = ngrid)
      smatz <- t(matrix(rep(zgrid, ngrid), ncol = ngrid))
      smat  <- array(c(mean(y, na.rm = TRUE) + 0 * smatx,
                       coef(lm(y ~ x))[1] + coef(lm(y ~ x))[2] * smatx,
                       coef(lm(y ~ z))[1] + coef(lm(y ~ z))[2] * smatz,
                       coef(lm(y ~ x + z))[1] + coef(lm(y ~ x + z))[2] * smatx +
                          coef(lm(y ~ x + z))[3] * smatz),
                     dim = c(ngrid, ngrid, 4))
      fv <- matrix(c(fitted(lm(y ~ 1)), fitted(lm(y ~ x)),
                     fitted(lm(y ~ z)), fitted(lm(y ~ x + z))), ncol = 4)
      dimnames(smat) <- list(NULL, NULL, model.nodes$label)
      dimnames(fv)   <- list(NULL, model.nodes$label)
      ylo     <- min(ylo, smat)
      yhi     <- max(yhi, smat)
      ylim    <- c(ylo, yhi)
      if (panel | !plot.nodes) scaling <- rp.plot3d(x, y, z, xlab = xlab, ylab = ylab,
                           zlab = zlab, ylim = ylim, col = clrs['points'])
      else scaling <- NULL
   }
   opar <- par(oma = c(0, 0, 1, 0), plt = c(0, 1, 0, 1))
   on.exit(par(opar))
   
   # Find the coefficient names from the maximal model
   form <- paste(yterm, '~', trms[1])
   if (length(trms) > 1)
      form <- paste(yterm, '~', trms[1], '+', trms[2])
   if (type %in% c('ancova', 'two.way')) {
      form      <- paste(form, ' + ', trms[1], ':', trms[2], sep = '')
   }
   model.max  <- update(model, form)
   # If the specified model contains an invalid interaction in regression.two
   # remove this
   if (type == 'regression.two' & length(trms) > 2) {
      model <- model.max
      warning('an interaction between covariates is not permitted.')
   }
   labels.max <- names(coefficients(model.max))[-1]
   
   # Deal with missing data
   lm.args <- list(...)
   nms     <- all.vars(model$terms)
   if ('data' %in% names(lm.args))
      data.dfrm <- lm.args$data[ , nms]
   else {
      data.dfrm <- mf[ , nms[1:2]]
      if (length(nms) > 2 ) data.dfrm <- cbind(data.dfrm, mf[ , nms[3]])
      data.dfrm <- as.data.frame(data.dfrm)
      names(data.dfrm) <- nms
   }
   ind       <- apply(data.dfrm, 1, function(x) any(is.na(x)))
   data.dfrm <- data.dfrm[!ind, ]

   # Check that the display.model and comparison.model formula, if supplied,
   # are submodels of the maximal model
   terms.max <- attr(terms(model.max), 'term.labels')
   if (!missing.display.model && !is.null(display.model)) {
      terms.display <- attr(terms(display.model), 'term.labels')
      if (!all(terms.display %in% terms.max))
         stop('display.model is not a valid model.')
   }
   if (!is.null(comparison.model)) {
      terms.comparison <- attr(terms(comparison.model), 'term.labels')
      if (!all(terms.comparison %in% terms.max))
         stop('comparison.model is not a valid model.')
   }

   # Set the default for display.model, if this has not been specified
   fn2 <- function(x, trms) {
      mt <- attr(terms(as.formula(x)), 'term.labels')
      (length(trms) == length(mt)) & all(trms %in% mt)
   }
   if (!missing.display.model) {
      if (is.null(display.model))
         hlight <- NA
      else {
         terms.display <- attr(terms(display.model), 'term.labels')
         terms.model   <- sapply(model.nodes$label, fn2, terms.display)
         hlight        <- which(terms.model)
      }
   }
   else {
      terms.model <- attr(terms(model), 'term.labels')
      terms.model <- sapply(model.nodes$label, fn2, terms.model)
      hlight      <- which(terms.model)
   }
   
   if (!is.null(comparison.model)) {
      terms.comparison <- attr(terms(comparison.model), 'term.labels')
      terms.model      <- sapply(model.nodes$label, fn2, terms.comparison)
      comp.ind         <- c(hlight, which(terms.model))
      fn <- function(x) all(comp.ind %in% x)
      comps    <- as.matrix(model.nodes[ , c('comparison1', 'comparison2')])
      comp.ind <- which(apply(comps, 1, fn))
      if (length(comp.ind) == 0) stop(paste('only adjacent models can be compared ',
                                            'in this function.\n  Use the anova',
                                            'function to perform more general comparisons.'))
      # The 1 below deals with the one.way case
      hlight   <- comps[comp.ind[1], ]
   }

   # Create a list of valid models
   # and, for anova, estimates and se's for comparisons.
   # Use this to define the plotted response range.
   # Missing data: use the dataframe from the specified model
   models  <- list()
   nmodels <- switch(type, regression.two = 4, ancova = 5,
                           one.way = 2, two.way = 5)
   for (i in 1:nmodels) {
      indx <- c(FALSE, TRUE, FALSE, TRUE, TRUE)[i]
      indz <- c(FALSE, FALSE, TRUE, TRUE, TRUE)[i]
      ind  <- names(contr) %in% c(xterm, zterm)[c(indx, indz)]
      models[[i]] <- update(model, model.nodes$label[i], data = data.dfrm,
                            contrasts = contr[ind])
   }
   
   if (type %in% c('one.way', 'two.way')) {
      model.est <- list()
      comp.est  <- list()
      comp.se   <- list()
      for (i in 1:nmodels) {
         mdlm   <- models[[i]]
         mdl    <- models[[model.nodes$comparison1[i]]]
         mdl0   <- models[[model.nodes$comparison2[i]]]
         lst    <- if (type == 'two.way') list(x, z)
                   else list(x)
         ngps    <- nrow(unique(data.frame(x, z)))
         df0     <- mdl0$df.residual
         df1     <- mdl$df.residual
         model.est[[i]] <- tapply(fitted(mdlm), lst, mean)
         comp.est[[i]]  <- tapply(fitted(mdl0), lst, mean)
         # if (any(is.na(coef(mdlm))))
         #    comp.se[[i]] <- comp.est[[i]] * NA
         # else {
         H   <- model.matrix(mdl)
         ind <- which(is.na(coef(mdl)))
         if (length(ind) > 0) H <- H[ , -ind]
         H   <- H %*% solve(crossprod(H)) %*% t(H)
         H0  <- model.matrix(mdl0)
         ind <- which(is.na(coef(mdl0)))
         if (length(ind) > 0) H0 <- H0[ , -ind]
         H0  <- H0 %*% solve(crossprod(H0)) %*% t(H0)
         cse <- sqrt(diag(tcrossprod(H - H0))) * summary(mdl)$sigma
         comp.se[[i]] <- tapply(cse, lst, mean)
         # }
         # Old version based on the contributions to the global test statistic
         # comp.se[[i]] <- tapply(fitted(mdl0), lst, length)
         # comp.se[[i]] <- summary(mdl)$sigma * sqrt(abs(df0 - df1)) /
         #                      sqrt(comp.se[[i]] * ngps)
      }
      response.range <- range(y, unlist(model.est),
                              unlist(comp.est) + 3 * unlist(comp.se),
                              unlist(comp.est) - 3 * unlist(comp.se),
                              na.rm = TRUE)
      drr <- 0.01 * diff(response.range)
      response.range <- response.range + c(-drr, drr)
   }
   else {
      model.est <- NULL
      comp.est  <- NULL
      comp.se   <- NULL
      response.range <- range(y)
   }
   
   # Linear regression with two covariates
   # if (length(numeric.ind) == 2 & length(factor.ind) == 0)
   #    return(rp.regression2.lm(y, x, z,
   #                             yterm = yterm, x1term = xterm, x2term = zterm,
   #                             ylab = ylab, x1lab = xlab, x2lab = zlab,
   #                             panel = panel, models = models, title = ttl,
   #                             display = display.model,
   #                             residuals.showing = residuals.showing))
      
   if (panel) {
      pnl <- rp.control(ttl, models = models, y = y, x = x, z = z,
                        model.est = model.est, comp.est = comp.est, comp.se = comp.se,
                        response.range = response.range,
                        jitter.x = jitter.x, uncertainty.display = uncertainty.display,
                        type = type, style = style, labels.max = labels.max,
                        xlab = xlab, ylab = ylab, zlab = zlab, seed = round(runif(1) * 10000),
                        yterm = yterm, xterm = xterm, zterm = zterm,
                        ci = ci, bgdcol = bgdcol, clrs = clrs,
                        highlighted.node = hlight, static = static,
                        model.display = inference,
                        model.nodes = model.nodes, click.coords = rep(NA, 2),
                        residuals.showing = residuals.showing, linewidth = linewidth,
                        current.model = current.model, scaling = scaling,
                        smat = smat, fv = fv, xgrid = xgrid, zgrid = zgrid)
      model.display <- NULL
      rp.menu(pnl, model.display, list(c('Inference', 'none', 'coefficients', 'terms')),
              action = rp.lm.redraw)
      rp.grid(pnl, "models", row = 0, column = 0, background = bgdcol)
      rp.tkrplot(pnl, 'modelnodes', rp.lm.modelnodes, action = rp.lm.click,
                 hscale = 0.7 * hscale, vscale = 0.5 * vscale, 
                 grid = "models", row = 0, column = 0, background = "white")
      
      if (panel.plot) {
         rp.grid(pnl, "dataplot", row = 0, column = 1, background = "white")
         if (type != 'regression.two')
            rp.tkrplot(pnl, 'plot', rp.lm.draw,
                  hscale = hscale, vscale = vscale, 
                  grid = "dataplot", row = 0, column = 0, background = "white")
         else {
            rp.do(pnl, rp.lm.draw)
            rp.checkbox(pnl, residuals.showing, rp.regression2.residuals, "Show residuals",
                        grid = "models", row = 1, column = 0)
         }
         rp.tkrplot(pnl, 'fplot', rp.lm.effectsplot,
                    hscale = hscale * 0.7, vscale = vscale * 0.5, 
                    grid = "models", row = 2, column = 0, background = bgdcol)
      }
      else {
         rp.do(pnl, rp.lm.draw)
      }
   }
   else {
      pnl <- list(ttl, models = models, y = y, x = x, z = z,
                  model.est = model.est, comp.est = comp.est, comp.se = comp.se,
                  response.range = response.range, residuals.showing = residuals.showing,
                  linewidth = linewidth,
                  jitter.x = jitter.x, uncertainty.display = uncertainty.display,
                  type = type, style = style, labels.max = labels.max,
                  xlab = xlab, ylab = ylab, zlab = zlab, seed = round(runif(1) * 10000),
                  yterm = yterm, xterm = xterm, zterm = zterm,
                  ci = ci, clrs = clrs, bgdcol = bgdcol,
                  highlighted.node = hlight, static = static,
                  model.display = inference,
                  model.nodes = model.nodes, click.coords = rep(NA, 2),
                  current.model = current.model, scaling = scaling,
                  smat = smat, fv = fv, xgrid = xgrid, zgrid = zgrid)
      if (plot.nodes)
         return(invisible(rp.lm.modelnodes(pnl)))
      plt <- rp.lm.draw(pnl)
      if (pnl$type != 'regression.two')
         return(plt)
      else
         return(invisible(plt))
   }
   
   invisible(pnl)
   
}

rp.lm.modelnodes <- function(panel) {
   fillcol <- rep('white', 5)
   if (!any(is.na(panel$highlighted.node)))
      fillcol[panel$highlighted.node] <- panel$clrs['node']

   with(panel$model.nodes, {
      par(oma = c(0, 0, 1, 0), plt = c(0, 1, 0, 1))
      plot(0:1, 0:1, type = 'n', axes = FALSE, xlab = '', ylab = '')
      segments(x[c(1, 1, 2, 3, 4)], y[c(1, 1, 2, 3, 4)],
               x[c(2, 3, 4, 4, 5)], y[c(2, 3, 4, 4, 5)])
      sw   <- strwidth(label)
      sh   <- strheight(label)
      frsz <- 0.02
      rect(x - sw/2 - frsz, y - sh/2 - frsz, x + sw/2 + frsz, y + sh/2 + frsz,
           col = fillcol)
      text(x, y, label)
      if (!is.null(comparison1)) {
         points((x[comparison1] + x[comparison2]) / 2,
                (y[comparison1] + y[comparison2]) / 2,
                pch = 16, col = 'grey', cex = 2)
      }
      if (!panel$static) text(0.5, 1, '(click to fit)')
      title('Model lattice', outer = TRUE)
   })
   
   # ggplot code - but this does not allow clicked co-ordinate identification
   # plt <- ggplot2::ggplot(panel$model.nodes, ggplot2::aes(x, y, label = label)) +
   #    ggplot2::annotate('segment',
   #                      x    = panel$model.nodes$x[c(1, 1, 2, 3, 4)],
   #                      y    = panel$model.nodes$y[c(1, 1, 2, 3, 4)],
   #                      xend = panel$model.nodes$x[c(2, 3, 4, 4, 5)],
   #                      yend = panel$model.nodes$y[c(2, 3, 4, 4, 5)]) +
   #    ggplot2::geom_label(size = 6, fill = fillcol) +
   #    ggplot2::theme_void() +
   #    ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) +
   #    ggplot2::ggtitle("Models") +
   #    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 20,
   #                                                      face = "bold"))
   # print(plt)
   
   panel
}

rp.lm.click <- function(panel, x, y) {
   d.nodes <- (panel$model.nodes$x - x)^2 + (panel$model.nodes$y - y)^2
   comp1   <- panel$model.nodes$comparison1
   comp2   <- panel$model.nodes$comparison2
   if (!is.null(comp1)) {
      x.comps <- (panel$model.nodes$x[comp1] + panel$model.nodes$x[comp2]) / 2
      y.comps <- (panel$model.nodes$y[comp1] + panel$model.nodes$y[comp2]) / 2
      d.comps <- (x.comps - x)^2 + (y.comps - y)^2
      d.nodes <- c(d.nodes, d.comps)
   }
   hpt <- which.min(d.nodes)
   n.nodes <- length(panel$model.nodes$x)
   panel$comparison <- NULL
   if (hpt > n.nodes) {
      hpt <- hpt - n.nodes
      panel$highlighted.node <- c(comp1[hpt], comp2[hpt])
      panel$comparison <- hpt
   }
   else
      panel$highlighted.node <- hpt
   rp.control.put(panel$panelname, panel)
   rp.tkrreplot(panel, 'modelnodes')
   rp.tkrreplot(panel, 'fplot')
   if (panel$type != 'regression.two')
      rp.tkrreplot(panel, 'plot')
   else
      panel <- rp.lm.draw(panel)
   panel
}

rp.lm.draw <- function(panel) {
   
   hlight <- panel$highlighted.node

   # -------------------------------------------------------------
   #                       Two covariates
   # -------------------------------------------------------------
   
   if (panel$type == 'regression.two') {
      with(panel, {
         if (current.model != "None") {
            rgl::pop3d()
            if (residuals.showing) rgl::pop3d()
         }
         if (!any(is.na(hlight))) {
            a <- scaling(xgrid, smat[ , , hlight[1]], zgrid)
            rgl::surface3d(x = a$x, z = a$z, y = a$y, col = clrs['estimate'], alpha = 0.5)
            if (residuals.showing) {
               a <- scaling(c(t(cbind(x, x))), c(t(cbind(y, fv[ , hlight[1]]))),
                            c(t(cbind(z, z))))
               rgl::segments3d(a$x, a$y, a$z, col = clrs['residuals'])
            }
         }
      })
      if (!any(is.na(hlight))) panel$current.model <- hlight[1]
   }
   
   # -------------------------------------------------------------
   #                          Ancova
   # -------------------------------------------------------------
   
   if (panel$type == 'ancova') {
      
      y    <- panel$y
      x    <- panel$x
      z    <- panel$z
      dfrm <- data.frame(y, x, z)
      xlab <- panel$xlab
      zlab <- panel$zlab
      if (is.factor(panel$x)) {
         dfrm$x <- panel$z
         dfrm$z <- panel$x
         xlab   <- panel$zlab
         zlab   <- panel$xlab
      }
      if (!any(is.na(hlight))) {
         mdl            <- panel$models[[hlight[1]]]
         fvals          <- fitted(mdl)
         dfrm$fvals     <- fvals
         var.types      <- attr(mdl$terms, 'dataClasses')
         factor.present <- any(var.types == 'factor')
      }
      
      if (panel$style == 'ggplot') {
         plt  <- ggplot2::ggplot(dfrm, ggplot2::aes(x, y, group = z, col = z)) +
            ggplot2::geom_point() +
            ggplot2::labs(x = xlab, y = panel$ylab, col = zlab)
         if (!any(is.na(hlight))) {
            if (factor.present)
               plt <- plt + ggplot2::geom_line(ggplot2::aes(y = fvals),
                                               linewidth = panel$linewidth)
            else
               plt <- plt + ggplot2::geom_line(ggplot2::aes(y = fvals,
                                             col = NULL, group = NULL),
                                             linewidth = panel$linewidth)
            form <- panel$model.nodes$label[hlight[1]]
         }
         else
            form <- 'none'
         plt <- plt + ggplot2::ggtitle(paste("Model:", form))
      }
   
   }
   
   # -------------------------------------------------------------
   #                          Anova
   # -------------------------------------------------------------

   if (panel$type %in% c('one.way', 'two.way')) {
      jitter.x <- panel$jitter.x
      dfrm <- data.frame(x = panel$x, y = panel$y, jitter.x)
      if (panel$type == 'two.way') dfrm$z <- panel$z
      
      # Plot setup
      plt  <- ggplot2::ggplot(dfrm, ggplot2::aes(y, x)) +
         ggplot2::xlab(panel$ylab) + ggplot2::ylab(panel$xlab) +
         ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                        panel.grid.minor = ggplot2::element_blank(),
                        panel.background = ggplot2::element_rect(fill = "grey90")) +
         # ggplot2::geom_hline(yintercept = as.numeric(2:nlevels(dfrm$x)) - 0.5,
         #                     col = 'grey80') +
         ggplot2::ggtitle(panel$ttl)
      ngrid <- 512
      xgrid <- seq(panel$response.range[1], panel$response.range[2], length = ngrid)
      del   <- diff(panel$response.range) / ngrid
      
      # Plot the data density
      if (length(hlight) != 2) {
         lst   <- if (panel$type == 'two.way') list(panel$x, panel$z) else list(panel$x)
         fn    <- function(x) {
            bw   <- if (length(x) > 1) bw.norm(x) else diff(panel$response.range) / 8
            dens <- density(x, bw = bw, n = ngrid,
                            from = panel$response.range[1] + del,
                            to   = panel$response.range[2] - del)
            cbind(dens$x, dens$y)
         }
         dgrid <- tapply(panel$y, lst, fn)
         xdens <- unlist(lapply(dgrid, function(x) x[ , 1]))
         ydens <- unlist(lapply(dgrid, function(x) x[ , 2]))
         fn1   <- function(x) if (!is.null(x)) nrow(x) else 0
         repl  <- unlist(lapply(dgrid, fn1))
         fnms  <- expand.grid(dimnames(dgrid))
         xf    <- factor(rep(fnms[ , 1], repl), levels = levels(panel$x))
         dfrm0 <- data.frame(xdens, ydens, x = xf)
         # dfrm0 <- data.frame(xdens, ydens, x = rep(as.numeric(levels(panel$x)), each = ngrid))
         if (panel$type == 'two.way') 
            dfrm0$z <- factor(rep(fnms[ , 2], repl), levels = levels(panel$z))
         plt <- plt + ggplot2::geom_tile(ggplot2::aes(xdens, x, fill = ydens, height = 0.8),
                                         data = dfrm0, show.legend = FALSE) +
                      ggplot2::scale_fill_gradient(low = 'grey90', high = 'grey70')
         # This creates missing tiles - I don't know why
         # plt <- plt + ggplot2::stat_density(ggplot2::aes(fill   = ggplot2::after_stat(density)),
         #                                    geom = "tile", trim = TRUE,
         #                                    height = 0.7, position = "identity",
         #                                    show.legend = FALSE) +
         #    ggplot2::scale_fill_gradient(low = 'grey90', high = 'grey50')
         # geom_violin doesn't look good with small samples
         # plt <- plt + ggplot2::geom_violin(bw = 'nrd', col = NA, fill = 'grey80')
      }
      
      # Set the range
      plt  <- plt + ggplot2::xlim(panel$response.range[1] - del, panel$response.range[2] + del)

      # Plot the data points
      set.seed(panel$seed)
      plt  <- plt +
         ggplot2::geom_point(ggplot2::aes(y, jitter.x), col = panel$clrs['points'])
      # ggplot2::geom_jitter(ggplot2::aes(y, x), height = 0.1, width = 0,
      #                      col = panel$clrs['points'])
      
      # Plot the fitted values of the display model
      if (!any(is.na(hlight))) {
         mdl <- panel$models[[hlight[1]]]
         if (mdl$df.residual > 0) {
            afx <- as.numeric(factor(dfrm$x))
            plt <- plt + ggplot2::geom_segment(ggplot2::aes(x    = fitted(mdl),
                                                            y    = afx - 0.45,
                                                            yend = afx + 0.45),
                                             linewidth = 1, col = panel$clrs['estline'])
         }
         else
            plt <- plt + ggplot2::ggtitle('There is insufficient data to fit this model.')
      }
      
      # Plot the model comparison uncertainties
      if (length(hlight) == 2) {
         m1 <- panel$models[[hlight[1]]]
         m2 <- panel$models[[hlight[2]]]
         if (m1$df.residual * m2$df.residual == 0)
            plt <- plt + ggplot2::ggtitle('There is insufficient data to fit both these models.')
         else {
            fn       <- function(x) all(hlight %in% x)
            comps    <- as.matrix(panel$model.nodes[ , c('comparison1', 'comparison2')])
            # The [1] below is a fix for the one-way case
            comp.ind <- which(apply(comps, 1, fn))[1]
            if (!all(is.na(panel$comp.se[[comp.ind]]))) {
               est      <- panel$comp.est[[comp.ind]]
               se       <- panel$comp.se[[comp.ind]]
               # Ensure xgrid covers the detail of each normal distribution as there may be very different se's
               cest     <- c(est)
               cse      <- c(se)
               cest     <- cest[!is.na(cest)]
               cse      <- cse[!is.na(cse)]
               ugrid    <- 100
               fun      <- function(i) seq(cest[i] - 3 * cse[i], cest[i] + 3 * cse[i], length = ugrid)
               xgrid    <- c(sapply(1:length(cest), fun))
               ngrid    <- length(xgrid)
               xwdth    <- rep(6 * cse / ugrid, each = ugrid)
               # xgrid    <- seq(panel$response.range[1] + del, panel$response.range[2] - del,
               #                 length = ngrid)
               if (panel$type == 'two.way') {
                  dfrm1 <- data.frame(y = c(est), x = rep(rownames(est), ncol(est)),
                                                  z = rep(colnames(est), each = nrow(est)))
                  dfrm1 <- dfrm1[!is.na(dfrm1$y), ]
                  dfrm1 <- data.frame(xgrid = xgrid, xwdth = xwdth,
                                      x = factor(rep(dfrm1$x, each = ugrid), levels = levels(panel$x)),
                                      z = factor(rep(dfrm1$z, each = ugrid), levels = levels(panel$z)))
                  dfrm1$dgrid <- dnorm(dfrm1$xgrid, est[cbind(dfrm1$x, dfrm1$z)], se[cbind(dfrm1$x, dfrm1$z)]) /
                                 dnorm(0, 0, se[cbind(dfrm1$x, dfrm1$z)])
               }
               else {
                  dfrm1 <- data.frame(y = est, x = names(est))
                  dfrm1 <- data.frame(xgrid = xgrid, xwdth = xwdth,
                                      x = factor(rep(dfrm1$x, each = ugrid), levels = levels(panel$x)))
                  dfrm1$dgrid <- dnorm(dfrm1$xgrid, est[dfrm1$x], se[dfrm1$x]) / dnorm(0, 0, se[dfrm1$x])
               }
               ind   <- apply(dfrm1, 1, function(x) any(is.na(x)))
               dfrm1 <- dfrm1[!ind, ]
               if (panel$uncertainty.display == 'shading') {
                  plt <- plt + ggplot2::geom_tile(ggplot2::aes(xgrid, as.numeric(factor(x)),
                                                  fill = dgrid, height = 0.8, width = xwdth + del, alpha = 0.9),
                                                  data = dfrm1, show.legend = FALSE) +
                     ggplot2::scale_fill_gradient(low = "grey90", high = panel$clrs['reference'])
               }
               else {
                  dfrm1$dgrid <- 0.8 * dfrm1$dgrid
                  # dfrm1$dgrid <- 0.8 * dfrm1$dgrid / dnorm(0, 0, min(se, na.rm = TRUE))
                  plt <- plt + ggplot2::geom_tile(ggplot2::aes(xgrid, as.numeric(x),
                                                               height = dgrid, width = xwdth + del),
                                                  col = NA, fill = panel$clrs['reference'],
                                                  # alpha = 0.7,
                                                  show.legend = FALSE, data = dfrm1)
               }
            }
         }
      }
      
      # plt <- plt + ggplot2::scale_y_discrete(labels = levels(x), breaks = 1:nlevels(x))

      plt  <- plt + ggplot2::coord_flip()
      if (panel$type == "two.way")
         plt <- plt + ggplot2::facet_grid(. ~ z)
   }
   
   # -------------------------------------------------------------
   #                    Return value
   # -------------------------------------------------------------
   
   if (panel$type != 'regression.two') {
      if (panel$static) return(plt) else print(plt)
   }
   
   panel
}

rp.lm.effectsplot <- function(panel) {
   with(panel, {
      nhl    <- length(highlighted.node)
      hlight <- (!any(is.na(highlighted.node)) && 
                 (highlighted.node[1] > 1 | nhl > 1))
      fn.blank <- function(text = '') {
         plot(c(0, 1), c(0, 1), type = "n", xlab = "", ylab = "", axes = FALSE,
              mar = c(0, 0, 0, 0) + 0.1, bg = bgdcol)
         text(0.5, 0.5, text)
         invisible()
      }
      if (hlight) {
         mdl <- models[[highlighted.node[1]]]
         if (nhl == 1) {
            if (mdl$df.residual == 0) {
               fn.blank('There is insufficient data to fit this model.')
               return(panel)
            }
            if (model.display == 'coefficients')
               print(rp.coefficients(mdl, ci = ci))
            else if (model.display == 'terms')
               print(rp.drop1(mdl))
            else
               fn.blank()
         }
         if (nhl == 2) {
            mdl0 <- models[[highlighted.node[2]]]
            if (mdl0$df.residual * mdl$df.residual == 0) {
               fn.blank('There is insufficient data to fit these models.')
               return(panel)
            }
            r0   <- rownames(anova(mdl0))
            r1   <- rownames(anova(mdl))
            trm  <- r1[!(r1 %in% r0)]
            print(rp.drop1(mdl, trm, col = clrs['reference']))
         }
      }
      else 
         fn.blank()
   })
   panel
}

rp.regression2.residuals <- function(panel) {
   with(panel, {
      if (length(highlighted.node) > 0) {
         if (residuals.showing) {
            mdl <- model.nodes$label[highlighted.node[1]]
            a <- scaling(c(t(cbind(x, x))), c(t(cbind(y, fv[ , mdl]))),
                         c(t(cbind(z, z))))
            rgl::segments3d(a$x, a$y, a$z, col = clrs['residuals'])
         }
         else
            rgl::pop3d()
      }
   })
   panel
}

rp.lm.redraw <- function(panel) {
   if (panel$type != 'regression.two')
      rp.tkrreplot(panel, 'plot')
   else
      panel <- rp.lm.draw(panel)
   rp.tkrreplot(panel, 'fplot')
   rp.tkrreplot(panel, 'modelnodes')
   panel
}
