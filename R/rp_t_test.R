#     A function for a confidence interval and hypothesis test
#     on two samples of data

rp.t_test <- function(x, y = NULL, panel = TRUE, mu = NULL, data.display = 'density',
                      display = c(distribution = FALSE, detail = FALSE),
                      ruler.position = 'none', candidate, print = FALSE,
                      cols, xlab, ylab, vlab, hscale = 1, vscale = hscale, ...) {
   
   if (!requireNamespace('ggplot2'))
      stop('the ggplot2 package is not available.')
   
   # Formula input for x
   if ('formula' %in% class(x)) {
      model <- lm(x, model = TRUE, ...)
      trms  <- attr(model$terms, 'term.labels')
      if (length(trms) > 1) stop('there should only be one predictor variable.')
      var.types    <- attr(model$terms, 'dataClasses')
      response.ind <- attr(model$terms, 'response')
      x            <- model$model[ , response.ind]
      if (inherits(x, 'Pair')) {
         x <- apply(x, 1, diff)
         if (missing(vlab)) vlab <- 'Difference'
      }
      if (length(trms) > 0) {
         factor.ind   <- which(var.types == 'factor')
         if (length(factor.ind) != 1) stop('the predictor variable should be a single factor.')
         y <- model$model[ , trms[1]]
         if (nlevels(y) != 2) stop('the predictor variable should have two levels.')
      }
      if (missing(vlab)) vlab <- names(model$model)[response.ind]
   }
   
   # x numeric and y character or a factor and set up labels
   if (!is.numeric(x)) stop('x is neither a formula nor numeric.')
   if (is.character(y)) y <- factor(y)
   if (is.factor(y)) {
      if (nlevels(y) > 2) stop('y is a factor with more than two levels.')
      if (missing(xlab)) xlab <- levels(y)[1]
      if (missing(ylab)) ylab <- levels(y)[2]
      if (missing(vlab)) vlab <- deparse(substitute(x))
      v <- x
      g <- y
      x <- v[g == levels(y)[1]]
      y <- v[g == levels(y)[2]]
   }
   else if (!is.null(y)) {
      # y is numeric
      if (missing(xlab)) xlab <- deparse(substitute(x))
      if (missing(ylab)) ylab <- deparse(substitute(y))
      if (missing(vlab)) vlab <- 'Value'
   }
   else {
      # y is null (absent)
      if (missing(vlab)) vlab <- deparse(substitute(x))
      xlab <- NA
      ylab <- NA
   }
   
   # Check the input arguments for display options
   
   if (length(data.display) > 1) stop('data.display should be of length 1.')
   data.display.options <- c('none', 'density', 'histogram')
   if (!(data.display %in% data.display.options)) {
      warning("display not recognised - reverting to 'density'.")
      data.display <- 'density'
   }
   if (length(ruler.position) > 1) stop('ruler.position should be of length 1.')
   ruler.options <- c('none', 'candidate', 'sample mean')
   reference  <- !is.null(mu)
   if (reference) ruler.options <- c(ruler.options, 'reference')
   if (!(ruler.position %in% ruler.options)) {
      msg <- 'ruler.position is invalid.'
      if (ruler.position == 'reference' & !reference)
         msg <- paste(msg, 'Setting is reference but mu has not been specified.')
      msg <- paste(msg, 'Reverting to none.')
      message(msg)
      ruler.position <- 'none'
   }
   if (!is.logical(display)) stop('display is not logical.')
   if (!('distribution' %in% names(display)))
      display <- c(display, 'distribution' = FALSE)
   if (!('detail'       %in% names(display)))
      display <- c(display, 'detail' = FALSE)
   display.names <- c('distribution', 'detail')
   invalid <- !(names(display) %in% display.names)
   if (any(invalid)) {
      message(paste(c('invalid display options identified and ignored:',
                      names(display)[invalid]), collapse = ' '))
      display <- display[!invalid]
   }
   
   horizontal <- TRUE
   height <- c('data' = 0.25, 'uncertainty' = 0.15, 'mean' = 0.5)
   
   if (!reference) mu <- 0
   ttest  <- t.test(x, y, mu = mu, ...)
   if (!reference) mu <- NULL
   if (print) print(ttest)
   
   ttest.args <- list(...)
   paired     <- if ('paired' %in% names(ttest.args)) ttest.args$paired else FALSE
   method     <- if      (is.null(y)) 'One'
                 else if (!paired)    'Two'
                 else                 'Paired'
   if (method == 'Paired') {
      x <- x - y
      y <- NULL
   }
   
   clrs    <- if (missing(cols)) rp.colours() else rp.colours(cols)
   bgdcol <- clrs['bgdcol']
   seed   <- round(runif(1) * 100000)

   if (missing(candidate)) {
      candidate <- ifelse(method == 'Two', ttest$estimate[2], ttest$estimate[1])
      candidate <- candidate + 4 * ttest$stderr * runif(1, -1, 1)
   }
   
   if (panel) {
      pnl <- rp.control(x = x, y = y, col = clrs, height = height, method = method,
                  data.display = data.display, seed = seed, mu = mu,
                  candidate = candidate, display = display,
                  conf = attr(ttest$conf.int, 'conf.level'), static = !panel,
                  ruler.position = ruler.position, reference = reference,
                  xlab = xlab, ylab = ylab, vlab = vlab, nmin = 10, bgdcol = bgdcol,
                  distribution = 't', horizontal = horizontal, ttest = ttest)
      rp.grid(pnl, "controls", row = 0, column = 0, background = bgdcol)
      rp.grid(pnl, "picture",  row = 0, column = 1, background = "white")
      if (method %in% c('One', 'Paired')) {
         rp.tkrplot(pnl, plot, rp.onesample,
                    hscale = hscale, vscale = vscale, 
                    grid = "picture", row = 0, column = 0, background = "white")
         rp.radiogroup(pnl, ruler.position, ruler.options,
                       title = 'se scale position',
                       initval = ruler.position, action = rp.onesample.redraw,
                       grid = "controls", row = 0, column = 0)
         rp.slider(pnl, candidate,
                   min(mu, ttest$estimate - 4 * ttest$stderr),
                   max(mu, ttest$estimate + 4 * ttest$stderr),
                   action = rp.onesample.redraw,
                   showvalue = TRUE, grid = "controls", row = 1, column = 0)
         rp.checkbox(pnl, display, rp.onesample.redraw, title = 'display',
                     labels = display.names,
                     grid = "controls", row = 2, column = 0)
         rp.menu(pnl, data.display, list(list('Data display', 'none', 'density', 'histogram')),
                 action = rp.onesample.redraw)
      }
      else {
         rp.tkrplot(pnl, plot, rp.twosample,
                    hscale = hscale, vscale = vscale, 
                    grid = "picture", row = 0, column = 0, background = "white")
         rp.radiogroup(pnl, ruler.position, ruler.options,
                       title = 'se scale position',
                       initval = ruler.position, action = rp.ttest.redraw,
                       grid = "controls", row = 0, column = 0)
         rp.slider(pnl, candidate,
                   min(mu, ttest$estimate[1] - 4 * ttest$stderr),
                   max(mu, ttest$estimate[1] + 4 * ttest$stderr),
                   action = rp.ttest.redraw,
                   showvalue = TRUE, grid = "controls", row = 1, column = 0)
         rp.checkbox(pnl, display, rp.ttest.redraw, title = 'display',
                     labels = c('distribution', 'detail'),
                     grid = "controls", row = 2, column = 0)
         rp.menu(pnl, data.display, list(list('Data display', 'none', 'density', 'histogram')),
                 action = rp.ttest.redraw)
      }
      return(invisible(pnl))
   }
   else{
      pnl <- list(x = x, y = y, col = clrs, horizontal = horizontal,
                  method = method, height = height, mu = mu,
                  data.display = data.display, seed = seed,
                  candidate = candidate, display = display,
                  conf = attr(ttest$conf.int, 'conf.level'), static = !panel,
                  ruler.position = ruler.position, reference = reference,
                  xlab = xlab, ylab = ylab, vlab = vlab, nmin = 10, bgdcol = bgdcol,
                  distribution = 't', ttest = ttest)
      plt <- if (method %in% c('One', 'Paired')) rp.onesample(pnl)
             else rp.twosample(pnl)
      return(plt)
   }
   
}

rp.twosample <- function(lst) {
   
   x              <- lst$x
   y              <- lst$y
   xlab           <- lst$xlab
   ylab           <- lst$ylab
   vlab           <- lst$vlab
   horizontal     <- lst$horizontal
   data.display   <- lst$data.display
   violin         <- (data.display == 'violin')
   mns            <- lst$ttest$estimate
   se             <- lst$ttest$stderr
   mu             <- lst$ttest$null.value
   ruler.position <- lst$ruler.position
   reference      <- lst$reference
   se.scale       <- (ruler.position != 'none')
   height         <- lst$height
   clrs           <- lst$col
   lst            <- rp.sescale_ticks(lst)
   
   set.seed(lst$seed)
   
   # Ensure that x has values and y is a factor

   if (!is.numeric(x))
      stop('x must be numeric.')
   if (is.numeric(y)) {
      # Match the order with the way t.test calculates differences
      fac  <- factor(c(rep(ylab, length(y)),
                       rep(xlab, length(x))), levels = c(ylab, xlab))
      x    <- c(y, x)
      y    <- fac
      temp <- xlab
      xlab <- ylab
      ylab <- temp
      mns  <- rev(mns)
   }
   else if (is.character(y) | is.logical(y))
      y <- factor(y)
   else
      if (!is.factor(y)) stop('y must be character, logical or a factor.')
   if (length(x) != length(y))
      stop('x and y have different lengths.')
   if (nlevels(y) != 2)
      stop('y should have only two levels.')

   # Compute data densities
   
   fn             <- function(x) density(x, bw = bw.norm(x))
   dens           <- tapply(x, y, fn)
   if (data.display == 'histogram') {
      hst  <- tapply(x, y, hist, plot = FALSE)
      fn   <- function(x) max(x$density)
      dmax <- max(sapply(hst, fn))
   }
   else {
      if (length(x) >= lst$nmin)
         dmax <- max(dens[[1]]$y, dens[[2]]$y)
      else {
         fn   <- function(x) dnorm(0, 0, sd(x))
         dmax <- max(tapply(x, y, fn))
      }
   }
   lst$dmax <- dmax
   
   # Set up the y-axis labels and scaling
   
   ht   <- c(main = 0.6, axis = 0.7, margin = 0.2)
   lst$ht <- ht
   dscl <- ht['main'] / dmax
   # if (violin) dscl <- 0.5 * scl
   dpos <- 2 * ht['margin'] + ht['main']
   slo  <- 2.5 * ht['margin'] + ht['main'] + 0.05 * ht['axis']
   shi  <- slo + 0.2 * ht['axis']
   lst$shi <- shi
   apos <- dpos - 0.5 * ht['margin']
   alab <- '  difference\nscale'
   if (lst$data.display != 'none') {
      apos <- c(ht['margin'] + 0.5 * ht['main'],
                ht['margin'] * 3 + ht['main'] * 1.5 + ht['axis'],
                apos)
      alab <- c(xlab, ylab, alab)
   }
   if (se.scale) {
      apos <- c(apos, shi + 0.5 * ht['margin'])
      alab <- c(alab, 'se scale')
   }
   if (((lst$ruler.position != 'none') & lst$display['distribution']) |    
       ((lst$ruler.position %in% c('sample mean', 'reference')) &
         lst$display['detail'])) {
      apos <- c(apos, shi + 1.5 * ht['margin'])
      alab <- c(alab, 't-distribution')
   }
   if (lst$display['detail']) {
      if (lst$ruler.position == 'sample mean') {
         apos <- c(apos, slo + 0.25 * ht['margin'])
         alab <- c(alab, paste(round(attr(lst$ttest$conf.int, 'conf.level') * 100),
                               '%\nconfidence\ninterval', sep = ''))
      }
      else if (lst$ruler.position == 'reference') {
         apos <- c(apos, (dpos + slo + 0.5 * ht['margin']) / 2)
         alab <- c(alab, paste('p-value\n', signif(lst$ttest$p.value, 2), sep = ''))
      }
   }

   # Find suitable x-axis limits
   # ylimits <- c(0, 4 * ht['margin'] + 2 * ht['main'] + ht['axis'])
   xlimits <- range(mns)
   if (lst$data.display != 'none') xlimits <- range(xlimits, dens[[1]]$x, dens[[2]]$x)
   xlimits <- range(xlimits, mns - 4.1 * se, mns + 4.1 * se,
                    mns[1] + mu - 4.1 * se, mns[1] + mu + 4.1 * se)
   # Ensure there is space to place arrows beyond
   estlocs <- mns[2]
   if (lst$ttest$alternative == 'two.sided') {
      xlimits <- range(xlimits, mns[1] + mu - (mns[2] - mns[1] - mu))
      estlocs <- c(estlocs,     mns[1] + mu - (mns[2] - mns[1] - mu))
   }
   if (any(abs(estlocs - xlimits[1]) < 0.1 * diff(xlimits)))
      xlimits[1] <- xlimits[1] - 0.1 * diff(xlimits)
   if (any(abs(estlocs - xlimits[2]) < 0.1 * diff(xlimits)))
      xlimits[2] <- xlimits[2] + 0.1 * diff(xlimits)
   lst$xlimits <- xlimits
   
   # Plot setup
   
   dfrm <- data.frame(x, as.numeric(y))
   plt  <- ggplot2::ggplot(dfrm, ggplot2::aes(x, y)) +
      ggplot2::theme(
         panel.grid.major.y = ggplot2::element_blank(),
         panel.grid.minor.y = ggplot2::element_blank(),
         panel.grid.major.x = ggplot2::element_blank(),
         panel.grid.minor.x = ggplot2::element_blank()) +
      ggplot2::scale_y_continuous(breaks = apos, labels = alab) +
      ggplot2::xlab(vlab)
   if (!horizontal) plt <- plt + ggplot2::coord_flip() +
      ggplot2::theme(axis.title.x = ggplot2::element_blank())
   else             plt <- plt + ggplot2::theme(axis.title.y = ggplot2::element_blank())
   
   # Plot the data
   
   if (data.display != 'none') {
      for (i in 1:2) {
         plt <- rp.add_data_density(plt, data.display, x[y == levels(y)[i]], apos[i],
                                    ht['main'], hst[[i]], dens[[i]], dscl, 'grey85')
      }
   }
   
   # Add the difference scale
   
   xinp <- if (data.display == 'none') xlimits else x
   tpos <- pretty(xinp - mns[1])
   tlab <- as.character(tpos)
   tpos <- tpos + mns[1]
   tscl <- if (data.display == 'none') 0.03 * ht['axis'] else 0.05 * ht['axis']
   if (violin) yp <- yp + 0.5 * ht['axis']
   plt <- plt +
      ggplot2::annotate('segment', x = min(tpos), xend = max(tpos),
                        y = dpos, col = 'grey25') +
      ggplot2::annotate('segment', x = tpos, xend = tpos,
                        y = dpos, yend = dpos - tscl, col = 'grey25') +
      ggplot2::annotate('text', x = tpos, y = dpos - 2.5 * tscl,
                        label = tlab, col = 'grey25')
   
   # Add the uncertainty distribution
   
   if (ruler.position != 'none') {
      ucol <- switch(ruler.position, 'candidate' = 'grey75',
                  'sample mean' = clrs['estimate'], 'reference' = clrs['reference'])
      xpos <- switch(ruler.position, 'candidate' = lst$candidate,
                  'sample mean' = mns[2], 'reference' = mns[1] + mu)
      ulo  <- shi + 0.5 * ht['margin']
      uht  <- 2 * ht['margin'] + ht['main'] + ht['axis'] - ulo
      plt  <- rp.add_uncertainty(plt, xpos, ulo, uht,
                                 (dpos + slo + 0.5 * ht['margin']) / 2, lst)
   }
   
   # Plot the sample means, difference and reference, as required
   
   value     <- mns[2]
   linestart <- if (se.scale) shi else dpos
   lineend   <- 2.25 * ht['margin'] + ht['main'] + ht['axis']
   gclr      <- clrs['estline']
   group     <- 'sample mean difference'
   if (data.display != 'none') {
      value <- c(mns, value)
      linestart <- c(0.5 * ht['margin'],
                     2.75 * ht['margin'] + ht['main'] + ht['axis'],
                     linestart)
      lineend   <- c(1.25 * ht['margin'] +     ht['main'],
                     3.5  * ht['margin'] + 2 * ht['main'] + ht['axis'],
                     lineend)
      gclr      <- c(rep('black', 2), gclr)
      group     <- c(rep('sample mean', 2), group)
   }
   if (lst$reference | (ruler.position == 'reference')) {
      value     <- c(value,     mns[1] + mu)
      linestart <- c(linestart, linestart[3])
      lineend   <- c(lineend,   lineend[3])
      gclr      <- c(gclr,      clrs['refline'])
      group     <- c(group,     'reference')
   }
   if (ruler.position == 'candidate') {
      value     <- c(value, lst$candidate)
      linestart <- c(linestart, slo + 0.5 * ht['margin'])
      lineend   <- c(lineend,   slo)
      gclr      <- c(gclr,      'grey50')
      group     <- c(group,     'candidate')
   }
   names(gclr) <- group
   dfrm.lines <- data.frame(value, linestart, lineend, gclr, group)
   n.lines <- nrow(dfrm.lines)
   plt <- plt + ggplot2::geom_segment(ggplot2::aes(x = value, y = linestart, yend = lineend,
                                                   col = group), data = dfrm.lines) +
      ggplot2::scale_color_manual(name = '', breaks = group, values = gclr) +
      ggplot2::theme(legend.position = 'top')
   
   # Plot the se axis
   
   if (se.scale) {
      # if (violin) spos <- spos + 0.5 * ht['axis']
      xpos <- switch(ruler.position, 'candidate' = lst$candidate,
                     'sample mean' = mns[2], 'reference' = mns[1] + mu)
      sclr <- switch(ruler.position, 'candidate' = 'grey75',
                     'sample mean' = clrs['estline'], 'reference' = clrs['refline'])
      plt <- rp.add_sescale(plt, diff(mns), xpos, slo + 0.5 * ht['margin'],
                            shi + 0.5 * ht['margin'], lst)
   }
   
   # Print and return
   if (lst$static) return(plt)
   print(plt)
   lst
}

rp.onesample <- function(lst) {
   
   x           <- lst$x
   col.dens    <- lst$col['density']
   mu          <- lst$mu
   estimate    <- lst$ttest$estimate
   se          <- lst$ttest$stderr
   se.scale    <- (lst$ruler.position != 'none')
   reference   <- lst$reference
   hst         <- hist(x, plot = FALSE)
   dmax        <- max(hst$density)
   if (lst$display['detail']) lst$display['distribution'] <- TRUE
   set.seed(lst$seed)
   lst            <- rp.sescale_ticks(lst)
   
   # Set up the plot
   
   dens    <- density(x, bw = bw.norm(x))
   xlimits <- range(estimate)
   if (lst$data.display != 'none') xlimits <- range(xlimits, dens$x)
   xlimits <- range(xlimits, estimate - 4.1 * se, estimate + 4.1 * se,
                    mu - 4.1 * se, mu + 4.1 * se)
   # Ensure there is space to place arrows beyond
   estlocs <- estimate
   if (lst$ttest$alternative == 'two.sided') {
      xlimits <- range(xlimits, mu - (estimate - mu))
      estlocs <- c(estlocs, mu - (estimate - mu))
   }
   if (any(abs(estlocs - xlimits[1]) < 0.1 * diff(xlimits)))
      xlimits[1] <- xlimits[1] - 0.1 * diff(xlimits)
   if (any(abs(estlocs - xlimits[2]) < 0.1 * diff(xlimits)))
      xlimits[2] <- xlimits[2] + 0.1 * diff(xlimits)
   lst$xlimits <- xlimits
   
   dfrm <- data.frame(x)
   plt  <- ggplot2::ggplot(dfrm, ggplot2::aes(x)) +
         ggplot2::scale_x_continuous(limits = xlimits, expand = ggplot2::expansion()) +
         ggplot2::theme(
            axis.ticks.y       = ggplot2::element_blank(),
            axis.title.y       = ggplot2::element_blank(),
            panel.grid.major.x = ggplot2::element_blank(),
            panel.grid.minor.x = ggplot2::element_blank(),
            panel.grid.major.y = ggplot2::element_blank(),
            panel.grid.minor.y = ggplot2::element_blank()) +
          ggplot2::xlab(lst$vlab)

   # Plot the data
   
   if (lst$data.display != 'none') {
      if (lst$data.display == 'histogram') {
         plt <- plt +
            ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)),
                                    breaks = hst$breaks, col = col.dens, fill = col.dens)
      }
      else {
         if (length(x) >= lst$nmin) {
            xgrid  <- dens$x
            d.dens <- data.frame(xgrid, dgrid = dens$y)
            dmax   <- max(dens$y)
            dens.y <- approx(dens$x, dens$y, xout = x)$y
            plt <- plt +
               ggplot2::geom_ribbon(ggplot2::aes(x = xgrid, y = 0,
                                                 ymin = 0, ymax = dgrid),
                                    data = d.dens, col = NA, fill = col.dens, alpha = 0.25)
         }
         else {
            dens.y <- dnorm(x, mean(x), sd(x))
            dmax   <- dnorm(0, mean(x), sd(x))
         }
         d <- dens.y
         r <- runif(length(dens.y), 0, 1)
         d.densd <- data.frame(x, d, r)
         plt <- plt +
            ggplot2::geom_point(ggplot2::aes(x, r * d), data = d.densd,
                                size = 0.1 + 37 / max(length(x), 25))
      }
   }
   else
      dmax <- 1
   lst$dmax <- dmax

   # Plot the uncertainty
   cntr <- switch(lst$ruler.position, 'none' = NA, 'candidate' = lst$candidate,
                  'sample mean' = estimate, 'reference' = mu)
   if (lst$ruler.position != 'none')
      plt <- rp.add_uncertainty(plt, cntr, -dmax, 0.8 * dmax, -1.25 * dmax, lst)

   # Plot the sample mean
   
   linestart <- -dmax
   lineend   <- if (lst$data.display == 'none') -0.1 * dmax else 1.2 * dmax
   plt <- plt +
      ggplot2::annotate('segment', x = estimate, xend = estimate,
                        y = linestart, yend = lineend, col = lst$col['estline']) +
      ggplot2::annotate('text', x = estimate, y = lineend + 0.05 * dmax,
                        hjust = hjst(estimate, xlimits, 0.25),
                        label = 'sample mean', col = lst$col['estline'])
   
   # Plot reference if requested
   
   if (lst$reference | (lst$ruler.position == 'reference')) {
      rlineend <- if (lst$data.display == 'none') -0.15 * dmax else 1.05 * dmax
      plt  <- plt +
         ggplot2::annotate('segment', x = mu, xend = mu,
                           y = linestart, yend =  rlineend, col = lst$col['refline']) +
         ggplot2::annotate('text', x = mu, y = rlineend + 0.05 * dmax, label = 'reference',
                           hjust = hjst(mu, xlimits, 0.25), col = lst$col['refline'])
   }
   
   # Plot the se axis
   
   if (se.scale) {
      plt  <- rp.add_sescale(plt, estimate, cntr, -1.1 * dmax, -dmax, lst)
      if (lst$ruler.position == 'candidate') {
         sclr   <- 'grey75'
         tscl   <- 0.105 * dmax
         plt <- plt +
            ggplot2::annotate('text', x = cntr, y = -1.1 * dmax + 9.5 * tscl,
                              col = sclr, label = 'candidate mean',
                              hjust = hjst(cntr, lst$xlimits, 0.25)) +
            ggplot2::annotate('segment', x = cntr, xend = cntr,
                              y = -1.1 * dmax + 9 * tscl, yend = -1.1 * dmax, col = sclr)
      }
   }
   
   # Add the y-axis labels
   
   ticks <- c(0.5, -0.5, -1, -1.25) * dmax
   ylabs <- c('             data', 't-distribution', 'se scale',
              paste(round(attr(lst$ttest$conf.int, 'conf.level') * 100),
                    '%\nconfidence\ninterval', sep = ''))
   if (lst$ruler.position == 'reference')
      ylabs[4] <- paste('p-value\n', signif(lst$ttest$p.value, 2), sep = '')
   if (lst$data.display == 'none') {
      ylabs[1] <- paste0(rep(' ', 20), collapse = '')
      ticks[1] <- -0.1 * dmax  
   }
   tind  <- 1
   if (lst$display['distribution'])  tind <- c(tind, 2)
   if (lst$ruler.position != 'none') tind <- c(tind, 3)
   if (lst$display['detail'] & (lst$ruler.position %in% c('sample mean', 'reference')))
      tind <- c(tind, 4)
   yhi <- ifelse(lst$data.display == 'none', 0, 1.3 * dmax)
   plt <- plt +
      ggplot2::scale_y_continuous(breaks = ticks[tind], labels = ylabs[tind],
                                  limits = c(-1.3 * dmax, yhi))

   if (lst$static) return(plt)
   print(plt)
   lst
}

rp.onesample.redraw <- function(panel) {
   rp.tkrreplot(panel, 'plot')
   panel
}

rp.ttest.redraw <- function(panel) {
   rp.tkrreplot(panel, 'plot')
   panel
}

rp.add_data_density <- function(plt, data.display, x, ypos, yht, hst, dens, scl, col) {
   
   violin <- (data.display == 'violin')
   if (!violin) ypos <- ypos - 0.5 * yht
   if (data.display == 'histogram') {
      # plt <- plt +
      #    ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)),
      #                            breaks = hst$breaks, col = col.dens, fill = col)
      ngp <- length(hst$density)
      xlo <- rev(rev(hst$breaks)[-1])
      xhi <- hst$breaks[-1]
      ylo <- rep(ypos, length(hst$breaks) - 1)
      yhi <- ypos + hst$density * scl
      dfrm.h <- data.frame(x   = rep(mean(x), ngp), y = rep(ypos, ngp),
                           xlo = xlo, xhi = xhi, ylo = ylo, yhi = yhi)
      plt <- plt +
         ggplot2::geom_rect(ggplot2::aes(xmin = xlo, xmax = xhi, ymin = ylo, ymax = yhi),
                            col = 'grey50', fill = col, data = dfrm.h)
   }
   else {
      sgn     <- if (violin) sign(rbinom(length(x), 1, 0.5) - 0.5) else 1
      xgrid   <- dens$x
      d.dens  <- data.frame(xgrid = xgrid, dgrid = dens$y)
      dens.y  <- approx(dens$x, dens$y, xout = x)$y
      d       <- dens.y
      r       <- runif(length(dens.y), 0, 1)
      d.densd <- data.frame(x = x, d = d, sgn = sgn, r = r)
      dr.lo   <- if (violin) ypos - d.dens$dgrid * scl else ypos
      plt <- plt +
         ggplot2::geom_ribbon(ggplot2::aes(x = xgrid, y = 0,
                                           ymin = dr.lo, ymax = ypos + dgrid * scl),
                              data = d.dens, col = NA, fill = col) +
         ggplot2::geom_point(ggplot2::aes(x, ypos + sgn * r * d * scl),
                             size = 0.1 + 37 / max(length(x), 25), data = d.densd)
   }

   plt
}

rp.sescale_ticks <- function(lst) {
   
   # Find the tick points and labels for the se scale
   
   se       <- lst$ttest$stderr
   estimate <- if (lst$method == 'Two') diff(lst$ttest$estimate)
               else lst$ttest$estimate
   sedist   <- if (lst$ruler.position == 'reference') (estimate - lst$mu) / se
               else if (lst$reference) (lst$mu - estimate) / se
               else NULL
   if (!is.null(sedist))
      sedist <- 2 * (ceiling(abs(sedist) / 2)) * sign(sedist)
   serange <- range(-4, 4, sedist)
   if (!is.null(sedist) & (lst$display['distribution'] | lst$display['detail']) & 
       (lst$ruler.position == 'reference') & lst$ttest$alternative == 'two.sided')
      serange <- range(serange, -sedist)
   if (max(abs(serange)) <= 8) {
      tpos    <- (serange[1]:serange[2]) * se
      tlab    <- seq(serange[1], serange[2], by = 2)
   }
   else{
      tlab <- pretty(serange)
      tpos <- tlab * se
   }
   lst$tpos <- tpos
   lst$tlab <- tlab
   lst
}

rp.add_sescale <- function(plt, estimate, xpos, ylo, yhi, lst) {
   
   tpos   <- xpos + lst$tpos
   tlab   <- lst$tlab
   sclr   <- switch(lst$ruler.position, 'candidate' = 'grey50',
                        'reference' = lst$col['refline'],
                        'sample mean' = lst$col['estline'])
   tscl   <- 0.10 * (yhi - ylo)
   drtpos <- tpos[2] - tpos[1]
   plt  <- plt +
      ggplot2::annotate('rect',
                        xmin = max(min(tpos) - 0.2 * drtpos, lst$xlimits[1]),
                        xmax = min(max(tpos) + 0.2 * drtpos, lst$xlimits[2]),
                        ymin = ylo, ymax = yhi,
                        fill = 'white') +
      ggplot2::annotate('segment',
                        x    = max(min(tpos), lst$xlimits[1]),
                        xend = min(max(tpos), lst$xlimits[2]),
                        y = ylo, yend = ylo, col = sclr) +
      ggplot2::annotate('segment', x = tpos, xend = tpos,
                        y = ylo, yend = ylo + tscl, col = sclr) +
      ggplot2::annotate('text', x = xpos + tlab * lst$ttest$stderr,
                        y = (ylo + yhi) / 2, col = sclr,
                        label = as.character(tlab)) +
      ggplot2::annotate('segment', x = tpos, xend = tpos,
                        y = yhi, yend = yhi - tscl, col = sclr) +
      ggplot2::annotate('segment',
                        x    = max(min(tpos), lst$xlimits[1]),
                        xend = min(max(tpos), lst$xlimits[2]),
                        y = yhi, col = sclr)
   plt
}

rp.add_uncertainty <- function(plt, xpos, ypos, yht, cipos, lst) {
   estimate <- lst$ttest$estimate
   se       <- lst$ttest$stderr
   dmax     <- lst$dmax
   xlimits  <- lst$xlimits
   ucol     <- switch(lst$ruler.position, 'none' = NA, 'candidate' = 'grey80',
                      'reference'   = lst$col['reference'],
                      'sample mean' = lst$col['estimate'])
   ulcol    <- switch(lst$ruler.position, 'none' = NA, 'candidate' = 'grey80',
                      'reference'   = lst$col['refline'],
                      'sample mean' = lst$col['estline'])
   d.fn     <- ifelse (lst$distribution == 't',
                    function(x) dt((x - xpos) /se, df = lst$ttest$parameter),
                    function(x) dnorm(x, xpos, se))
   scl      <- yht / dt(0, lst$ttest$parameter)
   if (lst$display['distribution'] | lst$display['detail']) {
      ngrid <- 100
      xgrid <- seq(xpos - 4 * se, xpos + 4 * se, length = ngrid)
      ymin  <- as.numeric(ypos)
      dens  <- d.fn(xgrid)
      dgrid <- data.frame(xgrid, ymin = ymin, dens = dens)
      plt <- plt +
         ggplot2::geom_ribbon(ggplot2::aes(x = xgrid, y = ymin,
                                           ymin = ymin, ymax = ypos + dens * scl),
                              data = dgrid, col = NA, fill = ucol)
   }
   # Add notches to the uncertainty distribution
   if (lst$display['detail']) {
      q.fn       <- ifelse (lst$distribution == 't',
                            function(x) xpos + se * qt(x, df = lst$ttest$parameter),
                            function(x) qnorm(x, xpos, se))
      notch.p    <- c(0.5 - lst$conf / 2, 0.5 + lst$conf / 2)
      notch.x    <- q.fn(notch.p)
      notch.y    <- ypos + d.fn(notch.x) * scl
      notch.dfrm <- data.frame(notch.x = notch.x, notch.y = notch.y)
      plt <- plt + ggplot2::geom_segment(ggplot2::aes(x = notch.x, y = notch.y, yend = ypos),
                                         col = lst$col['notch'], data = notch.dfrm)
   }
   # Add the confidence interval
   if (lst$display['detail'] & lst$ruler.position == 'sample mean') {
      ci.ends   <- lst$ttest$conf.int
      i         <- 1
      satisfied <- FALSE
      while (!satisfied) {
         i   <- i + 1
         sci <- signif(ci.ends, i)
         satisfied <- (abs(diff(ci.ends) - diff(sci)) / diff(ci.ends) < 0.01)
      }
      lbl <- paste(' ', as.character(signif(lst$ttest$conf.int, i)), ' ', sep = '')
      if (lst$method == 'Two') ci.ends <- ci.ends + estimate[2]
      plt <- plt +
         ggplot2::annotate('segment',
                           x = ci.ends[1], xend = ci.ends[2],
                           y = cipos, col = ulcol) +
         ggplot2::annotate('text',
                           x = ci.ends, y = rep(cipos, 2),
                           label = lbl, col = ulcol, hjust = c('right', 'left'))
   }
   # Add the p-value calculation
   if (lst$display['detail'] & lst$ruler.position == 'reference') {
      xstart  <- estimate[1]
      plimits <- xpos + range(lst$tpos)
      xstop   <- switch(lst$ttest$alternative, greater = plimits[2], less = plimits[1],
                        two.sided = ifelse(estimate > xpos, plimits[2], plimits[1]))
      xstop   <- max(xlimits[1], min(xstop, xlimits[2]))
      p       <- ifelse(lst$ttest$alternative == 'two.sided',
                        lst$ttest$p.value / 2, lst$ttest$p.value)
      p       <- as.character(signif(p, 2))
      hjust   <- ifelse (xstop > xstart, 'left', 'right')
      plt <- plt +
         ggplot2::annotate('segment', y = cipos,
                           x = xstart, xend = xstop, col = ulcol,
                           arrow = ggplot2::arrow(length = ggplot2::unit(0.1, "inches"))) +
         ggplot2::annotate('text', x = xstart, y = cipos + 0.08 * dmax,
                           label = p, hjust = hjust, col = ulcol) +
         ggplot2::annotate('text', x = xpos, y = cipos - 0.05 * dmax,
                           label = 'probability', col = ulcol)
      if (lst$ttest$alternative == 'two.sided') {
         xstart    <- xpos - (estimate[1] - xpos)
         xstop     <- ifelse(xstop > mean(plimits), plimits[1], plimits[2])
         xstop     <- max(xlimits[1], min(xstop, xlimits[2]))
         hjust     <- ifelse(hjust == 'left', 'right', 'left')
         if (lst$method == 'One') {
            linestart <- -dmax
            lineend   <- if (lst$data.display == 'none') 0.1 * dmax else 1.2 * dmax
         }
         else if (lst$method == 'Two') {
            linestart <- lst$shi
            lineend   <- 2.25 * lst$ht['margin'] + lst$ht['main'] + lst$ht['axis']
         }
         plt <- plt +
            ggplot2::annotate('segment', y = cipos,
                              x = xstart, xend = xstop, col = ulcol,
                              arrow = ggplot2::arrow(length = ggplot2::unit(0.1, "inches"))) +
            ggplot2::annotate('text', x = xstart, y = cipos + 0.08 * dmax,
                              label = p, hjust = hjust, col = ulcol) +
            ggplot2::annotate('segment', x = xstart, y = linestart, yend = lineend,
                              col = lst$col['estline'], linetype = 'dashed')
      }
   }
   plt
}

hjst <- function(x, xlimits, edge) {
   hjst <- (x - xlimits[1]) / diff(xlimits)
   if (hjst < edge)     hjst <- 2 * hjst
   if (hjst > 1 - edge) hjst <- 1 - 2 * (1 - hjst)
   hjst
}
