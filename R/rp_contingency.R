#     A new go at graphics for a contingency table

rp.contingency <- function(x, uncertainty = 'none', proportion.scale = 'fixed', cols) {
   
   if (!requireNamespace("ggplot2", quietly = TRUE))
      stop("the ggplot2 package is not available.")
   if (length(proportion.scale) != 1)
      stop("'proportion.scale' should be of length 1.")
   if (!(uncertainty %in% c('none', 'shading', 'violin')))
      stop("'uncertainty' setting not recognised.")
   # if (length(values) != 1)
   #    stop("'values' should be of length 1.")
   # if (!all(values %in% c('observed', 'proportions', 'expected')))
   #    stop("'values' setting not recognised.")
   if (!all(proportion.scale %in% c('fixed', 'free')))
      stop("'proportion.scale' setting not recognised.")
   if (any(is.na(x)))
      stop("missing data are not allowed.")
   if (!any(class(x) %in% c("matrix", "table")))
      stop("the input should be a matrix or a table.")
   if (length(dim(x)) != 2)
      stop("the input should be two-dimensional.")
   is.wholenumber <-
      function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol
   if (!all(is.wholenumber(c(x))))
      stop("the input should consist only of integer counts.")
   if (nrow(x) < 2 | ncol(x) < 2)
      stop("there must be at least two rows and columns.")
   if (proportion.scale == 'free') proportion.scale <- 'free_y'
   clrs <- if (missing(cols)) rp.colours() else rp.colours(cols)
   
   nrw      <- nrow(x)
   ncl      <- ncol(x)
   sep_x    <- 0
   sep_y    <- 0.02
   csums    <- colSums(x)
   rsums    <- rowSums(x)
   p        <- sweep(x, 2, csums, "/")
   rnms     <- factor(rep(rownames(p), ncl), levels = rownames(p))
   cnms     <- factor(rep(colnames(p), each = nrw), levels = colnames(p))
   expected <- round(outer(rsums, csums) / sum(x), 1)
   # lbl      <- switch(values, "proportions" = c(signif(p, 2)),
   #                            "observed"    = c(x),
   #                            "expected"    = c(expected))
   lbl      <- c(x)
   wdth     <- rep(csums / max(csums), each = ncl)
   shading  <- (uncertainty == 'shading')
   lstart   <- if (shading)  0.5 - wdth / 2 else 0.3
   lstop    <- if (shading)  0.5 + wdth / 2 else 0.7
   d        <- data.frame(p = c(p), x = c(x), rnms, cnms,
                          wdth, lstart, lstop, lbl = lbl, row.names = NULL)

   plt <- ggplot2::ggplot(d) +
          ggplot2::geom_rect(ggplot2::aes(xmin = 0.5 - wdth / 2, xmax = 0.5 + wdth / 2,
                                          ymin = 0, ymax = p), fill = 'grey75', col = 'grey75') +
          ggplot2::geom_text(ggplot2::aes(x = 0.5, y = min(p) / 2, label = lbl)) +
          ggplot2::ylab('proportion within column') +
          ggplot2::facet_grid(rnms ~ cnms, switch = 'y', scales = proportion.scale) +
          ggplot2::scale_y_continuous(position = "right", expand = c(0, 0.05 * max(p))) +
          ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(),
                         panel.grid.minor.x = ggplot2::element_blank(),
                         panel.grid.major.y = ggplot2::element_blank(),
                         panel.grid.minor.y = ggplot2::element_blank(),
                         # panel.spacing.y = ggplot2::unit(1, "lines"),
                         axis.title.x = ggplot2::element_blank(),
                         axis.text.x  = ggplot2::element_blank(),
                         axis.ticks.x = ggplot2::element_blank())
                         # panel.background = ggplot2::element_rect(fill = 'white', colour = 'white'))

   if (uncertainty != 'none') {
      # Add horizontal lines to highlight the observed proportions
      plt   <- plt + ggplot2::geom_segment(ggplot2::aes(x = lstart, xend = lstop, y = p),
                                           col = clrs['estline'], linewidth = 1,
                                           data = d)
      # Display the common pattern of proportions
      xc     <- rep(0.5, nrw * ncl)
      wdth   <- rep(1,   nrw * ncl)
      common <- rep(rsums / sum(x), ncl)
      # As in graphics for uncertainty paper
      # se    <- sqrt(common / (rep(csums, each = nrw) * nrw * ncl))
      # Equivalent to adjusted Pearson residuals
      se     <- sqrt((common / rep(csums, each = nrw)) *
                                 (1 - rep(csums, each = nrw) / sum(x)) *
                                 (1 - rep(rsums, ncl) / sum(x)))
      # For checking against chisq.test$stdres
      # stdres <- (p - common) / se
      # print(stdres)
      ngrid  <- 101
      dlim   <- 3
      ygrid  <- seq(-dlim, dlim, length = ngrid)
      dens   <- rep(dnorm(ygrid), nrw)
      fn     <- function(x) common[x] + ygrid * se[x]
      ind    <- 1:(nrw * ncl)
      y      <- c(sapply(ind, fn))
      wdth   <- rep(wdth, each = ngrid)
      hght   <- rep((ygrid[2] - ygrid[1]) * se, each = ngrid)
      dgrd <- data.frame(x = rep(xc, each = ngrid), y, wdth, hght,
                         rnms = rep(rnms, each = ngrid), cnms = rep(cnms, each = ngrid))
      if (shading) {
         alpha      <- dens * 0.7 / max(dens)
         dgrd$alpha <- alpha
         plt  <- plt +
            ggplot2::geom_tile(ggplot2::aes(x, y, width = wdth, height = hght, alpha = alpha),
                               fill = clrs['reference'], data = dgrd) +
            ggplot2::scale_alpha(guide = 'none')
      }
      else {
         # This line needs to be altered to scale the densities
         dscl      <- 0.3 / max(dens)
         dgrd$wdth <- dens * dscl
         plt  <- plt +
            ggplot2::geom_tile(ggplot2::aes(x, y, width = wdth, height = hght),
                               fill = clrs['reference'], data = dgrd)
      }
      # Add notches
      xstart <- if (shading) rep(0, 2)
                else rep(xc[ind] - dnorm(2) * dscl / 2, 2)
      xend   <- if (shading) rep(1, 2)
                else rep(xc[ind] + dnorm(2) * dscl / 2, 2)
      ltype  <- if (shading)  'dashed' else 'solid'
      dfrm   <- data.frame(x = xstart, xend = xend,
                           rnms = rep(rnms, 2), cnms = rep(cnms, 2),
                           y = c(common + 2 * se, common - 2 * se))
      clr    <- 'white'
      plt <- plt + ggplot2::geom_segment(ggplot2::aes(x = x, y = y, xend = xend),
                              col = clr, linetype = ltype,
                              data = dfrm)
    }
   
   # scales <- FALSE
   # if (!scales)
   #    plt <- plt + ggplot2::theme(axis.text.y = ggplot2::element_blank(),
   #                                axis.ticks.y = ggplot2::element_blank())
   
   print(plt)
   return(invisible(plt))
}
