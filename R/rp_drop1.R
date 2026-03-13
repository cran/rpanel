#     Anova on a regression model

rp.drop1 <- function(model, subset.terms, p.reference = c(0.05, 0.01), cols) {
   
   tbl     <- drop1(model, test = 'F')[-1, ]
   # if (!missing(subset.terms) | is.null(subset.terms)) {
   if (!missing(subset.terms)) {
      if (!is.character(subset.terms))
         stop('subset.terms is not character mode.')
      else
         subset.terms <- match(subset.terms, rownames(tbl))
      if (length(subset.terms) == 0)
         stop('no terms remain after subsetting.')
      tbl <- tbl[subset.terms, ]
   }
   clrs <- if (missing(cols)) rp.colours() else rp.colours(cols)
   tbl$df  <- tbl$Df
   tbl$Df  <- paste('Model terms with', tbl$Df, 'df')
   Fvalue  <- tbl$'F value'
   fmax    <- 1.1 * max(qf(1 - p.reference, tbl$df, model$df.residual), Fvalue)
   ngrid   <- 100
   fgrid   <- seq(0, fmax, length = ngrid)
   fht     <- numeric(0)
   udf     <- unique(tbl$df)
   for (degf in udf)
     fht <- c(fht, df(fgrid, degf, model$df.residual))
   maxfht  <- max(fht[!is.infinite(fht)])
   fht     <- fht * 0.75 / maxfht
   fhtlo   <- -0.75
   fhthi   <-  0.75
   fht     <- fhtlo + fht * (fhthi - fhtlo) / maxfht
   p.reference <- p.reference[!is.null(p.reference)]
   p.reference <- p.reference[!is.na(p.reference)]
   qvals   <- if (length(p.reference) == 0) 0.95 else 1 - p.reference
   curv.df <- data.frame(fht, fgrid = rep(fgrid, length(udf)),
                         Df = paste('Model terms with', rep(udf, each = ngrid), 'df'))
      
   plt <- ggplot2::ggplot(tbl, ggplot2::aes(Fvalue, rownames(tbl))) +
      ggplot2::geom_ribbon(ggplot2::aes(x = fgrid, ymin = fhtlo, ymax = fht),
                           fill = clrs['reference'], col = clrs['reference'],
                           inherit.aes = FALSE, data = curv.df) +
      ggplot2::geom_point() +
      ggplot2::xlab('F-value') +
      ggplot2::ylab('Model terms') +
      ggplot2::xlim(0, fmax) +
      ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(),
                     panel.grid.minor.x = ggplot2::element_blank())
   
   # Add a p-value scale and mark reference points
   if (!is.null(p.reference)) {
      ypos  <- seq(0.8, 0.9, length = length(p.reference))
      Df    <- paste('Model terms with', rep(udf, each = length(qvals)), 'df')
      qtls  <- qf(rep(qvals, length(udf)), rep(udf, each = length(qvals)), model$df.residual)
      qvals <- rep(qvals, length(udf))
      y     <- rep(ypos * fhtlo + (1 - ypos) * fhthi, length(udf))
      lbl   <- rep(paste('p:', as.character(p.reference), ' ', sep = ''), length(udf))
      p.df <- data.frame(Df, qvals, y, lbl)
      plt <- plt +
         ggplot2::geom_vline(ggplot2::aes(xintercept = qtls), col = grey(0.7), linetype = 2, data = p.df) +
         ggplot2::geom_text(ggplot2::aes(x = qtls, y = y, label = lbl), data = p.df,
                           col = grey(0.7), size = 3, hjust = 1)
      }
   
   # Use facets when there are multiple degrees of freedom
   if (length(unique(tbl$Df)) > 1) {
      # Use the ggforce package, if available, to scale the height of the rows
      if (requireNamespace('ggforce', quietly = TRUE))
         plt <- plt + ggforce::facet_col(Df ~ ., scales = 'free_y', space = 'free')
      else
         plt <- plt + ggplot2::facet_wrap(Df ~ ., scales = 'free_y') 
   }
   
   print(plt)
   plt

}
