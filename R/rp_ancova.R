#   Analysis of covariance: one covariate and one factor

rp.ancova <- function(x, y, group, panel = TRUE, panel.plot = TRUE,
                      model = NA, xlab, ylab, glab,
                      hscale = NA, vscale = hscale, style = 'ggplot') {
                    
   if(missing(x) || missing(y) || missing(group))
      stop('rp.ancova requires x, y, and group.')

   if (style == 'ggplot' & !requireNamespace('ggplot2', quietly = TRUE))
      stop('the ggplot2 package is not available.')
   
   if (missing(xlab)) xlab <- deparse(substitute(x))
   if (missing(ylab)) ylab <- deparse(substitute(y))
   if (missing(glab)) glab <- deparse(substitute(group))

   group <- factor(group)
   ind   <- !is.na(x + y + as.numeric(group))
   x     <- x[ind]
   y     <- y[ind]
   group <- group[ind]
         
   if (is.na(hscale)) hscale <- 1
   if (is.na(vscale)) vscale <- hscale
   
   if (style == 'ggplot') {
      dfrm <- data.frame(y = y, x = x, group = group)
      return(rp.lm(y ~ x + group, ylab = ylab, xlab = xlab, zlab = glab,
                   panel = panel, data = dfrm))
   }
   else {
      return(rp.ancova.old(x, y, group, panel = panel, panel.plot = panel.plot,
                           model = model, xlab = xlab, ylab = ylab,
                           hscale = hscale, vscale = vscale))
   }

}

rp.ancova.old <- function(x, y, group, panel = TRUE, panel.plot = TRUE, model = "None",
                          xlab = deparse(substitute(x)), ylab = deparse(substitute(y)),
                          hscale = NA, vscale = hscale) {
   
   if (any(is.na(model))) model <- "None"

   rp.ancova.old.draw <- function(panel) {
      with(panel, {
         group    <- factor(group)
         n.groups <- length(levels(group))
         plot(x, y, type = "n", xlab = xlab, ylab = ylab)
         for (i in 1:n.groups)
            points(x[group == levels(group)[i]],
                   y[group == levels(group)[i]], col = i, pch = i)
         ind   <- (!is.na(x) & !is.na(y) & !is.na(group))
         x <- x[ind]
         y <- y[ind]
         group <- group[ind]
         if      (model == "Single mean")     lm.model <- lm(y ~ 1)
         else if (model == "Single line")     lm.model <- lm(y ~ x)
         else if (model == "Parallel lines")  lm.model <- lm(y ~ group + x)
         else if (model == "Different lines") lm.model <- lm(y ~ group * x)
         title.text <- paste("Model:", model)
         if (model == "Single mean")
            abline(h = coef(lm.model))
         else if (model == "Single line")
            abline(coef(lm.model))
         else if (!(model == "None")) {
            if (model == "Parallel lines") {
              pval <- drop1(lm.model, test = "F")[["Pr(>F)"]][2]
              pval <- round(pval, 3)
              title.text <- paste(title.text, "\n", "Test of equal groups:", pval)
            }
            if (model == "Different lines") {
              pval <- drop1(lm.model, test = "F")[["Pr(>F)"]][2]
              pval <- round(pval, 3)
              title.text <- paste(title.text, "\n", "Test of parallelism:", pval)
            }
            for (i in 1:n.groups) {
               ind  <- (group == levels(group)[i])
               xgp  <- x[ind]
               fgp  <- fitted(lm.model)[ind]
               ind1 <- order(xgp)
               lines(xgp[ind1[range(ind1)]], fgp[ind1[range(ind1)]], col = i, lty = i, lwd = 2)
            }
         }
         title(title.text, cex.main = 1)
      })
      panel
   }

   rp.ancova.old.redraw <- function(panel) {
      rp.tkrreplot(panel, plot)
      panel
      }

   if (panel) {
      panel <- rp.control("One-way ancova", y = y, x = x, z = group,
                                 xlab = xlab, ylab = ylab)
      if (panel.plot) {
         rp.tkrplot(panel, plot, rp.ancova.old.draw, pos = "right",
                    hscale = hscale, vscale = vscale)
         action.fn <- rp.ancova.old.redraw
         }
      else
         action.fn <- rp.ancova.old.draw
      rp.radiogroup(panel, model,
         c("None", "Single mean", "Single line", "Parallel lines", "Different lines"),
         action = action.fn)
      rp.do(panel, action.fn)
      }
   else {
      panel <- list(x = x, y = y, z = group, xlab = xlab, ylab = ylab, model = model)   
      rp.ancova.old.draw(panel)
      }
   invisible(panel)
}
