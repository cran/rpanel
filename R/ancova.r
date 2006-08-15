#   ANCOVA

rp.ancova <- function(x, y, group, panel = TRUE, model = "None",
                  xlab = deparse(substitute(x)), ylab = deparse(substitute(y))) {
                    
#check arguments here just in case the function below causes any accidental wierdness
  if(missing(x) || missing(y) || missing(group)){
    stop("rp.ancova requires you to specify x, y, and group.  See help(rp.ancova) or, to see an example, run example(rp.ancova).")
  }

#define the action function here.  It finishes at the other line of #####
#############################################################################
rp.ancova.draw <- function(ancova) {
   with(ancova, {
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
           pval <- drop1(lm.model, test = "F")[["Pr(F)"]][2]
           pval <- round(pval, 3)
           title.text <- paste(title.text, "\n", "Test of equal groups:", pval)
         }
         if (model == "Different lines") {
           pval <- drop1(lm.model, test = "F")[["Pr(F)"]][2]
           pval <- round(pval, 3)
           title.text <- paste(title.text, "\n", "Test of parallelism:", pval)
         }
         for (i in 1:n.groups) {
            ind  <- (group == levels(group)[i])
            xgp  <- x[ind]
            fgp  <- fitted(lm.model)[ind]
            ind1 <- order(xgp)
            lines(xgp[ind1[range(ind1)]], fgp[ind1[range(ind1)]], col = i, lty = i)
         }
      }
      title(title.text)
   }) #end of with statement; the '})' is correct.
   ancova
}

########################################################################################

   

   if (panel) {
      ancova.panel <- rp.control("One-way ancova", y = y, x = x, group = group, xlab = xlab, ylab = ylab)
      rp.radiogroup(ancova.panel, model,
                      c("None", "Single mean", "Single line",
                        "Parallel lines", "Different lines"),
                      action = rp.ancova.draw)
      rp.ancova.draw(rp.panel(ancova.panel))
      invisible()
   }
   else {
      panel <- list(x = x, y = y, group = group, 
                         xlab = xlab, ylab = ylab, model = model)   
      rp.ancova.draw(panel)
      invisible()
   }
}
