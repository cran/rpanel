rp.regression3 <- function (model, prng, col) {

   if (!requireNamespace("denstrip", quietly = TRUE))
      stop("the denstrip package is not available.")

	 if (class(model) == "formula") model <- lm(model, x = TRUE)
	 if (is.na(col)) {
   	  col <- if (requireNamespace("colorspace", quietly = TRUE)) colorspace::rainbow_hcl(2)[2]
   	         else grey(0.5)
   }
   cfs   <- coef(model)[-1]
   p     <- length(cfs)
   x     <- model$x[ , -1]
   rng   <- diff(apply(x, 2, range))
   chng  <- cfs * rng
   se    <- coef(summary(model))[-1, 2] * rng
   ylab  <- attr(model$terms, "variables")
   ylab  <- strsplit(deparse(ylab), ",")[[1]][1]
   ylab  <- substr(ylab, 6, nchar(ylab))
   if (any(is.na(prng))) prng <- range(chng + 3 * se, chng - 3 * se)
   par(mar = c(3, 3, 1, 1) + 0.1, mgp = c(1.5, 0.2, 0), tcl = -0.2)
   plot(c(0.4, p + 0.6), prng, ylab = paste("Change in", ylab), xlab = "",
   		 type = "n", axes = FALSE)
   usr <- par("usr")
   rect(usr[1], usr[3], usr[2], usr[4], col = grey(0.9), border = NA)
   abline(h = axTicks(2), col = "white")
   abline(h = 0, lty = 2, lwd = 2)
   # grid(col = "white", lty = 1)
   axis(1, 1:p, names(cfs), tick = FALSE, lwd = 0, mgp = c(3, 0, 0))
   rng <- signif(apply(x, 2, range))
   rng <- paste(rng[1, ], "-", rng[2, ])
   axis(1, 1:p, rng,        tick = FALSE, lwd = 0, mgp = c(3, 1, 0), cex.axis = 0.7)
   axis(2, lwd = 0, lwd.ticks = 2, col = grey(0.6), col.ticks = grey(0.6),
   		 col.axis = grey(0.6), cex.axis = 0.8)
   xgrid <- seq(usr[3], usr[4], length = 500)
   for (i in 1:p) denstrip::denstrip(xgrid, dnorm(xgrid, chng[i], se[i]), i, 0.7,
                              colmax = col, colmin = "transparent", horiz = FALSE)
   # segments(usr[1], usr[3], usr[1], usr[4], lwd = 2)
   invisible()
}
