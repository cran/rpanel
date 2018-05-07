rp.sample <- function(mu = 0, sigma = 1, n = 25, panel.plot = TRUE,
                      hscale = NA, vscale = hscale) {

   if (is.na(hscale)) {
      if (.Platform$OS.type == "unix") hscale <- 1
      else                             hscale <- 1.4
      }
   if (is.na(vscale)) 
      vscale <- hscale


   sample.draw <- function(panel) {
   	  mu    <- panel$pars["mu"]
   	  sigma <- panel$pars["sigma"]
   	  n     <- panel$pars["n"]
   	  col.pars <- "green3"
      with(panel, {
      	 par(mfrow = c(2, 1), mar = c(2, 1.5, 1, 1) + 0.1, mgp = c(1, 0.2, 0), tcl = -0.2,
      	     yaxs = "i")
         hst <- hist(y, plot = FALSE,
               breaks = seq(min(y, mu - 3 * sigma), max(y, mu + 3 * sigma), length = 20))
         plot(mu + c(-3, 3) * sigma, c(0, 1 / sigma),
               type = "n", axes = FALSE, xlab = "Data", ylab = "")
         usr <- par("usr")
         rect(usr[1], usr[3], usr[2], usr[4], col = grey(0.9), border = NA)
         grid(col = "white", lty = 1)
         axis(1, lwd = 0, lwd.ticks = 2, col = grey(0.6), col.ticks = grey(0.6),
                 col.axis = grey(0.6), cex.axis = 0.8)
         # axis(2, lwd = 0, lwd.ticks = 2, col = grey(0.6), col.ticks = grey(0.9),
                 # col.axis = grey(0.6), cex.axis = 0.8)
         if (display.sample["data"]) {
            hist(y, probability = TRUE, add = TRUE,
                 col = grey(0.5), border = grey(0.9),
                 breaks = seq(min(y, mu - 3 * sigma), max(y, mu + 3 * sigma), length = 20),
                 axes = FALSE, ylab = "", main = "")
            ind <- which(hst$density > usr[4])
            if (length(ind) > 0)
               segments(hst$breaks[ind], usr[4], hst$breaks[ind + 1], usr[4], col = "red", lwd = 3)
            ind <- any(hst$density > 0 & hst$breaks[-1] < usr[1])
            if (ind) segments(usr[1], 0, usr[1], usr[4], col = "red", lwd = 3)
            ind <- any(hst$density > 0 & hst$breaks[-length(hst$breaks)] > usr[2])
            if (ind) segments(usr[2], 0, usr[2], usr[4], col = "red", lwd = 3)
         }
         if (display.sample["population"]) {
            xgrid <- seq(usr[1], usr[2], length = 100)
            lines(xgrid, dnorm(xgrid, mu, sigma), col = "blue", lwd = 2)
         }
         if (display.sample["mean"])
            lines(rep(mu, 2), c(0, 0.9 / sigma), col = col.pars, lwd = 2)
         if (display.sample["+/- 2 st.dev."])
            arrows(mu - 2 * sigma, 0.8 / sigma,
                   mu + 2 * sigma, 0.8 / sigma,
                   length = 0.05, code = 3, col = col.pars, lwd = 2)
         if (any(display.mean)) {
            hst <- hist(mns, plot = FALSE,
                        breaks = seq(mu - 3 * sigma, mu + 3 * sigma, length = 50))
            plot(mu + c(-3, 3) * sigma, c(0, sqrt(n) / sigma),
                        type = "n", axes = FALSE, xlab = "Sample mean", ylab = "")
            usr <- par("usr")
            rect(usr[1], usr[3], usr[2], usr[4], col = grey(0.9), border = NA)
            grid(col = "white", lty = 1)
            axis(1, lwd = 0, lwd.ticks = 2, col = grey(0.6), col.ticks = grey(0.6),
                        col.axis = grey(0.6), cex.axis = 0.8)
            if (any(display.mean["sample mean"])) {
               hist(mns, probability = TRUE, add = TRUE,
                   breaks = seq(mu - 3 * sigma, mu + 3 * sigma, length = 50),
                   axes = FALSE, col = grey(0.5), border = grey(0.9), ylab = "", main = "")
            }
            if (display.sample["mean"])
               lines(rep(mu, 2), c(0, 0.9 * usr[4]), col = col.pars, lwd = 2)
            if (display.mean["+/- 2 se"])
               arrows(mu - 2 * sigma / sqrt(n), 0.8 * usr[4],
                      mu + 2 * sigma / sqrt(n), 0.8 * usr[4],
                      length = 0.05, code = 3, col = col.pars, lwd = 2)
            if (display.mean["distribution"]) {
               xgrid <- seq(usr[1], usr[2], length = 200)
               lines(xgrid, dnorm(xgrid, mu, sigma / sqrt(n)), col = "blue", lwd = 2)
            }
         }
      	 par(mfrow = c(1, 1))         
      })
      panel
   }
   
   sample.redraw <- function(panel) {
      rp.tkrreplot(panel, plot)
      panel
   }
   
   sample.changepars <- function(panel) {
      panel$mns <- NULL
      rp.control.put(panel$panelname, panel)
      rp.do(panel, sample.new)
      panel
   }

   sample.new <- function(panel) {
      panel$y <- rnorm(panel$pars["n"], panel$pars["mu"], panel$pars["sigma"])
      if (!panel$display.mean["accumulate"]) panel$mns <- NULL
      panel$mns <- c(mean(panel$y), panel$mns)
      rp.control.put(panel$panelname, panel)
      if (panel$pplot) rp.tkrreplot(panel, plot) else sample.draw(panel)
      panel
   }

   display.sample        <- c(TRUE, rep(FALSE, 3))
   display.mean          <- rep(FALSE, 4)
   names(display.sample) <- c("data", "population", "mean", "+/- 2 st.dev.")
   names(display.mean)   <- c("sample mean", "accumulate", "+/- 2 se", "distribution")
   pars                  <- c(mu, sigma, n)
   names(pars)           <- c("mu", "sigma", "n")
   y                     <- rnorm(n, mu, sigma)
   panel <- rp.control(pars = pars, y = y, mns = mean(y),
                       display.sample = display.sample, display.mean = display.mean,
                       pplot = panel.plot)
   if (panel.plot) {
      rp.tkrplot(panel, plot, sample.draw, pos = "right", hscale = hscale, vscale = vscale)
      action.fn <- sample.redraw
   }
   else {
      action.fn <- sample.draw
      rp.do(panel, action.fn)
   }
   rp.textentry(panel, pars, sample.changepars, width = 10,
               c("mean", "st.dev.", "sample size"), c("mu", "sigma", "n"))
   rp.button(panel, sample.new, "Sample")
   rp.checkbox(panel, display.sample, action.fn, names(display.sample), title = "Sample")
   rp.checkbox(panel, display.mean,   action.fn, names(display.mean),   title = "Sample mean")

}
