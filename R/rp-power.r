rp.power <- function() {

#define the various action functions here
###########################################################################
powerplot.pars <- function(powerplot) {
   se              <- powerplot$sigma * 2 / sqrt(powerplot$ngrid)
   powerplot$pow   <- 1 - pnorm(2 - abs(powerplot$mu2 - powerplot$mu1) / se)
   powerplot.draw(powerplot)
   }

powerplot.draw <- function(powerplot) {
   powerplot$n <- max(powerplot$n, min(powerplot$ngrid))
   powerplot$n <- min(powerplot$n, max(powerplot$ngrid))
   with(powerplot, {
      if (populations.showing == "TRUE") par(mfrow = c(2, 1))
      plot(ngrid, pow, type = "n", ylim = c(0, 1), 
            xlab = "Sample size", ylab = "Power")
      lines(ngrid, pow, col = "blue")
      lines(c(n, n, min(ngrid)), c(0, rep(pow[ngrid == n], 2)), lty = 2)
      title("Power tool", col.main = "red", line = 3)
      title(paste(  "mu1:",   format(round(mu1,   3),           nsmall = 2), 
                  "  mu2:",   format(round(mu2,   3),           nsmall = 2),
                  "  sigma:", format(round(sigma, 3),           nsmall = 2)),
                  line = 2, cex.main = 0.85)
      title(paste("n:", n,
                  "  Power:", format(round(pow[ngrid == n], 3), nsmall = 3)),
                  line = 1, cex.main = 0.85, col.main = "blue")
      if (populations.showing == "TRUE") {
         plot(range(xgrid), c(0, popdens.lim), type = "n", 
              xlab = "x", ylab = "Density")
         lines(xgrid, dnorm(xgrid, mu1, sigma))
         lines(xgrid, dnorm(xgrid, mu2, sigma))
         }
      par(mfrow = c(1, 1))
      })
   powerplot
   }

#############################################################################

   power.panel <- rp.control("Power tool",
                     ngrid = seq(10, 300), mu1 = 0, mu2 = 1,
                     sigma = 1, n = 20, xgrid = seq(- 4, 5, length = 100),
                     popdens.lim = 0.7)
   rp.doublebutton(power.panel, n, 1, title = "n", action = powerplot.draw)
   rp.doublebutton(power.panel, mu1, 0.01, title = "mu1", action = powerplot.pars)
   rp.doublebutton(power.panel, mu2, 0.01, title = "mu2", action = powerplot.pars)
   rp.doublebutton(power.panel, sigma, 0.01, title = "sigma", action = powerplot.pars)
   rp.checkbox(power.panel, populations.showing, title = "Show populations", action = powerplot.draw)
   rp.do(power.panel,powerplot.pars)

}
