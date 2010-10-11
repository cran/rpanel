#   Statistical tables

rp.tables <- function(panel.plot = TRUE, hscale = NA, vscale = hscale) {
	
   if (is.na(hscale)) {
      if (.Platform$OS.type == "unix") hscale <- 1
      else                             hscale <- 1.4
      }
   if (is.na(vscale)) 
      vscale <- hscale

tables.draw <- function(tables) {
  with(tables, {
    xobs <- as.numeric(xobs.prob[1])
    prob <- as.numeric(xobs.prob[2])
    ngrid <- 100
    if (distribution == "Normal") {
      xrange <- c(-4, 4)
      x      <- seq(min(xrange[1], xobs * 1.1), max(xrange[2], xobs * 1.1), length = ngrid)
      dens   <- dnorm(x)
      ylim   <- c(0, 0.4)
      pval   <- pnorm(xobs)
      pshade <- min(pval, 1 - pval)
      qts    <- qnorm(c(prob, 1 - prob, prob/2, 1 - prob/2, pshade, 1 - pshade))
    }
    if (distribution == "t") {
      xrange <- c(-4, 4)
      x      <- seq(min(xrange[1], xobs * 1.1), max(xrange[2], xobs * 1.1), length = ngrid)
      dens   <- dt(x, degf1)
      ylim   <- c(0, 0.4)
      pval   <- pt(xobs, degf1)
      pshade <- min(pval, 1 - pval)
      qts    <- qt(c(prob, 1 - prob, prob/2, 1 - prob/2, pshade, 1 - pshade), degf1)
#     xp   <- if (two.sided) qt(c(1 - prob/2, prob/2), degf)     else qt(1 - prob, degf)
#     pval <- if (two.sided) 2 * (1 - pt(abs(xobs), degf)) else 1 - pt(xobs, degf)
    }
    if (distribution == "Chi-squared") {
      xrange <- c(0.01, degf1 + 3 * sqrt(2 * degf1))
      x      <- seq(min(xrange[1], xobs * 1.1), max(xrange[2], xobs * 1.1), length = ngrid)
      dens   <- dchisq(x, degf1)
      if (degf1 <= 2) ylim   <- c(0, max(dens))
        else          ylim   <- c(0, 1.1 * max(dens))
      pval   <- pchisq(xobs, degf1)
      pshade <- min(pval, 1 - pval)
      qts    <- qchisq(c(prob, 1 - prob, prob/2, 1 - prob/2, pshade, 1 - pshade), degf1)
    }
    if (distribution == "F") {
      xrange <- c(0.01, 10)
      x      <- seq(min(xrange[1], xobs * 1.1), max(xrange[2], xobs * 1.1), length = ngrid)
      dens   <- df(x, degf1, degf2)
      if (degf1 <= 2) ylim   <- c(0, max(dens))
        else          ylim   <- c(0, 1.1 * max(dens))
      pval   <- pf(xobs, degf1, degf2)
      pshade <- min(pval, 1 - pval)
      qts    <- qf(c(prob, 1 - prob, prob/2, 1 - prob/2, pshade, 1 - pshade), degf1, degf2)
    }
    plot(x, dens, type = "l", ylim = ylim, ylab = paste(distribution, "density"))
    abline(h = 0, lty = 3)
    if (distribution == "Normal")       title.text <- "Normal distribution"
    if (distribution == "t")            title.text <- paste("t(", degf1,") distribution", sep = "")
    if (distribution == "Chi-squared")  title.text <- paste("Chi-squared(", degf1,
                                                            ") distribution", sep = "")
    if (distribution == "F")            title.text <- paste("F(", degf1,",", degf2,
                                                            ") distribution", sep = "")
    xobs <- as.numeric(xobs)
    if (observed.value.showing & !is.na(xobs)) {
      lines(rep(xobs, 2), c(0, ylim[2] * 0.95), lty = 2)
      text(xobs, ylim[2] * 0.97, signif(xobs, 4))
      title.text <- paste(title.text, ";  xobs =", round(xobs, 2))
    }

    if (tail.area != "none") {
      if (tail.area == "fixed probability") {
        if      (tail.direction == "lower") shade <- c(qts[1], NA)
        else if (tail.direction == "upper") shade <- c(NA, qts[2])
        else                                shade <- qts[3:4]
        title.text <- paste(title.text, ";  p =", round(prob, 3))
      }
      else {
        if      (tail.direction == "lower") shade <- c(xobs, NA)
        else if (tail.direction == "upper") {
          shade <- c(NA, xobs)
          pval  <- 1 - pval
        }
        else {
          shade <- qts[5:6]
          pval  <- 2 * min(pval, 1 - pval)
        }
        title.text <- paste(title.text, ";  pval =", round(pval, 3))
      }
      if (!is.na(shade[1])) {
        ind  <- max((1:ngrid)[x < shade[1]])
        intp <- (shade[1] - x[ind]) / (x[ind + 1] - x[ind])
        dend <- (1 - intp) * dens[ind] + intp * dens[ind + 1]
        dt   <- c(dens[x < shade[1]], dend)
        xt   <- c(x[x < shade[1]], shade[1])
        polygon(c(xt, rev(xt)), c(dt, rep(0, length(xt))),
          density = -1, col = "red", border = "red")
      }
      if (!is.na(shade[2])) {
        ind  <- min((1:ngrid)[x > shade[2]])
        intp <- (shade[2] - x[ind - 1]) / (x[ind] - x[ind - 1])
        dend <- (1 - intp) * dens[ind - 1] + intp * dens[ind]
        dt <- c(dend, dens[x > shade[2]])
        xt <- c(shade[2], x[x > shade[2]])
        polygon(c(xt, rev(xt)), c(dt, rep(0, length(xt))),
          density = -1, col = "red", border = "red")
      }
    }
    title(title.text, cex.main = 1)
  })
  tables
  }

tables.redraw <- function(object) {
  rp.tkrreplot(object, plot)
  object
  }

  tables.panel <- rp.control("Distributions",
                  xobs.prob = c(1, 0.05), distribution = "Normal", degf1 = 5, degf2 = 30,
                  observed.value.showing = FALSE, tail.area = "none", tail.direction = "lower")
  if (panel.plot && require(tkrplot)) {
    tables.panel <- rp.tkrplot(tables.panel, plot, plotfun = tables.draw,
                    hscale = hscale, vscale = vscale, row = 1, column = 0, columnspan = 4,
                    sticky = "ew")
    action.fn <- tables.redraw
    }
  else
    action.fn <- tables.draw
  tables.panel <- rp.radiogroup(tables.panel, distribution, c("Normal", "t", "Chi-squared", "F"),
                  title = "Distribution", action = action.fn, row = 0, column = 0, sticky = "ns")
  rp.grid(tables.panel, "dfgrid", row = 0, column = 1, sticky = "ns")
  tables.panel <- rp.doublebutton(tables.panel, degf1, 1, range = c(1, NA),
                  title = "df1", action = action.fn, grid = "dfgrid", row = 0, column = 0)
  tables.panel <- rp.doublebutton(tables.panel, degf2, 1, range = c(1, NA),
                  title = "df2", action = action.fn, grid = "dfgrid", row = 0, column = 1)
  tables.panel <- rp.checkbox(tables.panel, observed.value.showing,
                  title = "Show observed value", action = action.fn,
                  grid = "dfgrid", row = 1, column = 0, columnspan = 2)                    
  tables.panel <- rp.textentry(tables.panel, xobs.prob, action.fn, 
                  c("Observed value", "Fixed probability, p"), title = "",
                  grid = "dfgrid", row = 2, column = 0, columnspan = 2, sticky = "ns")                    
  tables.panel <- rp.radiogroup(tables.panel, tail.area,
                  c("none", "from observed value", "fixed probability"),
                  title = "Tail probability", action = action.fn, row = 0, column = 2, sticky = "ns")
  tables.panel <- rp.radiogroup(tables.panel, tail.direction,
                  c("lower", "upper", "two-sided"),
                  title = "Tail direction", action = action.fn, row = 0, column = 3, sticky = "ns")
  }