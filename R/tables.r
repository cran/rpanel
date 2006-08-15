#   Statistical tables

rp.tables <- function(panel.plot = FALSE) {

# Action functions begin
#####################################

tables.draw <- function(tables) {
  with(tables, {
    xobs <- as.numeric(xobs)
    prob <- as.numeric(prob)
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
      ylim   <- c(0, 0.4)
      pval   <- pchisq(xobs, degf1)
      pshade <- min(pval, 1 - pval)
      qts    <- qchisq(c(prob, 1 - prob, prob/2, 1 - prob/2, pshade, 1 - pshade), degf1)
    }
    if (distribution == "F") {
      xrange <- c(0.01, 10)
      x      <- seq(min(xrange[1], xobs * 1.1), max(xrange[2], xobs * 1.1), length = ngrid)
      dens   <- df(x, degf1, degf2)
      ylim   <- c(0, 1)
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
    title(title.text)
  })
  tables
}

tables.redraw <- function(object) {
  rp.tkrreplot(object, plot)
  object
}

#####################################
# Action functions end

  panel.name <- rp.panelname()

  if (panel.plot && require(tkrplot)) {
    tables.panel <- rp.control("Distributions", size = c(625, 700),
                    realname = panel.name,
                    xobs = 1, prob = 0.05, distribution = "Normal", degf1 = 5, degf2 = 30)
    tables.panel <- rp.radiogroup(tables.panel, distribution, c("Normal", "t", "Chi-squared", "F"),
                    title = "Distribution", action = tables.redraw, pos = c(25, 15, 100, 120))
    tables.panel <- rp.doublebutton(tables.panel, degf1, 1, range = c(1, NA),
                    title = "df1", action = tables.redraw, pos = c(140, 10, 100, 50))
    tables.panel <- rp.doublebutton(tables.panel, degf2, 1, range = c(1, NA),
                    title = "df2", action = tables.redraw, pos = c(240, 10, 100, 50))
    tables.panel <- rp.checkbox(tables.panel, observed.value.showing,
                    title = "Show observed value", action = tables.redraw, pos = c(140, 50, 160, 25))
    tables.panel <- rp.textentry(tables.panel, xobs, tables.redraw, "Observed value",
                    pos = c(140, 75, 160, 25))
    tables.panel <- rp.textentry(tables.panel, prob, tables.redraw, "Fixed probability, p",
                    pos = c(140, 105, 160, 25))
    tables.panel <- rp.radiogroup(tables.panel, tail.area,
                    c("none", "from observed value", "fixed probability"),
                    title = "Tail probability", action = tables.redraw, pos = c(340, 15, 150, 120))
    tables.panel <- rp.radiogroup(tables.panel, tail.direction,
                    c("lower", "upper", "two-sided"),
                    title = "Tail direction", action = tables.redraw, pos = c(500, 15, 100, 120))
    tables.panel <- rp.tkrplot(tables.panel, plot, plotfun = tables.draw,
                    pos = c(0, 150, 625, 525))
  }
  else {
    if (panel.plot) rp.messagebox("Package TkrPlot was not available, falling back to external plot window.") 
    tables.panel <- rp.control("Distributions", size = c(625, 150),
                    realname = panel.name,
                    xobs = 1, prob = 0.05, distribution = "Normal", degf1 = 5, degf2 = 30)                    
    tables.panel <- rp.radiogroup(tables.panel, distribution, c("Normal", "t", "Chi-squared", "F"),
                    title = "Distribution", action = tables.draw, pos = c(25, 15, 100, 120))
    tables.panel <- rp.doublebutton(tables.panel, degf1, 1, range = c(1, NA),
                    title = "df1", action = tables.draw, pos = c(140, 10, 100, 50))
    tables.panel <- rp.doublebutton(tables.panel, degf2, 1, range = c(1, NA),
                    title = "df2", action = tables.draw, pos = c(240, 10, 100, 50))
    tables.panel <- rp.checkbox(tables.panel, observed.value.showing,
                    title = "Show observed value", action = tables.draw, pos = c(140, 50, 160, 25))
    tables.panel <- rp.textentry(tables.panel, xobs, tables.draw, "Observed value",
                    pos = c(140, 75, 160, 25))
    tables.panel <- rp.textentry(tables.panel, prob, tables.draw, "Fixed probability, p",
                    pos = c(140, 105, 160, 25))
    tables.panel <- rp.radiogroup(tables.panel, tail.area,
                    c("none", "from observed value", "fixed probability"),
                    title = "Tail probability", action = tables.draw, pos = c(340, 15, 150, 120))
    tables.panel <- rp.radiogroup(tables.panel, tail.direction,
                    c("lower", "upper", "two-sided"),
                    title = "Tail direction", action = tables.draw, pos = c(500, 15, 100, 120))
    rp.do(tables.panel, tables.draw)
  }
}
