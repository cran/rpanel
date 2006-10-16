#	An rpanel function for rmplot

rmplot.draw <- function(panel) {
   with(panel, {
      if (fac.showing) fac1 <- fac else fac1 <- NA
      if (data.range)
         rmplot(y, timept = timept, fac = fac1, type = type,
                    xlab = xlab, ylab = ylab, ylim = range(y, na.rm = TRUE))
      else
         rmplot(y, timept = timept, fac = fac1, type = type, xlab = xlab, ylab = ylab)
      if (case.showing) {
         rmplot(matrix(y[case,], nrow = 1), timept = timept, fac = fac1[case], type = "all",
                    xlab = xlab, ylab = ylab, add = TRUE, lwd = 3)
         title(paste("case =", case))
         }
      })
   panel
   }

rp.rmplot <- function(y, id = NA, timept = NA, fac = NA, xlab = NA, ylab = NA) {

   if (is.na(xlab)) xlab <- "Time"
   if (is.na(ylab)) ylab <- deparse(substitute(y))

   result <- rmplot(y, id, timept, fac, type = "none")
   y      <- result$ymat
   fac    <- result$fac
   times  <- result$times

   panel.name   <- rp.panelname()
   rmplot.panel <- rp.control("Repeated measurements", y = y, timept = times, fac = fac,
                      xlab = xlab, ylab = ylab, realname = panel.name)
   rmplot.panel <- rp.radiogroup(rmplot.panel, type, c("all", "mean", "mean+bar", "band"),
                      title = "Display", action = rmplot.draw)
   if (!all(is.na(fac)))
      rmplot.panel <- rp.checkbox(rmplot.panel, fac.showing, rmplot.draw,
                      title = "Show groups", initval = TRUE)
   rmplot.panel <- rp.checkbox(rmplot.panel, case.showing, rmplot.draw,
                      title = "Show cases")
   rmplot.panel <- rp.doublebutton(rmplot.panel, case, 1, range = c(1, nrow(y)),
                      title = "Case", action = rmplot.draw)
   rmplot.panel <- rp.checkbox(rmplot.panel, data.range, rmplot.draw,
                      title = "Use data range", initval = TRUE)
   rmplot.draw(rmplot.panel)

   assign(panel.name, rmplot.panel, env = .GlobalEnv)
   invisible(panel.name)
   }
