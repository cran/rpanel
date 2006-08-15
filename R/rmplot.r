#   An rpanel function for rmplot

rp.rmplot <- function(y, id = NA, timept = NA, fac = NA, xlab = NA, ylab = NA) {
# define the rmplot function:
################################################################
rmplot <- function(y, id = NA, timept = NA, fac = NA, type = "all", add = FALSE, ...)

#   Plots of repeated measurements data

   {

   if (!is.na(fac) && !is.factor(fac)) stop("fac must be a factor.")

   if (is.matrix(y) | is.data.frame(y))
      result <- rmplot.table(y, fac, type, timept, add = add, ...)
   
   else {

      if (all(is.na(fac))) fac <- factor(rep(1, length(y)))
      if (!is.factor(id )) stop("id  must be a factor.")

      n     <- length(id)
      ids   <- unique(id)
      times <- sort(unique(timept))
      facs  <- unique(fac)
      nid   <- length(ids)
      nt    <- length(times)
      nfac  <- length(facs)

      ymat   <- matrix(NA, nrow = nid, ncol = nt)
      id.fac <- factor(rep(NA, nid), levels = levels(fac))
      for (i in (1:n)) {
        rowind               <- (1:nid)[id[i] == ids]
        colind               <- (1:nt)[timept[i] == times]
        ymat[rowind, colind] <- y[i]
        if (!is.na(id.fac[rowind]) & (fac[i] != id.fac[rowind]))
           stop("Mismatch between id and fac.")
        id.fac[rowind] <- fac[i]
        }

      result <- rmplot.table(ymat, times = times, fac = id.fac, type = type, ...)
      # result <- list(id = ids, ymat = ymat, times = times, fac = id.fac)
      }

   invisible(result)

   }

rmplot.table <- function(y, fac = NA,
                  type = "all", times = (1:ncol(y)),
          xlab = deparse(substitute(times)),
          ylab = deparse(substitute(y)), add = FALSE, lwd = 1, ...)
   {

#       RMPLOT: plots of repeated measurements data

   if (all(is.na(fac))) fac <- factor(rep(1, nrow(y)))
   nt      <- ncol(y)
   facs    <- unique(fac)
   nfac    <- length(facs)
   nlevels <- length(levels(fac))     
    
#--------------------------------------------------------------     
#       Plot individual profiles
#--------------------------------------------------------------     

   if(type == "all") {
      if (!add) plot(range(times), range(y, na.rm = TRUE),
                     type = "n", xlab = xlab, ylab = ylab, ...)
      for(i in 1:nrow(y)) {
         if (!is.na(fac[i])) {
            ifac <- (1:nlevels)[fac[i] == levels(fac)]
            if (!all(is.na(y[i,]))) {
           lines(times, y[i,  ], col = ifac, lty = ifac, lwd = lwd)
           #   Plot isolated points
           yi <- c(NA, y[i, ], NA)
           ind <- (diff(diff(is.na(yi))) == 2)
           if (any(ind)) points(times[ind], y[i, ind], col = ifac)
           }
            }}
      }

#--------------------------------------------------------------
#       Calculate means and standard errors, if necessary
#--------------------------------------------------------------     

   if(!(type == "all")) {
      m  <- matrix(NA, nfac, nt)
      se <- matrix(NA, nfac, nt)
      for(i in 1:nfac) {
         # yi      <- y[fac == facs[i], ]
         yi      <- y[fac == levels(fac)[i], ]
         fun     <- function(y) length(y[!is.na(y)])
     ni      <- apply(yi, 2, fun)
     ni      <- max(ni, 1)
         m[i,  ] <- apply(yi, 2, mean, na.rm = TRUE)
         se[i, ] <- apply(yi, 2,   sd, na.rm = TRUE) / sqrt(ni)
         }
      }
   shift <- ((1:nfac) - 0.5 - nfac / 2) * diff(range(times)) / 200

#--------------------------------------------------------------
#       Plot mean profiles
#--------------------------------------------------------------     
        
   if(type == "mean") {
      if (!add) plot(range(times),
                     # range(m),
                     range(m, m - 2 * se, m + 2 * se, na.rm = TRUE),
                     type = "n", xlab = xlab, ylab = ylab, ...)
      for(i in 1:nfac) {
         # ifac <- (1:nlevels)[facs[i] == levels(fac)]
         ifac <- i
         lines(times + shift[i], m[i,  ], col = ifac, lty = ifac)
         if (nfac > 1) text(times + shift[i], m[i,  ], ifac, col = ifac)
         }
      }
     
#--------------------------------------------------------------     
#       Plot mean profiles + 2 s.e. bars, possibly also with band
#--------------------------------------------------------------  
   
   if(type == "band") {
      if (nlevels < 2)
         stop("Band cannot be drawn with missing means or se's.")
      if (any(is.na(m + se)))
         stop("Band cannot be drawn with only one group")
      if (!add) plot(range(times),
                     range(m, m - 2 * se, m + 2 * se, na.rm = TRUE),
                     type = "n", xlab = xlab, ylab = ylab, ...)
      hi <- rep(0, nt)
      lo <- rep(0, nt)
      for(j in 1:nt) {
         av    <- mean(c(m[1, j], m[2, j]))
         width <- sqrt(se[1, j]^2 + se[2, j]^2)
         hi[j] <- av + width
         lo[j] <- av - width
         }
      polygon(c(times, rev(times)), c(hi, rev(lo)), 
               density = -1, border = NA, col = "red")
      }

   if ((type == "band") | (type == "mean+bar")) {
      if (type == "mean+bar" & !add)
         plot(range(times), 
              range(m, m - 2 * se, m + 2 * se, na.rm = TRUE), 
              type = "n", xlab = xlab, ylab =  ylab, ...)
      for (i in 1:nfac) {
         # ifac <- (1:nlevels)[facs[i] == levels(fac)]
         ifac <- i
         lines(times + shift[i], m[i,  ], col = ifac, lty = ifac)
         if (nfac > 1) text( times + shift[i], m[i,  ], i, 
                            col = ifac)
         if (type == "mean+bar")
        segments(times + shift[i], m[i, ] - 2 * se[i, ],
                 times + shift[i], m[i, ] + 2 * se[i, ],
                     col = ifac, lty = ifac)
         }
      }
#--------------------------------------------------------------     
#       Scan individual cases
#--------------------------------------------------------------        

   if (type == "scan") {
      choices <- c("Next", "Previous", "Select", "Complete", "Stop")
      par(col = 1, lty = 1)
      plot(range(times), range(y, na.rm = TRUE), type = "n",
                xlab = xlab, ylab = ylab, ...)
      rmplot.table(y = y, fac = fac, type = "mean", times = times,
                xlab = xlab, ylab = ylab, add = TRUE, ...)

      plot.profile <- function(i, times, y, fac, erase = FALSE) {
         if (!is.na(fac[i])) {
            ifac <- (1:nlevels)[fac[i] == levels(fac)]
            if (!all(is.na(y[i,]))) {
           if (erase) lines(times, y[i,  ], col = "white", lty = ifac)
           else       lines(times, y[i,  ], col = ifac,    lty = ifac)
           }
            }
         if (erase) title(paste("Case", i), cex = 0.5, col.main = "white")
         else       title(paste("Case", i), cex = 0.5)
     }

      i <- 1
      direction <- 1
      plot.profile(i, times, y, fac)
      choice <- menu(choices)
      while (choice < 5) {
         plot.profile(i, times, y, fac, erase = TRUE)
         if (choice == 2) direction <- -1 else direction <- 1
         i <- i + direction
         if(choice == 3) i <- scan(what = integer())
         if(i < 1)       i <- nrow(y)
         if(i > nrow(y)) i <- 1
         plot.profile(i, times, y, fac)
         if(choice < 4 | i == nrow(y)) choice <- menu(choices)
         }
      }

   invisible(list(ymat = y, fac = fac, times = times))
}

################################################################
# define the action function:
############################################
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
##################################################################

   if (is.na(xlab)) xlab <- "Time"
   if (is.na(ylab)) ylab <- deparse(substitute(y))

   result <- rmplot(y, id, timept, fac, type = "none")
   y      <- result$ymat
   fac    <- result$fac
   times  <- result$times

   rmplot.panel <- rp.control("Repeated measurements", y = y, timept = times, fac = fac,
                      xlab = xlab, ylab = ylab)
   rp.radiogroup(rmplot.panel, type, c("all", "mean", "mean+bar", "band"),
                      title = "Display", action = rmplot.draw)
   if (!all(is.na(fac)))
      rmplot.panel <- rp.checkbox(rmplot.panel, fac.showing, rmplot.draw,
                      title = "Show groups", initval = TRUE)
   rp.checkbox(rmplot.panel, case.showing, rmplot.draw,
                      title = "Show cases")
   rp.doublebutton(rmplot.panel, case, 1, range = c(1, nrow(y)),
                      title = "Case", initval=1, action = rmplot.draw)
   rp.checkbox(rmplot.panel, data.range, rmplot.draw,
                      title = "Use data range", initval = TRUE)
   rp.do(rmplot.panel,rmplot.draw)

   invisible(rmplot.panel)
}

