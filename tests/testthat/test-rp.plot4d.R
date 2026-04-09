#     Tests for the rp.plot4d and rp.spacetime functions

# library(devtools)
# library(testthat)
# load_all()

#----------------------------------------------------------------
#     Simulated data
#----------------------------------------------------------------

test_that('Standard call', {
   n <- 1400
   z <- 1:n
   x <- cbind(long = 2 * z /n + rnorm(n), lat = 2 * z /n + rnorm(n))
   y <- 4 * z / n + rnorm(n)
   expect_no_error(pnl <- rp.plot4d(x, z, y))
   rp.control.dispose(pnl)
   expect_no_error(pnl <- rp.plot4d(x, z, y, coords = c(0, 0)))
   rp.control.dispose(pnl)
   expect_no_error(pnl <- rp.plot4d(x, z, y, retain.location.plot = TRUE))
   rp.control.dispose(pnl)
   expect_no_error(pnl <- rp.plot4d(x, z, y, location.plot = FALSE))
   rp.control.dispose(pnl)
   expect_no_error(pnl <- rp.plot4d(cbind(x[,1], x[,2]/2), z, y, eqscplot = TRUE))
   rp.control.dispose(pnl)
   expect_no_error(pnl <- rp.spacetime(x, z, y))
   rp.control.dispose(pnl)
   expect_no_error(pnl <- rp.plot4d(x, z))
   rp.control.dispose(pnl)
})

#----------------------------------------------------------------
#     Quake data
#----------------------------------------------------------------

position <- with(quakes, cbind(long, lat))

test_that('Standard call', {
   expect_no_error(pnl <- rp.plot4d(position, quakes$depth))
   rp.control.dispose(pnl)
   expect_no_error(pnl <- rp.plot4d(position, quakes$depth, quakes$mag))
   rp.control.dispose(pnl)
})


#----------------------------------------------------------------
#     Using a model
#----------------------------------------------------------------

# This inexplicably fails on some platforms.
# Error in xord[, 1] : incorrect number of dimensions
# 
# test_that('Standard call', {
   # n       <- 1400
   # z       <- 1:n
   # x       <- cbind(long = 2 * z /n + rnorm(n), lat = 2 * z /n + rnorm(n))
   # y       <- 4 * z / n + rnorm(n)
   # ngrid   <- 30
   # sq      <- seq(-2, 4, length = ngrid)
   # model.x <- cbind(sq, sq)
   # model.z <- seq(1, 1400, length = ngrid)
   # model.y <- array(dim = rep(ngrid, 3))
   # for (i in 1:ngrid)
   #    for (j in 1:ngrid)
   #       for (k in 1:ngrid)
   #          model.y[i, j, k] <- model.x[i, 1]^2 / 15 + model.x[j, 2]^2 / 15 +
   #    5 * model.z[k] / 1400
   # model   <- list(x = model.x, y = model.y, z = model.z)
#    
#    expect_no_error(pnl <- rp.plot4d(x, y, z))
#    rp.control.dispose(pnl)
#    expect_no_error(pnl <- rp.plot4d(x, z, y, model))
#    rp.control.dispose(pnl)
#    expect_no_error(rp.plot4d(x, z, y, model, panel = FALSE, z.window.pars = c(z0 = 800, zsd = 30)))
# })

#----------------------------------------------------------------
#     SO2 over Europe
#----------------------------------------------------------------

# 
# with(SO2, {
# location <- cbind(longitude, latitude)
# mapxy       <- maps::map('world', plot = FALSE,
#                      xlim = range(SO2$longitude), 
#                      ylim = range(SO2$latitude))
# 
# rp.plot4d(location, year, logSO2,
#           col.palette = rev(heat.colors(30)))
# rp.plot4d(location, year, logSO2,
#             col.palette = rev(heat.colors(30)),
#             foreground.plot = function() map(mapxy, add = TRUE))
# 
# location1 <- location[,1]
# location2 <- location[,2]
# model <- mgcv::gam(logSO2 ~ s(location1, location2, year))
# loc1  <- seq(min(location1), max(location1), length = 30)
# loc2  <- seq(min(location2), max(location2), length = 30)
# yr    <- seq(min(year), max(year), length = 30)
# newdata <- expand.grid(loc1, loc2, yr)
# names(newdata) <- c("location1", "location2", "year")
# model <- predict(model, newdata)
# model <- list(x = cbind(loc1, loc2), z = yr,
#               y = array(model, dim = rep(30, 3)))
# 
# rp.plot4d(location, year, logSO2, model,
#             col.palette = rev(heat.colors(30)),
#             foreground.plot = function() map(mapxy, add = TRUE))
# })
# 
# mdl <- model
# mdl$y[1:10, 20:30, ] <- NA
# 
# rp.plot4d(location, year, logSO2, mdl,
#             col.palette = rev(heat.colors(30)),
#             foreground.plot = function() map(mapxy, add = TRUE))
# 
# 
# #     DO in Clyde
# 
# data(Clyde)
# attach(Clyde)
# 
# source("rp-colour-key.r")
# source("rp-plot4d.r")
# rp.plot4d(cbind(Doy, DO), Station, location.plot = FALSE)
# rp.plot4d(cbind(Station, DO), Doy, location.plot = FALSE)
# 
# ind     <- Year >= 80 & Year <= 89 & !(Year == 85)
# year    <- Year[ind] + Doy[ind] / 365
# station <- Station[ind]
# doy     <- Doy[ind]
# do      <- DO[ind]
# group   <- factor(c("before 1985", "after 1985")[1 + 
#                 as.numeric(year < 85)])
# rp.plot4d(cbind(doy, do), station, group,
#      col.palette = c("red", "green"), location.plot = FALSE)
# 
# 
# #     Older material
# 
# source("/Volumes/adrian/research/madrid/sm.r")
# source("/Volumes/adrian/research/madrid/sm-pam-utilities.r")
# source("/Volumes/adrian/research/madrid/sm-fake-package.r")
# source("/Volumes/adrian/notes/computing/R/colour-key.r")
# 
# alpha <- c(1, 0.1, 1)
# plot(c(-1, 1), c(-1, 1), type = "n")
# points(-0.3,    0, col = hsv(rgb2hsv(1, 0, 0) * alpha), pch = 16, cex = 30)
# points(   0, -0.3, col = hsv(rgb2hsv(0, 1, 0) * alpha), pch = 16, cex = 30)
# points( 0.3,    0, col = hsv(rgb2hsv(0, 0, 1) * alpha), pch = 16, cex = 30)
# 
# plot(c(-1, 1), c(-1, 1), type = "n")
# points(-0.3,    0, col = "red", pch = 16, cex = 30)
# plot(c(-1, 1), c(-1, 1), type = "n")
# points(-0.3,    0, col = hsv(0,   1, 0.003921569), pch = 16, cex = 30)
# points(-0.3,    0, col = hsv(0, 0.1, 0.003921569), pch = 16, cex = 30)
# points(-0.3,    0, col = hsv(rgb2hsv(1, 0, 0) * alpha), pch = 16, cex = 30)
# 
# 
# clr <- c(rgb2hsv(col2rgb("green")))
# clr <- rbind(rep(clr[1], 10), (10:1) / 10, rep(1, 10))
# plot(1:10, pch = 16, col = hsv(clr[1,], clr[2,], clr[3,]), cex = 10)
# 
# plot(1:10)
# Rprof()
# n <- 1000
# z <- 1:n
# x <- cbind(2 * z /n + rnorm(n), 2 * z /n + rnorm(n))
# y <- 4 * z / n + rnorm(n)
# z0    <- 100
# zsd   <- 10
# n     <- length(y)
# ind   <- cut(y, 20, labels = FALSE)
# clr   <- col2rgb(topo.colors(20)[ind])
# alpha <- exp(-0.5 * (z - z0)^2 / zsd^2)
# clr   <- rgb2hsv(clr)
# clr1  <- rep(clr[1, ], n)
# clr2  <- clr[2, ] * alpha
# clr3  <- rep(clr[3, ], n)
# clr   <- hsv(clr1, clr2, clr3)
# plot(x, type = "n", pty = "s")
# for (i in order(alpha)) points(x[i,1], x[i,2], pch=16, col = clr[i])
# Rprof("")
# summaryRprof()
# 
# # See also the material in test-rp-plot4d-clyde.r
