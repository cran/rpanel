rp.cartoons <- function() {

panel.launch <- function(menu.panel) {
   if (menu.panel$demo == "q-q plot") {
      bc.fn <- function(y, lambda) {
         if (abs(lambda) < 0.001) z <- log(y)
         else z <- (y^lambda - 1)/ lambda
         }
      qq.draw <- function(panel) {
         z <- bc.fn(panel$y, panel$lambda)
         if (length(dev.list()) == 0) x11()
         qqnorm(z, main = paste("lambda =", round(panel$lambda, 2)))
         panel
         }
      x11()
      panel <- rp.control(y = exp(rnorm(50)), lambda = 1)
      rp.slider(panel, lambda, -2, 2, qq.draw)
      }
   else if (menu.panel$demo == "Binomial distribution") {
      plot.binomial <- function(panel) {
         with(panel, {
            n <- as.numeric(n)
            probs <- dbinom(0:n, n, prob)
            if (length(dev.list()) == 0) x11()
            plot(c(0,n), c(0,1), type = "n", xlab = "x", ylab = "Probability")
            segments(0:n, rep(0, n+1), 0:n, probs)
            title(paste("Binomial:  n =", n, "  p =", round(prob, 3)))
            })
         panel
         }
      rp.binomial <- function() {
         x11()
         pname <- rp.control("Binomial probabilities", n = 20, prob = 0.5)
         rp.slider(pname, prob, 0, 1, initval = 0.5, title = "Binomial proby, p:", 
                   action = plot.binomial)
         rp.textentry(pname, n, plot.binomial, "Sample size, n:")
         }
      rp.binomial()
      }
   else if (menu.panel$demo == "Tables") {
      rp.tables()
      }
   else if (menu.panel$demo == "Normal fitting") {
      y <- rnorm(50, mean = 10, sd = 0.5)
      rp.normal(y)
      }
   else if (menu.panel$demo == "Confidence intervals") {
      rp.ci()
      }
   else if (menu.panel$demo == "Regression - CofE (Attend)") {
      data(CofE)
      attach(CofE)
      rp.regression(Attend, Giving)
      }
   else if (menu.panel$demo == "Regression - rodent") {
      example(rodent, ask = FALSE)
      }
   else if (menu.panel$demo == "Regression - CofE (Attend & Employ)") {
      data(CofE)
      attach(CofE)
      rp.regression(cbind(Employ, Attend), Giving)
      }
   else if (menu.panel$demo == "Ancova") {
      example(rp.ancova, ask = FALSE)
      }
   else if (menu.panel$demo == "Logistic regression") {
      example(rp.logistic, ask = FALSE)
      }
   else if (menu.panel$demo == "Repeated measurements") {
      example(rp.rmplot, ask = FALSE)
      }
   else if (menu.panel$demo == "Likelihood - exponential") {
      data(aircond)
      rp.likelihood("sum(log(dexp(data, theta)))", aircond, 0.005, 0.03)
      }
   else if (menu.panel$demo == "Likelihood - gamma") {
      data(aircond)
      rp.likelihood("dgamma(data, theta[1], theta[2])", aircond, c(0.3, 0.005), c(3, 0.06))
      }
   else if (menu.panel$demo == "Power") {
      rp.power()
      }
   else if (menu.panel$demo == "Density estimation (1d)") {
      provide.data(tephra, options = list(describe = FALSE))
      sm.density(Al2O3, panel = TRUE)
      }
   else if (menu.panel$demo == "Density estimation (2d)") {
      provide.data(airpc, options = list(describe = FALSE))
      y <- cbind(Comp.1, Comp.2)[Period==3,]
      sm.density(y, panel = TRUE)
      }
   else if (menu.panel$demo == "Nonparametric regression (1d)") {
      provide.data(trawl, options = list(describe = FALSE))
      sm.regression(Longitude, Score1, panel = TRUE)
      }
   else if (menu.panel$demo == "Gulls") {
      rp.gulls()
      }
   else if (menu.panel$demo == "Spatial simulation") {
      rp.geosim()
      }
   else if (menu.panel$demo == "Mururoa") {
      rp.mururoa()
      }
   else if (menu.panel$demo == "Firth") {
      rp.firth()
      }
   else if (menu.panel$demo == "Firth") {
      rp.firth()
      }
   menu.panel
   }

menu.panel <- rp.control("Cartoons", homer = FALSE, number.list = list(),
      ss = list(), trans = list(), theta = list())
menu.list  <-  list(list("Introductory",
                         "q-q plot",
                         "Binomial distribution",
                         "Tables",
                         "Normal fitting",
                         "Confidence intervals"
                         ),
                    list("Regression",
                         "Regression - CofE (Attend)",
                         "Regression - rodent",
                         "Regression - CofE (Attend & Employ)",
                         "Ancova",
                         "Logistic regression"
                         ),
                    list("Advanced",
                         "Repeated measurements",
                         "Likelihood - exponential",
                         "Likelihood - gamma",
                         "Power"
                          ),
                    list("Smoothing",
                         "Density estimation (1d)",
                         "Density estimation (2d)",
                         "Nonparametric regression (1d)"
                          ),
                    list("Applications",
                         "Gulls",
                         "Spatial simulation",
                         "Mururoa",
                         "Firth"
                          )
                    )
                    
if (!require(sm)) menu.list <- menu.list[-4]
rp.menu(menu.panel, demo, menu.list, action = panel.launch)
image.file <- file.path(system.file(package = "rpanel"), "images", "cartoons.gif")
rp.image(menu.panel, image.file)

invisible()
}

