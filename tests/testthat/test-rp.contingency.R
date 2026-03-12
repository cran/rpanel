#     Tests for the rp.t_test function

# setwd('rpanel')
# library(devtools)
# library(testthat)
# load_all()

x <- matrix(c(19, 41, 32, 28), ncol = 2,
            dimnames = list(c("non-smoker", "smoker"),
                            c("cases", "controls")))

x2 <- t(matrix(c(87, 73, 64, 15, 25, 16, 59, 84, 92), ncol = 3,
               dimnames = list(c("red", "yellow", "blue"),
                               c("round", "square", "oblong"))))

x3 <- t(matrix(c(887, 73, 64, 815, 25, 16, 859, 84, 92), ncol = 3,
               dimnames = list(c("red", "yellow", "blue"),
                               c("round", "square", "oblong"))))

test_that('Standard calls', {
   for (i in 1:3) {
      xx <- switch(i, x, x2, x3)
      for (uncertainty in c('none', 'shading', 'violin')) {
         for (proportion.scale in c('fixed', 'free')) {
            expect_no_error(rp.contingency(xx, uncertainty, proportion.scale))
         }
      }
   }
})

test_that('Errors in input', {
   expect_error(rp.contingency(matrix(c(19.1, 41, 32, 28), ncol = 2)))
   expect_error(rp.contingency(matrix(c(-19, 41, 32, 28), ncol = 2)))
   expect_error(rp.contingency(array(1:8, dim = rep(2, 3))))
   expect_error(rp.contingency( matrix(1:3)))
})

test_that('Add to the ggplot object', {
   p <- rp.contingency(x)
   expect_no_error(p + ggplot2::ggtitle("A contingency table"))
   expect_no_error(rp.contingency(x) + ggplot2::ggtitle("A contingency table"))
})
