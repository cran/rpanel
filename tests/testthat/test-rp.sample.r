#     Tests for the rp.sample function

# library(devtools)
# library(testthat)
# load_all()

# rp.datalink("~/iCloud/teaching/book/data", "set local directory")

test_that('Standard calls', {
   expect_no_error(pnl <- rp.sample())
   rp.control.dispose(pnl)
   expect_no_error(pnl <- rp.sample(5, 0.4, 25))
   rp.control.dispose(pnl)
   expect_no_error(pnl <- rp.sample(display = 'violin'))
   rp.control.dispose(pnl)
   expect_no_error(pnl <- rp.sample(display = 'density'))
   rp.control.dispose(pnl)
   expect_no_error(pnl <- rp.sample(hscale = 1.5))
   rp.control.dispose(pnl)
   expect_no_error(pnl <- rp.sample(distribution = 'binomial'))
   rp.control.dispose(pnl)
   expect_no_error(rp.sample(distribution = 'binomial', panel = FALSE,
                             display.sample = c('st.dev. scale' = TRUE))$sample)
})

# Repeated simulations to test different data configurations
for (i in 1:100) {
test_that('Static mode', {
   expect_no_error(rp.sample(n = 25, mu = 5, sigma = 0.4, panel = FALSE, nbins = 10, nsim = 5000,
                             display.sample = c(mean = TRUE), show.out.of.range = FALSE,
                             display.mean = c('sample mean' = TRUE, 'accumulate' = TRUE,
                                              'se scale' = TRUE, 't-statistic' = TRUE)))
   expect_no_error(rp.sample(panel = FALSE))
   expect_no_error(rp.sample(panel = FALSE, display.sample = c('st.dev. scale' = TRUE)))
   expect_no_error(rp.sample(panel = FALSE,
                             display.sample = c('mean' = TRUE, 'st.dev. scale' = TRUE)))
   expect_no_error(result <- rp.sample(panel = FALSE, display = 'density',
                             display.sample = c('mean' = TRUE, 'st.dev. scale' = TRUE,
                                                'population' = TRUE),
                             display.mean = c('sample mean' = TRUE)))
   expect_no_error(print(result$sample))
   expect_no_error(print(result$mean))
   expect_no_error(rp.sample(n = 25, nbins = 10, display.sample = c(mean = TRUE), 
                             display.mean = c('sample mean' = TRUE), panel = FALSE))
   expect_no_error(rp.sample(n = 25, nbins = 10, nsim = 8, display.sample = c(mean = TRUE), 
                             display.mean = c('sample mean' = TRUE, accumulate = TRUE),
                             panel = FALSE))
   expect_no_error(rp.sample(n = 25, nbins = 10, nsim = 25, display.sample = c(mean = TRUE), 
                             display.mean = c('sample mean' = TRUE, 'accumulate' = TRUE),
                             panel = FALSE))
   expect_no_error(result <- rp.sample(n = 25, nbins = 10, display.sample = c(mean = TRUE), 
                             display.mean = c('sample mean' = TRUE), panel = FALSE))
   thm <- ggplot2::theme(axis.text  = ggplot2::element_text(size = 20),
                         axis.title = ggplot2::element_text(size = 20),
                         plot.title = ggplot2::element_text(size = 22))
   print(result$sample + thm + ggplot2::ggtitle('Sample size: 25'))
})
}

test_that('Standard graphics', {
   expect_no_error(pnl <- rp.sample(ggplot = FALSE))
   rp.control.dispose(pnl)
})

test_that('Data which previously caused an out-of-range error', {
   set.seed(64318)
   expect_no_error({
      for (i in 1:2)
         rp.sample(n = 25, mu = 5, sigma = 0.4, panel = FALSE, nbins = 10, nsim = 50,
                   display.sample = c(mean = TRUE),
                   display.mean = c('sample mean' = TRUE, 'accumulate' = FALSE))
   })
})
