#     Tests for the rp.datalink

# setwd('rpanel')
# library(devtools)
# library(testthat)
# load_all()

test_that('Standard calls', {
   expect_no_error(rp.colours())
   expect_no_error(rp.colors())
   expect_no_error(rp.colours(c('estimate' = 'darkgreen')))
   expect_warning(rp.colours(c('estmate' = 'darkgreen')))
   expect_warning(rp.colours(c('darkgreen')))
   expect_warning(rp.colours(c(2)))
})
