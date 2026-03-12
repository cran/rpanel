#     Tests for the rp.geosim function

# setwd('rpanel')
# library(devtools)
# library(testthat)
# load_all()

test_that('Interactive use', {
   expect_no_error(pnl <- rp.geosim())
   rp.control.dispose(pnl)
})

test_that('Setting parameters', {
   expect_no_error(rp.geosim(smgrid = 20, panel = FALSE))
   expect_no_error(rp.geosim(ngrid = 20, panel = FALSE))
   expect_no_error(rp.geosim(Range = 0.2, panel = FALSE))
   expect_no_error(rp.geosim(pSill = 0.2, panel = FALSE))
   expect_no_error(rp.geosim(Nugget = 0.1, panel = FALSE))
   expect_no_error(rp.geosim(kappa = 2, panel = FALSE))
   expect_no_error(rp.geosim(aniso.angle = -pi/2, aniso.ratio = 2, panel = FALSE))
})
