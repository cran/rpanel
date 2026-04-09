#     Tests for the rp.drop1 function

# setwd('rpanel')
# library(devtools)
# library(testthat)
# load_all()
# 
# rp.datalink("~/iCloud/teaching/book/data", "set local directory")

#----------------------------------------------------------------
# cat('\n** One covariate **\n')
#----------------------------------------------------------------

model <- lm(Giving ~ Employ, data = CofE)

test_that('Standard call', {
   expect_no_error(rp.drop1(model))
})

test_that('Change reference significance levels', {
   expect_no_error(rp.drop1(model, p.reference = c(0.05, 0.01, 0.001)))
})

test_that('Change colour', {
   expect_no_error(rp.drop1(model, cols = c('reference' = 'darkgreen')))
})

#----------------------------------------------------------------
#     Two factors
#----------------------------------------------------------------

test_that('Multiple degrees of freedom', {
   expect_no_error(rp.drop1(lm(stime ~ poison + treatment, data = poisons)))
})
