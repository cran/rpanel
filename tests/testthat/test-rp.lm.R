#     Tests for the rp.lm function

# setwd('rpanel')
# library(devtools)
# library(testthat)
# load_all()

#----------------------------------------------------------------
#     Regression with one covariate
#----------------------------------------------------------------

test_that('Standard call', {
   expect_no_error(pnl <- rp.lm(Giving ~ Employ, data = CofE))
   rp.control.dispose(pnl)
})
test_that('Change axis labels', {
   expect_no_error(pnl <- rp.lm(Giving ~ Employ, data = CofE, xlab = 'x', ylab = 'y'))
   rp.control.dispose(pnl)
})
test_that('Model as input', {
   model <- lm(pnl <- Giving ~ Employ, data = CofE)
   expect_no_error(pnl <- rp.lm(model))
   rp.control.dispose(pnl)
})
test_that('Error if no covariate is specified', {
   expect_error(rp.lm(Giving ~ 1, data = CofE))
})
test_that('Rodent data: lm', {
   expect_no_error(pnl <- rp.lm(log(Speed) ~ log(Mass), data = rodent))
   rp.control.dispose(pnl)
})
test_that('Rodent data: regression', {
   expect_no_error(pnl <- with(rodent, rp.regression(log(Mass), log(Speed))))
   rp.control.dispose(pnl)
})

#----------------------------------------------------------------
# cat('\n** Regression with two covariates **\n')
#----------------------------------------------------------------

test_that('Standard call', {
   expect_no_error(pnl <- rp.lm(Giving ~ Employ + Attend, data = CofE))
   rp.control.dispose(pnl)
})
test_that('Change axis labels', {
   expect_no_error(pnl <- rp.lm(Giving ~ Employ + Attend, data = CofE,
                         xlab = 'x', ylab = 'y', zlab = 'z'))
   rp.control.dispose(pnl)
})
test_that('Interaction between two covariates', {
   expect_warning(pnl <- rp.lm(Giving ~ Employ * Attend, data = CofE))
   rp.control.dispose(pnl)
})
test_that('Static mode: change axis labels with a specified model', {
   expect_no_error(rp.lm(Giving ~ Employ + Attend, data = CofE,
                         xlab = 'x', ylab = 'y', zlab = 'z',
                         display.model = ~ Employ, panel = FALSE))
})
test_that('Static mode: residuals showing', {
   expect_no_error(rp.lm(Giving ~ Employ + Attend, data = CofE,
                         xlab = 'x', ylab = 'y', zlab = 'z',
                         residuals.showing = TRUE, panel = FALSE))
})
test_that('Static mode: select the model to be displayed', {
   expect_no_error(rp.lm(Giving ~ Employ + Attend, data = CofE,
                         display.model = ~ Employ, panel = FALSE))
})
test_that('Static mode: select the null model to be displayed', {
   expect_no_error(rp.lm(Giving ~ Employ + Attend, data = CofE,
                         display.model = ~ 1, residuals.showing = TRUE,
                         panel = FALSE))
})
test_that('Static mode: transformations with a data argument', {
   expect_no_error(rp.lm(log(Giving) ~ Elect + Attend, data = CofE,
                         panel = FALSE, residuals.showing = TRUE))
})
test_that('Old style', {
   expect_no_error(pnl <- rp.lm(Giving ~ Employ + Attend, data = CofE,
                                display.model = ~ 1, residuals.showing = TRUE,
                                style = 'old'))
   rp.control.dispose(pnl)
})
test_that('rp.regression', {
   expect_no_error(pnl <- with(CofE, rp.regression(cbind(Employ, Attend), Giving)))
   rp.control.dispose(pnl)
})

cofemore <- CofE
cofemore$Giving_per_member <- cofemore$Giving

test_that('Static mode: transformations without a data argument', {
   expect_no_error(rp.lm(log(Giving_per_member) ~ Attend + Employ, panel = FALSE,
                         residuals.showing = TRUE, data = cofemore))
   expect_no_error(rp.lm(Giving_per_member ~ log(Attend) + Employ, panel = FALSE,
                         residuals.showing = TRUE, data = cofemore))
})
test_that('Model nodes when the names are long:', {
   expect_no_error(rp.lm(log(Giving_per_member) ~ Attend + Employ, panel = FALSE,
                         data = cofemore, plot.nodes = TRUE))
})

# Remove rgl windows
rgl::close3d(rgl::rgl.dev.list())

#----------------------------------------------------------------
# cat('\n** One covariate and one factor **\n')
#----------------------------------------------------------------

test_that('Standard call', {
   expect_no_error(pnl <- rp.lm(weight ~ hab + month, data = gullweight))
   rp.control.dispose(pnl)
})

test_that('Default display', {
   # When panel is TRUE, the default display.model is NULL.
   # When panel is FALSE, the default display.model is the specified model.
   expect_no_error(rp.lm(weight ~ hab + month, data = gullweight, panel = FALSE))
   expect_no_error(rp.lm(weight ~ hab * month, data = gullweight, panel = FALSE))
})

test_that('Old style of display', {
   expect_no_error(pnl <- rp.lm(weight ~ hab + month, data = gullweight, style = 'old'))
   rp.control.dispose(pnl)
   # Check that the factor is correctly identified
   expect_no_error(pnl <- rp.lm(weight ~ month + hab, data = gullweight, style = 'old'))
   rp.control.dispose(pnl)
})

rds  <- read.table('https://www.maths.gla.ac.uk/~adrian/data/rds.txt', header = TRUE,
                   stringsAsFactors = TRUE)
test_that('rds data', {
   # Adjust linewidth', {
   expect_no_error(rp.lm(lrate ~ RDS * GA, data = rds, panel = FALSE, linewidth = 2))
   # Increase font size
   expect_no_error(rp.lm(lrate ~ RDS * GA, data = rds, panel = FALSE, plot = FALSE) +
                      ggplot2::theme(plot.title = ggplot2::element_text(size = 20)) + 
                      ggplot2::theme(axis.text  = ggplot2::element_text(size = 20)) +
                      ggplot2::theme(axis.title = ggplot2::element_text(size = 20)))
})
   
test_that('Error: character variable', {
   rds$RDS <- as.character(rds$RDS)
   expect_error(rp.lm(lrate ~ RDS * GA, data = rds, panel = FALSE))
   rds$RDS <- factor(rds$RDS)
})

#----------------------------------------------------------------
# cat('\n** One factor **\n')
#----------------------------------------------------------------

test_that('Standard call', {
   expect_no_error(pnl <- rp.lm(stime ~ poison, data = poisons))
   rp.control.dispose(pnl)
})
test_that('Static mode: standard call', {
   expect_no_error(rp.lm(stime ~ poison, data = poisons, panel = FALSE))
})
test_that('Static mode: specify display model', {
   expect_no_error(rp.lm(stime ~ poison, data = poisons, panel = FALSE,
                                display.model = ~ poison))
})
test_that('Static mode: specify display and comparison models', {
   expect_no_error(rp.lm(stime ~ poison, data = poisons, panel = FALSE,
                                display.model = ~ poison, comparison.model = ~ 1))
})
test_that('Static mode: shading display', {
   expect_no_error(rp.lm(stime ~ poison, data = poisons, panel = FALSE,
                         comparison.model = ~ 1, uncertainty.display = 'shading'))
})
test_that('Static mode: no display model', {
   expect_no_error(rp.lm(stime ~ poison, data = poisons, panel = FALSE,
                         display.model = NULL))
})
test_that('Static mode: valid display.model', {
   expect_no_error(rp.lm(stime ~ poison, data = poisons, panel = FALSE,
                         display.model = ~ 1))
})
test_that('Static mode: invalid display.model', {
   expect_error(rp.lm(stime ~ poison + treatment, data = poisons, panel = FALSE,
                      display.model = ~ something))
})
test_that('Static mode: missing data present', {
   poisons1 <- poisons
   poisons1[cbind(sample(1:nrow(poisons1), 8), sample(1:3, 8, replace = TRUE))] <- NA
   expect_no_error(rp.lm(stime ~ poison, data = poisons1, panel = FALSE))
   expect_no_error(rp.lm(stime ~ poison, data = poisons1, panel = FALSE,
                         comparison.model = ~ 1))
})
test_that('Static mode: some categories with no data', {
   poisons1 <- poisons
   ind      <- which((poisons1$poison ==  '1'))
   poisons1 <- poisons1[-ind, ]
   expect_no_error(rp.lm(stime ~ poison, data = poisons1, panel = FALSE,
                         comparison.model = ~ 1))
})
test_that('Static mode: some categories are all missing', {
   poisons1 <- poisons
   ind      <- which((poisons1$poison ==  '1'))
   poisons1$poison[ind] <- NA
   expect_no_error(rp.lm(stime ~ poison, data = poisons1, panel = FALSE,
                         comparison.model = ~ 1))
})

# Doughnut data from Snedecor & Cochran p.258
absorption <- c(64, 72, 68, 77, 56, 95, 78, 91, 97, 82, 85, 77,
                75, 93, 78, 71, 63, 76, 55, 66, 49, 64, 70, 68)
fat <- factor(rep(c('A', 'B', 'C', 'D'), each = 6))
test_that('Standard call:', {
   expect_no_error(rp.lm(absorption ~ fat, panel = FALSE, comparison.model = ~ 1))
})

test_that('Old version of rp.anova', {
   expect_no_error(pnl <- rp.anova(1/poisons$stime, poisons$treatment))
   rp.control.dispose(pnl)
})

#----------------------------------------------------------------
# cat('\n** Two factors **\n')
#----------------------------------------------------------------

test_that('Standard call', {
   expect_no_error(pnl <- rp.lm(stime ~ poison + treatment, data = poisons))
   rp.control.dispose(pnl)
})

test_that('Specify a comparison model', {
   expect_no_error(pnl <- rp.lm(stime ~ poison + treatment, data = poisons,
                                comparison.model = ~ poison))
   rp.control.dispose(pnl)
})
test_that('Shading display', {
   expect_no_error(pnl <- rp.lm(stime ~ poison + treatment, data = poisons,
                                uncertainty.display = 'shading'))
   rp.control.dispose(pnl)
})
test_that('Error: display and comparison models are not adjacent', {
   expect_error(rp.lm(stime ~ poison + treatment, data = poisons,
                      display.model = ~ poison * treatment,
                      comparison.model = ~ poison, panel = FALSE))
})
test_that('Static mode: standard call', {
   expect_no_error(rp.lm(stime ~ poison + treatment, data = poisons, panel = FALSE))
})
test_that('Static mode: transformation', {
   expect_no_error(rp.lm(1/stime ~ poison + treatment, data = poisons, panel = FALSE))
})
test_that('Static mode: shading display', {
   expect_no_error(rp.lm(stime ~ poison + treatment, data = poisons, panel = FALSE,
                         uncertainty.display = 'shading'))
})
test_that('Static mode: no display model', {
   expect_no_error(rp.lm(stime ~ poison + treatment, data = poisons, panel = FALSE,
                         display.model = NULL))
})
test_that('Static mode: valid display.model', {
   expect_no_error(rp.lm(stime ~ poison + treatment, data = poisons, panel = FALSE,
                         display.model = ~ poison * treatment))
})
test_that('Static mode: invalid display.model', {
   expect_error(rp.lm(stime ~ poison + treatment, data = poisons, panel = FALSE,
                      display.model = ~ something))
})
test_that('Static mode: valid comparison.model', {
   expect_no_error(rp.lm(stime ~ poison + treatment, data = poisons, panel = FALSE,
                         display.model = ~ poison * treatment,
                         comparison.model = ~ poison + treatment))
   expect_no_error(rp.lm(1/stime ~ poison + treatment, data = poisons, panel = FALSE,
                         display.model = ~ poison * treatment,
                         comparison.model = ~ poison + treatment))
})
test_that('Static mode: display.model and comparison.model are not adjacent', {
   expect_error(rp.lm(stime ~ poison + treatment, data = poisons, panel = FALSE,
                      display = ~ poison, comparison.model = ~ poison * treatment))
})

test_that('Static mode: missing data present', {
   poisons1 <- poisons
   poisons1[cbind(sample(1:nrow(poisons1), 8), sample(1:3, 8, replace = TRUE))] <- NA
   expect_no_error(rp.lm(stime ~ poison + treatment, data = poisons1, panel = FALSE))
   expect_no_error(rp.lm(stime ~ poison + treatment, data = poisons1, panel = FALSE,
                         comparison.model = ~ poison * treatment))
})
test_that('Static mode: some categories with no data', {
   ind      <- which((poisons$poison ==  '1') & (poisons$treatment == '3'))
   poisons1 <- poisons[-ind, ]
   expect_no_error(rp.lm(stime ~ poison * treatment, data = poisons1, panel = FALSE,
                         comparison.model = ~ poison * treatment))
})

test_that('Very different sample sizes in factor levels', {
   y <- c(rnorm(10), rnorm(1000), rnorm(10), rnorm(10))
   x <- factor(c(rep(1, 1010), rep(2, 20)))
   z <- factor(c(rep(1, 10), rep(2, 1000), rep(1, 10), rep(2, 10)))
   expect_no_error(rp.lm(y ~ x + z, panel = FALSE, display.model = ~ x * z, comparison.model = ~ x + z))
   expect_no_error(rp.lm(y ~ x + z, panel = FALSE, display.model = ~ x * z, comparison.model = ~ x + z,
         uncertainty.display = 'shading'))
   expect_no_error(rp.lm(y ~ x, comparison.model = ~1, panel = FALSE))
   expect_no_error(rp.lm(y ~ x, comparison.model = ~1, panel = FALSE, uncertainty.display = 'shading'))
})

test_that('One observation per cell, so no interaction can be fitted', {
   y <- rnorm(4)
   x <- factor(rep(1:2, 2))
   z <- factor(rep(1:2, each = 2))
   expect_no_error(rp.lm(y ~ x + z, panel = FALSE, display.model = ~ x + z, comparison.model = ~ x * z))
})

test_that('Old version of rp.anova', {
   expect_no_error(pnl <- rp.anova(1/poisons$stime, poisons$treatment, poisons$poison))
   rp.control.dispose(pnl)
})

#----------------------------------------------------------------
# Different contrasts
#----------------------------------------------------------------

test_that('Different contrasts', {
   model <- lm(stime ~ poison + treatment,
               contrasts = list(poison = 'contr.poly', treatment = 'contr.poly'), data = poisons)
   expect_no_error(rp.lm(model, data = poisons, panel = FALSE))
   model <- lm(stime ~ poison,
               contrasts = list(poison = 'contr.poly'), data = poisons)
   expect_no_error(rp.lm(model, data = poisons, panel = FALSE))
   model <- lm(weight ~ hab + month, data = gullweight, contrasts = list(month = 'contr.poly'))
   expect_no_error(rp.lm(model, data = gullweight, panel = FALSE))
})

#----------------------------------------------------------------
# Plot model nodes
#----------------------------------------------------------------

# load('https://www.maths.gla.ac.uk/~adrian/data/DO_Clyde.rda')
# clyde.sub  <- subset(clyde, Station == 4)
# 
# test_that('Static mode: plot nodes - one highlight', {
#    expect_no_error(rp.lm(DO ~ Temperature + Salinity, data = clyde.sub,
#                          panel = FALSE, plot.nodes = TRUE))
# })
# test_that('Static mode: plot nodes - comparison', {
#    expect_no_error(rp.lm(DO ~ Temperature + Salinity, data = clyde.sub,
#                          comparison.model = ~ Temperature,
#                          panel = FALSE, plot.nodes = TRUE))
# })
# test_that('Static mode: plot nodes - comparison', {
#    expect_no_error(rp.lm(stime ~ poison + treatment, data = poisons,
#                          comparison.model = ~ poison * treatment,
#                          panel = FALSE, plot.nodes = TRUE))
# })

test_that('Static mode: plot nodes - one highlight', {
   expect_no_error(rp.lm(stime ~ poison + treatment, data = poisons,
                         panel = FALSE, plot.nodes = TRUE))
})
test_that('Static mode: plot nodes - comparison', {
   expect_no_error(rp.lm(stime ~ poison + treatment, data = poisons,
                         comparison.model = ~ poison,
                         panel = FALSE, plot.nodes = TRUE))
})
test_that('Static mode: plot nodes - comparison', {
   expect_no_error(rp.lm(stime ~ poison + treatment, data = poisons,
                         comparison.model = ~ poison * treatment,
                         panel = FALSE, plot.nodes = TRUE))
})

#----------------------------------------------------------------
# cat('\n** Save and amend the ggplot object **\n')
#----------------------------------------------------------------

test_that('Static mode: plot if no assignment', {
   expect_no_error(rp.lm(stime ~ poison + treatment, data = poisons,
                                comparison.model = ~ poison * treatment,
                                panel = FALSE))
   # A break in the sequence of plots for ease of review
   plot(4)
})
test_that('Static mode: no plot if there is an assignment', {
   expect_no_error(plt <- rp.lm(stime ~ poison + treatment, data = poisons,
                                comparison.model = ~ poison * treatment,
                                panel = FALSE))
   plot(5)
   print(plt)
   print(plt + ggplot2::ggtitle("Something"))
})

# Remove rgl windows
rgl::close3d(rgl::rgl.dev.list())

