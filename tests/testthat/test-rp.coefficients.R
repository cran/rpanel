#     Tests for the rp.coefficients function

# setwd('rpanel')
# library(devtools)
# library(testthat)
# load_all()

# rp.datalink("~/iCloud/teaching/book/data", "set local directory")

cofeplus <- CofE
cofeplus$group <- factor(sample(rep(1:3, each = 14)))
cofeplus$x1 <- rnorm(42)
cofeplus$x2 <- rnorm(42)
cofeplus$x3 <- rnorm(42)
model <- lm(Giving ~ Employ + Elect + Attend + group + x1 + x2 +x3, data = cofeplus)

test_that('Standard calls', {
  expect_no_error(rp.coefficients(model))
  expect_no_error(rp.coefficients(model, ci = FALSE))
  expect_no_error(rp.coefficients(model, se.scale = TRUE))
  expect_no_error(rp.coefficients(model, style = 'shading', se.scale = TRUE))
  expect_no_error(suppressMessages(rp.coefficients(model) + ggplot2::ylim(-100, 50)))
  expect_no_error(suppressMessages(rp.coefficients(model) + ggplot2::ylim(-20, 10)))
  expect_no_error(rp.coefficients(model) + ggplot2::coord_flip())
  expect_no_error(rp.coefficients(model, se.scale = TRUE) + ggplot2::coord_flip())
  expect_no_error(rp.coefficients(model) + ggplot2::xlab("Covariates"))
})

test_that('Interactions:', {
   gw <- dplyr::mutate(gullweight, x = rnorm(nrow(gullweight)))
   model <- lm(weight ~ hab * month, data = gullweight)
   expect_no_error(rp.coefficients(model))
   model <- lm(weight ~ hab * month * x, data = gw)
   expect_error(rp.coefficients(model))
})

test_that('marks:', {
   expect_no_error(rp.coefficients(model))
   expect_no_error(rp.coefficients(model, marks = NULL))
   expect_no_error(rp.coefficients(model, marks = 1))
})

# subset and labels currently disabled
# rp.coefficients(model, subset = 2:3)
# model0 <- lm(Giving ~ Employ + Attend, data = CofE)
# rp.coefficients(model0, subset = 2:3, labels = names(coefficients(model))[-1])

test_that('Options to deal with overlapping labels:', {
   expect_no_error(rp.coefficients(model) +
                      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90)))
   expect_no_error(suppressMessages(rp.coefficients(model) +
                                       ggplot2::scale_x_discrete(labels = scales::label_wrap(5))))
   expect_no_error(rp.coefficients(model) + ggplot2::coord_flip())
})

test_that('Errors:', {
  model <- lm(Giving ~ 1, data = CofE)
  expect_error(rp.coefficients(model))
})

test_that('Transformation:', {
   cofe_2019 <- suppressMessages(rp.wrangle('cofe_2019'))
   model <- lm(log(Giving_per_member) ~ Attachment + IMD, data = cofe_2019)
   expect_no_error(rp.coefficients(model, ci = FALSE) + ggplot2::coord_flip())
})

test_that('Standard calls: anova', {
   expect_no_error(rp.coefficients(lm(stime ~ poison + treatment, data = poisons)))
   expect_no_error(rp.coefficients(lm(stime ~ poison + treatment, data = poisons), ci = FALSE))
   expect_no_error(rp.coefficients(lm(stime ~ poison * treatment, data = poisons)))
   expect_no_error(rp.coefficients(lm(stime ~ poison * treatment, data = poisons), ci = FALSE))
   expect_no_error(rp.coefficients(lm(1/stime ~ poison * treatment, data = poisons), ci = FALSE))
})

test_that('Anova with cells with no data', {
   ind      <- which((poisons$poison ==  '1') & (poisons$treatment == '3'))
   poisons1 <- poisons[-ind, ]
   expect_no_error(rp.coefficients(lm(stime ~ poison * treatment, data = poisons1)))
})

test_that('Different contrasts', {
   model <- lm(stime ~ poison * treatment,
               contrasts = list(poison = 'contr.poly', treatment = 'contr.poly'), data = poisons)
   expect_no_error(rp.coefficients(model))
   model <- lm(stime ~ poison,
               contrasts = list(poison = 'contr.poly'), data = poisons)
   expect_no_error(rp.coefficients(model))
   model <- lm(weight ~ hab + month, data = gullweight, contrasts = list(month = 'contr.poly'))
   expect_no_error(rp.coefficients(model))
})

test_that('Colours', {
   model <- lm(log(Speed) ~ log(Mass), data = rodent, x = TRUE)
   expect_no_error(rp.coefficients(model, cols = c('estimate' = 'darkgreen')))
   expect_warning(rp.coefficients(model, cols = c('estmate' = 'darkgreen')))
})

test_that('Single term', {
   model <- lm(log(Speed) ~ log(Mass), data = rodent, x = TRUE)
   summary(model)$coefficients
   expect_no_error(rp.coefficients(model))
})

test_that('glms', {
   ethylene <- dplyr::filter(flour_beetles, Toxin == 'Ethylene_dichloride')
   model <- glm(cbind(Dead, Living) ~ Concentration, family = 'binomial', data = ethylene, x = TRUE)
   summary(model)$coefficients
   expect_no_error(rp.coefficients(model))
})
