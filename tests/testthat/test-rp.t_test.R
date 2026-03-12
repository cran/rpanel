#     Tests for the rp.t_test function

# setwd('rpanel')
# library(devtools)
# library(testthat)
# library(tidyverse)
# load_all()

test_that('Standard calls', {
   path  <- 'https://www.maths.gla.ac.uk/~adrian/data/trout.txt'
   trout <- read.table(path, header = TRUE, stringsAsFactors = TRUE)
   expect_no_error(pnl <- with(trout, rp.t_test(ration ~ group)))
   rp.control.dispose(pnl)
   expect_no_error(pnl <- with(trout, rp.t_test(ration ~ group, mu = 0)))
   rp.control.dispose(pnl)
   expect_no_error(with(trout, rp.t_test(ration ~ group, mu = 0,
                                         display = c(distribution = TRUE, detail = TRUE),
                                         ruler.position = 'reference', panel = FALSE)))
   expect_no_error(with(trout, rp.t_test(ration ~ group, mu = 0,
                                         display = c(distribution = TRUE, detail = TRUE),
                                         ruler.position = 'reference', panel = FALSE) +
                               ggplot2::coord_flip()))
})

sleep_wide <- tidyr::pivot_wider(sleep, values_from = extra, names_from = group,
                                 names_prefix = 'drug_')
sleep_diff <- with(sleep_wide, drug_2 - drug_1)

test_that('Single sample: interactive use', {
   expect_no_error(pnl <- rp.t_test(sleep_diff))
   rp.control.dispose(pnl)
})

test_that('Single sample: standard calls', {
   expect_no_error(rp.t_test(sleep_diff, panel = FALSE, ruler.position = 'none'))
   expect_no_error(rp.t_test(sleep_diff, panel = FALSE, ruler.position = 'candidate'))
   expect_no_error(rp.t_test(sleep_diff ~ 1, panel = FALSE))
   expect_no_error(rp.t_test(sleep_diff, panel = FALSE, mu = 0, ruler.position = 'reference',
                             display = c(distribution = TRUE)))
   expect_no_error(rp.t_test(sleep_diff, panel = FALSE, mu = 0, ruler.position = 'reference',
                             display = c(distribution = TRUE)))
   expect_no_error(rp.t_test(sleep_diff, panel = FALSE, mu = 0, ruler.position = 'reference',
                             display = c(detail = TRUE)))
})

test_that('Single sample: systematic calls with arguments', {
   for (data.display in c('histogram', 'density')) {
      for (dist.val in c(TRUE, FALSE)) {
         for (detail.val in c(TRUE, FALSE)) {
            for (ruler.position in c('none', 'candidate', 'sample mean', 'reference')) {
               expect_no_error(rp.t_test(sleep_diff, panel = FALSE,
                        data.display = data.display, mu = 0, 
                        display = c(distribution = dist.val, detail = detail.val),
                        ruler.position = ruler.position))
            }
         }
      }
   }
})

test_that('Paired data', {
   s1 <- sleep$extra[sleep$group == 1]
   s2 <- sleep$extra[sleep$group == 2]
   expect_no_error(rp.t_test(s2, s1, panel = FALSE, paired = TRUE))
   expect_no_error(rp.t_test(s2, s1, panel = FALSE, paired = TRUE, vlab = 'Something'))
   expect_no_error(rp.t_test(s2, s1, panel = FALSE, paired = TRUE, mu = 0))
   expect_no_error(rp.t_test(s2, s1, panel = FALSE, paired = TRUE,
                             ruler.position = 'reference', mu = 0))
   sleep2 <- reshape(sleep, direction = "wide", idvar = "ID", timevar = "group")
   expect_no_error(rp.t_test(Pair(sleep2$extra.1, sleep2$extra.2) ~ 1, panel = FALSE))
})

x  <- rnorm(25) + 1

test_that('Single sample: simulated data', {
   expect_no_error(rp.t_test(x, panel = FALSE))
   expect_no_error(rp.t_test(x + 10, panel = FALSE))
   expect_no_error(rp.t_test(x + 10, mu = 0, panel = FALSE))
   expect_no_error(rp.t_test(x, panel = FALSE, data.display  = 'none'))
   expect_no_error(rp.t_test(x + 10, panel = FALSE, data.display  = 'none'))
   expect_no_error(rp.t_test(x + 10, panel = FALSE, mu = 0, data.display  = 'none'))
   expect_no_error(rp.t_test(x, panel = FALSE, mu = 0, ruler.position = 'reference'))
   expect_no_error(rp.t_test(x, panel = FALSE, ruler.position = 'none', mu = 0))
   expect_no_error(rp.t_test(x, panel = FALSE, mu = 0))
   expect_no_error(rp.t_test(x, panel = FALSE, mu = 0, uncertainty = 'reference'))
   expect_no_error(rp.t_test(x, panel = FALSE, mu = 0))
   expect_no_error(rp.t_test(x, panel = FALSE, mu = 1))
   expect_no_error(rp.t_test(x, panel = FALSE, mu = 1, ruler.position = 'sample mean'))
})

# Put in tests for invalid options

x  <- rnorm(12) + 1
y  <- rnorm(250) + 2
xy <- c(x, y)
g  <- paste('group', as.character(rep(1:2, c(length(x), length(y)))))
gf <- factor(g)

test_that('Two-sample data', {
   expect_no_error(rp.t_test(xy, g, mu = -1, panel = FALSE))
   expect_no_error(rp.t_test(xy ~ gf, panel = FALSE))
})

test_that('Two-sample data: errors', {
   expect_error(rp.t_test(rnorm(1), rnorm(10), panel = FALSE))
})

test_that('Two-sample data', {
   expect_error(rp.t_test(x, y, paired = TRUE, panel = FALSE))
   expect_no_error(rp.t_test(xy, g, panel = FALSE))
   expect_no_error(rp.t_test(x, y, ruler.position = 'none', panel = FALSE))
   expect_no_error(rp.t_test(x, y, ruler.position = 'candidate', panel = FALSE))
   expect_no_error(rp.t_test(x, y, ruler.position = 'none', se.scale = TRUE,
                             mu = 0, panel = FALSE))
   expect_no_error(rp.t_test(x, y, panel = FALSE))
   expect_no_error(rp.t_test(x, y, xlab = 'xlabel', ylab = 'ylabel', vlab = 'Something',
                             , panel = FALSE))
   expect_no_error(rp.t_test(x, y, mu = 0, panel = FALSE))
   expect_no_error(suppressMessages(rp.t_test(x, y, ruler.position = 'reference',
                                                 panel = FALSE)))
   expect_no_error(rp.t_test(x, y, data.display = 'density', panel = FALSE))
   expect_no_error(rp.t_test(x, y, data.display = 'histogram', panel = FALSE))
   expect_no_error(suppressMessages(rp.t_test(x, y, ruler.position = 'reference',
                                                  panel = FALSE)))
   for (data.display in c('histogram', 'density')) {
      expect_no_error(rp.t_test(x, y, panel = FALSE))
      expect_no_error(rp.t_test(x, y, data.display = data.display,
                                mu = 0, ruler.position = 'none', panel = FALSE))
      expect_no_error(rp.t_test(x, y, data.display = data.display, zoom = TRUE,
                                    panel = FALSE))
      expect_no_error(rp.t_test(x, y, mu = 0, data.display = data.display, panel = FALSE))
      expect_no_error(rp.t_test(x, y, mu = 0, data.display = data.display, zoom = TRUE,
                                panel = FALSE))
      expect_no_error(rp.t_test(x, y, data.display = data.display, seed = 6245,
                                    panel = FALSE))
      expect_no_error(rp.t_test(x, y, mu = 0, ruler.position = 'reference',
                                data.display = data.display, seed = 6245, panel = FALSE))
      expect_no_error(rp.t_test(x, y, ruler.position = 'reference', mu = 1,
                                data.display = data.display, seed = 6245, panel = FALSE))
      expect_no_error(rp.t_test(x, y, mu = 1, data.display = data.display, seed = 6245,
                                panel = FALSE))
      expect_no_error(rp.t_test(x, y, var.equal = TRUE, data.display = data.display,
                                panel = FALSE, seed = 6245))
      expect_no_error(rp.t_test(y, x, zoom = TRUE, panel = FALSE))
      expect_warning(rp.t_test(y, x, data.display = 'something else', panel = FALSE))
   }
})

test_that('Two-sample data: formula input', {
   gf  <- factor(g)
   expect_no_error(rp.t_test(xy ~ gf, panel = FALSE))
   expect_no_error(rp.t_test(xy ~ gf, panel = FALSE, ruler.position = 'none'))
   expect_no_error(rp.t_test(xy ~ gf, panel = FALSE, ruler.position = 'sample mean'))
   expect_no_error(rp.t_test(xy ~ gf, panel = FALSE, mu = 0, ruler.position = 'reference'))
   gf3 <- factor(paste('group', as.character(c(rep(1:2, each = 20), rep(3, 10)))))
   expect_error(rp.t_test(xy ~ gf3, panel = FALSE))
})
