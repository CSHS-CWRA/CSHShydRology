##############################################################
## Test the low level function to fit Generalized Pareto
## Martin Durocher <mduroche@uwaterloo.ca>
###############################################################

rm(list = ls())

x <- qgpa(seq(0.01,.99, l = 1001))

## Verification to make sure that nothing as change
expect_equivalent(signif(fgpaMom(x)), c(0.9871750, 0.0234965))
expect_equivalent(signif(fgpaLmom(x)), c(1.0374800, 0.0756554))
expect_equivalent(signif(fgpa1d(x)), c(1.067500, 0.105857))
expect_equivalent(signif(fgpa2d(x)), c(1.067550, 0.105841))

## verify that take initial value
expect_length(fgpa2d(x, par0 = c(1.07,.106)),2)

expect_true(all(names(f1 <- fgpa1d(x, sol = TRUE)) == c('par','varcov')))
expect_true(all(names(f2 <- fgpa2d(x, sol = TRUE)) == c('par','varcov')))

expect_equivalent(as.numeric(f1$varcov),
                     c(0.002035810, 0.000953542, 0.000953542, 0.000798693))

##
x <- qgpa(seq(0.01,.99, l= 1001), 1, -1)
expect_true(fgpa1d(x)[2] < -.5)
expect_true(fgpa2d(x)[2] > -.5)


## verify at a different scale

x <- qgpa(seq(0.01,.99, l = 1001), 10, -.1)

## Verification to make sure that nothing as change
expect_equivalent(signif(fgpaMom(x)), c(10.6684,  0.00838934))
expect_equivalent(signif(fgpaLmom(x)), c(10.4274, -0.014396))
expect_equivalent(signif(fgpa1d(x)), c(10.6948,  0.0108715))
expect_equivalent(signif(fgpa2d(x)), c(10.6923,  0.0108057))
