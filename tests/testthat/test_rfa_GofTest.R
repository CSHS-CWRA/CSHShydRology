#################################################
## Test function GofTest
## Martin Durocher <mduroche@uwaterloo.ca>
##################################################

set.seed(1)

u <- seq(1000)/1001
x <- qAmax(u, c(100,30,-.1), 'gev')

fit <- FitAmax(x, 'gev', varcov = FALSE)
out <- GofTest(fit, nsim = 5)
expect_true(out$pvalue > .9)
expect_equal(signif(out$stat), 0.00575069)

fit <- FitAmax(u, 'gev', varcov = FALSE)
out <- GofTest(fit, nsim = 5)
expect_true(out$pvalue < .1)

## Just make sure that they don't create error
fit <- FitAmax(x, 'glo', method = 'mle', varcov = FALSE)
GofTest(fit, method = 'ad', nsim = 5)

## verify nsim = 0
out <- GofTest(f, method = 'ad', nsim = 0)
expect_true(is.na(out$stat))

fit <- FitAmax(x, 'pe3', method = 'lmom', varcov = FALSE)
GofTest(fit, method = 'shapiro', nsim = 5)

## For POT
x <- qgpa(u, 1.1, .1)
f <- FitPot(x , u = 0)
out <- GofTest(f)
expect_equal(signif(out$stat), 0.0116331)
GofTest(f, method = 'ad', nsim = 5)


## Verify the nsim = 10
out <- GofTest(f, method = 'ad', nsim = 0)
expect_true(is.na(out$stat))
