######################################################
## testing predict.amax function
#######################################################
rm(list = ls())

x <- ExtractAmax(flow~date,flowStJohn, tol = 355)

fit <- FitAmax(x$flow,'gev', method = 'mle')

rp <- 1-1/c(10,100)
rlev <- predict(fit, q = rp)
expect_equal(signif(rlev), c(3356.64, 4250.40))
expect_is(rlev, 'numeric')

out <- predict(fit, se = TRUE, ci = 'delta')
expect_true(all(colnames(out) == c('pred','se','lower','upper')))
expect_is(out, 'data.frame')

out <- predict(fit, se = FALSE, ci = 'delta')
expect_true(all(colnames(out)== c('pred','lower','upper')))

out <- predict(fit, se = TRUE)
expect_true(all(colnames(out) == c('pred','se')))


## The bootstrap sample used for CI are returned
fit <- FitAmax(x$flow, distr = 'gev', varcov = FALSE)
boot <- predict(fit, rp, se = FALSE, ci = 'boot',
                 nsim = 5, out.matrix = TRUE)

expect_is(boot, 'list')
expect_true(all(names(boot) == c('pred','para','qua')))

