#########################################################
## Martin Durocher <mduroche@uwaterloo.ca>
#########################################################

context("Testing predict.amax function")

test_that("Verifying predict.amax", {
  
pdf(file = NULL)

x <- ExtractAmax(flow~date,flowStJohn, tol = 355)

fit <- FitAmax(x$flow,'gev', method = 'mle')

plot(fit, ci = TRUE)

rp <- 1-1/c(10,100)
rlev <- predict(fit, p = rp)
expect_equal(signif(rlev), c(3356.64, 4250.40))
expect_is(rlev, 'numeric')

out <- predict(fit, p =c(.9,.99), se = TRUE, ci = 'delta')
expect_true(all(colnames(out) == c('pred','se','lower','upper')))
expect_is(out, 'data.frame')
expect_equal(rownames(out), c('0.90','0.99'))

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

})
