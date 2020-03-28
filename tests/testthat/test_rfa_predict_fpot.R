#########################################################
## Martin Durocher <mduroche@uwaterloo.ca>
#########################################################

context("Testing predict.fpot function")

test_that("Verifying predict.fpot", {

uu <- (1:10000)/10001
xd <- qgpa(uu, 1, -.1)

## verify the predict output
fit <- FitPot(xd, u = 5, varcov = F, method = 'lmome',unit = 100)
expect_error(pp <- predict(fit, se = TRUE))

## verif the output when se is returned
fit <- FitPot(xd, u = 5)
pp <- predict(fit, rt = c(2,20,100), se = TRUE)
expect_equal(dim(pp), c(3,2))
expect_equal(rownames(pp), as.character(c(2,20,100)))

## verify delta confidence interval
pp <- predict(fit, se = TRUE, ci = 'delta', alpha = .1)
expect_equal(pp$pred + qnorm(.95) * pp$se, pp$upper)
expect_equal(pp$pred - qnorm(.95) * pp$se, pp$lower)
expect_equal(colnames(pp), c('pred','se','lower','upper'))

se.ref <- c(0.836256, 1.371230, 1.878190, 2.480310, 3.434020, 4.284160)
expect_equal(signif(pp$se), se.ref)

## verify profile confidence interval
pp <- predict(fit, rt = c(10,100), ci = 'profile')
pp.ref <- matrix(nrow = 2, ncol = 3, byrow = TRUE, 
                 data = c(12.1644, 10.8399, 15.0029,
                          16.9163, 13.5834, 26.7115))
  
expect_true(all(signif(pp)==pp.ref))

## verify profile confidence interval
pp <- predict(fit, ci = 'boot', nsim = 10)
expect_equal(dim(pp), c(6,3))


## verify profile confidence interval
pp <- predict(fit, ci = 'boot', nsim = 10, out.matrix = TRUE)
expect_equal(names(pp), c('pred','para','qua'))

## verify that bootstrap work with only 
pp <- predict(fit, rt = c(10,100), ci = 'boot', nsim = 10)
pp <- predict(fit, rt = 10, ci = 'boot', nsim = 10)

})