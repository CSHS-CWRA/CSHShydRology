#########################################################
## Martin Durocher <mduroche@uwaterloo.ca>
#########################################################

context("Testing predict.reglmom function")

test_that("Verifying predict.reglmom", {

## create testing coordinates, distances and samples.  
coord20 <- expand.grid(1:5,1:4)

h20 <- as.matrix(dist(coord20))
colnames(coord20) <- c('lon','lat')

uu <- sapply(seq(0,.5, l = 20), function(z) sample((1:50-z)/(50-z+1)))
sim20 <- apply(uu, 2, qgev, 100, 30, -.05)

## Fit the pooling groups
sim20 <- FindNearest(sim20, h20[2,], 15)
fit1 <- FitRegLmom(sim20)

q0 <- c(.7,.93)
q1 <- .99

## verify output format
out <- predict(fit1,q0, ci = TRUE, nsim = 5)
expect_equal(dim(out),c(2,4))
expect_equal(class(out),'data.frame')
expect_equal(colnames(out),c('pred','rmse','lower','upper'))
expect_equal(rownames(out), c('0.70','0.93'))

## Verify prediction does not change since last validation
expect_equivalent(signif(predict(fit1, q0),6), c(131.165, 179.024))
expect_equivalent(signif(predict(fit1, q1),6), 238.538)

out <- predict(fit1, q1, ci = TRUE, nsim = 5)
expect_equal(class(out),'data.frame')
expect_equal(length(out), 4)

out1 <- predict(fit1,q1)
expect_equivalent(out[,1],out1)

#####################
## POT
#####################

x <- replicate(20,rgpa(40, .95, -.05))
colnames(x) <- paste0('s',1:20)

fit <- FitRegLmom(x, type = 'pot')

expect_equal(fit$type, 'pot')

out <- predict(fit, 1-1/(2.5*c(100)), ci = TRUE)
out <- predict(fit, ci = TRUE)

expect_equal(names(out), c('pred','rmse','lower','upper'))

out2 <- predict(fit)
expect_equal(out2, out[,1])

})
