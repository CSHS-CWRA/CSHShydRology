######################################################
## Martin Durocher <mduroche@uwaterloo.ca>
#######################################################

context("Testing FitPoolMargin function")

test_that("Verifying FitPoolMargin", {

## Create a validation dataset
u <- (1:100)/101
yw <- data.frame( s1 = qgev(u, 100, 30, -0.05),
                  s2 = qgev(u, 105, 27,  0.05),
                  s3 = qgev(u, 95,  32,  0.00))

yw[1:10,1] <- NA
yw[90:100,2] <- NA

yg <- data.frame( s1 = qgpa(u,  8, -0.05),
                  s2 = qgpa(u, 12,  0.05),
                  s3 = qgpa(u, 10,  0.00))

#################
## Fit margin
#################

## Simulate based on marginal model
fit <- FitPoolMargin(yw, 'gev', method = 'lmom')

## verify the structure
expect_equal(names(fit), c("para", "distr", "na", "dim" ))
expect_equal(fit$distr, 'gev')
expect_equal(fit$dim, c(100,3))
expect_true(all(is.na(as.matrix(yw)[fit$na])))
expect_equal(dim(fit$para), c(3,3))
expect_equal(colnames(fit$para), colnames(yw))

expect_equal(rownames(fit$para), 
             c('xi','alpha','kappa'))

expect_equivalent(round(fit$para[,1], 1), 
                  c(106.2, 25.7,-0.1))

## Need more than one site
expect_error(FitPoolMargin(yw[,1], 'gev', method = 'lmom'))

## Verify that all cominations of method and distribution work
out <- FitPoolMargin(yg, 'gpa', method = 'mle')
out <- FitPoolMargin(yw, 'glo', method = 'mle')
out <- FitPoolMargin(yw, 'gno', method = 'mle', 
                     method.optim = 'Nelder-Mead')
out <- FitPoolMargin(yw, 'pe3', method = 'mle', 
                     method.optim = 'BFGS', control = list(maxit = 2000))

out <- FitPoolMargin(yw, 'gpa', method = 'lmom')
out <- FitPoolMargin(yw, 'glo', method = 'lmom')
out <- FitPoolMargin(yw, 'gno', method = 'lmom')
out <- FitPoolMargin(yw, 'pe3', method = 'lmom')

#################
## Predict margin
#################

p0 <- c(0.9,0.9911)
out <- FitPoolMargin(yw, 'gev', method = 'lmom')
qua <- predict(out, p0)
expect_equal(rownames(qua), c('0.9','0.991'))
expect_equal(colnames(qua), paste0('s',1:3))

pp <- out$para[,1]
expect_equivalent(qgev(p0,pp[1],pp[2],pp[3]), qua[,1])

p0 <- c(0.8, 0.95)
out <- FitPoolMargin(yg, 'gpa', method = 'mle')
qua <- predict(out, p0)

expect_equal(rownames(qua), c('0.8','0.95'))
expect_equal(colnames(qua), paste0('s',1:3))
pp <- out$para[,1]
expect_equivalent(qgpa(p0,pp[1],pp[2]), qua[,1])

###################
## simulate margin
###################

set.seed(1)
sim <- simulate.poolmargin(fit, nsim = 1, corr = .5)

## verify output 
expect_equal(class(sim), 'matrix')
expect_equal(dim(sim), c(100,3))
expect_equivalent(which(is.na(sim)), fit$na)
expect_true(mean(sim[,1], na.rm = TRUE) > 5) ## not scale margins

## Make sure that there is correlation
cc <- mean(cor(sim, use = 'pairwise.complete.obs')[c(2:3,6)])
expect_true(cc > 0.3)

sim <- simulate.poolmargin(fit, nsim = 30)

expect_equal(class(sim), 'list')
expect_equal(length(sim), 30)

})