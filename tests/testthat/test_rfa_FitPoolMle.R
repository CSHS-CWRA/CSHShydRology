######################################################
## testing FitPoolMle functions
## Martin Durocher <mduroche@uwaterloo.ca>
#######################################################

#library(testthat)
#library(CSHShydRology)

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
fit.margin <- FitPoolMargin(yw, 'gev', method = 'lmom')

##########
## Mean ##
##########

fit <- FitPoolMle(yw, 'gev', type = 'mean')

expect_equivalent(names(fit), 
                  c("index","para","llik","type","distr","na","dim" ))

expect_equal(names(fit$index), paste0('s',1:3))
expect_equal(names(fit$para), c('xi','alpha','kappa'))
expect_equal(fit$type, 'mean')
expect_equivalent(fit$na, which(is.na(yw)))
expect_equivalent(fit$dim, c(100,3))

expect_equivalent(round(fit$para,3), c(0.873, 0.235, 0.038))

fit <- FitPoolMle(yw, 'gpa', type = 'mean')
expect_equal(names(fit$para), c('alpha','kappa'))

fit <- FitPoolMle(yw, 'glo', type = 'mean', method = 'Nelder-Mead')
fit <- FitPoolMle(yw, 'pe3', type = 'mean', control = list(maxit = 2000))
fit <- FitPoolMle(yw, 'gno', type = 'mean')

expect_equivalent(round(fit$para,3), c(0.960,  0.270, -0.292))

##########
## CV   ##
##########

fit <- FitPoolMle(yw, 'glo', type = 'cv')

expect_equal(names(fit$index), paste0('s',1:3))
expect_equal(names(fit$para), c('cv','kappa'))
expect_equal(fit$type, 'cv')

expect_equivalent(round(fit$para,3), c(0.161, -0.201))

fit <- FitPoolMle(yw, 'gev', type = 'cv', method = 'Nelder-Mead')

expect_equivalent(round(fit$para,3), c(0.267, 0.018))

fit <- FitPoolMle(yw, 'pe3', type = 'cv', control = list(maxit = 2000))
fit <- FitPoolMle(yw, 'gno', type = 'cv')

expect_error(FitPoolMle(yg, 'gpa', type = 'cv'))

#############
## shape   ##
#############

fit <- FitPoolMle(yw, 'gno', type = 'shape')

expect_equal(colnames(fit$index), paste0('s',1:3))
expect_equal(dim(fit$index), c(2,3))

expect_equal(names(fit$para), c('kappa'))
expect_equal(fit$type, 'shape')

expect_equivalent(round(fit$para,3), c(-0.332))

fit <- FitPoolMle(yw, 'gev', type = 'shape', method = 'Nelder-Mead')

expect_equivalent(round(fit$para,3), c(0.024))


fit <- FitPoolMle(yw, 'pe3', type = 'shape', control = list(maxit = 2000))
fit <- FitPoolMle(yw, 'gno', type = 'shape')

fit <- FitPoolMle(yg, 'gpa', type = 'shape')
expect_equal(names(fit$index), paste0('s',1:3))
fit
