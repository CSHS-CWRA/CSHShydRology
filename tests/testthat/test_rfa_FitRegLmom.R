##############################################################################
## Martin Durocher <mduroche@uwaterloo.ca>
#############################################################################

context("Testing FitRegLmom function")

test_that("Verifying FitRegLmom", {

require(lmomRFA)

## Import data for tests
set.seed(0)

lmm0 <- c(100,30,.3)
lmom20 <- t(replicate(20,lmm0))
coord20 <- replicate(2,runif(20))
h20 <- as.matrix(dist(coord20))
colnames(coord20) <- c('lon','lat')

## Define evaluation functions
rxd <- function(x,y) max(abs(1-y/x))
axd <- function(x,y) max(abs(y-x))
rmad <- function(x,y) mean(abs(1-y/x))
mad <- function(x,y) mean(abs(y-x))

nrec0 <- 10001:10020
sim20 <- RegSim(lmom20, distr = 'gev', nrec = nrec0, lscale = TRUE)

fit1 <- FitRegLmom(sim20, distr = 'gno')

## verify the output format
expect_equal(names(fit1),c('type', 'distr', 'para', 'lmom','rlmom',
                           'nrec', 'stat', 'discord'))

expect_true( all(fit1$nrec == nrec0))
expect_equal(dim(fit1$lmom), c(20,4))

## Verify that the right regional L-moment and parameter are found
expect_true(mad(fit1$rlmom[1:3], c(1,.3,.3)) <.01)
expect_true(mad(pelgno(fit1$rlmom),fit1$para) < 1e-4)

## verify that right number of lmom is returned
fit <- FitRegLmom(sim20, distr = 'gev', nmom = 5)
expect_equal(ncol(fit$lmom), 5)

fit <- FitRegLmom(sim20, distr = 'gev', nmom = 1)
expect_equal(ncol(fit$lmom), 4)

fit <- FitRegLmom(sim20, distr = 'wak', nmom = 1)
expect_equal(ncol(fit$lmom), 5)

fit <- FitRegLmom(sim20, distr = 'gum', nmom = 7)
expect_equal(ncol(fit$lmom), 7)
expect_equal(names(fit$rlmom),
             c("L1", "LCV", "LSK", "LKUR", "TAU5", "TAU6", "TAU7"))

## Verify that the diagnostics work

nrec0 <- 101:120
sim20 <- RegSim(lmom20, distr = 'gev', nrec = nrec0, lscale = TRUE)

fit1 <- FitRegLmom(sim20, diagnostic = TRUE)

## right format
expect_true(length(fit1$stat)==8)
expect_true(length(fit1$discord)==20)

## right selection
lstd <- c('glo','gev','gno','pe3','gpa')
expect_true(lstd[which.min(abs(fit1$stat[4:8]))] == fit1$distr)

## diagnostics done with the right arguments
fit1 <- FitRegLmom(sim20)
expect_true(length(fit1$stat)==8)

fit1 <- FitRegLmom(sim20, distr = 'gev', diagnostic = TRUE)
expect_true(length(fit1$stat)==8)

fit1 <- FitRegLmom(sim20, distr = 'gev', diagnostic = FALSE)
expect_null(fit1$stat)
expect_null(fit1$discord)

fit1 <- FitRegLmom(sim20, distr = 'gev')
expect_null(fit1$stat)
expect_null(fit1$discord)

## test fitting a pooling group using POT

set.seed(12)

coord5 <- replicate(2,runif(5))
h5 <- as.matrix(dist(coord5))

para <- cbind(rep(0,5), 1.1, -.1)
para0 <- c(0,.9,-.1)

sim <- RegSim(para, distr = 'gpa', nrec = 1e4, lmom = FALSE)

fit1 <- FitRegLmom(sim, type = 'pot')
fit2 <- FitRegLmom(sim, distr = 'gpa')

expect_true(fit1$para[1] == 0)
expect_false(any(fit1$para == fit2$para))

expect_true(axd(fit1$para,para0) < 0.01)
expect_true(axd(fit2$para,para0) < 0.01)

})
