##############################################################################
# Testing simulation function RegSim
# Martin Durocher <mduroche@uwaterloo.ca>
#############################################################################

library(lmomRFA)
rm(list = ls())

## Import data for testing
set.seed(0)

data(flowUngauged)
lmom5 <- flowUngauged[1:5, c('l1','lcv','lsk')]
lmom20 <- flowUngauged[1:20, c('l1','lcv','lsk')]

coord5 <- replicate(2,runif(5))
h5 <- as.matrix(dist(coord5))

coord20 <- replicate(2,runif(20))
h20 <- as.matrix(dist(coord20))

colnames(coord5) <- colnames(coord20) <- c('lon','lat')

## -------------------------------- ##
## Section should work correctly    ##
## -------------------------------- ##

## Define evaluation functions
rxd <- function(x,y) max(abs(1-y/x))
axd <- function(x,y) max(abs(y-x))
rmad <- function(x,y) mean(abs(1-y/x))
mad <- function(x,y) mean(abs(y-x))

## Verify that the right dimension is output
sim5 <- RegSim(lmom5, distr = 'gev', nrec = 3)
sim20 <- RegSim(lmom20, distr = 'pe3', nrec = 7)

expect_equal(dim(sim5), c(3,5))
expect_equal(dim(sim20), c(7,20))
expect_equal(class(sim5), 'matrix')
expect_null(colnames(sim5))
expect_null(rownames(sim5))

## Verify that record length are correct
sim <- RegSim(lmom5, distr = 'gev', nrec = 11:15)
sim.n <- apply(!apply(sim,2,is.na),2, sum)

expect_equal(sim.n, 11:15)

## Verify that the long format work

sim <- RegSim(lmom5, distr = 'gno', nrec = 11:15, long = TRUE)
sim.agg <- aggregate(value ~ site, sim, length)

expect_equal(dim(sim.agg),c(5,2))
expect_equal(sim.agg[,2],11:15)
expect_equal(names(sim.agg), c('site','value'))
expect_equal(class(sim.agg),'data.frame')

## Verify that using constant correlation works
set.seed(1)

fun <- function(rho){
  sim <- RegSim(lmom5, distr = 'gev', nrec = 1e5, corr = rho)
  corr <- cor(sim, method = 'spearman')
  corr <- 2*sin(pi/6*corr)
  axd(corr[lower.tri(corr)], rho)
}

expect_true(fun(0)  < .01)
expect_true(fun(.4) < .01)
expect_true(fun(.9) < .01)


## Verify that correlation coefficients are of correct
set.seed(2)

mat.corr <- exp(-h5)

fun <- function(rho, L = FALSE){
  sim <- RegSim(lmom5, distr = 'gev', nrec = 1e5, corr = rho, corr.sqrt = L)
  corr <- cor(sim, method = 'spearman')
  corr <- 2*sin(pi/6*corr)
  axd(corr,rho)
}

expect_true(fun(mat.corr) <.01)

## Verify that correlation coefficients are correct using corr.sqrt

set.seed(2)
mat.corr.sqrt <- chol(mat.corr)
sim <- RegSim(lmom5, distr = 'gev', nrec = 1e5, corr = mat.corr.sqrt, corr.sqrt = TRUE)
corr <- cor(sim, method = 'spearman')
corr <- 2*sin(pi/6*corr)

expect_true(axd(mat.corr,corr) <.01)

## Verify that the L-moments are correct

set.seed(3)

sim <- RegSim(lmom5, distr = 'pe3', nrec = 1e5)

sim.lmom <- t(apply(sim, 2, samlmu))

expect_true(rmad(lmom5[,1],sim.lmom[,1]) < 0.01)
expect_true(rmad(lmom5[,2],sim.lmom[,2]/sim.lmom[,1]) < 0.01)
expect_true(rmad(lmom5[,3],sim.lmom[,3]) < 0.01)

## Verify that using lscale = TRUE works
set.seed(4)

lmom5.mod <- lmom5
lmom5.mod[,2] <- lmom5[,1] * lmom5[,2]

sim.mod <- RegSim(lmom5.mod, distr = 'gno', nrec = 1e5, lscale = TRUE)

sim.mod.lmom <- t(apply(sim.mod, 2, samlmu))

expect_true(rmad(lmom5[,1],sim.mod.lmom[,1]) < 0.01)
expect_true(rmad(lmom5[,2],sim.mod.lmom[,2]/sim.lmom[,1]) < 0.01)
expect_true(rmad(lmom5[,3],sim.mod.lmom[,3]) < 0.01)

## Verify that passing parameter directly works
set.seed(5)

para5 <- t(apply(lmom5.mod, 1, lmom::pelgno))

sim <- RegSim(para5, distr = 'gno', nrec = 1e5, lmom = FALSE)

sim.lmom <- t(apply(sim, 2, samlmu))

expect_true(rmad(lmom5[,1],sim.lmom[,1]) < 0.01)
expect_true(rmad(lmom5[,2],sim.lmom[,2]/sim.lmom[,1]) < 0.01)
expect_true(rmad(lmom5[,3],sim.lmom[,3]) < 0.01)

## Verify the right distribution is selected
set.seed(6)

lmm <- c(100, 30, .2)
distr <- c('gev','gno','glo','pe3')
para <- rbind(pelgev(lmm),
              pelgno(lmm),
              pelglo(lmm),
              pelpe3(lmm))

lmom <- rbind(lmrgev(para[1,],4),
              lmrgno(para[2,],4),
              lmrglo(para[3,],4),
              lmrpe3(para[4,],4))

sim <- RegSim(para, distr = distr, nrec = 1e5, lmom = FALSE)

sim.lmom <- t(apply(sim, 2, samlmu))
expect_true(rmad(lmom[,1],sim.lmom[,1]) < 0.01)
expect_true(rmad(lmom[,2],sim.lmom[,2]) < 0.01)
expect_true(rmad(lmom[,3],sim.lmom[,3]) < 0.01)
expect_true(rmad(lmom[,4],sim.lmom[,4]) < 0.01)

sim <- RegSim(lmom, distr = distr, nrec = 1e5, lscale = TRUE)
sim.lmom <- t(apply(sim, 2, samlmu))

expect_true(rmad(lmom[,1],sim.lmom[,1]) < 0.01)
expect_true(rmad(lmom[,2],sim.lmom[,2]) < 0.01)
expect_true(rmad(lmom[,3],sim.lmom[,3]) < 0.01)
expect_true(rmad(lmom[,4],sim.lmom[,4]) < 0.01)


## Verify behavior with kappa distribution
set.seed(7)

sim <- RegSim(lmom[-3,], distr = 'kap', nrec = 1e5, lscale = TRUE)

sim.lmom <- t(apply(sim, 2, samlmu))
expect_true(rmad(lmom[-3,1],sim.lmom[,1]) < 0.01)
expect_true(rmad(lmom[-3,2],sim.lmom[,2]) < 0.01)
expect_true(rmad(lmom[-3,3],sim.lmom[,3]) < 0.01)
expect_true(rmad(lmom[-3,4],sim.lmom[,4]) < 0.01)

sim <- RegSim(lmom, distr = c('nor','exp','gum','kap'),
              nrec = 10, lscale = TRUE)
expect_equal(dim(sim), c(10,4))

## -------------------------------- ##
## Section Must raised an error     ##
## -------------------------------- ##

## Verify when length of arguments are not the right format
expect_error(RegSim(lmom5, distr = 'gev', nrec = c(10,10)))
expect_error(RegSim(lmom5, distr = c('gev','glo'), nrec = 10))
expect_error(RegSim(lmom5, distr = 'kap', nrec = 10))

## Verify robustess to missing values
lmom5.mod <- lmom5
lmom5.mod[3,3] <- NA

expect_error(RegSim(lmom5.mod, distr = 'gev', nrec = 10))

expect_error(RegSim(lmom5, distr = c('gum',NA,'gev','pe3',NA), nrec = 10))
expect_error(RegSim(lmom5, distr = NA, nrec = 10))
expect_error(RegSim(lmom5, distr = 'gev', nrec = NA))
expect_error(RegSim(lmom5, distr = 'gev', nrec = c(1:4,NA)))

corr.mod <- matrix(.4,5,5)
diag(corr.mod) <- 1
corr.mod[2,1] <- NA

expect_error(RegSim(lmom5, distr = 'gev', nrec = 10, corr = NA))
expect_error(RegSim(lmom5, distr = 'gev', nrec = 10, corr = corr.mod))

## Correlation must be positive definite
expect_error(RegSim(lmom5.mod, distr = 'gev', nrec = 10, corr = -1))

