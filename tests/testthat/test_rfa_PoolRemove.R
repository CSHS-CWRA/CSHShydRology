#########################################################
## Verify the removing sites
## Martin Durocher <mduroche@uwaterloo.ca>
#########################################################

rm(list = ls())
set.seed(20)

## prepare a dataset
coord20 <- replicate(2,runif(20))
h20 <- as.matrix(dist(coord20))
colnames(coord20) <- c('lon','lat')

para <- cbind(rep(100,20), 30, -.2)
distr <- c(rep('gev',18), rep('pe3',3))
sim20 <- RegSim(para, distr = distr, nrec = 300,
                lscale = TRUE, lmom = FALSE)

fit1 <- FitRegLmom(sim20)

## Remove no site
out1 <- PoolRemove(fit1, nmin = 30)
expect_equal(out1, fit1)

## Remove one site
out1 <- PoolRemove(fit1, nmin = 19)
expect_equal(nrow(out1$lmom), 19)

mid <- 20
expect_equal(out1$nrec, fit1$nrec[-mid])
expect_equal(out1$lmom, fit1$lmom[-mid,])
expect_equal(length(out1$discord), length(fit1$discord[-mid]))

expect_false(any(out1$para == fit1$para))
expect_true(out1$stat[1] < fit1$stat[1])


out1 <- PoolRemove(fit1, nmin = 15)
expect_true(out1$stat[1] < 2)

out1 <- PoolRemove(fit1, ntot.min = 5900)
expect_true(nrow(out1$lmom) == 20)

## The first site must be protected for removing
sim21 <- sim20[,c(20,2:19,1)]

fit1 <- FitRegLmom(sim21)
out1 <- PoolRemove(fit1, nmin = 19)

expect_equal(out1$lmom[1,],fit1$lmom[1,])
