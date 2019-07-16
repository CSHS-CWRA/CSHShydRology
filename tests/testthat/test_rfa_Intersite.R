##############################################################################
# Testing intersite function
# Martin Durocher <mduroche@uwaterloo.ca>
#############################################################################

library(lmomRFA)

## Set simulation data
set.seed(0)

data(flowUngauged)
lmom5 <- flowUngauged[1:5, c('l1','lcv','lsk')]
lmom20 <- flowUngauged[1:20, c('l1','lcv','lsk')]

coord5 <- replicate(2,runif(5))
h5 <- as.matrix(dist(coord5))

coord20 <- replicate(2,runif(20))
h20 <- as.matrix(dist(coord20))

colnames(coord5) <- colnames(coord20) <- c('lon','lat')

## Create a simulation of a large dataset
set.seed(8)

corr.mat <- exp(-h20)
nrec0 <- sample(10001:10020)

sim <- RegSim(lmom20, distr = 'gev', nrec = nrec0,
              corr = corr.mat, long = TRUE)

sim$site <- factor(sim$site)
lvl <- paste0('site_',1:20)
levels(sim$site) <- lvl


## ------------------------
## Case emp
## ------------------------

smat <- DataWide(value ~ site + time, sim)

ifit <- Intersite(smat, method = 'emp')

expect_equal(ifit$method, 'emp')

expect_equal(colnames(ifit$corr), lvl)
expect_equal(rownames(ifit$corr), lvl)

expect_equal(colnames(ifit$model), lvl)
expect_equal(rownames(ifit$model), lvl)

expect_true(ifit$para[1] >.1)
expect_null(ifit$distance)
expect_null(ifit$rmse)

## test definite positivness
decomp <- chol(ifit$model)

## verify that the correlation are good
mad <- mean(abs(as.vector(corr.mat - ifit$corr)))
expect_true(mad < 0.01)

mad <- mean(abs(as.vector(ifit$model - ifit$corr)))
expect_true(mad < 1e-4)

## Verify the minimum number of pairs

sby <- split(sim,sim[,2])
sby[[1]] <- sby[[1]][1:100,]
sby[[2]] <- sby[[2]][190:1000,]
sim1 <- do.call(rbind, sby)

## If there is no common pairwise records
x1 <- DataWide(value ~ site + time, sim1)
ifit <- Intersite(x1, method = 'emp')
expect_true(is.na(ifit$corr[1,2]))

## if there is less than 100 pairwise records
ifit <- Intersite(x1, method = 'emp', nmin = 100)
expect_true(is.na(ifit$corr[5,1]))

## impute zero
ifit <- Intersite(x1, method = 'emp', nmin = 100,
                  na.sub = 'zero', defpos = FALSE)

expect_equal(ifit$model[5,1], 0)

## impute avg (default)
ifit <- Intersite(x1, method = 'emp', nmin = 100,
                  defpos = FALSE)

expect_equal(ifit$model[5,1], ifit$para[1])

## correct for definite positivness
ifit <- Intersite(x1, method = 'emp', nmin = 100,
                  na.sub = 'zero')

mad <- abs(ifit$model[5,1]-ifit$model[2,1])
expect_true(mad > 1e-8)

## ------------------------
## Case method = exp
## ------------------------

## Fit the model and verify that a warning is issue if the label don't match
rownames(h20) <- colnames(h20) <- paste0(lvl,'0')

x0 <- DataWide(value ~ site + time, sim)

expect_warning(Intersite(x0, method = 'exp', distance = h20))

rownames(h20) <- colnames(h20) <- lvl
ifit <- Intersite(x0, method = 'exp',  distance = h20)

expect_equal(ifit$method, 'exp')

expect_equal(names(ifit$para),c('nugget', 'range', 'smooth'))
mad <- max(abs(ifit$para - c(0,3,1)))
expect_true(mad < .05)

mad <- mean(abs(as.vector(ifit$model-corr.mat)))
expect_true(mad < 0.01)

mad <- mean(abs(as.vector(corr.mat - ifit$corr)))
expect_true(mad < 0.01)

