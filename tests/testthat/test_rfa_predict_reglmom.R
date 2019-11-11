#########################################################
## Martin Durocher <mduroche@uwaterloo.ca>
#########################################################

context("Testing predict.reglmom function")

test_that("Verifying predict.reglmom", {
set.seed(48)

## Define evaluation functions
rxd <- function(x,y) max(abs(1-y/x))
axd <- function(x,y) max(abs(y-x))
rmad <- function(x,y) mean(abs(1-y/x))
mad <- function(x,y) mean(abs(y-x))

coord20 <- replicate(2,runif(20))
h20 <- as.matrix(dist(coord20))
colnames(coord20) <- c('lon','lat')

para <- cbind(rep(100,20), 30, 0)
sim20 <- RegSim(para, distr = 'gev', nrec = 1e4,
                lscale = TRUE, lmom = FALSE)

sim20 <- FindNearest(sim20, h20[2,],15)

fit1 <- FitRegLmom(sim20)

q0 <- c(.7,.93)
q1 <- .1
q2 <- c(.5, .8, .9, .95, .98, .99)

hat0 <- lmom::quagev(q0,c(100,30,0))
hat1 <- lmom::quagev(q1,c(100,30,0))
hat2 <- lmom::quagev(q2,c(100,30,0))

## verify output format
out <- predict(fit1, q0)
expect_equal(length(out), 2)
expect_equal(class(out), 'numeric')

out <- predict(fit1,q0, ci = TRUE, nsim = 5)
expect_equal(dim(out),c(2,4))
expect_equal(class(out),'data.frame')
expect_equal(colnames(out),c('pred','se','lower','upper'))
expect_equal(rownames(out), c('0.70','0.93'))


## Verify prediction
expect_true(rxd(predict(fit1, q0),hat0) < 0.01)
expect_true(rxd(predict(fit1, q1),hat1) < 0.01)
expect_true(rxd(predict(fit1),hat2) < 0.01)

out <- predict(fit1,q1, ci = TRUE, nsim = 5)
expect_equal(class(out),'data.frame')
expect_equal(length(out), 4)

})
