###############################################################################
## Test function FitNsAmax
## Martin Durocher <mduroche@@uwaterloo.ca>
##############################################################################
rm(list = ls())
library(lmomco)

## built a sample
ny <- 100
id <- c(66,35,27,97,61,21,13,45,86,55,88,100,32,38,94,2,96,9,37,52,80,40,87,14,
        58,99,78,16,17,43,41,6,3,44,62,39,36,34,89,31,70,64,77,15,60,25,10,79,
        69,93,84,28,73,22,24,83,46,4,12,51,42,75,72,29,90,57,68,53,67,11,19,91,
        59,50,54,23,48,18,76,30,81,71,49,56,65,63,98,26,33,7,8,95,1,47,85,82,92,
        5,74,20)

form <- flow~date
distr <- 'gev'

u <- (1:ny)/(ny+1)
x0 <- qAmax(u, c(1,.2,0), distr)[id]
dd <- seq(2000, length.out = ny)
mu0 <- seq(100, 120, length.out = ny)
x <- data.frame(flow = x0 * mu0, date = dd)

## Test all combination of model
obj <- FitNsAmax(flow ~ date, x, 'glo', type = 'mult')
obj <- FitNsAmax(flow ~ date, x, 'glo', type = 'add')
obj <- FitNsAmax(flow ~ date, x, 'gno', type = 'mult')
obj <- FitNsAmax(flow ~ date, x, 'gno', type = 'add')
obj <- FitNsAmax(flow ~ date, x, 'pe3', type = 'mult')
obj <- FitNsAmax(flow ~ date, x, 'pe3', type = 'add')
obj <- FitNsAmax(flow ~ date, x, 'gev', type = 'mult')

## Fit a GEV model
obj <- FitNsAmax(form, x, distr = 'gev', type = 'add')

## verify the structure of the output
expect_true(class(obj) %in% 'nsamax')

oname <- c("formula","data","beta","fitted","type","para")
expect_equal(names(obj), oname)
expect_equal(obj$data[,1], x$flow)
expect_equal(obj$data[,2], x$date)

expect_equal(obj$type, 'add')
expect_equal(nrow(obj$data), ny)
expect_equal(length(obj$fitted), ny)
expect_equal(x$flow - fitted.values(obj), 
             residuals(obj))

expect_equal(obj$para$type, 'gev')
expect_equal(names(obj$para$para), c('xi','alpha','kappa'))

## Normal
obj <- FitNsAmax(form, x, distr = 'nor', type = 'add')
expect_equal(obj$para$type, 'nor')
expect_equal(names(obj$para$para), c('mu','sigma'))

## pe3 ##
obj <- FitNsAmax(form, x, distr = 'pe3', type = 'add')
expect_equal(obj$para$type, 'pe3')
expect_equal(names(obj$para$para), c('mu','sigma','gamma'))


## verify that formula accept transformation and nonlinear covariate
form0 <- log(flow) ~ poly(date,2)
obj <- FitNsAmax(form0, x, distr = 'gev', type = 'add')


## Verify that it choses the right distribution
Fz <- function(z) AIC(FitNsAmax(form, x, distr = z, type = 'add'))
distrs <- c('gum','gev','gno','pe3','glo')
distr.best <- distrs[which.min(sapply(distrs, Fz))]
obj <- FitNsAmax(form, x, distr = distrs, type = 'add')
expect_equal(obj$para$type, distr.best)

## using multiplicative model
obj.mult <- FitNsAmax(form, x, distr = 'gev', type = 'mult')
expect_equal(obj.mult$type, 'mult')
expect_equal(x$flow / fitted.values(obj.mult), residuals(obj.mult))


obj.add <- FitNsAmax(form, x, distr = 'gev', type = 'add')
cref <- c(-243,0.1782, -11.98,21.82,0.02934)
expect_equivalent(signif(coef(obj.add), 4),cref)

##############################################################################

## try the simulate function
sim <- simulate(obj.add, u = u[id])
expect_equal(length(sim), ny)

## Verify the prediction
ya <- predict(obj.add, c(.9,.99))
qref <- c(149.0, 149.1, 195.4, 195.5)
expect_equivalent(as.numeric(signif(ya[1:2,1:2],4)), qref)

ym <- predict(obj.mult)
qref <- c(109.5, 109.6, 131.8, 132.0)
expect_equivalent(as.numeric(signif(ym[1:2,1:2],4)), qref)

expect_equal(dim(ya), c(ny, 2))
expect_equal(dim(ym), c(ny, 6))

## Verify the prediction of design level
qrel <- predict(obj.add, c(.9,.99), 
               newdata = obj.add$data[95:100,], 
               reliability = TRUE)

expect_true(qrel[1] <= max(ya[,1]))
expect_true(qrel[1] >= min(ya[,1]))
expect_true(qrel[2] <= max(ya[,2]))
expect_true(qrel[2] >= min(ya[,2]))

## Verify that the simulate function works
simulate(obj.add)

## Perform bootstrap and verify the output
nsim0 <- 10
boot.add <- BootNsAmax(obj.add, nsim = nsim0, verbose = FALSE)

expect_equal(names(boot.add), c('para','beta','qua'))
expect_equal(nrow(boot.add$para), nsim0)
expect_equal(colnames(boot.add$para), names(obj.add$para$para))
expect_equal(colnames(boot.add$beta), names(obj.add$beta))
expect_equal(colnames(boot.add$qua[,,1]), 
             as.character(c(0.5,0.8,0.9,0.95,0.98,0.99)))

expect_equal(dim(boot.add$qua), c(ny,6,nsim0))

## Verify the output of the summary function
ss <- summary(boot.add)

expect_equal(colnames(ss), c('mean','se','lower','upper'))

summary(boot.add, 'beta')
ss <- summary(boot.add, 'qua')

expect_equal(dim(ss), c(ny, 24))

## verify that it work when using reliability
boot.add <- BootNsAmax(obj.add, 
                       p = c(.35,.9), 
                       nsim = nsim0,
                       newdata = obj.add$data[91:100,],
                       reliability = TRUE)
ss <- summary(boot.add, 'qua')

expect_equal(dim(ss), c(2,4))


## verify that it works with various formula

obj <- FitNsAmax(log(flow) ~ poly(date,3), x, distr = 'gev', type = 'add')

b <- BootNsAmax(obj, 
                p = c(.35,.9), 
                nsim = 100,
                newdata = obj$data[81:88,],
                reliability = TRUE)

summary(b, 'qua')


###############################################################################
## Test the function FitNsAmaxMle
###############################################################################

fit <- FitNsAmaxMle(flow ~ I(date-2000), x, 'gev', type = 'add', 
                     method = 'BFGS', 
                     control = list(maxit = 2000, ndeps = rep(1e-5,4)))

fit0 <- FitNsAmaxMle(log(flow) ~ I(date-2000), x, 'pe3', type = 'add', 
                     method = 'BFGS', 
                     control = list(maxit = 2000, ndeps = rep(1e-5,4)))

fitm <- FitNsAmaxMle(flow ~ date, x, 'glo', type = 'mult', method = 'BFGS')
fitm <- FitNsAmaxMle(flow ~ date, x, 'gno', type = 'mult', method = 'BFGS')
fitm <- FitNsAmaxMle(flow ~ date, x, 'pe3', type = 'mult', method = 'BFGS')
fitm <- FitNsAmaxMle(flow ~ date, x, 'gev', type = 'mult', method = 'BFGS')

fita <- FitNsAmaxMle(flow ~ date, x, 'glo', type = 'add', method = 'BFGS')
fita <- FitNsAmaxMle(flow ~ date, x, 'gno', type = 'add', method = 'BFGS')
fita <- FitNsAmaxMle(flow ~ date, x, 'pe3', type = 'add', method = 'BFGS')
fita <- FitNsAmaxMle(flow ~ date, x, 'gev', type = 'add', method = 'BFGS')

print(fit)

plot(form,x, ylim = c(50,250))

sset <- 10:100
dd0 <- x$date[sset]
trend <- predict(fit, newdata = x[sset,], type = 'location')
lines(dd0 ,trend, col = 2)

trend.scale <- predict(fit, newdata = x[sset,], type = 'scale')
lines(dd0, trend - trend.scale, col = 'blue')
lines(dd0, trend + trend.scale, col = 'blue')

plot(form,x, ylim = c(50,250))

qua <- predict(fita, newdata = x[sset,], type = 'quantile')

for(ii in 1:6)
  lines(dd0, qua[,ii], col = 'darkgreen')

rel <- predict(fit, p = c(.8,.98), 
               newdata = x[sset,], type = 'reliability')

for(ii in 1:6)
  lines(dd0, rep(rel[ii],length(dd0)), col = 'magenta')

out <- BootNsAmaxMle(fit,  newdata = x[sset,], reliability = TRUE, nsim = 5,
                      method = 'BFGS')

expect_equivalent(colnames(out$para), names(fit$para))
