## ------------------------------------------------------------------------
library(CSHShydRology)
data(flowUngauged)

## Transform data if necessary.
xd0 <- with(flowUngauged, 
  data.frame(area   = scale(log(area)),
             wb     = scale(log(wb)),
             stream = scale(log(stream)),
             map    = scale(log(map)))
  )

## ------------------------------------------------------------------------
## Project the coordinates
coord <- cmdscale(GeoDist(~lon+lat,flowUngauged))
colnames(coord) <- c('lon','lat')

xd0 <- cbind(xd0, coord)

## ------------------------------------------------------------------------
## Convert L-moments to parameter
para100 <- apply(flowUngauged[,c('l1','lcv','lsk')], 
                 1, lAmax, distr = 'gev', lscale = FALSE)

## Estimate at-site flood quantiles
F100 <- function(z) qAmax(0.99, z, distr = 'gev')
qua100 <- apply(para100, 2, F100)
xd0 <- cbind(xd0, q100 = qua100)

## ------------------------------------------------------------------------
## Create a set of ungauged sites
id <- seq(1,501,20)
xd <- xd0[-id, ]
target <- xd0[id,]

## ------------------------------------------------------------------------
## Define the ROI model
formula.phy <- log(q100) ~ area + map + stream + wb
formula.dist <- ~ area + map + stream + wb

## Fit the model
fit <- FitRoi(x = xd, xnew = target, nk = 30, 
              phy = formula.phy, similarity = formula.dist) 
print(fit)

## ----fig.height = 4, fig.width = 6---------------------------------------
## Graphics of the predicted versus known flood quantiles a ungauged sites
plot(log(target$q100),fit$pred,
     xlab = 'At-site flood quantiles (log)',
     ylab = 'Predicted flood quantiles (log)')
abline(0,1)


## ------------------------------------------------------------------------
## List of size to try.
nk.lst <- seq(30,100,10)

## Perform cross-validation
cv0 <- CvRoi(x = xd, nk = nk.lst, fold = 5,
            phy = formula.phy, similarity = formula.dist,
            verbose = FALSE)

## output results
head(signif(cv0,3), crit = 'mad')

## ----fig.height = 4, fig.width = 6---------------------------------------
## Mean absolute deviation with respect to 'nk'
plot(cv0, crit = 'mad')


## ------------------------------------------------------------------------

## Create cross-validation groups
set.seed(392)
kf <- sample(rep_len(1:5, nrow(xd)))

## Perform cross-validation with all descriptors
cv0 <- CvRoi(x = xd, nk = nk.lst, fold = kf,
            phy = formula.phy, similarity = formula.dist,
            verbose = FALSE)

## Formula without wb and stream
formula.phy2 <- log(q100) ~ area + map 

cv1 <- CvRoi(x = xd, nk = nk.lst, fold = kf,
            phy = formula.phy2, similarity = formula.dist,
            verbose = FALSE)

## Compare the prediction power
head(signif(cv0,3), crit = 'mad')
head(signif(cv1,3), crit = 'mad')



## ------------------------------------------------------------------------
## Evaluate the predictions and residuals from the cross-validation for a given 
## ROI model

fit <- FitRoi(x = xd, xnew = target, nk = 50, 
              phy = formula.phy, similarity = formula.dist) 

hat <- predict(fit, xd, fold = kf)
res <- residuals(fit, xd, fold = kf)

## Median of relative absolute error
median(abs(res/hat))


## ---- warning = FALSE----------------------------------------------------
formula.krig <- ~ lon + lat

cvk <- CvRoi(x = xd, nk = nk.lst, fold = kf,
            phy = formula.phy, similarity = formula.dist,
            kriging = formula.krig, verbose = FALSE)

## ---- warning = FALSE, fig.height=4, fig.width = 6-----------------------
## Effect of kriging of the prediction of flood quantiles.
plot(cvk, ylim = c(0.38, 0.45))
lines(mad~nk, cv0, col = 'red')
legend('topleft', horiz = TRUE, 
       legend = c('with','without'),
       col = c('black','red'), lty = rep(1,2))

## ------------------------------------------------------------------------
## Size of the bootstrap sample
nboot <- 30

## Empirical L-moments
lmm <- flowUngauged[-id, c('l1','lcv','lsk')]

## Function that return the flood quantiles of 100 years return period
## for one site.
Fqua <- function(z){
  l <- lmom::samlmu(z) 
  p <- lmom::pelgev(l)
  return(lmom::quagev(.99,p))
}

## Function that simulates a dataset and return flood quantiles for all sites
Fsim <- function(){
  xs <- RegSim(lmm, 'gev', nrec = 50, corr = .4)
  return(apply(xs, 2, Fqua))
}

## Perform bootstrap for the at-site analysis
set.seed(12)
lq100 <- log(replicate(nboot, Fsim()))


## ------------------------------------------------------------------------
## Fit the model
fit <- FitRoi(x = xd, xnew = target, nk = 50, 
              phy = formula.phy, similarity = formula.dist,
              fitted = TRUE) 

## Extract model residuals assuming they are symmetrical.
resid.mdl <- c(fit$resid, -fit$resid)

## create a copy that could be modified
xd.boot <- xd

## Fit ROI on a bootstrap sample
Fboot <- function(){
  
  ## sample residual of sampling and modeling errors.
  boot.jj <- sample.int(nboot, 1)
  boot.resid <- sample(resid.mdl, nrow(xd), replace = TRUE)
  xd.boot$q100 <- exp(lq100[,boot.jj] + boot.resid)
  
  return(FitRoi(x = xd.boot, xnew = target, nk = 50, 
              phy = formula.phy, similarity = formula.dist)$pred) 
}

boot <- replicate(nboot, Fboot())

## Evaluate the standard deviation of the bootstrap sample.
boot.se <- apply(boot, 1, sd)


