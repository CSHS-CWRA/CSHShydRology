## ------------------------------------------------------------------------
library(CSHShydRology)
data(flowStJohn)

## Verify the complete years of data 
y <- as.integer(format(flowStJohn$date,'%Y'))
ty <- tapply(y, y, length)
ty[which(ty< 365)]

## Keep only complet year
xd <- cbind(flowStJohn[y>=1927,], year = y[y>=1927])

## ------------------------------------------------------------------------
## Random sample of GPA
rx <- rgpa(2000, 1, 0)

## Fitting POT using MLE
fit <- FitPot(rx)
print(fit)

## ------------------------------------------------------------------------
## Show assymetrical confidence intervals
round(coef(fit, ci = TRUE),3)

## ----fig.height = 4,fig.width = 6----------------------------------------

## Define a threshold and the minimum separating time. Drainage Area =  14700.
thresh <- 500
r0 <- round(4 + log(14700)) # 14 days

## Extract peaks based on run declustering
peaks1 <- which.clusters(flow~date, xd, u = thresh, r = 5)

## Extract peaks based on WRC recommendations
peaks2 <- which.floodPeaks(flow~date, xd, u = thresh, r = r0, rlow = 0.75)

## Plot the peaks extracted for the year 1927
plot(flow~date, xd[xd$year == 1927,],type = 'l', col = 'grey')
abline(h = thresh, col = 2 ,lwd = 2)
points(flow~date, xd[peaks1,], pch = 16, col = 'blue', cex=2)
points(flow~date, xd[peaks2,], pch = 17, col = 'red')
legend('topleft', col = c('blue','red'), pch = 16:17, 
       legend = c('Cluster','WRC'))


## ------------------------------------------------------------------------
## Fit a POT model after declustering
fit <- FitPot(flow~date, xd, u = 1000, declust = 'wrc', r = r0)
print(fit)

## ------------------------------------------------------------------------
## Predict flood quantile of return period 10 and 100 years
predict(fit, rt = c(10,100), se = TRUE, ci = 'profile')

## ---- fig.height=4, fig.width=6------------------------------------------
## List of candidate thresholds
ulst <- seq(500,1500, 25)

## Mean residual life plot
PlotMrl(flow~date, xd, u = ulst, declust = 'wrc', r = r0)

## ---- fig.height = 4, fig.width = 6--------------------------------------
candidates <- SearchThresh(flow~date, xd, u = ulst, declust = 'wrc', r = r0,
                           verbose = FALSE)

PlotThresh(candidates,  type = c('kappa'))

## ---- fig.height = 4, fig.width = 6--------------------------------------
PlotThresh(candidates,  type = c('q2','q5','q10','q20','q50'))

## ---- fig.height = 4, fig.width = 6--------------------------------------
PlotThresh(candidates,  type = c('ad'))

## ------------------------------------------------------------------------
## Threshold with maximum AD p-value
cvars <- c('u','ppy','ad','q50')
FindThresh(candidates, method = 'max')[,cvars]

## First threshold with AD p-value > 0.25
FindThresh(candidates, method = 'sgn', tol.sgn = 0.25, 
           ppy = c(1,3))[,cvars]


## ------------------------------------------------------------------------
## Threshold with approximately 2.2 PPY
FindThresh(candidates, method = 'ppy', tol.ppy = 2.2)[,cvars]

## ------------------------------------------------------------------------
candidates.mod <- candidates
candidates.mod$ad <- pmin(0.2, candidates.mod$ad)

## First threshold with AD p-value > 0.25
FindThresh(candidates.mod, method = 'sgn-ppy', 
           tol.sgn = 0.25, tol.ppy = 2.2)[,cvars]

FindThresh(candidates.mod, method = 'sgn-ppy', 
           tol.sgn = 0.15, tol.ppy = 2.2)[,cvars]

## ------------------------------------------------------------------------
## Verify the discrepencies with reference threshold ~ 1PPY
FindThresh(candidates, method = 'sgn', ppy = c(1,3), tol.sgn = 0.0, 
           qua = 'q50', tol.qua = .01)[,cvars]

FindThresh(candidates, method = 'sgn', ppy = c(1,3), tol.sgn = 0.25, 
           qua = 'q50', tol.qua = .01)[,cvars]


