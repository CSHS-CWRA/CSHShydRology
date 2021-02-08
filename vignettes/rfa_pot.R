## ------------------------------------------------------------------------
library(CSHShydRology)
data(flowStJohn)

## Extract the year
y <- format(flowStJohn$date,'%Y')

## Evaluate the record length of each year 
ty <- tapply(y, y, length)

## Print the years with missing values
print(ty[which(ty < 365)])

## Identify observations associated with a complete year
cid <- y %in% names(ty[ty>=365])

## Keep only complet year
xd <- cbind(flowStJohn[cid,], year = y[cid])

## ------------------------------------------------------------------------
## Random sample of GPA
rx <- rgpa(2000, 1, 0)

## Fitting POT using MLE
fit <- FitPot(rx, unit = 1)
print(fit)

## ------------------------------------------------------------------------
## Show assymetrical confidence intervals
round(coef(fit, ci = TRUE),3)

## ------------------------------------------------------------------------
## AD test using MLE and a table
GofTest(fit)

## AD test using the method of moment and bootstrap. 
fitm <- FitPot(rx, method = 'mom', varcov = FALSE) 
GofTest(fitm, method = 'ad', nsim = 500)

## ---- fig.height = 4, fig.width = 6--------------------------------------
## List of candidate thresholds
ulst <- seq(500,1500, 25)

## Mean residual life plot
r0 <- 14
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

## Create a situation where there is no treshold associated with a p-value
## greater than 0.25
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


## ----fig.height = 4,fig.width = 6----------------------------------------

## Define a threshold and the minimum separating time. Drainage Area =  14700.
thresh <- 500
r0 <- round(4 + log(14700)) # ~14 days

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

## ----fig.height=5, fig.width=6-------------------------------------------
## Predict flood quantile of return period 10 and 100 years
predict(fit, rt = c(10,100), se = TRUE, ci = 'profile')

## Return level plot
plot(fit, ci = TRUE)

## ------------------------------------------------------------------------
## Create trend in the data
idm <- xd$flow > mean(xd$flow)
xd$utrend <- xd$flow 
xd$utrend[idm] <- xd$utrend[idm] + as.integer(xd$date[idm]) * 0.02
  
## Fit the model
fit <- FitNsPot(utrend~ date, x = xd, tau = .95, declust = 'wrc', r = 14,
                thresh = ~ date)
plot(fit)


## ------------------------------------------------------------------------
## Count the number of peaks per years
xn <- data.frame(date = fit$data[fit$peak,2])
xn$year <- as.integer(format(xn$date, '%Y'))
xn <- aggregate(date~year, xn, length)

## Fitting a Poisson regression model
ff <- glm(date~year, xn, family = quasipoisson())
print(summary(ff)$coef, digit = 3)

## ------------------------------------------------------------------------
## Fit the model
fit0 <- FitNsPot(flow~ date, x = xd, tau = .95, declust = 'wrc', r = 14,
                trend = ~ 1)

fit <- FitNsPot(flow ~ date, x = xd, tau = .95, declust = 'wrc', r = 14,
                trend = ~ date)

fit3 <- FitNsPot(flow~ date, x = xd, tau = .95, declust = 'wrc', r = 14,
                trend = ~ poly(date,2))

c(AIC(fit0), AIC(fit), AIC(fit3))
plot(fit)


## ------------------------------------------------------------------------
yr <- which.day(flowStJohn$date, '0715')
yr <- yr[59:88]

## Extract the threshold and trend
xf <- fitted(fit, newdata = flowStJohn[yr,])
head(xf)

## Graph of the 
plot(fit, do.legend = FALSE)

qua <- predict(fit, rt = c(10,50), newdata = flowStJohn)
rel <- predict(fit, rt = c(10,50), newdata = flowStJohn[yr,],
               reliability = TRUE)

lines(flowStJohn$date, qua[,1], col = 'magenta', lwd = 2, lty = 2)
arrows(min(xf$time), rel[1], max(xf$time), col = 'cyan', lwd = 3,
       code = 3, length = .1 )

legend('topleft', legend = c('Flood quantile','Design level'), 
       col = c('magenta','cyan'), lty = c(2,1))

## ------------------------------------------------------------------------
hat <- ch_rfa_BootNsPot(fit, x = flowStJohn, newdata = flowStJohn[yr,], nsim = 50, 
                 reliability = TRUE, verbose = FALSE)
summary(hat, variable = 'para')
summary(hat, variable = 'qua')

## ------------------------------------------------------------------------
tau <- seq(.93, .98, .002)
candidates.tau <- SearchThreshNs(flow~date, flowStJohn, tau = tau, 
                       trend = ~ poly(date,3), thresh = ~ date, method = 'mle',
                       declust = 'wrc', r = 14, newdata = flowStJohn[yr,])

## First threshold with AD p-value > 0.25
FindThresh(candidates.tau, method = 'sgn-max', ppy = c(1,3))[,cvars]


## ---- fig.height = 8, fig.width = 8--------------------------------------
layout(matrix(c(1,3,2,4),2,2))
plot(ad~u, candidates.tau, type = 'l')
plot(mrl~u, candidates.tau, type = 'l')
plot(kappa~u, candidates.tau, type = 'l')
plot(q50~u, candidates.tau, type = 'l')

