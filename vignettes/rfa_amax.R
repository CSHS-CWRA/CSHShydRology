## ------------------------------------------------------------------------
library(CSHShydRology)
data("flowStJohn")

## ------------------------------------------------------------------------
## Extract the annual maxima
an <- ExtractAmax(flow ~ date, flowStJohn, tol = 365)
nrow(an)

## ------------------------------------------------------------------------
fitMle <- FitAmax(an$flow, 'gev', method = 'mle')
print(fitMle)

## ------------------------------------------------------------------------
## Flood quantiles of 10 and 100 return periods
predict(fitMle, q = c(.9,.99), se = TRUE, ci = 'delta', alpha = .1)

## ------------------------------------------------------------------------
## Fitting using L-moments
fitLmm <- FitAmax(an$flow, 'gev', method = 'lmom', varcov = FALSE)

## Prediction using bootstrap
out <- predict(fitLmm, q = c(.9,.99), ci = 'boot', 
               nsim = 500, out.matrix = TRUE)

## Structure of the output
names(out)

print(out$pred)

## ---- fig.height= 4,fig.width=6------------------------------------------
## Return level plot
plot(fitMle, ci = TRUE)

## ------------------------------------------------------------------------
## Anderson-Darling test of goodness of fit
GofTest(fitLmm, nsim = 500)

## ------------------------------------------------------------------------
## Fitting of the Log-normal distribution
FitAmax(an$flow, 'ln3', varcov = FALSE)

## ------------------------------------------------------------------------
## Candidates distribution
candidates <- c('gev','glo','ln3','pe3')

## Function that computes the AIC for a given distribution
FAIC <- function(d)
   AIC(FitAmax(an$flow, d, method = 'mle'))

## AIC of all distributions
sapply(candidates, FAIC)

## Automatic selection of the distribution
FitAmax(an$flow, candidates, method = 'mle')$distr

## ------------------------------------------------------------------------
## Add a trend
flow.sd <- sd(an$flow)
an$flow.add <- an$flow + seq(-1,1, len = nrow(an)) * flow.sd
an$flow.mult<- an$flow * seq(.7,1.3, len = nrow(an))

## Recenter years to 1970
an$yy <- as.integer(an$yy)


## ---- fig.height= 5, fig.width=7-----------------------------------------
plot(flow~yy, an, type = 'l', ylim = c(500,5500), col = 'black')
lines(flow.add ~ yy, an, col = 'red', lty = 2)
lines(flow.mult ~ yy, an, col = 'blue', lty = 2)
legend('topleft', horiz = TRUE, col = c(1:2,4), lty = c(1,2,2),
       legend = c('Original', 'Add', 'Mult'))

## ------------------------------------------------------------------------
fit.lin <- FitNsAmax(flow.add ~ yy, an, distr = 'gev', type = 'add')
print(fit.lin)

## ------------------------------------------------------------------------
## Other non linear trends
an$stp <- as.integer(an$yy>1965)
fit.stp <- FitNsAmax(flow.add ~ stp, an, distr = 'gev', type = 'add')

library(splines)
fit.ns <- FitNsAmax(flow.add ~ ns(yy, df = 7), an, distr = 'gev', type = 'add')

## ---- fig.height= 5, fig.width=8-----------------------------------------
an85 <- (an$yy>1985) 
prob <- c(0.9)
plot(flow.add~yy, an, type = 'l',
     xlab = 'Year',
     ylab = 'Flow')

Fgraph <- function(fit, col){
  hat <- predict(fit, prob, newdata = an)
  rhat <- predict(fit, prob, newdata = an[an85,], reliability = TRUE)

  lines(an$yy, fitted(fit), col = col, lty = 2)
  lines(an$yy, hat, col = col)
  lines(cbind(an[an85,'yy'], rhat), col = col, lwd=3)
}

Fgraph(fit.lin, 'red')
Fgraph(fit.stp, 'darkgreen')
Fgraph(fit.ns, 'blue')

legend('topleft', 
       legend = c('Trend', 'Quantile', 'Reliability'),
       col = c('darkgrey','darkgrey','darkgrey'), 
       lty = c(2,1,1), 
       lwd = c(1,1,3))

legend('top', horiz = TRUE, 
       legend = c( 'Linear', 'Step', 'Spline'),
       col = c( 'red', 'darkgreen','blue'), 
       lty = c(1,1), 
       lwd = c(1,1))


## ------------------------------------------------------------------------
hat <- ch_rfa_boot_nsamax(fit.lin, p = prob, reliability = TRUE, nsim = 500, verbose = FALSE)
summary(hat, variable = 'para')
summary(hat, variable = 'qua')

## ------------------------------------------------------------------------
fit.mult <- FitNsAmax(flow.mult ~ poly(yy, 3), an, distr = 'gno', type = 'mult')
fit.mle <- FitNsAmaxMle(flow.mult ~ poly(yy, 3), an, distr = 'gno', type = 'mult', 
                        control = list(maxit = 5000))

## ----fig.height= 5, fig.width=8------------------------------------------

plot(flow.add~yy, an, type = 'l',
     xlab = 'Year (ref = 1970)',
     ylab = 'Flow')

Fgraph(fit.mult, 'red')

## MLE model
hat <- predict(fit.mle, prob)
loc <- predict(fit.mle, type = 'location')
rhat <- predict(fit.mle, prob, newdata = an[an85,], type = 'reliability')

col <- 'blue'
lines(an$yy, loc, col = col, lty = 2)
lines(an$yy, hat, col = col)
lines(cbind(an[an85,'yy'], rhat), col = col, lwd=3)

legend('topleft', 
       legend = c('Location', 'Quantile', 'Reliability'),
       col = c('darkgrey','darkgrey','darkgrey'), 
       lty = c(2,1,1), 
       lwd = c(1,1,3))

legend('top', horiz = TRUE, 
       legend = c( 'Regression', 'Stochastic'),
       col = c( 'red', 'blue'), 
       lty = c(1,1), 
       lwd = c(1,1))


