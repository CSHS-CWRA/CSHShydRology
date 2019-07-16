## ------------------------------------------------------------------------
library(CSHShydRology)

## Extract the annual maximums
anData <- ExtractAmax(flow ~ date, flowStJohn, tol = 365)

## ------------------------------------------------------------------------
fitMle <- FitAmax(anData$flow, 'gev', method = 'mle')
print(fitMle)

## ------------------------------------------------------------------------
## Fit GEV distribution using L-moments
fitLmm <-FitAmax(anData$flow, 'gev', method = 'lmom', varcov = FALSE)

## ------------------------------------------------------------------------
## Flood quantiles of 10 and 100 return periods
predict(fitMle, q = c(.9,.99), se = TRUE, ci = 'delta', alpha = .1)

## ------------------------------------------------------------------------
out <- predict(fitLmm, q = c(.9,.99), ci = 'boot', 
               nsim = 100, out.matrix = TRUE)

## Estimated Flood quantiles
out$pred

## Bootstraps sample of model parameters
head(out$para)

## Bootstraps sample of flood quantiles
head(out$qua)

## ---- fig.height= 4,fig.width=6------------------------------------------
## Return level plot
plot(fitMle, ci = TRUE)

## ------------------------------------------------------------------------
## Anderson-Darling goodness of fit test
GofTest(fitLmm, nsim = 500)

## ------------------------------------------------------------------------
## 
FitAmax(anData$flow, 'ln3', method = 'lmom', varcov =  FALSE)

## ------------------------------------------------------------------------
## Candidates distribution
candidates <- c('gev','glo','ln3','pe3')

## Function that compute the AIC for a given distribution
FAIC <- function(d)
   AIC(FitAmax(anData$flow, d, method = 'mle', varcov =  FALSE))

## AIC of all distribution
sapply(candidates, FAIC)

## Automatic selection of the distribution
FitAmax(anData$flow, candidates, method = 'mle', tol.gev = 2)

