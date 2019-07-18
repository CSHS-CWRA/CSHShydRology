## ------------------------------------------------------------------------
library(CSHShydRology)
data("flowStJohn")

## ------------------------------------------------------------------------
## Extract the annual maximums
anData <- ExtractAmax(flow ~ date, flowStJohn, tol = 365)
nrow(anData)

## ------------------------------------------------------------------------
fitMle <- FitAmax(anData$flow, 'gev', method = 'mle')
print(fitMle)

## ------------------------------------------------------------------------
## Flood quantiles of 10 and 100 return periods
predict(fitMle, q = c(.9,.99), se = TRUE, ci = 'delta', alpha = .1)

## ------------------------------------------------------------------------
## Fitting using L-moments
fitLmm <- FitAmax(anData$flow, 'gev', method = 'lmom', varcov = FALSE)

## Prediction using bootstrap
out <- predict(fitLmm, q = c(.9,.99), ci = 'boot', 
               nsim = 1000, out.matrix = TRUE)

## Structure of the output
names(out)

## ---- fig.height= 4,fig.width=6------------------------------------------
## Return level plot
plot(fitMle, ci = TRUE)

## ------------------------------------------------------------------------
## Anderson-Darling test of goodness of fit
GofTest(fitLmm, nsim = 1000)

## ------------------------------------------------------------------------
## Fitting of the Log-normal distribution
FitAmax(anData$flow, 'ln3', varcov = FALSE)

## ------------------------------------------------------------------------
## Candidates distribution
candidates <- c('gev','glo','ln3','pe3')

## Function that computes the AIC for a given distribution
FAIC <- function(d)
   AIC(FitAmax(anData$flow, d, method = 'mle'))

## AIC of all distributions
sapply(candidates, FAIC)

## Automatic selection of the distribution
FitAmax(anData$flow, candidates, method = 'mle')$distr

