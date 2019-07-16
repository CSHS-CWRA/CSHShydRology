######################################################
## testing FitPot and predict.fpot functions
## Martin Durocher <mduroche@uwaterloo.ca>
#######################################################

rm(list = ls())

uu <- (1:10000)/10001
xd <- qgpa(uu, 1, -.2)
xd0 <- qgpa(uu, 1, 0)

## Simple fit
fit <- FitPot(xd, u = 0)

## verify the output
expect_equal(fit$excess, xd)
expect_equal(fit$time,1:10000)
expect_equal(signif(fit$estimate), 
             c(alpha=1.001330, kappa =-0.198129))
expect_equal(fit$u,0)
expect_equal(fit$method, 'mle')

vm <- matrix(c(0.000240263, 0.000119972,
               0.000119972, 0.000143551),
             2,2)
colnames(vm) <- rownames(vm) <- c('alpha','kappa')
expect_equal(signif(fit$varcov),vm) 
expect_equal(signif(fit$mrl), 1.24874)
expect_equal(fit$ntot, 10000)
expect_equal(fit$nexcess, length(fit$excess))

## verify basic functions
print(fit)
expect_equal(vcov(fit), fit$varcov) 
expect_equal(coef(fit), fit$estimate)             
expect_equal(signif(AIC(fit)),23993.1)

## Check MLE2 for bounded estimation
expect_warning(fit <- FitPot(c(xd0, 30:40), u = 5, method = 'mle2'))
expect_equal(coef(fit)[2], c(kappa = -.5))
expect_equal(fit$nexcess, length(fit$excess))

## Verify that vcov is not returned
fit <- FitPot(xd0, u = 5, method = 'mle2', varcov = F)
expect_true(is.null(fit$varcov))
expect_true(is.null(vcov(fit)))

## try Lmoment estimator
fit <- FitPot(xd0, u = 5, method = 'lmom', varcov = F)
fit <- FitPot(xd0, u = 5, method = 'lmom', varcov = T, nsim = 5)


## Verif confidence interval for parameter by profile likelihood
cc <- coef(fit, ci = TRUE)
expect_equal(colnames(cc), c('estimate','lower','upper'))
expect_equal(rownames(cc), c('alpha','kappa'))

vm <- matrix(c(0.9759520,  0.715246, 1.477390,
               0.0159441, -0.251472, 0.297897),
             2,3, byrow = TRUE)

expect_true(all(signif(cc)==vm))

## verify unit
expect_equal(fit$unit, 365.25)

fit <- FitPot(xd0, u = 5, method = 'lmom', varcov = F, unit = 100)
expect_equal(fit$nyear, 100)


## verify the declustering

fit <- FitPot(flow~date, flowStJohn, u = 1000,
              declust = 'wrc', r = 10, rlow = .6)
xid <- which.floodPeaks(flow~date, flowStJohn, u = 1000, r = 10, rlow = .6)
expect_equal(flowStJohn$flow[xid], fit$excess + fit$u)

fit <- FitPot(flow~date, flowStJohn, u = 900, declust = 'run', r = 10)
xid <- which.clusters(flow~date, flowStJohn, u = 900, r = 10)
expect_equal(flowStJohn$flow[xid], fit$excess + fit$u)

## verify formula
fit1 <- FitPot(flow~date, flowStJohn, u = 1000,
              declust = 'wrc', r = 10, rlow = .6)

fit2 <- FitPot(flowStJohn$flow, flowStJohn$date, u = 1000,
              declust = 'wrc', r = 10, rlow = .6)

expect_equal(fit1,fit2)
