###############################################################################
## Test function FitNsPot
## Martin Durocher <mduroche@@uwaterloo.ca>
##############################################################################
rm(list = ls())

fit0 <- FitNsPot(flow~date, x = flowStJohn, tau = .95, 
                trend = ~ 1, thresh = ~ 1, declust = 'wrc', r = 14)

fit <- FitNsPot(flow~date, x = flowStJohn, tau = .95, 
                trend = ~ date, thresh = ~ date, declust = 'wrc', r = 14)

dref <- as.Date(paste(1985:2014,7,15, sep = '/'))
xyy<- flowStJohn[flowStJohn$date %in% dref,]

expect_equivalent(length(coef(fit, 'kappa')), 2)
expect_equivalent(length(coef(fit, 'all')), 5)

## Verify the return component of the model.
ff <- fitted(fit, newdata = xyy)
expect_equal(colnames(ff), c('time','original', 'threshold', 'trend', 'scale'))
expect_equal(xyy$date, ff$time)
expect_equal(xyy$flow, ff$original)
expect_equal(ff$scale, (ff$original-ff$threshold)/ff$trend)

ff <- fitted(fit)
rr <- residuals(fit, 'thresh')

expect_equal(ff$original-ff$threshold, rr)

rr <- residuals(fit, 'scale')
expect_equal((ff$original-ff$threshold)/ff$trend, rr)

## verify that the GOF test works ##

gof <- GofTest(fit, method = 'adtab')
expect_true(gof$pvalue >= .5)

gof <- GofTest(fit, method = 'ad', nsim = 500)
expect_true(gof$pvalue > .5)

gof <- GofTest(fit, method = 'shapiro')
expect_true(gof$pvalue > .5)

##########################################################################
## Verify that predicting the return level works
##########################################################################
rt0 <- c(10,100)

## COnstant model
qua0 <- predict(fit0, rt0, xyy)
expect_equal(names(qua0), paste0('q',rt0))
expect_equal(class(qua0), 'numeric')
expect_equivalent(signif(qua0, 6), c(3387.78, 4118.20))

## time varying return level
qua <- predict(fit, c(10,100), xyy)
expect_equal(colnames(qua), paste0('q',rt0))
expect_equal(class(qua), 'matrix')
expect_equal(dim(qua), c(nrow(xyy),2))

gm.mean = function(x, na.rm=TRUE){
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}

qua.avg <- apply(qua, 2, gm.mean)
expect_true(max(abs(1-qua.avg/c(3508.58, 4284.70))) <0.01)

## design level
rel <- predict(fit, c(10,100), xyy, reliability = TRUE)
expect_equal(names(rel), paste0('q',rt0))
expect_equal(class(rel), 'numeric')

## verify that boot
out0 <- ch_rfa_BootNsPot(fit, rt = c(10,100), nsim = 5, x = flowStJohn, reliability = FALSE)

out0 <- ch_rfa_BootNsPot(fit, rt = c(10,100), x = flowStJohn, nsim = 5,
                  newdata = xyy, reliability = FALSE)

round(summary(out0), 3)

out0 <- ch_rfa_BootNsPot(fit, rt = c(10,100), x = flowStJohn, nsim = 5,
                  newdata = xyy, reliability = FALSE)

out <- ch_rfa_BootNsPot(fit, rt = c(10,100), x = flowStJohn,
                 newdata = xyy, reliability = TRUE)

summary(out, 'para')
summary(out, 'qua')

fit <- FitNsPot(flow~date, x = flowStJohn, tau = .95, trend.method = 'mle',
                trend = ~ date, thresh = ~ date, declust = 'wrc', r = 14)
out <- predict(fit, reliability = TRUE)

out <- ch_rfa_BootNsPot(fit, rt = c(10,100), x = flowStJohn,
                 newdata = xyy, reliability = TRUE)
summary(out, 'qua')
##############################################################################
fit <- FitNsPot(flow~date, x = flowStJohn, tau = .95, declust = 'wrc', r = 14)

xyy<- data.frame(flow =0, date = as.Date(paste(1985:2014, 7, 15, sep = '-')))

predict(fit, rt = c(10,100), newdata = xyy, reliability = TRUE)

form <- flow~date
x <- flowStJohn
trend <- ~ date
trend.link <- 'identity'
thresh <- ~ date
declust <- 'wrc'
r <- 14
rlow <- .75
nmin <- 20
verbose <- TRUE
unit = 365.25
sorted = FALSE
method = 'reg-mle'
tau <- .94
  

xx <- flowStJohn[-c(1:92),]

tau <- seq(.93, .98, .002)

us <- SearchThreshNs(flow~date, x = xx, tau = tau, trend = ~date, thresh = ~date, 
                   declust = 'wrc', r = 14, method = 'reg-lmom', trend.control = list(maxit = 2000),
                   trend.method = 'Nelder-Mead')

FindThresh(us, method = 'sgn-max')


plot(kappa~ tau,us)

xx <- flowStJohn[-c(1:92),]
ff <- FitNsPot(flow~date, x = xx, tau = 0.958, trend = ~date, thresh = ~date, 
               declust = 'wrc', r = 14, method = 'mle', trend.control = list(maxit = 200))
GofTest(ff)
summary(residuals(ff,'scale'))

ff2 <- FitNsPot(flow~date, x = xx, tau = 0.958, trend = ~date, thresh = ~date, 
               declust = 'wrc', r = 14, method = 'reg-mle', trend.control = list(maxit = 200))
GofTest(ff2)
summary(residuals(ff2,'scale'))





