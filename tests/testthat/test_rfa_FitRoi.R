###############################################################################
## Test function FitRoi
## Martin Durocher <mduroche@uwaterloo.ca>
##############################################################################

rm(list = ls())
attach(flowUngauged)
 
## Using multidimensional scaling for projecting coordinates
coord <- cbind(lon,lat)
coord <- cmdscale(GeoDist(coord))
colnames(coord) <- c('lon','lat')

## Transform data if necessary
xdf <- data.frame(y      = l1,
                  ly     = log(l1),
                  area   = scale(log(area)),
                  wb     = scale(log(wb)),
                  stream = scale(log(stream)),
                  map    = scale(log(map)),
                  coord)


## select a validation and training set
set.seed(9382)
vid <- runif(nrow(xdf)) > .8
tid <- !vid

response <- xdf[vid,'ly'] 

# formula of the relationship between flood quantile and descriptors
fphy0 <- ly ~ area + map + wb + stream
fphy1 <- log(y) ~ area + map + poly(wb,2) + poly(stream,2)
fphy2 <- log(y) ~ area + map + wb + stream

fsimilarity <- ~ area + map
fkriging <- ~ lon + lat

## Fit a local regression model
fit0 <- FitRoi(x = xdf[tid,], xnew = xdf[vid,], nk = 60,
             phy = fphy0, similarity = fsimilarity)

sd(response - fit0$pred)
plot(response, fit0$pred);abline(0,1)

fit1 <- FitRoi(x = xdf[tid,], xnew = xdf[vid,], nk = 60,
             phy = fphy1, similarity = fsimilarity)

sd(response - fit1$pred)
plot(response, fit1$pred);abline(0,1)

fit2 <- FitRoi(x = xdf[tid,], xnew = xdf[vid,], nk = 60,
             phy = fphy2, similarity = fsimilarity)

expect_equal(fit0$pred, fit2$pred)
expect_true(cor(fit2$pred, fit1$pred) > .99)
expect_true(sd(response - fit1$pred) < .5)

expect_equal(class(fit1),'roi')
expect_true(all(names(fit1) == c('call','pred')))
expect_true(all(names(fit1$call) == 
                  c('nk','npred','nsite','ker','kriging','phy','similarity')))

expect_equal(length(fit1$pred), sum(vid))

##Verify the se option
fit1 <- FitRoi(x = xdf[tid,], xnew = xdf[vid,], nk = 60,
             phy = fphy1, similarity = fsimilarity, se = TRUE)

expect_equal(length(fit1$pred), length(fit1$pred.se))
expect_true(all(names(fit1) == c('call','pred','pred.se')))

print(fit1)
##--------------------------
## Using kriging
##---------------------------

## Refit the model and perform the kriging of the residuals
fitk <- FitRoi(x = xdf[tid,], 
               xnew = xdf[vid, ], 
               nk = 60,
               phy = fphy1, 
               similarity = fsimilarity ,
               kriging = fkriging,
               model = 'Exp')
               
lname <- c("call", "phy", "fitted", "resid", "vgm", "model", "pred", 'krige') 
expect_true(all(names(fitk) == lname))

expect_true(max(abs(fitk$phy + fitk$krige - fitk$pred)) <1e-8 )
expect_true(max(abs(fitk$fitted + fitk$resid - xdf$ly[tid])) <1e-8)

expect_true(sd(response - fitk$pred) < .4)

expect_equal(as.character(fitk$model[2,1]),'Exp')

## Test with another kriging model ans SE
fitk <- FitRoi(x = xdf[tid,], 
               xnew = xdf[vid, ], 
               nk = 60,
               phy = fphy1, 
               similarity = fsimilarity ,
               kriging = fkriging,
               model = 'Sph', se = TRUE)

expect_equal(as.character(fitk$model[2,1]),'Sph')

print(fitk)

expect_equal(length(fitk$phy),length(fitk$phy.se))
expect_equal(length(fitk$fitted),length(fitk$fitted.se))
expect_equal(length(fitk$krige),length(fitk$krige.se))

## Pass a kriging model directly
fitk2 <- FitRoi(x = xdf[tid,], 
               xnew = xdf[vid, ], 
               nk = 60,
               phy = fphy1, 
               similarity = fsimilarity ,
               kriging = fkriging,
               model = fitk$model)

expect_equal(fitk2$model,fitk$model)
expect_equal(fitk2$pred,fitk$pred)

##------------------------------
## testing predict and residual
##------------------------------

out <- predict(fit1, xdf[tid,]) 
expect_equal(length(out), sum(tid))

out <- predict(fit1, xdf) 
expect_equal(length(out), nrow(xdf))

##Catch error with Not enough site
expect_error(predict(fit1, xdf[1:60,]))

## The same output wheen fold is passed

out1 <- predict(fit1, xdf) 
expect_true(any(out1 != out))

kf <- sample(rep_len(1:5, nrow(xdf)))
  
out1 <- predict(fit1, xdf, fold = kf) 
out2 <- predict(fit1, xdf, fold = kf) 
expect_equal(out1, out2)

## check residuals
res <- residuals(fit1, xdf, fold = kf)
res2 <- out1 - log(xdf$y)
expect_equal(res, res2)

## with kriging
out <- predict(fitk, xdf) 
expect_equal(length(out), nrow(xdf))


detach(flowUngauged)
