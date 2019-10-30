## ------------------------------------------------------------------------
library(CSHShydRology)
data(flowAtlantic) 
ams <- flowAtlantic$ams
info <- flowAtlantic$info

## ------------------------------------------------------------------------
## Evaluate seasonal statistics
s <- ams[ams$id == ams$id[1], ]$date
SeasonStat(s)

st <- SeasonStat(date ~ id, ams)
head(st)

## ------------------------------------------------------------------------
## Site names
sname <- as.character(info$id)

## Distance using cartesian coordinates
distance.st <- as.matrix(dist(st[,c('x','y')]))
colnames(distance.st) <- rownames(distance.st) <- sname

## Distance using polar coordinates
distance.st2 <- DistSeason(radius ~ angle , st)
colnames(distance.st2) <- rownames(distance.st2) <- sname


## ---- fig.height=5, fig.width=5------------------------------------------
set.seed(0)
km <- kmeans(as.dist(distance.st2), 3, nstart = 5)
regime <- c('blue','darkgreen','orange')[km$cluster]

JulianPlot()
points(st, pch = 16, col = regime)

## ------------------------------------------------------------------------

## Organisation of the data in a matrix
ams$year <- format(ams$date, '%Y')
xd <- DataWide(ams ~ id + year, ams)

## Extract a pooling groups
distance.target <- distance.st2[colnames(xd),'01AK007']
xd.target <- FindNearest(xd, distance = distance.target, 25)

## ------------------------------------------------------------------------
## Compute the Euclidean distance
phy <- scale(log(info[,c('area','map')]))

## Make sure that the order of the sites match with the wide dataset
rownames(phy) <- info$id
phy <- phy[colnames(xd),]

## Split the sites using clustering techniques
phy.cluster <- hclust(dist(phy),method = 'ward.D')
super <- cutree(phy.cluster, 2)

## Find the pooling group for '01AK007' inside the super region
xd.super <- FindNearest(xd[,super], distance = distance.target[super], 15)

## ------------------------------------------------------------------------
## Euclidean distance with target
distance.phy <- as.matrix(dist(phy))

## Extract a pooling group with super regions
xd.super <- FindNearest(xd, distance = distance.target, n = 15,
                        super.distance = distance.phy[,'01AK007'],
                        super.n = 30)


## ------------------------------------------------------------------------
## Fit regional growth curve
fit.target <- FitRegLmom(xd.target)
print(fit.target)

## ----fig.height=5, fig.width=6-------------------------------------------
plot(fit.target)

## ------------------------------------------------------------------------
## Remove heterogenous sites 
fit.target2 <- PoolRemove(fit.target)

## New heterogeneity measure
fit.target2$stat[1]

## ------------------------------------------------------------------------
sid <- sitenames(fit.target2)
icor <- Intersite(xd[,sid])
round(icor$model[1:3,1:3],2)

## ----fig.height=5, fig.width=6-------------------------------------------

## Compute the distance
distance.geo <- GeoDist(~lon+lat, info)
colnames(distance.geo) <- rownames(distance.geo) <- sname
geo.target <- distance.geo[sid,sid]


## Fit the power exponential model 
Finter <- function(sm){
  Intersite(xd[,sid], type = 'exp',
                   distance = geo.target, 
                   distance.max = 500,
                   distance.bin = 15, smooth = sm)  
}

print(icor2 <- Finter(1))


## Plot correlation cloud
tri <-lower.tri(icor2$corr)
theta <- icor2$corr[tri]
h <- geo.target[tri]

plot(h, theta, col = 'grey', pch = '+',
     xlab = 'distance', ylab = 'Spatial correlation')

points(icor2$bin, pch = 16, col = 'black', cex = 1.5)

## Plot POW with p = 1
hid <- order(h)
theta <- icor2$model[tri]
lines(h[hid], theta[hid], col = 'red', lwd = 2)

## Plot POW with p = 2
theta <- Finter(2)$model[tri]
lines(h[hid], theta[hid], col = 'blue', lwd = 2)



## ------------------------------------------------------------------------
## Using exponential model
hat <- predict(fit.target2, q = c(.9, .99), ci = TRUE, corr = icor2$model)
print(round(hat,1))

## Using constant coefficient of correlation
hat <- predict(fit.target2, q = c(.9, .99), ci = TRUE, corr = icor$para)
print(round(hat,1))

## ------------------------------------------------------------------------
xw <- xd[,sitenames(fit.target2)]
fit <- FitPoolMle(xw, distr = 'gev', type = 'mean')
print(fit)

cat('\nFlood quantiles for station 01AK007\n')
predict(fit, index = fit$index[1])

## ------------------------------------------------------------------------
print(fit.shp <- FitPoolMle(xw, distr = 'gev', type = 'shape'))
print(fit.cv <- FitPoolMle(xw, distr = 'gev', type = 'cv'))

## ------------------------------------------------------------------------
## Create bootstrap
fit.margin <- FitPoolMargin(xw, 'gev', method = 'mle', 
                            method.optim = 'Nelder-Mead', 
                            control = list(maxit = 5000))

boot <- simulate(fit.margin, nsim = 20, corr = 0.3)


## ------------------------------------------------------------------------
## Fit the regional models on the samples
ffun <- function(z, type) FitPoolMle(z, distr = 'gev', type = type)
fboot.mean <- lapply(boot, ffun, type = 'mean')
fboot.cv <- lapply(boot, ffun, type = 'cv')
fboot.shp <- lapply(boot, ffun, type = 'shape')


## Boostrap samples of the flood quantiles
qboot.mean <- sapply(fboot.mean, predict, p = .9)
qboot.cv <- sapply(fboot.cv, predict, p = .9)
qboot.shp <- sapply(fboot.shp, predict, p = .9)

## Evaluate the standard deviation
mean.sd <- apply(qboot.mean, 1, sd)
cv.sd <- apply(qboot.cv, 1, sd)
shp.sd <- apply(qboot.shp, 1, sd)

## Extract at-site estimate
hat <- predict(fit.margin, .9)

print(
  cbind(
    Index.flood = mean(mean.sd/hat),
    Lik.cv = mean(cv.sd/hat), 
    Lik.shp = mean(shp.sd/hat)),
  digits = 3) 


## ------------------------------------------------------------------------
set.seed(1)

## Station
stations <- colnames(xd.target)

## Number of peaks
npeak <- apply(xd.target, 2, function(z) rpois(1,sum(is.finite(z))))

## GEV parameter
para <- FitPoolMargin(xd.target, 'gev')$para

## Loose correspondance
u <- apply(xd.target, 2, quantile, 0.1, na.rm = TRUE)
alpha <- para[2,] + para[3,] * (u - para[1,])
kap <- para[3,]


## Create the random sample in the long format
Fz <- function(m,n,a,k){
  x <- rgpa(n,a,k)
  data.frame(station = m, year = seq_along(x), value = x)
}

xs <- mapply(Fz, stations, npeak, alpha, kap, SIMPLIFY = FALSE)
xs <- do.call(rbind, xs)
rownames(xs) <- NULL


## ------------------------------------------------------------------------
## Transform to wide format
xs.target <- DataWide(value ~ station + year, xs, order.time = FALSE)

## Fit regional growth curve using 3 parameter
fit.target <- FitRegLmom(xs.target, distr = 'gpa')
print(fit.target)

## Fit regional growth curve using 2 parameter
fit.target <- FitRegLmom(xs.target, type = 'pot')
print(fit.target)

# Create a homogenous group
fit.target <- PoolRemove(fit.target)

## ------------------------------------------------------------------------

## Average number of exceedances at target
lambda <- 2.2

## return periods of interest
period <- c(5, 50)

## Associated probabilities
prob <- 1 - 1/(lambda * period)

## Estimate flood quantiles for POT
hat <- predict(fit.target, prob, ci = TRUE)
print(round(hat,1))

## ------------------------------------------------------------------------
## Fitting
xs.lik <- xs.target[,sitenames(fit.target)]
fit.lik <- FitPoolMle(xs.lik, distr = 'gpa', type = 'mean')
hat <- predict(fit.lik, p = prob, index = fit.lik$index[1])

## Bootstrap
fit.margin <- FitPoolMargin(xw, 'gpa', method = 'mle')
boot <- simulate(fit.margin, nsim = 50, corr = 0)
Fz <- function(z) {
  f <- FitPoolMle(z, distr = 'gpa', type = 'mean')
  predict(f, p = prob, index = f$index[1])
}

pboot <- sapply(boot, Fz)

## Uncertainty
se <- apply(pboot, 1, sd)
round(cbind(pred = hat, se = se), 3)


