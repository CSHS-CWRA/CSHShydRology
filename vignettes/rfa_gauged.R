## ------------------------------------------------------------------------
library(CSHShydRology)
attach(flowAtlantic) 

## Compute the Euclidean distance between descriptors
covar <- scale(log(info[,c('area','map')]))
distance.covar <- as.matrix(dist(covar))
colnames(distance.covar) <- rownames(distance.covar) <- as.character(info$id)

## Compute great-circle distance
coord <- info[,c('lon','lat')]
distance.geo <- GeoDist(coord)
colnames(distance.geo) <- rownames(distance.geo) <- as.character(info$id)

## ------------------------------------------------------------------------
SeasonStat(ams[ams$id == ams$id[1], ]$date)
st <- SeasonStat(date ~ id, ams)
head(st)


## ------------------------------------------------------------------------
## Distance using cartesian coordinates
distance.st <- as.matrix(dist(st[,c('x','y')]))
colnames(distance.geo) <- rownames(distance.geo) <- as.character(info$id)

## Distance using polar coordinates
distance.st2 <- DistSeason(radius ~ angle , st)
colnames(distance.st2) <- rownames(distance.st2) <- as.character(info$id)

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

## Fit the exponential model
geo.target <- distance.geo[sid,sid]

icor2 <- Intersite(xd[,sid], method = 'exp',
                   distance = geo.target, 
                   distance.max = 300)

print(icor2)

## Display the results
theta <- icor2$corr[lower.tri(icor2$corr)]
theta.model <- icor2$model[lower.tri(icor2$corr)]
h <- geo.target[lower.tri(icor2$corr)]

plot(h, theta, pch = '.', cex = 2)
points(h, theta.model, col = 'red', pch = 16)


## ------------------------------------------------------------------------
predict(fit.target2, q = c(.9, .99), ci = TRUE, corr = icor$model)

predict(fit.target2, q = c(.9, .99), ci = TRUE, corr = icor$para)

