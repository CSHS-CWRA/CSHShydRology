###############################################################################
## Test function CvRoi
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
fphy <- ly ~ area + map + wb + stream
fsimilarity <- ~ area + map
fkriging <- ~ lon + lat

## Perform cross-validation.
out <- CvRoi(x = xdf, nk = seq(20,150, 10), fold = 5,
                phy = fphy,  similarity = fsimilarity, model = 'Exp')

## verify output scale
expect_true(all(out$mad < 0.5))
expect_true(all(out$nsh > 0.7))

expect_equal(class(out), c('roicv', 'data.frame'))
expect_equal(dim(out), c(14,7))
print(out)

## verify that it is correctly sorted
z <- head(out, 'nsh')[,'nsh']
expect_equal(z, sort(z,decreasing = TRUE))

z <- head(out, 'mad')[,'mad']
expect_equal(z, sort(z))

expect_equal(out$nk, seq(20,150, 10))
plot(out, 'rmse', best.cex = .5, best.col = 'green', best.pch = 1, lty =3)
plot(out, 'smad', ylab = '', xlab = '')

## -----------------------------
## modify formula
## -----------------------------

fphy <- ly ~ area + map + poly(wb,3) + poly(stream,3)
out <- CvRoi(x = xdf, nk = seq(40,60, 10), fold = 10,
                phy = fphy,  similarity = fsimilarity, model = 'Exp')

expect_true(all(out$mad < 0.5))
expect_true(all(out$nsh > 0.7))

fphy <- log(y) ~ area + map + wb + wb
out <- CvRoi(x = xdf, nk = seq(40,60, 10), fold = 10,
                phy = fphy,  similarity = fsimilarity, model = 'Exp')

expect_true(all(out$mad < 0.5))
expect_true(all(out$nsh > 0.7))


detach(flowUngauged)
