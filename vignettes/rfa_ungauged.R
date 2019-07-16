## ------------------------------------------------------------------------
library(CSHShydRology)
data(flowUngauged)

## Transform data if necessary.
xd0 <- with(flowUngauged, 
  data.frame(area   = scale(log(area)),
             wb     = scale(log(wb)),
             stream = scale(log(stream)),
             map    = scale(log(map)))
  )

## ------------------------------------------------------------------------
## 
coord <- cmdscale(GeoDist(~lon+lat,flowUngauged))
colnames(coord) <- c('lon','lat')

xd0 <- cbind(xd0, coord)

