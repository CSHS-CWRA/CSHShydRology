## ------------------------------------------------------------------------
## Extract annual maximum
xd <- ExtractAmax(flow~date, flowStJohn, ylab = 'year', tol = 365)

## Add a trend
flow.sd <- sd(xd$flow)
xd$nsflow <- xd$flow + seq(-1,1, len = nrow(xd)) * flow.sd

## Center years to 1970
xd$yy <- as.integer(xd$year) - 1970


## ---- fig.height= 5, fig.width=7-----------------------------------------
plot(flow~year, xd, type = 'l')
lines(nsflow~year, xd, col = 'red')
legend('topleft', horiz = TRUE, col = 1:2, lty = rep(1,2), bty = 'n',
       legend = c('Original', 'With trend'))

## ----fig.height= 4, fig.width=8------------------------------------------
## Mann-Kendall
layout(matrix(c(1,2), 1,2))
acf(xd$nsflow)
pacf(xd$nsflow)

## ------------------------------------------------------------------------


