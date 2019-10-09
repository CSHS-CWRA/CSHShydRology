
########################
amax <- flowAtlantic$ams
amax$year <- format(amax$date, '%Y')
xx <- split(amax, amax$id) 

FF <- function(z){
 z$z <- qnorm(rank(z$ams)/ (nrow(z)+1)) 
 return(z)
}

xx <- lapply(xx, FF)
amax <- do.call(rbind, xx)

fit1 <- lm.ridge( z ~ year -1, amax, lambda = 21)
          
xd <- xx[[42]]
xd <- cbind(xd, indice = fit1$coef[as.character(1916:2015) %in% xd$year])
       
fit <- FitNsAmax(ams ~ indice, xd, distr = 'gev', type = 'mult')

xd$yhat <- fitted(fit)
plot(ams ~ year, xd,type = 'l')
lines(yhat ~ year, xd, col = 4)

xd0 <- xd[xd$year >1980,]

rhat <- predict(fit, p = c(.5,.99), newdata = xd0)
for(ii in 1:2) lines(xd0$year,rhat[,ii], col = 'cyan')

rhat <- predict(fit, p = c(.5,.99), newdata = xd0, reliability = TRUE)
abline(h = rhat, col = 'magenta')

fit <- FitNsAmax(ams ~ 1, xd, distr = 'gev', type = 'add')
rhat <- predict(fit, newdata = xd0, reliability = TRUE)
abline(h = rhat, col = 'magenta')

#####################################################################
fit <- FitNsAmax.mle(ams ~ indice, xd, distr = 'gev', type = 'mult')

xd$yhat <- fitted(fit)
plot(ams ~ year, xd,type = 'l')
lines(yhat ~ year, xd, col = 2)

predict(fit, type = 'reliability')
