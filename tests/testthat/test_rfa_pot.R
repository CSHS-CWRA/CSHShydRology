#############################################################################
## Pad POT
#############################################################################

dt <- seq(as.Date('2000/02/02'), as.Date('2010/12/20'), 'days')
xd <- data.frame(dt = dt, x = seq_along(dt))

## pad the begining and the end
xp <- PadPOT(x~dt, xd)

dref <- as.Date('2005/05/05')
v1 <- xp[xp[,1] == dref,2]
v2 <- xd[xd[,1] == dref,2]

expect_equal(colnames(xp),colnames(xd))

expect_equal(xp[1,2],0)
expect_equal(xp[nrow(xp),2],0)
expect_equal(v1,v2)
expect_equal(unique(diff(xp[,1])),1)

## Affect the right value
xp <- PadPOT(x~dt, xd, value = 5)

expect_equal(xp[1,2],5)
expect_equal(xp[nrow(xp),2],5)


## Verify inner missing

xd <- xd[!(xd[,1] == dref),]

xp <- PadPOT(x~dt, xd, value = 5)
v1 <- xp[xp[,1] == dref,2]

expect_equal(xp[1,2],5)
expect_equal(xp[nrow(xp),2],5)
expect_equal(v1,5)

## Verify the sorting

xd <- xd[sample.int(nrow(xd)),]
xp <- PadPOT(x~ dt, xd, value = 5)
v1 <- xp[xp[,1] == dref,2]

expect_equal(xp[1,2],5)
expect_equal(xp[nrow(xp),2],5)
expect_equal(v1,5)
expect_equal(unique(diff(xp[,1])),1)

#############################################################################
## Basic GPA fiting
#############################################################################


## Correspondance between method
xd <- qgpa(1:100/101, 1, -.1)
expect_equivalent(FitPot(xd, u = 0, method = 'mle')$estimate,
                  fgpa1d(xd))
expect_equivalent(FitPot(xd, u = 0, method = 'mle2')$estimate,
                  fgpa2d(xd))
expect_equivalent(FitPot(xd, u = 0, method = 'lmom')$estimate,
                  fgpaLmom(xd))

expect_equivalent(FitPot(xd, u = 0, method = 'mle')$varcov,
                  fgpa1d(xd, sol = TRUE)$varcov)

expect_equivalent(FitPot(xd, u = 0, method = 'mle2')$varcov,
                  fgpa2d(xd, sol = TRUE)$varcov)

fit <- FitPot(xd, u = 0, method = 'mle')

expect_equal(coef(fit),fit$estimate)
expect_equal(vcov(fit),fit$varcov)

#############################################################################
## declustering
#############################################################################

x <- flowStJohn$flow

## Verify the local optimum function
expect_equal(id1 <- which.lmax(x[1:100]),
             c(1, 5, 9, 13, 21, 27, 43, 52, 59, 61, 70, 76, 84, 86))

expect_equal(id2 <- which.lmin(x[1:100]),
             c(4, 12, 18, 23, 33, 38, 40, 47, 58, 60, 66, 69, 75, 83, 90, 100))

expect_equal(x[id1], lmax(x[1:100]))
expect_equal(x[id2], lmin(x[1:100]))

x0 <- x
x0[599] <- NA ##988

expect_equal(which.lmax(x0[1:1000]), which.lmax(x[1:1000]))

## validate with evd::clusters
expect_equal(which.clusters(x[1:1000], u = 1000, r = 5),
       c(206, 402, 415, 562, 586, 949))

## different length x and dt

expect_error(which.clusters(x[1:1000], 1:999, u = 1000, r = 5))

## declustering of the FitPot correspond to function "which"
id_wrc <- which.floodPeaks(x, u = 900, r = 14, ini = 'wrc')
fit <- FitPot(x, u = 900, r = 14, declust = 'wrc')
expect_equal(fit$time, id_wrc)

id_run <- which.clusters(x, u = 900, r = 14)
fit <- FitPot(x, u = 900, r = 14, declust = 'run')
expect_equal(fit$time, id_run)

##############################################################################
## Find Threshold function
##############################################################################

lstu <- seq(600,1500,20)

obj <- SearchTresh(flow~date, flowStJohn, u = lstu, declust = 'wrc', r = 14)


