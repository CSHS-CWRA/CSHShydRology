######################################################
## testing Amax functions
## Martin Durocher <mduroche@uwaterloo.ca>
#######################################################

rm(list = ls())

uu <- (1:100 - .5) /100

x <- rAmax(10, c(0,1,0), 'gno')
expect_equal(length(x), 10)

x <- rAmax(10, c(0,1,0), 'gev')
expect_equal(length(x), 10)

q <- qAmax(uu, c(100,30,-.5), 'gev')
p <- pAmax(q, c(100,30,-.5), 'gev')
expect_equal(uu,p)

q <- qAmax(uu, c(100,30,-.5), 'gno')
p <- pAmax(q, c(100,30,-.5), 'gno')
expect_equal(uu,p)

f0 <- FitAmax(q, 'gev', method = 'mle', varcov = FALSE)
AIC0 <- -2*sum(dAmax(f0$data, f0$para, f0$distr, log = TRUE)) + 6
expect_equal(AIC0, AIC(f0))

f0 <- FitAmax(q, 'gum', method = 'lmom', varcov = FALSE)
AIC0 <- -2*sum(dAmax(f0$data, f0$para, f0$distr, log = TRUE)) + 4
expect_equal(AIC0, AIC(f0))

###########################################################
library(lmomco)

## verif GEV ##
u0 <- seq(0,1, l = 101)
xi0 <- seq(95,105, l = 101)
alf0 <- seq(25,35, l = 101)

par0 <- vec2par(c(100,30,-.1),'gev')
x0 <- rlmomco(101, par0)
expect_equivalent(dgev(x0, 100, 30, -.1), dlmomco(x0, par0))
expect_equivalent(pgev(x0, 100, 30, -.1), plmomco(x0, par0))
expect_equivalent(qgev(u0, 100, 30, -.1), qlmomco(u0, par0))

dgev(x0, xi0, 30, -.1)
dgev(x0, xi0, alf0, -.1, log = TRUE)

pgev(x0, xi0, 30, -.1)
pgev(x0, xi0, alf0, -.1)

qgev(u0, xi0, 30, -.1)
qgev(u0, xi0, alf0, -.1)


## verif GLO ##

par0 <- vec2par(c(100,30,-.1),'glo')
x0 <- rlmomco(101, par0)
expect_equivalent(dglo(x0, 100, 30, -.1), dlmomco(x0, par0))
expect_equivalent(pglo(x0, 100, 30, -.1), plmomco(x0, par0))
expect_equivalent(qglo(u0, 100, 30, -.1), qlmomco(u0, par0))

dglo(x0, xi0, 30, -.1)
dglo(x0, xi0, alf0, -.1, log = TRUE)

pglo(x0, xi0, 30, -.1)
pglo(x0, xi0, alf0, -.1)

qglo(u0, xi0, 30, -.1)
qglo(u0, xi0, alf0, -.1)

## verif PE3 ##

par0 <- vec2par(c(100,30,-.1),'pe3')
x0 <- rlmomco(101, par0)
expect_equivalent(dpe3(x0, 100, 30, -.1), dlmomco(x0, par0))
expect_equivalent(ppe3(x0, 100, 30, -.1), plmomco(x0, par0))
expect_equivalent(qpe3(u0, 100, 30, -.1), qlmomco(u0, par0))

dpe3(x0, xi0, 30, -.1)
dpe3(x0, xi0, alf0, -.1, log = TRUE)

ppe3(x0, xi0, 30, -.1)
ppe3(x0, xi0, alf0, -.1)

qpe3(u0, xi0, 30, -.1)
qpe3(u0, xi0, alf0, -.1)

## verif GNO ##

par0 <- vec2par(c(100,30,-.1),'gno')
x0 <- rlmomco(101, par0)
expect_equivalent(dgno(x0, 100, 30, -.1), dlmomco(x0, par0))
expect_equivalent(pgno(x0, 100, 30, -.1), plmomco(x0, par0))
expect_equivalent(qgno(u0, 100, 30, -.1), qlmomco(u0, par0))

dgno(x0, xi0, 30, -.1)
dgno(x0, xi0, alf0, -.1, log = TRUE)

pgno(x0, xi0, 30, -.1)
pgno(x0, xi0, alf0, -.1)

qgno(u0, xi0, 30, -.1)
qgno(u0, xi0, alf0, -.1)

######################
## Verify the fitting f-functions
u0 <- (1:1000)/1001

f0 <- c(100,30, -.05)
f <- fgev(qgev(u0, 100, 30, -.05))
expect_true(all(abs(f-f0) < c(.05, .2, .01)))

f <- fgno(qgno(u0, 100, 30, -.05))
expect_true(all(abs(f-f0) < c(.05, .2, .01)))

f <- fglo(qglo(u0, 100, 30, -.05))
expect_true(all(abs(f-f0) < c(.05, .2, .01)))

f <- fpe3(qpe3(u0, 100, 30, -.05))
expect_true(all(abs(f-f0) < c(.05, .2, .01)))

