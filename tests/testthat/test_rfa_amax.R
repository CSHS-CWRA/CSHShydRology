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

AIC0 <- -2*sum(dAmax(f0$data, f0$para, f0$distr, log = TRUE)) + 6
expect_equal(AIC0, AIC(f0))

f0 <- FitAmax(q, 'gum', method = 'lmom', varcov = FALSE)
AIC0 <- -2*sum(dAmax(f0$data, f0$para, f0$distr, log = TRUE)) + 4
expect_equal(AIC0, AIC(f0))

