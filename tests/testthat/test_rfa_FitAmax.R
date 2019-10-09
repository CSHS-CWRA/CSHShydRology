######################################################
## testing FitAmax
## Martin Durocher <mduroche@uwaterloo.ca>
#######################################################

rm(list = ls())

uu <- (1:100 - .5) /100
xgev <- qAmax(uu, c(100,30,0), 'gev')
vname <-  c("lmom", "method", "para", "distr", "varcov", "llik", "data")

## Verify that important distribution and method works
TestObj <- function(f){
  expect_is(f, 'amax')
  expect_equal(names(f), vname)
  expect_true(is.na(f$varcov))
  expect_equal(length(f$data), 100)
  expect_equal(length(coef(f)), 3)
  expect_true(all(is.finite(coef(f))))
}

S <- function(z) signif(as.numeric(z))

TestObj(f <- FitAmax(xgev, distr = 'gev', method = 'lmom', varcov = FALSE))
expect_equal(S(f$para), c(99.8386, 30.1261,  0.0000))
expect_equal(S(f$para), c(99.8386, 30.1261,  0.0000))

TestObj(f <- FitAmax(xgev, distr = 'gno', method = 'lmom', varcov = FALSE))
expect_equal(S(f$lmom), c(117.228, 20.8818, 3.54844, 3.19116, 1.18389))

TestObj(FitAmax(xgev, distr = 'glo', method = 'lmom', varcov = FALSE))
TestObj(FitAmax(xgev, distr = 'pe3', method = 'lmom', varcov = FALSE))
TestObj(FitAmax(xgev, distr = 'gev', method = 'mle', varcov = FALSE))
TestObj(FitAmax(xgev, distr = 'gno', method = 'mle', varcov = FALSE))
TestObj(FitAmax(xgev, distr = 'glo', method = 'mle', varcov = FALSE))

TestObj(f <- FitAmax(xgev, distr = 'pe3', method = 'mle', varcov = FALSE))
expect_match(f$method , 'mle')
expect_match(f$distr , 'pe3')

## verify output quantities are fine
f <- FitAmax(xgev,'gev', method = 'lmom', varcov = TRUE, nsim = 10)
expect_true(all(is.finite(vcov(f))))
expect_equal(dim(vcov(f)), c(3,3))
expect_equal(S(AIC(f)), 1000.42)
expect_true(all(is.finite(coef(f))))


f <- FitAmax(xgev,'gev', method = 'mle', varcov = TRUE)
expect_true(all(is.finite(vcov(f))))
expect_equal(dim(vcov(f)), c(3,3))
expect_equal(S(AIC(f)), 1000.39)

## verify labels
expect_equal(colnames(f$varcov), names(f$para))


## Verify the automatic selection
f <- FitAmax(xgev, distr = c('gev','glo','gno','pe3'), method = 'mle')
expect_match(f$distr, 'gno')

f <- FitAmax(xgev, distr = c('gev','glo','gno','pe3'),
                  method = 'mle', tol.gev = 2)
expect_match(f$distr, 'gev')

f <- FitAmax(xgev, distr = c('gev','gum'), method = 'mle')
expect_match(f$distr, 'gum')

f <- FitAmax(xgev, distr = c('gev','glo','gno','pe3'),
                  method = 'lmom', varcov = FALSE)
expect_match(f$distr, 'gno')

f <- FitAmax(xgev, distr = c('gev','glo','gno','pe3'),
                  method = 'lmom', varcov = FALSE, tol.gev = 2)
expect_match(f$distr, 'gev')

f <- FitAmax(xgev, distr = c('gev','gum'), method = 'lmom',
                  varcov = FALSE)
expect_match(f$distr, 'gum')


## Verifying return error
expect_error(FitAmax(xgev, distr = 'gev', nsim = 1))

xgev[1] <- NA
expect_error(FitAmax(xgev, distr = 'gev'))

xgev[1] <- Inf
expect_error(FitAmax(xgev, distr = 'gev'))

