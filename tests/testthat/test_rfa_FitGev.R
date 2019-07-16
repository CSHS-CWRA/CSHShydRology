######################################################
## testing FitGev
#######################################################
rm(list = ls())

S <- function(z) signif(as.numeric(z))

uu <- (seq(1000)-.5)/1000
xgev <- qAmax(uu, c(100,30, -.15), 'gev')
f <- FitGev(xgev)

expect_equal(S(f$para), c(99.9884, 29.9728, -0.149058))
expect_match(f$distr, 'gev')
expect_match(f$method, 'gml')
expect_equal(f$prior, c(-0.1, 0.015))
expect_true(all(is.finite(vcov(f))))
expect_equal(length(f$data), 1000)
expect_equal(S(AIC(f)), 10134.6)


## as approximation to mle when uniform prior is used
f <- FitGev(xgev, varcov = FALSE, mu = 0, sig2 = 1/12)
f0 <- FitAmax(xgev, 'gev', method = 'mle', varcov = FALSE)
expect_equal(S(AIC(f0)), S(AIC(f)))
expect_true(all((1-coef(f)/coef(f0))^2 < 1e-6))
