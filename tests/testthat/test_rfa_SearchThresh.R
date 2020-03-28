#########################################################
## Martin Durocher <mduroche@uwaterloo.ca>
#########################################################

context("Testing SearchThresh and FindThresh function")

test_that("Verifying SearchThresh + Find", {
  
  
## Modify the series to have a heavy tails
xd <- flowStJohn
u0 <- 1100
id0 <- xd$flow > u0
y0 <- xd$flow[id0]-u0
beta <- fgpa1d(y0)
y1 <- qgpa(pgpa(y0, beta[1],beta[2]) , beta[1], -.05)
xd$flow[id0] <- u0 + y1


ulst <- sort(unique(xd$flow[xd$flow]))[200:270]

fit <- SearchThresh(flow~date, xd, u = ulst, declust = 'wrc', r = 14,
                    verbose = FALSE)

cvars <- c('u','q10','ad','fdr', 'ppy')

##
out <- FindThresh(fit, method = 'sgn', tol.sgn = 2)[,cvars]
expect_true(out$ad > 0.05)

out <- FindThresh(fit, method = 'sgn', tol.sgn = .3)[,cvars]
expect_true(out$ad > 0.3)

out <- FindThresh(fit, method = 'fdr', tol.sgn = .3)[,cvars]
expect_true(out$ad > 0.3)

FindThresh(fit, method = 'max', tol.sgn = .05)[,cvars]

out <- FindThresh(fit, method = 'sgn-max', tol.sgn = .25)[,cvars]

out <- FindThresh(fit, method = 'sgn-ppy', tol.sgn = .05)[,cvars]
expect_true(out$ppy > 2)
out <- FindThresh(fit, method = 'sgn-ppy', tol.sgn = 1.8)[,cvars]
expect_true(out$ppy < 2)

out <- FindThresh(fit, method = 'max', tol.sgn = .05)[,cvars]
expect_true(out$ad == 0.5)

out <- FindThresh(fit, method = 'ppy', tol.ppy = 2)[,cvars]
expect_true(abs(out$ppy-2) < 0.05)



###############################################################
## nonstationary
###############################################################
taus <- seq(.92,.97, .001)
fit <- SearchThreshNs(flow~date, xd, tau = taus, declust = 'wrc', r = 14,
                      trend = ~ date, thresh = ~date,
                      method = 'reg-lmom', verbose = FALSE)


##
out <- FindThresh(fit, method = 'sgn', tol.sgn = 2)[,cvars]
expect_true(out$ad > 0.05)

out <- FindThresh(fit, method = 'sgn', tol.sgn = .3)[,cvars]
expect_true(out$ad > 0.3)

out <- FindThresh(fit, method = 'fdr', tol.sgn = .3)[,cvars]
expect_true(out$ad > 0.3)

FindThresh(fit, method = 'max', tol.sgn = .05)[,cvars]

out <- FindThresh(fit, method = 'sgn-max', tol.sgn = .25)[,cvars]

out <- FindThresh(fit, method = 'sgn-ppy', tol.sgn = .05)[,cvars]
expect_true(out$ppy > 2)
out <- FindThresh(fit, method = 'sgn-ppy', tol.sgn = 1.8)[,cvars]
expect_true(out$ppy < 2)

out <- FindThresh(fit, method = 'max', tol.sgn = .05)[,cvars]
expect_true(out$ad == 0.5)

out <- FindThresh(fit, method = 'ppy', tol.ppy = 2)[,cvars]
expect_true(abs(out$ppy-2) < 0.05)

#plot(ad~u, fit, type = 'b')
#lines(fdr~u, fit, col = 'red')

})