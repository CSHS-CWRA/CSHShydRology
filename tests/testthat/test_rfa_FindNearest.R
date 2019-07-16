##############################################################################
# Testing FindNearest function
# Martin Durocher <mduroche@@uwaterloo.ca>
#############################################################################

## create data
x <- matrix(0:99, nrow = 10)
id <- c(2,4,3,6,10,8,7,9,1,5)
x <- x[id, id]
x[lower.tri(x)] <- t(x)[lower.tri(x)] 
diag(x) <- 0
colnames(x) <- paste0('site',1:10)

## VSerify the result of the search works
x0 <- FindNearest(x, x[2,], 3)
expect_equal(x0[2,], sort(x[2,])[1:3])
expect_equal(colnames(x0), c('site2', 'site9', 'site3'))
expect_equal(dim(x0), c(10,3))

## Verify the argument "row" works
x0 <- FindNearest(x, x[2,], 3, row = TRUE)
expect_equal(dim(x0), c(3,3))
expect_equal(x0, x[c(2,9,3),c(2,9,3)])

## Verify the super region option
id <- c(1,3,5,7,9,2,4,6,8,10)
x2 <- x[id, id]
colnames(x2) <- paste0('site',1:10)

x0 <- FindNearest(x, x[1,], 3, x2[1,], 5)

xs0 <- FindNearest(x, x2[1,],5)
xs1 <- FindNearest(xs0, xs0[1,],3)

expect_equal(x0,xs1)

