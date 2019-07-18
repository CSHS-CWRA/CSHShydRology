##############################################################################
# Testing DataWide function
# Martin Durocher <mduroche@@uwaterloo.ca>
#############################################################################

rm(list = ls())

## basic test
xini <- x0 <- data.frame(1:12, expand.grid(1:3,1:4)[,c(2,1)])
names(xini) <- names(x0) <- c('value','site','time')

xmat <- DataWide(xini)
xmat0 <- matrix(xini$value, 3,4)

expect_true(all(xmat == xmat0))

## using factor
lb.s <- paste0('s',c(2,1,4,3))
lb.t <- paste0('t',c(2,3,1))
x0[,2] <- factor(x0[,2], labels = lb.s)
x0[,3] <- factor(x0[,3], labels = lb.t)

xmat <- DataWide(x0)
expect_true(all(xmat == xmat0))
expect_equal(colnames(xmat), lb.s)
expect_equal(rownames(xmat), lb.t)

## verify ordering

x1 <- xini[order(as.character(x0[,2]),
         as.character(x0[,3])),]

xmat <- DataWide(x1, order.time = FALSE, order.site = FALSE)
xmat1 <- matrix(c(6,3,12,9,
                  4,1,10,7,
                  5,2,11,8), 3,4, byrow = TRUE)

expect_true( all(xmat == xmat1))

xmat <- DataWide(x1, order.time = TRUE, order.site = FALSE)
xmat1 <- matrix(c(4,1,10,7,
                  5,2,11,8,
                  6,3,12,9), 3,4, byrow = TRUE)

expect_true( all(xmat == xmat1))

xmat <- DataWide(x1, order.time = FALSE, order.site = TRUE)
xmat1 <- matrix(c(3,6,9,12,
                  1,4,7,10,
                  2,5,8,11), 3,4, byrow = TRUE)

expect_true( all(xmat == xmat1))

xmat <- DataWide(x1, order.time = TRUE, order.site = TRUE)
expect_true( all(xmat == xmat0))

## using formula

x0 <- x0[,c(2,3,1)]
xmat <- DataWide(value~site+time, x0)
expect_true( all(xmat == xmat0))

## Create the missing value when necessary

x1 <- x0
cnew <- xini[,3]
cnew[4] <- 4
x1[,2] <- factor(cnew, labels = paste0('t',c(2,3,1,4)))

xmat <- DataWide(value~site+time, x1)

xmat1 <- matrix(c( 1, NA,  7, 10,
                   2,  5,  8, 11,
                   3,  6,  9, 12,
                  NA,  4, NA, NA), 4,4, byrow = TRUE)
id1 <- c(4,5,12,16)
expect_equal(xmat[-id1], xmat1[-id1])
expect_equal(xmat[id1], xmat1[id1])

## if time is a date

x0[,2] <- rep(c(as.Date('2000/1/2'),
                as.Date('2000/1/3'),
                as.Date('2000/1/1')),4)

xmat <- DataWide( value~site+time, x0)
xmat1 <- matrix(c(3,6,9,12,
                  1,4,7,10,
                  2,5,8,11), 3,4, byrow = TRUE)
expect_true( all(xmat == xmat1))

## If there is a missing value

x1 <- x0
x1$value[1] <- NA

xmat <- DataWide(value~site+time, x1)
expect_equal(xmat[-2], xmat1[-2])
expect_true(is.na(xmat[2]))

## Raise error for duplicate

x1$site[5] <- 's2'
expect_error(DataWide(value~site+time, x1))

## Testing the independent mode

x2 <- x0
x2[,2] <- sample(1:12)
xmat <- DataWide(value ~ site, x2)

expect_true(all(xmat == xmat0))


x2 <- x2[-1, ]
xmat <- DataWide(value ~ site, x2)
xmat0 <- matrix(c(2,4,7,10,
                  3,5,8,11,
                  NA,6,9,12), 3,4, byrow = TRUE)

cid <- !is.na(xmat)
expect_true(all(xmat[cid] == xmat0[cid]))
