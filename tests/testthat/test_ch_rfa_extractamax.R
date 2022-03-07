######################################################
## Testing ch_rfa_extractamax
## Martin Durocher <mduroche@uwaterloo.ca>
#######################################################

## Verify that the function return the right output
x <- ch_rfa_extractamax(flow~date, CAN01AD002)
expect_equal(dim(x), c(89,4))
expect_equal(names(x), c("flow","date","n","yy" ))

## Verify the filtering work. there is one year of 92 observations
x <- ch_rfa_extractamax(flow~date, CAN01AD002, tol = 92)
expect_equal(dim(x), c(89,4))

x <- ch_rfa_extractamax(flow~date, CAN01AD002, tol = 93)
expect_equal(dim(x), c(88,4))

## Verify that work with data.frame
y <- ch_rfa_extractamax(CAN01AD002[,c('flow','date')], tol = 365)
expect_identical(y,x)

## multiple site
x2 <- rbind(cbind(station = 1, x),cbind(station = 2, x))

ex1 <- ch_rfa_extractamax(flow~date, x)
ex2 <- ch_rfa_extractamax(flow~station + date, x2)

ex1.2 <- rbind(cbind(ex1, station = 1),
               cbind(ex1, station = 2))[,c(1,5,2,3,4)]

expect_true( all(ex2 == ex1.2))


## create a dataset
dt <- seq(as.Date('1970/10/01'), as.Date('2010/12/31'), 'day')
n <- length(dt)
xd <- data.frame(value = rlnorm(n), date = dt)
xts <- ts(xd$value, start = dt[1])


## verify the default colnames
a <- ch_rfa_extractamax(xd)
expect_equal(colnames(a), c('value','date','n', 'yy'))

## Verify the option nlab and ylab for renaming
a <- ch_rfa_extractamax(xd, nlab = 'nn', ylab = 'year')
expect_equal(colnames(a), c('value','date','nn', 'year'))

## Verify the option nlab and ylab for removing colums
a <- ch_rfa_extractamax(xd, nlab = NULL, ylab = NULL)
expect_equal(colnames(a), c('value','date'))

## Verify that extra columns are preserved
xd0 <- xd
xd0$year <- format(xd0$date,'%Y')
xd0$month <- format(xd0$date,'%m')

a <- ch_rfa_extractamax(xd0)
expect_equal(colnames(a), c('value','date','year','month','n', 'yy'))

## verify that extracted years are right.
expect_equal(a$year, a$yy)

## verify the "tol" option filter the year
yy <- format(a$date,'%Y')
expect_equal(yy, as.character(1970:2010))

xd0 <- with(xd0, xd0[year != '2000' & month != '5', ])
a <- ch_rfa_extractamax(xd0, tol = 360)

expect_equal(a$yy, as.character(c(1971:1999,2001:2010)))

## verify formula work
a <- ch_rfa_extractamax(value ~ date, xd0, tol = 360, nlab = NULL, ylab = NULL)
expect_equal(colnames(a), c('value','date'))

## Multiple sites
xd1 <- rbind(cbind(xd0, site = 1), cbind(xd0, site = 2))

a0 <- ch_rfa_extractamax(value ~ site + date, xd1, tol = 360, nlab = NULL, ylab = NULL)

a1 <- a0[a0$site == 1,-2]
a2 <- a0[a0$site == 2,-2]
expect_equal(colnames(a0), c('value', 'site', 'date'))
expect_true(all((a == a1) & a == a2))

## Verify columns for multiple sites
a0 <- ch_rfa_extractamax(value ~ site + date, xd1, nlab = 'nn', ylab = NULL)
expect_equal(colnames(a0), c('value', 'site', 'date','nn'))

a0 <- ch_rfa_extractamax(value ~ site + date, xd1)
expect_equal(colnames(a0), c('value', 'site', 'date', 'n', 'yy'))

