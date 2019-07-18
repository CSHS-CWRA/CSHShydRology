######################################################
## testing SeasonStat
## Martin Durocher <mduroche@uwaterloo.ca>
#######################################################

rm(list = ls())

date.lst <- seq(as.Date('1950/1/1'),as.Date('2009/12/31'), 'days')

set.seed(843)
dd <- sample(date.lst, 200)

## Verify that a known exemple work
fit <- SeasonStat(dd)

expect_true(class(fit) == 'numeric')
expect_equal(names(fit), c('x','y','angle','radius'))


## verify that multiple stations works
dd2 <- rbind(data.frame(station = 1, date = dd),
            data.frame(station = 2, date = dd))


fit2 <- SeasonStat(date~station, dd2)
expect_true(class(fit2) == 'matrix')
expect_equivalent(rbind(fit,fit), fit2)

## Verify that using a date in characters works
expect_equal(fit,SeasonStat(as.character(dd)))

## return an error if it is not a date
expect_error(SeasonStat(1:5))
expect_error(SeasonStat(month.name))
