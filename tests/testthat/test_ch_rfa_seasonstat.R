######################################################
## testing ch_rfa_seasonstat
## Martin Durocher <mduroche@uwaterloo.ca>
#######################################################

date.lst <- seq(as.Date('1950/1/1'),as.Date('2009/12/31'), 'days')

set.seed(843)
dd <- sample(date.lst, 200)

## Verify that a known example works
fit <- ch_rfa_seasonstat(dd)

expect_true(class(fit) == 'numeric')
expect_equal(names(fit), c('x','y','angle','radius'))


## verify that multiple stations work
dd2 <- rbind(data.frame(station = 1, date = dd),
            data.frame(station = 2, date = dd))


fit2 <- ch_rfa_seasonstat(date~station, dd2)

# changed by Kevin Shook, May, 2 2021 to extract first element from class
expect_true(class(fit2)[1] == 'matrix')
expect_equivalent(rbind(fit,fit), fit2)

## Verify that using a date in characters works
expect_equal(fit,ch_rfa_seasonstat(as.character(dd)))

## return an error if it is not a date
expect_error(ch_rfa_seasonstat(month.name))
