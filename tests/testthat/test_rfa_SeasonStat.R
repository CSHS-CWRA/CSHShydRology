######################################################
## testing SeasonStat
## Martin Durocher <mduroche@uwaterloo.ca>
#######################################################

## Verify that a known exemple work
fit <- SeasonStat(ex1$date)
expect_true(class(fit) == 'numeric')
ref <- c(-0.4259258,  0.8665048,  2.0276568,  0.9655275)
expect_true(all(abs(ref-fit) < 1e-5))

## verify that multiple stations works
fit2 <- SeasonStat(date~station, ex2)
expect_true(class(fit2) == 'matrix')
expect_true(all(rbind(fit,fit) == fit2))

## Verify that using a date in characters works
expect_equal(fit,SeasonStat(as.character(ex1$date)))

## return an error if it is not a date
expect_error(SeasonStat(1:5))
expect_error(SeasonStat(month.name))
