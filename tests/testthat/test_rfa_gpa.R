#################################################
## Martin Durocher <mduroche@uwaterloo.ca>
##################################################

context("Testing GPA function")

test_that("Verifying GPA", {

## qgpa and pgpa are inverse
u <- seq(0.001,.999, l = 21)

for(kk in seq(-.5,1,l=31)){
  q <- qgpa(u,1,0)
  expect_equal(pgpa(q,1,0), u)
}

expect_equal(qgpa(.25,1,0),qgpa(.75,1,0, lower.tail = FALSE))

## Verify lower tail option
q <- qgpa(u,1,.1, lower.tail = FALSE)
expect_equal(expect_equal(pgpa(q,1,.1, lower.tail = FALSE), u), u)

## validation with evd package
expect_equal(signif(dgpa(.5,1,0)), 0.606531)
expect_equal(signif(dgpa(.2,1,-.1, log = TRUE)), -0.217829)
expect_equal(signif(dgpa(.25,.5,-.2)), 1.12895)
expect_equal(signif(dgpa(.75, 1.5,.2)), 0.437400)

expect_equal(signif(qgpa(.5,1,0)), 0.693147)
expect_equal(signif(qgpa(.25,.5,-.2)), 0.14806)
expect_equal(signif(qgpa(.75, 1.5,.2)), 1.81606)

## Behaviour with value 0-1 and Inf
expect_error(qgpa(-1,1,0))
expect_error(qgpa(2,1,0))
expect_error(qgpa(.5,-1,0))
expect_error(qgpa(.5,1,c(0,0)))

expect_equal(qgpa(0,1,0),0)
expect_equal(qgpa(1,1,0),Inf)
expect_equal(pgpa(-Inf,1,0),0)
expect_equal(pgpa(Inf,1,0),1)

# monotone
expect_true(all(diff(pgpa(u,1,0))>0 )  )

## random value
expect_length(rgpa(5,1,0) , 5)

set.seed(1)
x <- pgpa(rgpa(5000,1,0),1,0)
expect_lt(max(abs(sort(x-.5)/5000)), 1e-4)

})
