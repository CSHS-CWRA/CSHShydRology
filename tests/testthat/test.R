#Test to make sure it works

#data <- readRDS('../../data/owikeno.RDS')
output <- readRDS('owikenorunoff.RDS')

#context('Lumped Runoff')

test_that('Runoff is correct', {
  expect_equal(DCWBM(owikeno),output)
})