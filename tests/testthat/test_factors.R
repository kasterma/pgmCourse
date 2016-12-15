context("general factor tests")

test_that("factors test",{
  expect_error(create_factor(c(1,2), list(c(1,2), c(1,3))), regexp = "!anyDuplicated")
  expect_error(create_factor(c(1,2), list(c(1,2), c(2,3))), regexp = "prod")
  })
