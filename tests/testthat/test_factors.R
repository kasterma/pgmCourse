context("general factor tests")

test_that("check index_to_assignment and assignment_to_idex", {
  expect_equal(index_to_assignment(1, list(c(2,1))),1)
  N <- 111
  for (idx in seq(1, N)) {
    expect_equal(index_to_assignment(idx, list(c(2,N))),idx)
  }
  vars <- list(c(1,3),c(3,2))
  assigns <- data.matrix(expand.grid(c(1,2,3), c(1,2)))
  for (idx in seq(1, 2*3)) {
    expect_equivalent(index_to_assignment(idx, vars), assigns[idx,])
  }
  expect_equivalent(index_to_assignment(seq(1, 2*3), vars), assigns)

  expect_equivalent(assignment_to_index(assigns, vars), seq(1, 2*3))
})

test_that("factors test",{
  expect_error(create_factor(c(1,2), list(c(1,2), c(1,3))), regexp = "!anyDuplicated")
  expect_error(create_factor(c(1,2), list(c(1,2), c(2,3))), regexp = "prod")
  })
