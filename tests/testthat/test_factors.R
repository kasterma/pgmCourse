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

test_that("factor_product", {
  f1 <- create_factor(c(1,2,3), list(c(1, 3)))
  f2 <- create_factor(c(1,2,3), list(c(2, 3)))
  expect_equal(factor_product(f1, f2)$vars, list(c(1,3), c(2,3)))
  expect_equal(factor_product(f1, f2)$vals, c(1,2,3,2,4,6,3,6,9))

  f1 <- create_factor(c(1,2), list(c(1, 2)))
  f2 <- create_factor(c(1,2,3), list(c(2, 3)))
  expect_equal(factor_product(f1, f2)$vars, list(c(1,2), c(2,3)))
  expect_equal(factor_product(f1, f2)$vals, c(1,2,2,4,3,6))

  f1 <- create_factor(c(1,2,3,4,5,6), list(c(1, 2), c(2,3)))
  f2 <- create_factor(c(1,2,3,4,5,6), list(c(2, 3), c(1,2)))
  expect_equal(factor_product(f1, f2)$vars, list(c(1,2), c(2,3)))
  expect_equal(factor_product(f1, f2)$vals, c(1,8,6,20,15,36))

  f1 <- create_factor(c(1,2,3,4,5,6), list(c(1, 2), c(2,3)))
  f2 <- create_factor(c(1,2,3,4,5,6), list(c(2, 3), c(3,2)))
  expect_equal(factor_product(f1, f2)$vars, list(c(1,2), c(2,3), c(3,2)))
})

test_that("factor marg", {
  fact <- create_factor(c(1,2,3,4,5,6), list(c(1,2), c(2,3)))
  fm1 <- factor_marginaliztion(fact,3)
  fm2 <- factor_marginaliztion(fact,2)
  fm3 <- factor_marginaliztion(fact,1)
  expect_equal(fm1$vals, c(1,2,3,4,5,6))
  expect_equal(fm2$vals, c(9, 12))
  expect_equal(fm3$vals, c(3, 7, 11))
})

test_that("factor reduction", {
  fact <- create_factor(c(1,2,3,4,5,6), list(c(1,2), c(2,3)))
  expect_equal(factor_reduction(fact, 1, 1), create_factor(c(1,3,5), list(c(2,3))))
  expect_equal(factor_reduction(fact, 1, 2), create_factor(c(2,4,6), list(c(2,3))))
  expect_equal(factor_reduction(fact, 2, 1), create_factor(c(1,2), list(c(1,2))))
  expect_equal(factor_reduction(fact, 2, 2), create_factor(c(3,4), list(c(1,2))))
})
