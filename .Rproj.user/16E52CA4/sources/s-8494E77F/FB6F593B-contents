test_that("addvecs works", {
  expect_equal(addvecs(c(1,1,1,1), c(1,1,1,1)), c(2,2,2,2))
})

test_that("multvecs works", {
  expect_equal(multvecs(c(1,1,1,1), c(2,2,2,2)), c(2,2,2,2))
})

test_that("dotprod works", {
  expect_equal(dotprod(c(1,1,1,1), c(2,2,2,2)), 8)
})

test_that("dotprod returns an error if >2 vectors are input", {
  expect_error(dotprod(c(1,1),c(1,1),c(1,1)))
})
