context("errors")

test_that("data set with no positive labels is an error", {
  y <- c(-1, -1, -1, -1)
  w <- c(1, 1, 1, 1)
  y.hat <- c(1, 2, 3, 4)
  expect_error({
    WeightedROC(y.hat, y, w)
  }, "no positive labels")
})

test_that("data set with no negative labels is an error", {
  y <- c(1, 1, 1, 1)
  w <- c(1, 1, 1, 1)
  y.hat <- c(1, 2, 3, 4)
  expect_error({
    WeightedROC(y.hat, y, w)
  }, "no negative labels")
})
