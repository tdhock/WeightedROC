context("errors")

test_that("data set with no positive labels is an error", {
  y <- c(-1, -1, -1, -1)
  w <- c(1, 1, 1, 1)
  y.hat <- c(1, 2, 3, 4)
  expect_error({
    WeightedROC(y.hat, y, w)
  }, "only one label value")
})

test_that("data set with no negative labels is an error", {
  y <- c(1, 1, 1, 1)
  w <- c(1, 1, 1, 1)
  y.hat <- c(1, 2, 3, 4)
  expect_error({
    WeightedROC(y.hat, y, w)
  }, "only one label value")
})

test_that("NA labels is an error", {
  y <- c(-1, NA, 1, 1)
  w <- c(1, 1, 1, 1)
  y.hat <- c(1, 2, 3, 4)
  expect_error({
    tp.fp <- WeightedROC(y.hat, y, w)
  })
})

test_that("NA scores is an error", {
  y <- c(-1, -1, 1, 1)
  w <- c(1, 1, 1, 1)
  y.hat <- c(1, NA, 3, 4)
  expect_error({
    tp.fp <- WeightedROC(y.hat, y, w)
  }, "ROC curve undefined for NA guess")
})

test_that("NA weights is an error", {
  y <- c(-1, -1, 1, 1)
  w <- c(1, 1, NA, 1)
  y.hat <- c(1, 2, 3, 4)
  expect_error({
    tp.fp <- WeightedROC(y.hat, y, w)
  })
})

