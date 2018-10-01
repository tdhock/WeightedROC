##works_with_R("3.3.1", GsymPoint="1.0")
library(testthat)
library(GsymPoint)
library(WeightedROC)
context("AUC")

ties <- function(score.vec){
  score.tab <- table(score.vec)
  score.tab[1 < score.tab]
}

data(elastase)
gsym.point.GPQ.elastase <- gsym.point(
  methods = "GPQ", data = elastase, 
  marker = "elas", status = "status", tag.healthy = 0, categorical.cov = NULL, 
  CFN = 1, CFP = 1, control = control.gsym.point(), confidence.level = 0.95, 
  trace = FALSE, seed = FALSE, value.seed = 3)
tp.fp <- with(elastase, WeightedROC(elas, status))
my.auc <- WeightedAUC(tp.fp)
gsym.auc <- GsymPoint:::calculate.empirical.AUC(elastase, "elas", "status", 0)
test_that("AUC consistent with GsymPoint for elastase data set", {
  expect_equal(my.auc, gsym.auc)
})

## Figure 1: melanoma cancer data.
data(melanoma)
melanoma.cutpoint1 <- gsym.point(
  methods = "GPQ", data = melanoma,
  marker = "X", status = "group", tag.healthy = 0, categorical.cov = NULL, 
  CFN = 2, CFP = 1, control = control.gsym.point(),confidence.level = 0.95, 
  trace = FALSE, seed = TRUE, value.seed = 3)
tp.fp <- with(melanoma, WeightedROC(X, group))
my.auc <- WeightedAUC(tp.fp)
gsym.auc <- GsymPoint:::calculate.empirical.AUC(melanoma, "X", "group", 0)
test_that("AUC consistent with GsymPoint for melanoma data set", {
  expect_equal(my.auc, gsym.auc)
})

## Figure 2: prostate cancer data.
data(prostate)
gsym.point.GPQ.prostate <- gsym.point(
  methods = "GPQ", data = prostate,
  marker = "marker", status = "status", tag.healthy = 0, categorical.cov = NULL, 
  CFN = 10, CFP = 1, control = control.gsym.point(I=1500), confidence.level = 0.95, 
  trace = FALSE, seed = TRUE, value.seed = 3)
tp.fp <- with(prostate, WeightedROC(marker, status))
my.auc <- WeightedAUC(tp.fp)
gsym.auc <- GsymPoint:::calculate.empirical.AUC(prostate, "marker", "status", 0)
test_that("AUC consistent with GsymPoint for melanoma data set", {
  expect_equal(my.auc, gsym.auc)
})

library(geometry)
## (FPR,TPR) starts at (1,1) and ends at (0,0)
geometry.auc <- with(tp.fp, polyarea(c(FPR, 1, 1), c(TPR, 0, 1)))
test_that("geometry package auc", {
  expect_equal(my.auc, geometry.auc)
})

test_that("identity weights with no ties and perfect classification", {
  y <- c(-1, -1, 1, 1)
  w <- c(1, 1, 1, 1)
  y.hat <- c(1, 2, 3, 4)
  tp.fp <- WeightedROC(y.hat, y, w)
  expect_equal(tp.fp$TPR, c(1, 1, 1, 0.5, 0))
  expect_equal(tp.fp$FN, c(0, 0, 0, 1, 2))
  expect_equal(tp.fp$FPR, c(1, 0.5, 0, 0, 0))
  expect_equal(tp.fp$FP, c(2, 1, 0, 0, 0))
  expect_equal(tp.fp$threshold, c(1, 2, 3, 4, Inf))
  auc <- WeightedAUC(tp.fp)
  expect_equal(auc, 1)
})

test_that("identity weights with no ties and 1 bad error", {
  y <- c(-1, -1, 1, 1)
  w <- c(1, 1, 1, 1)
  y.hat <- c(1, 2, 3, -1)
  tp.fp <- WeightedROC(y.hat, y, w)
  expect_equal(tp.fp$TPR, c(1, 0.5, 0.5, 0.5, 0))
  expect_equal(tp.fp$FN, c(0, 1, 1, 1, 2))
  expect_equal(tp.fp$FPR, c(1, 1, 0.5, 0, 0))
  expect_equal(tp.fp$FP, c(2, 2, 1, 0, 0))
  expect_equal(tp.fp$threshold, c(-1, 1, 2, 3, Inf))
  auc <- WeightedAUC(tp.fp)
  expect_equal(auc, 1/2)
})

test_that("identity weights with no ties and 1 not so bad error", {
  y <- c(-1, -1, 1, 1)
  w <- c(1, 1, 1, 1)
  y.hat <- c(1, 2, 3, 1.5)
  tp.fp <- WeightedROC(y.hat, y, w)
  expect_equal(tp.fp$TPR, c(1, 1, 0.5, 0.5, 0))
  expect_equal(tp.fp$FN, c(0, 0, 1, 1, 2))
  expect_equal(tp.fp$FPR, c(1, 0.5, 0.5, 0, 0))
  expect_equal(tp.fp$FP, c(2, 1, 1, 0, 0))
  expect_equal(tp.fp$threshold, c(1, 1.5, 2, 3, Inf))
  auc <- WeightedAUC(tp.fp)
  expect_equal(auc, 3/4)
})

test_that("identity weights with 1 tie", {
  y <- c(-1, -1, 1, 1)
  w <- c(1, 1, 1, 1)
  y.hat <- c(1, 2, 3, 1)
  tp.fp <- WeightedROC(y.hat, y, w)
  expect_equal(tp.fp$TPR, c(1, 0.5, 0.5, 0))
  expect_equal(tp.fp$FN, c(0, 1, 1, 2))
  expect_equal(tp.fp$FPR, c(1, 0.5, 0, 0))
  expect_equal(tp.fp$FP, c(2, 1, 0, 0))
  expect_equal(tp.fp$threshold, c(1, 2, 3, Inf))
  auc <- WeightedAUC(tp.fp)
  expect_equal(auc, 5/8)
})

test_that("variable weights with 1 tie", {
  y <- c(-1, -1, 1, 1)
  w <- c(1, 1, 1, 9)
  y.hat <- c(1, 2, 3, 1)
  tp.fp <- WeightedROC(y.hat, y, w)
  expect_equal(tp.fp$TPR, c(1, 0.1, 0.1, 0))
  expect_equal(tp.fp$FN, c(0, 9, 9, 10))
  expect_equal(tp.fp$FPR, c(1, 0.5, 0, 0))
  expect_equal(tp.fp$FP, c(2, 1, 0, 0))
  expect_equal(tp.fp$threshold, c(1, 2, 3, Inf))
  auc <- WeightedAUC(tp.fp)
  expect_equal(auc, 0.1 + 0.9*0.5/2)
})

test_that("variable weights with 2 ties", {
  y <- c(-1, -1, 1, 1, 1)
  w <- c(1, 1, 1, 4, 5)
  y.hat <- c(1, 2, 3, 1, 1)
  tp.fp <- WeightedROC(y.hat, y, w)
  expect_equal(tp.fp$TPR, c(1, 0.1, 0.1, 0))
  expect_equal(tp.fp$FN, c(0, 9, 9, 10))
  expect_equal(tp.fp$FPR, c(1, 0.5, 0, 0))
  expect_equal(tp.fp$FP, c(2, 1, 0, 0))
  expect_equal(tp.fp$threshold, c(1, 2, 3, Inf))
  auc <- WeightedAUC(tp.fp)
  expect_equal(auc, 0.1 + 0.9*0.5/2)
})

test_that("ROCR.simple", {
  library(ROCR)
  data(ROCR.simple)
  pred <- prediction( ROCR.simple$predictions, ROCR.simple$labels)
  perf <- performance(pred,"tpr","fpr")
  tp.fp <- with(ROCR.simple, WeightedROC(predictions, labels))
  expect_equal(tp.fp$TPR, rev(perf@y.values[[1]]))
  expect_equal(tp.fp$FPR, rev(perf@x.values[[1]]))
  ROCR.auc <- performance(pred, "auc")@y.values[[1]]
  my.auc <- WeightedAUC(tp.fp)
  expect_equal(ROCR.auc, my.auc)
})

test_that("aSAH", {
  library(pROC)
  data(aSAH)
  proc <- roc(outcome ~ s100b, aSAH)
  tp.fp <- with(aSAH, WeightedROC(s100b, outcome))
  expect_equal(tp.fp$FPR, 1-proc$specificities)
  expect_equal(tp.fp$TPR, proc$sensitivities)
  my.auc <- WeightedAUC(tp.fp)
  expect_equal(my.auc, as.numeric(proc$auc))
})
