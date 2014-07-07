WeightedAUC <- structure(function
### Calculate the exact area under the ROC curve.
(tpr.fpr
### Output of WeightedROC: data.frame with the true positive rate
### (TPR) and false positive rate (FPR).
 ){
  stopifnot(is.data.frame(tpr.fpr))
  stopifnot(nrow(tpr.fpr) > 1)
  for(var.name in c("TPR", "FPR")){
    ## Ensure that the curve is sorted in decreasing order.
    stopifnot(diff(tpr.fpr[[var.name]]) <= 0)
  }
  right <- tpr.fpr[-nrow(tpr.fpr),]
  left <- tpr.fpr[-1,]
  width <- right$FPR - left$FPR
  rect.area <- left$TPR * width
  triangle.h <- right$TPR - left$TPR
  triangle.area <- triangle.h * width / 2
  my.auc <- sum(rect.area, triangle.area)
  my.auc
### Numeric scalar.
}, ex=function(){
  library(ROCR)
  library(microbenchmark)
  ## For this un-weighted data set, verify that our AUC is the same as
  ## that of ROCR.
  data(ROCR.simple)
  microbenchmark(ROCR={
    pred <- with(ROCR.simple, prediction(predictions, labels))
    rocr <- performance(pred, "auc")@y.values[[1]]
  }, WeightedROC={
    tp.fp <- with(ROCR.simple, WeightedROC(predictions, labels))
    wroc <- WeightedAUC(tp.fp)
  })
  rbind(ROCR=rocr, WeightedROC=wroc)
  ## Compute the AUC for this weighted data set.
  y <- c(-1, -1, 1, 1, 1)
  w <- c(1, 1, 1, 4, 5)
  y.hat <- c(1, 2, 3, 1, 1)
  tp.fp <- WeightedROC(y.hat, y, w)
  WeightedAUC(tp.fp)
})
