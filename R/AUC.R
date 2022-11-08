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
  expected.df <- data.frame(value=1:0, i=c(1,nrow(tpr.fpr)))
  for(expected.i in 1:nrow(expected.df)){
    expected.row <- expected.df[expected.i,]
    computed.row <- tpr.fpr[expected.row$i,]
    bad <- function(XPR)computed.row[[XPR]] != expected.row$value
    if(bad("FPR") || bad("TPR")){
      warning(sprintf("ROC curve incomplete (expected FPR=TPR=%s in row %d, got FPR=%s, TPR=%s)", paste(expected.row$value), expected.row$i, paste(computed.row$FPR), paste(computed.row$TPR)))
    }
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

  library(WeightedROC)
  ## Compute the AUC for this weighted data set.
  y <- c(0, 0, 1, 1, 1)
  w <- c(1, 1, 1, 4, 5)
  y.hat <- c(1, 2, 3, 1, 1)
  tp.fp <- WeightedROC(y.hat, y, w)
  (wauc <- WeightedAUC(tp.fp))

  ## For the un-weighted ROCR example data set, verify that our AUC is
  ## the same as that of ROCR/pROC.
  if(require(microbenchmark) && require(ROCR) && require(pROC)){
    data(ROCR.simple, envir=environment())
    microbenchmark(WeightedROC={
      tp.fp <- with(ROCR.simple, WeightedROC(predictions, labels))
      wroc <- WeightedAUC(tp.fp)
    }, ROCR={
      pred <- with(ROCR.simple, prediction(predictions, labels))
      rocr <- performance(pred, "auc")@y.values[[1]]
    }, pROC={
      proc <- pROC::auc(labels ~ predictions, ROCR.simple, algorithm=2)
    }, times=10)
    rbind(WeightedROC=wroc, ROCR=rocr, pROC=proc) #same
  }

  ## For the un-weighted pROC example data set, verify that our AUC is
  ## the same as that of ROCR/pROC.
  data(aSAH, envir=environment())
  table(aSAH$s100b)
  if(require(microbenchmark)){
    microbenchmark(WeightedROC={
      tp.fp <- with(aSAH, WeightedROC(s100b, outcome))
      wroc <- WeightedAUC(tp.fp)
    }, ROCR={
      pred <- with(aSAH, prediction(s100b, outcome))
      rocr <- performance(pred, "auc")@y.values[[1]]
    }, pROC={
      proc <- pROC::auc(outcome ~ s100b, aSAH, algorithm=2)
    }, times=10)
    rbind(WeightedROC=wroc, ROCR=rocr, pROC=proc)
  }

})
