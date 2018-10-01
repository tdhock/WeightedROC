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

  library(WeightedROC)
  ## Compute the AUC for this weighted data set.
  y <- c(0, 0, 1, 1, 1)
  w <- c(1, 1, 1, 4, 5)
  y.hat <- c(1, 2, 3, 1, 1)
  tp.fp <- WeightedROC(y.hat, y, w)
  (wauc <- WeightedAUC(tp.fp))

  if(require(glmnet)){

    ## glmnet 1.9-5 (2013-8-1) also has a function called auc, with a
    ## third argument "w" that seems to be for weights. In a data set
    ## with no tied scores, glmnet::auc is the same as WeightedAUC.
    y <- c(0, 0, 1, 0, 1)
    y.hat <- c(1, 2, 3, 4, 5)
    w <- c(1, 1, 1, 4, 5)
    rbind(
      `glmnet::auc`=c(
        missing.weights=glmnet::auc(y, y.hat),
        unequal.weights=glmnet::auc(y, y.hat, w),
        equal.weights=glmnet::auc(y, y.hat, rep(1, length(y)))),
      `WeightedROC::WeightedAUC`=c(
        WeightedAUC(WeightedROC(y.hat, y)),
        WeightedAUC(WeightedROC(y.hat, y, w)),
        WeightedAUC(WeightedROC(y.hat, y, rep(1, length(y))))))
    
    ## In a data set with tied scores, using glmnet::auc(y,prob) with
    ## a missing third argument gives the same result as WeightedAUC,
    ## but glmnet::auc(y,prob,w) does not.
    set.seed(1)
    y <- c(0, 0, 1, 1, 1)
    w <- c(1, 1, 1, 4, 5)
    y.hat <- c(1, 2, 3, 1, 1)
    rbind(
      `glmnet::auc`=c(
        missing.weights=glmnet::auc(y, y.hat),
        unequal.weights=glmnet::auc(y, y.hat, w),
        equal.weights=glmnet::auc(y, y.hat, rep(1, length(y)))),
      `WeightedROC::WeightedAUC`=c(
        WeightedAUC(WeightedROC(y.hat, y)),
        wauc,
        WeightedAUC(WeightedROC(y.hat, y, rep(1, length(y))))))
    
  }

  library(ROCR)
  library(pROC)
  ## For the un-weighted ROCR example data set, verify that our AUC is
  ## the same as that of ROCR/pROC/glmnet. Note that glmnet::auc is
  ## faster than WeightedROC::WeightedAUC because glmnet::auc it does
  ## not include the overhead of computing a data.frame(TPR, FPR) that
  ## could be plotted.
  data(ROCR.simple, envir=environment())
  if(require(microbenchmark)){
    microbenchmark(WeightedROC={
      tp.fp <- with(ROCR.simple, WeightedROC(predictions, labels))
      wroc <- WeightedAUC(tp.fp)
    }, ROCR={
      pred <- with(ROCR.simple, prediction(predictions, labels))
      rocr <- performance(pred, "auc")@y.values[[1]]
    }, pROC={
      proc <- pROC::auc(labels ~ predictions, ROCR.simple, algorithm=2)
    }, glmnet={
      gnet <- with(ROCR.simple, {
        glmnet::auc(labels, predictions, rep(1, length(labels)))
      })
    }, times=10)
    rbind(WeightedROC=wroc, ROCR=rocr, pROC=proc, glmnet=gnet) #same
  }

  ## For the un-weighted pROC example data set, verify that our AUC is
  ## the same as that of ROCR/pROC/glmnet. 
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
    }, glmnet={
      gnet <- with(aSAH, {
        glmnet::auc(ifelse(outcome=="Good", 0, 1), s100b, rep(1, length(s100b)))
      })
    }, times=10)
    rbind(WeightedROC=wroc, ROCR=rocr, pROC=proc, glmnet=gnet)
  }
  
})
