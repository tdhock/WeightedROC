WeightedROC <- structure(function
### Compute a weighted ROC curve.
(guess,
### Numeric vector of scores.
 label,
### True positive/negative labels. A factor with 2 unique values, or
### integer/numeric with values all in {0=negative,1=positive} or
### {1=negative,2=positive} or {-1=negative,1=positive}.
 weight=rep(1, length(label))
### Positive weights, by default 1.
 ){
  if(is.factor(label)){
    label <- as.integer(label)
  }
  stopifnot(is.numeric(label))
  if(all(label %in% c(0, 1))){
    label[label==0] <- -1
  }
  if(all(label %in% c(1, 2))){
    label[label==1] <- -1
    label[label==2] <- 1
  }
  ## by now, label must be in {-1,1}.
  stopifnot(label %in% c(-1,1))
  ## guess must be real.
  stopifnot(is.numeric(guess))
  stopifnot(length(label) == length(guess))
  ## weights must be positive.
  stopifnot(is.numeric(weight))
  stopifnot(length(label) == length(weight))
  stopifnot(weight > 0)
  ## order by guess to use cumsum.
  ord <- order(guess)
  y <- label[ord]
  w <- weight[ord]
  y.hat <- guess[ord]
  is.positive <- y == 1
  is.negative <- y == -1
  w.positive <- w.negative <- w
  w.positive[is.negative] <- 0
  w.negative[is.positive] <- 0
  cum.positive <- cumsum(w.positive)
  cum.negative <- cumsum(w.negative)
  is.end <- c(diff(y.hat) != 0, TRUE)
  n <- length(y)
  TPR <- c(1, 1-cum.positive[is.end]/cum.positive[n])
  FPR <- c(1, 1-cum.negative[is.end]/cum.negative[n])
  d <- data.frame(TPR, FPR)
  d
### data.frame with the true positive rate (TPR) and false positive
### rate (FPR).
}, ex=function(){
  library(ROCR)
  library(pROC)
  library(microbenchmark)
  data(ROCR.simple)
  ## Compare speed and plot ROC curves for the ROCR example data set.
  microbenchmark(WeightedROC={
    tp.fp <- with(ROCR.simple, WeightedROC(predictions, labels))
  }, ROCR={
    pred <- with(ROCR.simple, prediction(predictions, labels))
    perf <- performance(pred, "tpr", "fpr")
  }, pROC.1={
    proc <- roc(labels ~ predictions, ROCR.simple, algorithm=1)
  }, pROC.2={
    proc <- roc(labels ~ predictions, ROCR.simple, algorithm=2)
  }, pROC.3={
    proc <- roc(labels ~ predictions, ROCR.simple, algorithm=3)
  })
  perfDF <- function(p){
    data.frame(FPR=p@x.values[[1]], TPR=p@y.values[[1]], package="ROCR")
  }
  procDF <- function(p){
    data.frame(FPR=1-p$specificities, TPR=p$sensitivities, package="pROC")
  }
  roc.curves <- rbind(data.frame(tp.fp, package="WeightedROC"),
                      perfDF(perf), procDF(proc))
  library(ggplot2)
  ggplot()+
    geom_path(aes(FPR, TPR, color=package, linetype=package),
              data=roc.curves, size=1)+
    coord_equal()
  ## Compare speed and plot ROC curves for the pROC example data set.
  data(aSAH)
  microbenchmark(WeightedROC={
    tp.fp <- with(aSAH, WeightedROC(s100b, outcome))
  }, ROCR={
    pred <- with(aSAH, prediction(s100b, outcome))
    perf <- performance(pred, "tpr", "fpr")
  }, pROC.1={
    proc <- roc(outcome ~ s100b, aSAH, algorithm=1)
  }, pROC.2={
    proc <- roc(outcome ~ s100b, aSAH, algorithm=2)
  }, pROC.3={
    proc <- roc(outcome ~ s100b, aSAH, algorithm=3)
  })
  roc.curves <- rbind(data.frame(tp.fp, package="WeightedROC"),
                      perfDF(perf), procDF(proc))
  ggplot()+
    geom_path(aes(FPR, TPR, color=package, linetype=package),
              data=roc.curves, size=1)+
    coord_equal()
  ## Compute a small ROC curve with 1 tie to show the diagonal.
  y <- c(-1, -1, 1, 1)
  y.hat <- c(1, 2, 3, 1)
  microbenchmark(WeightedROC={
    tp.fp <- WeightedROC(y.hat, y)
  }, ROCR={
    pred <- prediction(y.hat, y)
    perf <- performance(pred, "tpr", "fpr")
  }, pROC.1={
    proc <- roc(y ~ y.hat, algorithm=1)
  }, pROC.2={
    proc <- roc(y ~ y.hat, algorithm=2)
  }, pROC.3={
    proc <- roc(y ~ y.hat, algorithm=3)
  })
  roc.curves <- rbind(data.frame(tp.fp, package="WeightedROC"),
                      perfDF(perf), procDF(proc))
  ggplot()+
    geom_path(aes(FPR, TPR, color=package, linetype=package),
              data=roc.curves, size=1)+
    coord_equal()
  ## WeightedROC can compute ROC curves for data sets with variable
  ## weights.
  y <- c(-1, -1, 1, 1, 1)
  w <- c(1, 1, 1, 4, 5)
  y.hat <- c(1, 2, 3, 1, 1)
  tp.fp <- WeightedROC(y.hat, y, w)
  ggplot()+
    geom_path(aes(FPR, TPR), data=tp.fp)+
    coord_equal()
})
