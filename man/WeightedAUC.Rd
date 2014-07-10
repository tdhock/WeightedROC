\name{WeightedAUC}
\alias{WeightedAUC}
\title{WeightedAUC}
\description{Calculate the exact area under the ROC curve.}
\usage{WeightedAUC(tpr.fpr)}
\arguments{
  \item{tpr.fpr}{Output of WeightedROC: data.frame with the true positive rate
(TPR) and false positive rate (FPR).}
}

\value{Numeric scalar.}

\author{Toby Dylan Hocking}




\examples{
## Compute the AUC for this weighted data set.
y <- c(-1, -1, 1, 1, 1)
w <- c(1, 1, 1, 4, 5)
y.hat <- c(1, 2, 3, 1, 1)
tp.fp <- WeightedROC(y.hat, y, w)
wauc <- WeightedAUC(tp.fp)
wauc

## glmnet 1.9-5 (2013-8-1) also has a function called auc but it
## does not compute the correct AUC when there are ties in the
## predicted score (y.hat).
library(glmnet)
rbind(`glmnet::auc`=c(unequal.weights=glmnet::auc(y, y.hat, w),
        no.weights=glmnet::auc(y, y.hat),
        equal.weights=glmnet::auc(y, y.hat, rep(1, length(y)))),
      `WeightedROC::WeightedAUC`=c(wauc,
        WeightedAUC(WeightedROC(y.hat, y)),
        WeightedAUC(WeightedROC(y.hat, y, rep(1, length(y))))))          

## with no ties, glmnet::auc can give the correct answer. For the
## case with equal weights you must provide a vector of equal
## weights as an argument (if you leave the third argument of
## glmnet::auc missing, then it gives the wrong answer).
y <- c(-1, -1, 1, -1, 1)
y.hat <- c(1, 2, 3, 4, 5)
w <- c(1, 1, 1, 4, 5)
rbind(`glmnet::auc`=c(unequal.weights=glmnet::auc(y, y.hat, w),
        no.weights=glmnet::auc(y, y.hat),
        equal.weights=glmnet::auc(y, y.hat, rep(1, length(y)))),
      `WeightedROC::WeightedAUC`=c(WeightedAUC(WeightedROC(y.hat, y, w)),
        WeightedAUC(WeightedROC(y.hat, y)),
        WeightedAUC(WeightedROC(y.hat, y, rep(1, length(y))))))
        
library(ROCR)
library(pROC)
library(microbenchmark)
## For the un-weighted ROCR example data set, verify that our AUC is
## the same as that of ROCR/pROC/glmnet. Note that glmnet::auc is
## faster than WeightedROC::WeightedAUC because glmnet::auc it does
## not include the overhead of computing a data.frame(TPR, FPR) that
## could be plotted.
data(ROCR.simple)
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
})
rbind(WeightedROC=wroc, ROCR=rocr, pROC=proc, glmnet=gnet) #same

## For the un-weighted pROC example data set, verify that our AUC is
## the same as that of ROCR/pROC. WARNING: glmnet::auc does not work
## for these data since there are ties in the aSAH$s100b score.
data(aSAH)
table(aSAH$s100b)
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
    glmnet::auc(outcome, s100b, rep(1, length(s100b)))
  })
})
rbind(WeightedROC=wroc, ROCR=rocr, pROC=proc, glmnet=gnet)
}
