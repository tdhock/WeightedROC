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
library(ROCR)
library(pROC)
library(microbenchmark)
## For the un-weighted ROCR example data set, verify that our AUC is
## the same as that of ROCR/pROC.
data(ROCR.simple)
microbenchmark(WeightedROC={
  tp.fp <- with(ROCR.simple, WeightedROC(predictions, labels))
  wroc <- WeightedAUC(tp.fp)
}, ROCR={
  pred <- with(ROCR.simple, prediction(predictions, labels))
  rocr <- performance(pred, "auc")@y.values[[1]]
}, pROC={
  proc <- auc(labels ~ predictions, ROCR.simple)
})
rbind(WeightedROC=wroc, ROCR=rocr, pROC=proc)
## For the un-weighted pROC example data set, verify that our AUC is
## the same as that of ROCR/pROC.
data(aSAH)
microbenchmark(WeightedROC={
  tp.fp <- with(aSAH, WeightedROC(s100b, outcome))
  wroc <- WeightedAUC(tp.fp)
}, ROCR={
  pred <- with(aSAH, prediction(s100b, outcome))
  rocr <- performance(pred, "auc")@y.values[[1]]
}, pROC={
  proc <- auc(outcome ~ s100b, aSAH)
})
rbind(WeightedROC=wroc, ROCR=rocr, pROC=proc)
## Compute the AUC for this weighted data set.
y <- c(-1, -1, 1, 1, 1)
w <- c(1, 1, 1, 4, 5)
y.hat <- c(1, 2, 3, 1, 1)
tp.fp <- WeightedROC(y.hat, y, w)
WeightedAUC(tp.fp)
}
