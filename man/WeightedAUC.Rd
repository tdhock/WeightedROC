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
library(microbenchmark)
data(ROCR.simple)
microbenchmark(ROCR={
  pred <- with(ROCR.simple, prediction(predictions, labels))
  rocr <- performance(pred, "auc")@y.values[[1]]
}, WeightedROC={
  tp.fp <- with(ROCR.simple, WeightedROC(predictions, labels))
  wroc <- WeightedAUC(tp.fp)
})
rbind(ROCR=rocr, WeightedROC=wroc)
}
