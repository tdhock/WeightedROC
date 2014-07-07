\name{WeightedROC}
\alias{WeightedROC}
\title{WeightedROC}
\description{Compute a weighted ROC curve.}
\usage{WeightedROC(guess, label, weight = rep(1, length(label)))}
\arguments{
  \item{guess}{Numeric vector of scores.}
  \item{label}{True positive/negative labels. A factor with 2 unique values, or
integer/numeric with values all in {0=negative,1=positive} or
{1=negative,2=positive} or {-1=negative,1=positive}.}
  \item{weight}{Positive weights, by default 1.}
}

\value{data.frame with the true positive rate (TPR) and false positive
rate (FPR).}

\author{Toby Dylan Hocking}




\examples{
library(ROCR)
library(microbenchmark)
data(ROCR.simple)
microbenchmark(ROCR={
  pred <- with(ROCR.simple, prediction(predictions, labels))
  perf <- performance(pred, "tpr", "fpr")
}, WeightedROC={
  tp.fp <- with(ROCR.simple, WeightedROC(predictions, labels))
})
perfDF <- function(p){
  data.frame(FPR=p@x.values[[1]], TPR=p@y.values[[1]],
             package="ROCR")
}
roc <- rbind(data.frame(tp.fp, package="WeightedROC"), perfDF(perf))
library(ggplot2)
ggplot()+
  geom_path(aes(FPR, TPR, color=package, linetype=package), data=roc)+
  coord_equal()
## Compute a small ROC curve with 1 tie to show the diagonal.
y <- c(-1, -1, 1, 1)
y.hat <- c(1, 2, 3, 1)
microbenchmark(ROCR={
  pred <- prediction(y.hat, y)
  perf <- performance(pred, "tpr", "fpr")
}, WeightedROC={
  tp.fp <- WeightedROC(y.hat, y)
})
roc <- rbind(data.frame(tp.fp, package="WeightedROC"), perfDF(perf))
ggplot()+
  geom_path(aes(FPR, TPR, color=package, linetype=package), data=roc)+
  coord_equal()
## Variable weights.
y <- c(-1, -1, 1, 1, 1)
w <- c(1, 1, 1, 4, 5)
y.hat <- c(1, 2, 3, 1, 1)
tp.fp <- WeightedROC(y.hat, y, w)
ggplot()+
  geom_path(aes(FPR, TPR), data=tp.fp)
}
