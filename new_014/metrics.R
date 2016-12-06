# Calculates the mean average precision given a vector of items (the truth) and
# a vector of rank predictions.

# TODO double check this stuff
# See below, which probably directly works on the preds from XGBoost
# so can be used as a metric!
# library(Metrics)
# map5 <- function(preds, dtrain) {
#   labels <- as.list(getinfo(dtrain,"label"))
#   num.class = 100
#   pred <- matrix(preds, nrow = num.class)
#   top <- t(apply(pred, 2, function(y) order(y)[num.class:(num.class-4)]-1))
#   top <- split(top, 1:NROW(top))
#   
#   map <- mapk(5, labels, top)
#   return(list(metric = "map5", value = map))
# }
# Then set eval_metric=map5 in your params.
# or
# map5 = function(preds, dtrain) {
#   labels = getinfo(dtrain, 'label')
#   preds = t(matrix(preds, ncol = length(labels)))
#   preds = t(apply(preds, 1, order, decreasing = T))[, 1:5] - 1
#   succ = (preds == labels)
#   w = 1 / (1:5)
#   map5 = mean(succ %*% w)
#   return (list(metric = 'map5', value = map5))
# }
# https://www.kaggle.com/c/expedia-hotel-recommendations/forums/t/20556/map5-function-or-eval-metric?forumMessageId=120974

# 'truth' is vector of T/F's indicating items presence
# 'predranks' is vector of ranks of each item in the prediction
# 'k' is size of top ranking to consider - defaults to all
mapk <- function(truth, predranks, k=length(predranks)) {
  if (!is.logical(truth)) stop("truth should be boolean vector")
  idxs <- seq(k)
  
  # cat("Truth: ", letters[seq(length(truth))][truth], fill=T)
  # cat("Preds: ", letters[1:max(predranks)][predranks], fill=T) 
  
  # map_nom <- (truth[predranks]*cumsum(truth[predranks]))[1:k]
  # map_nom <- (cumsum(truth[predranks]))[1:k]
  topK <- truth[names(predranks)[predranks <= k]]
  map_nom <- topK*cumsum(topK)
  
  # cat("MAP nominator:", map_nom, fill=T)
  # cat("MAP denominator:", idxs, fill=T)
  
  result <- mean(map_nom/idxs, na.rm = T)
  # cat("MAP = mean of",map_nom/idxs, "=",result,fill=T)
  
  return(result)
}

# # find optimal threshold to binarize the predictions
# delta <- 0.5
# th <- 0.5
# target <- mean(truth_train)
# for (itr in seq(20)) {
#   delta <- delta/2
#   preds_binarized <- (predictions_train > th)
#   current <- mean(preds_binarized)
#   # cat("Iteration",itr,"threshold",th,"target",target,"current",current,"delta",delta,fill=T)
#   if (current < target) {
#     th <- th - delta
#   } else {
#     th <- th + delta
#   }
# }
# predictions_train_binary <- (predictions_train > th)
# cat("AUC on Train w threshold",th,":",
#     as.numeric(auc(truth_train, as.numeric(predictions_train_binary))),fill=T)


# mapk(c(F,  T,  F,  T), 
#      c(1,  2,  4,  3), 3)
# 
# # Example from https://www.kaggle.com/c/FacebookRecruiting/forums/t/2002/alternate-explanation-of-mean-average-precision
# mapk(c(F, F, F, F, F, F, F, F, F, F, T, T, T, T, T, T, T, T, T, T),
#      c(2, 11, 12, 3, 13, 4, 5, 6, 14, 7))
# 
