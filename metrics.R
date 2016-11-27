# Calculates the mean average precision given a vector of items (the truth) and
# a vector of rank predictions.
#
# 'truth' is vector of T/F's indicating items presence
# 'predranks' is vector of ranks of each item in the prediction
# 'k' is size of top ranking to consider - defaults to all
mapk <- function(truth, predranks, k=length(predranks)) {
  if (!is.logical(truth)) stop("truth should be boolean vector")
  idxs <- seq(k)
  
  # cat("Truth: ", letters[seq(length(truth))][truth], fill=T)
  # cat("Preds: ", letters[1:max(predranks)][predranks], fill=T) 
  
  map_nom <- (truth[predranks]*cumsum(truth[predranks]))[1:k]

  # cat("MAP nominator:", map_nom, fill=T)
  # cat("MAP denominator:", idxs, fill=T)
  
  result <- mean(map_nom/idxs)
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
