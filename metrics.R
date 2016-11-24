
# given a correctness vector it gets easier
# https://www.kaggle.com/c/FacebookRecruiting/forums/t/2002/alternate-explanation-of-mean-average-precision

k <- 10
correctness <- c(F,T,T,F,T,F,F,F,T,F)
mapk <- mean((cumsum(correctness)/seq(k)) [correctness])


