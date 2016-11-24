
# given a correctness vector it gets easier
# F/T for element at that position in the predicted values (ordered) to match the truth
# https://www.kaggle.com/c/FacebookRecruiting/forums/t/2002/alternate-explanation-of-mean-average-precision

k <- 10
correctness <- c(F,T,T,F,T,F,F,F,T,F)
mapk <- mean((cumsum(correctness)/seq(k)) [correctness])


truth <- c(1, 4)
pred  <- c(3, 1, 2, 4)
correctness <- c(F, T, F, T)

# labels 1  2  3  4
# truth  1  0  0  1
# preds  2  3  1  4 (ranks)
# -->    2  0  0  4 (multiply ranks with truth)
# -->    0  2  0  4 (?)
