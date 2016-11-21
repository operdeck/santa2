library(data.table)
library(fasttime)
library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)

# require(devtools)
# install_github('tqchen/xgboost',subdir='R-package')

# install.packages("drat", repos="https://cran.rstudio.com")
# drat:::addRepo("dmlc")
# install.packages("xgboost", repos="http://dmlc.ml/drat/", type = "source")

require(xgboost)
library(Ckmeans.1d.dp)
library(DiagrammeR)

# Read data

data_colClasses = list(character=c("ult_fec_cli_1t","indrel_1mes","conyuemp"))
data_dateFlds = c("fecha_dato","fecha_alta","ult_fec_cli_1t")
train <- fread("data/train_ver2.csv", colClasses = data_colClasses, stringsAsFactors = T)
for(f in data_dateFlds) { train[[f]] <- fasttime::fastPOSIXct(train[[f]])}
productFlds <- names(train)[25:48]

test <- fread("data/test_ver2.csv", colClasses = data_colClasses, stringsAsFactors = T)
for(f in data_dateFlds) { test[[f]] <- fasttime::fastPOSIXct(test[[f]])}

train$monthNr <- year(train$fecha_dato)*12+month(train$fecha_dato)-1
test$monthNr <- year(test$fecha_dato)*12+month(test$fecha_dato)-1

# train:
# [ t0 : a, b, c ==> cum-ind 1, cum-ind 2, cum-ind 3 ]
# [ t1 : a, b, c ==> cum-ind 1, cum-ind 2, cum-ind 3 ]
# ...
# [ tn : a, b, c ==> cum-ind 1, cum-ind 2, cum-ind 3 ]
#
# test:
# [ tn+1 : a, b, c ]

# Data summaries
interactionFreqs <- group_by(train, ncodpers, segmento) %>%
  summarise(n=n())
print(ggplot(interactionFreqs, aes(as.factor(n), fill=segmento))+geom_bar()+
        ggtitle("Number of Records per Person"))

# Sample for development
set.seed(12345)
cat("Before dev sampling", dim(train), fill = T)
train <- train[ncodpers %in% sample(train$ncodpers, 100000),]
cat("After dev sampling", dim(train), fill = T)

setkey(train, ncodpers, monthNr)
setkey(test, ncodpers, monthNr)

# Set outcome

# train:
# [ t0 : -- ]
# [ t1 : a, b, c + cum-ind 1 t0, cum-ind 2 t0, cum-ind 3 t0, other derived_features ==> ind 1, ind 2, ind 3 ]
# ...
# [ tn : a, b, c + cum-ind 1 tn-1, cum-ind 2 tn-1, cum-ind 3 tn-1, other derived_features ==> ind 1, ind 2, ind 3 ]
#
# test:
# [ tn+1 : a, b, c + cum-ind 1 tn, cum-ind 2 tn, cum-ind 3 tn, other derived_features ==> [Y] ]

# Lots of data.table mess to efficiently calculate things. Eventually,
# purchased_xxx = TRUE if xxx is 1 in the next month and 0 currently
# purchased_count = number of purchased_xxx that are true (for reporting/calibration)

# add last row of train as a feature set to test
lastMonthProducts <- train[monthNr == max(train$monthNr),c(productFlds,"ncodpers","monthNr"),with=F]
lastMonthProducts$monthNr <- lastMonthProducts$monthNr+1
names(lastMonthProducts) <- ifelse(startsWith(names(lastMonthProducts),"ind_"),
                                   paste("prev",names(lastMonthProducts),sep="."),
                                   names(lastMonthProducts))
setkeyv(lastMonthProducts, key(test))
test <- test[lastMonthProducts]

# derive new products flags
prevMonthProducts <- train[,c(productFlds,"ncodpers","monthNr"),with=F]
prevMonthProducts[, nextMonthNr := monthNr+1]
prevMonthProducts[, monthNr := NULL]
setkey(prevMonthProducts, ncodpers, nextMonthNr)
train <- train[prevMonthProducts,nomatch=0] # inner join, so first month will be out
for (col in productFlds) {
  train[[paste("prev",col,sep = ".")]] <- train[[paste("i",col,sep = ".")]]
  train[[paste("i",col,sep = ".")]] <- NULL # clumsy rename i. --> prev.
  train[[paste("purchased",col,sep = ".")]] <- train[[paste("prev",col,sep = ".")]]==0 & train[[col]]==1
  train[[col]] <- NULL 
}
train[["purchased.count"]] <- rowSums(select(train, starts_with("purchased.")))
print(ggplot(train, aes(purchased.count,fill=sexo))+geom_histogram(binwidth = 1)+
        scale_y_log10()+
        ggtitle("Distribution of new prods in next month"))

# Derived features

# TODO!

# Create a model for each outcome individually
outcomeCols <- names(train)[startsWith(names(train),"purchased_ind_")]

# outcomeCols <- c("purchased.ind_nomina_ult1")
trainRowz <- which(!is.na(train$purchased.count))
trainMatrix <- as.matrix((train[trainRowz, which(!startsWith(names(train), "purchased.")), with=F])
                         [,lapply(.SD,as.numeric)]) # factors/syms --> numeric
param <- list("objective" = "binary:logistic",
              "eval_metric" = "auc") # make sure to maximize!
cv.nround <- 5
cv.nfold <- 3
nround = 5

for (col in outcomeCols) {
  print(col)
  if (length(unique(as.integer(train[[col]][trainRowz]))) < 2) {
    next
  }
  bst.cv = xgb.cv(param=param, data = trainMatrix, missing=NaN,
                  label = as.integer(train[[col]][trainRowz]), 
                  nfold = cv.nfold, nrounds = cv.nround, maximize=T)
  bst = xgboost(param=param, data = trainMatrix, missing=NaN,
                label = as.integer(train[[col]][trainRowz]), 
                nrounds=nround, maximize=T)
  
  # Compute & plot feature importance matrix & summary tree
  importance_matrix <- xgb.importance(dimnames(trainMatrix)[[2]], model = bst)
  print(xgb.plot.importance(importance_matrix)+ggtitle(paste("Feature imp for",col)))

  # xgb.plot.tree(feature_names = dimnames(trainMatrix)[[2]], model = bst, n_first_tree = 2)
  # xgb.plot.multi.trees(model = bst, feature_names = dimnames(trainMatrix)[[2]], features.keep = 3)
}

# Goal
# From products 2015-01-28 01:00:00 - 2016-05 predict what they will have 1 month later

# Thoughts
# Train on 2015-01 - 2016-04 --> 2016-05, predict 2015-02 - 2016-05 --> 2016-06 ? or
# Train simply based on current month --> next month?
# Products are not independent. Encode as one big factor? Cols 25:48

# We could try independent predictions first
# Maybe massage the output to fit with the original

# this gets 12.220 different product combinations
# train <- unite(train, productsCombined, ind_ahor_fin_ult1:ind_recibo_ult1)

# For now maybe just predict next month?
# ind-xyz = f(status pref month), 47 models

#             p1 p1-added-next-month model
# 01-14 ABS   0  F                   0.5
# 02-14 ABS   0  T                   0.8
# 03-14 ABS   1  F                   0.7
# 04-14 ABS   1  F                   0.6
# 05-14 ABS   0  F                   0.3

# find which date has highest model output; compare that to a threshold

# Given some predictions for several prods
# p1  p2  p3
# 0.6 0.4 0.8
# which actual product combination is closest, given that not all are possible
# (0,  1,  1)
# (1,  0,  1) <<-- most likely combination, pick that one
# (1,  1,  0)


# outcomes
stop()
# Any help from standard R ts funcs?
# save a numeric vector containing 72 monthly observations
# from Jan 2009 to Dec 2014 as a time series object
myts <- ts(train$ind_deme_fin_ult1, start=c(2015, 1), end=c(2016, 5), frequency=12) 

# subset the time series (June 2014 to December 2014)
myts2 <- window(myts, start=c(2015, 6), end=c(2015, 12)) 

# plot series
plot(myts)

