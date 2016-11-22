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

data_folder <- "data"
data_folder <- "data-unittest"

data_colClasses = list(character=c("ult_fec_cli_1t","indrel_1mes","conyuemp"))
data_dateFlds = c("fecha_dato","fecha_alta","ult_fec_cli_1t")

train <- fread(paste(data_folder,"train_ver2.csv",sep="/"), 
               colClasses = data_colClasses)

# Sample to speed up development
set.seed(12345)
cat("Before dev sampling", dim(train), fill = T)
if (nrow(train) > 1000000) {
  train <- train[ncodpers %in% sample(train$ncodpers, trunc(0.10*nrow(train))),]
}
cat("After dev sampling", dim(train), fill = T)

productFlds <- names(train)[grepl("^ind_.*ult1$",names(train))] # products purchased

test <- fread(paste(data_folder,"test_ver2.csv",sep="/"), 
              colClasses = data_colClasses)

# Concatenate both into one set but keep the rownumbers. Concatenating both
# makes it a lot easier to do all the data processing and generation of derived fields.
trainRowz <- seq(nrow(train))
testRowz <- max(trainRowz)+seq(nrow(test))
all <- bind_rows(train, test)
all$dataset <- c(rep("Train", length(trainRowz)), rep("Test", length(testRowz)))
rm(list=c("train","test"))

for(f in intersect(data_dateFlds, names(all))) { 
  all[[f]] <- fasttime::fastPOSIXct(all[[f]])
  all[[paste("xf.year",f,sep=".")]] <- year(all[[f]])
  all[[paste("xf.month",f,sep=".")]] <- month(all[[f]])
}

all$xf.monthnr <- all$xf.year.fecha_dato*12 + all$xf.month.fecha_dato - 1

for (f in names(all)[sapply(all, class) == "character"]) {
  all[[f]] <- factor(all[[f]])
}

# Quick data summaries

interactionFreqs <- group_by(all, ncodpers, segmento) %>%
  summarise(n=n())
print(ggplot(interactionFreqs, aes(as.factor(n), fill=segmento))+geom_bar()+
        ggtitle("Number of Records per Person"))

setkey(all, ncodpers, xf.monthnr)

# Add last month products as features to this month
lastMonthProducts <- all[,c("ncodpers","xf.monthnr",productFlds),with=F]
lastMonthProducts$xf.monthnr <- lastMonthProducts$xf.monthnr+1
names(lastMonthProducts)[2+seq(length(productFlds))] <- paste("xf.prev",productFlds,sep=".")
setkey(lastMonthProducts, ncodpers, xf.monthnr)

all <- lastMonthProducts[all]

# Set outcome

for (f in productFlds) {
  all[[paste("products",f,sep=".")]] <- 
    ifelse(is.na(all[[f]]) | is.na(all[[paste("xf.prev",f,sep=".")]]), NA, (all[[f]] == 1) & (all[[paste("xf.prev",f,sep=".")]] == 0))

}

all$xf.prev.products.count <- rowSums(all[,paste("xf.prev",productFlds,sep="."),with=F])
all$xf.products.newcount <- rowSums(all[,paste("products",productFlds,sep="."),with=F])

print(ggplot(filter(all, !is.na(xf.products.newcount)), 
             aes(xf.products.newcount,fill=dataset))+
        geom_histogram(binwidth = 1)+
        scale_y_log10()+
        ggtitle("Distribution of new prods in next month"))

stop()

# More derived features

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

