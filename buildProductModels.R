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
library(pROC)
library(corrplot)
library(scales)

source("metrics.R")

set.seed(12345)
samplingRatio <- 0.2   # Sample ratio of full development set
validationRatio <- 0.3 # Split training salmple into Train and Validate

data_folder <- "data"
# data_folder <- "data-unittest"

# Read data
data_colClasses = list(character=c("ult_fec_cli_1t","indrel_1mes","conyuemp"))
data_dateFlds = c("fecha_dato","fecha_alta","ult_fec_cli_1t")

train <- fread(paste(data_folder,"train_ver2.csv",sep="/"), 
               colClasses = data_colClasses)

productFlds <- names(train)[grepl("^ind_.*ult1$",names(train))] # products purchased

# Sample to speed up development, then split into Train/Validate

allPersonz <- unique(train$ncodpers)
cat("Before dev sampling train size:", dim(train), "unique persons:",length(allPersonz),fill = T)
if (nrow(train) > 1000000) {
  train <- train[ncodpers %in% sample(allPersonz, trunc(samplingRatio*length(allPersonz))),]
}
allPersonz <- unique(train$ncodpers)
cat("After dev sampling train size:", dim(train), "unique persons:",length(allPersonz),fill = T)
rm(allPersonz)

test <- fread(paste(data_folder,"test_ver2.csv",sep="/"), 
              colClasses = data_colClasses)

# Concatenate both into one set makes it a lot easier to do all the 
# data processing and generation of derived fields.
all <- bind_rows(train, test)
all$dataset <- c(sample(c("Train","Validate"),nrow(train),replace=T,prob=c(1-validationRatio,validationRatio)), rep("Test", nrow(test)))
rm(list=c("train","test"))
print(ggplot(group_by(all, dataset) %>% summarise(n = n(), pct = n()/nrow(all)), 
       aes(dataset,pct,label=n,fill=dataset))+
  geom_bar(stat="identity")+geom_text()+scale_y_continuous(labels=percent)+ggtitle("Data set splits"))

# Quick data summaries

interactionFreqs <- group_by(all, ncodpers, dataset) %>% summarise(n=n())
print(ggplot(interactionFreqs, aes(n, fill=dataset))+geom_bar()+
        ggtitle("Number of Records per Person"))

# Dates

for(f in intersect(data_dateFlds, names(all))) { 
  all[[f]] <- fasttime::fastPOSIXct(all[[f]])
  all[[paste("xf.year",f,sep=".")]] <- year(all[[f]])
  all[[paste("xf.month",f,sep=".")]] <- month(all[[f]])
}
all$xf.monthnr <- all$xf.year.fecha_dato*12 + all$xf.month.fecha_dato - 1
# TODO also date differences between all pairs of date fields
# use is.POSIXct(all$fecha_alta) to traverse the date fields

# Categorical

for (f in names(all)[sapply(all, class) == "character"]) {
  cat("Transforming categorical field",f,fill=T)
  all[[f]] <- factor(all[[f]])

  # Count of factor levels
  grp <- data.table(group_by_(all, f) %>% summarise(n = n()))
  names(grp)[2] <- paste("xf.n",f,sep=".")
  setkeyv(grp, f)
  setkeyv(all, f)
  all <- all[grp]
}

# Add last month products as features to this month

setkey(all, ncodpers, xf.monthnr)
lastMonthProducts <- all[,c("ncodpers","xf.monthnr",productFlds),with=F]
lastMonthProducts$xf.monthnr <- lastMonthProducts$xf.monthnr+1
names(lastMonthProducts)[2+seq(length(productFlds))] <- paste("xf.prev",productFlds,sep=".")
setkey(lastMonthProducts, ncodpers, xf.monthnr)
all <- lastMonthProducts[all]
setkey(all, ncodpers, xf.monthnr)

# Set outcome as the products newly purchased in the current month

for (f in productFlds) {
  all[[paste("products",f,sep=".")]] <- 
    as.numeric(ifelse(is.na(all[[f]]) | is.na(all[[paste("xf.prev",f,sep=".")]]), 
                      NA, 
                      (all[[f]] == 1) & (all[[paste("xf.prev",f,sep=".")]] == 0)))
}

all$xf.prev.products.count <- rowSums(all[,paste("xf.prev",productFlds,sep="."),with=F])
all$products.newcount <- rowSums(all[,paste("products",productFlds,sep="."),with=F])

print(ggplot(filter(all, !is.na(products.newcount)), 
             aes(products.newcount, fill=dataset))+
        geom_histogram(binwidth = 1)+
        scale_y_log10()+
        ggtitle("Distribution of new product count"))

# summaries for data check

for (ds in c("Test","Train")) {
  cat("Size of",ds,":",nrow(filter(all, dataset==ds)),fill=T)
  cat("Missing prev products in",ds,":",
      sum(is.na(filter(all, dataset==ds) %>% select(xf.prev.products.count))),fill=T)
  cat("Missing outcomes in",ds,":",
      sum(is.na(filter(all, dataset==ds) %>% select(products.newcount))),fill=T)
  cat("First month by person for", ds, ":",
      nrow(group_by(filter(all, dataset==ds), ncodpers) %>% summarise(firstMonth = min(xf.monthnr))),fill=T)
}

# More derived features

# TODO!

# Build Models
# Create a model for each outcome individually

outcomeCols <- paste("products",productFlds,sep=".")

trainRowz <- which(all$dataset == "Train" & !is.na(all$products.newcount))
validateRowz <- which(all$dataset == "Validate" & !is.na(all$products.newcount))
testRowz <- which(all$dataset == "Test")
modelTrainFlds <- which(!startsWith(names(all), "products.") & !names(all) %in% productFlds)

outcomeColsValid <- 
  sapply(outcomeCols, function(fld) {sum(!is.na(unique(all[[fld]])))} > 1)

# go all out see https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html
corrMatrix <- cor(as.matrix((all[trainRowz, outcomeCols[outcomeColsValid], with=F])))
corrplot(corrMatrix, type="upper",order ="AOE")
corrplot(corrMatrix, method="number",type="upper",order ="AOE")

trainMatrix <- as.matrix((all[trainRowz, modelTrainFlds, with=F])[,lapply(.SD,as.numeric)]) # factors/syms --> numeric
validateMatrix <- as.matrix((all[validateRowz, modelTrainFlds, with=F])[,lapply(.SD,as.numeric)]) # factors/syms --> numeric
testMatrix <- as.matrix((all[testRowz, modelTrainFlds, with=F])[,lapply(.SD,as.numeric)]) # factors/syms --> numeric

param <- list("objective" = "binary:logistic",
              max.depth = 5,
              eta = 0.01,
              "eval_metric" = "auc") # make sure to maximize!
cv.nround <- 5
cv.nfold <- 3
nround = 5

results <- data.frame(ncodpers=all[dataset=="Test", ncodpers])

for (col in outcomeCols) {
  cat(which(col==outcomeCols),"/",length(outcomeCols),":",col,fill=T)
  y <- as.integer(all[[col]][trainRowz])
  yrate <- sum(y)/length(y)
  results[[col]] <- FALSE
  if (length(unique(y)) < 2) {
    cat("Skipping",col,": too few distinct values,",length(unique(y)),fill=T)
    next
  }
  if (yrate < 5e-4) {
    cat("Skipping",col,": too low rate,",yrate,fill=T)
    next
  }
  # bst.cv = xgb.cv(param=param, data = trainMatrix, missing=NaN,
  #                 label = y, 
  #                 nfold = cv.nfold, nrounds = cv.nround, maximize=T)
  bst = xgboost(params=param, data = trainMatrix, missing=NaN,
                label = as.integer(all[[col]][trainRowz]), 
                nrounds=nround, maximize=T)
  
  # Compute & plot feature importance matrix & summary tree
  importance_matrix <- xgb.importance(dimnames(trainMatrix)[[2]], model = bst)
  print(xgb.plot.importance(importance_matrix)+ggtitle(paste("Feature imp for",col)))

  # xgb.plot.tree(feature_names = dimnames(trainMatrix)[[2]], model = bst, n_first_tree = 2)
  # xgb.plot.multi.trees(model = bst, feature_names = dimnames(trainMatrix)[[2]], features.keep = 3)
  
  # test on train/validation set, get some accuracy idea
  # TODO: keep part of the "Train" set as "Validation" and use that to double check things
  predictions_train <- predict(bst, trainMatrix, missing=NaN)
  truth_train <- as.integer(all[[col]][trainRowz])
  cat("AUC on Train:",as.numeric(auc(truth_train, predictions_train)),fill=T)
  
  predictions_validate <- predict(bst, validateMatrix, missing=NaN)
  truth_validate <- as.integer(all[[col]][validateRowz])
  cat("AUC on Validate:",as.numeric(auc(truth_validate, predictions_validate)),fill=T)
  
  # find optimal threshold to binarize the predictions
  delta <- 0.5
  th <- 0.5
  target <- mean(truth_train)
  for (itr in seq(20)) {
    delta <- delta/2
    preds_binarized <- (predictions_train > th)
    current <- mean(preds_binarized)
    # cat("Iteration",itr,"threshold",th,"target",target,"current",current,"delta",delta,fill=T)
    if (current < target) {
      th <- th - delta
    } else {
      th <- th + delta
    }
  }
  predictions_train_binary <- (predictions_train > th)
  cat("AUC on Train w threshold",th,":",
      as.numeric(auc(truth_train, as.numeric(predictions_train_binary))),fill=T)

  predictions <- predict(bst, testMatrix, missing=NaN)
  print(summary(predictions))
  results[[col]] <- (predictions > th)
}

# TODO keep track of the AUCs in a plot
# all fields, before and after binarization
# on Train and Validate

# TODO look at MAPS7 evalation
# Forum: if you want to maximize your score you should always predict seven products
# so no thresholding, only ordering?
# precision = TP / (TP + FP)



# Concatenate names for selected products
resultBinary <- select(results, -ncodpers)
names(resultBinary) <- productFlds
results$added_products <- apply(resultBinary, 1, function(row) { paste(productFlds[as.logical(row)],collapse=" ")})

write.csv(select(results, ncodpers, added_products), "data/submission.csv",row.names = F, quote=F)

# subm <- fread("data/sample_submission.csv")
# subm2 <- fread("data/submission.csv")

