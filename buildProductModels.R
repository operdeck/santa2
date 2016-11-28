# Kaggle Santander 2 
# data prep: https://www.kaggle.com/apryor6/santander-product-recommendation/detailed-cleaning-visualization/comments

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
samplingRatio <- 0.4   # Sample ratio of full development set
validationRatio <- 0.1 # Split training salmple into Train and Validate

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
        ggtitle("Number of purchases per Person"))

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

# Remaining fields to numerics - not needed, data.matrix will do that
# for (f in setdiff(names(all)[sapply(all, class) != "numeric" & sapply(all, class) != "integer"], c("dataset"))) {
#   cat("Transforming factor field",f,fill=T)
#   all[[f]] <- as.numeric(all[[f]])
# }

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
  all[[paste("change",f,sep=".")]] <-
    # all[[f]] - all[[paste("xf.prev",f,sep=".")]]

  factor(all[[f]] - all[[paste("xf.prev",f,sep=".")]],
           levels=c(NA,0,1,-1),
           labels=c("Maintained","Added","Dropped"))
}

# TODO - consider added/maintained/dropped seperately for previous months
all$xf.prev.products.count <- rowSums(all[,paste("xf.prev",productFlds,sep="."),with=F])
# all$change.newcount <- rowSums(all[,paste("change",productFlds,sep="."),with=F])

# Plot of service changes by month
# see https://www.kaggle.com/apryor6/santander-product-recommendation/detailed-cleaning-visualization/comments
monthVar <- "xf.month.fecha_dato"
allPrdChanges <- NULL
for (f in productFlds) {
  tmp <- all[, .N,by=c(paste("change",f,sep="."),monthVar)]
  names(tmp)[1] <- "status"
  names(tmp)[2] <- "month"
  tmp$feature <- f
  if (is.null(allPrdChanges)) { 
    allPrdChanges <- tmp
  } else {
    allPrdChanges <- rbind(allPrdChanges, tmp)
  }
}
# add 0's for those that only have NA or Maintained
allPrdChanges <- spread(allPrdChanges, status, N, fill=0) %>% gather(status, N, -month,-feature)
print(ggplot(filter(allPrdChanges, !is.na(status) & status != "Maintained" & status != "<NA>"), 
       aes(y=N,x=factor(month.abb[month],levels=month.abb[seq(12,1,-1)])))+
  geom_bar(aes(fill=status),stat="identity")+
  facet_wrap(facets=~feature,ncol=6)+
  coord_flip() + ylab("Count") + xlab("") + ggtitle("Service Changes by Month") +
  scale_fill_manual(values=c("cyan","magenta"))+
  theme_dark() +
  theme(axis.text =element_text(size=8),
        axis.text.x = element_text(angle = 45, hjust = 1)))

print(group_by(allPrdChanges, 
              feature, status) %>% summarise(n=sum(N)) %>%
        spread(status, n) %>% arrange(Added))
     
# summaries for data check - TODO uitvlooien

for (ds in unique(all$dataset)) {
  cat("Size of",ds,":",nrow(filter(all, dataset==ds)),fill=T)
  cat("Missing prev products in",ds,":",
      sum(is.na(filter(all, dataset==ds) %>% select(xf.prev.products.count))),fill=T)
  # cat("Missing outcomes in",ds,":",
  #     sum(is.na(filter(all, dataset==ds) %>% select(change.newcount))),fill=T)
  cat("First month by person for", ds, ":",
      nrow(group_by(filter(all, dataset==ds), ncodpers) %>% summarise(firstMonth = min(xf.monthnr))),fill=T)
}

# TODO More derived features

# Build Models
# Create a model for each outcome individually

outcomeCols <- paste("change",productFlds,sep=".")
modelTrainFlds <- names(all)[which(!startsWith(names(all), "change.") & !names(all) %in% productFlds)]

# Correlation plot
# go all out see https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html
outcomeColsValid <-
  sapply(outcomeCols, function(fld) {sum(!is.na(unique(all[[fld]])))} > 1)
corrData <- data.matrix((all[,outcomeCols[outcomeColsValid], with=F]))
corrMatrix <- cor(corrData[complete.cases(corrData),])
corrplot(corrMatrix, method="number",type="upper",order ="AOE")

xgb.params <- list("objective" = "binary:logistic",
              max.depth = 5,
              eta = 0.01,
              "eval_metric" = "auc") # make sure to maximize!
# cv.nround <- 5
# cv.nfold <- 3
nround = 5
auc.train <- list()
auc.validate <- list()

for (col in outcomeCols) {
  cat(which(col==outcomeCols),"/",length(outcomeCols),":",col,fill=T)

  # TODO: fix up
  # trainRowz is the outcome rows that are not NA
  # trainRowz is simply the outcome rows that we want to consider
  # ie the valid ones of all[[col]]
  
  trainRowz <- which(all$dataset == "Train" & !is.na(all$change.newcount))
  validateRowz <- which(all$dataset == "Validate" & !is.na(all$change.newcount))
  testRowz <- which(all$dataset == "Test")
  
  
  trainMatrix <- xgb.DMatrix(data.matrix(all[trainRowz, modelTrainFlds, with=F]), 
                             missing=NaN, 
                             label=all[[col]][trainRowz])
  validateMatrix <- xgb.DMatrix(data.matrix(all[validateRowz, modelTrainFlds, with=F]), 
                                missing=NaN, 
                                label=all[[col]][validateRowz])
  testMatrix <- xgb.DMatrix(data.matrix(all[testRowz, modelTrainFlds, with=F]), 
                            missing=NaN)
  

  testResults <- data.frame(ncodpers=all[testRowz, ncodpers])
  validateResults <- data.frame(ncodpers=all[validateRowz, ncodpers, xf.monthnr])
  
  y <- as.integer(all[[col]][trainRowz])
  # yrate <- sum(y)/length(y)
  testResults[[col]] <- mean(y, na.rm = T)
  validateResults[[col]] <- mean(y, na.rm = T)
  auc.train[[col]] <- 0.50
  auc.validate[[col]] <- 0.50
  tryCatch({
    bst = xgb.train(params=xgb.params, data = trainMatrix, missing=NaN,
                    watchlist=list(train=trainMatrix, validate=validateMatrix),
                    label = as.integer(all[[col]][trainRowz]), 
                    nrounds=nround, 
                    maximize=T)
    
    # Compute & plot feature importance matrix & summary tree
    importance_matrix <- xgb.importance(modelTrainFlds, model = bst)
    print(xgb.plot.importance(importance_matrix)+ggtitle(paste("Feature imp for",col)))
  
    # xgb.plot.tree(feature_names = dimnames(trainMatrix)[[2]], model = bst, n_first_tree = 2)
    # xgb.plot.multi.trees(model = bst, feature_names = dimnames(trainMatrix)[[2]], features.keep = 3)
    
    # test on train/validation set, get some accuracy idea
    # TODO take from XGB results
    predictions_train <- predict(bst, trainMatrix, missing=NaN)
    truth_train <- as.integer(all[[col]][trainRowz])
    cat("AUC on Train:",as.numeric(auc(truth_train, predictions_train)),fill=T)
    auc.train[[col]] <- as.numeric(auc(truth_train, predictions_train))
    
    predictions_validate <- predict(bst, validateMatrix, missing=NaN)
    truth_validate <- as.integer(all[[col]][validateRowz])
    cat("AUC on Validate:",as.numeric(auc(truth_validate, predictions_validate)),fill=T)
    auc.validate[[col]] <- as.numeric(auc(truth_validate, predictions_validate))
    
    predictions <- predict(bst, testMatrix, missing=NaN)
    print(summary(predictions))
    testResults[[col]] <- predictions
  
    predictions <- predict(bst, validateMatrix, missing=NaN)
    validateResults[[col]] <- predictions
  }, warning = function(msg) {
    print(msg)
  }, error = function(msg) {
    print(paste("XGB error:", msg))
  }
  )
}

# Write results with concatenates names for selected products
resultBinary <- select(testResults, -ncodpers)
names(resultBinary) <- productFlds
testResults$added_products <- apply(resultBinary, 1, function(row) { paste(productFlds[as.logical(row)],collapse=" ")})
write.csv(select(testResults, ncodpers, added_products), "data/submission.csv",row.names = F, quote=F)

# AUC plot for individual fields
aucplot <- as.data.frame(t(rbind(as.data.frame(auc.train), 
                                 as.data.frame(auc.validate))))
names(aucplot) <- c("train","validate")
aucplot$field <- rownames(aucplot)
aucplot <- gather(aucplot, dataset, auc, -field)
print(ggplot(aucplot, aes(field, auc, fill=dataset))+
  geom_bar(stat="identity",position = "dodge")+coord_flip())

# Calculate score on validation set - TODO get this much faster
avgprecision <- 0
for (i in 1:nrow(validateMatrix)) {
  if (i %% 1000 == 0) { 
    print(i) 
    print(avgprecision/i)
  }
  truth <- all[validateRowz[i], outcomeCols, with=F]
  predranks <- rank(-validateResults[i, outcomeCols], ties.method = "first") # or just - values
  avgprecision <- avgprecision + mapk(as.logical(truth), predranks, 7)
}
avgprecision <- avgprecision/nrow(validateMatrix)
cat("Average mean precision on validation set:",avgprecision,fill=T)



