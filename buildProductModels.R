# Kaggle Santander 2 
# data prep: https://www.kaggle.com/apryor6/santander-product-recommendation/detailed-cleaning-visualization/comments

# data is cplx https://www.kaggle.com/sudalairajkumar/santander-product-recommendation/when-less-is-more/code
# https://www.kaggle.com/c/santander-product-recommendation/forums/t/25579/when-less-is-more
# https://www.kaggle.com/alexeylive/santander-product-recommendation/june-2015-customers/run/468128

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

# with 0.4 sample 0.0044671 vs 0.018294 before on 0.2 - is this just random?

set.seed(12345)
samplingRatio <- 0.4   # Sample ratio of full development set
validationRatio <- 0.1 # Split training sample into Train and Validate

data_folder <- "data"
# data_folder <- "data-unittest"

# Read data
data_colClasses = list(character=c("ult_fec_cli_1t","indrel_1mes","conyuemp"))
data_dateFlds = c("fecha_dato","fecha_alta","ult_fec_cli_1t")
trainDates <- c('2015-05-28', '2015-06-28')
train <- fread(paste(data_folder,"train_ver2.csv",sep="/"), 
               colClasses = data_colClasses)
train <- train[fecha_dato %in% trainDates,]
# only keep rows that have persons at both dates
# probably not... logic is different...
# target D --> N months before
# same for test set
train[, nMonth := rep(.N,.N) ,by=ncodpers]
train <- train[nMonth == length(trainDates),]
train[,nMonth := NULL]

productFlds <- names(train)[grepl("^ind_.*ult1$",names(train))] # products purchased

allPersonz <- unique(train$ncodpers)
cat("Sampling train size:", dim(train), "; unique persons:",length(allPersonz),fill = T)

test <- fread(paste(data_folder,"test_ver2.csv",sep="/"), # fecha_dato is "2016-06-28" for all
              colClasses = data_colClasses)

# Concatenate both into one set makes it a lot easier to do all the 
# data processing and generation of derived fields.
# train/val not done properly!
sampling <- data.table(ncodpers = allPersonz,
                       dataset = sample(c("Train","Validate"),length(allPersonz),replace=T,prob=c(1-validationRatio,validationRatio)))
setkey(train, ncodpers)
setkey(sampling, ncodpers)
train <- train[sampling]
test$dataset <- "Test"
all <- bind_rows(train, test)

rm(list=c("train","test"))
print(ggplot(group_by(all, dataset) %>% summarise(n = n(), pct = n()/nrow(all)), 
       aes(dataset,pct,label=n,fill=dataset))+
  geom_bar(stat="identity")+geom_text()+scale_y_continuous(labels=percent)+ggtitle("Data set splits"))

# Quick data summaries

interactionFreqs <- group_by(all, ncodpers, dataset) %>% summarise(n=n())
print(ggplot(interactionFreqs, aes(n, fill=dataset))+geom_bar()+
        ggtitle("How often the same persons occur"))

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
lastMonthProducts$xf.monthnr <- lastMonthProducts$xf.monthnr+1 # is really target month nr
names(lastMonthProducts)[2+seq(length(productFlds))] <- paste("xf.prev",productFlds,sep=".")
setkey(lastMonthProducts, ncodpers, xf.monthnr)
all <- lastMonthProducts[all]
setkey(all, ncodpers, xf.monthnr)

#table(all$xf.monthnr)

# Set outcome as the products newly purchased in the current month
for (f in productFlds) {
  all[[paste("change",f,sep=".")]] <-
    all[[f]] - all[[paste("xf.prev",f,sep=".")]]
  # factor(all[[f]] - all[[paste("xf.prev",f,sep=".")]],
  #          levels=c(NA,0,1,-1),
  #          labels=c("Maintained","Added","Dropped"))
}

allMelted <- melt(all, 
                  id.vars = setdiff(names(all),paste("change",productFlds,sep=".")),
                  measure.vars = paste("change",productFlds,sep="."),
                  variable.name = "product",
                  value.name = "action"
                  )
allMelted <- allMelted[action == 1 | dataset == "Test",] # all NA in test set
allMelted[,action := NULL]
cat("Unique persons in train set that made a purchase:",length(unique(allMelted[dataset=="Train",ncodpers])),fill=T)

# TODO - consider added/maintained/dropped seperately for previous months
#all$xf.prev.products.count <- rowSums(all[,paste("xf.prev",productFlds,sep="."),with=F])
# all$change.newcount <- rowSums(all[,paste("change",productFlds,sep="."),with=F])

# Plot of service changes by month
# see https://www.kaggle.com/apryor6/santander-product-recommendation/detailed-cleaning-visualization/comments
# monthVar <- "xf.month.fecha_dato"
# allPrdChanges <- NULL
# for (f in productFlds) {
#   tmp <- all[, .N,by=c(paste("change",f,sep="."),monthVar)]
#   names(tmp)[1] <- "status"
#   names(tmp)[2] <- "month"
#   tmp$feature <- f
#   if (is.null(allPrdChanges)) { 
#     allPrdChanges <- tmp
#   } else {
#     allPrdChanges <- rbind(allPrdChanges, tmp)
#   }
# }
# # add 0's for those that only have NA or Maintained
# allPrdChanges <- spread(allPrdChanges, status, N, fill=0) %>% gather(status, N, -month,-feature)
# print(ggplot(filter(allPrdChanges, !is.na(status) & status != "Maintained" & status != "<NA>"), 
#        aes(y=N,x=factor(month.abb[month],levels=month.abb[seq(12,1,-1)])))+
#   geom_bar(aes(fill=status),stat="identity")+
#   facet_wrap(facets=~feature,ncol=6)+
#   coord_flip() + ylab("Count") + xlab("") + ggtitle("Service Changes by Month") +
#   scale_fill_manual(values=c("cyan","magenta"))+
#   theme_dark() +
#   theme(axis.text =element_text(size=8),
#         axis.text.x = element_text(angle = 45, hjust = 1)))
# 
# print(group_by(allPrdChanges, 
#               feature, status) %>% summarise(n=sum(N)) %>%
#         spread(status, n) %>% arrange(Added))
     
# summaries for data check - TODO uitvlooien

# for (ds in unique(all$dataset)) {
#   cat("Size of",ds,":",nrow(filter(all, dataset==ds)),fill=T)
#   cat("Missing prev products in",ds,":",
#       sum(is.na(filter(all, dataset==ds) %>% select(xf.prev.products.count))),fill=T)
#   # cat("Missing outcomes in",ds,":",
#   #     sum(is.na(filter(all, dataset==ds) %>% select(change.newcount))),fill=T)
#   cat("First month by person for", ds, ":",
#       nrow(group_by(filter(all, dataset==ds), ncodpers) %>% summarise(firstMonth = min(xf.monthnr))),fill=T)
# }

# TODO More derived features

# Build multiclass model
xgb.params <- list("objective" = "multi:softprob",
                   max.depth = 5,
                   num_class = 1+length(productFlds), # factor starts at 1
                   eta = 0.01)

modelTrainFlds <- names(allMelted)[which(!names(allMelted) %in% c("product",productFlds))]
trainMatrix <- xgb.DMatrix(data.matrix(allMelted[dataset == "Train", modelTrainFlds, with=F]), 
                           missing=NaN, 
                           label=as.integer(allMelted$product[allMelted$dataset == "Train"]))
validateMatrix <- xgb.DMatrix(data.matrix(allMelted[dataset == "Validate", modelTrainFlds, with=F]), 
                           missing=NaN, 
                           label=as.integer(allMelted$product[allMelted$dataset == "Validate"]))

bst = xgb.train(params=xgb.params, data = trainMatrix, missing=NaN,
                #watchlist=list(train=trainMatrix),
                watchlist=list(train=trainMatrix, validate=validateMatrix),
                #label = as.integer(all[[col]][trainRowz] == "Added"), 
                nrounds=10, 
                maximize=F)

# Compute & plot feature importance matrix & summary tree
importance_matrix <- xgb.importance(modelTrainFlds, model = bst)
print(xgb.plot.importance(importance_matrix))

# xgb.plot.tree(feature_names = dimnames(trainMatrix)[[2]], model = bst, n_first_tree = 2)
# xgb.plot.multi.trees(model = bst, feature_names = dimnames(trainMatrix)[[2]], features.keep = 3)

stop("Now apply somehow")
stop("Train needs prior month as well - for prev.ind, perhaps more")

# Create a model for each outcome individually

outcomeCols <- paste("change",productFlds,sep=".")

# Correlation plot
# go all out see https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html
# outcomeColsValid <-
#   sapply(outcomeCols, function(fld) {sum(!is.na(unique(all[[fld]])))} > 1)
# corrData <- data.matrix((all[,outcomeCols[outcomeColsValid], with=F]))
# corrMatrix <- cor(corrData[complete.cases(corrData),])
# corrplot(corrMatrix, method="number",type="upper",order ="AOE")

testRowz <- which(all$dataset == "Test")
testMatrix <- xgb.DMatrix(data.matrix(all[testRowz, modelTrainFlds, with=F]), 
                          missing=NaN)
testResults <- data.frame(ncodpers=all[testRowz, ncodpers])

# For validation, rows with all outcomes defined
validateRowz <- which(all$dataset == "Validate" & complete.cases(all[, outcomeCols, with=F]))
validateResults <- data.frame(ncodpers=all[validateRowz, ncodpers, xf.monthnr])

xgb.params <- list("objective" = "binary:logistic",
              max.depth = 5,
              eta = 0.01,
              "eval_metric" = "auc") # make sure to maximize!
# cv.nround <- 5
# cv.nfold <- 3
nround = 5
auc.train <- list()
auc.validate <- list()

# good test case "change.ind_cco_fin_ult1"
# col <- outcomeCols[3]

# TODO: sort by dependencies, use previous outcome(s) in next prediction
for (col in rev(outcomeCols)) {
  gc()
  cat(which(col==outcomeCols),"/",length(outcomeCols),":",col,fill=T)

  trainRowz <- which(all$dataset == "Train" & !is.na(all[[col]]))
  
  ybase <- mean(as.integer(all[[col]][trainRowz] == "Added"))
  testResults[[col]] <- ybase
  validateResults[[col]] <- ybase
  auc.train[[col]] <- 0.50
  auc.validate[[col]] <- 0.50
  
  validateMatrix <- xgb.DMatrix(data.matrix(all[validateRowz, modelTrainFlds, with=F]), 
                                missing=NaN, 
                                label=(all[[col]][validateRowz] == "Added"))
  
  tryCatch({
    bst = xgb.train(params=xgb.params, data = trainMatrix, missing=NaN,
                    # watchlist=list(train=trainMatrix),
                    watchlist=list(train=trainMatrix, validate=validateMatrix),
                    label = as.integer(all[[col]][trainRowz] == "Added"), 
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
print("Assembling results...")
testPropensitities <- select(testResults, -ncodpers)
names(testPropensitities) <- productFlds
testResults$added_products <- apply(testPropensitities, 1, 
                                    function(row) { 
                                      paste(names(sort(rank(-row, ties.method = "first")))[1:7], collapse=" ") })
print("Writing submission file...")
write.csv(select(testResults, ncodpers, added_products), 
          "data/submission.csv",row.names = F, quote=F)

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
  predranks <- rank(-validateResults[i, outcomeCols], ties.method = "first")
  avgprecision <- avgprecision + mapk(truth == "Added", predranks, 7)
}
avgprecision <- avgprecision/nrow(validateMatrix)
cat("Average mean precision on validation set:",avgprecision,fill=T)

stop("pruts")

dt <- data.table(pers = rep(letters[1:3],3),
                 month = (1:9-1) %/% 3 + 1,
                 purch = 1:9 %% 2)
dt[, nmonth := 1+month, by = .(pers)]
dt[, sump := c(cumsum(purch[1:2]),NA), by = .(pers)]
print(arrange(dt, pers))
# group_by(dt, pers) %>% summarise(np = sum(purch))



