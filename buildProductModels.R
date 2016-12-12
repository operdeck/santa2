# Kaggle Santander 2 
# data prep: https://www.kaggle.com/apryor6/santander-product-recommendation/detailed-cleaning-visualization/comments

# data is cplx https://www.kaggle.com/sudalairajkumar/santander-product-recommendation/when-less-is-more/code
# https://www.kaggle.com/c/santander-product-recommendation/forums/t/25579/when-less-is-more
# https://www.kaggle.com/alexeylive/santander-product-recommendation/june-2015-customers/run/468128


source("santa2.R")
source("metrics.R")

library(caret)

train <- fread(paste(data_folder,"train_ver2.csv",sep="/"), colClasses = data_colClasses)
test <- fread(paste(data_folder,"test_ver2.csv",sep="/"), colClasses = data_colClasses)

# TODO: consider this to force probs to 0 for products already in portfolio 1 m before test
# set aside outcomes of 1 month before test set
# testPrevPortfolio <- train[fecha_dato == testDatePrev, 
#                            c("fecha_dato", "ncodpers", productFlds), with=F]
# testPrevPortfolio$fecha_dato <- toMonthNr(testPrevPortfolio$fecha_dato)

productFlds <- names(train)[grepl("^ind_.*ult1$",names(train))] # products purchased

# TODO: consider truncating train set to test customers
# Test and train summaries are very similar
# All of test ncodpers are in train. Almost all of train are in test.
# cat("Unique train customers",length(unique(train$ncodpers)),fill=T)
# train <- train[ncodpers %in% unique(test$ncodpers) ,]
# cat("Unique train customers after truncating to test persons",length(unique(train$ncodpers)),fill=T)

# TODO: consider removing snapshots for customers before they joined
# cat("Train size before removing NAs",dim(train),fill=T)
# train <- train[!is.na(antiguedad),] # remove snapshots where customers were not customers yet 
# cat("Train size after removing NAs",dim(train),fill=T)

allPersonz <- unique(train$ncodpers)
cat("Train size:", dim(train), "; unique persons:",length(allPersonz),
    "; date range", unique(train$fecha_dato), fill = T)

# TODO: consider replacing or adding high cardinality symbolics
# Add derived field with nr of distincts - before we truncate the train set to just certain dates

# Dates

for(f in intersect(data_dateFlds, names(train))) { 
  train[[f]] <- toMonthNr(train[[f]])
  test[[f]] <- toMonthNr(test[[f]])
}
dateCombos <- combn(intersect(data_dateFlds, names(train)),2)
for(i in 1:ncol(dateCombos)) {
  train[[ paste("diff",dateCombos[1,i],dateCombos[2,i],sep="_") ]] <- train[[dateCombos[1,i]]] - train[[dateCombos[2,i]]]
  test[[ paste("diff",dateCombos[1,i],dateCombos[2,i],sep="_") ]] <- test[[dateCombos[1,i]]] - test[[dateCombos[2,i]]]
}

# This field antiguedad seems not set consistently. It is the same as diff_fecha_dato_fecha_alta anyway.
train[["antiguedad"]] <- NULL
test[["antiguedad"]] <- NULL

# Figure out the birthdays here using both sets but only the more recent values because customer
# fields in first 6 months are not set properly
print("Set birthdays / fix age")

bdaySet <- data.table(rbind(train[fecha_dato >= min(test$fecha_dato)-11, c("fecha_dato", "ncodpers", "age"), with=F],
                 test[, c("fecha_dato", "ncodpers", "age"), with=F]) %>% arrange(ncodpers))
setkeyv(bdaySet, c("ncodpers","fecha_dato"))
bdaySet_prevmonth <- bdaySet
bdaySet_prevmonth$fecha_dato <- 1 + bdaySet_prevmonth$fecha_dato
setkeyv(bdaySet_prevmonth, key(bdaySet))
birthdays <- merge(bdaySet, bdaySet_prevmonth) %>% 
  filter(age.x == (1+age.y)) %>% 
  dplyr::mutate(age.months.at.abirthday = 12*age.x) %>%
  select(-age.x, -age.y) %>%
  dplyr::rename(date.at.abirthday = fecha_dato)
# some customers appear multiple times... fix this by taking first birthday
birthdays <- data.table(group_by(birthdays, ncodpers) %>% 
                          dplyr::summarise(date.at.abirthday = min(date.at.abirthday),
                                    age.months.at.abirthday = min(age.months.at.abirthday)))

setkey(train, fecha_dato, ncodpers)
setkeyv(test, key(train))
setkey(birthdays, ncodpers)

train <- merge(train, birthdays, all.x=T)
age_in_months <- train$age.months.at.abirthday + train$fecha_dato - train$date.at.abirthday
train$age <- age_in_months %/% 12
train$is.birthday <- (age_in_months %% 12)==0
train$months.to.18.bday <- abs(12*18 - age_in_months)
train[, date.at.abirthday := NULL]
train[, age.months.at.abirthday := NULL]

test <- merge(test, birthdays, all.x=T)
age_in_months <- test$age.months.at.abirthday + test$fecha_dato - test$date.at.abirthday
test$age <- age_in_months %/% 12
test$is.birthday <- (age_in_months %% 12)==0
test$months.to.18.bday <- abs(12*18 - age_in_months)
test[, date.at.abirthday := NULL]
test[, age.months.at.abirthday := NULL]

# Categorical

for (f in names(train)[sapply(train, class) == "character"]) {
  cat("Transforming categorical field",f,fill=T)
  lvls <- unique(unique(train[[f]]),unique(test[[f]]))
  train[[f]] <- factor(train[[f]], levels = lvls)
  test[[f]] <- factor(test[[f]], levels = lvls)
  
  # Count of factor levels - nope should do this before subsetting to specific dates
  # grp <- data.table(group_by_(all, f) %>% summarise(n = n()))
  # names(grp)[2] <- paste("xf.n",f,sep=".")
  # setkeyv(grp, f)
  # setkeyv(all, f)
  # all <- all[grp]
}

# Remaining fields to numerics - not needed, data.matrix will do that

# Compare customer detail to previous month. Using matrix comparison and being very careful that
# columns and rows are in the same order. Generate data sets that should be joined to the predictors. 

# custDetailFields <- setdiff(names(train),c("dataset","fecha_dato",productFlds))
# custDetailAtTrainMonth <- train[fecha_dato==trainDateNr, custDetailFields, with=F]
# setorder(custDetailAtTrainMonth, ncodpers)
# custDetailChangesTrain <- custDetailAtTrainMonth[,c("ncodpers"),with=F]
# custDetailBeforeTrainMonth <- 
#   merge(custDetailChangesTrain, 
#         train[fecha_dato==(trainDateNr-1), custDetailFields, with=F], 
#         by="ncodpers",
#         all.x=TRUE, all.y=F)[,custDetailFields,with=F]
# setorder(custDetailBeforeTrainMonth, ncodpers)
# custDetailChangesTrain$cust.detail.diffToLastMonth <- rowSums((is.na(custDetailBeforeTrainMonth) & !is.na(custDetailAtTrainMonth)) | (custDetailAtTrainMonth != custDetailBeforeTrainMonth), na.rm=T)

# custDetailAtTestMonth <- test[fecha_dato==testDateNr, custDetailFields, with=F]
# setorder(custDetailAtTestMonth, ncodpers)
# custDetailChangesTest <- custDetailAtTestMonth[,c("ncodpers"),with=F]
# custDetailBeforeTestMonth <- 
#   merge(custDetailChangesTest, 
#         train[fecha_dato==(testDateNr-1), custDetailFields, with=F],  # from train set!
#         by="ncodpers",
#         all.x=TRUE, all.y=F)[,custDetailFields,with=F]
# setorder(custDetailBeforeTestMonth, ncodpers)
# custDetailChangesTest$cust.detail.diffToLastMonth <- rowSums((is.na(custDetailBeforeTestMonth) & !is.na(custDetailAtTestMonth)) | (custDetailAtTestMonth != custDetailBeforeTestMonth), na.rm=T)

# test$cust.detail.turned18 <- train[, ]
# # NB: because customer attributes in first 6 months are unreliable. This relies on the fact that test & train dates
# # are exactly 12 months apart
# custDetailChangesTrain$cust.detail.turned18 <- (custDetailBeforeTestMonth$age == 18 & custDetailAtTestMonth$age == 19)

# easier...

# TODO use and TODO check correct for npers etc.
# 
# print(
# ggplot(rbindlist(list(mutate(custDetailChangesTrain,dataset="train"),
#                       mutate(custDetailChangesTrain[ncodpers %in% custDetailChangesTest$ncodpers,],dataset="train-test custs"),
#                       mutate(custDetailChangesTest,dataset="test"))), 
#        aes(factor(cust.detail.diffToLastMonth),fill=dataset)) + 
#   geom_bar(position="dodge") +scale_y_log10()+ggtitle("Changes in Customer Record since last month")
# )
# 
# print(
#   ggplot(rbindlist(list(mutate(custDetailChangesTrain,dataset="train"),
#                         mutate(custDetailChangesTrain[ncodpers %in% custDetailChangesTest$ncodpers,],dataset="train-test custs"),
#                         mutate(custDetailChangesTest,dataset="test"))), 
#          aes(factor(cust.detail.turned18),fill=dataset)) + 
#     geom_bar(position="dodge") +scale_y_log10()+ggtitle("Customers turned 18")
# )


# TODO: plot suggests to be careful, maybe only for 1 or 2 change - rest looks too different
# TODO: here or elsewhere add flag for "turned18" etc.
# TODO: use this data...

# Subset the train data to only one year before the test date.
# TODO: vary on this theme

train <- train[fecha_dato %in% trainDateNrs,]

# Set outcome by subtracting last month portfolio
setkey(train, fecha_dato, ncodpers)
train2 <- train[, c(key(train), productFlds), with=F]
train2$fecha_dato <- train2$fecha_dato+1
setkeyv(train2, key(train))
train <- merge(train, train2, all=FALSE) # inner join - only target date will remain
# TODO consider this, or is this really the same?
# train <- merge(train, train2, all.x=TRUE)
# train <- train[fecha_dato != min(train$fecha_dato),] # chop off first month
rm(train2)

# summary(train[, paste(productFlds,"x",sep="."), with=F])
# summary(train[, paste(productFlds,"y",sep="."), with=F]) # many NA's...??
# stop()

# Replace portfolio by the portfolio difference of this and previous month; this is the outcome
for (f in productFlds) {
  train[[f]] <- ifelse(is.na(train[[paste(f,"y",sep=".")]]),
                       train[[paste(f,"x",sep=".")]],
                       train[[paste(f,"x",sep=".")]] - train[[paste(f,"y",sep=".")]])
  train[[paste(f,"x",sep=".")]] <- NULL
  train[[paste(f,"y",sep=".")]] <- NULL
}

uniqueBefore <- length(unique(train$ncodpers))
cat("Train size before melting:", dim(train), "; unique persons:",uniqueBefore,fill = T)
train <- melt(train, id.vars = setdiff(names(train), productFlds), measure.vars = productFlds,
              variable.name = "product", value.name = "action")

# TODO consider plot of actions vs products replacing product distrib plot down
# productDistributions <-
#   group_by(train, product) %>%
#   summarise(additions = sum(action==1, na.rm=T))
# productDistributions$additions.rel <- productDistributions$additions/sum(productDistributions$additions)
# productDistributions$dataset <- "Train"
# productDistributions <- arrange(productDistributions, additions.rel)
# 
# print(ggplot(filter(train, is.na(action) | action != 0), aes(factor(product, levels=productDistributions$product), fill=factor(action)))+
#         geom_bar()+coord_flip()+
#         ggtitle("Changes by product"))

train <- train[action == 1,] # only keep the additions
train$action <- NULL
uniqueAfter <- length(unique(train$ncodpers))
cat("Train size after melting:", dim(train), "; unique persons:",uniqueAfter,fill = T)

productDistributions <-
  group_by(train, product) %>%
  dplyr::summarise(additions = n(), additions.rel = n()/nrow(train)) %>%
  arrange(additions.rel)

print(ggplot(productDistributions, 
       aes(factor(product, levels=unique(productDistributions$product)),additions.rel,label=additions))+
      geom_bar(stat="identity",position="dodge",fill="lightblue")+coord_flip()+
      geom_text()+
      scale_y_continuous(labels = percent)+ggtitle("Additions by product")+
    xlab("product")+ylab("Added"))

nProductsByCusts <- rbind(data.frame(n_products = 0,
                                    n_customers = uniqueBefore-uniqueAfter),
                         group_by(train, ncodpers) %>%
                           dplyr::summarise(n_products = n()) %>%
                           group_by(n_products) %>%
                           dplyr::summarise(n_customers = n()))
print(ggplot(nProductsByCusts, aes(x=factor(n_products), y=n_customers, label=n_customers))+
        geom_bar(stat="identity",fill="lightblue")+geom_text()+
        scale_y_log10()+
        ggtitle("How many people bought how many products?"))

# Join in extra fields

aggregates <- fread(paste(data_folder,"interactionAggregates.csv",sep="/"))
setkey(train, fecha_dato, ncodpers)
setkeyv(test, key(train))
setkeyv(aggregates, key(train))
train <- merge(train, aggregates, all.x=TRUE)
test <- merge(test, aggregates, all.x=TRUE)

# Show distributions of predictors in train vs test
for (f in names(test)) {
  if (is.numeric(test[[f]]) & !is.integer(test[[f]])) {
    cat("Plotting numeric", f, fill=T)
    print(ggplot(rbindlist(list(dplyr::mutate(train[, f, with=F], dataset="Train"),
                                dplyr::mutate(test[, f, with=F], dataset="Test"))),
                 aes_string(f, fill="dataset",  colour="dataset")) +
      geom_density(alpha = 0.2)+ggtitle(f))
  } else if (is.factor(test[[f]])) {
    cat("Plotting factor", f, fill=T)
    print(ggplot(rbindlist(list(group_by_(train, f) %>% 
                                  dplyr::summarise(pct = n()/nrow(train)) %>% 
                                  dplyr::mutate(dataset="Train"),
                                group_by_(test, f) %>% 
                                  dplyr::summarise(pct = n()/nrow(test)) %>% 
                                  dplyr::mutate(dataset="Test"))),
                 aes_string(f, "pct", fill="dataset",  colour="dataset")) +
            geom_bar(alpha = 0.2, position="dodge", stat="identity")+ggtitle(f))
  } else {
    cat("Not plotting", f, class(test[[f]]), fill=T)
  }
}

#### TODO: plot the integers too. Factors only top-50 or so.


# Based on plots, drop these
differentDistros <- c("diff_fecha_dato_ult_fec_cli_1t", "ult_fec_cli_1t",
                      "tiprel_1mes")
for (f in differentDistros) {
  test[[f]] <- NULL
  train[[f]] <- NULL
}

# Build multiclass model

modelTrainFlds <- names(train)[which(!names(train) %in% c("product","ncodpers",productFlds))]
trainMatrix <- xgb.DMatrix(data.matrix(train[, modelTrainFlds, with=F]), 
                           missing=NaN, 
                           label=as.integer(train$product)-1)

# Caret hyperparameter tuning
caretHyperParamSearch <- trainControl(method = "cv", number=5, 
                                      classProbs = TRUE,
                                      summaryFunction = mnLogLoss,
                                      verbose=T)
searchGrid <- expand.grid(nrounds = seq(200, 800,by=100),
                          eta = seq(0.02, 0.08, by=0.02),
                          max_depth = 4:7,
                          gamma = 2,
                          colsample_bytree = 1,
                          min_child_weight = 1,
                          subsample = 1)
predictors <- data.matrix(train[, modelTrainFlds, with=F])
predictors[which(is.na(predictors))] <- 99999

tuningResults <-
  train(x = predictors,
        y = factor(train$product, levels=unique(train$product)), # some products never occur
        method = "xgbTree",
        metric = "logLoss",
        maximize = F,
        trControl = caretHyperParamSearch,
        tuneGrid = searchGrid)

print(tuningResults)
print(ggplot(tuningResults)+ggtitle("Hyperparameters"))

# The final values used for the model were nrounds =
#   800, max_depth = 5, eta = 0.04, gamma = 2, colsample_bytree
# = 1, min_child_weight = 1 and subsample = 1.

stop()
cv.nfold <- 5
cv.rounds <- 500
xgb.rounds <- 800

xgb.params <- list(objective = "multi:softprob",
                   eval_metric = "mlogloss", # i really want "map@7" but get errors
                   max.depth = 5,
                   min_child_weight = 1,
                   colsample_bytree = 1,
                   subsample = 1,
                   gamma = 2,
                   num_class = length(productFlds),
                   eta = 0.04)

# See https://github.com/dmlc/xgboost/blob/master/R-package/demo/custom_objective.R 
# for custom error function

# N-Fold cross validation to find best hyperparameters. Stratifying on the customer ID's
# to keep validation fair (multiple entries exist for the same people, so otherwise there would
# be leakage)

do.CV <- F
if (do.CV) {
  allCusts <- unique(train$ncodpers)
  allCusts <- sample(allCusts, length(allCusts)) # shuffle
  cv.folds <- list()
  for (i in 1:cv.nfold) {
    foldCusts <- allCusts[((length(allCusts) %/% 5)*(i-1)+1) : ((length(allCusts) %/% 5)*i)]
    cv.folds[[i]] <- which(train$ncodpers %in% foldCusts)
  }
  cvresults <- xgb.cv(params=xgb.params, data = trainMatrix, missing=NaN,
                      nrounds=cv.rounds,
                      folds=cv.folds,
                      early.stop.round=10,
                      maximize=F)
  
  # Vizualize results of cross validation
  cv2 <- rbindlist(list(data.frame(error.mean = cvresults[[paste("train",xgb.params[["eval_metric"]],"mean",sep=".")]],
                                   error.std = cvresults[[paste("train",xgb.params[["eval_metric"]],"std",sep=".")]],
                                   dataset = "train",
                                   round = seq(1:nrow(cvresults))),
                        data.frame(error.mean = cvresults[[paste("test",xgb.params[["eval_metric"]],"mean",sep=".")]],
                                   error.std = cvresults[[paste("test",xgb.params[["eval_metric"]],"std",sep=".")]],
                                   dataset = "test",
                                   round = seq(1:nrow(cvresults)))))
  print(ggplot(cv2, aes(x=round, y=error.mean, colour=dataset, group=dataset))+
          geom_errorbar(aes(ymin=error.mean-error.std, ymax=error.mean+error.std))+
          ggtitle(paste("CV error", "depth", xgb.params[["max.depth"]],"eta",xgb.params[["eta"]]))+
          geom_line(colour="black")+
          ylab(xgb.params[["eval_metric"]]))
}

# Run the real model
bst = xgb.train(params=xgb.params, data = trainMatrix, missing=NaN,
                watchlist=list(train=trainMatrix),
                nrounds=xgb.rounds, 
                maximize=F)

# Compute & plot feature importance matrix & summary tree
print("Feature importance matrix...")
importance_matrix <- xgb.importance(modelTrainFlds, model = bst)
print(importance_matrix)
print(xgb.plot.importance(head(importance_matrix, min(20, nrow(importance_matrix)))))

# Version 0.6 will support this
# xgb.plot.tree(feature_names = dimnames(trainMatrix)[[2]], model = bst, n_first_tree = 2)
# xgb.plot.multi.trees(model = bst, feature_names = dimnames(trainMatrix)[[2]], features.keep = 3)

print("Train predictions...")
xgbpred <- predict(bst, trainMatrix, missing=NaN)
trainProbabilities <- t(matrix(xgbpred, nrow=length(productFlds), ncol=nrow(train)))
colnames(trainProbabilities) <- productFlds

print("Test predictions...")
testMatrix <- xgb.DMatrix(data.matrix(test[, modelTrainFlds, with=F]), missing=NaN)
xgbpred <- predict(bst, testMatrix, missing=NaN)
testProbabilities <- t(matrix(xgbpred, nrow=length(productFlds), ncol=nrow(test)))
colnames(testProbabilities) <- productFlds

# TODO: consider this
# Force probabilities to 0 for products already in portfolio of previous month
# portfolioMultipliers <- test[, "ncodpers", with=F]
# portfolioMultipliers <- left_join(portfolioMultipliers, (1-testPrevPortfolio), by="ncodpers") %>% select(-ncodpers, -fecha_dato)
# testProbabilities <- testProbabilities * portfolioMultipliers
# with multiply: 0.0279001, without: 0.0279001. Really? 

# Viz distributions together with the earlier ones from the actual data
print("Test distributions")

trainDistrib <- data.frame(
  product   = productFlds,
  additions = rowSums(apply(-trainProbabilities, 1, rank, ties.method = "first") <= 7),
  stringsAsFactors = F)
trainDistrib$additions.rel <- trainDistrib$additions/sum(trainDistrib$additions) 

testDistrib <- data.frame(
  product   = productFlds,
  additions = rowSums(apply(-testProbabilities, 1, rank, ties.method = "first") <= 7),
  stringsAsFactors = F)
testDistrib$additions.rel <- testDistrib$additions/sum(testDistrib$additions) 

print(ggplot(rbindlist(list(dplyr::mutate(productDistributions, dataset="Distribution Train Set"), 
                            dplyr::mutate(trainDistrib, dataset="Predictions on Train Set"),
                            dplyr::mutate(testDistrib, dataset="Predictions on Test Set")), 
                       use.names = T, fill=T), 
             aes(factor(product,levels=productDistributions$product),
                 additions.rel,fill=dataset))+
        geom_bar(stat="identity",position="dodge")+coord_flip()+
        scale_y_continuous(labels = percent)+ggtitle("Product additions")+
        xlab("product")+ylab("Added"))

print("Assembling results...")
testResults <- data.frame(ncodpers = test[, ncodpers])
testResults$added_products <- apply(testProbabilities, 1, 
                                    function(row) { 
                                      paste(names(sort(rank(-row, ties.method = "first")))[1:7], collapse=" ") })

print("Writing submission file...")
submFile <- paste(data_folder,"newsubmission.csv",sep="/")
write.csv(testResults, submFile,row.names = F, quote=F)

# Estimate the error. First back into original 'wide' format
truth <- spread(dplyr::mutate(train[,c( key(train), "product"), with=F], value=1),
                product, value, fill=0)
for (f in setdiff(productFlds, names(truth))) {
  truth[[f]] <- 0 # some products are not present at all, make sure to have them
}
truth <- truth[, productFlds] # correct order

# Remove the duplicates (these happened because the data was melted to long format)
predictedAsWideRowz <- which( !duplicated(data.matrix(train[, key(train), with=F])) )
trainPredictions <- trainProbabilities[predictedAsWideRowz, productFlds]

# Calculate score on validation set - TODO get this much faster
print("Average mean precision on train set...")
avgprecision <- 0
for (i in 1:nrow(truth)) {
  predranks <- rank(-trainPredictions[i, productFlds], ties.method = "first")
  avgprecision <- avgprecision + mapk(truth[i,] == 1, predranks, 7)
}
avgprecision <- avgprecision/nrow(truth)
cat("Average mean precision on train set:",avgprecision,fill=T)

# Zip up
zipFile <- paste(data_folder,"newsubmission.csv.zip",sep="/")
if (file.exists(zipFile)) {
  file.remove(zipFile)
}
if (Sys.info()[["sysname"]] == "Windows") {
  zip(zipFile, submFile,
      zip="c:\\Program Files\\7-Zip\\7z.exe", flags="a")
} else {
  zip(zipFile,submFile)
}
