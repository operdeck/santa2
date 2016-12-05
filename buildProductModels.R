# Kaggle Santander 2 
# data prep: https://www.kaggle.com/apryor6/santander-product-recommendation/detailed-cleaning-visualization/comments

# data is cplx https://www.kaggle.com/sudalairajkumar/santander-product-recommendation/when-less-is-more/code
# https://www.kaggle.com/c/santander-product-recommendation/forums/t/25579/when-less-is-more
# https://www.kaggle.com/alexeylive/santander-product-recommendation/june-2015-customers/run/468128

# TODO mini version with just a-priori probabilities...
# TODO high cardinality categoricals to nrs

source("santa2.R")
source("metrics.R")

train <- fread(paste(data_folder,"train_ver2.csv",sep="/"), colClasses = data_colClasses)
test <- fread(paste(data_folder,"test_ver2.csv",sep="/"), colClasses = data_colClasses)
productFlds <- names(train)[grepl("^ind_.*ult1$",names(train))] # products purchased

# Add derived field with nr of distincts - before we truncate the train set to just certain dates
# TODO potentially use test set as well

for (f in intersect(setdiff(names(train), c(productFlds,data_dateFlds,"ncodpers")), 
                    names(train)[sapply(train, class)=="character"])) {
  print(f)
  if (F & length(unique(train[[f]])) > 20) {
    cat("Transforming high cardinality field to frequencies",f,class(f),fill=T)
    highCardinalityFieldSummary <- 
      data.table(rbindlist(list(data.frame(group_by_(train, f) %>% summarise(freq = n()), stringsAsFactors = F),
                                data.frame(group_by_(test, f) %>% summarise(freq = n()), stringsAsFactors = F)),
                use.names = T) %>% group_by_(f) %>% summarise(freq = sum(freq, na.rm=T)) %>%
      arrange(desc(freq)))
    # Create hot-one encoding for the top-3
    for (topN in 1:3) {
      highCardinalityFieldSummary[[paste("is",f,make.names(as.character(highCardinalityFieldSummary[[1]][topN])),sep=".")]] <- 
        c(rep(FALSE, topN-1), TRUE, rep(FALSE, nrow(highCardinalityFieldSummary)-topN))
    }
    setnames(highCardinalityFieldSummary, "freq", paste("freq",f,sep="."))
    setkeyv(train, f)
    setkeyv(test, f)
    setkeyv(highCardinalityFieldSummary, f)
    train <- merge(train, highCardinalityFieldSummary, all.x=TRUE)
    test <- merge(test, highCardinalityFieldSummary, all.x=TRUE)

    train[[f]] <- NULL
    test[[f]] <- NULL
  } else {
    cat("Transforming categorical field to factor levels",f,fill=T)
    lvls <- unique(unique(train[[f]]),unique(test[[f]]))
    train[[f]] <- factor(train[[f]], levels = lvls)
    test[[f]] <- factor(test[[f]], levels = lvls)
  }
}

# TODO: data cleaning (Age etc)

# set aside outcomes of 1 month before test set
testPrevPortfolio <- train[fecha_dato == testDatePrev, 
                           c("fecha_dato", "ncodpers", productFlds), with=F]
testPrevPortfolio$fecha_dato <- toMonthNr(testPrevPortfolio$fecha_dato)

# TODO : for validation run this whole thing predicting e.g. '2016-05-28' from '2015-05-28'

# Only keep the records we need for the prediction dates
train <- train[fecha_dato %in% trainDates,]

allPersonz <- unique(train$ncodpers)
cat("Train size:", dim(train), "; unique persons:",length(allPersonz),fill = T)

# Dates

for(f in intersect(data_dateFlds, names(train))) { 
  train[[f]] <- toMonthNr(train[[f]])
  test[[f]] <- toMonthNr(test[[f]])
}
dateCombos <- combn(intersect(data_dateFlds, names(train)),2)
for(i in 1:ncol(dateCombos)) {
  train[[ paste("dates",dateCombos[1,i],dateCombos[2,i],sep="_") ]] <- train[[dateCombos[1,i]]] - train[[dateCombos[2,i]]]
  test[[ paste("dates",dateCombos[1,i],dateCombos[2,i],sep="_") ]] <- test[[dateCombos[1,i]]] - test[[dateCombos[2,i]]]
}

# Categorical

# for (f in names(train)[sapply(train, class) == "character"]) {
#   cat("Transforming categorical field",f,fill=T)
#   lvls <- unique(unique(train[[f]]),unique(test[[f]]))
#   train[[f]] <- factor(train[[f]], levels = lvls)
#   test[[f]] <- factor(test[[f]], levels = lvls)
#   
#   # Maybe drop the ones that already have a frequency associated
#   
#   # Count of factor levels - nope should do this before subsetting to specific dates
#   # grp <- data.table(group_by_(all, f) %>% summarise(n = n()))
#   # names(grp)[2] <- paste("xf.n",f,sep=".")
#   # setkeyv(grp, f)
#   # setkeyv(all, f)
#   # all <- all[grp]
# }

# Remaining fields to numerics - not needed, data.matrix will do that

# Set outcome by subtracting last month portfolio
train2 <- train[, c("fecha_dato", "ncodpers", productFlds), with=F]
train2$fecha_dato <- train2$fecha_dato+1
setkey(train, fecha_dato, ncodpers)
setkey(train2, fecha_dato, ncodpers)
train <- merge(train, train2, all=FALSE) # inner join - only target date will remain
rm(train2)

# Replace portfolio by the portfolio difference of this and previous month; this is the outcome
for (f in productFlds) {
  train[[f]] <- train[[paste(f,"x",sep=".")]] - train[[paste(f,"y",sep=".")]]
  train[[paste(f,"x",sep=".")]] <- NULL
  train[[paste(f,"y",sep=".")]] <- NULL
}

cat("Train size before melting:", dim(train), "; unique persons:",length(unique(train$ncodpers)),fill = T)
train <- melt(train, id.vars = setdiff(names(train), productFlds), measure.vars = productFlds,
              variable.name = "product", value.name = "action")
# super viz could be done here - 
train <- train[action == 1,] # only keep the additions
train$action <- NULL
cat("Train size after melting:", dim(train), "; unique persons:",length(unique(train$ncodpers)),fill = T)

productDistributions <-
  group_by(train, product) %>%
  summarise(additions = n()) %>%
  mutate(additions.rel = additions/nrow(train)) %>% 
  arrange(additions.rel)

print(ggplot(productDistributions, 
       aes(factor(product, levels=unique(productDistributions$product)),additions.rel))+
  geom_bar(stat="identity",position="dodge")+coord_flip()+
  scale_y_continuous(labels = percent)+ggtitle("Additions by product")+
    xlab("product")+ylab("Added"))

cat("Unique customers in train set that made a purchase:",length(unique(train[,ncodpers])),fill=T)

# Join in extra fields
aggregates <- fread(paste(data_folder,"interactionAggregates.csv",sep="/"))
setkey(train, fecha_dato, ncodpers)
setkey(test, fecha_dato, ncodpers)
setkey(aggregates, fecha_dato, ncodpers)
train <- merge(train, aggregates, all.x=TRUE)
test <- merge(test, aggregates, all.x=TRUE)

# Build multiclass model
xgb.params <- list(objective = "multi:softprob",
                   eval_metric = "merror", #"mlogloss", # i really want "map@7" but get errors
                   max.depth = 5,
                   num_class = length(productFlds),
                   eta = 0.05)

# See https://github.com/dmlc/xgboost/blob/master/R-package/demo/custom_objective.R 
# for custom error function

modelTrainFlds <- names(train)[which(!names(train) %in% c("product","ncodpers",productFlds))]
trainMatrix <- xgb.DMatrix(data.matrix(train[, modelTrainFlds, with=F]), 
                           missing=NaN, 
                           label=as.integer(train$product)-1)

cv <- xgb.cv(params=xgb.params, data = trainMatrix, missing=NaN,
       nrounds=100,
       nfold=5,
       maximize=F)
cv2 <- rbindlist(list(data.frame(merror.mean = cv$train.merror.mean,
                                 merror.std = cv$train.merror.std,
                                 dataset = "train",
                                 round = seq(1:nrow(cv))),
                      data.frame(merror.mean = cv$test.merror.mean,
                                 merror.std = cv$test.merror.std,
                                 dataset = "test",
                                 round = seq(1:nrow(cv)))))
print(ggplot(cv2, aes(x=round, y=merror.mean, colour=dataset, group=dataset))+
  geom_errorbar(aes(ymin=merror.mean-merror.std, ymax=merror.mean+merror.std))+
  ggtitle(paste("CV error", "depth", xgb.params[["max.depth"]],"eta",xgb.params[["eta"]]))+
  geom_line(colour="black"))

bst = xgb.train(params=xgb.params, data = trainMatrix, missing=NaN,
                watchlist=list(train=trainMatrix),
                nrounds=60, 
                maximize=F)

# Compute & plot feature importance matrix & summary tree
print("Feature importance matrix...")
importance_matrix <- xgb.importance(modelTrainFlds, model = bst)
print(xgb.plot.importance(importance_matrix))

# xgb.plot.tree(feature_names = dimnames(trainMatrix)[[2]], model = bst, n_first_tree = 2)
# xgb.plot.multi.trees(model = bst, feature_names = dimnames(trainMatrix)[[2]], features.keep = 3)

print("Test predictions...")
testMatrix <- xgb.DMatrix(data.matrix(test[, modelTrainFlds, with=F]), missing=NaN)
xgbpred <- predict(bst, testMatrix, missing=NaN)
testProbabilities <- t(matrix(xgbpred, nrow=length(productFlds), ncol=nrow(test)))
colnames(testProbabilities) <- productFlds

# Force probabilities to 0 for products already in portfolio of previous month
portfolioMultipliers <- test[, "ncodpers", with=F]
portfolioMultipliers <- left_join(portfolioMultipliers, (1-testPrevPortfolio), by="ncodpers") %>% select(-ncodpers, -fecha_dato)
# testProbabilities <- testProbabilities * portfolioMultipliers
# with multiply: 0.0279001, without: 0.0279001. Really? 

# Viz distributions together with the earlier ones from the actual data
print("Test distributions")
productDistributions$dataset <- "Train"
testDistrib <- data.frame(
  product   = productFlds,
  additions = rowSums(apply(-testProbabilities, 1, rank, ties.method = "first") <= 7),
  dataset   = "Test Predictions")
testDistrib$dataset.total <- sum(testDistrib$additions) 
testDistrib$additions.rel <- testDistrib$additions/testDistrib$dataset.total 
productDistributions <- rbindlist(list(productDistributions, testDistrib), use.names = T, fill=T)

print(ggplot(productDistributions, 
             aes(factor(product, levels=unique(arrange(productDistributions,additions.rel)$product)),additions.rel,fill=dataset))+
        geom_bar(stat="identity",position="dodge")+coord_flip()+
        scale_y_continuous(labels = percent)+ggtitle("Additions by product")+
        xlab("product")+ylab("Added"))

print("Assembling results...")
# TODO use testPrevPortfolio to set props to 0 for things already in portfolio
testResults <- data.frame(ncodpers = test[, ncodpers])
testResults$added_products <- apply(testProbabilities, 1, 
                                    function(row) { 
                                      paste(names(sort(rank(-row, ties.method = "first")))[1:7], collapse=" ") })
print("Writing submission file...")
write.csv(testResults, "data/newsubmission.csv",row.names = F, quote=F)

# Get validation set back into original 'wide' format
# validationTruth <- spread(mutate(train[dataset == "Validate", 
#                      c("fecha_dato","ncodpers","product"), with=F], value=1),
#        product, value, fill=0)
# for (f in setdiff(productFlds,names(validationTruth))) {
#   validationTruth[[f]] <- 0
# }
# validationTruth <- validationTruth[, c("fecha_dato","ncodpers", productFlds)]
# 
# # Get validation predictions but remove the duplicates (these happened because the 
# # data was melted to long format)
# validationWideRowz <- which( !duplicated(data.matrix(train[dataset == "Validate", c("fecha_dato","ncodpers"), with=F])) )
# validationPredicitons <- validateProbabilities[validationWideRowz,productFlds]
# 
# # Calculate score on validation set
# avgprecision <- 0
# for (i in 1:nrow(validationTruth)) {
#   truth <- as.numeric(validationTruth[i, productFlds])==1
#   names(truth) <- productFlds
#   predranks <- rank(-validationPredicitons[i, productFlds], ties.method = "first")
#   # print(mapk(truth == 1, predranks, 7))
#   avgprecision <- avgprecision + mapk(truth, predranks, 7)
# }
# avgprecision <- avgprecision/nrow(validationTruth)
# cat("Average mean precision on validation set:",avgprecision,fill=T)


zip("data/newsubmission.csv.zip","data/newsubmission.csv")


