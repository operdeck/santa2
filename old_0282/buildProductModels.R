# Kaggle Santander 2 
# data prep: https://www.kaggle.com/apryor6/santander-product-recommendation/detailed-cleaning-visualization/comments

# data is cplx https://www.kaggle.com/sudalairajkumar/santander-product-recommendation/when-less-is-more/code
# https://www.kaggle.com/c/santander-product-recommendation/forums/t/25579/when-less-is-more
# https://www.kaggle.com/alexeylive/santander-product-recommendation/june-2015-customers/run/468128


source("santa2.R")
source("metrics.R")

validationRatio <- 0.1 # Split training sample into Train and Validate

train <- fread(paste(data_folder,"train_ver2.csv",sep="/"), colClasses = data_colClasses)
test <- fread(paste(data_folder,"test_ver2.csv",sep="/"), colClasses = data_colClasses)
train <- train[fecha_dato %in% trainDates,]
productFlds <- names(train)[grepl("^ind_.*ult1$",names(train))] # products purchased

allPersonz <- unique(train$ncodpers)
cat("Train size:", dim(train), "; unique persons:",length(allPersonz),fill = T)

# Split train/validation by persons

split <- data.table(ncodpers = allPersonz,
                    dataset = sample(c("Train","Validate"),length(allPersonz),replace=T,prob=c(1-validationRatio,validationRatio)))
setkey(train, ncodpers)
setkey(split, ncodpers)
train <- train[split]

# Concatenate both into one set makes it a lot easier to do all the 
# data processing and generation of derived fields.
# train/val not done properly!
# test$dataset <- "Test"
# all <- bind_rows(train, test)
# rm(list=c("train","test"))

print(ggplot(group_by(train, dataset) %>% summarise(n = n(), pct = n()/nrow(train)), 
       aes(dataset,pct,label=n,fill=dataset))+
  geom_bar(stat="identity")+geom_text()+scale_y_continuous(labels=percent)+ggtitle("Data set splits"))

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

# Categorical

for (f in setdiff(names(train)[sapply(train, class) == "character"], c("dataset"))) {
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
  group_by(train, product, dataset) %>%
  summarise(additions = n()) %>%
  left_join(group_by(train, dataset) %>% summarise(dataset.total = n()), by="dataset") %>%
  mutate(additions.rel = additions/dataset.total) %>% 
  arrange(additions.rel)

print(ggplot(productDistributions, 
       aes(factor(product, levels=unique(productDistributions$product)),additions.rel,fill=dataset))+
  geom_bar(stat="identity",position="dodge")+coord_flip()+
  scale_y_continuous(labels = percent)+ggtitle("Additions by product")+
    xlab("product")+ylab("Added"))

cat("Unique customers in train set that made a purchase:",length(unique(train[dataset=="Train",ncodpers])),fill=T)

# Join in extra fields
aggregates <- fread(paste(data_folder,"interactionAggregates.csv",sep="/"))
setkey(train, fecha_dato, ncodpers)
setkey(test, fecha_dato, ncodpers)
setkey(aggregates, fecha_dato, ncodpers)
train <- merge(train, aggregates, all.x=TRUE)
test <- merge(test, aggregates, all.x=TRUE)

# Build multiclass model
xgb.params <- list(objective = "multi:softprob",
                   eval_metric = "mlogloss", # i really want "map@7" but get errors
                   max.depth = 5,
                   num_class = length(productFlds),
                   eta = 0.01)

# See https://github.com/dmlc/xgboost/blob/master/R-package/demo/custom_objective.R 
# for custom error function

modelTrainFlds <- names(train)[which(!names(train) %in% c("product","dataset",productFlds))]
trainMatrix <- xgb.DMatrix(data.matrix(train[dataset == "Train", modelTrainFlds, with=F]), 
                           missing=NaN, 
                           label=as.integer(train$product[train$dataset == "Train"])-1)
validateMatrix <- xgb.DMatrix(data.matrix(train[dataset == "Validate", modelTrainFlds, with=F]), 
                           missing=NaN, 
                           label=as.integer(train$product[train$dataset == "Validate"])-1)

bst = xgb.train(params=xgb.params, data = trainMatrix, missing=NaN,
                watchlist=list(train=trainMatrix, validate=validateMatrix),
                nrounds=50, # 500
                maximize=F)

# Compute & plot feature importance matrix & summary tree
print("Feature importance matrix...")
importance_matrix <- xgb.importance(modelTrainFlds, model = bst)
print(xgb.plot.importance(importance_matrix))

# xgb.plot.tree(feature_names = dimnames(trainMatrix)[[2]], model = bst, n_first_tree = 2)
# xgb.plot.multi.trees(model = bst, feature_names = dimnames(trainMatrix)[[2]], features.keep = 3)

print("Test/Validate predictions...")
xgbpred <- predict(bst, validateMatrix, missing=NaN)
validateProbabilities <- t(matrix(xgbpred, nrow=length(productFlds), ncol=sum(train$dataset == "Validate")))
colnames(validateProbabilities) <- productFlds

testMatrix <- xgb.DMatrix(data.matrix(test[, modelTrainFlds, with=F]), missing=NaN)
xgbpred <- predict(bst, testMatrix, missing=NaN)
testProbabilities <- t(matrix(xgbpred, nrow=length(productFlds), ncol=nrow(test)))
colnames(testProbabilities) <- productFlds

# Viz distributions together with the earlier ones from the actual data
print("Test/Validate distributions")
testDistrib <- data.frame(
  product   = productFlds,
  additions = rowSums(apply(-testProbabilities, 1, rank, ties.method = "first") <= 7),
  dataset   = "Test Predictions")
testDistrib$dataset.total <- sum(testDistrib$additions) 
testDistrib$additions.rel <- testDistrib$additions/testDistrib$dataset.total 

validateDistrib <- data.frame(
  product   = productFlds,
  additions = rowSums(apply(-validateProbabilities, 1, rank, ties.method = "first") <= 7),
  dataset   = "Validate Predictions")
validateDistrib$dataset.total <- sum(validateDistrib$additions) 
validateDistrib$additions.rel <- validateDistrib$additions/validateDistrib$dataset.total 
productDistributions <- rbindlist(list(productDistributions, testDistrib, validateDistrib), use.names = T, fill=T)

print(ggplot(productDistributions, 
             aes(factor(product, levels=unique(arrange(productDistributions,additions.rel)$product)),additions.rel,fill=dataset))+
        geom_bar(stat="identity",position="dodge")+coord_flip()+
        scale_y_continuous(labels = percent)+ggtitle("Additions by product")+
        xlab("product")+ylab("Added"))

print("Assembling results...")
testResults <- data.frame(ncodpers = test[, ncodpers])
testResults$added_products <- apply(testProbabilities, 1, 
                                    function(row) { 
                                      paste(names(sort(rank(-row, ties.method = "first")))[1:7], collapse=" ") })
print("Writing submission file...")
write.csv(testResults, paste(data_folder,"newsubmission.csv",sep="/"),row.names = F, quote=F)

# Get validation set back into original 'wide' format
validationTruth <- spread(mutate(train[dataset == "Validate", 
                     c("fecha_dato","ncodpers","product"), with=F], value=1),
       product, value, fill=0)
for (f in setdiff(productFlds,names(validationTruth))) {
  validationTruth[[f]] <- 0
}
validationTruth <- validationTruth[, c("fecha_dato","ncodpers", productFlds)]

# Get validation predictions but remove the duplicates (these happened because the 
# data was melted to long format)
validationWideRowz <- which( !duplicated(data.matrix(train[dataset == "Validate", c("fecha_dato","ncodpers"), with=F])) )
validationPredicitons <- validateProbabilities[validationWideRowz,productFlds]

# Calculate score on validation set - TODO get this much faster
avgprecision <- 0
for (i in 1:nrow(validationTruth)) {
  truth <- validationTruth[i,productFlds]
  predranks <- rank(-validationPredicitons[i, productFlds], ties.method = "first")
  avgprecision <- avgprecision + mapk(truth == 1, predranks, 7)
}
avgprecision <- avgprecision/nrow(validationTruth)
cat("Average mean precision on validation set:",avgprecision,fill=T)


zip(paste(data_folder,"newsubmission.csv.zip",sep="/"),paste(data_folder,"newsubmission.csv",sep="/"))

