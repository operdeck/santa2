# Kaggle Santander 2 
# data prep: https://www.kaggle.com/apryor6/santander-product-recommendation/detailed-cleaning-visualization/comments

# data is cplx https://www.kaggle.com/sudalairajkumar/santander-product-recommendation/when-less-is-more/code
# https://www.kaggle.com/c/santander-product-recommendation/forums/t/25579/when-less-is-more
# https://www.kaggle.com/alexeylive/santander-product-recommendation/june-2015-customers/run/468128


source("santa2.R")
source("metrics.R")

# TODO: validating like this really makes no sense, need a different approach
validationRatio <- 0.1 # Split training sample into Train and Validate

train <- fread(paste(data_folder,"train_ver2.csv",sep="/"), colClasses = data_colClasses)
test <- fread(paste(data_folder,"test_ver2.csv",sep="/"), colClasses = data_colClasses)

# TODO: consider this to force probs to 0 for products already in portfolio 1 m before test
# set aside outcomes of 1 month before test set
# testPrevPortfolio <- train[fecha_dato == testDatePrev, 
#                            c("fecha_dato", "ncodpers", productFlds), with=F]
# testPrevPortfolio$fecha_dato <- toMonthNr(testPrevPortfolio$fecha_dato)

train <- train[fecha_dato %in% trainDates,]
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
cat("Train size:", dim(train), "; unique persons:",length(allPersonz),fill = T)

# TODO: consider replacing or adding high cardinality symbolics
# Add derived field with nr of distincts - before we truncate the train set to just certain dates

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
# TODO consider this, or is this really the same?
# train <- merge(train, train2, all.x=TRUE)
# train <- train[fecha_dato != min(train$fecha_dato),] # chop off first month
rm(train2)

# summary(train[, paste(productFlds,"x",sep="."), with=F])
# summary(train[, paste(productFlds,"y",sep="."), with=F]) # many NA's...??
# stop()

# Replace portfolio by the portfolio difference of this and previous month; this is the outcome
# TODO: consider checking for NA on y, as this would represent a new customer
# **DONE**
for (f in productFlds) {
  train[[f]] <- ifelse(is.na(train[[paste(f,"y",sep=".")]]),
                       train[[paste(f,"x",sep=".")]],
                       train[[paste(f,"x",sep=".")]] - train[[paste(f,"y",sep=".")]])
  
  # train[[f]] <- train[[paste(f,"x",sep=".")]] - train[[paste(f,"y",sep=".")]]
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

print("Number of product additions to number of customers",fill=T)
cat(uniqueBefore-uniqueAfter, "bought nothing",fill=T)
print(group_by(train, ncodpers) %>%
        summarise(n_products = n()) %>%
        group_by(n_products) %>%
        summarise(n_customers = n()))

cat("Unique customers that made a purchase:",length(unique(train[,ncodpers])),fill=T)
cat("Unique customers in train set that made a purchase:",length(unique(train[dataset=="Train",ncodpers])),fill=T)
cat("Unique customers in validate set that made a purchase:",length(unique(train[dataset=="Validate",ncodpers])),fill=T)

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

cvresults <- xgb.cv(params=xgb.params, data = trainMatrix, missing=NaN,
                    nrounds=500,
                    nfold=5,
                    maximize=F)

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

bst = xgb.train(params=xgb.params, data = trainMatrix, missing=NaN,
                watchlist=list(train=trainMatrix, validate=validateMatrix),
                nrounds=500, 
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

# TODO: consider this
# Force probabilities to 0 for products already in portfolio of previous month
# portfolioMultipliers <- test[, "ncodpers", with=F]
# portfolioMultipliers <- left_join(portfolioMultipliers, (1-testPrevPortfolio), by="ncodpers") %>% select(-ncodpers, -fecha_dato)
# testProbabilities <- testProbabilities * portfolioMultipliers
# with multiply: 0.0279001, without: 0.0279001. Really? 

# Viz distributions together with the earlier ones from the actual data
print("Test/Validate distributions")
testDistrib <- data.frame(
  product   = productFlds,
  additions = rowSums(apply(-testProbabilities, 1, rank, ties.method = "first") <= 7),
  dataset   = "Test Predictions",
  stringsAsFactors = F)
testDistrib$dataset.total <- sum(testDistrib$additions) 
testDistrib$additions.rel <- testDistrib$additions/testDistrib$dataset.total 

validateDistrib <- data.frame(
  product   = productFlds,
  additions = rowSums(apply(-validateProbabilities, 1, rank, ties.method = "first") <= 7),
  dataset   = "Validate Predictions",
  stringsAsFactors = F)
validateDistrib$dataset.total <- sum(validateDistrib$additions) 
validateDistrib$additions.rel <- validateDistrib$additions/validateDistrib$dataset.total 

print(ggplot(rbindlist(list(productDistributions, testDistrib, validateDistrib), use.names = T, fill=T), 
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
submFile <- paste(data_folder,"newsubmission.csv",sep="/")
write.csv(testResults, submFile,row.names = F, quote=F)

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
