# Kaggle Santander 2 
# data prep: https://www.kaggle.com/apryor6/santander-product-recommendation/detailed-cleaning-visualization/comments

# data is cplx https://www.kaggle.com/sudalairajkumar/santander-product-recommendation/when-less-is-more/code
# https://www.kaggle.com/c/santander-product-recommendation/forums/t/25579/when-less-is-more
# https://www.kaggle.com/alexeylive/santander-product-recommendation/june-2015-customers/run/468128

library("caret")
source("santa2.R")
source("metrics.R")

do.HyperTuning <- F
do.CV <- F
do.testSetOnly <- F # when F it will score the test set
cv.nfold <- 5
cv.rounds <- 500
xgb.rounds <- 500

train <- fread(paste(data_folder,"train_ver2.csv",sep="/"), colClasses = data_colClasses)
test <- fread(paste(data_folder,"test_ver2.csv",sep="/"), colClasses = data_colClasses)

# test set

tstNCODPRS <- c(15889, # no birthday,
                15890, # no purchases,
                15892, # 1 purchase in june 2015,
                1170604, # 1 different purchase
                1170632) # multiple purchases

# TODO: consider this to force probs to 0 for products already in portfolio 1 m before test
# set aside outcomes of 1 month before test set
# testPrevPortfolio <- train[fecha_dato == testDatePrev, 
#                            c("fecha_dato", "ncodpers", productFlds), with=F]
# testPrevPortfolio$fecha_dato <- toMonthNr(testPrevPortfolio$fecha_dato)

if (do.testSetOnly) {
  train <- train[ncodpers %in% tstNCODPRS,]
  test <- test[ncodpers %in% tstNCODPRS,]
}
productFlds <- names(train)[grepl("^ind_.*ult1$",names(train))] # products purchased

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

print(ggplot(rbindlist(list(group_by(train, fecha_dato) %>%
                              dplyr::summarise(n = n(), pct = n()/nrow(train)), 
                            group_by(test, fecha_dato) %>%
                              dplyr::summarise(n = n(), pct = n()/nrow(test)))),
             aes(factor(fecha_dato),pct,label=n,fill=factor(fecha_dato)))+
        geom_bar(stat="identity")+geom_text()+scale_y_continuous(labels=percent)+
        coord_flip()+
        ggtitle("Data set size by month"))

# Dates

for(f in intersect(data_dateFlds, names(train))) { 
  train[[f]] <- toMonthNr(train[[f]])
  test[[f]] <- toMonthNr(test[[f]])
}
dateCombos <- combn(intersect(data_dateFlds, names(train)),2)
for(i in 1:ncol(dateCombos)) {
  train[[ paste("diff",dateCombos[1,i],dateCombos[2,i],sep=".") ]] <- train[[dateCombos[1,i]]] - train[[dateCombos[2,i]]]
  test[[ paste("diff",dateCombos[1,i],dateCombos[2,i],sep=".") ]] <- test[[dateCombos[1,i]]] - test[[dateCombos[2,i]]]
}

# This field antiguedad is not set consistently. It is the same (+/- 1) as diff.fecha_dato_fecha_alta anyway.
train[["antiguedad"]] <- NULL
test[["antiguedad"]] <- NULL

# Figure out the birthdays here using both sets but only the more recent values because customer
# fields in first 6 months are not set properly
print("Set birthdays / fix age")

bdaySet <- data.table(rbind(train[fecha_dato >= min(test$fecha_dato)-11, c("fecha_dato", "ncodpers", "age"), with=F],
                            test[, c("fecha_dato", "ncodpers", "age"), with=F]) %>% 
                        arrange(ncodpers))
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
train$age <- ifelse(is.na(age_in_months), train$age, age_in_months %/% 12)
train$is.birthday <- (age_in_months %% 12)==0
train$months.to.18.bday <- 12*18 - age_in_months
train[, date.at.abirthday := NULL]
train[, age.months.at.abirthday := NULL]

test <- merge(test, birthdays, all.x=T)
age_in_months <- test$age.months.at.abirthday + test$fecha_dato - test$date.at.abirthday
test$age <- ifelse(is.na(age_in_months), test$age, age_in_months %/% 12)
test$is.birthday <- (age_in_months %% 12)==0
test$months.to.18.bday <- 12*18 - age_in_months
test[, date.at.abirthday := NULL]
test[, age.months.at.abirthday := NULL]

# Categorical

# TODO: consider replacing or adding high cardinality symbolics
# Add derived field with nr of distincts - before we truncate the train set to just certain dates
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

# Get the purchases (= outcomes) as the diff between portfolio this and previous month

train$next_month <- train$fecha_dato+1
train <- merge(train, train[, c("ncodpers", "next_month", productFlds), with=F], 
               all.x=F, all.y=F, 
               by.x = c("ncodpers", "fecha_dato"),
               by.y = c("ncodpers", "next_month"),
               suffixes = c("", ".prev")) # inner self join
for (f in productFlds) {
  train[[paste("outcome",f,sep=".")]] <- 
    ifelse(is.na(train[[paste(f,"prev",sep=".")]]), train[[f]], train[[f]] - train[[paste(f,"prev",sep=".")]])
  train[[paste(f,"prev",sep=".")]] <- NULL
}

# Portfolio size / other derived vars here
train[ , outcome.portfolio.size := rowSums(.SD, na.rm=T), .SDcols = productFlds]
train[ , outcome.purchased.size := rowSums(.SD==1, na.rm=T), .SDcols = paste("outcome",productFlds,sep=".")]

# "outcome."productFlds are the purchases - the outcomes
# productFlds is *current* portfolio - to be dropped from predictor set

# Add lags of 1-4 months back to both train & test set

lagFields <- setdiff(names(train), 
                     c(data_dateFlds, 
                       "ncodpers", "fecha_dato", "next_month",
                       names(train)[startsWith(names(train), "diff.")],
                       "is.birthday", "months.to.18.bday"))

lagDur <- seq(1:4)
lagInd <- paste("M",lagDur,sep="")
for (lag in lagDur) {
  cat("Adding lag fields for lag", lag, fill=T)
  
  train$next_month <- train$fecha_dato+lag
  train <- merge(train, train[, c("ncodpers", "next_month", lagFields), with=F], 
                 all.x=T, all.y=F, 
                 by.x = c("ncodpers", "fecha_dato"),
                 by.y = c("ncodpers", "next_month"),
                 suffixes = c("", paste(".M", lag, sep=""))) # left join
  
  xtraLagFieldsInTest <- setdiff(lagFields, names(test))
  test <- merge(test, train[, c("ncodpers", "next_month", lagFields), with=F], 
                 all.x=T, all.y=F, 
                 by.x = c("ncodpers", "fecha_dato"),
                 by.y = c("ncodpers", "next_month"),
                 suffixes = c("", paste(".M", lag, sep=""))) # left join
  setnames(test, xtraLagFieldsInTest, paste(xtraLagFieldsInTest, paste("M", lag, sep=""), sep="."))
}

train[, next_month := NULL]

for (f in productFlds) {
  cat("Adding aggregates for lagged fields for",f,fill=T)
  
  # Nr of purchases
  train[ , greatnewfield := rowSums(.SD == 1, na.rm=T), .SDcols = paste("outcome", f, lagInd, sep=".")]
  setnames(train, "greatnewfield", paste("sum.purchased", f, sep="."))
  test[ , greatnewfield := rowSums(.SD == 1, na.rm=T), .SDcols = paste("outcome", f, lagInd, sep=".")]
  setnames(test, "greatnewfield", paste("sum.purchased", f, sep="."))
  
  # Changes
  train[, c(paste("changes", f, sep=".")) :=
    (train[[paste(f, "M1", sep=".")]] != train[[paste(f, "M2", sep=".")]]) +
    (train[[paste(f, "M2", sep=".")]] != train[[paste(f, "M3", sep=".")]]) +
    (train[[paste(f, "M3", sep=".")]] != train[[paste(f, "M4", sep=".")]])]
  
  test[, c(paste("changes", f, sep=".")) :=
    (test[[paste(f, "M1", sep=".")]] != test[[paste(f, "M2", sep=".")]]) +
    (test[[paste(f, "M2", sep=".")]] != test[[paste(f, "M3", sep=".")]]) +
    (test[[paste(f, "M3", sep=".")]] != test[[paste(f, "M4", sep=".")]])]
  
  train[, paste("outcome", f, lagInd, sep=".") := NULL]
  test[, paste("outcome", f, lagInd, sep=".") := NULL]
  train[, paste(f, lagInd, sep=".") := NULL]
  test[, paste(f, lagInd, sep=".") := NULL]
}

for (f in setdiff(lagFields, 
                  c(names(train)[startsWith(names(train), "outcome.")], productFlds, paste("outcome", productFlds, sep=".")))) {
  cat("Adding aggregates for lagged fields for",f,fill=T)
  
  train[, c(paste("changes", f, sep=".")) :=
          (train[[paste(f, "M1", sep=".")]] != train[[paste(f, "M2", sep=".")]]) +
          (train[[paste(f, "M2", sep=".")]] != train[[paste(f, "M3", sep=".")]]) +
          (train[[paste(f, "M3", sep=".")]] != train[[paste(f, "M4", sep=".")]])]
  
  test[, c(paste("changes", f, sep=".")) :=
         (test[[paste(f, "M1", sep=".")]] != test[[paste(f, "M2", sep=".")]]) +
         (test[[paste(f, "M2", sep=".")]] != test[[paste(f, "M3", sep=".")]]) +
         (test[[paste(f, "M3", sep=".")]] != test[[paste(f, "M4", sep=".")]])]

  train[, paste(f, lagInd, sep=".") := NULL]
  test[, paste(f, lagInd, sep=".") := NULL]
}

# TODO maybe trending like M1 > M4 etc?


# Only now subset to the train month !!

train <- train[fecha_dato == trainDateNr,]

# Drop the current month portfolio (won't exist in train set...) and rename the outcomes
for (f in productFlds) {
  train[[f]] <- NULL
}
setnames(train, paste("outcome", productFlds, sep="."), productFlds)

uniqueBefore <- length(unique(train$ncodpers))
cat("Train size before melting:", dim(train), "; unique persons:",uniqueBefore,fill = T)
train <- melt(train, 
              id.vars = setdiff(names(train), productFlds), 
              measure.vars = productFlds,
              variable.name = "product", value.name = "action")

train <- train[action == 1,] # only keep the additions
train$action <- NULL
uniqueAfter <- length(unique(train$ncodpers))
cat("Train size after melting:", dim(train), "; unique persons:",uniqueAfter,fill = T)

productDistributions <-
  group_by(train, product) %>%
  dplyr::summarise(additions = n(), 
                   additions.rel = additions/nrow(train)) %>%
  mutate(dataset = "Train Set") %>%
  arrange(additions.rel)

print(ggplot(productDistributions, 
             aes(factor(product, levels=unique(productDistributions$product)),additions.rel,label=additions))+
        geom_bar(stat="identity",position="dodge",fill="blue")+coord_flip()+
        geom_text()+
        scale_y_continuous(labels = percent)+ggtitle("Additions by product in train set")+
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

# Model building
predictorFlds <- names(train)[which((!names(train) %in% c("product", productFlds)) &
                                       !startsWith(names(train),"outcome."))]

# TODO consider to exclude these
# # Based on plots, drop these
# differentDistros <- c("diff_fecha_dato_ult_fec_cli_1t", "ult_fec_cli_1t",
#                       "tiprel_1mes")


# Caret hyperparameter tuning
if (do.HyperTuning) {
  caretHyperParamSearch <- trainControl(method = "cv", number=5, 
                                        classProbs = TRUE,
                                        summaryFunction = mnLogLoss,
                                        verbose=T)
  searchGrid <- expand.grid(nrounds = 500, #seq(200, 800,by=100),
                            eta = seq(0.03,0.05,by=0.01), #0.04, #seq(0.02, 0.08, by=0.02),
                            max_depth = 4:6, #5, # 4:7,
                            gamma = c(0, 1:3), # 0:5, #2,
                            colsample_bytree = 1,
                            min_child_weight = 0:2, # 0:5, #1,
                            subsample = 1)
  predictors <- data.matrix(train[, predictorFlds, with=F])
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
}

# Build multiclass model
xgb.params <- list(objective = "multi:softprob",
                   eval_metric = "mlogloss", # i really want "map@7" but get errors
                   max.depth = 5,
                   num_class = length(productFlds),
                   eta = 0.04,
                   gamma = 2, 
                   colsample_bytree = 1, 
                   min_child_weight = 0,
                   subsample = 1)

# See https://github.com/dmlc/xgboost/blob/master/R-package/demo/custom_objective.R 
# for custom error function

trainMatrix <- xgb.DMatrix(data.matrix(train[, predictorFlds, with=F]), 
                           missing=NaN, 
                           label=as.integer(train$product)-1)
if (do.CV) {
  cvresults <- xgb.cv(params=xgb.params, data = trainMatrix, missing=NaN,
                      nrounds=cv.rounds,
                      nfold=cv.nfold,
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
}

bst = xgb.train(params=xgb.params, data = trainMatrix, missing=NaN,
                watchlist=list(train=trainMatrix),
                nrounds=xgb.rounds, 
                maximize=F)

# Compute & plot feature importance matrix & summary tree
print("Feature importance matrix...")
importance_matrix <- xgb.importance(predictorFlds, model = bst)
print(importance_matrix)
print(xgb.plot.importance(head(importance_matrix, min(20, nrow(importance_matrix)))))

# Version 0.6 will support this
# xgb.plot.tree(feature_names = dimnames(trainMatrix)[[2]], model = bst, n_first_tree = 2)
# xgb.plot.multi.trees(model = bst, feature_names = dimnames(trainMatrix)[[2]], features.keep = 3)

if (do.testSetOnly) {
  stop("Not scoring - test mode")
}

print("Train predictions...")
xgbpred <- predict(bst, trainMatrix, missing=NaN)
trainProbabilities <- t(matrix(xgbpred, nrow=length(productFlds), ncol=nrow(train)))
colnames(trainProbabilities) <- productFlds

print("Test predictions...")
testMatrix <- xgb.DMatrix(data.matrix(test[, predictorFlds, with=F]), missing=NaN)
xgbpred <- predict(bst, testMatrix, missing=NaN)
testProbabilities <- t(matrix(xgbpred, nrow=length(productFlds), ncol=nrow(test)))
colnames(testProbabilities) <- productFlds

# TODO: consider this
# Force probabilities to 0 for products already in portfolio of previous month

# Viz distributions together with the earlier ones from the actual data
print("Test/Train distributions")

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

