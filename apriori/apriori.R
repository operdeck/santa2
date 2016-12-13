# Kaggle Santander 2 
# predictions without models, purely based on a priori probabilities
# Scores 0.0183025 on LB using all months
# ... using May and June
# https://www.kaggle.com/operdeck/santander-product-recommendation/predictions-without-models

library(data.table)
library(fasttime)

# Read data

data_folder <- "../data"
data_colClasses <- list(character=c("ult_fec_cli_1t","indrel_1mes","conyuemp"))
train <- fread(paste(data_folder,"train_ver2.csv",sep="/"), colClasses = data_colClasses)
test <- fread(paste(data_folder,"test_ver2.csv",sep="/"), colClasses = data_colClasses)
productFlds <- names(train)[grepl("^ind_.*ult1$",names(train))] # products purchased
train <- train[fecha_dato %in% c("2015-05-28","2015-06-28","2016-04-28","2016-05-28"), 
               c("ncodpers","fecha_dato",productFlds), with=F]

train$fecha_dato <- fastPOSIXct(train$fecha_dato)
test$fecha_dato <- fastPOSIXct(test$fecha_dato)
train$monthnr <- month(train$fecha_dato)+ 12*year(train$fecha_dato)-1
test$monthnr <- month(test$fecha_dato)+ 12*year(test$fecha_dato)-1

# Self-merge so previous month is next to current month

train$nextmonthnr <- 1+train$monthnr
train <- merge(train, train, by.x=c("ncodpers","monthnr"), by.y=c("ncodpers","nextmonthnr"))

# Outcomes are products in portfolio this month but not in previous

d1 <- as.matrix( train[, paste(productFlds, "x", sep="."), with=F])
d2 <- as.matrix( train[, paste(productFlds, "y", sep="."), with=F])
aPrioris <- colSums((d1 == 1) & (is.na(d2) | (d2 == 0)), na.rm = T) / colSums(!is.na(d1) & !is.na(d2)) 
names(aPrioris) <- productFlds
print(aPrioris)

# Merge the test set with the last month from the train set so we can null out the
# probabilities for products already owned, otherwise set them to the a priori probabilities

test <- merge(test[, c("ncodpers","monthnr"), with=F], 
              train[, c("ncodpers","nextmonthnr",paste(productFlds, "x", sep=".")), with=F], 
              by.x=c("ncodpers","monthnr"), by.y=c("ncodpers","nextmonthnr"),
              all.x = T, all.y = F)
setnames(test, paste(productFlds, "x", sep="."), productFlds)

probs <- apply( 1-as.matrix(test[, productFlds, with=F]), 1, "*", aPrioris)

# Just for verification, check the resulting probabilities
aPosterioris <- rowSums(apply(-probs, 2, rank, ties.method = "first") <= 7) / ncol(probs)
print(cor(aPosterioris, aPrioris))

# Create the submission file. Take only the first 7 predictions because of the map@7 evaluation

testResults <- data.frame(ncodpers = test[, ncodpers])
testResults$added_products <- apply(probs, 2, function(col) {
  paste(names(sort(rank(-col, ties.method = "first")))[1:7], collapse=" ") })

submFile <- paste(data_folder,"mysubmission.csv",sep="/")
write.csv(testResults, submFile,row.names = F, quote=F)


