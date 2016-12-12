# Kaggle Santander 2 
# predictions without models, purely based on apriori probabilities

library(data.table)
library(fasttime)

data_folder <- "../data"
data_colClasses <- list(character=c("ult_fec_cli_1t","indrel_1mes","conyuemp"))
train <- fread(paste(data_folder,"train_ver2.csv",sep="/"), colClasses = data_colClasses)
test <- fread(paste(data_folder,"test_ver2.csv",sep="/"), colClasses = data_colClasses)
productFlds <- names(train)[grepl("^ind_.*ult1$",names(train))] # products purchased
train <- train[, c("ncodpers","fecha_dato",productFlds), with=F]

train$fecha_dato <- fastPOSIXct(train$fecha_dato)
test$fecha_dato <- fastPOSIXct(test$fecha_dato)
train$monthnr <- month(train$fecha_dato)+ 12*year(train$fecha_dato)-1
test$monthnr <- month(test$fecha_dato)+ 12*year(test$fecha_dato)-1
train$nextmonthnr <- 1+train$monthnr

train <- merge(train, train, by.x=c("ncodpers","monthnr"), by.y=c("ncodpers","nextmonthnr"))
d1 <- as.matrix( train[, paste(productFlds, "x", sep="."), with=F])
d2 <- as.matrix( train[, paste(productFlds, "y", sep="."), with=F])
productAprioris <- colSums((d1 == 1) & (is.na(d2) | (d2 == 0)), na.rm = T) / colSums(!is.na(d1) & !is.na(d2)) 
# names(productAprioris) <- productFlds
# productAprioris <- sort(productAprioris,decreasing = T)

test <- merge(test[, c("ncodpers","monthnr"), with=F], 
              train[, c("ncodpers","nextmonthnr",paste(productFlds, "x", sep=".")), with=F], 
              by.x=c("ncodpers","monthnr"), by.y=c("ncodpers","nextmonthnr"),
              all.x = T, all.y = F)
setnames(test, paste(productFlds, "x", sep="."), productFlds)

xxx <- apply( 1-as.matrix(test[1:8, productFlds, with=F]), 1, "*", productAprioris)
apply(xxx, 2, function(col) {
  paste(names(sort(rank(-col, ties.method = "first")))[1:7], collapse=" ") })

# now just create the DF from this

