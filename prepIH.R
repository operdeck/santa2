# Kaggle Santander 2 

# Interaction Aggregatation preparations


source("santa2.R")

train <- fread(paste(data_folder,"train_ver2.csv",sep="/"), colClasses = data_colClasses)
productFlds <- names(train)[grepl("^ind_.*ult1$",names(train))]

train <- train[, names(train) %in% c("fecha_dato","ncodpers",productFlds), with=F]
train$fecha_dato <- toMonthNr(train$fecha_dato)
setkey(train, fecha_dato, ncodpers)

# Add last month portfolio into the train & test aggregates as our first batch of extra variables
trainInteractionAggregates <- train[fecha_dato == trainDateNr-1, ]
testInteractionAggregates <- train[fecha_dato == testDateNr-1, ]
setnames(trainInteractionAggregates, productFlds, paste("lastmonth",productFlds,sep="."))
setnames(testInteractionAggregates, productFlds, paste("lastmonth",productFlds,sep="."))

print(unique(train$fecha_dato))

train2 <- train
train2$fecha_dato <- train2$fecha_dato+1
setkey(train2, fecha_dato, ncodpers)

train <- merge(train, train2, all=FALSE) # left merge(X, Y, all.x=TRUE)
rm(train2)

print("Date range:")
print(unique(train$fecha_dato)) # first month will be gone
cat("train date:",trainDateNr,"test date:",testDateNr,fill=T)

# Replace portfolio by the product changes (current month - last month)
for (f in productFlds) {
  train[[paste("change",f,sep=".")]] <- train[[paste(f,"x",sep=".")]] - train[[paste(f,"y",sep=".")]]
  train[[paste(f,"x",sep=".")]] <- NULL
  train[[paste(f,"y",sep=".")]] <- NULL
}

# Number of additions
train[ , n.additions := rowSums(.SD == 1, na.rm=T), .SDcols = paste("change",productFlds,sep=".")]
train[ , n.removals  := rowSums(.SD == -1, na.rm=T), .SDcols = paste("change",productFlds,sep=".")]
train[ , n.nochange  := rowSums(.SD == 0, na.rm=T), .SDcols = paste("change",productFlds,sep=".")]





