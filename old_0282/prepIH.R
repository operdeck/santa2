# Kaggle Santander 2 

# Interaction Aggregatation preparations


source("santa2.R")

train <- fread(paste(data_folder,"train_ver2.csv",sep="/"), colClasses = data_colClasses)
productFlds <- names(train)[grepl("^ind_.*ult1$",names(train))]

train <- train[, names(train) %in% c("fecha_dato","ncodpers",productFlds), with=F]
train$fecha_dato <- toMonthNr(train$fecha_dato)
setkey(train, fecha_dato, ncodpers)

# Aggregate: last month portfolio and size of portfolio
interactionAggregates <- train[fecha_dato %in% (c(trainDateNr,testDateNr)-1), ]
interactionAggregates$fecha_dato <- interactionAggregates$fecha_dato+1
interactionAggregates[ , lastmonth.portfolio.size := rowSums(.SD == 0, na.rm=T), .SDcols = productFlds]
setnames(interactionAggregates, productFlds, paste("lastmonth",productFlds,sep="."))

print(ggplot(interactionAggregates, aes(factor(lastmonth.portfolio.size), fill=factor(fecha_dato)))+
        geom_bar()+ggtitle("Last month portfolio size for target dates"))

print(unique(train$fecha_dato))

# Set outcome by subtracting last month portfolio
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
  train[[f]] <- train[[paste(f,"x",sep=".")]] - train[[paste(f,"y",sep=".")]]
  train[[paste(f,"x",sep=".")]] <- NULL
  train[[paste(f,"y",sep=".")]] <- NULL
}

# Number of additions across all products
print("Row sums of number of additions etc")
train[ , n.additions := rowSums(.SD == 1, na.rm=T), .SDcols = productFlds]
train[ , n.removals  := rowSums(.SD == -1, na.rm=T), .SDcols = productFlds]
train[ , n.change    := rowSums(.SD != 0, na.rm=T), .SDcols = productFlds]

# Aggregate: number of additions etc across all products in the last N months
setkey(interactionAggregates, fecha_dato, ncodpers)
maxHistoryWindow <- trainDateNr-min(train$fecha_dato)
for (h in 1:maxHistoryWindow) {
  cat("Calculating nr of changes for last",h,"months",fill=T)
  # NB could be faster if needed
  # other aggregates here as well?
  tmp <- rbind(train[ fecha_dato >= (trainDateNr-h) & fecha_dato <= (trainDateNr-1), 
                      list(fecha_dato = trainDateNr, n.additions = sum(n.additions, na.rm=T), n.removals = sum(n.removals, na.rm=T), n.changes = sum(n.change, na.rm=T)), by=ncodpers ],
               train[ fecha_dato >= (testDateNr-h) & fecha_dato <= (testDateNr-1), 
                      list(fecha_dato = testDateNr, n.additions = sum(n.additions, na.rm=T), n.removals = sum(n.removals, na.rm=T), n.changes = sum(n.change, na.rm=T)), by=ncodpers ])
  
  setnames(tmp, names(tmp)[3:length(names(tmp))], paste(names(tmp)[3:length(names(tmp))], ".M", h, sep=""))
  setkey(tmp, fecha_dato, ncodpers)
  
  interactionAggregates <- merge(interactionAggregates, tmp, all.x=TRUE)
}

write.csv(interactionAggregates, paste(data_folder,"interactionAggregates.csv",sep="/"), row.names = F)

