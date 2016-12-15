# Kaggle Santander 2 

# Interaction Aggregatation preparations


source("santa2.R")

train <- fread(paste(data_folder,"train_ver2.csv",sep="/"), colClasses = data_colClasses)
productFlds <- names(train)[grepl("^ind_.*ult1$",names(train))]

train <- train[, names(train) %in% c("fecha_dato","ncodpers",productFlds), with=F]
train$fecha_dato <- toMonthNr(train$fecha_dato)
setkey(train, fecha_dato, ncodpers)


# TODO: consider not only product fields but also a (selected) few others
# # Convert character fields to factor levels
# for (f in names(train)[!sapply(train, is.numeric)]) {
#   train[[f]] <- as.integer(factor(train[[f]]))
# }

# Aggregate: last months portfolio and size of portfolio
interactionAggregates <- train[fecha_dato %in% (c(trainDateNr,testDateNr)-1), ]
interactionAggregates$fecha_dato <- interactionAggregates$fecha_dato+1
interactionAggregates[ , lag.M1.portfolio.size := rowSums(.SD == 0, na.rm=T), .SDcols = productFlds]
setnames(interactionAggregates, productFlds, paste("lag.M1",productFlds,sep="."))
setkeyv(interactionAggregates, key(train))

lag.M2 <- train[fecha_dato %in% (c(trainDateNr,testDateNr)-2), ]
lag.M2$fecha_dato <- lag.M2$fecha_dato+2
lag.M2[ , lag.M2.portfolio.size := rowSums(.SD == 0, na.rm=T), .SDcols = productFlds]
setnames(lag.M2, productFlds, paste("lag.M2",productFlds,sep="."))
setkeyv(lag.M2, key(train))
interactionAggregates <- merge(interactionAggregates, lag.M2, all.x=TRUE)

lag.M3 <- train[fecha_dato %in% (c(trainDateNr,testDateNr)-2), ]
lag.M3$fecha_dato <- lag.M3$fecha_dato+2
lag.M3[ , lag.M3.portfolio.size := rowSums(.SD == 0, na.rm=T), .SDcols = productFlds]
setnames(lag.M3, productFlds, paste("lag.M3",productFlds,sep="."))
setkeyv(lag.M3, key(train))
interactionAggregates <- merge(interactionAggregates, lag.M3, all.x=TRUE)

lag.M4 <- train[fecha_dato %in% (c(trainDateNr,testDateNr)-4), ]
lag.M4$fecha_dato <- lag.M4$fecha_dato+4
lag.M4[ , lag.M4.portfolio.size := rowSums(.SD == 0, na.rm=T), .SDcols = productFlds]
setnames(lag.M4, productFlds, paste("lag.M4",productFlds,sep="."))
setkeyv(lag.M4, key(train))
interactionAggregates <- merge(interactionAggregates, lag.M4, all.x=TRUE)

# Some trend-like variables
# There could be more like M1.portfoliosize > M4.portfoliosize etc.
# for others than outcome this could include the current month
interactionAggregates[["lag.portfolio.maxchange"]] <- 
  abs(pmin(interactionAggregates$lag.M4.portfolio.size, interactionAggregates$lag.M3.portfolio.size, interactionAggregates$lag.M2.portfolio.size, interactionAggregates$lag.M1.portfolio.size, na.rm=T) -
        pmax(interactionAggregates$lag.M4.portfolio.size, interactionAggregates$lag.M3.portfolio.size, interactionAggregates$lag.M2.portfolio.size, interactionAggregates$lag.M1.portfolio.size, na.rm=T))
interactionAggregates[["lag.portfolio.avgsize"]] <- 
  (interactionAggregates$lag.M4.portfolio.size + interactionAggregates$lag.M3.portfolio.size + interactionAggregates$lag.M2.portfolio.size + interactionAggregates$lag.M1.portfolio.size)/4
interactionAggregates[["lag.portfolio.difftomean"]] <- 
  (interactionAggregates$lag.M4.portfolio.size + interactionAggregates$lag.M3.portfolio.size + interactionAggregates$lag.M2.portfolio.size + interactionAggregates$lag.M1.portfolio.size)/4 -
  mean(c(interactionAggregates$lag.M4.portfolio.size, interactionAggregates$lag.M3.portfolio.size, interactionAggregates$lag.M2.portfolio.size, interactionAggregates$lag.M1.portfolio.size), na.rm=T)
interactionAggregates[["lag.portfolio.changes"]] <- 
  (interactionAggregates$lag.M4.portfolio.size != interactionAggregates$lag.M3.portfolio.size) +
  (interactionAggregates$lag.M3.portfolio.size != interactionAggregates$lag.M2.portfolio.size) + 
  (interactionAggregates$lag.M2.portfolio.size != interactionAggregates$lag.M1.portfolio.size)

print(ggplot(gather(select(interactionAggregates, ends_with(".portfolio.size")), lag, size), 
             aes(size, fill=factor(lag)))+
        geom_bar(position = "dodge")+ggtitle("Portfolio size for target dates"))

print(unique(train$fecha_dato))

# Set outcome by subtracting last month portfolio
train$next_month <- train$fecha_dato+1
train <- merge(train, train[, c("ncodpers", "next_month", productFlds), with=F], 
               all.x=F, all.y=F, 
               by.x = c("ncodpers", "fecha_dato"),
               by.y = c("ncodpers", "next_month")) # inner self join - only target date will remain
train[, next_month := NULL]

print("Date range:")
print(unique(train$fecha_dato)) # first month will be gone
cat("train date:",trainDateNr,"test date:",testDateNr,fill=T)

# Replace portfolio by the product changes (current month - last month)
for (f in productFlds) {
  train[[f]] <- ifelse(is.na(train[[paste(f,"y",sep=".")]]),
                       train[[paste(f,"x",sep=".")]],
                       train[[paste(f,"x",sep=".")]] - train[[paste(f,"y",sep=".")]])
  train[[paste(f,"x",sep=".")]] <- NULL
  train[[paste(f,"y",sep=".")]] <- NULL
}

# Number of additions across all products
print("Row sums of number of additions etc")
train[ , n.additions := rowSums(.SD == 1, na.rm=T), .SDcols = productFlds]
train[ , n.removals  := rowSums(.SD == -1, na.rm=T), .SDcols = productFlds]
train[ , n.change    := rowSums(.SD != 0, na.rm=T), .SDcols = productFlds]

# Aggregate to person level
# TODO consider clustering persons on this and other attributes (or in the main file)
# customerAggregates <- train[ , list(person.additions=sum(n.additions, na.rm = T),
#                                     person.removals=sum(n.removals, na.rm = T),
#                                     person.change=sum(n.change, na.rm = T)), by=ncodpers]
# setkey(customerAggregates, ncodpers)
# interactionAggregates <- merge(interactionAggregates, customerAggregates, all.x = T, by = "ncodpers")

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

cat("Writing to ", data_folder, fill=T)
write.csv(interactionAggregates, paste(data_folder,"interactionAggregates.csv",sep="/"), row.names = F)

