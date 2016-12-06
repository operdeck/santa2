# Kaggle Santander 2 

# Interaction Aggregatation preparations

# The key is to use lag features of products owned in previous months. Try include few lags of every product and you will see some improvement; When I started, I used lags of order 5 to reach 0.030; also don't forget to include lags of non-product related fields, i.e. lag of a segment, lag of age, etc.
# 
# with correct feature space, 5-minute model is enough to reach 0.030. Good luck :)
# https://www.kaggle.com/c/santander-product-recommendation/forums/t/25579/when-less-is-more?page=3


source("santa2.R")

history <- fread(paste(data_folder,"train_ver2.csv",sep="/"), colClasses = data_colClasses)
productFlds <- names(history)[grepl("^ind_.*ult1$",names(history))]

# history <- history[, names(history) %in% c("fecha_dato","ncodpers",productFlds), with=F]
history$fecha_dato <- toMonthNr(history$fecha_dato)
setkeyv(history, c("fecha_dato","ncodpers"))

maxHistoryWindow <- trainDateNr-min(history$fecha_dato)

# Build interaction aggregates for the target train/test dates only
interactionAggregates <- history[fecha_dato %in% c(trainDateNr,testDateNr), key(history), with=F]
setkeyv(interactionAggregates, key(history))

# Join portfolio of previous month
# for (h in c(1,maxHistoryWindow)) {
#   prevPortfolio <- history[fecha_dato %in% (c(trainDateNr,testDateNr) - h),,]
#   prevPortfolio$fecha_dato <- h + prevPortfolio$fecha_dato
#   prevPortfolio[["size"]] <- rowSums(prevPortfolio[,productFlds,with=F], na.rm=T)
#   setkeyv(prevPortfolio, key(interactionAggregates))
#   setnames(prevPortfolio, 
#            setdiff(names(prevPortfolio),key(prevPortfolio)),
#            paste("portfolio",setdiff(names(prevPortfolio),key(prevPortfolio)), "M", h, sep="."))
#   interactionAggregates <- merge(interactionAggregates, prevPortfolio, all.x = T)
# }
# 
# print(ggplot(gather(interactionAggregates, history, size, starts_with(("portfolio.size"))), 
#              aes(y=size, x=history))+
#         geom_boxplot()+ggtitle("Last month portfolio size for target dates"))

print(unique(history$fecha_dato))

# Set outcome by subtracting last month portfolio
train2 <- history
train2$fecha_dato <- train2$fecha_dato+1
setkey(train2, fecha_dato, ncodpers)
history <- merge(history, train2, all=FALSE) # left merge(X, Y, all.x=TRUE)
rm(train2)

print("Date range:")
print(unique(history$fecha_dato)) # first month will be gone
cat("history date:",trainDateNr,"test date:",testDateNr,fill=T)

# Replace portfolio by the product changes (current month - last month)
for (f in productFlds) {
  history[[f]] <- ifelse(is.na(history[[paste(f,"y",sep=".")]]),
                         history[[paste(f,"x",sep=".")]],
                         history[[paste(f,"x",sep=".")]] - history[[paste(f,"y",sep=".")]])
  history[[paste(f,"x",sep=".")]] <- NULL
  history[[paste(f,"y",sep=".")]] <- NULL
}

# Number of additions across all products
print("Row sums of number of additions etc")
history[ , n.additions := rowSums(.SD == 1, na.rm=T), .SDcols = productFlds]
history[ , n.removals  := rowSums(.SD == -1, na.rm=T), .SDcols = productFlds]
history[ , n.changes   := rowSums(.SD != 0, na.rm=T), .SDcols = productFlds]

# Aggregate: number of additions etc across all products in the last N months
for (h in c(1,maxHistoryWindow)) {
  cat("Calculating nr of changes for last",h,"months",fill=T)
  # NB could be faster if needed
  # other aggregates here as well?
  tmp <- rbind(history[ fecha_dato >= (trainDateNr-h) & fecha_dato <= (trainDateNr-1), 
                      list(fecha_dato = trainDateNr, 
                           n.additions = sum(n.additions, na.rm=T), 
                           n.removals = sum(n.removals, na.rm=T), 
                           n.changes = sum(n.changes, na.rm=T)), by=ncodpers ],
               history[ fecha_dato >= (testDateNr-h) & fecha_dato <= (testDateNr-1), 
                      list(fecha_dato = testDateNr, 
                           n.additions = sum(n.additions, na.rm=T), 
                           n.removals = sum(n.removals, na.rm=T), 
                           n.changes = sum(n.changes, na.rm=T)), by=ncodpers ])
  
  setnames(tmp, 
           names(tmp)[3:length(names(tmp))], 
           paste(names(tmp)[3:length(names(tmp))], "M", h, sep="."))
  setkeyv(tmp, key(interactionAggregates))
  
  interactionAggregates <- merge(interactionAggregates, tmp, all.x=TRUE)
}

# Global nr of additions etc over whole period by person
totalsByPerson <-
  history[ ,list(total.person.additions = sum(n.additions, na.rm=T), 
               total.person.removals = sum(n.removals, na.rm=T), 
               total.person.changes = sum(n.changes, na.rm=T)), by=ncodpers ]
setkey(totalsByPerson, ncodpers)
interactionAggregates <- merge(interactionAggregates, totalsByPerson, all.x=TRUE)

# TODO: consider a PCA or clustering of persons
for (f in names(interactionAggregates)) {
  if (!is.numeric(interactionAggregates[[f]])) {
    interactionAggregates[[f]] <- as.integer(factor(interactionAggregates[[f]]))
  }
}

print(summary(interactionAggregates))
print(dim(interactionAggregates))

write.csv(interactionAggregates, paste(data_folder,"interactionAggregates.csv",sep="/"), row.names = F)

