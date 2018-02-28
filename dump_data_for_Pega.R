# This script is in support of the Pega application that compares various
# ADM configurations to train models for the Kaggle Santander dataset.
#
# It will generate a number of CSV files that are imported in the app and
# which are used to compose a complete "customer" record per snapshot month:
#
# - santa_snapshots : just the list of customer ID and and snapshot month 
# - santa_portfolio : current portfolio of customer at snapshot month
# - santa_additions : the product additions (used to drive the outcomes of the models)
# - santa_profile   : the customer profile at that moment
#
# These need to be uploaded to PRPC in the corresponding file datasets. Then, in PRPC,
# they should be "converted" to DDS datasets (copied) so they can be used as secondary
# source and support batch partitions.
#
# After these prep steps, the main data flow that composes them and trains and/or runs models
# can be kicked off. The main flow lives in "Data-Customer-SantaSnapshot" and composes the
# customer into a couple of embedded pages.


# this file really lives in Documents/competitions/kaggle_santander

# app "santa" on pegalabs http://10.60.215.32:9080/prweb/PRServlet
# access group Santa:Administrators
# Data-Customer-SantaSnapshot : train & test customer ID + snapshot dates
# Data-Customer-SantaProducts : all 24 products. Datasets available with current portfolio (last month) and for training, the additions in curr month
# https://www.kaggle.com/c/santander-product-recommendation/data

library(plyr)
library(tidyverse)

data_folder <- "data"
# data_folder <- "data-unittest"

data_colClasses <- list(character=c("ult_fec_cli_1t","indrel_1mes","conyuemp"))
data_dateFlds <- c("Snapshot","fecha_alta","ult_fec_cli_1t")

# Not used currently - can do if doing the original competition
# trainDate <- c('2015-06-28')
# testDate <- c('2016-06-28')

toMonthNr <- function(str)
{
  strAsDate <- fasttime::fastPOSIXct(str)
  return((year(strAsDate)-2015)*12 + month(strAsDate))
}

train <- fread(paste(data_folder,"train_ver2.csv",sep="/"), colClasses = data_colClasses)
test <- fread(paste(data_folder,"test_ver2.csv",sep="/"), colClasses = data_colClasses)

# This field antiguedad is not set consistently. It is the same (+/- 1) as diff.Snapshot_fecha_alta anyway.
train[, antiguedad := NULL]
test[, antiguedad := NULL]

# Rename customer ID & snapshot date for ease of use
setnames(train, revalue(names(train), c("ncodpers"="CustomerID", "fecha_dato"="Snapshot")))
setnames(test, revalue(names(test), c("ncodpers"="CustomerID", "fecha_dato"="Snapshot")))

cat("Train original size:", dim(train), "; unique persons:",length(unique(train$CustomerID)),fill = T)

# Sample only a subset of the customers - otherwise things get too big for PRPC

sampleCustomers <- sample(test$CustomerID, 0.10*length(unique(train$CustomerID)), replace=F)
train <- train[CustomerID %in% sampleCustomers]
test <- test[CustomerID %in% sampleCustomers]

cat("Train sampled size:", dim(train), "; unique persons:",length(unique(train$CustomerID)),fill = T)

print("Convert dates to numbers")
for(f in intersect(data_dateFlds, names(train))) { 
  train[[f]] <- toMonthNr(train[[f]])
  test[[f]] <- toMonthNr(test[[f]])
}

print("Extract portfolio")

outcomefields <- sort(setdiff(names(train), names(test)))
setkey(train, CustomerID, Snapshot)

# Portfolio is just the products you already had last month
portfolio <- train[, c("CustomerID", "Snapshot", outcomefields), with=F]
setkey(portfolio, CustomerID, Snapshot)
portfolio[, Snapshot := Snapshot + 1]

print("Find additions")

# Find product additions by substracting previous portfolio from current portfolio
joinedwithprev <- merge(train, portfolio, suffixes=c(".current", ".previous"))
additions <- data.table(sapply(outcomefields, function(f) {return(joinedwithprev[[paste(f,"current",sep=".")]] - 
                                                                    joinedwithprev[[paste(f,"previous",sep=".")]])}))
additions$CustomerID <- joinedwithprev$CustomerID
additions$Snapshot <- joinedwithprev$Snapshot

print("Expected base propensities")
print(sapply(outcomefields, function(x) {return(sprintf("%.4f %%",100*length(which(additions[[x]]==1))/nrow(additions)))}))

print("Extract profile")

# Profile is really just the current customer snapshot, using the names of the test set as these don't include current portfolio
profile <- rbindlist(list(train[, names(test), with=F], test))

# PRPC doesnt deal with NA's, set special value
portfolio[is.na(portfolio)] <- -9999
additions[is.na(additions)] <- -9999
profile[is.na(profile)] <- -9999

print("Dump files")

write.csv(portfolio[, c("CustomerID", "Snapshot")], paste(data_folder,"santa_snapshots.csv",sep="/"), row.names=F)
write.csv(portfolio, paste(data_folder,"santa_currentportfolio.csv",sep="/"), row.names=F)
write.csv(additions, paste(data_folder,"santa_additions.csv",sep="/"), row.names=F)
write.csv(profile, paste(data_folder,"santa_profile.csv",sep="/"), row.names=F)

print("Done")

# 18603/6 is a good test case : 1 addition
