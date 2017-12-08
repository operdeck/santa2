library(tidyverse)

# app "santa" on pegalabs http://10.60.215.32:9080/prweb/PRServlet
# access group Santa:Administrators
# Data-Customer-SantaSnapshot : train & test customer ID + snapshot dates
# Data-Customer-SantaProducts : all 24 products. Datasets available with current portfolio (last month) and for training, the additions in curr month
# https://www.kaggle.com/c/santander-product-recommendation/data

data_folder <- "data"
# data_folder <- "data-unittest"

data_colClasses <- list(character=c("ult_fec_cli_1t","indrel_1mes","conyuemp"))
data_dateFlds <- c("fecha_dato","fecha_alta","ult_fec_cli_1t")

trainDate <- c('2015-06-28')
testDate <- c('2016-06-28')

toMonthNr <- function(str)
{
  strAsDate <- fasttime::fastPOSIXct(str)
  return((year(strAsDate)-2015)*12 + month(strAsDate))
}

train <- fread(paste(data_folder,"train_ver2.csv",sep="/"), colClasses = data_colClasses)
test <- fread(paste(data_folder,"test_ver2.csv",sep="/"), colClasses = data_colClasses)

# This field antiguedad is not set consistently. It is the same (+/- 1) as diff.fecha_dato_fecha_alta anyway.
train[["antiguedad"]] <- NULL
test[["antiguedad"]] <- NULL

allPersonz <- unique(train$ncodpers)
cat("Train size:", dim(train), "; unique persons:",length(allPersonz),fill = T)

print("Dates")
for(f in intersect(data_dateFlds, names(train))) { 
  train[[f]] <- toMonthNr(train[[f]])
  test[[f]] <- toMonthNr(test[[f]])
}

# generate separate files
# - additions june 2015 vs may 2015 (= outcomes)
# - portfolio may 2015 & may 2016 (= predictors)
# - profile june 2015 & june 2016 (= predictors)

outcomefields <- sort(setdiff(names(train), names(test)))
june2015joinedwithprevious <- merge(train[fecha_dato == toMonthNr('2015-06-28'), c("ncodpers", outcomefields), with=F], 
                                    train[fecha_dato == toMonthNr('2015-05-28'), c("ncodpers", outcomefields), with=F], by="ncodpers")
additions <- data.table(sapply(outcomefields, function(f) {return(june2015joinedwithprevious[[paste(f,"x",sep=".")]] - 
                                                                            june2015joinedwithprevious[[paste(f,"y",sep=".")]])}))
additions[["ncodpers"]] <- june2015joinedwithprevious[["ncodpers"]]
additions[["fecha_dato"]] <- toMonthNr('2015-06-28')

portfolio <- rbindlist(list(train[fecha_dato == toMonthNr('2015-05-28'), c("ncodpers", "fecha_dato", outcomefields), with=F],
                         train[fecha_dato == toMonthNr('2016-05-28'), c("ncodpers", "fecha_dato", outcomefields), with=F]))
portfolio[["fecha_dato"]] <- portfolio[["fecha_dato"]]+1
# setorder(portfolio, ncodpers) # nice for testing stuff but in real life want to keep the order so remove this again!!

profile <- rbindlist(list(train[fecha_dato == toMonthNr('2015-06-28'), names(test), with=F], test))


# PRPC doesnt deal with NA's, set special value
portfolio[is.na(portfolio)] <- 9999
additions[is.na(additions)] <- 9999
profile[is.na(profile)] <- 9999

write.csv(portfolio[, c("ncodpers", "fecha_dato")], paste(data_folder,"santa_snapshots.csv",sep="/"), row.names=F)
write.csv(portfolio, paste(data_folder,"santa_portfolio.csv",sep="/"), row.names=F)
write.csv(additions, paste(data_folder,"santa_additions.csv",sep="/"), row.names=F)
write.csv(profile, paste(data_folder,"santa_profile.csv",sep="/"), row.names=F)

print("Expected base propensities")
print(sapply(outcomefields, function(x) {return(sprintf("%.4f %%",100*length(which(additions[[x]]==1))/nrow(additions)))}))

# 18603/6 is a good test case : 1 addition
