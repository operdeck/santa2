# This script is in support of the Pega application that compares various
# ADM configurations to train models for the Kaggle Santander dataset.
#
# It will generate a number of CSV files that are imported in the app and
# which are used to compose a complete "customer" record per snapshot month:
#
# - santa_snapshots : just the list of customer ID and and snapshot month 
# - santa_portfolio : current portfolio of customer at snapshot month
# - santa_additions : the product additions/removals (used to drive the outcomes of the models)
# - santa_profile   : the customer profile at that moment
# - santa_interactionsIHStyle : just the customer additions/removals in "tall" format: customer ID / time / product / action suitable for IH insertion
#
# These need to be uploaded to PRPC in the corresponding file datasets. Then  in PRPC 
# "converted" to DDS datasets (copied) so they can be used as secondary source and support 
# partitioning. The "Prepare Data" data flow run will do all of this. Monitor the batch 
# processing landing page to verify that all is completed.
#
# Then
# - FillIH (data flow in InteractionsIHStyle) will take the tall format interactions and put them in IH (for months 5..16  so covering 1 year). These
#   IH facts will be used by IH summaries and/or IH imports
# - FillESM (data flow in SantaProducts) will take the broad format additions  runs months 5..16 through ESM and creates 
#   one or more datasets keyed by customerID with aggregates
#
# After these prep steps  the main data flow that composes them and trains and/or runs models
# can be kicked off. The main flow lives in "Data-Customer-SantaSnapshot" and composes the
# customer into a couple of embedded pages.


# this file really lives in Documents/competitions/kaggle_santander

# app "santa" on pegalabs http://10.60.215.32:9080/prweb/PRServlet
# access group Santa:Administrators
# Data-Customer-SantaSnapshot : train & test customer ID + snapshot dates
# Data-Customer-SantaProducts : all 24 products. Datasets available with current portfolio (last month) and for training  the additions in curr month
# https://www.kaggle.com/c/santander-product-recommendation/data

library(plyr)
library(tidyverse)
library(data.table)

set.seed(1966)

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

cat("Train resampled size:", dim(train), "; unique persons:",length(unique(train$CustomerID)),fill = T)

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
print(sort(sapply(outcomefields, function(x) {return(sprintf("%.4f %%",100*length(which(additions[[x]]==1))/nrow(additions)))})))

print("Extract profile")

# Profile is really just the current customer snapshot  using the names of the test set as these don't include current portfolio
profile <- rbindlist(list(train[, names(test), with=F], test))

# PRPC doesnt deal with NA's  set special value
portfolio[is.na(portfolio)] <- -9999
additions[is.na(additions)] <- -9999
profile[is.na(profile)] <- -9999

print("Dump files")

setorder(portfolio, Snapshot, CustomerID)
setorder(additions, Snapshot, CustomerID)
setorder(profile, Snapshot, CustomerID)

cat("Snapshots:", nrow(portfolio), "snapshots", "for", length(unique(portfolio$CustomerID)), "unique customers", fill=T)
cat("Additions:", nrow(additions), "additions", "for", length(unique(additions$CustomerID)), "unique customers", fill=T)
cat("Profile:", uniqueN(select(profile, -Snapshot, -fecha_alta)), "unique profiles", "for", length(unique(profile$CustomerID)), "unique customers", fill=T)

write.csv(portfolio[, c("CustomerID", "Snapshot")], paste(data_folder,"santa_snapshots.csv",sep="/"), row.names=F)
write.csv(portfolio, paste(data_folder,"santa_currentportfolio.csv",sep="/"), row.names=F)
write.csv(additions, paste(data_folder,"santa_additions.csv",sep="/"), row.names=F)
write.csv(profile, paste(data_folder,"santa_profile.csv",sep="/"), row.names=F)

# IH-style version of the additions with customer ID  date  proposition and outcome
interactionsIHStyle <- as.data.table(additions %>% gather(key="Proposition", value="Outcome", -CustomerID, -Snapshot) %>% filter(Outcome != 0))
setorder(interactionsIHStyle, Snapshot, CustomerID)
cat("IH interactions:",nrow(interactionsIHStyle),"additions only:",nrow(interactionsIHStyle[Outcome==1]),fill=T)
write.csv(interactionsIHStyle, paste(data_folder,"santa_interactionsIHStyle.csv",sep="/"), row.names=F)

print("Mid size set")

midsizeCustomers <- sort(sample(portfolio$CustomerID, 1000 ))
write.csv(portfolio[CustomerID %in% midsizeCustomers, c("CustomerID", "Snapshot")], 
          paste(data_folder,paste("santa_snapshots-", length(midsizeCustomers), ".csv", sep=""),sep="/"), row.names=F)
cat("IH interactions for the mid-size set:",nrow(interactionsIHStyle[CustomerID %in% midsizeCustomers]),"additions only:",nrow(interactionsIHStyle[CustomerID %in% midsizeCustomers & Outcome==1]),fill=T)

# This is to help identify customers/snapshots where 1 or more products were added and/or removed at the same time
additions[, nAdditions := rowSums(.SD==1), .SDcols=names(additions)[grepl("ind.*", names(additions))]]
additions[, nRemovals := rowSums(.SD==-1), .SDcols=names(additions)[grepl("ind.*", names(additions))]]

print("Create interaction strings to search for patterns")

fldMapping <- data.table(
  fld = c("ind_ahor_fin_ult1","ind_aval_fin_ult1","ind_cco_fin_ult1","ind_cder_fin_ult1","ind_cno_fin_ult1","ind_ctju_fin_ult1","ind_ctma_fin_ult1","ind_ctop_fin_ult1","ind_ctpp_fin_ult1","ind_deco_fin_ult1","ind_deme_fin_ult1","ind_dela_fin_ult1","ind_ecue_fin_ult1","ind_fond_fin_ult1","ind_hip_fin_ult1","ind_plan_fin_ult1","ind_pres_fin_ult1","ind_reca_fin_ult1","ind_tjcr_fin_ult1","ind_valo_fin_ult1","ind_viv_fin_ult1","ind_nomina_ult1","ind_nom_pens_ult1","ind_recibo_ult1"),
  descr = c("Saving Account","Guarantees","Current Accounts","Derivada Account","Payroll Account","Junior Account", "Más particular Account","particular Account","particular Plus Account","Short-term deposits","Medium-term deposits","Long-term deposits","e-account","Funds","Mortgage","Pensions","Loans","Taxes","Credit Card","Securities","Home Account","Payroll","Pensions","Direct Debit"))
fldMapping$abbr <- letters[1:nrow(fldMapping)]

interactionsOnly <- copy(additions[ nAdditions>0 | nRemovals>0])
setnames(interactionsOnly, mapvalues(tolower(names(interactionsOnly)), from=fldMapping$fld, to=fldMapping$abbr))
i2str <- function(x) {
  paste(toupper(paste(names(x)[x==1],collapse="")), paste(names(x)[x==-1],collapse=""),sep="")
}
interactionsOnly <- interactionsOnly[, interactions := apply(.SD,1,i2str), .SD=fldMapping$abbr][, c("customerid","snapshot","interactions")]
newCustomers <- copy(profile[which(0==(profile$Snapshot - profile$fecha_alta)), c("CustomerID","Snapshot")])
newCustomers$interactions <- "*" # to indicate new customer in the "movie"
setnames(newCustomers,tolower(names(newCustomers)))
interactionsAsStringsByCustomer <- spread(rbind(interactionsOnly, newCustomers),  
                                          value="interactions", key="snapshot", fill="") 
interactionsAsStringsByCustomer[, cbn := apply(.SD,1,paste,collapse="|"), .SDcols=setdiff(names(interactionsAsStringsByCustomer),"customerid")]
View(interactionsAsStringsByCustomer[ grepl("J[^j]*L", cbn, ignore.case = F)]) # example regexp
write.csv(interactionsAsStringsByCustomer,
          paste(data_folder,
                paste("santa_interactionstrings-", nrow(interactionsAsStringsByCustomer), ".csv",sep=""),sep="/"), 
          row.names=F, na = "")

print("Test set")

# This is to help find active customers
setkey(additions, CustomerID)
customerSummary <- additions[, list(nAdditions=sum(nAdditions), nRemovals=sum(nRemovals)), by=c("CustomerID")]

testSetCustomers <- sort(sample( customerSummary[ nAdditions > 1, ]$CustomerID, 10 ))

cat("These customers added one or more products over time:", testSetCustomers, fill=T)
testAdditionsOverview <- data.table(additions[CustomerID %in% testSetCustomers] %>% select(CustomerID, Snapshot, nAdditions) %>% filter(nAdditions>0) %>% spread(Snapshot, nAdditions, fill=0))
testAdditionsOverview$`Sum5-16` <- rowSums(testAdditionsOverview[, which(names(testAdditionsOverview) %in% as.character(seq(from=5,to=16))), with=F])
testAdditionsOverview$`Sum12-16` <- rowSums(testAdditionsOverview[, which(names(testAdditionsOverview) %in% as.character(seq(from=12,to=16))), with=F])
testAdditionsOverview$`Sum-All` <- rowSums(testAdditionsOverview[, which(names(testAdditionsOverview) %in% as.character(seq(from=1,to=18))), with=F])
print(testAdditionsOverview)

print(interactionsIHStyle[CustomerID %in% testSetCustomers & Outcome==1])

write.csv(portfolio[CustomerID %in% testSetCustomers, c("CustomerID", "Snapshot")], 
          paste(data_folder,"santa_snapshots-small.csv",sep="/"), row.names=F)
cat("Nr of test records:", nrow(portfolio[CustomerID %in% testSetCustomers]), fill=T)
cat("Nr of additions in the test set:", sum(additions[CustomerID %in% testSetCustomers] == 1), fill=T)
cat("IH interactions for the test set:",nrow(interactionsIHStyle[CustomerID %in% testSetCustomers]),"additions only:",nrow(interactionsIHStyle[CustomerID %in% testSetCustomers & Outcome==1]),fill=T)

print("Finding subsequent additions")
additionsTall <- interactionsIHStyle[Outcome == 1]
additionsTallNextMonth <- copy(additionsTall)
additionsTallNextMonth[, Snapshot := Snapshot - 1] # join with prev month
subsequentAdditions <- merge(additionsTall, additionsTallNextMonth, by=c("CustomerID", "Snapshot"), suffixes=c(".current",".next"))
subsequentAdditionsSummary <- merge(subsequentAdditions[, .(n = .N), by=c("Proposition.current","Proposition.next")][order(n,decreasing = T)],
                                    additionsTall[, .(nTotalAdditions=.N, pctTotalAdditions=.N/nrow(additionsTall)), by=Proposition],
                                    all.x = T,
                                    by.x="Proposition.current",
                                    by.y="Proposition")

print(head(subsequentAdditionsSummary))

print("Few examples of subsequent additions")
print(head( subsequentAdditions[Proposition.current == subsequentAdditionsSummary[1]$Proposition.current & Proposition.next == subsequentAdditionsSummary[1]$Proposition.next] ))
print(head( subsequentAdditions[Proposition.current == subsequentAdditionsSummary[2]$Proposition.current & Proposition.next == subsequentAdditionsSummary[2]$Proposition.next] ))

library(igraph)
set.seed(1234)
gd <- head( subsequentAdditionsSummary, 75 )
gd <- gd[Proposition.next %in% gd$Proposition.current]  # only nodes that in turn have another destination
plot( graph.data.frame(gd), vertex.size=trunc(100*gd$pctTotalAdditions), edge.arrow.size=0.02, edge.label=gd$n, edge.label.cex=0.7, layout=layout.circle,
      main="Most frequent subsequent additions")

print("Done")

