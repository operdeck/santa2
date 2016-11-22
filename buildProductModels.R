library(data.table)
library(fasttime)
library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)

# require(devtools)
# install_github('tqchen/xgboost',subdir='R-package')

# install.packages("drat", repos="https://cran.rstudio.com")
# drat:::addRepo("dmlc")
# install.packages("xgboost", repos="http://dmlc.ml/drat/", type = "source")

require(xgboost)
library(Ckmeans.1d.dp)
library(DiagrammeR)

# Read data

data_folder <- "data"
# data_folder <- "data-unittest"

data_colClasses = list(character=c("ult_fec_cli_1t","indrel_1mes","conyuemp"))
data_dateFlds = c("fecha_dato","fecha_alta","ult_fec_cli_1t")

train <- fread(paste(data_folder,"train_ver2.csv",sep="/"), 
               colClasses = data_colClasses)

productFlds <- names(train)[grepl("^ind_.*ult1$",names(train))] # products purchased

# Sample to speed up development
set.seed(12345)
allPersonz <- unique(train$ncodpers)
cat("Before dev sampling train size:", dim(train), "unique persons:",length(allPersonz),fill = T)
if (nrow(train) > 1000000) {
  train <- train[ncodpers %in% sample(allPersonz, trunc(0.10*length(allPersonz))),]
}
allPersonz <- unique(train$ncodpers)
cat("After dev sampling train size:", dim(train), "unique persons:",length(allPersonz),fill = T)

test <- fread(paste(data_folder,"test_ver2.csv",sep="/"), 
              colClasses = data_colClasses)

# Concatenate both into one set makes it a lot easier to do all the 
# data processing and generation of derived fields.
all <- bind_rows(train, test)
all$dataset <- c(rep("Train", nrow(train)), rep("Test", nrow(test)))
rm(list=c("train","test"))

for(f in intersect(data_dateFlds, names(all))) { 
  all[[f]] <- fasttime::fastPOSIXct(all[[f]])
  all[[paste("xf.year",f,sep=".")]] <- year(all[[f]])
  all[[paste("xf.month",f,sep=".")]] <- month(all[[f]])
}

all$xf.monthnr <- all$xf.year.fecha_dato*12 + all$xf.month.fecha_dato - 1

for (f in names(all)[sapply(all, class) == "character"]) {
  all[[f]] <- factor(all[[f]])
}

# Quick data summaries

interactionFreqs <- group_by(all, ncodpers, segmento) %>%
  summarise(n=n())
print(ggplot(interactionFreqs, aes(as.factor(n), fill=segmento))+geom_bar()+
        ggtitle("Number of Records per Person"))

setkey(all, ncodpers, xf.monthnr)

# Add last month products as features to this month
lastMonthProducts <- all[,c("ncodpers","xf.monthnr",productFlds),with=F]
lastMonthProducts$xf.monthnr <- lastMonthProducts$xf.monthnr+1
names(lastMonthProducts)[2+seq(length(productFlds))] <- paste("xf.prev",productFlds,sep=".")
setkey(lastMonthProducts, ncodpers, xf.monthnr)
all <- lastMonthProducts[all]

# Set outcome

for (f in productFlds) {
  all[[paste("products",f,sep=".")]] <- 
    as.numeric(ifelse(is.na(all[[f]]) | is.na(all[[paste("xf.prev",f,sep=".")]]), NA, (all[[f]] == 1) & (all[[paste("xf.prev",f,sep=".")]] == 0)))
}

all$xf.prev.products.count <- rowSums(all[,paste("xf.prev",productFlds,sep="."),with=F])
all$products.newcount <- rowSums(all[,paste("products",productFlds,sep="."),with=F])

print(ggplot(filter(all, !is.na(products.newcount)), 
             aes(products.newcount))+
        geom_histogram(binwidth = 1)+
        scale_y_log10()+
        ggtitle("Distribution of new prods in next month"))

# summaries for data check
for (ds in c("Test","Train")) {
  cat("Size of",ds,":",nrow(filter(all, dataset==ds)),fill=T)
  cat("Missing prev products in",ds,":",
      sum(is.na(filter(all, dataset==ds) %>% select(xf.prev.products.count))),fill=T)
  cat("Missing outcomes in",ds,":",
      sum(is.na(filter(all, dataset==ds) %>% select(products.newcount))),fill=T)
  cat("First month by person for", ds, ":",
      nrow(group_by(filter(all, dataset==ds), ncodpers) %>% summarise(firstMonth = min(xf.monthnr))),fill=T)
}

# More derived features

# TODO!

# Build Models
# Create a model for each outcome individually

outcomeCols <- paste("products",productFlds,sep=".")

modelRowz <- which(all$dataset == "Train" & !is.na(all$products.newcount))
modelTrainFlds <- which(!startsWith(names(all), "products.") & !names(all) %in% productFlds)

trainMatrix <- as.matrix((all[modelRowz, modelTrainFlds, with=F])[,lapply(.SD,as.numeric)]) # factors/syms --> numeric
testMatrix <- as.matrix((all[dataset=="Test", modelTrainFlds, with=F])[,lapply(.SD,as.numeric)]) # factors/syms --> numeric

param <- list("objective" = "binary:logistic",
              max.depth = 5,
              eta = 0.01,
              "eval_metric" = "auc") # make sure to maximize!
cv.nround <- 5
cv.nfold <- 3
nround = 5

results <- data.frame(ncodpers=all[dataset=="Test", ncodpers])

for (col in outcomeCols) {
  cat(which(col==outcomeCols),"/",length(outcomeCols),":",col,fill=T)
  y <- as.integer(all[[col]][modelRowz])
  yrate <- sum(y)/length(y)
  results[[col]] <- 0
  if (length(unique(y)) < 2) {
    cat("Skipping",col,": too few distinct values,",length(unique(y)),fill=T)
    next
  }
  if (yrate < 0.001) {
    cat("Skipping",col,": too low rate,",yrate,fill=T)
    next
  }
  # bst.cv = xgb.cv(param=param, data = trainMatrix, missing=NaN,
  #                 label = y, 
  #                 nfold = cv.nfold, nrounds = cv.nround, maximize=T)
  bst = xgboost(params=param, data = trainMatrix, missing=NaN,
                label = as.integer(all[[col]][modelRowz]), 
                nrounds=nround, maximize=T)
  
  # Compute & plot feature importance matrix & summary tree
  importance_matrix <- xgb.importance(dimnames(trainMatrix)[[2]], model = bst)
  print(xgb.plot.importance(importance_matrix)+ggtitle(paste("Feature imp for",col)))

  # xgb.plot.tree(feature_names = dimnames(trainMatrix)[[2]], model = bst, n_first_tree = 2)
  # xgb.plot.multi.trees(model = bst, feature_names = dimnames(trainMatrix)[[2]], features.keep = 3)
  
  predictions <- predict(bst, testMatrix, missing=NaN)
  print(summary(predictions))
  results[[col]] <- predictions
}

# normalize
avgOutcome <- sum(all[dataset=="Train", outcomeCols, with=F], na.rm=T) / sum(all$dataset == "Train")
sum(select(results, -ncodpers) > 0.57) / nrow(results)

qplot(as.vector(as.matrix(select(results, -ncodpers))), geom="density")
