# Quick analysis of product purchase data
# as exported from Pega Purchases data set

#library(jsonlite)
library(dplyr)
library(ggplot2)
library(scales)

datasetexport <- "C:/Users/PERDO/Downloads/Exam-ESM-Data-Santander-Purchases_Purchases_20161214T165835_GMT.zip"

unzip(datasetexport,file="data.json")
# json = fromJSON("data.json") -- invalid format!

dataset <- read.table("data.json", sep=",", quote = "\"")

purchases <- data.frame(
  Customer = sapply( strsplit( as.character(dataset$V2), split = ":"), function(l) { return (l[[2]]) } ),
  Product = sapply( strsplit( as.character(dataset$V3), split = ":"), function(l) { return (l[[2]]) } ),
  Date = as.POSIXct(sapply( strsplit( as.character(dataset$V5), split = ":|}|T"), function(l) { return (l[[2]]) } ), format="%Y%m%d")
)

smry <- group_by(purchases, Product, Date) %>% summarise(n=n(), pct=n()/nrow(dataset)) %>% arrange(pct)
ggplot(smry, aes(factor(Product, smry$Product), pct, label=n, fill=Date))+ 
  geom_bar(stat="identity")+coord_flip() +
  scale_y_continuous(labels = percent)+
  geom_text()

