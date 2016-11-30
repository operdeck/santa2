# Kaggle Santander 2 

# Common stuff

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
library(pROC)
library(corrplot)
library(scales)

set.seed(12345)

data_folder <- "data"
# data_folder <- "data-unittest"

data_colClasses <- list(character=c("ult_fec_cli_1t","indrel_1mes","conyuemp"))
data_dateFlds <- c("fecha_dato","fecha_alta","ult_fec_cli_1t")

trainDate <- c('2015-06-28')
testDate <- c('2016-06-28')

toMonthNr <- function(str)
{
  strAsDate <- fasttime::fastPOSIXct(str)
  return(year(strAsDate)*12 + month(strAsDate) - 1)
}

trainDateNr <- toMonthNr(trainDate)
testDateNr <- toMonthNr(testDate)

