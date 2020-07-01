list.of.packages <- c(
                      "tidyr",
                      "ggplot2",
                      "caTools",
                      "smbinning",
                      "dplyr",
                      "zoo",
                      "vcd",
                      "pROC",
                      "forcats",
                      "woe",
                      "riv",
                      "DMwR",
                      "corrplot",
                      "leaps",
                      "VSURF",
                      "LogisticDx",
                      "gtools",
                      "plotROC",
                      "xgboost",
                      "sm",
                      "randomForest",
                      "gbm",
                      "caret")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.us.r-project.org")


library(tidyr)
library(ggplot2)
library(caTools)
library(smbinning) 
library(dplyr)
library(zoo)
library(vcd)
library(pROC)
library(forcats)
library(woe)
library(riv)
library(DMwR)
library(corrplot)
library(leaps) #subsetting selection
library(VSURF)
library(LogisticDx) # - gof()
library(gtools) # smartbind()
library(plotROC)
library(randomForest)
library(gbm)
library(caret)
library(xgboost)
library(sm)
library(knitr)