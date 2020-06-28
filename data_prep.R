library(imputeTS)
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


data <- read.csv('data/cs-training.csv')

summary(data)

data$def <- data$SeriousDlqin2yrs
data$SeriousDlqin2yrs <- NULL
data$X <- NULL


# ------------------------------------------------------------------------------ train/test split

set.seed(361309)
split_var <- sample.split(data$def, SplitRatio = 0.9)
train <- data[split_var==T,]
test <- data[split_var==F,]

# ------------------------------------------------------------------------------ NA 
sum(is.na(train))
colSums(is.na(train))

# Monthly income NA filling
train$no_income <- ifelse(is.na(train$MonthlyIncome), '1', '0')
test$no_income <- ifelse(is.na(test$MonthlyIncome), '1', '0')
sum(train$no_income == '1')

summary(train$MonthlyIncome)
# mean for test set: 6643

train$MonthlyIncome <- na_mean(train$MonthlyIncome)
test$MonthlyIncome <- ifelse(is.na(test$MonthlyIncome), 6643, test$MonthlyIncome)
sum(is.na(train$MonthlyIncome))

# NumberOfDependents NA filling
summary(train$NumberOfDependents)

train$no_dependents <- ifelse(is.na(train$NumberOfDependents), '1', '0')
test$no_dependents <- ifelse(is.na(test$NumberOfDependents), '1', '0')
train$NumberOfDependents <- ifelse(is.na(train$NumberOfDependents), 1, train$NumberOfDependents)
test$NumberOfDependents <- ifelse(is.na(test$NumberOfDependents), 1, test$NumberOfDependents)

summary(train$NumberOfDependents)
sum(is.na(train$NumberOfDependents))


# ------------------------------------------------------------------------------ Factors
# No factors


# ------------------------------------------------------------------------------ Outliers
# Removing outliers from train (and leaving the outliers that are in test)

remove_outliers <- function(x, na.rm = TRUE) {
  qnt <- quantile(x, probs=c(.05, .95), na.rm = na.rm)
  H <- 2 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- qnt[1]
  y[x > (qnt[2] + H)] <- qnt[2]
  y
}

summary(train)
colnames(train)

train$DebtRatio <- remove_outliers(train$DebtRatio)
train$MonthlyIncome <- remove_outliers(train$MonthlyIncome)
train$RevolvingUtilizationOfUnsecuredLines <- remove_outliers(train$RevolvingUtilizationOfUnsecuredLines)


# ------------------------------------------------------------------------------ Histograms of all variables
summary(train)

hist_cols <- c("RevolvingUtilizationOfUnsecuredLines", "age",
               "DebtRatio","MonthlyIncome",
               "NumberOfDependents")
hist_df <- gather(train[, -which(names(train) %in% hist_cols)], key = "name", value = "value")

ggplot(hist_df) +
  geom_histogram(aes(value)) +
  facet_wrap(~name, ncol = 5, scales = "free") +
  ggtitle("Histograms of all variables")

# different distributions, a lot of outliers, woe will deal with this
hist_df <-NULL



# ------------------------------------------------------------------------------ WOE
summary(train)
colnames(train)

cols_to_woe <-  c(#"NumberOfTime30.59DaysPastDueNotWorse", 
                  "NumberOfOpenCreditLinesAndLoans",
                  "NumberOfTimes90DaysLate",
                  "NumberRealEstateLoansOrLines",
                  "NumberOfTime60.89DaysPastDueNotWorse",
                  "NumberOfDependents"
                  )

train_woe <- as.data.frame(train)
test_woe <- as.data.frame(test)
iv_values <- list()
gini <- list()


for (variable in cols_to_woe){
  print(variable)
  col_name_coarse <- paste(variable, 'coarse', sep='_')
  col_name_woe <- paste(variable, 'woe', sep='_')
  
  var_woe <- smbinning(train[,c("def", variable)],y="def", x=variable, p=0.1)
  cut_points <- list(var_woe$cuts)
  IV <- var_woe$ivtable
  
  
  train_woe[,col_name_coarse]<- cut(train[,variable], breaks=c(-Inf,unique(var_woe$cuts),Inf), 
                                    labels=c(paste("<=", unique(var_woe$cuts)),"<= Inf"),
                                    include.lowest = T)
  train_woe[,col_name_coarse]<-fct_explicit_na(train_woe[,col_name_coarse], na_level="Missing")
  
  test_woe[,col_name_coarse]<- cut(test[,variable], breaks=c(-Inf,unique(var_woe$cuts),Inf), 
                                   labels=c(paste("<=", unique(var_woe$cuts)),"<= Inf"),
                                   include.lowest = T)
  test_woe[,col_name_coarse]<-fct_explicit_na(test_woe[,col_name_coarse], na_level="Missing")
  
  IVw<-sum(iv.mult(test_woe,"def_two",vars=col_name_coarse)[[1]][,"miv"])
  IV$Cutpoint<-ifelse(grepl(">",IV$Cutpoint)==T,"<= Inf",IV$Cutpoint)
  
  train_woe <- merge(train_woe,IV[,c("Cutpoint", "WoE")],by.x= col_name_coarse, by.y="Cutpoint", all.x=T, sort=F)
  colnames(train_woe)[which(names(train_woe) == "WoE")] <- col_name_woe
  
  test_woe <- merge(test_woe,IV[,c("Cutpoint", "WoE")],by.x= col_name_coarse, by.y="Cutpoint", all.x=T, sort=F)
  colnames(test_woe)[which(names(test_woe) == "WoE")] <- col_name_woe
  
  iv_values <- c(iv_values, var_woe$iv)
  gini<- c(gini, 2*auc(train_woe$def,train_woe[,col_name_woe])-1)
}

names(iv_values) <- cols_to_woe
names(gini) <- cols_to_woe