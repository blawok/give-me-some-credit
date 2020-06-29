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
library(DMwR)
library(corrplot)


data <- read.csv('data/cs-training.csv')

summary(data)

data$def <- data$SeriousDlqin2yrs
data$SeriousDlqin2yrs <- NULL
data$X <- NULL
data$def_two <- (1 - data$def)
data$NumberOfTime30_59DaysPastDueNotWorse <- data$NumberOfTime30.59DaysPastDueNotWorse
data$NumberOfTime60_89DaysPastDueNotWorse <- data$NumberOfTime60.89DaysPastDueNotWorse 
data$NumberOfTime30.59DaysPastDueNotWorse <- NULL
data$NumberOfTime60.89DaysPastDueNotWorse <- NULL

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

numeric_cols <- colnames(select_if(train, is.numeric))[1:10]
hist_df <- gather(train[, which(names(train) %in% numeric_cols)], key = "name", value = "value")

ggplot(hist_df) +
  geom_histogram(aes(value)) +
  facet_wrap(~name, ncol = 5, scales = "free") +
  ggtitle("Histograms of all variables")

# different distributions, a lot of outliers, woe will deal with this
rm(hist_df)


# ------------------------------------------------------------------------------ WOE

train_woe <- as.data.frame(train)
test_woe <- as.data.frame(test)


result_1 <- smbinning(train[,c("def", "NumberOfTime30_59DaysPastDueNotWorse")], y="def", x="NumberOfTime30_59DaysPastDueNotWorse", p=0.1)
train_woe[,"NumberOfTime30_59DaysPastDueNotWorse_coarse"]<- cut(as.numeric(train[,"NumberOfTime30_59DaysPastDueNotWorse"]), breaks=c(-Inf,unique(result_1$cuts),Inf), 
                                     labels=c(paste("<=", unique(result_1$cuts)),"<= Inf"),
                                     include.lowest = T)
test_woe[,"NumberOfTime30_59DaysPastDueNotWorse_coarse"]<- cut(as.numeric(test[,"NumberOfTime30_59DaysPastDueNotWorse"]), breaks=c(-Inf,unique(result_1$cuts),Inf), 
                                    labels=c(paste("<=", unique(result_1$cuts)),"<= Inf"),
                                    include.lowest = T)
IV <- result_1$ivtable
IV$Cutpoint<-ifelse(grepl(">",IV$Cutpoint)==T,"<= Inf",IV$Cutpoint)
train_woe <- merge(train_woe,IV[,c("Cutpoint", "WoE")],by.x= "NumberOfTime30_59DaysPastDueNotWorse_coarse", by.y="Cutpoint", all.x=T, sort=F)
colnames(train_woe)[which(names(train_woe) == "WoE")] <- "NumberOfTime30_59DaysPastDueNotWorse_woe"
test_woe <- merge(test_woe,IV[,c("Cutpoint", "WoE")],by.x= "NumberOfTime30_59DaysPastDueNotWorse_coarse", by.y="Cutpoint", all.x=T, sort=F)
colnames(test_woe)[which(names(test_woe) == "WoE")] <- "NumberOfTime30_59DaysPastDueNotWorse_woe"
# OK IV 0.6618


result_1 <- smbinning(train[,c("def", "NumberOfOpenCreditLinesAndLoans")], y="def", x="NumberOfOpenCreditLinesAndLoans", p=0.1)
train_woe[,"NumberOfOpenCreditLinesAndLoans_coarse"]<- cut(as.numeric(train[,"NumberOfOpenCreditLinesAndLoans"]), breaks=c(-Inf,unique(result_1$cuts),Inf), 
                                     labels=c(paste("<=", unique(result_1$cuts)),"<= Inf"),
                                     include.lowest = T)
test_woe[,"NumberOfOpenCreditLinesAndLoans_coarse"]<- cut(as.numeric(test[,"NumberOfOpenCreditLinesAndLoans"]), breaks=c(-Inf,unique(result_1$cuts),Inf), 
                                    labels=c(paste("<=", unique(result_1$cuts)),"<= Inf"),
                                    include.lowest = T)
IV <- result_1$ivtable
IV$Cutpoint<-ifelse(grepl(">",IV$Cutpoint)==T,"<= Inf",IV$Cutpoint)
train_woe <- merge(train_woe,IV[,c("Cutpoint", "WoE")],by.x= "NumberOfOpenCreditLinesAndLoans_coarse", by.y="Cutpoint", all.x=T, sort=F)
colnames(train_woe)[which(names(train_woe) == "WoE")] <- "NumberOfOpenCreditLinesAndLoans_woe"
test_woe <- merge(test_woe,IV[,c("Cutpoint", "WoE")],by.x= "NumberOfOpenCreditLinesAndLoans_coarse", by.y="Cutpoint", all.x=T, sort=F)
colnames(test_woe)[which(names(test_woe) == "WoE")] <- "NumberOfOpenCreditLinesAndLoans_woe"
# OK IV 0.0661

result_1 <- smbinning(train[,c("def", "NumberRealEstateLoansOrLines")], y="def", x="NumberRealEstateLoansOrLines", p=0.1)
train_woe[,"NumberRealEstateLoansOrLines_coarse"]<- cut(as.numeric(train[,"NumberRealEstateLoansOrLines"]), breaks=c(-Inf,unique(result_1$cuts),Inf), 
                                     labels=c(paste("<=", unique(result_1$cuts)),"<= Inf"),
                                     include.lowest = T)
test_woe[,"NumberRealEstateLoansOrLines_coarse"]<- cut(as.numeric(test[,"NumberRealEstateLoansOrLines"]), breaks=c(-Inf,unique(result_1$cuts),Inf), 
                                    labels=c(paste("<=", unique(result_1$cuts)),"<= Inf"),
                                    include.lowest = T)
IV <- result_1$ivtable
IV$Cutpoint<-ifelse(grepl(">",IV$Cutpoint)==T,"<= Inf",IV$Cutpoint)
train_woe <- merge(train_woe,IV[,c("Cutpoint", "WoE")],by.x= "NumberRealEstateLoansOrLines_coarse", by.y="Cutpoint", all.x=T, sort=F)
colnames(train_woe)[which(names(train_woe) == "WoE")] <- "NumberRealEstateLoansOrLines_woe"
test_woe <- merge(test_woe,IV[,c("Cutpoint", "WoE")],by.x= "NumberRealEstateLoansOrLines_coarse", by.y="Cutpoint", all.x=T, sort=F)
colnames(test_woe)[which(names(test_woe) == "WoE")] <- "NumberRealEstateLoansOrLines_woe"
# OK IV 0.0466


result_1 <- smbinning(train[,c("def_two", "DebtRatio")], y="def_two", x="DebtRatio", p=0.1)
train_woe[,"DebtRatio_coarse"]<- cut(as.numeric(train$DebtRatio), breaks=c(-Inf,unique(result_1$cuts),Inf), 
                                  labels=c(paste("<=", unique(result_1$cuts)),"<= Inf"),
                                  include.lowest = T)
test_woe[,"DebtRatio_coarse"]<- cut(as.numeric(test$DebtRatio), breaks=c(-Inf,unique(result_1$cuts),Inf), 
                                     labels=c(paste("<=", unique(result_1$cuts)),"<= Inf"),
                                     include.lowest = T)
IV <- result_1$ivtable
IV$Cutpoint<-ifelse(grepl(">",IV$Cutpoint)==T,"<= Inf",IV$Cutpoint)
train_woe <- merge(train_woe,IV[,c("Cutpoint", "WoE")],by.x= "DebtRatio_coarse", by.y="Cutpoint", all.x=T, sort=F)
colnames(train_woe)[which(names(train_woe) == "WoE")] <- "DebtRatio_woe"
test_woe <- merge(test_woe,IV[,c("Cutpoint", "WoE")],by.x= "DebtRatio_coarse", by.y="Cutpoint", all.x=T, sort=F)
colnames(test_woe)[which(names(test_woe) == "WoE")] <- "DebtRatio_woe"
# OK IV 0.0723

rm(IV)
rm(result_1)

cols_to_woe <-  c("NumberOfTime30_59DaysPastDueNotWorse",
                  "NumberOfOpenCreditLinesAndLoans",
                  "NumberRealEstateLoansOrLines",
                  "DebtRatio")

train_woe <- train_woe[, -which(colnames(train_woe) %in% grep("coarse", colnames(train_woe), value = TRUE) | colnames(train_woe) %in% cols_to_woe) ]
test_woe <- test_woe[, -which(colnames(test_woe) %in% grep("coarse", colnames(test_woe), value = TRUE) | colnames(test_woe) %in% cols_to_woe) ]

# ------------------------------------------------------------------------------ Recategorizing factors

train_woe$def <- factor(ifelse(train_woe$def == "1","yes","no"))
train_woe$no_income <- factor(ifelse(train_woe$no_income == "1","yes","no"))
train_woe$no_dependents <- factor(ifelse(train_woe$no_dependents == "1","yes","no"))

test_woe$def <- factor(ifelse(test_woe$def == "1","yes","no"))
test_woe$no_income <- factor(ifelse(test_woe$no_income == "1","yes","no"))
test_woe$no_dependents <- factor(ifelse(test_woe$no_dependents == "1","yes","no"))

train_woe$def_two <- NULL
test_woe$def_two <- NULL

# ------------------------------------------------------------------------------ Correlation 

res <- cor(train[,which(colnames(train) %in% numeric_cols | colnames(train) %in% grep("def", colnames(train), value=TRUE))])
round(res, 2)
corrplot(res)
# Highest correlation coefficient is 0.43 - we keep all variables 

# ------------------------------------------------------------------------------ Validation set

set.seed(361309)
split_var <- sample.split(train_woe$def, SplitRatio = 0.9)
train_woe <- train_woe[split_var==T,]
validation_woe <- train_woe[split_var==F,]
rm(split_var)

# ------------------------------------------------------------------------------ SMOTE 

train_woe$ID <- seq.int(nrow(train_woe))
temp_train <- DMwR::SMOTE(def ~ ., train_woe, perc.over = 50, k = 3)
temp_train <- subset(temp_train, def == "yes")
temp_train <- rbind(train_woe, temp_train)
train_woe_smote <- distinct(temp_train)
train_woe$ID <- NULL
train_woe_smote$ID <- NULL
rm(temp_train)

summary(train_woe$def)
summary(train_woe_smote$def)

# ------------------------------------------------------------------------------ Save datasets 

save(train_woe, file = "data/train_woe.Rdata")
save(test_woe, file = "data/test_woe.Rdata")
save(validation_woe, file = "data/validation_woe.Rdata")
save(train_woe_smote, file = "data/train_woe_smote.Rdata")

