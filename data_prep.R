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


# ---------------------------------------------------------------- NumberOfTime30_59DaysPastDueNotWorse

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
# Cutpoint CntRec CntGood CntBad CntCumRec CntCumGood CntCumBad PctRec GoodRate BadRate   Odds  LnOdds     WoE     IV
# 1     <= 0 113446    4548 108898    113446       4548    108898 0.8403   0.0401  0.9599 0.0418 -3.1757 -0.5394 0.1944
# 2   <= Inf  21554    4475  17079    135000       9023    125977 0.1597   0.2076  0.7924 0.2620 -1.3393  1.2970 0.4674
# 3  Missing      0       0      0    135000       9023    125977 0.0000      NaN     NaN    NaN     NaN     NaN    NaN
# 4    Total 135000    9023 125977        NA         NA        NA 1.0000   0.0668  0.9332 0.0716 -2.6363  0.0000 0.6618


#plotting woe for this variable
plot<-IV[!is.na(IV$WoE) & IV$Cutpoint!="Total",]
# plot$WoE<-ifelse(plot$WoE==Inf, 8, ifelse(plot$WoE==-Inf, -8,plot$WoE))
plot$pos <- c(-0.25, 0.65)
ggplot(plot, aes(x=Cutpoint, fill=WoE)) +
  geom_bar(aes(weight=WoE)) +
  ylab('WoE') +
  ggtitle('NumberOfTime30_59DaysPastDueNotWorse')+
  geom_label(aes(label=paste("WoE=",format(plot$WoE,digits=1)," \n Fill=",round(plot$PctRec,3)*100, "%", sep=""), y=pos), colour = "white", fontface = "bold")




# ---------------------------------------------------------------- NumberOfOpenCreditLinesAndLoans

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
# Cutpoint CntRec CntGood CntBad CntCumRec CntCumGood CntCumBad PctRec GoodRate BadRate   Odds  LnOdds     WoE     IV
# 1     <= 3  19865    2135  17730     19865       2135     17730 0.1471   0.1075  0.8925 0.1204 -2.1168  0.5195 0.0498
# 2    <= 13  95328    5505  89823    115193       7640    107553 0.7061   0.0577  0.9423 0.0613 -2.7922 -0.1559 0.0160
# 3   <= Inf  19807    1383  18424    135000       9023    125977 0.1467   0.0698  0.9302 0.0751 -2.5894  0.0469 0.0003
# 4  Missing      0       0      0    135000       9023    125977 0.0000      NaN     NaN    NaN     NaN     NaN    NaN
# 5    Total 135000    9023 125977        NA         NA        NA 1.0000   0.0668  0.9332 0.0716 -2.6363  0.0000 0.0661


#plotting woe for this variable
plot<-IV[!is.na(IV$WoE) & IV$Cutpoint!="Total",]
# plot$WoE<-ifelse(plot$WoE==Inf, 8, ifelse(plot$WoE==-Inf, -8,plot$WoE))
plot$pos <- c(0.3, -0.08, 0.02)
ggplot(plot, aes(x=Cutpoint, fill=WoE)) +
  geom_bar(aes(weight=WoE)) +
  ylab('WoE') +
  ggtitle('NumberOfOpenCreditLinesAndLoans')+
  scale_x_discrete(limits = c('<= 3','<= 13','<= Inf')) +
  geom_label(aes(label=paste("WoE=",format(plot$WoE,digits=1)," \n Fill=",round(plot$PctRec,3)*100, "%", sep=""), y=pos), colour = "white", fontface = "bold")


# ---------------------------------------------------------------- NumberRealEstateLoansOrLines

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
# Cutpoint CntRec CntGood CntBad CntCumRec CntCumGood CntCumBad PctRec GoodRate BadRate   Odds  LnOdds     WoE     IV
# 1     <= 0  50574    4216  46358     50574       4216     46358 0.3746   0.0834  0.9166 0.0909 -2.3975  0.2388 0.0237
# 2     <= 1  47053    2450  44603     97627       6666     90961 0.3485   0.0521  0.9479 0.0549 -2.9017 -0.2654 0.0219
# 3   <= Inf  37373    2357  35016    135000       9023    125977 0.2768   0.0631  0.9369 0.0673 -2.6984 -0.0621 0.0010
# 4  Missing      0       0      0    135000       9023    125977 0.0000      NaN     NaN    NaN     NaN     NaN    NaN
# 5    Total 135000    9023 125977        NA         NA        NA 1.0000   0.0668  0.9332 0.0716 -2.6363  0.0000 0.0466

#plotting woe for this variable
plot<-IV[!is.na(IV$WoE) & IV$Cutpoint!="Total",]
# plot$WoE<-ifelse(plot$WoE==Inf, 8, ifelse(plot$WoE==-Inf, -8,plot$WoE))
plot$pos <- c(0.15, -0.15, -0.03)
ggplot(plot, aes(x=Cutpoint, fill=WoE)) +
  geom_bar(aes(weight=WoE)) +
  ylab('WoE') +
  ggtitle('NumberRealEstateLoansOrLines')+
  scale_x_discrete(limits = c('<= 0','<= 1','<= Inf')) +
  geom_label(aes(label=paste("WoE=",format(plot$WoE,digits=1)," \n Fill=",round(plot$PctRec,3)*100, "%", sep=""), y=pos), colour = "white", fontface = "bold")



# ---------------------------------------------------------------- DebtRatio

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
# Cutpoint CntRec CntGood CntBad CntCumRec CntCumGood CntCumBad PctRec GoodRate BadRate    Odds LnOdds     WoE     IV
# 1 <= 0.4234  75711   71244   4467     75711      71244      4467 0.5608   0.9410  0.0590 15.9490 2.7694  0.1331 0.0094
# 2 <= 0.6447  18587   17075   1512     94298      88319      5979 0.1377   0.9187  0.0813 11.2930 2.4242 -0.2121 0.0068
# 3 <= 3.9724  13689   12123   1566    107987     100442      7545 0.1014   0.8856  0.1144  7.7414 2.0466 -0.5897 0.0456
# 4   <= 1267  13501   12687    814    121488     113129      8359 0.1000   0.9397  0.0603 15.5860 2.7464  0.1101 0.0012
# 5    <= Inf  13512   12848    664    135000     125977      9023 0.1001   0.9509  0.0491 19.3494 2.9627  0.3263 0.0093
# 6   Missing      0       0      0    135000     125977      9023 0.0000      NaN     NaN     NaN    NaN     NaN    NaN
# 7     Total 135000  125977   9023        NA         NA        NA 1.0000   0.9332  0.0668 13.9618 2.6363  0.0000 0.0723

#plotting woe for this variable
plot<-IV[!is.na(IV$WoE) & IV$Cutpoint!="Total",]
# plot$WoE<-ifelse(plot$WoE==Inf, 8, ifelse(plot$WoE==-Inf, -8,plot$WoE))
plot$pos <- c(0.07, -0.1, -0.3,0.055,0.18)
ggplot(plot, aes(x=Cutpoint, fill=WoE)) +
  geom_bar(aes(weight=WoE)) +
  ylab('WoE') +
  ggtitle('DebtRatio')+
  scale_x_discrete(limits = c('<= 0.4234','<= 0.6447','<= 3.9724', '<= 1267', '<= Inf')) +
  geom_label(aes(label=paste("WoE=",format(plot$WoE,digits=1)," \n Fill=",round(plot$PctRec,3)*100, "%", sep=""), y=pos), colour = "white", fontface = "bold")


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
rm(split_var)
split_var <- sample.split(train_woe$def, SplitRatio = 0.9)
train_woe <- train_woe[split_var==T,]
validation_woe <- train_woe[split_var==F,]

colSums(is.na(train_woe))
colSums(is.na(validation_woe))

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

