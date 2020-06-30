library(randomForest)
library(gbm)
library(caret)
library(dplyr)
library(pROC) # - auc(), roc.test itp.
library(xgboost)
library(sm)

set.seed(361309)

# load("data/train_woe_smote.Rdata")
load("data/train_woe.Rdata")
load("data/validation_woe.Rdata")

result_table <- as.data.frame(validation_woe$def)
colnames(result_table) <- "def"


# ------------------------------------------------------------------------------ Logistic Regression

logit_model <- glm(def ~ RevolvingUtilizationOfUnsecuredLines +
                     NumberOfTime30_59DaysPastDueNotWorse_woe +
                     age +
                     NumberOfTimes90DaysLate +
                     NumberOfTime60_89DaysPastDueNotWorse +
                     MonthlyIncome +
                     NumberOfDependents +
                     NumberOfOpenCreditLinesAndLoans_woe +
                     NumberRealEstateLoansOrLines_woe +
                     DebtRatio_woe +
                     no_dependents,
                   data=train_woe,
                   family=binomial("logit"))
summary(logit_model)

result_table$logit_model<-predict(logit_model, newdata=validation_woe, type="response") 

# ROC
roc_logit <- roc(result_table$def, result_table$logit_model)
auc(roc_logit)
plot(roc_logit, print.auc=TRUE)
# AUC = 0.8212

# GINI
2*auc(result_table$def, result_table$logit_model, direction="<")-1
# 0.6423601

# Scoring
result_table$logit_score<-(660-40/log(1/2)*log(1/72))+40/log(1/2)*predict(logit_model, newdata=validation_woe, type="link") 

hist(result_table[result_table$def=="no",c("logit_score")])
hist(result_table[result_table$def=="yes",c("logit_score")],add=TRUE,col="red", alpha=I(0.5))
sm.density.compare(result_table$logit_score, result_table$def, xlab="Scores from Logit Model")
title(main="Score distribution per default")

# add legend via mouse click
colfill<-c(2:(2+length(levels(result_table$def))))
legend(locator(1), levels(result_table$def), fill=colfill)

# ------------------------------------------------------------------------------ Random Forest

rf_model <- randomForest(x = train_woe[,-6], 
                         y = train_woe[,6], 
                         ntree=100, #number of estimators
                         mtry=5, #number of features at each split
                         nodesize=1 #minimal node size
                         )


result_table$rf_model <- predict(rf_model, validation_woe, type = "prob")[,2]

# ROC
roc_rf <- roc(result_table$def, result_table$rf_model)
auc(roc_rf)
plot(roc_rf, print.auc=TRUE)
# AUC =  0.8135

# GINI
2*auc(result_table$def, result_table$rf_model, direction="<")-1
# 0.6270622

# Scoring
result_table$rf_score<-(660-40/log(1/2)*log(1/72))+40/log(1/2)*ifelse(log(result_table$rf_model) == -Inf, -5, log(result_table$rf_model))

hist(result_table[result_table$def=="no",c("rf_score")])
hist(result_table[result_table$def=="yes",c("rf_score")],add=TRUE,col="red", alpha=I(0.5))
sm.density.compare(result_table$rf_score, result_table$def, xlab="Scores from RF Model")
title(main="Score distribution per default")

# add legend via mouse click
colfill<-c(2:(2+length(levels(result_table$def))))
legend(locator(1), levels(result_table$def), fill=colfill)

# ------------------------------------------------------------------------------ AdaBoost

train_woe$def_num <- as.numeric(ifelse(train_woe$def == "yes", 1, 0))

gbm_model <- gbm(data = train_woe[,-6], 
                 formula = def_num ~ ., 
                 distribution = "adaboost",
                 n.trees = 80, #110
                 shrinkage = 0.07, #0.05
                 bag.fraction = 0.7, #0.5
                 verbose = TRUE)

result_table$gbm_model <- predict(gbm_model, validation_woe, n.trees = 80, type = "response")

# ROC
roc_gbm <- roc(result_table$def, result_table$gbm_model)
auc(roc_gbm)
plot(roc_gbm, print.auc=TRUE)
# AUC = 0.8507

# GINI
2*auc(result_table$def, result_table$gbm_model, direction="<")-1
# 0.7014977

train_woe$def_num <- NULL

# Scoring
result_table$gbm_score<-(660-40/log(1/2)*log(1/72))+40/log(1/2)*log(result_table$gbm_model)

hist(result_table[result_table$def=="no",c("gbm_score")])
hist(result_table[result_table$def=="yes",c("gbm_score")],add=TRUE,col="red", alpha=I(0.5))
sm.density.compare(result_table$gbm_score, result_table$def, xlab="Scores from Adaboost Model")
title(main="Score distribution per default")

# add legend via mouse click
colfill<-c(2:(2+length(levels(result_table$def))))
legend(locator(1), levels(result_table$def), fill=colfill)

# ------------------------------------------------------------------------------ XGBoost
train_woe$def_num <- as.numeric(ifelse(train_woe$def == "yes", 1, 0))

train_woe$no_income <- as.numeric(ifelse(train_woe$no_income == "yes", 1, 0))
train_woe$no_dependents <- as.numeric(ifelse(train_woe$no_dependents == "yes", 1, 0))

validation_woe$no_income <- as.numeric(ifelse(validation_woe$no_income == "yes", 1, 0))
validation_woe$no_dependents <- as.numeric(ifelse(validation_woe$no_dependents == "yes", 1, 0))

xgb_model <- xgboost(data = as.matrix(train_woe[,-c(6, 14)]), 
                     label = as.matrix(train_woe[,14]), 
                     eta = 0.3, #0.05, 0.2, 0.25
                     gamma=0,
                     max.depth = 4, #10, 15, 6, 8
                     max_delta_step=0,
                     subsample = 0.7, #0.85
                     colsample_bytree = 0.85,#0.7
                     lambda = 1, #0.95, 0.99
                     alpha=0, #0.05
                     scale_pos_weight = 4, #9
                     nrounds = 20, #10, 15, 25, 30
                     objective = "binary:logistic")

result_table$xgb_model <- predict(xgb_model, as.matrix(validation_woe[,-6]))

# ROC
roc_xgb <- roc(result_table$def, result_table$xgb_model)
auc(roc_xgb)
plot(roc_gbm, print.auc=TRUE)
# AUC = 0.8546

# GINI
2*auc(result_table$def, result_table$xgb_model, direction="<")-1
# 0.7091504

train_woe$def_num <- NULL

# Scoring
result_table$xgb_score<-(660-40/log(1/2)*log(1/72))+40/log(1/2)*log(result_table$xgb_model)

hist(result_table[result_table$def=="no",c("xgb_score")])
hist(result_table[result_table$def=="yes",c("xgb_score")],add=TRUE,col="red", alpha=I(0.5))
sm.density.compare(result_table$xgb_score, result_table$def, xlab="Scores from XGBoost Model")
title(main="Score distribution per default")

# add legend via mouse click
colfill<-c(2:(2+length(levels(result_table$def))))
legend(locator(1), levels(result_table$def), fill=colfill)

# ------------------------------------------------------------------------------ Saving models

save(logit_model, file = "models/logit_model.Rdata")
save(rf_model, file = "models/rf_model.Rdata")
save(gbm_model, file = "models/gbm_model.Rdata")
save(xgb_model, file = "models/xgb_model.Rdata")
save(result_table, file = "models/result_table.RData")
