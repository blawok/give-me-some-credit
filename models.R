library(randomForest)
library(gbm)
library(caret)
library(dplyr)
library(pROC) # - auc(), roc.test itp.
library(xgboost)

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

roc_logit <- roc(result_table$def, result_table$logit_model)
auc(roc_logit)
# Area under the curve: 0.8212
plot(roc_logit, print.auc=TRUE)

2*auc(result_table$def, result_table$logit_model, direction="<")-1
# 0.6423601


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
# AUC =  0.8146

# GINI
2*auc(result_table$def, result_table$rf_model, direction="<")-1
# 0.6291081

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
# AUC = 0.8511

# GINI
2*auc(result_table$def, result_table$gbm_model, direction="<")-1
# 0.7021342

train_woe$def_num <- NULL

# ------------------------------------------------------------------------------ XGBoost
train_woe$def_num <- as.numeric(ifelse(train_woe$def == "yes", 1, 0))

train_woe$no_income <- as.numeric(ifelse(train_woe$no_income == "yes", 1, 0))
train_woe$no_dependents <- as.numeric(ifelse(train_woe$no_dependents == "yes", 1, 0))

validation_woe$no_income <- as.numeric(ifelse(validation_woe$no_income == "yes", 1, 0))
validation_woe$no_dependents <- as.numeric(ifelse(validation_woe$no_dependents == "yes", 1, 0))

set.seed(361309)
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
roc_gbm <- roc(result_table$def, result_table$xgb_model)
auc(roc_gbm)
plot(roc_gbm, print.auc=TRUE)
# AUC = 0.8545

# GINI
2*auc(result_table$def, result_table$xgb_model, direction="<")-1
# 0.7089527

# ------------------------------------------------------------------------------ Saving models

save(logit_model, file = "models/logit_model.Rdata")
save(rf_model, file = "models/rf_model.Rdata")
save(gbm_model, file = "models/gbm_model.Rdata")
save(xgb_model, file = "models/xgb_model.Rdata")

