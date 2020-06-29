
library(randomForest)
library(gbm)
library(caret)
library(dplyr)
library(pROC) # - auc(), roc.test itp.
library(xgboost)


load("data/train_woe_smote.Rdata")
load("data/train_woe.Rdata")
load("data/test_woe.Rdata")

result_table <- as.data.frame(test_woe$def)
colnames(result_table) <- "def"

x_train_smote <- train_woe_smote[,-6]
y_train_smote <- train_woe_smote[,6]
x_train <- train_woe[,-6]
y_train <- train_woe[,6]
x_test <- test_woe[,-6]
y_test <- test_woe[,6]

# ------------------------------------------------------------------------------ Logistic Regression



# ------------------------------------------------------------------------------ Random Forest

rf_model <- randomForest(x = x_train, 
                         y = y_train, 
                         ntree=100)

result_table$rf_model <- predict(rf_model, test_woe, type = "prob")[,2]
result_table$rf_prediction <-
  as.factor(ifelse(result_table$rf_model > 0.05,
                   "yes",
                   "no"))

# Confusion Matrix
caret::confusionMatrix(result_table$rf_prediction, result_table$def)

# ROC
roc_rf <- roc(result_table$def, result_table$rf_model)
auc(roc_rf)
plot(roc_rf, print.auc=TRUE)
# AUC = 0.815

# GINI
2*auc(result_table$def, result_table$rf_model, direction="<")-1
# 0.62 

# ------------------------------------------------------------------------------ AdaBoost

train_woe$def_num <- as.numeric(ifelse(train_woe$def == "yes", 1, 0))

gbm_model <- gbm(data = train_woe[,-6], 
                 formula = def_num ~ ., 
                 distribution = "adaboost",
                 n.trees = 100, 
                 verbose = TRUE)

result_table$gbm_model <- predict(gbm_model, test_woe, n.trees = 100, type = "response")
result_table$gbm_prediction <-
  as.factor(ifelse(result_table$gbm_model > 0.05,
                   "yes",
                   "no"))

# Confusion Matrix
caret::confusionMatrix(result_table$gbm_prediction, result_table$def)

# ROC
roc_gbm <- roc(result_table$def, result_table$gbm_model)
auc(roc_gbm)
plot(roc_gbm, print.auc=TRUE)
# AUC = 0.844

# GINI
2*auc(result_table$def, result_table$gbm_model, direction="<")-1
# 0.69

# ------------------------------------------------------------------------------ XGBoost

train_woe$no_income <- as.numeric(ifelse(train_woe$no_income == "yes", 1, 0))
train_woe$no_dependents <- as.numeric(ifelse(train_woe$no_dependents == "yes", 1, 0))

test_woe$no_income <- as.numeric(ifelse(test_woe$no_income == "yes", 1, 0))
test_woe$no_dependents <- as.numeric(ifelse(test_woe$no_dependents == "yes", 1, 0))

xgb_model <- xgboost(data = as.matrix(train_woe[,-c(6, 14)]), 
                     label = as.matrix(train_woe[,14]), 
                     max.depth = 20, 
                     eta = 0.3, 
                     nrounds = 20, 
                     objective = "binary:logistic")

result_table$xgb_model <- predict(xgb_model, as.matrix(test_woe[,-6]))
result_table$xgb_prediction <-
  as.factor(ifelse(result_table$xgb_model > 0.05,
                   "yes",
                   "no"))

# Confusion Matrix
caret::confusionMatrix(result_table$xgb_prediction, result_table$def)

# ROC
roc_gbm <- roc(result_table$def, result_table$xgb_model)
auc(roc_gbm)
plot(roc_gbm, print.auc=TRUE)
# AUC = 0.844

# GINI
2*auc(result_table$def, result_table$xgb_model, direction="<")-1
# 0.65