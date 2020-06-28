library(DMwR)
library(corrplot)
library(smotefamily)


res <- cor(train[,which(colnames(train) %in% numeric_cols)])
round(res, 2)
corrplot(res)
# Highest correlation coefficient is 0.43 - we keep all variables 

# ------------------------------------------------------------------------------ SMOTE 


train_woe_smoted <- SMOTE(def ~ ., train_woe, perc.over = 100, perc.under = 500, k = 5)


smote1 <- smotefamily::SMOTE(train_woe[,-c(6:7)], "def", K = 4, dup_size = 0)
formula1 <- "class ~ ." %>% as.formula
model.smote <- caret::train(formula1, method = "rpart", smote1$data)
predictions.smote <- predict(model.smote, smote1$data[,1:2]) %>% print
cv2 <- confusionMatrix(smote1$data$class %>% as.factor, predictions.smote)
