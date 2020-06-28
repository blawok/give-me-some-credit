library(imputeTS)
library(tidyr)
library(ggplot2)

train <- read.csv('data/cs-training.csv')

summary(train)

train$def <- train$SeriousDlqin2yrs
train$SeriousDlqin2yrs <- NULL
train$X <- NULL

# ------------------------------------------------------------------------------ NA 
sum(is.na(train))
colSums(is.na(train))

# Monthly income NA filling
train$no_income <- ifelse(is.na(train$MonthlyIncome), '1', '0')
sum(train$no_income == '1')

summary(train$MonthlyIncome)
# mean for test set: 6670

train$MonthlyIncome <- na_mean(train$MonthlyIncome)
sum(is.na(train$MonthlyIncome))

# NumberOfDependents NA filling
summary(train$NumberOfDependents)

train$no_dependents <- ifelse(is.na(train$NumberOfDependents), '1', '0')
train$NumberOfDependents <- ifelse(is.na(train$NumberOfDependents), 1, train$NumberOfDependents)

summary(train$NumberOfDependents)
sum(is.na(train$NumberOfDependents))


# ------------------------------------------------------------------------------ Factors
# No factors


# ------------------------------------------------------------------------------ Outliers

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