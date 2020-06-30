
load(file = "models/logit_model.Rdata")
load(file = "models/rf_model.Rdata")
load(file = "models/gbm_model.Rdata")
load(file = "models/xgb_model.Rdata")
load(file = "models/result_table.RData")
load(file = "models/train_result_table.RData")
load(file = "models/test_table.RData")
load(file = "models/whole_table.RData")
source("model_functions.R")

models <- c("Logit", "Random_Forest", "Adaboost", "XGBoost")

# ------------------------------------------------------------------------------ AUCs
# Logit
logit_auc <- round(1*auc(roc(result_table$def, result_table$logit_model)), 4)
# RF
rf_auc <- round(1*auc(roc(result_table$def, result_table$rf_model)), 4)
# Adaboost
gbm_auc <- round(1*auc(roc(result_table$def, result_table$gbm_model)), 4)
# XGBoost
xgb_auc <- round(1*auc(roc(result_table$def, result_table$xgb_model)), 4)

aucs <- c(logit_auc, rf_auc, gbm_auc, xgb_auc)

# ------------------------------------------------------------------------------ GINIs
# Logit
logit_gini <- round(2*auc(result_table$def, result_table$logit_model, direction="<")-1, 4)
# RF
rf_gini <- round(2*auc(result_table$def, result_table$rf_model, direction="<")-1, 4)
# Adaboost
gbm_gini <- round(2*auc(result_table$def, result_table$gbm_model, direction="<")-1, 4)
# XGBoost
xgb_gini <- round(2*auc(result_table$def, result_table$xgb_model, direction="<")-1, 4)

ginis <- c(logit_gini, rf_gini, gbm_gini, xgb_gini)

# ------------------------------------------------------------------------------ K-S Tests
# Logit
logit_ks_score <- round(ks.test(result_table[result_table$def=='no',c("logit_score")],result_table[result_table$def=='yes',c("logit_score")])$statistic, 4)
# RF
rf_ks_score <- round(ks.test(result_table[result_table$def=='no',c("rf_score")],result_table[result_table$def=='yes',c("rf_score")])$statistic, 4)
# Adaboost
gbm_ks_score <- round(ks.test(result_table[result_table$def=='no',c("gbm_score")],result_table[result_table$def=='yes',c("gbm_score")])$statistic, 4)
# XGBoost
xgb_ks_score <- round(ks.test(result_table[result_table$def=='no',c("xgb_score")],result_table[result_table$def=='yes',c("xgb_score")])$statistic, 4)

k_s_scores <- c(logit_ks_score, rf_ks_score, gbm_ks_score, xgb_ks_score)

# ------------------------------------------------------------------------------ PSI Test
# Logit
logit_psi <- round(cal_psi(data1=train_result_table, data2=result_table, bench="logit_score",target="logit_score",bin=20), 4)
# RF
rf_psi <- round(cal_psi(data1=train_result_table, data2=result_table, bench="rf_score",target="rf_score",bin=20), 4)
# Adaboost
gbm_psi <- round(cal_psi(data1=train_result_table, data2=result_table, bench="gbm_score",target="gbm_score",bin=20), 4)
# XGBoost
xgb_psi <- round(cal_psi(data1=train_result_table, data2=result_table, bench="xgb_score",target="xgb_score",bin=20), 4)

psis <- c(logit_psi, rf_psi, gbm_psi, xgb_psi)

# ------------------------------------------------------------------------------ Optimal Cut-off

# EXCUSE ME WHAT THE DUCK


# ------------------------------------------------------------------------------ # Validation set results

comparison_table <- as.data.frame(models)
comparison_table <- cbind(comparison_table, aucs, ginis, k_s_scores, psis)
colnames(comparison_table) <- c("model_name", "auc", "gini", "k_s_stat", "psi")

View(comparison_table)


# ------------------------------------------------------------------------------ # Test set results GINI

# Logit
logit_gini <- round(2*auc(test_table$def, test_table$logit_model, direction="<")-1, 4)
# RF
rf_gini <- round(2*auc(test_table$def, test_table$rf_model, direction="<")-1, 4)
# Adaboost
gbm_gini <- round(2*auc(test_table$def, test_table$gbm_model, direction="<")-1, 4)
# XGBoost
xgb_gini <- round(2*auc(test_table$def, test_table$xgb_model, direction="<")-1, 4)

test_ginis <- c(logit_gini, rf_gini, gbm_gini, xgb_gini)


test_comparison <- as.data.frame(models)
test_comparison <- cbind(test_comparison, test_ginis)
colnames(test_comparison) <- c("model_name", "gini")

View(test_comparison)


# ------------------------------------------------------------------------------ # Cutting the score on test

test_table$def <- as.numeric(ifelse(test_table$def == "yes", 0, 1))
test_table$xgb_score<-(660-40/log(1/2)*log(1/72))+40/log(1/2)*log(test_table$xgb_model)
score_cut <- smbinning(test_table[,c("def", "xgb_score")], y="def", x="xgb_score", p=0.1)
score_cut$ivtable


# ------------------------------------------------------------------------------ # Cutting the score on all data

whole_table$def <- as.numeric(ifelse(whole_table$def == "yes", 0, 1))
whole_table$xgb_score<-(660-40/log(1/2)*log(1/72))+40/log(1/2)*log(whole_table$xgb_model)
score_cut <- smbinning(whole_table[,c("def", "xgb_score")], y="def", x="xgb_score", p=0.13)
score_cut$ivtable

# Cutpoint CntRec CntGood CntBad CntCumRec CntCumGood CntCumBad PctRec GoodRate BadRate     Odds LnOdds     WoE     IV
# 1 <= 473.0546  19503   13406   6097     19503      13406      6097 0.1300   0.6874  0.3126   2.1988 0.7879 -1.8484 0.9470
# 2 <= 518.3472  30355   27971   2384     49858      41377      8481 0.2024   0.9215  0.0785  11.7328 2.4624 -0.1739 0.0066
# 3 <= 565.6915  29687   28804    883     79545      70181      9364 0.1979   0.9703  0.0297  32.6206 3.4849  0.8487 0.0999
# 4 <= 589.5328  29007   28600    407    108552      98781      9771 0.1934   0.9860  0.0140  70.2703 4.2523  1.6161 0.2646
# 5 <= 609.4681  21228   21050    178    129780     119831      9949 0.1415   0.9916  0.0084 118.2584 4.7729  2.1366 0.2834
# 6  > 609.4681  20220   20143     77    150000     139974     10026 0.1348   0.9962  0.0038 261.5974 5.5668  2.9305 0.3992
# 7     Missing      0       0      0    150000     139974     10026 0.0000      NaN     NaN      NaN    NaN     NaN    NaN
# 8       Total 150000  139974  10026        NA         NA        NA 1.0000   0.9332  0.0668  13.9611 2.6363  0.0000 2.0007
