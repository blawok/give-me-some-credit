
load(file = "models/logit_model.Rdata")
load(file = "models/rf_model.Rdata")
load(file = "models/gbm_model.Rdata")
load(file = "models/xgb_model.Rdata")
load(file = "models/result_table.RData")

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
logit_ks_score <- ks.test(result_table[result_table$def=='no',c("logit_score")],result_table[result_table$def=='yes',c("logit_score")])$statistic
# RF
rf_ks_score <- ks.test(result_table[result_table$def=='no',c("rf_score")],result_table[result_table$def=='yes',c("rf_score")])$statistic
# Adaboost
gbm_ks_score <- ks.test(result_table[result_table$def=='no',c("gbm_score")],result_table[result_table$def=='yes',c("gbm_score")])$statistic
# XGBoost
xgb_ks_score <- ks.test(result_table[result_table$def=='no',c("xgb_score")],result_table[result_table$def=='yes',c("xgb_score")])$statistic

k_s_scores <- c(logit_ks_score, rf_ks_score, gbm_ks_score, xgb_ks_score)

# ------------------------------------------------------------------------------ Optimal Cut-off

# EXCUSE ME WHAT THE DUCK


comparison_table <- as.data.frame(models)
comparison_table <- cbind(comparison_table, aucs, ginis, k_s_scores)
colnames(comparison_table) <- c("model_name", "auc", "gini", "k_s_stat")
