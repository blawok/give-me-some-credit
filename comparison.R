
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

save(test_comparison, file = 'models/test_comparison_table.RData')
save(comparison_table, file = 'models/comparison_table.RData')

# ------------------------------------------------------------------------------ # Cutting the score on test
load('models/test_comparison_table.RData')
load('models/comparison_table.RData')

test_table$def <- as.numeric(ifelse(test_table$def == "yes", 0, 1))
test_table$xgb_score<-(660-40/log(1/2)*log(1/72))+40/log(1/2)*log(test_table$xgb_model)
score_cut <- smbinning(test_table[,c("def", "xgb_score")], y="def", x="xgb_score", p=0.1)
score_cut$ivtable

# Cutpoint CntRec CntGood CntBad CntCumRec CntCumGood CntCumBad PctRec GoodRate BadRate     Odds LnOdds     WoE     IV
# 1 <= 461.0667   1592    1031    561      1592       1031       561 0.1061   0.6476  0.3524   1.8378 0.6086 -2.0273 0.9846
# 2 <= 490.1397   1501    1363    138      3093       2394       699 0.1001   0.9081  0.0919   9.8768 2.2902 -0.3457 0.0139
# 3 <= 512.6249   1524    1412    112      4617       3806       811 0.1016   0.9265  0.0735  12.6071 2.5343 -0.1016 0.0011
# 4 <= 561.2695   2936    2829    107      7553       6635       918 0.1957   0.9636  0.0364  26.4393 3.2748  0.6390 0.0610
# 5 <= 579.0801   1789    1753     36      9342       8388       954 0.1193   0.9799  0.0201  48.6944 3.8856  1.2497 0.1117
# 6  > 579.0801   5658    5609     49     15000      13997      1003 0.3772   0.9913  0.0087 114.4694 4.7403  2.1045 0.7405
# 7     Missing      0       0      0     15000      13997      1003 0.0000      NaN     NaN      NaN    NaN     NaN    NaN
# 8       Total  15000   13997   1003        NA         NA        NA 1.0000   0.9331  0.0669  13.9551 2.6358  0.0000 1.9128

IV <- score_cut$ivtable
#plotting woe for this variable
plot<-IV[!is.na(IV$WoE) & IV$Cutpoint!="Total",]
plot$pos <- c(-1, -0.17, 0,0.3,0.6,1.1)
ggplot(plot, aes(x=Cutpoint, fill=WoE)) +
  geom_bar(aes(weight=WoE)) +
  ylab('WoE') +
  ggtitle('WoE Scores Test Set')+
  geom_label(aes(label=paste("WoE=",format(plot$WoE,digits=1)," \n Fill=",round(plot$PctRec,3)*100, "%", sep=""), y=pos), colour = "white", fontface = "bold")


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

IV <- score_cut$ivtable
#plotting woe for this variable
plot<-IV[!is.na(IV$WoE) & IV$Cutpoint!="Total",]
plot$pos <- c(-1, -0.1, 0.45,0.8,1,1.5)
ggplot(plot, aes(x=Cutpoint, fill=WoE)) +
  geom_bar(aes(weight=WoE)) +
  ylab('WoE') +
  ggtitle('WoE Scores Full Set')+
  geom_label(aes(label=paste("WoE=",format(plot$WoE,digits=1)," \n Fill=",round(plot$PctRec,3)*100, "%", sep=""), y=pos), colour = "white", fontface = "bold")

iv_cuts <- score_cut$cuts
plot$cuts <- c(473.0546 ,518.3472 ,565.6915 ,589.5328 ,609.4681,609.4681)
ggplot(plot, aes(x=cuts, y=WoE)) +
  geom_line() + geom_smooth()
