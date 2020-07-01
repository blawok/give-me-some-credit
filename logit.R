library(leaps) #subsetting selection
library(VSURF)
library(LogisticDx) # - gof()
library(pROC) # - auc(), roc.test itp.
library(gtools) # smartbind()
library(plotROC)

load("data/train_woe_smote.Rdata")
load("data/train_woe.Rdata")
load("data/validation_woe.Rdata")
source("model_functions.R")

# ------------------------------------------------------------------------------ Baseline model estimation

baza<-glm(def ~ 1,data=train_woe, family=binomial("logit"))

max<-glm(def ~ .,data=train_woe, family=binomial("logit"))
summary(max)


# ------------------------------------------------------------------------------ Stepwise (both)

tim <- proc.time ()[1]	## applying cor (standard R implementation)
model_stepwise_both<-step(baza, scope = list(upper=max, lower=baza ), direction = "both", trace=T,steps=30,k=4)
cat ("cor runtime [s]:", proc.time ()[1] - tim, "(n =", ncol (baza)-3, ")\n")
save(model_stepwise_both,file="data/model_stepwise_both.rdata")
summary(model_stepwise_both)



# ------------------------------------------------------------------------------ Evaluation

mdl<-"model_stepwise_both"
model<-model_stepwise_both

################################################################################


gf<-pchisq(model$deviance, model$df.residual,lower.tail = F)
# 1 - (cannot reject H0) model is well fitted

ist<-pchisq(model$null.deviance-model$deviance, model$df.null-model$df.residual,lower.tail = F)
# 0 - (reject H0) variables are not statistically irrelevant

hr<-hosmerlem(y=train_woe$def, yhat=fitted(model),g=10)
hosmerlem(y=train_woe$def, yhat=fitted(model),g=7)
hosmerlem(y=train_woe$def, yhat=fitted(model),g=8)
hosmerlem(y=train_woe$def, yhat=fitted(model),g=9)
hr$p.value
#not meaningfull for factors?

# ------------------------------------------------------------------------------ Assaigning PD to train_woe 

train_woe$baza<-baza$fitted.values
train_woe$model<-model$fitted.values
train_woe$max<-max$fitted.values

train_woe$model<-predict(model, newdata=train_woe, type="response") 
train_woe$model_b<-predict(model_stepwise_b, newdata=train_woe, type="response") 
train_woe$model_f<-predict(model_stepwise_for, newdata=train_woe, type="response") 


validation_woe$model<-predict(model, newdata=validation_woe, type="response") 
validation_woe$max<-predict(max, newdata=validation_woe, type="response") 
validation_woe$model_b<-predict(model_stepwise_b, newdata=validation_woe, type="response") 
validation_woe$model_f<-predict(model_stepwise_for, newdata=validation_woe, type="response") 


# ------------------------------------------------------------------------------ Score computation (train and test)

train_woe$score<-(660-40/log(1/2)*log(1/72))+40/log(1/2)*model$linear.predictors
validation_woe$score<-(660-40/log(1/2)*log(1/72))+40/log(1/2)*predict(model, newdata=validation_woe, type="link") 


train_woe$score<-(660-40/log(1/2)*log(1/72))+40/log(1/2)*predict(model, newdata=train_woe, type="link") 

# ------------------------------------------------------------------------------ ROC
#H0  ROC curves are equally good
roc_test_baza<-roc.test(train_woe$def, train_woe$model, train_woe$baza,method="d")$p.value
roc_test_og<-roc.test(train_woe$def, train_woe$max, train_woe$model,method="d")$p.value
roc_test_baza
# 0
roc_test_og
# 0.003825073
# (reject H0 in both cases) roc curves are not equally good 

# ------------------------------------------------------------------------------ Plotting scores

hist(train_woe[train_woe$def=="no",c("score")])
hist(train_woe[train_woe$def=="yes",c("score")],add=TRUE,col="red", alpha=I(0.5))

# ------------------------------------------------------------------------------ GINI

# MODEL BOTH STEPWISE
gini_t<-2*auc(train_woe$def,train_woe$model,direction="<")-1
gini_w<-2*auc(validation_woe$def,validation_woe$model,direction="<")-1
gini_t
# [1] 0.6539727
gini_w
# [1] 0.6473204

# MODEL BACKWARD STEPWISE
2*auc(train_woe$def,train_woe$model_b,direction="<")-1
# [1] 0.6539727
2*auc(validation_woe$def,validation_woe$model_b,direction="<")-1
# [1] 0.6473204

# MODEL FORWARD STEPWISE
2*auc(train_woe$def,train_woe$model_f,direction="<")-1
# [1] 0.6539727
2*auc(validation_woe$def,validation_woe$model_f,direction="<")-1
# [1] 0.6473204

# MODEL ALL VARIABLES
2*auc(train_woe$def,train_woe$max,direction="<")-1
# [1] 0.6539799
2*auc(validation_woe$def,validation_woe$max,direction="<")-1
# [1] 0.6473194



# ------------------------------------------------------------------------------ confidence intervals calculation gini_t

# method "delong" - analytical formula ; "bootstrap" - simulations
ci_delong_t<-2*ci.auc(train_woe$def, train_woe$model,method="d",direction="<")-1
ci_delong_w<-2*ci.auc(validation_woe$def, validation_woe$model,method="d",direction="<")-1

ci_delong_t
# [1] 0.6445685 0.6539727 0.6633770
ci_delong_w
# [1] 0.6169587 0.6473204 0.6776822

# ------------------------------------------------------------------------------ KS-Statistics

ks_score_t<-ks.test(train_woe[train_woe$def=='no',c("score")],train_woe[train_woe$def=='yes',c("score")])$statistic
# 0.511
ks_score_w<-ks.test(validation_woe[validation_woe$def=='no',c("score")],validation_woe[validation_woe$def=='yes',c("score")])$statistic
# 0.521 - what conclusions?


# ------------------------------------------------------------------------------ Stability of a model

# PSI - checkig difference between two distirbutions (IV)
psi<-cal_psi(data1=train_woe, data2=validation_woe, bench="score",target="score",bin=20)
psi
# [1] 0.001320731

ks<-ks.test(train_woe$score,validation_woe$score)$p.value
ks
# 0.9529714

# ------------------------------------------------------------------------------ Confusion matrix
train_woe$class_prediction <-
  as.factor(ifelse(train_woe$model > 0.10,
                   "yes",
                   "no"
  ))

caret::confusionMatrix(train_woe$class_prediction,as.factor(train_woe$def))

# ------------------------------------------------------------------------------ Plotting ROC Curve

roc_rf <- roc(train_woe$def, train_woe$model)
auc(roc_rf)
# 0.827
plot(roc_rf, print.auc=TRUE)

# roc.estimate <- calculate_roc(train_woe$model, train_woe$def)
# single.rocplot <- ggroc(roc.estimate)
# plot_journal_roc(single.rocplot)

