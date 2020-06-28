library(leaps) #subsetting selection
library(VSURF)
library(LogisticDx) # - gof()
library(pROC) # - auc(), roc.test itp.
library(gtools) # smartbind()
library(plotROC)

source("model_functions.R")

train_woe$def_two <- NULL
test_woe$def_two <- NULL
# ------------------------------------------------------------------------------ Baseline model estimation

baza<-glm(def ~ 1,data=train_woe, family=binomial("logit"))

max<-glm(def ~ .,data=train_woe, family=binomial("logit"))
summary(max)


# ------------------------------------------------------------------------------ Forward

tim <- proc.time ()[1]	## applying cor (standard R implementation)
model_stepwise_for<-step(baza, scope = list(upper=max, lower=~1 ), direction = "forward", trace=T,steps=30,k=2)
cat ("cor runtime [s]:", proc.time ()[1] - tim, "(n =", ncol (baza)-3, ")\n")
save(model_stepwise_for,file="data/model_stepwise_for.rdata")
summary(model_stepwise_for)

# ------------------------------------------------------------------------------ Evaluation

mdl<-"model_stepwise_both"
model<-model_stepwise_for

################################################################################


gf<-pchisq(model$deviance, model$df.residual,lower.tail = F)

ist<-pchisq(model$null.deviance-model$deviance, model$df.null-model$df.residual,lower.tail = F)


hr<-hosmerlem(y=train_woe$def, yhat=fitted(model),g=10)
hosmerlem(y=train_woe$def, yhat=fitted(model),g=7)
hosmerlem(y=train_woe$def, yhat=fitted(model),g=8)
hosmerlem(y=train_woe$def, yhat=fitted(model),g=9)
hr$p.value


# ------------------------------------------------------------------------------ Assaigning PD to train_woe 

train_woe$baza<-baza$fitted.values
train_woe$model<-model$fitted.values
train_woe$max<-max$fitted.values


# ------------------------------------------------------------------------------ Score computation (train and test)

train_woe$score<-(660-40/log(1/2)*log(1/72))+40/log(1/2)*model$linear.predictors

test_woe$model<-predict(model, newdata=test_woe, type="response") 
test_woe$score<-(660-40/log(1/2)*log(1/72))+40/log(1/2)*predict(model, newdata=test_woe, type="link") 

train_woe$model<-predict(model, newdata=train_woe, type="response") 
train_woe$score<-(660-40/log(1/2)*log(1/72))+40/log(1/2)*predict(model, newdata=train_woe, type="link") 

# ------------------------------------------------------------------------------ ROC

roc_test_baza<-roc.test(train_woe$def, train_woe$model, train_woe$baza,method="d")$p.value
roc_test_og<-roc.test(train_woe$def, train_woe$max, train_woe$model,method="d")$p.value
roc_test_baza
roc_test_og

# ------------------------------------------------------------------------------ Plotting scores

hist(train_woe[train_woe$def=="no",c("score")])
hist(train_woe[train_woe$def=="yes",c("score")],add=TRUE,col="red", alpha=I(0.5))

# ------------------------------------------------------------------------------ GINI

gini_t<-2*auc(train_woe$def,train_woe$model,direction="<")-1
gini_w<-2*auc(test_woe$def,test_woe$model,direction="<")-1

2*auc(train_woe$def,train_woe$max,direction="<")-1
# [1] 0.3094761

# ------------------------------------------------------------------------------ confidence intervals calculation gini_t

# method "delong" - analytical formula ; "bootstrap" - simulations
ci_delong_t<-2*ci.auc(train_woe$def, train_woe$model,method="d",direction="<")-1
ci_delong_w<-2*ci.auc(test_woe$def, test_woe$model,method="d",direction="<")-1

ci_delong_t
# [1] 0.3055929 0.3080511 0.3105093
ci_delong_w
# [1] 0.2985146 0.3059324 0.3133503

# ------------------------------------------------------------------------------ KS-Statistics

ks_score_t<-ks.test(train_woe[train_woe$def==0,c("score")],train_woe[train_woe$def==1,c("score")])$statistic
ks_score_w<-ks.test(test_set[test_set$def==0,c("score")],test_set[test_set$def==1,c("score")])$statistic
# both around 0.22 - what conclusions?


# ------------------------------------------------------------------------------ Stability of a model

# PSI - checkig difference between two distirbutions (IV)
psi<-cal_psi(train_woe1=train_woe, train_woe2=test_set, bench="score",target="score",bin=20)
psi
# [1] 0.001100005

ks<-ks.test(train_woe$score,test_set$score)$p.value
ks
# [1] 1.735169e-07

# ------------------------------------------------------------------------------ Confusion matrix
train_woe$class_prediction <-
  as.factor(ifelse(train_woe$model > 0.10,
                   "yes",
                   "no"
  ))

caret::confusionMatrix(train_woe$class_prediction,as.factor(train_woe$def))

# ------------------------------------------------------------------------------ Plotting ROC Curve



roc.estimate <- calculate_roc(train_woe$model, train_woe$def)
single.rocplot <- ggroc(roc.estimate)
plot_journal_roc(single.rocplot)
abline(0,1)

