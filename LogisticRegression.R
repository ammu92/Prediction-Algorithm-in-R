library(ggplot2)
library(dplyr)
library(readr)
library(corrplot)
library(caret)
library(ROCR)
library(tree)
library(randomForest)
library(rstanarm)
library(pROC)

pima <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/pima-indians-diabetes/pima-indians-diabetes.data",col.names=c("Pregnant","Plasma_Glucose","Dias_BP","Triceps_Skin","Serum_Insulin","BMI","DPF","Age","Diabetes"), sep=",")
sapply(pima, function(x) sum(is.na(x)))
summary(pima)
pima$Diabetes <- as.factor(pima$Diabetes)
pima <- pima[apply(pima[,c(2,3,6)],1,function(x) !any(x==0)),]


set.seed(15689)
index <- createDataPartition(pima$Diabetes,p = 0.7,list = F)
train <- pima[index,]
test  <- pima[-index,]

#Logistic Regression
m1 <- glm(Diabetes ~ ., data = train, family = binomial(link = "logit"))
summary(m1)
anova(m1,test = "Chisq")
mod_fin <- glm(Diabetes ~ Pregnant + Plasma_Glucose + Triceps_Skin + BMI + DPF,
               data = train, family = binomial(link = "logit"))
summary(mod_fin)
summary(residuals(mod_fin))
par(mfrow=c(2,2))
plot(mod_fin)
test_pred <- predict(mod_fin,test, type = "response")
pred_test <- as.data.frame(cbind(test$Diabetes,test_pred))
colnames(pred_test) <- c("Original","Test_pred")
pred_test$outcome <- ifelse(pred_test$Test_pred > 0.5, 1, 0)
error <- mean(pred_test$outcome != test$Diabetes)
print(paste('Test Data Accuracy', round(1-error,2)*100,'%'))
confusionMatrix(test$Diabetes,pred_test$outcome)
acc_lg <- confusionMatrix(test$Diabetes,pred_test$outcome)$overall['Accuracy']

par(mfrow=c(1,1))
plot.roc(test$Diabetes,test_pred,percent=TRUE,col="#1c61b6",print.auc=TRUE,
         main = "Area under the curve for Logistic Regression")
