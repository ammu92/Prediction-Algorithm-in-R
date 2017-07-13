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
pima <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/pima-indians-diabetes/pima-indians-diabetes.data",col.names=c("Pregnant","Plasma_Glucose","Dias_BP","Triceps_Skin","Serum_Insulin","BMI","DPF","Age","Diabetes"))
sapply(pima, function(x) sum(is.na(x)))
summary(pima)
pima$Diabetes <- as.factor(pima$Diabetes)
pima <- pima[apply(pima[,c(2,3,6)],1,function(x) !any(x==0)),]
set.seed(15689)
index <- createDataPartition(pima$Diabetes,p = 0.7,list = F)
train <- pima[index,]
test  <- pima[-index,]

#Random Forest
m_dt <- tree(Diabetes ~ ., data = train)
pred_dt <- predict(m_dt, train, type = "class")
confusionMatrix(train$Diabetes,pred_dt)[2:3]
plot(m_dt)
text(m_dt, pretty = 0)

pred_dt_test <- predict(m_dt, test, type = "class")
confusionMatrix(test$Diabetes,pred_dt_test)

acc_dt <- confusionMatrix(pred_dt_test,test$Diabetes)$overall['Accuracy']
set.seed(15689)
opt_mod <- tuneRF(train[-as.numeric(ncol(train))],train$Diabetes,ntreeTry = 150, 
                  stepFactor = 2, improve = 0.05,trace = T, plot = T, doBest = F)
mtry_fin <- opt_mod[as.numeric(which.min(opt_mod[,"OOBError"])),"mtry"]

rf_fin <- randomForest(Diabetes~.,data=train, mtry=mtry_fin, ntree=101, 
                       keep.forest=TRUE, proximity=TRUE, importance=TRUE,test=test)

pred_test <- predict(rf_fin, newdata = test)
confusionMatrix(test$Diabetes,pred_test)
acc_rf <- confusionMatrix(test$Diabetes,pred_test)$overall['Accuracy']

par(mfrow=c(1,2))
varImpPlot(rf_fin,type = 2,main = "Variable Importance",col = 'black')
plot(rf_fin,main = "Error vs no. of trees grown")

setwd("E:/MTech CSE/Sem2/Minor Project/Predictive algorithm")
new_data  <- read.table("pima_indians_diabetes_new.txt",col.names=c("Pregnant","Plasma_Glucose","Dias_BP","Triceps_Skin","Serum_Insulin","BMI","DPF","Age","Diabetes"),sep=",")
pred_test <- predict(rf_fin, newdata = new_data)
head(pred_test)
head(new_data$Diabetes)
library(RDCOMClient)
a <- table(head(pred_test))
if(a[names(a)==1]>=3)
{
  OutApp <- COMCreate("Outlook.Application")
  
  outMail = OutApp$CreateItem(0)
  outMail[["To"]] = "amrutakulkarni@outlook.com"
  outMail[["subject"]] = "Indicative Diabetes"
  outMail[["body"]] = "Your health parameters indicate Diabetes. Please consult a doctor for getting the required medication. You can also try the following home remedies:
#1 Holy Basil (tulsi) leaves
  + The leaves of holy basil are packed with antioxidants and essential oils that produce eugenol, methyl eugenol and caryophyllene. Collectively these compounds help the pancreatic beta cells (cells that store and release insulin) to function properly and increase sensitivity to insulin. An added advantage is that the antioxidants present in the leaves help beat the ill effects of oxidative stress.
  + Tip: Consume two to three tulsi leaves whole or about one tablespoon full of its juice on an empty stomach to lower the blood sugar levels. Here are top 10 health benefits of tulsi
  + #2 Flax seeds (Alsi)
  + Due to their high fibre content flaxseeds help digestion and aid in the proper absorption of fats and sugars. Consuming flax seed helps reduce a diabetic's postprandial sugar level by almost 28 per cent.
  + Tip: Consume one tablespoon of ground flaxseed powder every morning on an empty stomach with a glass of warm water. However, do not have more than 2 tablespoons per day, as it can be detrimental to your health. Here are 11 ways to include flaxseeds in your diet.
  + #3 Leaves of bilberry (neelabadari) plant
  + The leaves of bilberry have been used in Ayurveda for many centuries to control diabetes. Recently, the Journal of Nutrition stated that the leaves of the Bilberry plant contain high amounts of anthocyanidin, which enhance the action of various proteins involved in glucose transportation and fat metabolism. Due to this unique property, bilberry leaves are a great way to lower one's blood sugar levels.
  + Tip: Crush bilberry leaves in a mortar and pestle and consume 100 milligrams of this extract everyday on an empty stomach.
  + #4 Cinnamon (dalchini)
  + Also known as dalchini, it improves insulin sensitivity and lower blood glucose levels. Having as little as ½ teaspoon of cinnamon per day can improve one's insulin sensitivity and help controlling weight, thereby decreasing one's risk for heart disease.
  + Tip: Include about 1 gram of dalchini into your daily diet for about a month to help lower blood sugar levels. Read more health benefits of cinnamon.
  + #5 Green Tea
  + Unlike other tea leaves, green tea is unfermented and is high in polyphenol content. Polyphenol is a strong antioxidant and hypo-glycaemic compound that helps control the release of blood sugars and helps the body use insulin better. Read more 10 types of flavoured green tea that have 20 health benefits.
  + Tip: Steep a bag of green tea in hot water for 2-3 minutes. Remove the bag and drink a cup of this tea in the morning or before your meals.
  + #6 Drumstick leaves
  + Also called moringa, the leaves of this plant are best known for their ability to boost one's energy. In the case of diabetics, the moringa leaf increases satiety and slows the breakdown of food and lower blood pressure.
  + Tip: Take a few drumstick leaves, wash and crush them to extract their juice. Now take about 1/4th cup of this juice and drink it on an empty stomach, every morning to keep your sugar levels under control.
  + #7 Psyllium husk (Isabgol)
  + Also known as psyllium husk is often used as a laxative. When isabgol comes in contact with water, it swells to form a gel-like substance. This slows the breakdown and absorption of blood glucose. Isabgol also protects the stomach lining from ulcers and acidity.
  + Tip: Cosume isabgol after every meal, ideally with milk or water. Avoid having it with curd as it can lead to constipation. Read in detail about 8 health benefits of isabgol or psyllium husk you didn't know.
  + #8 Bitter gourd (Karela)
  + Rich in plant insulin-polypeptide-P, a bio-chemical that mimics the insulin produced by the human pancreas and thus, reduces sugar levels in the body. You may like to read about home remedies for diabetes. It is also known to be highly beneficial for diabetics owing to the two very essential compounds called charatin and momordicin, which are the key compounds in lowering one's blood sugar levels.
  + Tip: Consume karela at least once a week either as a subzi or in a curry. If you want quick results, try having a glass of karela juice on an empty stomach once in three days. Read more about 8 healthy reasons to drink bittergourd or karela juice!
  + #9 Neem
  + Found abundantly in India, the bitter leaf has a number of amazing medicinal properties. Neem enhances insulin receptor sensitivity, helps improve blood circulation by dilating the blood vessels, lowers blood glucose levels and reduces one's dependence on hypoglycaemic drugs.  Here are more health benefits of neem.
  + Tip: Drink the juice of the tender shoot of neem leaves on an empty stomach for best results.
  + #10 Indian blackberry (Jamun)
  + A glycoside present in the seeds of Indian blackberry prevents the conversion of starch to sugar. It lowers blood sugar and helps prevent insulin spikes. Jambul also has properties that can protect you from heart diseases and other vascular disorders.
  + Tip: Eat around 5 - 6 jamuns in the morning to control your blood sugar levels. Alternatively, you can also add a spoonful of jamun seeds powder to a glass of warm water or milk and drink this daily for better control of diabetes."
  outMail$Send()
}

