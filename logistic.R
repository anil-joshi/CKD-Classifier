## Step 0  - Read in Data
data=read.csv("imputed_data.csv")
names(data)
data=data[,-1]  ## remove ID


## Step 1 - Explore and relabel Data
y=data$CKD
class(data)
summary(data)

out_sample=which(is.na(data$CKD)==1) ## without ckd (2000 prediction variables)

data_out=data[out_sample,]   ## the ones without a disease status 2819
data_in=data[-out_sample,]   ## the ones with a disease status 6000
summary(data_in)

dim(data_out) # 2819
dim(data_in)  # 6000

data_in=na.omit(data_in)
dim(data_in) # after ignore missing value, we have 4136 data points / after we filled the dataset, 6000


## Step 2  - Run the Logistic Regression with one variable
names(data)
model=glm(CKD~Age,family="binomial",data=data_in)
summary(model)


## Step 3  - Run the Logistic Regression on all data, explore backward elimination
dim(data)
dim(data_in)
model=glm(CKD~.,family="binomial",data=data_in)
summary(model)

model4=step(model,direction="backward")
# this will run the ENTIRE model with all variables, and then remove one at a time according to a 
#  p-value of the coefficient. it will result in only those variables "which matter"
#   ** note - some consider this unethical because you see the results before you select variables
formula(model4)
summary(model4)


## Step 4 - Explore your new model
formula(model3)
summary(model3)

model3=glm(CKD~Age+Height+Obese+LDL+PVD+Female+Hypertension+Diabetes+CVD+Anemia+
           SBP  + Unmarried + DBP+ Racegrpother + Racegrphispa + Racegrpblack,family="binomial",data=data_in)
summary(model3)
formula(model3)

#################
# Training & Testing Data
set.seed (6000)
seperate_data <- sample(2, nrow(data_in),
                        replace = TRUE,
                        prob = c(0.8, 0.2))

train <- data_in[seperate_data == 1,]
test <- data_in[seperate_data == 2,]

dim(train) # 4774 data points
dim(test) # 1226 data points

model_train = glm(CKD~Age+Height+Obese+LDL+PVD+Female+Hypertension+Diabetes+CVD+Anemia+
           SBP  + Unmarried + DBP+ Racegrpother + Racegrphispa + Racegrpblack,family="binomial",data=train)
summary(model_train)


predict_test = round(predict(model_train, test, type = "response"),0)
predict_test
confusionMatrix(table(predict_test, test$CKD))
# 80/20 method: 94% of data fit in the model, kappa coefficient is 0.29

model_test <- glm(CKD~Age+Height+Obese+HDL+Unmarried+PVD+Female+Hypertension+Diabetes+CVD+Anemia+Racegrphispa,family="binomial",data=test)
summary(model_train)


##########################
library(caret)
library(lattice)
library(ggplot2)
set.seed(6000)

crossValSettings <- trainControl( method = "repeatedcv",
                            number = 10,
                            savePredictions = TRUE, 
                            repeats = 5)

crossVal <- train(as.factor(CKD)~Age+Height+Obese+LDL+PVD+Female+Hypertension+Diabetes+CVD+Anemia+
           SBP  + Unmarried + DBP+ Racegrpother + Racegrphispa + Racegrpblack,
                  data = data_in, 
                  family = "binomial",
                  method = "glm",
                  trControl = crossValSettings)
                
crossVal # crossval based on 6000 set
  # kappa: reliability: 0.25  (better to reach to 0.8)
  # compare with 80/20, model fits

ori = data_in$CKD

pred <- predict(crossVal, newdata = data_in) # used crossVal model to predict the orignial data
confusionMatrix(table(pred, ori)) # FN: 377 FP: 57
# sensitivity: able to detect the diease is 98%

#####
crossVal_full <- train(as.factor(CKD)~ Age + Weight + Height + Waist + DBP + HDL + LDL + Female + 
    Unmarried + Obese + PVD + Hypertension + Diabetes + CVD + 
    CHF + Anemia + Racegrphispa,
                  data = data_in, 
                  family = "binomial",
                  method = "glm",
                  trControl = crossValSettings)
crossVal_full

#####

crossVal_full <- train(as.factor(CKD)~.,
                       data = data_in, 
                       family = "binomial",
                       method = "glm",
                       trControl = crossValSettings)
crossVal_full

ori = data_in$CKD

pred_full <- predict(crossVal_full, newdata = data_in) # used crossVal model to predict the orignial data
confusionMatrix(table(pred_full, ori)) # FN: 377 FP: 57
# sensitivity: able to detect the diease is 98%


# Age + Waist + HDL + Female + Unmarried + Obese + Hypertension + Diabetes + CVD + Anemia + Racegrphispa
model4 =glm(CKD~Age  + HDL + Unmarried + PVD + Hypertension + Diabetes + CVD + Anemia + Racegrphispa,family="binomial",data=data_in)
summary(model4)

########################
# K Nearest Neighbour - Classification Algorithm
#######################

# crossVal_k <- train(as.factor(CKD)~.,
                     # data = train, 
                      #method = "knn",
                      # trControl = crossValSettings)

#crossVal_k

##################
# plot
predicted.data <- data.frame(probability.of.ckd = model3$fitted.values, ckd=data_in$CKD)
predicted.data <- predicted.data[order(predicted.data$probability.of.ckd, decreasing = FALSE),]
predicted.data$ckd <- 1:nrow(predicted.data)
library(ggplot2)
library(cowplot)
ggplot(data = predicted.data, aes(x = ckd, y = probability.of.ckd)) +
  geom_point(aes(color = ckd),alpha =1, shape= 4, stroke = 2)+
  xlab("Index") +
  ylab("Predicted Probability of getting CKD")

##########
# chisq.test(data_in, correct = TRUE)

# model_test= glm(CKD~Age+Female+Unmarried + PVD + Hypertension +Diabetes + CVD +Anemia +Racegrphispa, family="binomial",data=data_in)
# summary(model_test)
# confidence intervals of the model coefficients (should not include 0 if p-value<.05)
confint = confint.default(model3)# if includes 0, means no relvant

write.csv(confint, "confint_6000.csv")
confint(model)


## Step 5 - Hypotehsis test of model, Compare 2 models, Definition 5-3
with(model3, null.deviance - deviance)
##df
with(model3, df.null - df.residual)
## pvalue of difference
with(model3, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
# if <.05, then model is significant from a null model (model with no variables)
# note that you can do this incrementally by adding one variable at a time.


## Step 5 - Alternate. Ho:  Model Fits the Data, Ha: Model does not Fit, Definition 5-2
## devniance
-2*logLik(model)
## test
with(model3, pchisq(deviance, df.residual, lower.tail = FALSE))


## Step 6 - Predict probabilities and Odds Ratios of New Data
## predictions of new data 
newdata1=data_out[1:4,]  ## these are 4 "new patients"
newdata1  ## uh oh, 1 has missing data! damn!
phatnew=predict(model3, newdata = newdata1, type = "response")

## odds ratios
phatnew/(1-phatnew)


## Step 7 - Predict and Plot in-sample data
phat3=predict(model3,type="response")  # predicts for ALL in sample data
round(summary(phat3),4)  # probabilities range from .01% to 83.4%   - cool!

# let's compare that to a "simple" model with only age
model=glm(CKD~Age,family="binomial",data=data_in)
Age=seq(0,100,by=.1)
phat=predict(model,list(Age = Age),type="response")
summary(phat)  # range from 0.02% to 76.2%, it's similar but doesn't fit as well
plot(data_in$Age, data_in$CKD, pch = 16, xlab = "Age", ylab = "CKD")
lines(Age, phat)

## plot the actual probabilities for the "complicated" model
plot(data_in$Age, data_in$CKD, pch = 16, xlab = "Age", ylab = "CKD")
points(data_in$Age, phat3,col="blue")  # this plots all phat3's according to age


## Step 8 - Classification
summary(phat3)
classify=ifelse(phat3>.50,1,0)  # this is a threshold, we say if probability >50% , then say "yes"
summary(classify)  # notice that not many are "yes"  - is this desirable?
# [NOTE] 1.69% of people who have patient
# $100 cost

acc_l <- c()
for(trial in (0:100))
{
  ratio <- 0.0
  ratio <- ratio + trial * 0.01
  classify <- ifelse(phat3 > ratio, 1, 0)
  acc = round(c_accuracy(data_in$CKD, classify), 4)
  print(acc)
  acc_l <- c(acc_l, acc)
  #cost <- acc[9] * 100 + acc[10] * 200
  #print(cost)
}
acc_l
write.csv(acc_l, "result.csv")

round(c_accuracy(data_in$CKD,classify),2)  # to run this you must run my code below first.
# jump to the end (professor's code)

round(c_accuracy(data_in$CKD,classify),2)

# consider the cost
classify_full=ifelse(phat_full>.50,1,0)  # this is a threshold, we say if probability >50% , then say "yes"
summary(classify_full) 
round(c_accuracy(data_in$CKD,classify_full),2)  
phat_full = predict(model, type = "response")
round(summary(phat_full), 4)


## Step 9 - Caclculate Costs
acc=c_accuracy(data_in$CKD,classify)
c1=100   # penalize me  $100 for a false positive
c2=200  #  penalize me $200 for a false negatives
cost=acc[9]*c1+acc[10]*c2

cost  

install.packages('pROC')
library(pROC)
pROC(data_in$CKD,phat3)
roc(data_in$CKD~phat3,percent=TRUE,plot=TRUE)

model=roc(data_in$CKD~phat3,percent=TRUE,plot=TRUE)



## Function Below, RUN THIS FIRST
## make sure actuals and classifications are 0 (no) or 1 (yes) only 
##  Built by Prof. Matthew J. Schneider

c_accuracy=function(actuals,classifications){
  df=data.frame(actuals,classifications);
  
  
  tp=nrow(df[df$classifications==1 & df$actuals==1,]);        
  fp=nrow(df[df$classifications==1 & df$actuals==0,]);
  fn=nrow(df[df$classifications==0 & df$actuals==1,]);
  tn=nrow(df[df$classifications==0 & df$actuals==0,]); 
  
  
  recall=tp/(tp+fn)
  precision=tp/(tp+fp)
  accuracy=(tp+tn)/(tp+fn+fp+tn)
  tpr=recall
  fpr=fp/(fp+tn)
  fmeasure=2*precision*recall/(precision+recall)
  scores=c(recall,precision,accuracy,tpr,fpr,fmeasure,tp,tn,fp,fn)
  names(scores)=c("recall","precision","accuracy","tpr","fpr","fmeasure","tp","tn","fp","fn")
  
  #print(scores)
  return(scores);
}




