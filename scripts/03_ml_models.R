# Title: Non-adherence to tamoxifen Breast Cancer patients ML models. 
# Programmer: Gayathri Yerrapragada

# loading data in a data frame from teradata tables

cohort <- data.table(tdRWDSquery("SELECT * FROM f_cohort;"), stringsAsFactors=F) %>% rename_all(tolower)
cohort_all <- data.table(tdRWDSquery("SELECT * FROM all_features;"), stringsAsFactors=F) %>% rename_all(tolower)
cohort1 <- merge(cohort,cohort_all, by = "enrolid")
cohort_f <- merge(cohort1,medications, by = "enrolid")
cohort_f <- merge(cohort_f,adherdata, by = "enrolid")

data <- data.frame(cohort_f) # loading data from Teradata table

#selecting columns from the dataset
pdc <- data[,c(1,5:8)]

pdc_n<- data[,c(1,11,15:18,20,26,28,29,31:36,38:42,44,58,59,60,62:66,68,69,72,73,75,77:79,93,99,101,102,113,114,125:127,130,131,133,150,151,153,170,182,190,193,
                196:200,207,210,211,213,215,217,218,221,225,226,228:233,236,239,240,245,247,266,274)]

pdc$plantyp[is.na(pdc$plantyp)] <- "missing"


# one hot encoding categorical variables

library(caret)

dmy <- dummyVars(" ~.", data = pdc)
pdc_s <- data.frame(predict(dmy, newdata = pdc))
pdc_full <- merge(pdc_s, pdc_n, by = "enrolid")

pdc_final <- pdc_full[,c(2:113)]


pdc_final[] <- lapply(pdc_final, factor) # the "[]" keeps the dataframe structure
col_names <- names(pdc_final)
# do it for some names in a vector named 'col_names'
pdc_final[col_names] <- lapply(pdc_final[col_names],factor)


### load libraries
library(caret) 
library(randomForest)
library(caTools)
library(mlbench)
library(FSelector)
library(unbalanced)

#####
####### chi-squared feature selection
#####

library(mlbench)
library(FSelector)


#Calculate the chi square statistics 
weights<- chi.squared(pdc1flag~., pdc_final)


# Print the results 
print(weights)


# Select top 40 variables
subset<- cutoff.k(weights, 40)


# Print the final formula that can be used in classification
f<- as.simple.formula(subset, "pdc1flag")
print(f)

#########  Selected features

pdc_selected <- pdc_final[,c(5,25,7,81,17,28,94,97,26,85,1,54,102,71,2,93,20,123,89,87,108,101,72,69,109,41,77,84,86,100,32,49,14,103,76,4,15,106,105,114,127)]


### data split (80 - 20 split)

set.seed(1)

n= nrow(pdc_selected)
dd=pdc_selected
#
dds=dd[order(dd$pdc1flag),]
nf=tabulate(dds$pdc1flag)
n0=nf[1]
n1=nf[2]
#
dd2=dds[1:n0,]
nn=n0+1
dd1=dds[nn:n,]
#
percent=0.80

#
np1=round(percent*n1)
#np2=n1-np1
nn1=round(percent*n0)
#nn2=n0-nn1
#
dd1a=sample(c(1:np1),np1)
#dd2a=sample(c(1:np2),np2)
dd1b=sample(c(1:nn1),nn1)
#dd2b=sample(c(1:nn2),nn2)
#
train.dd1=dd1[dd1a,]
test.dd1=dd1[-dd1a,]
#
train.dd2=dd2[dd1b,]
test.dd2=dd2[-dd1b,]
#
train.dat=rbind(train.dd1,train.dd2)
test.dat=rbind(test.dd1,test.dd2)
#
ns1=nrow(train.dat)
ns2=nrow(test.dat)
#
nt1=sample(c(1:ns1),ns1)
nt2=sample(c(1:ns2),ns2)
#
train=train.dat[nt1,]
test=test.dat[nt2,]

### NEURAL NETWORKS ###
library(caret)
# training the model

TrainingParameters <- trainControl(method = "repeatedcv", number = 10, repeats = 3)  # 10 fold CV

NNModel <- train(train[,-41], train$pdc1flag,
                 method = "nnet",
                 trControl= TrainingParameters,
                 na.action = na.omit
)

## train
NNPred_train <-predict(NNModel, train)
table(NNPred_train, train$pdc1flag)


# testing the model
NNPred1 <-predict(NNModel, test, type = "prob")


# Create confusion matrix
table(NNPred1, test$pdc1flag)


# Performance metrics 
library(mltest)
ml_test(NNPred1, test$pdc1flag, output.as.table = FALSE)

# ROC Curve - pROC
library(pROC)
roc_nnet1 = roc(test$pdc1flag, NNPred1[,1])  
plot(roc_nnet1, col = 1, lty = 1, main = "ROC")



### LOGISTIC REGRESSION ###

library(caret)
lr_1 <- train(train[,-41], train$pdc1flag,
              method = "glm",
              trControl= TrainingParameters,
              na.action = na.omit
)

summary(lr_1)


#training
train_predictions = predict(lr_1, newdata = train, type = "response")
train_class_predictions <- ifelse(train_predictions > 0.5,1,0)
table(train_class_predictions, train$pdc1flag)

#testing
test_predictions = predict(lr_1, test, type = "response")
test_class_predictions <- ifelse(test_predictions > 0.5,1,0)

# confusion matrix
table(test_class_predictions, test$pdc1flag)

#performance metrics
ml_test(test_class_predictions, test$pdc1flag, output.as.table = FALSE)


# ROC Curve
roc_lr = roc(test$pdc1flag,test_predictions)
plot(roc_lr, col = 1, lty = 1, main = "ROC",colorize = TRUE)

# Variable Importance
VarImp(lr_1)


#
### BOOSTED LOGISTIC REGRESSION ###

library(caTools)

TrainingParameters <- trainControl(method = "repeatedcv", number = 10, repeats = 3) # 10 fold CV

# training 
logitboost <- train(pdc1flag~.,train,
                    method = "LogitBoost",
                    trControl= TrainingParameters,
                    na.action = na.omit
)

#testing
logit_pred <-predict(logitboost, test, type = "prob")

# Create confusion matrix
table(logit_pred , test$pdc1flag)

#performance metrics
ml_test(logit_pred, test1$pdc1flag, output.as.table = FALSE)

#ROC Curve 
roc_logit = roc(test$pdc1flag, logit_pred[,2])
plot(roc_logit, col = 1, lty = 1, main = "ROC",colorize = TRUE)

#
### RANDOM FORESTS ###

library(randomForest)
set.seed(111)
rf_1 <- randomForest(train[,-41], y = train$pdc1flag, ntree = 1000, mtry = 7,keep.forest = TRUE, nodesize = 1 ,do.trace = TRUE,importance = TRUE)

# Predicting on train set
predTrain <- predict(rf_1, train)
# Checking classification accuracy
table(predTrain, train$pdc1flag)  

# Predicting on Validation set
predValid <- predict(rf_1, test, type = "prob")


# Confusion matrix
table(predValid,test$pdc1flag)
confusionMatrix(predValid, test$pdc1flag)

#performance metrics
ml_test(predValid, test$pdc1flag, output.as.table = FALSE) # performance evaluation

## variable importance
aa=varImpPlot(rf_1)
aaa=aa[order(-aa[,2]),]
aaa100=100*aaa[,2]/max(aaa[,2])
cn=rownames(aaa)
#png("imp.png")
#barplot(aaa100,col="red",horiz=T,names.arg=cn,las=1,
#        xlab="variable Importance")
#dev.off()

pdf("rf1.pdf",width=7,height=7,paper="a4r")
mar.default=c(4,8,2,1)+0.1
par(mar=mar.default)
barplot(aaa100,col=c("red","blue"),horiz=T,names.arg=cn,las=1,
        xlab="variable Importance", main = "Random Forest Variable importance")
dev.off()

# ROC curve
roc_rf = roc(test$pdc1flag, predValid[,1])
plot(roc_rf, col = 1, lty = 1, main = "ROC",colorize = TRUE)


### ROC plots
plot(roc_nnet1, col = 1, lty = 1, main = "ROC")
plot(roc_lr, col = 2, lty = 1, add = TRUE)
plot(roc_logit, col = 3, lty = 1, add = TRUE)
plot(roc_rf, col = 4, lty = 1, add = TRUE)
legend(.5, .4, legend=c("Feed Forward Neural Network", "Logistic Regression", "Boosted Logistic Regression", "Random Forests"),
       col=c(1,2,3,4), lty=1:1, cex=0.7)

#
####### Balancing data using SMOTE
#

##
library(unbalanced)
n1<-ncol(train)
output1<-train$pdc1flag
input1<-train[ ,-n1]

#balance the dataset
data_b <- ubSMOTE(X = input1, Y = output1, k = 5)
balancedData<-cbind(data_b$X,data_b$Y)
names(balancedData)[41] <- "pdc1flag"



#### RANDOM FOREST ###

library(randomForest)
set.seed(220)
rf1 <- randomForest(balancedData[,-41],y = balancedData$pdc1flag, ntree = 1000, do.trace = TRUE, nodesize = 32, importance = TRUE)

#pROC for roc curves training data
library(pROC)
roc1 <- roc(factor(1 * (rf1$y=="0")), rf1$votes[,1])
par(mfrow=c(1,1))
plot(roc1, main="Random forest training ROC curve", print.auc=TRUE) #black

#AUC
auc(roc1)

#prediction
prediction <- predict(rf1, test, type = "prob")

#confusion matrix
table(prediction,test$pdc1flag)
confusionMatrix(prediction, test$pdc1flag)

par(mfrow=c(1,1))

#ROC curve
roc_newrf = roc(test$pdc1flag, prediction[,1])
plot(roc_newrf, main="Random forest test ROC curve", print.auc=TRUE)

#Variable importance
aa=varImpPlot(rf1)
aaa=aa[order(-aa[,2]),]
aaa100=100*aaa[,2]/max(aaa[,2])
cn=rownames(aaa)

pdf("rf1.pdf",width=7,height=7,paper="a4r")
mar.default=c(4,8,2,1)+0.1
par(mar=mar.default)
barplot(aaa100,col=c("red","blue"),horiz=T,names.arg=cn,las=1,
        xlab="variable Importance", main = "Random Forest Variable importance")
dev.off()

#performance metric
ml_test(prediction, test$pdc1flag, output.as.table = FALSE) # performance evaluation


### NEURAL NETWORKS

TrainingParameters <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

set.seed(111)
Neural_nModel1 <- train(balancedData[,-41], balancedData$pdc1flag,
                        method = "nnet",
                        trControl= TrainingParameters,
                        na.action = na.omit
)


# testing the model
Neural_pred1 <-predict(Neural_nModel1, test)


# Create confusion matrix
table(Neural_pred1, test$pdc1flag)


# Performance metrics 
ml_test(Neural_pred1, test$pdc1flag, output.as.table = FALSE)

# ROC Curve
library(pROC)
roc_new_nnet = roc(test$pdc1flag, as.numeric(Neural_pred1)) ## Neural_pred1 
plot(roc_new_nnet, col = 1, lty = 1, main = "ROC")

###


#### LOGISTIC REGRESSION

library(caret)
set.seed(111)
lr1 <- train(balancedData[,-41], balancedData$pdc1flag,
             method = "glm",
             trControl= TrainingParameters,
             na.action = na.omit
)


#testing
test_predictions = predict(lr, test, type = "response")
test_class_predictions <- ifelse(test_predictions > 0.5,1,0)

# confusion matrix
table(test_class_predictions, test$pdc1flag)

# performance metrics
ml_test(test_class_predictions, test$pdc1flag, output.as.table = FALSE)


# ROC Curve
roc_lr = roc(test_o$pdc1flag,test_predictions)
plot(roc_lr, col = 1, lty = 1, main = "ROC",colorize = TRUE)

#Variable importance
VarImp(lr)

#
### BOOSTED LOGISTIC REGRESSION ###
library(caTools)

TrainingParameters <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

# training 

logitboost <- train(pdc1flag~.,balancedData,
                    method = "LogitBoost",
                    trControl= TrainingParameters,
                    na.action = na.omit
)

#testing
logit_pred <-predict(logitboost, test)

# Create confusion matrix
table(logit_pred , test$pdc1flag)

#performance metrics
ml_test(logit_pred, test$pdc1flag, output.as.table = FALSE)

#ROC Curve 
roc_logit = roc(test$pdc1flag, as.numeric(logit_pred))
plot(roc_logit, col = 1, lty = 1, main = "ROC",colorize = TRUE)


### ROC plots

plot(roc_new_nnet, col = 1, lty = 1, main = "ROC")
plot(roc_lr, col = 2, lty = 1, add = TRUE)
plot(roc_logit, col = 3, lty = 1, add = TRUE)
plot(roc_newrf, col = 4, lty = 1, add = TRUE)
legend(.5, .4, legend=c("Feed Forward Neural Network", "Logistic Regression", "Boosted Logistic Regression", "Random Forests"),
       col=c(1,2,3,4), lty=1:1, cex=0.7)
