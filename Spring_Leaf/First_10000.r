##Set up the environment 
setwd("~/Documents/Project/Springleaf")
data = read.csv("train.csv",nrows=10000)
raw_data = data[,-1]  #remove id column 
###############################
#Preprocess the data
###############################
##look close at the data##
##1.Store the data with multiple/5 levels into a new data frame-Char_data
##2.Those colounm will be NA in the  raw_data 
char_data = data.frame(rep(NA,nrow(raw_data)))
k=1
for (i in 1:ncol(raw_data)){
  if (is.factor(raw_data[,i])){
    if (length(levels(raw_data[,i])) > 5){
      char_data[,k]  = raw_data[,i]
      char_data=data.frame(char_data,rep(NA,nrow(raw_data)))
      names(char_data)[k]=colnames(raw_data)[i]
      k = k+1
      raw_data[,i] = NA
    }
    else{
      raw_data[,i] = as.numeric(raw_data[,i])
    }
  }
}


#Delete NA colomn 
raw_data = raw_data[, colSums(is.na(raw_data)) ==0]

###create Y
Y = raw_data[,"target"]
##############
#Scaling the feature 
#############
##Becuase the range is too large，let's normalize them 
library(clusterSim)
raw_data_Normal = data.Normalization(subset(raw_data,select=-c(target)),type="n1",normalization="column")
#Delete NA colomn 
raw_data_Normal  = raw_data_Normal [, colSums(is.na(raw_data_Normal)) ==0]
###############################
#Split the data 
###############################
library(caret)
set.seed(10086)
train_index = createDataPartition(y=Y,p=0.7,list=FALSE)
test_index = createDataPartition(y=Y,p=0.3,list=FALSE)
cv_index = createDataPartition(y=Y,p=0.3,list=FALSE)
train = raw_data_Normal[train_index,]
y_train = Y[train_index]
test = raw_data_Normal[test_index,]
y_test = Y[test_index]
cv = raw_data_Normal[cv_index,]
y_cv = Y[cv_index]

###############################
#Threshold
###############################
table(Y)
2199/(7801+2199) 
##Because the percentage of 1 is only 21.2%, so we set the threshold is 21.2%
##and we will use F1 score to evaluate 

###############################
#Algorithm:xgboost 
###############################
##Parameters setting 
param <- list("objective" = "reg:logistic",    # 0/1 classification 
              "num_class" = 2,    # number of classes 
              "eval_metric" = "auc",    # evaluation metric 
              "max_depth" = 16,    # maximum depth of tree 
              "eta" = 0.3,    # step size shrinkage 
              "gamma" = 0,    # minimum loss reduction 
              "subsample" = 1,    # part of data instances to grow tree ,range[0,1]
              "colsample_bytree" = 1,  # subsample ratio of columns when constructing each tree 
              "min_child_weight" = 1,  # minimum sum of instance weight needed in a child 
              "set.seed"=10086,
              "lambda"=10
)
##data processing
dtrain = as.matrix(train)
y = y_train
library(xgboost)
cv.res=xgb.cv(data=dtrain,param=param,nfold=4,label=y,nrounds=100,prediction=TRUE,verbose=TRUE)
cv.res.plot = subset(cv.res$dt,select=c(train.auc.mean,test.auc.mean))
plot(cv.res.plot$train.auc.mean,type="l",col="blue",ylim=c(0,1),xlab="numbers of round",ylab="AUC")
lines(cv.res.plot$test.auc.mean, type="l", pch=22,col="red")
title(main="xgboost", col.main="red", font.main=4)

###############################
#Dignosie Result
###############################
##From the plot, we can find that the grapgic has a high variance problem, which means overfitting

###############################
#Three Stategy 
###############################
##1. Get more data
##2. Try smaller set of features
##3. Try increase lambda (regularization term)

###############################
##Try step 2 first, step3 later and step 1 in the last round 
###############################
###################
#Remove the non-zero variance 
###################
##For All data
non_zero = nearZeroVar(raw_data_Normal,saveMetrics = TRUE)
non_zero_rows = row.names(subset(non_zero,zeroVar=="FALSE" & nzv=="FALSE"))
data_non_zero = raw_data_Normal[,c(non_zero_rows)]
###################
#PCA
###################
data_non_zero.pca <- prcomp(data_non_zero,center = TRUE,scale. = TRUE) 
data_non_zero_pca = predict(data_non_zero.pca,data_non_zero)[,1:46] #80% variances are explained 
train.pca = data_non_zero_pca[train_index,]
test.pca = data_non_zero_pca[test_index,]
cv.pca = data_non_zero_pca[cv_index,]

#########
#Xgboost V.S size of sample
########
xgb_test_data = data.frame(n.sample.size=rep(NA,15),test.auc.mean=rep(NA,15),train.auc.mean=rep(NA,15))
j = 0
for (p in seq(0.3,1,by=0.05)){
    j=j+1
  xgb_test_index = createDataPartition(y=y_train,p=p,list=FALSE)
  cv.res.pca=xgb.cv(data=train_pca[xgb_test_index,],param=param,nfold=4,label=y[xgb_test_index],nrounds=50,prediction=TRUE,verbose=TRUE)
  xgb_test_data$test.auc.mean[j] = cv.res.pca$dt[which.max(cv.res.pca$dt$test.auc.mean),]$test.auc.mean
  xgb_test_data$train.auc.mean[j] = cv.res.pca$dt[which.max(cv.res.pca$dt$train.auc.mean),]$train.auc.mean
  xgb_test_data$n.sample.size[j] = p*nrow(train_pca)
}
plot(x=xgb_test_data$n.sample.size,y=xgb_test_data$test.auc.mean,type="l",
     ,ylim=c(0.4,1.1),col="blue",xlab="numbers of round",ylab="AUC")
lines(x=xgb_test_data$n.sample.size,y=xgb_test_data$train.auc.mean, pch=22,col="red")
title(main="xgboost", col.main="red", font.main=4)

#From the plot, we can find that the 46 feature could explain good as 1934 features do, if the numbers of round are high than 15
###################
#To inrease the lambda
###################
##We have tried to increase the lambda, the error rate did not improve well

###################
#Increase the data
###################
##Before we increase the data, we should build the new feature which can explain the data better

######################################
#Second Hidden Layers
######################################
#train.pca
#test.pca
#cv.pca
################
#1.Logistic regression
################
library(boot)
train_2 = data.frame(train.pca,Y=y_train)
train_2$Y = as.factor(train_2$Y)
log.fit = glm(Y~.,data=train_2,family="binomial"(link = "logit"))
logi_feat = predict(log.fit,newdata=data.frame(test.pca),type = "response")
###cv.glm-tuning parameters of the functions
#log.err.data = data.frame(cv.err=rep(NA,15),threshold=rep(NA,15))
#k = 1
#for (i in seq(0.1,1,by=0.02)){
 # cost.logi <- function(Y, pi = 0) mean(abs(Y-pi) < i)
  #set.seed(10086)
  #log.err.data[k,1]=cv.glm(data=train_2,glmfit=log.fit,cost=cost.logi,K=10)$delta[1]
  #log.err.data[k,2]=i
  #k = k + 1
#}
#plot(x=log.err.data$threshold,y=log.err.data$cv.err,type="l")
##Roc function
library(ROCR)
count = 0 
record.F1 = data.frame(F1=NA,threshold=NA)
for (k in seq(0.1,1,by=0.05)){
threshold = k
count = count + 1
logi_feat.y =logi_feat
for (i in 1:length(logi_feat)){
  if (logi_feat[i] >= threshold){
    logi_feat.y[i] = 1
  }
  else{
    logi_feat.y[i] = 0
  }
}
con = confusionMatrix(logi_feat.y,y_test)
#F1 Score:
p = con$byClass["Neg Pred Value"]
r = con$byClass["Specificity"] 
F1= 2*(p*r/(p+r))
record.F1[count,1]=F1
record.F1[count,2]=k
}
record.F1
plot(x=record.F1$threshold,y=record.F1$F1,type="l",ylab="F1_Score",xlab="threshold")
best_thresh = record.F1[which.max(record.F1$F1),]
##Best Threshold: 0.2,F1 score is 44.62

###############
##Evaluation - Cross Validation
###############
logi_cv = predict(log.fit,newdata=data.frame(cv.pca),type = "response")
for (i in 1:length(logi_cv)){
  if (logi_cv[i] >= 0.2){
    logi_cv[i] = 1
  }
  else{
    logi_cv[i] = 0
  }
}
con = confusionMatrix(logi_cv,y_cv)
p = con$byClass["Neg Pred Value"]
r = con$byClass["Specificity"] 
F1= 2*(p*r/(p+r))
F1
##46.68% for F1 score, which is not bad, at least there is no over-fitting for the thresh

