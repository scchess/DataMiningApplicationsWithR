###########################################################
###   14.5. MODEL DEVELOPMENT: BUILDING CLASSIFIERS   #####
###########################################################
rm(list=ls())
load(file="object_scores.RData")

########################
## 14.5.1 LEARNING STEP 
########################
dtset.ind <- data.frame(Xc, y)
set.seed(987654)
idx <- sample(1:nrow(dtset.ind),80)
learn <- dtset.ind[-idx,]
test  <- dtset.ind[idx,] 


###########################################################
## 14.5.1.1 RANDOM FOREST (RF)
###########################################################
library(caret)
library(doParallel)
clus <- parallel::makeCluster(spec=6, type='PSOCK')
registerDoParallel(clus)
ctrl.train <- trainControl(method='repeatedcv',number=10,repeats=15)
fit.rf <- train(y ~ ., data=learn, method='rf', metric='Accuracy',
           tuneGrid=expand.grid(.mtry=1:6),trControl=ctrl.train,
           ntree=1000)
stopCluster(clus)
print(fit.rf)
y.rf <- predict(fit.rf$finalModel, newdata=test[,1:6],type='class')


###########################################################
## 14.5.1.2 CLASSIFICATION NEURAL NETWORK (NNET)
###########################################################
clus <- parallel::makeCluster(spec=6, type='PSOCK')
registerDoParallel(clus)
fit.nnet <- caret::train(y~., data=learn, method='mlp', metric='Accuracy',
             tuneGrid=expand.grid(.size=1:15),learnFunc="SCG",
             trControl=ctrl.train)
stopCluster(clus)
summary(fit.nnet$finalModel)
probs.nnet <- predict(fit.nnet$finalModel, newdata=test[,1:6])
head(probs.nnet)
y.nnet <- apply(probs.nnet,1,which.max)
y.nnet <- factor(y.nnet,levels=1:3,labels=levels(test$y))


###########################################################
## 14.5.1.3 K-NEAREST NEIGHBOR ALGORITHM (KNN)
###########################################################
clus <- parallel::makeCluster(spec=6, type='PSOCK')
registerDoParallel(clus)
fit.knn <- caret::train(y~., data=learn, method='knn',
            tuneGrid=expand.grid(.k=5:100), 
            metric='Accuracy', trControl=ctrl.train)
stopCluster(clus)
yhat.knn<-predict(fit.knn$finalModel,newdata=test[,1:6],type="class")


###########################################################
## 14.5.1.4 NAÏVE BAYESIAN CLASSIFICATION (NBAYES)
###########################################################
library(klaR)
fit.NB <- NaiveBayes(y~., data=learn)
pred.NB <-  predict(fit.NB, newdata=test)
probs.NB <- pred.NB$posterior
y.nb <- pred.NB$class


###########################################################
## 14.5.1.5 MULTINOMIAL LOGISTIC REGRESSION MODEL (MLOGIT)
###########################################################
library(globaltest)
fit.mlog1 <- mlogit(y~., data=learn)
print(cf.mlog1 <- fit.mlog1@coefficients)

library(nnet)
fit.mlog2 <- multinom(y ~., data=learn)
print(t(coef(fit.mlog2)))

y.mlog <- predict(fit.mlog2, newdata=test)

save(fit.rf, fit.nnet, fit.knn, fit.NB, fit.mlog1,
     fit.mlog2, learn, test, file="classifiers.RData")




