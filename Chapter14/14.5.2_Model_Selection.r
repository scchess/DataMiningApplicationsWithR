#########################
# 14.5.2 Model selection
#########################
rm(list=ls())
load(file="classifiers.RData")

library(nnet)
y.mlog <- predict(fit.mlog2, newdata=test)

CM.mlogit <- caret::confusionMatrix(y.mlog, test$y)
print(CM.mlogit)


##############################
# Assessing model performance
##############################
library(randomForest)
library(caret)
library(nnet)
library(klaR)
library(doParallel)
library(RSNNS)

no.resamp <- 2
mtx.mlogit <- array(0,c(4,7,no.resamp))
mtx.nb <- mtx.mlogit
mtx.knn <- mtx.mlogit
mtx.rf <- mtx.mlogit
mtx.nnet <- mtx.mlogit
dtset <- rbind(learn,test)

set.seed(12345)
for (cnt in 1:no.resamp) {
   cat(cnt,"\n")
   idx <- sample(1:nrow(dtset),80)
   learn <- dtset[-idx,]
   test  <- dtset[idx,]

   ######################################
   # Random Forest
   ######################################
   cat(cnt,".1 Random Forest \n",sep="")
   cl <- makeCluster(detectCores(), type='PSOCK')
   registerDoParallel(cl)
   opt <- trainControl(method='repeatedcv', number=10, 
         repeats=15, classProbs=TRUE)
   fit.rf <- train(y~., data=learn, method='rf',
            tuneGrid=expand.grid(.mtry=1:6),ntree=1000,
            metric='Accuracy', trControl=opt)
   stopCluster(cl)
   y.rf <- predict(fit.rf$finalModel,newdata=test[,1:6],type="class")
   CM.rf <- caret::confusionMatrix(y.rf, test$y)
   mtx.rf[,,cnt] <- rbind(CM.rf$overall,CM.rf$byClass)

   ######################################
   # Neural Network
   ######################################
   cat(cnt,".2 Neural network \n",sep="")
   cl <- makeCluster(detectCores(), type='PSOCK')
   registerDoParallel(cl)
   opt <- trainControl(method='repeatedcv', number=10, 
         repeats=15, classProbs=TRUE)
   fit.nnet <- suppressWarnings(train(y~., data=learn, method="mlp",
              tuneGrid=expand.grid(.size=1:15),
              metric="Accuracy", learnFunc="SCG", trControl=opt))
   stopCluster(cl)
   probs <- predict(fit.nnet$finalModel,newdata=test[,1:6])
   y.nnet <- apply(probs,1,which.max)
   y.nnet <- factor(y.nnet,levels=1:3,labels=levels(dtset$y))
   CM.nnet <- caret::confusionMatrix(y.nnet, test$y)
   mtx.nnet[,,cnt] <- rbind(CM.nnet$overall,CM.nnet$byClass)

   ######################################
   # KNN
   ######################################
   cat(cnt,".3 KNN \n",sep="")
   cl <- makeCluster(detectCores(), type='PSOCK')
   registerDoParallel(cl)
   opt <- trainControl(method='repeatedcv', number=10, 
          repeats=15, classProbs=TRUE)
   fit.knn <- train(y~., data=learn, method='knn',
         tuneGrid=expand.grid(.k=5:25),
         metric='Accuracy', trControl=opt)
   stopCluster(cl)
   yhat.knn <- predict(fit.knn$finalModel,newdata=test[,1:6],type="class")
   CM.knn <- caret::confusionMatrix(yhat.knn, test$y)
   mtx.knn[,,cnt] <- rbind(CM.knn$overall,CM.knn$byClass)

   ######################################
   # Naive Bayes
   ######################################
   cat(cnt,".4 Naive Bayes \n",sep="")
   fit.nb <- NaiveBayes(y ~., data=learn)
   y.nb <- suppressWarnings(predict(fit.nb,newdata=test[,1:6])$class)
   CM.nb <- caret::confusionMatrix(y.nb, test$y)
   mtx.nb[,,cnt] <- rbind(CM.nb$overall,CM.nb$byClass)

   ######################################
   # Multinomial logistic regression
   ######################################
   cat(cnt,".5 Multinomial logistic regression \n",sep="")
   fit.mlogit <- multinom(y ~., data=learn)
   y.mlogit <- predict(fit.mlogit,newdata=test[,1:6])
   CM.mlogit <- caret::confusionMatrix(y.mlogit, test$y)
   mtx.mlogit[,,cnt] <- rbind(CM.mlogit$overall,CM.mlogit$byClass)

}
save(mtx.nb, mtx.mlogit, mtx.knn, mtx.rf, mtx.nnet,
     file="model_performance.RData")


##################
# Figure 14.7 a)
##################
rm(list=ls())
load(file="model_performance.RData")
library(ggplot2)
grp1 <- c(rep(1,dim(mtx.rf)[3]),rep(2,dim(mtx.nnet)[3]),
         rep(3,dim(mtx.knn)[3]),rep(4,dim(mtx.nb)[3]),
         rep(5,dim(mtx.mlogit)[3]))
grp1 <- c(grp1,grp1)
grp1 <- factor(grp1,labels=c("RF","NNET","KNN","NBayes","MLogit"))
accuracy <- c(nb=mtx.rf[1,1,],mlogit=mtx.nnet[1,1,],
  knn=mtx.knn[1,1,],rf=mtx.nb[1,1,],nnet=mtx.mlogit[1,1,])
kappa <- c(nb=mtx.rf[1,2,],mlogit=mtx.nnet[1,2,],
  knn=mtx.knn[1,2,],rf=mtx.nb[1,2,],nnet=mtx.mlogit[1,2,])
grp2 <- factor(c(t(matrix(rep(1:2,length(kappa)),2))),
        labels=c("Accuracy","Kappa"))

dtset1 <- data.frame(grp1,grp2,perf=c(accuracy,kappa))
p1 <- ggplot(aes(y=perf,x=grp1),data=dtset1) + 
      geom_boxplot(aes(fill=grp1)) + 
      coord_flip() + facet_wrap(~grp2, ncol=1, scales="fixed")+
      scale_fill_discrete(guide=F) +
      scale_x_discrete(name="")+scale_y_continuous(name="")+
      theme(text=element_text(size = 24))

postscript(file="fig7A.eps", height=8, width=8, horizontal= F, 
    paper="special", colormodel="rgb")
print(p1)
dev.off()

##################
# Figure 14.7 b)
##################
grp1 <- c(rep(1,dim(mtx.rf)[3]),rep(2,dim(mtx.nnet)[3]),
         rep(3,dim(mtx.knn)[3]),rep(4,dim(mtx.nb)[3]),
         rep(5,dim(mtx.mlogit)[3]))
grp1 <- c(grp1,grp1,grp1)
grp1 <- factor(grp1,labels=c("RF","NNET","KNN","NBayes","MLogit"))
sensW <- c(nb=mtx.rf[2,1,],mlogit=mtx.nnet[2,1,],
 knn=mtx.knn[2,1,],rf=mtx.nb[2,1,],nnet=mtx.mlogit[2,1,])
sensL <- c(nb=mtx.rf[3,1,],mlogit=mtx.nnet[3,1,],
 knn=mtx.knn[3,1,],rf=mtx.nb[3,1,],nnet=mtx.mlogit[3,1,])
sensD <- c(nb=mtx.rf[4,1,],mlogit=mtx.nnet[4,1,],
 knn=mtx.knn[4,1,],rf=mtx.nb[4,1,],nnet=mtx.mlogit[4,1,])
grp2 <- factor(c(t(matrix(rep(1:3,length(sensW)),3))),
     labels=c("Sensitivity W","Sensitivity L","Sensitivity D"))
dtset2 <- data.frame(grp1,grp2,perf=c(sensW,sensL,sensD))
p2 <- ggplot(aes(y=perf,x=grp1),data=dtset2) + 
      geom_boxplot(aes(fill=grp1)) + 
      coord_flip() + facet_wrap(~grp2, ncol=1, scales="fixed")+
      scale_fill_discrete(guide=F) +
      scale_x_discrete(name="")+scale_y_continuous(name="")+
      theme(text=element_text(size = 24))

postscript(file="fig7B.eps", height=8, width=8, horizontal= F, 
    paper="special", colormodel="rgb")
print(p2)
dev.off()


##################
# Table 14.4
##################
library(raster)
dtset <- rbind(dtset1,dtset2)

mtx.mn <- round(matrix(by(dtset$perf,list(dtset$grp1,dtset$grp2),mean),5),2)
mtx.sd <- round(matrix(by(dtset$perf,list(dtset$grp1,dtset$grp2),sd),5),2)
mtx.cv <- round(matrix(by(dtset$perf,list(dtset$grp1,dtset$grp2),cv),5),1)
tab4 <- matrix(c(rbind(mtx.mn,mtx.sd,mtx.cv)),5)
rownames(tab4) <- c("RF","NNET","KNN","NBayes","MLogit")
colnames(tab4) <- c("Ave.Acc","SD.Acc","CV.Acc","Ave.Kap","SD.Kap","CV.Kap",
        "Ave.SensW","SD.SensW","CV.SensW","Ave.SensL","SD.SensL","CV.SensL",
        "Ave.SensD","SD.SensD","CV.SensD")
print(tab4)



