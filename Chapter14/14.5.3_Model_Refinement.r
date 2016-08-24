##########################
# 14.5.3 Model refinement
##########################
rm(list=ls())
load(file="classifiers.RData")
library(nnet)

wgt <- rep(1,length(learn$y))
wgt[learn$y=="W"] <- .5
fit.mlogit.bal <- multinom(y ~., weights=wgt, data=learn)
print(t(exp(coef(fit.mlogit.bal))))

y.mlogit.bal <- predict(fit.mlogit.bal,newdata=test)
CM.mlogit <- caret::confusionMatrix(y.mlogit.bal, test$y)
print(CM.mlogit)


##########################
# Model refinement
##########################
library(nnet)

no.resamp <- 1000
mtx.mlogit <- array(0,c(4,7,no.resamp))
mtx.mlogit.wgt <- mtx.mlogit
dtset <- rbind(learn,test)

set.seed(123456)
for (cnt in 1:no.resamp) {
   if (cnt %% round(no.resamp/10)==0) cat(cnt,"\n")
   idx <- sample(1:nrow(dtset),80)
   learn <- dtset[-idx,]
   test  <- dtset[idx,]

   ######################################
   # Multinomial logistic regression
   ######################################
   capture.output(fit.mlogit <- multinom(y ~., data=learn))
   y.mlogit <- predict(fit.mlogit,newdata=test[,1:6])
   CM.mlogit <- caret::confusionMatrix(y.mlogit, test$y)
   mtx.mlogit[,,cnt] <- rbind(CM.mlogit$overall, CM.mlogit$byClass)

   ############################################
   # Weighted Multinomial Logistic regression
   ############################################
   wgt <- rep(1,length(learn$y))
   wgt[learn$y=="W"] <- .5
   capture.output(fit.mlogit.wgt <- multinom(y ~., weights=wgt, data=learn))
   y.mlogit.wgt <- predict(fit.mlogit.wgt, newdata=test[,1:6])
   CM.mlogit.wgt <- caret::confusionMatrix(y.mlogit.wgt, test$y)
   mtx.mlogit.wgt[,,cnt] <- rbind(CM.mlogit.wgt$overall, CM.mlogit.wgt$byClass)

}
save(mtx.mlogit, mtx.mlogit.wgt, file="model_refinement.RData")


##########################
# Figure unpublished 1a
##########################
rm(list=ls())
load(file="model_refinement.RData")
library(ggplot2)
grp1 <- c(rep(1,dim(mtx.mlogit)[3]),rep(2,dim(mtx.mlogit.wgt)[3]))
grp1 <- c(grp1,grp1)
grp1 <- factor(grp1,labels=c("MLogit","Balanced.MLogit"))
accuracy <- c(mlogit=mtx.mlogit[1,1,],mlogit.wgt=mtx.mlogit.wgt[1,1,])
kappa <- c(mlogit=mtx.mlogit[1,2,],mlogit.wgt=mtx.mlogit.wgt[1,2,])
grp2 <- factor(c(t(matrix(rep(1:2,length(kappa)),2))),
        labels=c("Accuracy","Kappa"))

dtset1 <- data.frame(grp1,grp2,perf=c(accuracy,kappa))
p1 <- ggplot(aes(y=perf,x=grp1),data=dtset1) + 
      geom_boxplot(aes(fill=grp1)) + 
      coord_flip() + facet_wrap(~grp2, ncol=1, scales="fixed")+
      scale_fill_discrete(guide=F) +
      scale_x_discrete(name="")+scale_y_continuous(name="")

print(p1)


########################
# Figure unpublished 1b
########################
grp1 <- c(rep(1,dim(mtx.mlogit)[3]),rep(2,dim(mtx.mlogit.wgt)[3]))
grp1 <- c(grp1,grp1,grp1)
grp1 <- factor(grp1,labels=c("MLogit","Balanced.MLogit"))
sensW <- c(mlogit=mtx.mlogit[2,1,],mlogit.wgt=mtx.mlogit.wgt[2,1,])
sensL <- c(mlogit=mtx.mlogit[3,1,],mlogit.wgt=mtx.mlogit.wgt[3,1,])
sensD <- c(mlogit=mtx.mlogit[4,1,],mlogit.wgt=mtx.mlogit.wgt[4,1,])
grp2 <- factor(c(t(matrix(rep(1:3,length(sensW)),3))),
     labels=c("Sensitivity W","Sensitivity L","Sensitivity D"))
dtset2 <- data.frame(grp1,grp2,perf=c(sensW,sensL,sensD))
p2 <- ggplot(aes(y=perf,x=grp1),data=dtset2) + 
      geom_boxplot(aes(fill=grp1)) + 
      coord_flip() + facet_wrap(~grp2, ncol=1, scales="fixed")+
      scale_fill_discrete(guide=F) +
      scale_x_discrete(name="")+scale_y_continuous(name="")

print(p2)


##############
# Table 14.5
##############
library(raster)
dtset <- rbind(dtset1,dtset2)

mtx.mn <- round(matrix(by(dtset$perf,list(dtset$grp1,dtset$grp2),mean),2),2)
mtx.sd <- round(matrix(by(dtset$perf,list(dtset$grp1,dtset$grp2),sd),2),2)
mtx.cv <- round(matrix(by(dtset$perf,list(dtset$grp1,dtset$grp2),cv),2),1)
tab5 <- matrix(c(rbind(mtx.mn,mtx.sd,mtx.cv)),2)
rownames(tab5) <- c("MLogit","Balanced.MLogit")
colnames(tab5) <- c("Ave.Acc","SD.Acc","CV.Acc","Ave.Kap","SD.Kap","CV.Kap",
        "Ave.SensW","SD.SensW","CV.SensW","Ave.SensL","SD.SensL","CV.SensL",
        "Ave.SensD","SD.SensD","CV.SensD")
print(tab5)

