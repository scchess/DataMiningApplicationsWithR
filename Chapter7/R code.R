###################################################
###Code chunk number 1: ch-7:Pg-2
##################################################
describe(dataset1[145]==1)
describe(dataset1[145]==0)

###################################################
###Code chunk number 2: ch-7:Pg-3
###################################################
 dataset1<- read.csv(file = "C:/Documents and Settings/fool/Desktop/ticdata2000lrn.csv")

###################################################
###Code chunk number 3: ch-7:Pg-3
###################################################
summary(dataset1)

###################################################
###Code chunk number 4: ch-7:Pg-3
###################################################
str(dataset1)

###################################################
###Code chunk number 5: ch-7:Pg-3
###################################################
cor(dataset1[1:85],dataset1[86], method="spearman")

###################################################
###Code chunk number 6: ch-7:Pg-4
###################################################
library(Hmisc)
describe(dataset1)

###################################################
###Code chunk number 7: ch-7:Pg-4
###################################################
prop.table(table(MOSTYPE, CARAVAN), 1)

###################################################
###Code chunk number 8: ch-7:Pg-5
###################################################
summary(cust.logit)
###################################################
###Code chunk number 9: ch-7:Pg-9
###################################################
library(rpart)
cust.rp<- rpart(CARAVAN~. , data=dataset1)
cust.rp

###################################################
###Code chunk number 10: ch-7:Pg-9
###################################################
printcp(cust.rp$cptable)

###################################################
###Code chunk number 11: ch-7:Pg-10
###################################################
cust4.var.imp <- varImp(cust4.rp, useModel=rpart)

###################################################
###Code chunk number 12: ch-7:Pg-11
###################################################
library(ipred)
cust.ip<- bagging(CARAVAN~. , data=dataset1, coob=TRUE)
cust.ip.prob<- predict(cust.ip, type="prob", newdata=dataset2)

###################################################
###Code chunk number 13: ch-7:Pg-11
###################################################
cust4.var.imp <- varImp(cust4.ip, useModel=bagging)




##########################################################
###Code chunk number 14: ch-7:Pg-13
###################################################
library(e1071)
cust.svm<-svm(CARAVAN~.,data=dataset1,method="C-classification", kernel="radial",cost=10,gamma=0.1,cross=0,fitted=TRUE, probability=TRUE)

###################################################
###Code chunk number 15: ch-7:Pg-13
###################################################
cust.svm.feature.weights = t(cust.svm$coefs) %*%cust.svm$SV [this calculates the feature weights]
cust.svm.feature.weights

###################################################
###Code chunk number 16: ch-7:Pg-16
###################################################
cust.logit<- glm(CARAVAN~. , data=dataset1, family=binomial(link="logit"))
summary(cust.logit)

###################################################
###Code chunk number 17: ch-7:Pg-16
###################################################
cust.var.imp<- varImp(cust.logit, useModel=glm)

###################################################
###Code chunk number 18: ch-7:Pg-18
###################################################
ppi<- 300
png(filename="ROC curve without religion variables.png", width=6*ppi, height=6*ppi,res=ppi)
plot(cust.rp.perf, col=2, main="ROC curve without religion variables")
legend(0.5,0.5, c('rpart','bagging','svm', 'logistic'), 2:5)
plot(cust.ip.perf, col=3, add=TRUE)
plot(cust.svm.perf, col=4, add=TRUE)
plot(cust.logit.perf, col=5, add=TRUE)
dev.off()
###################################################
###Code chunk number 19: ch-7:Pg-18
###################################################
cust.rp.perf.auc<-performance(cust.rp.prob,.rocr, 'auc')
cust.ip.perf.auc<- performance(cust.ip.prob.rocr, 'auc')
cust.svm.perf.auc<- performance(cust.svm.prob.rocr, 'auc')
cust.logit.perf.auc<- performance(cust.logit.prob.rocr, 'auc')

###################################################
###Code chunk number 20: ch-7:Pg-20
###################################################
cust.rp.perf.cr <- performance(cust5.rp.prob.rocr, "rec", "rpp")
cust.ip.perf.cr <- performance(cust5.rp.prob.rocr, "rec", "rpp")
cust.svm.perf.cr <- performance(cust5.rp.prob.rocr, "rec", "rpp")
cust.logit.perf.cr <- performance(cust5.rp.prob.rocr, "rec", "rpp")
ppi<- 300
png(filename=" Cumulative recall curve(without religion variables)",width=6*ppi, height=6*ppi,res=ppi)
plot(cust.rp.perf.cr, col=2, main="Cumulative recall curve(without religion variables)")
legend(0.5,0.5,c('rpart','bagging','svm','logistic'),2:5)
plot(cust.ip.perf.cr,col=3,add=TRUE)
plot(cust.svm.perf.cr,col=4,add=TRUE)
plot(cust.logit.perf.cr,col=5,add=TRUE)
dev.off()

###################################################
###Code chunk number 21: ch-7:Pg-21
###################################################
cust.rp.perf.acc<- performance(cust5.rp.prob.rocr,"acc")
cust.ip.perf.acc<- performance(cust5.ip.prob.rocr,"acc")
cust.svm.perf.acc<- performance(cust5.svm.prob.rocr,"acc")
cust.logit.perf.acc<- performance(cust5.logit.prob.rocr,"acc")
ppi<-300
png(filename="Accuracy versus Cut-off curve without religion variables.png", width=6*ppi, height=6*ppi. res=ppi)
plot(cust.rp.perf.acc, col=2, main="Accuracy versus Cut-off curve without religion variables")
legend(0.5,0.5, c('rpart','bagging','svm', 'logistic'), 2:5)
plot(cust.ip.perf.acc, col=3, add=TRUE)
plot(cust.svm.perf.acc, col=4, add=TRUE)
plot(cust.logit.perf.acc, col=5, add=TRUE)
dev.off()

###################################################
###Code chunk number 22: ch-7:Pg-23
###################################################
time.rp<-system.time({ cust.rp<- rpart(CARAVAN~. , data=dataset1)
cust.rp.pred<- predict(cust.rp, type="matrix", newdata=dataset2)
 cust.rp.prob.rocr<- prediction(cust.rp.pred, dataset2$CARAVAN)
})

###################################################
###Code chunk number 23: ch-7:Pg-40
###################################################
library(ROCR)
library(rpart)
cust.rp.pred<- predict(cust.rp, type="matrix", newdata=dataset2)
cust.rp.prob.rocr<- prediction(cust.rp.pred, dataset2$CARAVAN)
cust.rp.perf<- performance(cust.rp.prob.rocr, "tpr", "fpr")
plot(cust.rp.perf, main="ROC curve using recursive partioning", colorize=T)


###################################################
###Code chunk number 24: ch-7:Pg-40
###################################################
library(ipred)
cust.ip.prob<- predict(cust.ip, type="prob", newdata=dataset2)
cust.ip.prob.rocr<- prediction(cust.ip.prob, dataset2$CARAVAN)
cust.ip.perf<- performance(cust.ip.prob.rocr,"tpr", "fpr")
plot(cust.ip.perf, main="ROC curve using bagging ensemble", colorize=T)


###################################################
###Code chunk number 25: ch-7:Pg-40
###################################################
library(e1071)
cust.svm.prob<- predict(cust.svm, type="prob" , newdata=dataset2,probability=TRUE)
cust.svm.prob.rocr<- prediction(cust.svm.prob , dataset2$CARAVAN)
cust.svm.perf<- performance(cust.svm.prob.rocr,"tpr", "fpr")
plot(cust.svm.perf , main="ROC curve using SVM", colorize=T)




###################################################
###Code chunk number 26: ch-7:Pg-40
###################################################
cust.logit.pred<- predict(cust.logit, newdata=dataset2, type="response")
cust.logit.prob.rocr<- prediction(cust.logit.pred, dataset2$CARAVAN)
cust.logit.perf<- performance(cust.logit.prob.rocr, "tpr","fpr")
plot(cust.logit.perf , main="ROC using Logistic regression", colorize=T)

###################################################
###Code chunk number 27: ch-7:Pg-40
###################################################
library(rpart)
control<- rpart.control(xval=5) #[xval defines number of cross validations]
cust.rp<- rpart(CARAVAN~. , data=dataset1, control)


###################################################
###Code chunk number 28: ch-7:Pg-40
###################################################
library(adabag)
cust.baggingcv<- bagging.cv(CARAVAN ~ ., v=10, data=dataset12, mfinal=10)  #[v defines number of cross validations]

###################################################
###Code chunk number 29: ch-7:Pg-40
###################################################
library(e1071)
cust.svm<- svm(CARAVAN ~., data=dataset1,method="C-classification", kernel="radial", cost=10,gamma=0.1,cross=10,fitted=TRUE, probability=TRUE)  #[cross defines number of cross validations ]


###################################################
###Code chunk number 30: ch-7:Pg-40
###################################################
library(boot)
cv.logit<-cv.glm(dataset1, cust4.logit, K=5) # [K defines number of cross validations]
cv.logit



