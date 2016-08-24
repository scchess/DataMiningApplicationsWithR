> levels(as.factor(mydata[,"x2"]))
> levels(as.factor(mydata[,"x4"]))

# normalization function
norm01 <- function(data,x)
{
for(j in x)
{
data[!(is.na(data[,j])),j]=
(data[!(is.na(data[,j])),j]-min(data[!(is.na(data[,j])),j]))/
(max(data[!(is.na(data[,j])),j])-min(data[!(is.na(data[,j])),j]))
}
return(data)
}
attach(mydata)
# transforming into a (0,1) scale
c <- c(1,5,6,7,11,13,14)
data_norm <- norm01(mydata,c)
# drawing boxplot
boxplot(data_norm[,c])
c <- c(8,9,17,18)
# drawing the boxplot after transforming the ratio variable into a log scale
boxplot(log(mydata[,c]))


require(cluster)
attach(mydata)
# creating proximity matrix
dist=daisy(mydata[,-19],stand=TRUE,metric=c("gower"),
type = list(interval=c(1,5,6,7,11,13,14),ratio=c(7,8,17,18),
nominal=c(2,12),binary=c(3,4,10,15,16)))
# clustring objects by agglomerative hierarchical clustering
# this function obtains a score for each object
require(DMwR)
outl=outliers.ranking(dist,test.data=NULL,method="sizeDiff", meth = "ward")
# drawing boxplot of objects's score
boxplot(outl$prob.outliers[outl$rank.outliers])
qcust=quantile(outl$rank.outliers)
q1=qcust[1]
q3=qcust[4]
# eliminating objects with out of range score
filtquant=(outl$rank.outlier > q3*1.3)
data_noout=mydata[!filtquant,]
require(DMwR)
attach(mydata)
# missing data imputation by 5 nearest neighbours
co=knnImputation(data_norm, k = 5, scale = T, meth = "weighAvg",
distData = NULL)


# correlation matrix using ellipse-shaped glyphs
library(package="ellipse")
c= c(1,5,6,7,8,9,11,13,14,17,18)
plotcorr(cor(mydata[,c]),col=cl<-c("green","red","blue"))
# rggobi for nominal and binary data
c= c(1,seq(5,9,1),11,13,14,17,18)
# transforming into (0,1) scale
data_norm=norm01(mydata[,c],c)
c=c(2,12)
data_nomi[,c]=mydata[,c]/max(mydata[,c])
c=c(3,4,10,15,16,19)
newdata=cbind(data_norm,data_nomi,mydata[,c])
library(rggobi)
attach(newdata)
# creating a GGobi object
gd <- ggobi(newdata)[1]
library(DMwR)
attach(mydata)
# sampling data to make independent train and test data set
tr<-sample(nrow(m), round(nrow(mydata)*0.7))
train=mydata[tr,]
test=mydata[-tr,]
# balancing the train set
data_smot=train
data_smot$defn <- factor(ifelse(data_smot$defn == 1, "def", "nondef"))
data_samp <- SMOTE(defn ~ ., data_smot, k=5,perc.over = 500)
library(cluster)
# creating distance matrix
# target feature as a factor
dist=daisy(data_samp[,-19],stand=TRUE,metric=c("gower"),
type=list(interval=c(1,5,6,7,11,13,14),
ratio=c(7,8,17,18), nominal=c(2,12),
binary=c(3,4,10,15,16))),11,15,16)))
# two dimensional scaling of a distance matrix
loc=cmdscale(dist,k=2)
x=loc[,1]
y=loc[,2]
# plotting a set of points that the distances between them are approximately equal to the object's dissimilarities
plot(x,y,type="n")
text(x,y,labels=as.expression(as.numeric(data_samp[,19])),
col=as.numeric(data_samp[,19]))

# creating an object of class "randomForest"
attach(data_samp)
library(randomForest)
set.seed(4543)
data.frame(intbo_samp)
rf<-randomForest(x19~ ., data=data_samp,ntree=700, importance=TRUE, proximity=TRUE)
# displaying features importance type "1" for "mean decrease accuracy" measure
importance(rf, type=1, scale=TRUE)
varImpPlot(rf)
fo=rfcv(data_samp[,-19],data_samp[,19], cv.fold=10, scale="log", step=0.9)
best <- which.max(fo$error.cv)
plot( fo$n.var,rfo$error.cv, type= "h", main = "importance",
xlab="number of features", ylab = "classifier error rate")
axis(1, best, paste("best", best, sep="\n"), col = "red", col.axis = "red")
# creating an object of class rpart
library(rpart)
sdf=data.frame(train)
fit=rpart(train$x19 ~.,data=sdf,method="class")
printcp(fit)
plot(fit, uniform=TRUE,main="Classification Tree")
text(fit, use.n=TRUE, cex=1)
sdfto=data.frame(test)
a=predict(fit,sdfto)
a
table(predict(fit, test, type="class",na.action=na.pass), test[, "x19"])


