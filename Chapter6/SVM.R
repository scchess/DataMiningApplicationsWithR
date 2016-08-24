rm(list=ls())
library(RODBC)
library(RSQLite)
library(randomForest)
library(varSelRF)
library(e1071)
library(ROCR)

m <- dbDriver("SQLite")
con <- dbConnect(m, dbname = "parsian3.dbms")

customerdata=dbGetQuery(con,"select ID,target,avgtranlong,avgtranlonglast,avgtranshortlast,    
                                      inter14_4,inter17_14,inter19_14,inter28_14,inter55_27,inter55_6,            
                                      inter87_55,longlastyear,notranshort,          
                                      notranshortlast,notrantotal,notrantotallast,      
                                      recencylastaccount,recencylasttran,recencymaxtrandate,   
                                      totaldepositshort,totaldepositshortlast,totaltran,            
                                      totaltranlast,totaltranlong,totaltranlonglast,    
                                      totaltranshort,totaltranshortlast   
                                      from customerselectedfscale" )


non2 <- customerdata[customerdata$target==0,]
res2 <- customerdata[customerdata$target==1,]
nrow(non2)
nrow(res2)

#split data into a training and test set
index=1:nrow(customerdata)
testindex=sample(index,trunc(length(index)/3))
testset=customerdata[testindex,]
trainset=customerdata[-testindex,]

non1 <- testset[testset$target==0,]
res1 <- testset[testset$target==1,]
nrow(non1)
nrow(res1)

#Balancing 
non <- trainset[trainset$target==0,]
res <- trainset[trainset$target==1,]
nrow(non)
nrow(res)
indexnon <-sample(1:nrow(non),2*nrow(res),replace=FALSE)
sampletrain <- rbind(res,non[indexnon,])

sampletrain=(subset(sampletrain, select=-ID))
sampletrain$target <- factor(sampletrain$target)

wts <- 100 / table(sampletrain$target)

mytune <- tune(svm, target ~ ., data=sampletrain ,class.weights = wts,
               probability =TRUE,ranges=list(gamma = 2^(-8:-5),cost = 10^(-2:4)),scale=FALSE, 
               tunecontrol = tune.control(best.model = TRUE,performances=TRUE,
               sampling="cross",cross=5))


model <- mytune$best.model
summary(model)
mytune$best.parameters
mytune$best.performance
mytune$ performances
testset=(subset(testset, select=-ID))
testset$target <- factor(testset$target)
predTarget1 <- predict(model, subset(testset, select=-target),decision.values=TRUE,probability = TRUE)
a1=attr(predTarget1, "probabilities")
table(pred=predTarget1,testset$target)
sum(predTarget1==testset$target)/length(testset$target)
summary(model)


plot(mytune,transform.x = log2,transform.y = log10)
plot(mytune)
plot(mytune,transform.x = log2,transform.y = log10, 
    color.palette = hsv_palette(h = 2/3, from = 0.01, to = 0.9))

plot(mytune,transform.x = log2,transform.y = log10, 
    color.palette = terrain.colors)


plot(mytune,transform.x = log2,transform.y = log10, 
    color.palette = colorRampPalette(c("white", "green", "orange", "yellow","red")))


plot(mytune,transform.x = log2,transform.y = log10, 
    color.palette = colorRampPalette(c("white", "orange","red","yellow","green")))

plot(mytune,transform.x = log2,transform.y = log10, 
    color.palette = colorRampPalette(c("white", "green","violet","pink","blue")))

plot(mytune,transform.x = log2,transform.y = log10, 
    color.palette = colorRampPalette(c("white", "green", "orange", "yellow","blue")))





pred <- prediction( a1[,1], testset$target)
perf <- performance(pred,"tpr","rpp")
plot(perf)
