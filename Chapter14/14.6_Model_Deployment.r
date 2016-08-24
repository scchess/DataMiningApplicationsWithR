##########################
# 14.6. MODEL DEPLOYMENT
##########################
rm(list=ls())
library(nnet)
load(file="classifiers.RData")
cf.mlog1 <- fit.mlog1@coefficients
cf.mlog2 <- coef(fit.mlog2)

rrr1 <- cbind(exp(cf.mlog1[,2])/exp(cf.mlog1[,1]),
        exp(cf.mlog1[,3])/exp(cf.mlog1[,1]))
colnames(rrr1) <- c("L","D")
print(rrr1)
t(exp(cf.mlog2))


##################
# Table 14.6
##################
wgt <- rep(1,length(learn$y))
wgt[learn$y=="W"] <- .5
fit.mlogit.bal <- multinom(y ~., weights=wgt, data=learn)
RR.bal <- t(exp(coef(fit.mlogit.bal)))[-1,]
RR <- t(exp(cf.mlog2))[-1,]
round(cbind(RR, RR.bal),2)


##################
# Figure 14.8 a)
##################
x.name <- "shot.attack.home"
j <- which(names(learn) %in% x.name)
k <- 200
x <- seq(-3,3,length.out=k)
X0  <- matrix(0,k,6)
X0[,j] <- x
dimnames(X0)[[2]] <- names(learn)[1:6]
probs.mlog <- predict(fit.mlog2, newdata=X0, type="probs")

library(ggplot2)
dtst <- data.frame(Xj=rep(x,3),Prbs=c(probs.mlog[,c(1,3,2)]),
             Outcome=factor(rep(1:3,each=k),labels=c("W","D","L")))

postscript(file="fig8A.eps", height=8, width=8, horizontal= F, 
    paper="special", colormodel="rgb")
ggplot(dtst,aes(x=Xj,y=Prbs,group=Outcome,fill=Outcome)) + 
geom_area(position="fill") +
scale_x_continuous(name="Shot attack (home team)") +
scale_y_continuous(name="Average predicted probabilities") +
scale_fill_manual(values=c("W"="green","D"="yellow","L"="red"))+
theme(text=element_text(size = 24))
dev.off()


##################
# Figure 14.8 b)
##################
x.name <- "shot.attack.away"
j <- which(names(learn) %in% x.name)
X0  <- matrix(0,k,6)
X0[,j] <- x
dimnames(X0)[[2]] <- names(learn)[1:6]
probs.mlog <- predict(fit.mlog2, newdata=X0, type="probs")

dtst <- data.frame(Xj=rep(x,3),Prbs=c(probs.mlog[,c(1,3,2)]),
             Outcome=factor(rep(1:3,each=k),labels=c("W","D","L")))

postscript(file="fig8B.eps", height=8, width=8, horizontal= F, 
    paper="special", colormodel="rgb")
ggplot(dtst,aes(x=Xj,y=Prbs,group=Outcome,fill=Outcome)) + 
geom_area(position="fill") +
scale_x_continuous(name="Shot attack (away team)") +
scale_y_continuous(name="Average predicted probabilities") +
scale_fill_manual(values=c("W"="green","D"="yellow","L"="red"))+
theme(text=element_text(size = 24))
dev.off()


##################
# Figure 14.9
##################
dtset <- rbind(learn,test)
probs.mlog <- predict(fit.mlog2, newdata=dtset, type="probs")

assign.pr <- apply(cbind(probs.mlog,dtset$y),1,function(x){x[x[4]]})
idx <- order(assign.pr, decreasing=T)
assign.pr <- assign.pr[idx]
max.pr <- apply(probs.mlog,1,max)[idx]
diff.prob <- max.pr - assign.pr
thres <- mean(diff.prob[diff.prob>0])
cols <- cut(diff.prob, c(-1,0,thres,1), labels=c("1","2","3"))
dtst <- data.frame(assign.pr, cols, cnt=1:nrow(dtset))

postscript(file="fig9.eps", height=8, width=16, horizontal= F, 
    paper="special", colormodel="rgb")
ggplot(aes(x=cnt, y=assign.pr, fill=cols), data=dtst) + 
  geom_bar(aes(width=1), stat="identity", colour="black")+
  scale_fill_manual(values=c("1"="green", "2"="orange", "3"="red")) +
  scale_x_continuous(limits=c(0,nrow(dtset)),name="")+
  scale_y_continuous(name="Probability assigned by model to match outcome")+
  theme(legend.position="none",text=element_text(size = 24))
dev.off()



