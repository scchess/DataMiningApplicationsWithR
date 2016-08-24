#######################################################
#####   14.3. DATA EXTRACTION AND EXPLORATION    ######
#######################################################

rm(list=ls())


###########################
# 14.3.1 DATA EXTRACTION 
###########################
load(file="football_data.Rdata")
ls()
str(dtset)
y  <- factor(dtset[,1], labels=c("W","L","D"))
Xf <- dtset[,2:ncol(dtset)]

###########################
## 14.3.2 DATA EXPLORATION 
###########################
AbsFreq <- table(y)
PerFreq <- round(prop.table(AbsFreq)*100,1)
cbind(AbsFreq,PerFreq) 

#############
# Fig. 14.2A
#############
library(ggplot2)
Freq <- data.frame(PerFreq)
postscript(file="fig2A.eps", height=8, width=4, horizontal= F, 
     paper="special", colormodel="rgb")
ggplot(Freq,aes(x="", fill=y, weight=Freq))+ geom_bar(width = 1)+ 
    scale_fill_manual(values=c("green","yellow","red"))+
    scale_y_continuous("Percentage frequency")+scale_x_discrete(name="")+
    theme(text=element_text(size = 24))
dev.off()
library(Hmisc)
describe(Xf)

x.name <- "O_OCCAS_C"
x <- Xf[,names(Xf) %in% x.name]
pf <- prop.table(table(x,y),1)[,c(1,3,2)]
dtst <- data.frame(PctFreq=c(t(pf)),
      x=rep(as.numeric(rownames(pf)),each=ncol(pf)),
      Outcome=ordered(rep(1:3,nrow(pf)), labels=colnames(pf))) 

#############
# Fig. 14.2B
#############
postscript(file="fig2B.eps", height=8, width=16, horizontal= F, 
    paper="special", colormodel="rgb")
ggplot(dtst, aes(x=x, y=PctFreq, group=Outcome, fill=Outcome)) +
    geom_area(position="fill") +  scale_x_continuous(x.name) +
    scale_y_continuous("Percentage frequency") +
    scale_fill_manual(values = c("green","yellow","red"))+
    theme(text=element_text(size = 24))
#savePlot( file="fig2B.eps", type="eps")
dev.off()


