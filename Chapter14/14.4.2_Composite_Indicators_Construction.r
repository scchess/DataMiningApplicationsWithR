####################################################
####        14.4. DATA PREPROCESSING           #####
####################################################

############################################
## 14.4.2 COMPOSITE INDICATORS CONSTRUCTION
############################################

rm(list=ls())

load(file="selected_covariates.RData")

##################################
## 14.4.2.1 PCA for the home team
##################################

X <- Xs[,grep("_C$", names(Xs))]	

PCA <- function(X,b) {
  pca <- princomp(X, cor=T, scores=T)
  loadings <- pca$loadings
  obj.scores <- pca$scores[,1:b]
  Rot <- varimax(loadings[,1:b])
  list(eigvals=(pca$sdev)^2, loadings=loadings, obj.scores=obj.scores,
  loadings.rot=Rot$loadings, obj.scores.rot=scale(obj.scores)%*%Rot$rotmat)
}

dims <- 3
pca <- PCA(X,dims)
objs.rot.home <- pca$obj.scores.rot

#############
# Fig. 14.5
#############
postscript(file="fig5.eps", height=8, width=16, horizontal= F, 
    paper="special", colormodel="rgb")
par(mfrow=c(1,2),cex=1.7)
p <- length(pca$eigvals)
plot(pca$eigvals, xaxp=c(1,p,p-1), type='b', main='Scree Plot',
    xlab='Principal Components', ylab='Eigenvalues')
lines(c(0,p+1),c(1,1),lty='dashed')
text(pca$eigvals, as.character(round(pca$eigvals,digits=2)),
    cex=0.6, pos=c(4,4,4,4,3,3))
plot(100*cumsum(pca$eigvals)/p, xaxp=c(1,p,p-1), type='b',
    xlab='Principal Components', ylab='CVAF (%)', 
    main='Cumulative Variance Accounted For ')
text(100*cumsum(pca$eigvals)/p, 
    as.character(round((cumsum(pca$eigvals)/p)*100,digits=1)),
    cex=0.6, pos=c(4,4,4,2,1,1))
dev.off()

###############################################
# Change signs of first and third rotated components
###############################################
objs.rot.home[,1] <- -objs.rot.home[,1]
pca$loadings.rot[,1] <- -pca$loadings.rot[,1]

#############
# Table 14.2
#############
print(pca$loadings.rot, cutoff=0)


###############################
## 14.4.2.2 PCA for the away team
###############################
X <- Xs[,grep("_O$", names(Xs))]
dims <- 3
pca <- PCA(X,dims)
objs.rot.away <- pca$obj.scores.rot

#############
# Fig. 14.6
#############
postscript(file="fig6.eps", height=8, width=16, horizontal= F, 
    paper="special", colormodel="rgb")
par(mfrow=c(1,2))
p <- length(pca$eigvals)
par(mfrow=c(1,2),cex=1.7)
plot(pca$eigvals, xaxp=c(1,p,p-1), type='b',
	xlab='Principal Components', ylab='Eigenvalues', main='Scree Plot')
	lines(c(0,p+1),c(1,1),lty='dashed')
	text(pca$eigvals,as.character(round(pca$eigvals,digits=2)),
      cex=0.6,pos=c(4,4,4,4,3,3,3))
plot((cumsum(pca$eigvals)/p)*100, xaxp=c(1,p,p-1), type='b',
	xlab='Principal Components', ylab='CVAF (%)', 
      main='Cumulative Variance Accounted For')
	text((cumsum(pca$eigvals)/p)*100,
      as.character(round((cumsum(pca$eigvals)/p)*100,digits=1)),
	cex=0.6,pos=c(4,4,4,2,2,1,1))
dev.off()

###############################################
# Change signs of first and third components
###############################################
objs.rot.away[,c(1,3)] <- -objs.rot.away[,c(1,3)]
pca$loadings.rot[,c(1,3)] <- -pca$loadings.rot[,c(1,3)]

#############
# Table 14.3
#############
print(pca$loadings.rot, cutoff=0)


Xc <- data.frame(objs.rot.home,objs.rot.away)
names(Xc) <- c("air.attack.home", "shot.attack.home", "defense.home",
             "defense.away", "shot.attack.away", "counterattack.away")

round(cor(Xc),3)

save(Xc,y,file="object_scores.RData")



