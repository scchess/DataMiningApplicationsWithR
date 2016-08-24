#!/usr/bin/Rscript
# R script to make kmean clusters

# importations
library(Matrix)
source("./plot_functions.r")

# several variables
filename = "cluster_entry.txt"	 					# input filename
filename = "Cluster3_dnssec.txt"
r = 0.9									# quantity of total magnitude hold (related to tolerance for PCA)
nb_cluster = 4								# cluster for kmean
output_file = paste(format(Sys.time(), "%F-%T"), "-Rout.txt", sep="")	# file where to print
par(cex=2)

tmp_file="/tmp/r"

clab <- c("euQR", "reQR", "tR", "euBR", "reBR", "tBR", "cQRT", "reOCC", "euOCC", "tOCC", "CHR", "eQR", "eCPU", "ePRT", "succRatio", "failRatio", "cltQNbr", "pltQNbr", "Qlen", "Rlen", "Sigcheck", "MIRT", "SDIRT", "MPRT", "SDPRT", "MTTL", "SDTTL")
clabf <- c("euBR", "reBR", "QNbr", "pltQNbr", "CHR", "cQRT", "MIRT", "MPRT", "MTTL")


mat_ent <- read.table(filename, row.names=1, col.names=clab)

mat_ent$pIRT = mat_ent$MIRT + mat_ent$SDIRT			# addition of column
mat_ent$mIRT = mat_ent$MIRT - mat_ent$SDIRT			# addition of column
mat_ent$pPRT = mat_ent$MPRT + mat_ent$SDPRT			# addition of column
mat_ent$mPRT = mat_ent$MPRT - mat_ent$SDPRT			# addition of column
mat_ent$pTTL = mat_ent$MTTL + mat_ent$SDTTL			# addition of column
mat_ent$mTTL = mat_ent$MTTL - mat_ent$SDTTL			# addition of column
#mat_ent <- subset(mat_ent, select= -SDIRT)		# select column
#mat_ent <- subset(mat_ent, select= -SDPRT)		# select column
#mat_ent <- subset(mat_ent, select= -SDTTL)		# select column

#sink(output_file)
#X11(width=12, height=12)
mat <- subset(mat_ent, cQRT > 0)			# python script return -1 if no request is present and cQRT is used to plot several variables
cor(mat_ent)

# sort
attach(mat_ent)
mat_ent_sorted <- mat_ent[order(tBR),]
detach(mat_ent)

mat <- subset(mat, tBR > mat_ent_sorted[dim(mat_ent)[1] - 1000,]$tBR) 	# subset the 1000 costliest fqdn
#mat <- subset(mat, tBR > mean(tBR) )			# selection of rows
mat <- subset(mat, MTTL > 0)
mat <- subset(mat, select=clabf)
mat$invTTL = 1/mat$MTTL
pca <- prcomp(mat, scale=TRUE, center=TRUE)
mag <- sum(pca$sdev * pca$sdev)			# total magnitude
pca <- prcomp(mat, tol= (1-r)*mag/(pca$sdev[1] * pca$sdev[1]), scale=TRUE, center=TRUE)
write.table(pca$x, file=tmp_file)
d<-read.table(tmp_file, header=TRUE, row.names=1)
write.table(pca$rotation, file=tmp_file)
rot<-read.table(tmp_file, header=TRUE, row.names=1)
k<-kmeans(d,nb_cluster)				# kmean
print(pca$rotation)				# new vectors
print(k$size)					# size of clusters

write.table(k$cluster, file="/tmp/clusters.txt")
g <- read.table ("/tmp/clusters.txt", header=TRUE, row.names=1)

# distance between centers and 0, and elimination of cluster near 0
nmin=Inf 	# norm min
nmax=0		# norm max
for (i in c(1:nb_cluster)){
	tmp=norm(rbind(k$centers[i,]), type="f")
	if(nmin > tmp) { 
		nmin = tmp 
		cmin = i }
	if(nmax < tmp) {
		nmax = tmp
		cmax = i }
}
mat0 <- subset(mat_ent, match(row.names(mat_ent), row.names(subset(g, x!=cmin))) > 0 )	

#plot(d, col=k$cluster)
#X11()
#plot(mat, col=k$cluster)

# print clusters for extra manipulations
for (i in c(1:nb_cluster)) {
	g0 <- subset(mat_ent, match(row.names(mat_ent), row.names(subset(g, x==i))) > 0 )	# all dataset parameters for cluster i
	write.table(g0, file=paste("/tmp/cluster_", i, ".txt", sep=""))
}

