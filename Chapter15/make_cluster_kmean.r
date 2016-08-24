#!/usr/bin/Rscript
# R script to make kmean clusters

filename = "cluster_entry.txt"	 					# input filename
r = 0.9									# quantity of total magnitude hold (related to tolerance for PCA)
nb_cluster = 5								# cluster for kmean
output_file = paste(format(Sys.time(), "%F-%T"), "-Rout.txt", sep="")	# file where to print
par(cex=2)

tmp_file="/tmp/r"

clab <- c("euQR", "reQR", "tR", "euBR", "reBR", "tBR", "cQRT", "reOCC", "euOCC", "tOCC", "CHR", "eQR", "eCPU", "ePRT", "succRatio", "failRatio", "QNbr", "pltQNbr", "Qlen", "Rlen", "Sigcheck", "MIRT", "SDIRT", "MPRT", "SDPRT", "MTTL", "SDTTL")
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

mat <- read.table(filename, row.names=1, col.names=clab)
sink(output_file)
mat_ent <- subset(mat_ent, cQRT > 0)			# python script return -1 if no request is present and cQRT is used to plot several variables

# sort
attach(mat_ent)
mat_ent_sorted <- mat_ent[order(-euQR),]
detach(mat_ent)

#mat <- subset(mat, tBR > mat_ent_sorted[dim(mat_ent)[1] - 1000,]$tBR) 	# subset the 1000 costliest fqdn
#mat <- subset(mat_ent_sorted, tBR > mean(tBR))	# selection of rows
#mat <- subset(mat, select=clabf)
pca <- prcomp(mat, scale=TRUE, center=TRUE)
mag <- sum(pca$sdev * pca$sdev)			# total magnitude
pca <- prcomp(mat, tol= (1-r)*mag/(pca$sdev[1] * pca$sdev[1]), scale=TRUE, center=TRUE)
write.table(pca$x, file=tmp_file)
d<-read.table(tmp_file, header=TRUE, row.names=1)

k<-kmeans(d,nb_cluster)				# kmean
filename <- "/tmp/routing"			# output filename 
write.table(x=k$cluster, file=filename, sep="\t", quote=FALSE, col.names=FALSE)
sink()
sink()
sink()
sink()
sink()
