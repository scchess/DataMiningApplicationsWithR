#!/usr/bin/Rscript
library(Matrix)
library(cluster)
source("./plot_functions.r")

# several variables
filename = "Cluster3_dnssec.txt"					# input filename
r = 0.9									# quantity of total magnitude hold (related to tolerance for PCA)
nb_cluster = 5								# cluster for kmean
output_file = paste( "k_clusters.txt")					# file where to print

tmp_file="/tmp/r2"

clab <- c("euQR", "reQR", "tR", "euBR", "reBR", "tBR", "cQRT", "reOCC", "euOCC", "tOCC", "CHR", "eQR", "eCPU", "ePRT", "succRatio", "failRatio", "cltQNbr", "pltQNbr", "Qlen", "Rlen", "Sigcheck", "MIRT", "SDIRT", "MPRT", "SDPRT", "MTTL", "SDTTL")
clabf <- c("euQR", "reQR","CHR", "cQRT", "MIRT", "MPRT", "MTTL")


mat_ent <- read.table(filename, row.names=1, col.names=clab)
mat_ent <- subset(mat_ent, cQRT > 0)			# python script return -1 if no request is present and cQRT is used to plot several variables
mat_ent <- subset(mat_ent, MTTL > 0)
mat <- mat_ent

mat$logeuQR <- log(mat$euQR)
mat$logreQR <- log(mat$reQR)
mat$logSigcheck <- log(mat$Sigcheck)


pca <- prcomp(mat, scale=TRUE, center=TRUE)
mag <- sum(pca$sdev * pca$sdev)			# total magnitude
pca <- prcomp(mat, tol= (1-r)*mag/(pca$sdev[1] * pca$sdev[1]), scale=TRUE, center=TRUE)
pca2 <- prcomp(mat, tol= (1-r)*mag/(pca$sdev[1] * pca$sdev[1]), scale=TRUE, center=TRUE)

write.table(pca2$x, file=tmp_file)
d<-read.table(tmp_file, header=TRUE, row.names=1)
k<-kmeans(d,nb_cluster)				# kmean


png(file="scatter_pca2.png")
#plot(d, col=k$cluster, pch=4)
#plot(d, col=141 + 12 * k$cluster, pch=4)
dev.off()

png(file="scatter2.png")
#plot(subset(mat, select=clabf), col=k$clusteir, pch=4)
dev.off()

png(file="pca2.png")
plot(pca)
dev.off()


write.table(pca$x, file=tmp_file)
d<-read.table(tmp_file, header=TRUE, row.names=1)
k<-kmeans(d,nb_cluster)				# kmean


png(file="scatter_pca1.png")
#plot(d, col=k$cluster, pch=4)
dev.off()

png(file="scatter1.png")
#plot(subset(mat, select=clabf), col=k$clusteri, pch=4)
dev.off()

png(file="pca1.png")
plot(pca)
dev.off()

