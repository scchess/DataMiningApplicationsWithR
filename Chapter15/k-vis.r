#!/usr/bin/Rscript
# R script to make kmean clusters and visualize them with ponderation of reQR

# importations
library(Matrix)
source("./plot_functions.r")
par(cex=2)

# several variables
#filename = "Cluster3_dnssec.txt"					# input filename
filename = "cluster_entry_5min.txt"
r = 0.9									# quantity of total magnitude hold (related to tolerance for PCA)
nb_cluster = 4								# cluster for kmean
output_file = paste( "k_clusters.txt")					# file where to print

tmp_file="/tmp/r"

clab <- c("euQR", "reQR", "tR", "euBR", "reBR", "tBR", "cQRT", "reOCC", "euOCC", "tOCC", "CHR", "eQR", "eCPU", "ePRT", "succRatio", "failRatio", "cltQNbr", "pltQNbr", "Qlen", "Rlen", "Sigcheck", "MIRT", "SDIRT", "MPRT", "SDPRT", "MTTL", "SDTTL")
clabf <- c("euQR", "reQR","CHR", "cQRT", "MIRT", "MPRT", "MTTL")
clabf <- c("euQR", "reQR","CHR", "cQRT", "MIRT", "MPRT", "iTTL")


mat_ent <- read.table(filename, row.names=1, col.names=clab)
mat_ent <- subset(mat_ent, cQRT > 0)			# python script return -1 if no request is present and cQRT is used to plot several variables
mat_ent <- subset(mat_ent, MTTL > 0)
mat_ent$iTTL <- 1/mat_ent$MTTL

sink (output_file)



for (k in c(1:5)) {
	mat <- mat_ent
	write.table(scale (mat, scale=TRUE, center=TRUE), file=tmp_file)
	mat <- read.table(tmp_file, header=TRUE, row.names=1)
	mat$reQR <- k * mat$reQR			# ponderation of reQR
	mat0 <- subset(mat, select=clabf)		# small matrix
	mat1 <- subset(mat, select=c("reQR", "euQR"))	# tiny matrix

	km0 <- kmeans(mat0, nb_cluster)			# kmean performed on small matrix
	km1 <- kmeans(mat1, nb_cluster)			# kmean performed on tiny matrix

	print(k)
	print(km0$size)
	print(km1$size)
	png(filename=paste("k", k, "m0km0.png", sep=""), width = 2000, height =2000 , units = "px")
	par(cex=2);plot(subset(mat_ent, select=clabf), col=km0$cluster * 5, pch=4)
	dev.off()
	png(filename=paste("k", k, "m1km1.png", sep=""), width = 2000, height =2000 , units = "px")
	par(cex=2);plot(subset(mat_ent, select=c("reQR", "euQR")), col=km1$cluster * 5, pch=4)
	dev.off()
	png(filename=paste("k", k, "m0km1.png", sep=""), width = 2000, height =2000 , units = "px")
	par(cex=2);plot(subset(mat_ent, select=clabf), col=km1$cluster * 5, pch=4)
	dev.off()
	png(filename=paste("k", k, "m1km0.png", sep=""), width = 2000, height =2000 , units = "px")
	par(cex=2);plot(subset(mat_ent, select=c("reQR", "euQR")), col=km0$cluster * 5, pch=4)
	dev.off()
}

