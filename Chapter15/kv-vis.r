#!/usr/bin/Rscript
# R script to make kmean clusters and visualize them with ponderation of reQR

# importations
library(Matrix)
library(cluster)
source("./plot_functions.r")
par(cex=2)


# several variables
filename = "Cluster3_dnssec.txt"					# input filename
#filename = "cluster_entry_5min.txt"
output_file = paste( "k_clusters_kv.txt")					# file where to print

tmp_file="/tmp/r5"
cost_unbound_r=0.096612818
cost_unbound_q=0.005427997
cost_bind_r=0.173573505
cost_bind_q=0.015105740

clab <- c("euQR", "reQR", "tR", "euBR", "reBR", "tBR", "cQRT", "reOCC", "euOCC", "tOCC", "CHR", "eQR", "eCPU", "ePRT", "succRatio", "failRatio", "cltQNbr", "pltQNbr", "Qlen", "Rlen", "Sigcheck", "MIRT", "SDIRT", "MPRT", "SDPRT", "MTTL", "SDTTL")


mat_ent <- read.table(filename, row.names=1, col.names=clab)
mat_ent <- subset(mat_ent, cQRT > 0)			# python script return -1 if no request is present and cQRT is used to plot several variables
mat_ent <- subset(mat_ent, MTTL > 0)
mat_ent$iTTL <- 1/mat_ent$MTTL
mat_ent$cost_unbound = cost_unbound_q * mat_ent$euQR + cost_unbound_q * mat_ent$reQR
mat_ent$cost_bind = cost_bind_q * mat_ent$euQR + cost_bind_q * mat_ent$reQR
attach(mat_ent)
mat_sorted <- mat_ent[order(-cost_bind),]
detach(mat_ent)
mat_ent <- mat_sorted

mat <- mat_ent
mat <- subset(mat, cltQNbr > 20 )                       #82% of traffic left 
write.table(scale (mat, scale=TRUE, center=TRUE), file=tmp_file)
mat <- read.table(tmp_file, header=TRUE, row.names=1)

sink (output_file, split=TRUE)



for (k in c(2:5)) {
	mat1 <- subset(mat, select=c("reQR", "euQR"))	# tiny matrix

	km1 <- kmeans(mat1, k)			# kmean performed on tiny matrix (euQR, reQR)
	km2 <- pam(mat1, k)

	print(k)
	print("reQR, euQR")
	print(km1$size)
#	png(filename=paste("k", k, "maaa.png", sep=""), width = 2000, height =2000 , units = "px")
#	plot(subset(mat_ent, select=c("reQR", "euQR")), col=km1$cluster, pch=4)
#	dev.off()
	png(filename=paste("k", k, "mpaa.png", sep=""))
	par(cex=2);plot(subset(mat_ent, select=c("reQR", "euQR")), col=km1$cluster * 5, pch=4)
	dev.off()
	png(filename=paste("k", k, "mpbb.png", sep=""))
	par(cex=2);plot(subset(mat_ent, select=c("reQR", "euQR")), col=km2$clustering * 5, pch=4)
	dev.off()
#	png(filename=paste("k", k, "mppa.png", sep=""), width=250, height=250, units="px")
#	plot(subset(mat_ent, select=c("reQR", "euQR")), col=km1$cluster, pch=4)
#	dev.off()
#	png(filename=paste("k", k, "maaalog.png", sep=""), width = 2000, height =2000 , units = "px")
#	plot(subset(mat_ent, select=c("reQR", "euQR")), log="xy", col=km1$cluster, pch=4)
#	dev.off()


	# kmeans on costs
#	print("cost unbound")
#	mat1 <- subset(mat, select=c("cost_unbound"))
#	km1 <- kmeans(mat1, k)
#	print(km1$size)
#	png(filename=paste("k", k, "unbound_costs.png", sep=""), width = 2000, height =2000 , units = "px")
#	plot(mat_ent$cost_unbound, col=km1$cluster, pch=4)
#	dev.off()
#	png(filename=paste("k", k, "unbound_costslog.png", sep=""), width = 2000, height =2000 , units = "px")
#	plot(mat_ent$cost_unbound, col=km1$cluster, log="xy", pch=4)
#	dev.off()
#	png(filename=paste("k", k, "unbound_costslogp.png", sep=""))
#	plot(mat_ent$cost_unbound, col=km1$cluster, log="xy", pch=4)
#	dev.off()
#	png(filename=paste("k", k, "unbound_costslogpp.png", sep=""), width=250, height=250, units="px")
#	plot(mat_ent$cost_unbound, col=km1$cluster, log="xy", pch=4)
#	dev.off()

#	print("cost bind")
#	mat1 <- subset(mat, select=c("cost_bind"))
#	km1 <- kmeans(mat1, k)
#	print(km1$size)
#	png(filename=paste("k", k, "bind_costs.png", sep=""), width = 2000, height =2000 , units = "px")
#	plot(mat_ent$cost_bind, col=km1$cluster, pch=4)
#	dev.off()
#	png(filename=paste("k", k, "bind_costslog.png", sep=""), width = 2000, height =2000 , units = "px")
#	plot(mat_ent$cost_bind, col=km1$cluster, log="xy")
#	dev.off()
#	png(filename=paste("k", k, "bind_costslogp.png", sep=""))
#	plot(mat_ent$cost_bind, col=km1$cluster, log="xy", pch=4)
#	dev.off()
#	png(filename=paste("k", k, "bind_costslogpp.png", sep=""), width=250, height=250, units="px")
#	plot(mat_ent$cost_bind, col=km1$cluster, log="xy", pch=4)
#	dev.off()
}

