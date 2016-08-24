#!/usr/bin/Rscript
# Rscript cost k mean

# importations
library(Matrix)
library(cluster)
source("./plot_functions.r")
par(cex=2)

# several variables
filename = "Cluster3_dnssec.txt"					# input filename
#filename = "cluster_entry_5min.txt"
output_file = paste( "k_clusters_bin_unbound.txt")					# file where to print

# function to print vector summary
manuvectorsummary <- function(v){
    v[1] <- v[2]
    print(paste("max (", which.max(v), ") =", max(v)))
    print(paste("min (", which.min(v), ") =", min(v)))
    print(v)
}


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
write.table(scale (mat, scale=TRUE, center=TRUE), file=tmp_file)
mat <- read.table(tmp_file, header=TRUE, row.names=1)
mat <- subset(mat, cltQNbr > 20)

matbind <- subset(mat, select=c("cost_bind"))
matbind <- log(matbind)
swbind <- numeric(15)
twbind <- numeric(15)
sink (output_file, split=TRUE)



for (k in c(2:5)) {
	print(paste("k",k))
	km <- kmeans(matbind, k, iter.max=1000)
	swbind[k] <- summary(silhouette(km$cluster, daisy(matbind))) $ avg.width
	png(paste("bind",k,"kmean.png", sep=""))
	par(cex=2);plot(mat$cost_bind, col=km$cluster * 5, log="xy", ylab="cost")
	dev.off()
	km <- pam(matbind, k)
	twbind[k] <- km $ silinfo $ avg.width
	png(paste("bind",k,"kmedo.png", sep=""))
	par(cex=2);plot(mat$cost_bind, col=km$clustering * 5, log="xy", ylab="cost")
	dev.off()
}

manuvectorsummary(swbind)
manuvectorsummary(twbind)
