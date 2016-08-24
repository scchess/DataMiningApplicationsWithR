#!/usr/bin/Rscript
library(Matrix)
library(cluster)
source("./plot_functions.r")
par(cex=2)

# several variables
filename = "Cluster3_dnssec.txt"                                        # input filename
r = 0.9                                                                 # quantity of total magnitude hold (related to tolerance for PCA)
nb_cluster = 5                                                          # cluster for kmean
output_file = paste( "k_clusters.txt")                                  # file where to print

tmp_file="/tmp/r2"

# functions to print kmean summary
mysummarykmean <- function(km){
	print("center")
	print(km$center)
	print("withinss")
	print(km$withinss)
	print("size")
	print(km$size)
}

# function to print vector summary
manuvectorsummary <- function(v){
    v[1] <- v[2]
    print(paste("max (", which.max(v), ") =", max(v)))
    print(paste("min (", which.min(v), ") =", min(v)))
    print(v)
}

clab <- c("euQR", "reQR", "tR", "euBR", "reBR", "tBR", "cQRT", "reOCC", "euOCC", "tOCC", "CHR", "eQR", "eCPU", "ePRT", "succRatio", "failRatio", "cltQNbr", "pltQNbr", "Qlen", "Rlen", "Sigcheck", "MIRT", "SDIRT", "MPRT", "SDPRT", "MTTL", "SDTTL")

mat_ent <- read.table(filename, row.names=1, col.names=clab)
mat_ent <- subset(mat_ent, cQRT > 0)                    # python script return -1 if no request is present and cQRT is used to plot several variables
mat_ent <- subset(mat_ent, MTTL > 0)                    # remove non valid TTL
mat <- mat_ent

mat <- subset(mat, cltQNbr > 20 ) 			#82% of traffic left
mat$logeuQR <- log(mat$euQR)
mat$logreQR <- log(mat$reQR)
mat$logSigcheck <- log(mat$Sigcheck)

submat <- subset(mat, select=c("euQR", "reQR", "Sigcheck"))
csubmat <- scale(submat, scale=TRUE, center=FALSE)
qrmat <- subset(mat, select=c("reQR", "euQR"))
cqrmat <- scale(qrmat, scale=FALSE, center=FALSE)
logmat <- subset(mat, select=c("logeuQR", "logreQR", "logSigcheck"))
lqrmat <- subset(logmat, select=c("logreQR","logeuQR"))
logmat <- subset(logmat, logSigcheck > 7 )              # remove singular points
clogmat <- scale(logmat, scale=FALSE, center=FALSE)
clqrmat <- scale(lqrmat, scale=FALSE, center=FALSE)

# silhouette width for pam
sw <- numeric(25)
swqr <- numeric(25)
swl <- numeric(25)
swlqr <- numeric(25)

# silhouette width for kmean 
ksw <- numeric(25)
kswqr <- numeric(25)
kswl <- numeric(25)
kswlqr <- numeric(25)

sink(file=output_file, split=TRUE)

for (k in c(2:3)) {
	# kmean normal
#	km <- kmeans(csubmat, center=k)
#    s <- silhouette(km$cluster, daisy(csubmat))
#    ksw[k] <- summary(s) $ avg.width
#	png(paste("k0", k, "kmean.png", sep=""))
#	plot(submat, col=km$cluster)
	#dev.off() 

	#print(paste(" - kmean - k =", k))
	#mysummarykmean(km)

	# kmed normal
	#km <- pam(csubmat, k=k)
    #sw[k] <- km $ silinfo $ avg.width
	#png(paste("k0", k, "kmed.png", sep=""))
	#plot(submat, col=km$clustering)
	#dev.off()
	
	#print(paste(" - kmed - k =", k))
	#print(km$clusinfo)
	
	# kmean log
	#km <- kmeans(clogmat, center=k)
    #kswl[k] <- summary(silhouette(km$cluster, daisy(clogmat))) $ avg.width
	#png(paste("k0", k, "kmean_log.png", sep=""))
	#plot(logmat, col=km$cluster)
	#dev.off() 

	#print(paste(" - kmean log - k =", k))
	#mysummarykmean(km)

	# kmed log
	#km <- pam(clogmat, k=k)
    #swl[k] <- km $ silinfo $ avg.width
	#png(paste("k0", k, "kmed_log.png", sep=""))
	#plot(logmat, col=km$clustering)
	#dev.off()
	
	#print(paste(" - kmed log - k =", k))
	#print(km$clusinfo)

    # kmean qr
    km <- kmeans(cqrmat, center=k)
    kswqr[k] <- summary(silhouette(km$cluster, daisy(cqrmat))) $ avg.width
    png(paste("k0", k, "qr_kmean.png", sep=""))
    par(cex=2);plot(qrmat, col=km$cluster * 5)
    dev.off()
	print(paste(" - qr kmean - k =", k))
    mysummarykmean(km)

    # kmed qr
    km <- pam(cqrmat, k=k)
    png(paste("k0", k, "qr_kmed.png", sep=""))
    swqr[k] <- km $ silinfo $ avg.width
    par(cex=2);plot(qrmat, col=km$clustering * 5)
    dev.off()
    print(paste(" - qr kmed - k =", k))
    print(km$clusinfo)

    # kmean log qr
    km <- kmeans(clqrmat, center=k, iter.max=1000)
    kswlqr[k] <- summary(silhouette(km$cluster, daisy(clqrmat))) $ avg.width
    png(paste("k0", k, "qr_log_kmean.png", sep=""))
    par(cex=2);plot(qrmat, col=km$cluster * 5, log="xy", pch=km$cluster)
    dev.off()
    print(paste(" - log qr kmean - k =", k))
    mysummarykmean(km)

    # kmed log qr
    km <- pam(clqrmat, k=k)
    swlqr[k] <- km $ silinfo $ avg.width
    png(paste("k0", k, "qr_log_kmed.png", sep=""))
    par(cex=2);plot(qrmat, col=km$clustering * 5, log="xy", pch=km$cluster)
    dev.off()
    print(paste(" - log qr kmed - k =", k))
    print(km$clusinfo)
}	

#print("ksw")
#manuvectorsummary(ksw)
print("kswqr")
manuvectorsummary(kswqr)
#print("kswl")
#manuvectorsummary(kswl)
print("kswlqr")
manuvectorsummary(kswlqr)


#print("sw")
#manuvectorsummary(sw)
print("swqr")
manuvectorsummary(swqr)
#print("swl")
#manuvectorsummary(swl)
print("swlqr")
manuvectorsummary(swlqr)

dev.off()
sink()
