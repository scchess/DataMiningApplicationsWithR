#!/usr/bin/Rscript
# R script to make kmean clusters

# importations
library(Matrix)
source("./plot_functions.r")
par(cex=2)


# several variables
filename = "cluster_entry.txt"	 					# input filename
filename="Cluster3_dnssec.txt"
r = 0.9									# quantity of total magnitude hold (related to tolerance for PCA)
nb_cluster = 4								# cluster for kmean
output_file = paste(format(Sys.time(), "%F-%T"), "-Rout.txt", sep="")	# file where to print

tmp_file="/tmp/r"

clab <- c("euQR", "reQR", "tR", "euBR", "reBR", "tBR", "cQRT", "reOCC", "euOCC", "tOCC", "CHR", "eQR", "eCPU", "ePRT", "succRatio", "failRatio", "cltQNbr", "pltQNbr", "Qlen", "Rlen", "Sigcheck", "MIRT", "SDIRT", "MPRT", "SDPRT", "MTTL", "SDTTL")
clabf <- c("euQR", "reQR", "CHR", "cQRT", "MIRT", "MPRT", "MTTL")


mat_ent <- read.table(filename, row.names=1, col.names=clab)

mat <- subset(mat_ent, cQRT > 0)			# python script return -1 if no request is present and cQRT is used to plot several variables
pca <- prcomp(mat, scale=TRUE, center=TRUE)
mag <- sum(pca$sdev * pca$sdev)			# total magnitude
pca <- prcomp(mat, tol= (1-r)*mag/(pca$sdev[1] * pca$sdev[1]), scale=TRUE, center=TRUE)


s_mat <- subset(mat, select=clabf)
s_pca <- prcomp(s_mat, scale=TRUE, center=TRUE)
mag <- sum(s_pca$sdev * s_pca$sdev)			# total magnitude
s_pca <- prcomp(s_mat, tol= (1-r)*mag/(s_pca$sdev[1] * s_pca$sdev[1]), scale=TRUE, center=TRUE)

m_mat <- subset(mat, select=c("reQR", "euQR"))
m_pca <- prcomp(m_mat, scale=TRUE, center=TRUE)

km <- kmeans(mat, nb_cluster)
s_km <- kmeans(mat, nb_cluster)
m_km <- kmeans(mat, nb_cluster)


X11()
par(cex=2);plot(s_mat, col=km$cluster)
par(cex=2);plot(m_mat, col=km$cluster)
