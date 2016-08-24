#!/usr/bin/Rscript
# Rscript to plot barplot of savg sil vlue for k in c[2..15] for cluster form 
# "article_plot0.r" (should be run before)


aabb <- mat.or.vec(2,15)
aabb[1,1:15] <- kswlqr[1:15] 
aabb[2,1:15] <- swlqr[1:15] 
par(cex=2)
barplot(aabb[,2:15], beside=TRUE, col=c("dark blue", "pink"), names.arg=c(2:15), xlab="cluster number", ylab="average silhouette width", legend=c("kmean", "kmedoid"))

