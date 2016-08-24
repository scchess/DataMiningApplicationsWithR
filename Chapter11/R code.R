## Chapter 11 Predicting Seabed Hardness Using Random Forest in R

setwd("C:/temp/")
hard <- read.table("hardness.csv", header = TRUE, sep = ",")
class(hard)
hard[hard==9999] <- NA
str(hard)
hard$hardness2 <- 0
hard$hardness2[hard$hardness == "hard"] <- 1
hcor <- round(cor(hard[,-c(1, 17)], use = "na.or.complete", method = "spearman"), 2)
library(gridExtra)
grid.draw(tableGrob(hcor[, -16], show.csep = TRUE, show.rsep = TRUE, show.box = TRUE, separator = "grey"))
pairs(hard[, -c(1, 17, 18)])
h2 <- hard[,-c(1, 17)]
par(mfrow = c(3,5))
for (i in 1:15){
    h3 <- subset(h2, abs(h2[,i]) >= 0)
    plot(h3[, i], h3[,16], ylab = "Hardness", xlab = names(h3)[i])
    lines(lowess(h3[,16]~h3[,i]), col = "blue")
}
hard2 <- na.omit(hard)
dim(hard2)

library(randomForest)
tuneRF(hard2[,-c(1,17,18)], hard2[,17], ntreeTry = 100)
set.seed(123)
rf.1 <- randomForest(hard2[,-c(1,17,18)], hard2[,17], data = hard2,
importance = TRUE, ntree = 500, proximity = TRUE)
varImpPlot(rf.1)
names(rf.1)

dev.rf1 <- predict(rf.1, hard2)
grid.table(table(hard2[,17], dev.rf1))
result1 <- replicate(100, rfcv(hard2[,-c(1,17,18)], hard2[,17], scale =
"non.log", cv.fold = 10, step = -1), simplify = FALSE)
error.cv <- sapply(result1, "[[", "error.cv")
matplot(result1[[1]]$n.var, cbind(rowMeans(error.cv), error.cv), type = "l",
lwd = c(2, rep(1, ncol(error.cv))), col = c(2, rep(1, ncol(error.cv))), lty = 1, xlab = "Number of variables", ylab = "CV Error")

plot(result1[[1]]$n.var, (1-rowMeans(error.cv))*100, type = "l", lwd = 2, col = 2, lty = 1, xlab = "Number of variables", ylab = "Correct classification
rate (%)")
ccr.cv.1 <- NULL; kappa.cv.1 <- NULL; mccr.cv.1 <- NULL; mkappa.cv.1 <- NULL
for (i in 1:100){
rfcv.1 <- rf.cv(hard2[,-c(1,17,18)], hard2[,17], cv.fold = 10, ntree = 500)
ccr.cv.1[i] <- rfcv.1$ccr.cv; kappa.cv.1[i] <- rfcv.1$kappa.cv
mccr.cv.1[i] <- mean(ccr.cv.1); mkappa.cv.1[i] <- mean(kappa.cv.1)
}
x <- c(1:100)
par(mfrow=c(2,1), font.axis = 2, font.lab = 2)
plot(ccr.cv.1 ~ x, xlab = "Iteration", ylab = "Correct classification rate")
points(mccr.cv.1 ~ x, col = 2)
plot(kappa.cv.1 ~ x, xlab = "Iteration", ylab = "Kappa")
points(kappa.cv.1 ~ x, col = 2)
par(mfrow = c(3,1), font.axis = 2, font.lab = 2)
partialPlot(rf.1, hard2, prock)
partialPlot(rf.1, hard2, bs)
partialPlot(rf.1, hard2, bathy)
ra <- read.csv("area_a1.csv", sep = ",", header = FALSE)
dim(ra);
[1] 4653653      15
pred.a1 <- ra[, c(1,2)]
pred.a1$hardness <- predict(rf.1, ra)
write.table(pred.a1, "hardness.pred.a1_3vars.csv", sep = ",", row.names =
FALSE)
rf.cv <- function (trainx, trainy, cv.fold = 10,
    mtry = function(p) max(1, floor(sqrt(p))), ntree=500, ...) {
    classRF <- is.factor(trainy)
    n <- nrow(trainx)
    p <- ncol(trainx)
    cv.pred <- NULL
    if (classRF) {
        f <- trainy
    }     else {
        stop ("This function is only for categorical response variable")
    }
    nlvl <- table(f)
    idx <- numeric(n)
    for (i in 1:length(nlvl)) {
        idx[which(f == levels(f)[i])] <- sample(rep(1:cv.fold,
            length = nlvl[i]))
    }
    for (i in 1:cv.fold) {
        all.rf <- randomForest(trainx[idx != i, , drop = FALSE],
            trainy[idx != i], trainx[idx == i, , drop = FALSE],
            trainy[idx == i], mtry = mtry(p), ntree=ntree)
        cv.pred[idx == i] <- all.rf$test$predicted
    }

    require(psy)
        data1<-as.data.frame(cbind(cv.pred, trainy))
        kappa.cv<-ckappa(data1)$kappa
        ccr.cv<-sum(diag(table(data1)))/sum(table(data1))*100

    list(kappa.cv = kappa.cv, ccr.cv = ccr.cv, predicted = cv.pred)
}

