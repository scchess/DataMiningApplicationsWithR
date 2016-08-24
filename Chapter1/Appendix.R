pmuNames <- apply(expand.grid(LETTERS, LETTERS)[,2:1], 1, function(x) paste(x, sep="", collapse=""))[1:38]
colNames <- as.vector(t(sapply(c("flag", "freq"), function(x) paste(pmuNames, x, sep="."))))

library(MASS)
set.seed(4321)
Sigma <- matrix(nrow=38, ncol=38, data=0.99)
diag(Sigma) <- 1
innov <- mvrnorm(n=4001, rep(0, 38), Sigma) * 0.8
freqs <- apply(innov, 2, function(x) {
	tmp <- arima.sim(3001, model=list(ar=c(0.9771, 0.1647, 0.0361, -0.1121, 0.0245, -0.1473)), start.innov=x[1:1000], innov=x[-c(1:1000)])
	as.integer(splinefun(seq(1, 90030, by=30), tmp)(1:90000) + rnorm(90000, sd=0.2))
})
flags <- matrix(nrow=90000, data=sample(c(rep(0, 2000), 128:132), 90000*38, replace=TRUE))
freqs[flags >= 128] <- -1                                                          
pmudat <- data.frame(matrix(nrow=90000, ncol=38*2))
flagColIndex <- seq(1, 38*2, by=2)                                                 
pmudat[,flagColIndex] <- flags
pmudat[,flagColIndex + 1] <- freqs
colnames(pmudat) <- colNames

startTimes <- as.integer(as.POSIXct("2012-01-01", tz="UTC")) + c(0:9) * 5*60

pmudat <- lapply(seq_along(startTimes), function(i) {
   ind <- ((i - 1) * 9000 + 1):(i * 9000)
   list(
      startTimes[i],
      data.frame(pmudat[ind,], time=as.POSIXct(startTimes[i], 
         origin="1970-01-01", tz="UTC") + c(0:8999) / 30)
   )
})
# insert some bad data: a repeated sequence, white noise and a freq shift
pmudat[[5]][[2]][2000:3000,8] <- 0
pmudat[[10]][[2]][,2] <- rnorm(9000, sd=3.6)
pmudat[[2]][[2]][1000:2000,2] <- pmudat[[2]][[2]][1000:2000,2] + 20

library(Rhipe)
rhinit()

hdfs.setwd("/")

rhwrite(pmudat, "blocks5min")



