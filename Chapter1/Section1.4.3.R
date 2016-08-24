# code block 1

map.oos <- rhmap({
   colNames <- colnames(r)
   freqColumns <- which(grepl("freq", colNames))
   pmuName <- gsub("(.*)\\.freq", "\\1", colNames[freqColumns])
   
   # make r only contain frequency information
   tt <- r$time
   r <- r[,freqColumns]
   names(r) <- pmuName
   
   # get all combinations of pairs
   freqPairs <- combn(ncol(r), 2)
   freqPairNames <- rbind(
      names(r)[freqPairs[2,]], names(r)[freqPairs[1,]]
   )
   
   # loop through all pairs and look for significant differences
   for(i in 1:ncol(freqPairs)) {
      s1 <- freqPairs[1,i]
      s2 <- freqPairs[2,i]
      
      isSignif <- ifelse(abs(r[,s1] - r[,s2]) > 10, 1, 0)

      changeIndex <- which(diff(isSignif) != 0) # find index of changes
      changeIndex <- c(0, changeIndex, length(isSignif)) # pad
      runLengths <- diff(changeIndex) # run length is diff between changes
      runValues <- isSignif[changeIndex[-1]]
      # we are interested in 1’s that repeat more than 90 times
      signifIndex <- which(runValues == 1 & runLengths > 90)

      for(ix in signifIndex) {
         rhcollect(
	  freqPairNames[,i], 
	  data.frame(time=tt[changeIndex[-1][ix]], length=runLengths[ix])
      )
      }
   }
})

oosFreq <- rhwatch(
   map=map.oos, 
   reduce=reduce.rbind, 
   input="blocks5min",
   output="frequency_outofsync"
)

# code block 2

map.freqoverlap <- rhmap({
   timeVec <- r$time
   freqColumns <- which(grepl("freq", names(r)))
   r <- r[,freqColumns]
   r <- data.frame(r, time=timeVec)

   rhcollect(k, r)
   rhcollect(k - 5*60, r[as.numeric(timeVec) < k + 30,])
   rhcollect(k + 5*60, r[as.numeric(timeVec) >= k + 5*60 - 30,])
})

z <- rhwatch(
   map=map.freqoverlap, reduce=reduce.rbind, 
   input="blocks5min", output="blocks5min_freq_overlap",
   readback=FALSE
)

# code block 3

freqOverlap <- rhread("blocks5min_freq_overlap", max=2)
nrow(freqOverlap[[1]][[2]])

# code block 4

getTripFeatures <- function(freqMat, tt, span = 14, minLength = 1) {
   # get the mean frequency of the 38 PMUs at each time point
   freqMeans <- apply(freqMat, 1, function(x) mean(x, na.rm=TRUE))
   nobs <- length(freqMeans)

   startTime <- ceiling(span * 30 / 2)
   endTime <- floor(nobs - span * 30 / 2)

   # apply loess smoothing to the time point-wise means
   smoothFreq <- predict(
      loess(freqMeans ~ c(1:length(freqMeans)), degree=2, 
         span=span * 30 / nobs, family="symmetric", 
         control=loess.control(sufrace="direct")), 
      newdata=1:nobs
   )

   # get information about each increasing and decreasing segment
   slopeDir <- sign(diff(smoothFreq))
   changePoints <- which(abs(diff(slopeDir)) == 2) + 1 
   starts <- c(1, changePoints)
   ends <- c(changePoints - 1, length(smoothFreq))

   ind <- starts > span*30 / 2   & 
      ends-starts > minLength*30 & 
      ends < nobs - span*30/2
   starts <- starts[ind]
   ends <- ends[ind]
   signs <- sign(smoothFreq[ends] - smoothFreq[starts])

   sliceVec <- rep(NA, nobs)
   features <- do.call(rbind, lapply(seq_along(starts), function(i) {
      sliceVec[starts[i]:ends[i]] <<- i

      data.frame(
         start = tt[starts[i]],
         duration = (ends[i] - starts[i]) / 30,
         magnitude = abs(smoothFreq[starts[i]] - smoothFreq[ends[i]]),
         largestSlopeChange = 
            max(abs(diff(smoothFreq[starts[i]:ends[i]]))) * signs[i]
      )
   }))

   list(
      features = features,
      data = data.frame(time=tt, freqMeans=freqMeans, 
         smoothFreq=smoothFreq, slice=sliceVec)
   )
}

# code block 5

v <- freqOverlap[[2]][[2]] # get the value of the second key/value pair
v <- v[order(v$time),] # make sure they are ordered
freqColumns <- which(grepl("freq", colnames(v)))

vFeat <- getTripFeatures(v[,freqColumns], v$time)

xyplot(smoothFreq ~ time, groups=slice, data=vFeat$data, type="l", lwd=3,
   panel=function(x, y, ...) {
      panel.points(vFeat$data$time, vFeat$data$freqMeans, col="lightgray")
      panel.xyplot(x, y, ...)
   }
)

# code block 6

map.gentrip <- rhmap({
   r <- r[order(r$time),] # make sure it is ordered
   freqColumns <- which(grepl("freq", colnames(r)))

   rhcollect(k, getTripFeatures(r[,freqColumns], r$time)$features)
})

tripFeat <- rhwatch(
   map=map.gentrip, reduce=reduce.rbind, 
   input="blocks5min", output="frequency_gentrip"
)


