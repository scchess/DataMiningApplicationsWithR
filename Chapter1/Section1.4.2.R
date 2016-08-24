# code block 1

map.pmusumm <- rhmap({
   # r is the data.frame of values for a 5-minute block
   # k is the key (time) for the block
   colNames <- colnames(r)
   freqColumns <- which(grepl("freq", colNames))
   pmuName <- gsub("(.*)\\.freq", "\\1", colNames[freqColumns])
   for(j in seq_along(freqColumns)) { # loop through frequency columns
      v <- r[,freqColumns[j]] / 1000 + 60 # convert to HZ
      
      rhcollect(
         pmuName[j], # key is the PMU
         data.frame(
            time   = k,
            min    = min(   v, na.rm=TRUE),
            max    = min(   v, na.rm=TRUE),
            mean   = mean(  v, na.rm=TRUE),
            stdev  = sd(    v, na.rm=TRUE),
            median = median(v, na.rm=TRUE),
            nna    = length(which(is.na(v)))
         )
      )
   }
})

# code block 2

reduce.pmusumm <- expression(
   pre = {
      res <- NULL
   },
   reduce = {
      res <- do.call(rbind, c(list(res), reduce.values))
   },
   post = {
      res <- res[order(res$time),] # order results by time
      res$time <- as.POSIXct(res$time, origin="1970-01-01", tz="UTC")
      rhcollect(reduce.key, res)
   }
)

# code block 3

summ5min <- rhwatch(
   map=map.pmusumm, 
   reduce=reduce.pmusumm, 
   input="blocks5min",
   output="blocks5min_summary"
)

# code block 4

map.freqquant <- rhmap({
   colNames <- colnames(r)
   freqColumns <- which(grepl("freq", colNames))
   pmuName <- gsub("(.*)\\.freq", "\\1", colNames[freqColumns])
   
   for(j in seq_along(freqColumns)) { # loop through frequency columns
      freqtab <- table(r[,freqColumns[j]])
      
      rhcollect(
         pmuName[j], # key is the PMU
         data.frame(
            level = as.integer(names(freqtab)),
            count = as.numeric(freqtab)
         )
      )
   }
})

# code block 5

reduce.tab <- expression(
   pre = {
      res <- NULL
   },
   reduce = {
      tabUpdate <- do.call(rbind, c(list(res), reduce.values))
      tabUpdate <- xtabs(count ~ level, data = tabUpdate)
      res <- data.frame(
         level = as.integer(names(tabUpdate)),
         count = as.numeric(tabUpdate)
      )
   },
   post = {
      rhcollect(reduce.key, res)
   }
)


# code block 6

freqtab <- rhwatch(
   map=map.freqquant, reduce=reduce.tab, 
   input="blocks5min", output="frequency_quantile"
)

# code block 7

ftpmu <- freqtab[[1]][[2]]
cs <- cumsum(ftpmu$count)
f <- ppoints(2000, 1)
q <- ftpmu$level[sapply(f * sum(ftpmu$count), function(x) 
   min(which(x <= cs)))]
plot(f, q / 1000 + 60, xlab="Quantile", ylab="Frequency (HZ)")

# code block 8

map.repeat <- rhmap({
   colNames <- colnames(r)
   freqColumns <- which(grepl("freq", colNames))
   pmuName <- gsub("(.*)\\.freq", "\\1", colNames[freqColumns])
   
   for(j in seq_along(freqColumns)) { # step through frequency columns
      curFreq <- r[,freqColumns[j]]
      curFreq <- curFreq[!is.na(curFreq)] # omit missing values
      changeIndex <- which(diff(curFreq) != 0) # find index of changes
      changeIndex <- c(0, changeIndex, length(curFreq)) # pad with beg/end
      runLengths <- diff(changeIndex) # run length is diff between changes
      runValues <- curFreq[changeIndex[-1]] # get value assoc with lengths
   
      uRunValues <- unique(runValues)
      for(val in uRunValues) { # for each unique runValue tabulate lengths
         repeatTab <- table(runLengths[runValues == val])
         rhcollect(
            list(pmuName[j], val), 
            data.frame(
               level = as.integer(names(repeatTab)),
               count = as.numeric(repeatTab)
            )
         )
      }
   }
})


# code block 9

repeatTab <- rhwatch(
   map=map.repeat, reduce=reduce.tab, 
   input="blocks5min", output="frequency_repeated"
)

# code block 10

repeatZero <- do.call(rbind, lapply(repeatTab, function(x) {
   if(x[[1]][[2]] == 0) {
      data.frame(pmu=x[[1]][[1]], x[[2]])
   }
}))
library(lattice)
xyplot(log2(count) ~ log2(level) | pmu, data=repeatZero)

# code block 11

map.ljung <- rhmap({
   colNames <- colnames(r)
   freqColumns <- which(grepl("freq", colNames))
   pmuName <- gsub("(.*)\\.freq", "\\1", colNames[freqColumns])
   
   # apply Box.test() to each column
   pvalues <- apply(r[,freqColumns], 2, function(x) {
      boxres <- try(
         Box.test(x, lag=10, type="Ljung-Box")$p.value, 
         silent=TRUE
      )
      ifelse(inherits(boxres, "try-error"), NA, boxres)
   })
   
   rhcollect(
      "1", 
      data.frame(time=k, t(pvalues))
   )
})

# code block 12

reduce.rbind <- expression(
   pre = {
      res <- NULL
   },
   reduce = {
      res <- rbind(res, do.call(rbind, reduce.values))
   },
   post = {
      rhcollect(reduce.key, res)
   }
)

ljungPval <- rhwatch(
   map=map.ljung, reduce=reduce.rbind, 
   input="blocks5min", output="frequency_ljung"
)

