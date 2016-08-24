# code block 1

set.seed(4321) # make sure that we always get the same partition
permute <- sample(1:150, 150)
splits <- rep(1:3, 50)
irisSplit <- tapply(permute, splits, function(x) {
   iris[x, c("Sepal.Length", "Species")]
})

# code block 2

library(Rhipe)
rhinit()
hdfs.setwd("/")

# code block 3

irisSplit <- lapply(seq_along(irisSplit), function(i) 
   list(i, irisSplit[[i]])
)
rhwrite(irisSplit, "irisData")

# code block 4

maxMap <- expression({
   for(r in map.values) {
      by(r, r$Species, function(x) {
         rhcollect(
            as.character(x$Species[1]), # key
            max(x$Sepal.Length)         # value
         )
      })
   } 
})

# code block 5

map.keys <- lapply(irisSplit, "[[", 1)
map.values <- lapply(irisSplit, "[[", 2)

# code block 6

maxReduce <- expression(
   pre={
      speciesMax <- NULL
   },
   reduce={
      speciesMax <- max(c(speciesMax, do.call(c, reduce.values)))
   },
   post={
      rhcollect(reduce.key, speciesMax)
   }
)

# code block 7

maxSepalLength <- rhwatch(
   map=maxMap, 
   reduce=maxReduce, 
   input="irisData", 
   output="irisMaxSepalLength", 
)

# code block 8

do.call("rbind", lapply(maxSepalLength, function(x) {
   data.frame(species=x[[1]], max=x[[2]])
}))

# code block 9

maxMap <- rhmap({
   by(r, r$Species, function(x) {
      rhcollect(
         as.character(x$Species[1]), # key
         max(x$Sepal.Length)         # value
      )
   })
})

