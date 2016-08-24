
#
# Section 4.2
#
setwd("Data")
library(tm)
s <- Corpus(DirSource("abstracts"))
s <- tm_map(s, tolower)
s <- tm_map(s, removePunctuation)
s <- tm_map(s, removeWords, stopwords("english"))
s <- tm_map(s, stripWhitespace)
summary(s)
s[[1]]

#
# Subsection 4.3.1
#
dtm <- DocumentTermMatrix(s)
inspect(dtm[1:5, 1:5])
dtm
dtm.2 <- DocumentTermMatrix(s, control=list(bounds = list(global = c(2,Inf))))
dtm.2
removeSparseTerms(dtm, 0.99)

#
# Subsection 4.3.2
#
dtm.tfidf <- DocumentTermMatrix(s, control=list(weighting=weightTfIdf))
dtm.tfidf
inspect(removeSparseTerms(dtm.tfidf, 0.9)[1:5, 1:5])

#
# Subsection 4.3.3
#
findFreqTerms(dtm, 400)
findAssocs(dtm.2, "graph", 0.3)[1:3]
findAssocs(dtm.2, "edge", 0.3)

#
# Subsection 4.4.2
#
library(lda)
lex <- lexicalize(s)
head(lex$vocab)
lex$documents[[1]]
res <- lda.collapsed.gibbs.sampler(lex$documents, 10, lex$vocab, 100, 0.1, 0.1, compute.log.likelihood=T)
res$assignments[[1]]
res$document_sums[,1:10]
res$log.likelihoods

#
# Subsection 4.4.3
#
par(mfrow=c(2,1), pch=20)
plot(1:100, res$log.likelihoods[1,])
plot(1:100, res$log.likelihoods[2,])

#
# Subsection 4.4.4
#
top.topic.words(res$topics, 5, by.score=T)

#
# Subsection 4.4.5
#
library(reshape)
res$document_sums[,1:6]
d <- melt(res$document_sums)
colnames(d) <- c("topic", "document", "value")
head(d)
library(ggplot2)
library(RColorBrewer)
d2 <- subset(d, d$document < 50)
d2$topic <- as.factor(d2$topic)
d2$document <- as.factor(d2$document)
ggplot(d2, aes(x = document)) + geom_bar(aes(weight=value, fill = topic), position = 'fill') + scale_fill_manual(values = rev(brewer.pal(10, "Spectral")))

#
# Subsection 4.5.1
#
mat <- t(as.matrix(res$document_sums)) %*% as.matrix(res$document_sums)
d <- diag(mat)
sim <- t(t(mat/sqrt(d))/sqrt(d))
sim[1:5, 1:5]
as.matrix(dist(t(res$document_sums)))[1:5, 1:5]

#
# Subsection 4.5.2
#
heatmap(sim[1:100,1:100])
heatmap(sim[1:20, 1:20])

#
# Subsection 4.6.1
#
load("s.authors.RData")
head(s.authors, 4)
getJointPapers <- function(s) { if (length(s) > 1) s else c() }
s.authors <- sapply(s.authors, getJointPapers)
library(igraph)
foo <- graph(c(1,2,1,3,3,4,3,5), directed=F)
plot(foo)
authors <- unique(as.vector(unlist(s.authors)))
head(authors)
getAuthorId <- function(n) which(authors == n)
getAuthorId("Jinggang Tan")
s.authors[[3]]
combn(letters[1:4], 2)
combn(letters[1:4], 3)
getEdges <- function(s) { if (is.null(s) || length(s) < 2) { c() } else { unlist(lapply(combn(s, 2), getAuthorId)) } }
getEdges(s.authors[[3]])
authors[c(5,6,7)]
g <- graph(as.vector(unlist(lapply(s.authors, getEdges))), directed=F)
g

#
# Subsection 4.6.2
#
library(sna)
b <- betweenness(as.array(get.adjacency(g)))
head(b)
which(b>25)
authors[which(b>25)]
top <- data.frame(name=authors[which(b>25)], centrality=b[which(b>25)])
top[order(top$centrality, decreasing=T),]
b <- betweenness(as.array(get.adjacency(g)), cmode="lengthscaled")
top <- data.frame(name=authors[which(b>12)], centrality=b[which(b>12)])
top[order(top$centrality, decreasing=T),]