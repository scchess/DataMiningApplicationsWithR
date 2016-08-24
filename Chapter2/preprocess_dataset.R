require(tm)
require(XML)

# Use TextMining package to load and preprocess documents
loadReutersDataset <- function() {  
  cat(sprintf("load dataset...\n"))
  reuters <- Corpus(DirSource("./data/reuters21578/"),
                    readerControl = list(reader = readReut21578XML))
  return(reuters)
}

# This function preprocess an XML Corpus (tm package) into a plain text dataset.
preprocessDataset <- function(dataset) {
    # Transform XML into plain text
    cat(sprintf("transform into plain text...\n"))
    dataset_plain <- tm_map(dataset, as.PlainTextDocument)
    
    # Remove extra white spaces
    cat(sprintf("remove extra white spaces...\n"))
    dataset_plain <- tm_map(dataset_plain, stripWhitespace)
    
    # To lower case
    cat(sprintf("letters to lower case...\n"))
    dataset_plain <- tm_map(dataset_plain, tolower)
    
    # Remove stopwords
    cat(sprintf("remove stopwords...\n"))
    dataset_plain <- tm_map(dataset_plain, removeWords, stopwords("english"))
    
    # Stem words (currently not working on MAC OS X Lion)
    # dataset_plain <- tm_map(dataset_plain, stemDocument)
    
    return(dataset_plain)
}

# Create document-term matrix from dataset
createDocumentTermMatrix <- function(dataset) {
  dtm <- DocumentTermMatrix(dataset)

  return(dtm)
}

# Extract and return document-term sparse matrix 
extractDocumentTermMatrix <- function(dtm) {
  # dataset <- as(inspect(dtm), "sparseMatrix")  
  # Avoid printing the inspection of the data-term matrix
  { sink("/dev/null"); dataset <- as(inspect(dtm), "sparseMatrix"); sink(); }
  return(dataset)
}

# Select the documents of a class and generate class labels
extractLabels <- function(dataset, class) {
  
  # Function to find a topic in dataframe of list
  findTopic <- function(list.of.topics, topic) {
    topic %in% list.of.topics
  }
  
  # Build dataframe of topics to query
  query.df <- prescindMeta(dataset, "Topics")
  # Apply function to dataframe
  la <- lapply(query.df$Topics, FUN = findTopic, class)
  # Unlist results
  lu <- unlist(la)
  # Build vector of labels
  labels <- rep("neg", length(dataset))
  # Update with positive examples
  labels[lu] <- "pos"
  
  return(labels)
}

# Take in input a triplet form row,column,value
# and returns a sparse matrix
loadDataset <- function(dataset) {
  t <- read.table(dataset)
  return(sparseMatrix(t[,1], t[,2], x=t[,3]))
}

# Take in input a column file of classes labels
# and returns a dense column matrix
loadLabels <- function(labels) {
  m <- as.matrix(read.table(labels))
  colnames(m)[1] <- "labels"
  return(m)
}
