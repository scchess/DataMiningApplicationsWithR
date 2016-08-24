nb <- function(model = "bernoulli", smoothing = "laplace") {
  # Initialize a simple Naive-Bayes binary classifier
  #
  # Args:
  #   model: The parametric form of the features distribution.
  #   smoothing: The smoothing method to be used for the estimation.
  #
  # Returns:
  #   A Naive-Bayes classifier ready to be estimated.
  
  # Initialize the object and the basic structures
  inst <- list()
  class(inst) <- c("nb", model)
  inst$type <- model
  inst$smoothing <- smoothing
  inst$classes.names <- list()
  inst$features.num <- 0
  inst$examples.num <- 0
  # Create distributional parameters
  inst$smoothing.params <- list()
  inst$features.params <- list()
  inst$classes.params <- list()
  # Sufficient statistics for the objects' parametric distribution
  inst$features.freq <- list()
  # Data structure to speed-up computation
  inst$features.freq.tot <- list()
  
  return(inst)
}

# Generic function for naive bayes models
nbEstimate <- function(x, dataset, labels, params) {
  UseMethod("nbEstimate")
}

# Estimate Bernoulli parameters
nbEstimate.bernoulli <- function(x, dataset, labels, params) {
  # Preprocess dataset for Bernoulli
  dataset <- prepareDataset(x, dataset)
  
  # Initialize dataset specific variables
  # Sorted class names
  x$classes.names <- sort(unique(labels), decreasing=TRUE)
  x$smoothing.params <- params  
  
  # Number of unique features
  x$features.num <- dim(dataset)[2]
  x$examples.num <- dim(dataset)[1]
  
  # Compute sufficient statistics for each class
  # features.freq is a features x class matrix
  # features.freq.tot is a tot_features x class matrix for the current model
  x$features.freq <- sapply(x$classes.names,
                            function(r) colSums(dataset[c(labels == r),]))
  x$features.freq.tot <- as.vector(table(labels)[x$classes.names])

  # Bernoulli parameter for each class
  x$classes.params <- as.vector(table(labels)[x$classes.names]) / x$examples.num
  
  # Compute probability estimates
  x <- nbUpdate(x, params)
  return(x)
}

# Estimate multinomial parameters
nbEstimate.multinomial <- function(x, dataset, labels, params) {
  # Initialize dataset specific variables
  # Sorted class names
  x$classes.names <- sort(unique(labels), decreasing=TRUE)
  x$smoothing.params <- params  
  
  # Number of unique features
  x$features.num <- dim(dataset)[2]
  x$examples.num <- dim(dataset)[1]
    
  # Compute sufficient statistics for each class
  # features.freq is a features x class matrix
  # features.freq.tot is a tot_features x class matrix for the current model
  x$features.freq <- sapply(x$classes.names,
                            function(r) colSums(dataset[c(labels == r),]))
  x$features.freq.tot <- colSums(x$features.freq)
    
  # A priori probability for each class
  x$classes.params <- as.vector(table(labels)[x$classes.names]) / x$examples.num
  
  # Compute probability estimates
  x <- nbUpdate(x, params)
  return(x)
}

# Estimate Poisson parameters 
nbEstimate.poisson <- function(x, dataset, labels, params) {
  # Initialize dataset specific variables
  # Sorted class names
  x$classes.names <- sort(unique(labels), decreasing=TRUE)
  x$smoothing.params <- params  
  
  # Number of unique features
  x$features.num <- dim(dataset)[2]
  x$examples.num <- dim(dataset)[1]
  
  # Compute sufficient statistics for each class
  # features.freq is a features x class matrix
  # features.freq.tot is the total number of objects for each class
  x$features.freq <- sapply(x$classes.names,
                            function(r) colSums(dataset[c(labels == r),]))
  x$features.freq.tot <- as.vector(table(labels)[x$classes.names])
  
  # A priori probability for each class
  x$classes.params <- as.vector(table(labels)[x$classes.names]) / x$examples.num
  
  # Compute probability estimates
  x <- nbUpdate(x, params)
  return(x) 
}

# Update estimates
nbUpdate <- function(x, ...) {
  UseMethod("nbUpdate")
}

# Update Bernoulli estimates
nbUpdate.bernoulli <- function(x, params) {
  # Laplace smoothing for Bernoulli
  if (x$smoothing == "laplace") {
    x$features.params <- t(t(x$features.freq + 1) / (x$features.freq.tot + 2))
  }
  # Beta prior
  else if (x$smoothing == "prior") {
    x$smoothing.params$alpha <- (alpha <- params$alpha)
    x$smoothing.params$beta  <- (beta <- params$beta)
    x$features.params <- t(t(x$features.freq + alpha) / 
                            (x$features.freq.tot + (alpha + beta)))
  }
  # Jelinek-Mercer interpolation
  else if (x$smoothing == "interpolation") {
    x$smoothing.params$lambda <- (lambda <- params$lambda)
    features.freqs    <- rowSums(x$features.freq)
    collection.freqs  <- sum(x$features.freq.tot)
    x$features.params <- (1 - lambda) * t(t(x$features.freq) / x$features.freq.tot) +
                         lambda * (features.freqs / collection.freqs)
  }
  return(x)
}

# Update multinomial estimate
nbUpdate.multinomial <- function(x, params) {
  # Laplace smoothing for multinomial
  if (x$smoothing == "laplace") {
    x$features.params <- t(t(x$features.freq + 1) / 
                            (x$features.freq.tot + x$features.num))
  }
  # Dirichlet prior
  else if (x$smoothing == "prior") {
    features.freqs     <- rowSums(x$features.freq)
    collection.freqs   <- sum(x$features.freq.tot)
    x$smoothing.params <- (mu <- params$mu)
    collection.probs   <- mu * features.freqs / collection.freqs
    sum.probs          <- sum(collection.probs)
    x$features.params  <- t(t(x$features.freq + collection.probs) / 
                             (x$features.freq.tot + sum.probs))
  }
  # Jelinek mercer interpolation
  else if (x$smoothing == "interpolation") {
    features.freqs     <- rowSums(x$features.freq)
    collection.freqs   <- sum(x$features.freq.tot)
    x$smoothing.params <- (lambda <- params$lambda)
    x$features.params  <- (1 - lambda) * t(t(x$features.freq) / x$features.freq.tot) +
                          lambda * (features.freqs / collection.freqs)
  }
  return(x)
}

# Update Poisson estimates
nbUpdate.poisson <- function(x, params) {
  # Laplace smoothing for Poisson
  if (x$smoothing == "laplace") {
    x$features.params <- t(t(x$features.freq + 1) / (x$features.freq.tot + 2))
  }
  # Gamma prior
  else if (x$smoothing == "prior") {
    x$smoothing.params$alpha <- (alpha <- params$alpha)
    x$smoothing.params$beta  <- (beta <- params$beta)
    x$features.params <- t(t(x$features.freq + alpha) / 
                            (x$features.freq.tot + (alpha + beta)))
  }
  # Jelinek-Mercer interpolation
  else if (x$smoothing == "interpolation") {
    x$smoothing.params$lambda <- (lambda <- params$lambda)
    features.freqs    <- rowSums(x$features.freq)
    collection.freqs  <- sum(x$features.freq.tot)
    x$features.params <- (1 - lambda) * t(t(x$features.freq) / x$features.freq.tot) +
                         lambda * (features.freqs / collection.freqs)
  }
  return(x)
}

# Classify examples
nbClassify <- function(x, ...) {
  UseMethod("nbClassify")
}

# Classify using bernoulli model
nbClassify.bernoulli <- function(x, dataset) {
  dataset <- prepareDataset(x, dataset)

  # precompute the sum over features of log(1 - param)
  sumnegative  <- colSums(log(1 - x$features.params))
  
  # compute p(d, c)
  logfeatures  <- log(x$features.params)
  lognfeatures <- log(1 - x$features.params)
  scores <- dataset %*% (logfeatures - lognfeatures)
  scores <- t(t(scores) + sumnegative + log(x$classes.params))
  
  return(scores)
}

# Classify using multinomial model
nbClassify.multinomial <- function(x, dataset) {  
  logfeatures <- log(x$features.params)
  
  # compute p(d, c)
  scores <- (dataset %*% (logfeatures))
  scores <- t(t(scores) + log(x$classes.params))
  colnames(scores) <- x$classes.names
  
  return(scores)
}

# Classify using poisson model
nbClassify.poisson <- function(x, dataset) {
  logfeatures <- log(x$features.params)
  
  # compute p(d, c)
  scores <- (dataset %*% (logfeatures))
  scores <- t(t(scores) + log(x$classes.params) - colSums(x$features.params))
  colnames(scores) <- x$classes.names
  
  return(scores)
}


# PrepareDataset makes the necessary changes in the 
# dataset before running the estimation
prepareDataset <- function(x, ...) UseMethod("prepareDataset")
prepareDataset.default <- function(x, dataset) return(dataset)
prepareDataset.bernoulli <- function(x, dataset) {
  dataset[dataset > 0] <- 1
  return(dataset)
}