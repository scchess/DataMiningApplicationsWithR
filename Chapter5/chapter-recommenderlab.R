### R code from vignette source 'chapter-recommenderlab.Rnw'

###################################################
### code chunk number 1: chapter-recommenderlab.Rnw:69-78
###################################################
# Load required library
library(recommenderlab)
library(ggplot2) # For plots

# Load the data we are going to work with
data(MovieLense)
MovieLense

# Visualizing a sample of this


###################################################
### code chunk number 2: chapter-recommenderlab.Rnw:83-84
###################################################
image(MovieLense, main = "Raw ratings") 


###################################################
### code chunk number 3: chapter-recommenderlab.Rnw:92-93
###################################################
summary(getRatings(MovieLense)) # Skewed to the right


###################################################
### code chunk number 4: chapter-recommenderlab.Rnw:98-101
###################################################
# Visualizing ratings
qplot(getRatings(MovieLense), binwidth = 1, 
      main = "Histogram of ratings", xlab = "Rating")


###################################################
### code chunk number 5: chapter-recommenderlab.Rnw:111-113
###################################################
qplot(getRatings(normalize(MovieLense, method = "Z-score")),
      main = "Histogram of normalized ratings", xlab = "Rating")


###################################################
### code chunk number 6: chapter-recommenderlab.Rnw:119-120
###################################################
summary(getRatings(normalize(MovieLense, method = "Z-score")))


###################################################
### code chunk number 7: chapter-recommenderlab.Rnw:126-131
###################################################
# How many movies did people rate on average
qplot(rowCounts(MovieLense), binwidth = 10, 
      main = "Movies Rated on average", 
      xlab = "# of users", 
      ylab = "# of movies rated") 


###################################################
### code chunk number 8: chapter-recommenderlab.Rnw:141-146
###################################################
# What is the mean rating of each movie
qplot(colMeans(MovieLense), binwidth = .1, 
      main = "Mean rating of Movies", 
      xlab = "Rating", 
      ylab = "# of movies") 


###################################################
### code chunk number 9: chapter-recommenderlab.Rnw:163-190
###################################################

recommenderRegistry$get_entries(dataType = "realRatingMatrix")
# We have a few options

# Split the data into train and test. Here, train is 90%.
# For testing it will take any 10 movie ratings by the user
# and predict n others. Then compare to see if they match.

scheme <- evaluationScheme(MovieLense, method = "split", train = .9,
                           k = 1, given = 10, goodRating = 4)

scheme
# Here we are using split, but other schemes are also available
# For production testing, I STRONGLY recommend using cross-validation scheme

# Let's check some algorithms against each other
algorithms <- list(
  "random items" = list(name="RANDOM", param=list(normalize = "Z-score")),
  "popular items" = list(name="POPULAR", param=list(normalize = "Z-score")),
  "user-based CF" = list(name="UBCF", param=list(normalize = "Z-score",
                                                 method="Cosine",
                                                 nn=50, minRating=3)),
  "item-based CF" = list(name="IBCF", param=list(normalize = "Z-score"
                                                 ))
  
  )



###################################################
### code chunk number 10: chapter-recommenderlab.Rnw:197-199
###################################################
# run algorithms, predict next n movies
results <- evaluate(scheme, algorithms, n=c(1, 3, 5, 10, 15, 20))


###################################################
### code chunk number 11: chapter-recommenderlab.Rnw:204-206
###################################################
# Draw ROC curve
plot(results, annotate = 1:4, legend="topleft")


###################################################
### code chunk number 12: chapter-recommenderlab.Rnw:214-216
###################################################
# See precision / recall
plot(results, "prec/rec", annotate=3)


###################################################
### code chunk number 13: chapter-recommenderlab.Rnw:230-268
###################################################
# Let's start with a regular matrix of 5 users, 10 items
set.seed(2358)
my.mat <- matrix(sample(c(as.numeric(-2:2), NA), 50,
                   replace=TRUE, 
                   prob=c(rep(.4/5,5),.6)), ncol=10, 
            dimnames=list(user=paste("u", 1:5, sep=''),
                          item=paste("i", 1:10, sep='')))

my.mat
# Here user u2 has rated i2 as 2 and i3 as 0. 
# Please note that 0 could be a valid value
# All unrated values are NA

# Convert this to realRatingMatrix
(my.realM <- as(my.mat, "realRatingMatrix"))

str(my.realM)
# Hmm, can we look at the underlying object?
rating.obj <- my.realM@data

# This is the class called sparse Matrix (notice the uppercase M)
# By default all 0s in Matrix are dropped to save space.
# Since we expect mostly NAs, it has taken our input mat, 
# and converted it to 0s. We can do this another way

dropNA(my.mat)

identical (rating.obj, dropNA(my.mat))

# OK, let's convert it back
as.matrix(rating.obj)

# This is wrong. We had NAs!
# What happened here is as.matrix applied to Matrix class, 
# and so it translated it zeroes instead of NAs.
# For the right translation, we need -
as (my.realM, "matrix")



###################################################
### code chunk number 14: SVD
###################################################
# At the time of writing this, you still have to create function .get_parameters, 
# but you can take the code from AAA.R so it is trivial. 

## helper functions and registry
# From AAA.R
.get_parameters <- function(p, parameter) {
    if(!is.null(parameter) && length(parameter) != 0) {
        o <- pmatch(names(parameter), names(p))

        if(any(is.na(o)))
        stop(sprintf(ngettext(length(is.na(o)),
                    "Unknown option: %s",
                    "Unknown options: %s"),
                paste(names(parameter)[is.na(o)],
                    collapse = " ")))

        p[o] <- parameter
    }

    p
}

# Now our new method using SVD
REAL_SVD <- function(data, parameter= NULL) {
  
  p <- .get_parameters(list(
    categories = 50,
    method="Cosine",
    normalize = "center",
    normalize_sim_matrix = FALSE,
    alpha = 0.5,
    treat_na = "0",
    minRating = NA
    ), parameter)
  
  # Do we need to normalize data?
  if(!is.null(p$normalize))
    data <- normalize(data, method=p$normalize)
  
  # Just save everything for now.
  model <- c(list(
    description = "full matrix",
    data = data
    ), p)
  
  predict <- function(model, newdata, n = 10,
                      type=c("topNList", "ratings"), ...) {
    
    type <- match.arg(type)
    n <- as.integer(n)
    
    # Do we need to denormalize?
    if(!is.null(model$normalize))
      newdata <- normalize(newdata, method=model$normalize)

    # Get the old data
    data <- model$data@data
    # Add new data to it to create combined matrix
    data <- rBind(data, newdata@data)
    
    ### svd does as.matrix which sets all missing values to 0!
    # So we have to treat missing values before we pass it to svd (fix by Michael Hahsler)
    data <- as(data, "matrix")

    if(model$treat_na=="min") data[is.na(data)] <- min(data, na.rm=TRUE)
    else if(model$treat_na=="mean") data[is.na(data)] <- mean(data, na.rm=TRUE)
    else if(model$treat_na=="median") data[is.na(data)] <- median(data, na.rm=TRUE)
    else if(model$treat_na=="max") data[is.na(data)] <- max(data, na.rm=TRUE)
    else if(model$treat_na=="0") data[is.na(data)] <- 0
    else stop("No valid way to treat NAs specified (treat_na)!")

    # Calculate SVD using available function
    s<-svd(data)
    
    # Get Diag but only of p elements
    S <- diag(s$d[1:p$categories])
    
    # Multiply it back up, but only using p elements
    ratings <- s$u[,1:p$categories] %*% S %*% t(s$v[,1:p$categories])
    
    # Put back correct names
    rownames(ratings) <- rownames(data)
    colnames(ratings) <- colnames(data)
    
    # Only need to give back new users
    ratings <- ratings[(dim(model$data@data)[1]+1):dim(ratings)[1],]
    
    # Convert to right type
    ratings <- new("realRatingMatrix", data=dropNA(ratings))
    ## prediction done
    
    ratings <- removeKnownRatings(ratings, newdata)
    
    if(!is.null(model$normalize))
      ratings <- denormalize(ratings)
    
    if(type=="ratings") return(ratings)
    
    getTopNLists(ratings, n=n, minRating=model$minRating)
    
  }
  
  ## construct recommender object
  new("Recommender", method = "SVD", dataType = class(data),
      ntrain = nrow(data), model = model, predict = predict)
}

# Add it to registry
recommenderRegistry$set_entry(
  method="SVD", dataType = "realRatingMatrix", fun=REAL_SVD,
  description="Recommender based on SVD approximation (real data).")



###################################################
### code chunk number 15: chapter-recommenderlab.Rnw:414-429
###################################################
algorithms <- list(
  "random items" = list(name="RANDOM", param=list(normalize = "Z-score")),
  "popular items" = list(name="POPULAR", param=list(normalize = "Z-score")),
  "user-based CF" = list(name="UBCF", param=list(normalize = "Z-score",
                                                 method="Cosine",
                                                 nn=50, minRating=3)),
  "item-based CF" = list(name="IBCF", param=list(normalize = "Z-score"
                                                 )),
  "SVD CF" = list(name="SVD", param=list(normalize = "Z-score",
                                                  treat_na = "0"
                                                 ))
  )

# run algorithms, predict next n movies
results <- evaluate(scheme, algorithms, n=c(1, 3, 5, 10, 15, 20))


###################################################
### code chunk number 16: chapter-recommenderlab.Rnw:434-436
###################################################
# Draw ROC curve
plot(results, annotate = 1:4, legend="topleft")


###################################################
### code chunk number 17: chapter-recommenderlab.Rnw:444-446
###################################################
# See precision / recall
plot(results, "prec/rec", annotate=3)


###################################################
### code chunk number 18: PCA
###################################################
REAL_PCA <- function(data, parameter= NULL) {
  
  p <- .get_parameters(list(
    categories = 20,
    method="Cosine",
    normalize = "center",
    normalize_sim_matrix = FALSE,
    alpha = 0.5,
    na_as_zero = FALSE,
    minRating = NA
    ), parameter)
  
  
  if(!is.null(p$normalize))
    data <- normalize(data, method=p$normalize)
  
  # Perform PCA
  data <- data@data
  
  # We will use princomp function, there are other methods available as well in R
  # princomp does an as.matrix as well but it does not matter in this case
  pcv<-princomp(data, cor=TRUE)
  
  # Get the loadings
  lpcv<-loadings(pcv)
  
  # Total number of categories
  cats <- min(dim(lpcv)[2], p$categories)
  
#   det(lpcv[,1:99] %*% t(lpcv[,1:99]))
  # This is just a check. If this is close to 1 that means we did well.
  
  # Convert to right type
  itemcat <- new("realRatingMatrix", 
                          data = as(lpcv[,1:cats], "dgCMatrix"))
  
  # Save the model
  model <- c(list(
    description = "PCA: Reduced item-category matrix",
    itemcat = itemcat
    ), p)
  
  predict <- function(model, newdata, n = 10,
                      type=c("topNList", "ratings"), ...) {
    
    type <- match.arg(type)
    n <- as.integer(n)
    
    if(!is.null(model$normalize))
      newdata <- normalize(newdata, method=model$normalize)
    
    ## predict all ratings
    u <- as(newdata, "dgCMatrix")
    itemcat <- as(model$itemcat, "dgCMatrix")
    ratings <- u %*% itemcat  %*% t(itemcat)
  
    ratings <- new("realRatingMatrix", data=dropNA(ratings),
                   normalize = getNormalize(newdata))
    ## prediction done
    
    ratings <- removeKnownRatings(ratings, newdata)
    
    if(!is.null(model$normalize))
      ratings <- denormalize(ratings)
    
    if(type=="ratings") return(ratings)
    
    getTopNLists(ratings, n=n, minRating=model$minRating)
    
  }
  
  ## construct recommender object
  new("Recommender", method = "PCA", dataType = class(data),
      ntrain = nrow(data), model = model, predict = predict)
}

# Add to registry
recommenderRegistry$set_entry(
  method="PCA", dataType = "realRatingMatrix", fun=REAL_PCA,
  description="Recommender based on PCA approximation (real data).")
  


###################################################
### code chunk number 19: chapter-recommenderlab.Rnw:550-576
###################################################
rm(MovieLense, scheme) # Clean up

data(Jester5k) # Load another dataset

scheme.jester <- evaluationScheme(Jester5k, method = "split", train = .9,
                           k = 1, given = 10, goodRating = 4)

scheme.jester

algorithms <- list(
  "random items" = list(name="RANDOM", param=list(normalize = "Z-score")),
  "popular items" = list(name="POPULAR", param=list(normalize = "Z-score")),
  "user-based CF" = list(name="UBCF", param=list(normalize = "Z-score",
                                                 method="Cosine",
                                                 nn=50, minRating=3)),
  "item-based CF" = list(name="IBCF", param=list(normalize = "Z-score"
                                                 )),
  "SVD CF" = list(name="SVD", param=list(normalize = "Z-score",
                                                  treat_na = "0"
                                                 )),
  "PCA CF" = list(name="PCA", param=list(normalize = "Z-score"
                                                 ))
  )

# run algorithms, predict next n movies
results <- evaluate(scheme.jester, algorithms, n=c(1, 3, 5, 10, 15, 20))


###################################################
### code chunk number 20: chapter-recommenderlab.Rnw:581-583
###################################################
# Draw ROC curve
plot(results, annotate = 1:4, legend="topleft")


###################################################
### code chunk number 21: chapter-recommenderlab.Rnw:591-593
###################################################
# See precision / recall
plot(results, "prec/rec", annotate=3)


###################################################
### code chunk number 22: chapter-recommenderlab.Rnw:668-807
###################################################
REAL_LRMF <- function(data, parameter= NULL) {
  
  p <- .get_parameters(list(
    categories = min(100, round(dim(data@data)[2]/2)),
    method="Cosine",
    normalize = "Z-score",
    normalize_sim_matrix = FALSE,
    minRating = NA,
    lambda = 1.5, # regularization
    maxit = 2000 # Number of iterations for optim
  ), parameter)
  
  if(!is.null(p$normalize))
    data <- normalize(data, method=p$normalize)
  
  model <- c(list(
    description = "full matrix",
    data = data
  ), p)
  
  predict <- function(model, newdata, n = 10,
                      type=c("topNList", "ratings"), ...) {
    
    type <- match.arg(type)
    n <- as.integer(n)
    
    if(!is.null(model$normalize))
      newdata <- normalize(newdata, method=model$normalize)
    
    # Get new data, make one Matrix object
    data <- model$data@data
    data <- rBind(data, newdata@data)

    Y <- t(data)
    
    # initialization
    # Users
    theta <- Matrix(runif(p$categories * dim(Y)[2]), ncol = p$categories)
    # Items
    X <- Matrix(runif(dim(Y)[1] * p$categories), ncol = p$categories)
    
    # We are going to scale the data so that optim converges quickly
    scale.fctr <- max(abs(Y@x))
    Y@x <- Y@x / scale.fctr
    
    # Let's optimize
    system.time(
      res <- optim(c(as.vector(X), as.vector(theta)),
                   fn = J_cost_full, gr = grad, 
                   Y=Y, lambda = model$lambda,
                   num_users = dim(theta)[1], num_books = dim(X)[1],
                   num_cats  = model$categories,
                   method = "CG", # Slow method, faster methods available
                   control = list(maxit=model$maxit, factr = 1e-2)
                   )
    )    
    
    print(paste("final cost: ", res$value, " convergence: ", res$convergence, 
                res$message, " counts: ", res$counts))
    
    X_final     <- unroll(res$par, num_users = dim(theta)[1], 
                          num_books = dim(X)[1], num_cats = p$categories)[[1]]
    theta_final <- unroll(res$par, num_users = dim(theta)[1], 
                          num_books = dim(X)[1], num_cats = p$categories)[[2]]
    
    Y_final <- (X_final %*% t(theta_final) ) 
    Y_final <- Y_final * scale.fctr
    dimnames(Y_final) = dimnames(Y)

    ratings <- t(Y_final)

    # Only need to give back new users
    ratings <- ratings[(dim(model$data@data)[1]+1):dim(ratings)[1],]
    
    ratings <- new("realRatingMatrix", data=dropNA(as.matrix(ratings)))
    ## prediction done
    
    ratings <- removeKnownRatings(ratings, newdata)
    
    if(!is.null(model$normalize))
      ratings <- denormalize(ratings)
    
    if(type=="ratings") return(ratings)
    
    getTopNLists(ratings, n=n, minRating=model$minRating)
    
  }
  
  ## construct recommender object
  new("Recommender", method = "LRMF", dataType = class(data),
      ntrain = nrow(data), model = model, predict = predict)
}

# Helper functions
unroll <- function (Vec, num_users, num_books, num_cats) {
  # Unroll the vector
  endIdx <- num_books * num_cats
  X <- Matrix(Vec[1:endIdx], nrow = num_books)
  theta <- Matrix(Vec[(endIdx + 1): (endIdx + (num_users * num_cats))], 
                  nrow = num_users)

  return (list(X, theta))  
}

J_cost_full <- function (Vec, Y, lambda, num_users, num_books, num_cats) {
  # Calculate the cost
  # Unroll the vector
  Vec.unrolled <- unroll(Vec, num_users, num_books, num_cats)
  X <- Vec.unrolled[[1]]
  theta <- Vec.unrolled[[2]]
  
  R          <-  as(Y, "nsparseMatrix") * 1 # Creates binary matrix
  Y_dash     <-  (X %*% t(theta) ) * R #(Y!=0)
  J_cost     <- .5 * (sum ((Y_dash - Y)^2) 
                      + lambda/2 * sum(X^2) 
                      + lambda/2 * sum(theta^2) )

  return (J_cost)
}

grad <- function (Vec, Y, lambda, num_users, num_books, num_cats) {
  # Unroll the vector
  Vec.unrolled <- unroll(Vec, num_users, num_books, num_cats)
  X <- Vec.unrolled[[1]]
  theta <- Vec.unrolled[[2]]
  
  # Calculate gradients
  R          <-  as(Y, "nsparseMatrix") * 1 # Creates binary matrix
  Y_dash     <-  (X %*% t(theta) ) * R #(Y!=0)
  X_gr       <-  (    (Y_dash - Y) * R ) %*% theta  + lambda * X    
  theta_grad <-  (  t((Y_dash - Y) * R ) %*% X      + lambda * theta) 
  
  return (c(as.vector(X_gr), as.vector(theta_grad)))
}

recommenderRegistry$set_entry(
  method="LRMF", dataType = "realRatingMatrix", fun=REAL_LRMF,
  description="Recommender based on Low Rank Matrix Factorization (real data).")



###################################################
### code chunk number 23: chapter-recommenderlab.Rnw:814-834
###################################################
algorithms <- list(
  "random items" = list(name="RANDOM", param=list(normalize = "Z-score")),
  "popular items" = list(name="POPULAR", param=list(normalize = "Z-score")),
  "user-based CF" = list(name="UBCF", param=list(normalize = "Z-score",
                                                 method="Cosine",
                                                 nn=50, minRating=3)),
  "item-based CF" = list(name="IBCF", param=list(normalize = "Z-score"
                                                 )),
  "SVD CF" = list(name="SVD", param=list(normalize = "Z-score",
                                                  treat_na = "0"
                                                 )),
  "PCA CF" = list(name="PCA", param=list(normalize = "Z-score"
                                                 )),
  "LRMF" = list(name="LRMF", param=list(normalize = "Z-score",
                                        maxit = 5000
                                                 ))
  )

# run algorithms, predict next n movies
results <- evaluate(scheme.jester, algorithms, n=c(1, 3, 5, 10))


###################################################
### code chunk number 24: chapter-recommenderlab.Rnw:839-841
###################################################
# Draw ROC curve
plot(results, annotate = 1:4, legend="topleft")


###################################################
### code chunk number 25: chapter-recommenderlab.Rnw:852-879
###################################################
rm(Jester5k, scheme.jester) # Clean up
data(MovieLense) # Load data

# Binarize
MovieLense.bin <- binarize(MovieLense, minRating = 3)

# What is available to us?
recommenderRegistry$get_entries(dataType = "binaryRatingMatrix")
# We have a few options

# Let's check some algorithms against each other
scheme.bin <- evaluationScheme(MovieLense.bin, 
                               method = "split", train = .9,
                               k = 1, given = 6) # Had to decrease given

scheme.bin

algorithms <- list(
  "random items" = list(name="RANDOM", param=NULL),
  "popular items" = list(name="POPULAR", param=NULL),
  "user-based CF" = list(name="UBCF", param=NULL),
  "item-based CF" = list(name="IBCF", param=NULL),
  "association rules CF" = list(name="AR", param=NULL)
  )

# run algorithms, predict next n movies
results.bin <- evaluate(scheme.bin, algorithms, n=c(1, 3, 5, 10, 15, 20))


###################################################
### code chunk number 26: chapter-recommenderlab.Rnw:884-886
###################################################
# Draw ROC curve
plot(results.bin, annotate = 1:4, legend="topleft")


###################################################
### code chunk number 27: fig14
###################################################
# See precision / recall
plot(results.bin, "prec/rec", annotate=3)


###################################################
### code chunk number 28: chapter-recommenderlab.Rnw:908-958
###################################################
#######################
# Data Aquisition
#######################

# Name of download file
temp <- tempfile(fileext = ".zip")

# Get the file from http://www.informatik.uni-freiburg.de/~cziegler/BX/
download.file("http://www.informatik.uni-freiburg.de/~cziegler/BX/BX-CSV-Dump.zip", temp)

# Read in bookratings
bookratings <- read.csv(unz(temp, "BX-Book-Ratings.csv"),
                        header=FALSE, sep = ';', 
                        stringsAsFactors = FALSE, skip = 1,
                        col.names = c("User.ID", "ISBN", "Book.Rating"))

# Not used here, provided to peak your curiosity
# users <- read.csv(unz(temp, "BX-Users.csv"),
#                   header=FALSE, sep = ';', 
#                   stringsAsFactors = FALSE, skip = 1,
#                   col.nam = c("User-ID", "Location", "Age"))                  

# Are there any duplicates?
bookratings[duplicated(bookratings)] 

# No duplicate ratings

# Read the book names
books <- read.csv(unz(temp, "BX-Books.csv"),
                  header=FALSE, sep = ';', 
                  stringsAsFactors = FALSE, skip = 1,
                  col.names = c("ISBN", "Book-Title", "Book-Author", 
                                "Year-Of-Publication", "Publisher", 
                                "Image-URL-S", "Image-URL-M", "Image-URL-L"))


#######################
# Data Wrangling
#######################

# Merge the two datasets
bookratings.dtl <- merge(bookratings, books, on = ISBN)
bookratings.dtl$Book.detail <- with(bookratings.dtl, paste(ISBN, Book.Title, Book.Author, sep="::"))

# We only need these fields
bookratings.dtl <- bookratings.dtl[, c("User.ID", "Book.detail", "Book.Rating")]

# Convert it to a realRatingMatrix
(bookratings.r <- as(bookratings.dtl, "realRatingMatrix"))



###################################################
### code chunk number 29: chapter-recommenderlab.Rnw:963-972
###################################################
# Look at the distribution

# Books were rated by how many users?
qplot(as.vector(colCounts(bookratings.r)), 
      binwidth=100,
      main = "How many rated a book?", 
      xlab = "Book", 
      ylab = "# of raters")



###################################################
### code chunk number 30: chapter-recommenderlab.Rnw:980-995
###################################################

# Taking the 1000 most rated books (approx.)
colIdx <- colCounts(bookratings.r)
# Seeing the cutoff value here
sort(colIdx, decreasing = TRUE)[1000]

# using cutoff threshold
bookratings.r@data <- bookratings.r@data[, which(colIdx >= 24)]
bookratings.r

# Let's also cut down on number of users
# We are going to evaluate on users by giving the model 5 things they have rated
# And ask to predict the next 5
# So we need to have atleast 10 ratings per user
summary(rowCounts(bookratings.r))


###################################################
### code chunk number 31: chapter-recommenderlab.Rnw:1000-1006
###################################################
rowIdx <- rowCounts(bookratings.r)
qplot(rowIdx, binwidth = 10, 
      main = "Books Rated on average", 
      xlab = "# of users", 
      ylab = "# of books rated")



###################################################
### code chunk number 32: chapter-recommenderlab.Rnw:1012-1039
###################################################
# If 5 are given and 5 are predicted, need to remove <10
length(id2remove <- which(rowIdx < 10))
bookratings.r <- bookratings.r[-1*id2remove]

# Final realRatingMatrix
bookratings.r

#######################
# Model Evaluation
#######################

scheme <- evaluationScheme(bookratings.r, method="cross-validation", goodRating=5,
                           k=2, given=10)


algorithms <- list(
  "random items" = list(name="RANDOM", param=NULL),
  "popular items" = list(name="POPULAR", param=NULL),
  "user-based CF" = list(name="UBCF", param=list(method="Cosine",
                                                 nn=10, minRating=1)),
  "Item-based CF" = list(name = "IBCF", param = list(normalize="Z-score")),
  "LRMF (100 categories)" = list(name = "LRMF", param = list(categories=100, 
                                                             normalize="Z-score"))
)

results <- evaluate(scheme, algorithms, n=c(1, 3, 5, 10))



###################################################
### code chunk number 33: fig17
###################################################
plot(results, annotate=c(1,3), legend="topleft")


