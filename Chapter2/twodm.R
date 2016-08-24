require(Matrix)
require(ggplot2)
require(gWidgetsRGtk2)
options("guiToolkit"="RGtk2")
require(cairoDevice)
source("nb.R")
source("preprocess_dataset.R")

twodm <- function(dataset, labels)
{ 
  # Handlers
  # Create a classification model
  hndleCreateModel <- function (h, ...) {
    
    # Get selected model
    model <- svalue(combo.model)
    # Get selected smoothing method
    smoothing <- svalue(combo.smooth)
    
    # Delete parameters layouts
    for (w in list.layouts) {
      delete(frm.params, w)
    }
    
    # Build widgets according to prior and model
    if (smoothing == "prior") {
      if (model == "bernoulli") {
        names(frm.params) <- "Adjust Beta Prior"
        add(frm.params, list.layouts$bernprior)
      } else if (model == "poisson") {
        names(frm.params) <- "Adjust Gamma Prior"
        add(frm.params, list.layouts$bernprior)
      } else { 
        names(frm.params) <- "Adjust Dirichlet Prior"
        add(frm.params, list.layouts$multiprior)
      }
    } else if (smoothing == "interpolation") { 
      names(frm.params) <- "Adjust Interpolation Factor"
      add(frm.params, list.layouts$interp)
    }
    
    # Initialize parameters
    params <<- lapply(list.sliders, svalue)

    # Create the model
    nbmodel <<- nb(model, smoothing)
    nbmodel <<- nbEstimate(nbmodel, dataset, labels, params)
    
    # Update everything
    fireUpdate()
    
    # Update status bar value
    svalue(status.bar) <- paste("Parametric model:", model, "/ Smoothing:", smoothing)
  }
  
  # Update parameters, plot and performances
  fireUpdate <- function() {
    # Update the value of the parameters
    params  <<- lapply(list.sliders, svalue)

    # Update the model
    nbmodel <<- nbUpdate(nbmodel, params)
    # Update scores
    scores  <<- nbClassify(nbmodel, dataset)
    # Update plot and performances
    updatePlot()
    updatePerformances()
  }
  
  # Update the plot
  updatePlot <- function(...) {
    # Update dataframe
    df$x <<- scores[, 1]
    df$y <<- scores[, 2]
    
    pp <- p
    pp <- (pp %+% df)
    # Add layers 
    if (svalue(chk.points))
    {
      pp <- pp + geom_point(alpha = 0.2)
    }
    if (svalue(chk.smooth))
    {
      pp <- pp + geom_smooth()
    }
    # Add decision line
    pp <- pp + geom_abline(colour = "green")
    
    # Adjust coordinate
    pp <- pp + coord_cartesian(xlim = c(svalue(slider.coord), 0),
                               ylim = c(svalue(slider.coord), 0))
    # Plot
    print(pp)
  }
  
  # Update the actual performance
  updatePerformances <- function() {
    # Get the positive scores
    p <- scores[which(labels == "pos"), ]
    # Get the negative scores
    n <- scores[which(labels == "neg"), ]
    # Compute correct and wrong decisions
    true_positive  <- sum(p[, 1] > p[, 2])
    false_positive <- sum(n[, 1] > n[, 2])
    
    # Compute Recall, Precision and F1 measures    
    recall    <- true_positive / dim(p)[1]
    precision <- true_positive / (true_positive + false_positive)
    f1        <- (2 * recall * precision) / (recall + precision)

    # Update table
    delete(frm.performances, performances.table)
    
    # Create metrics object
    metrics            <- list()
    metrics$model      <- nbmodel$type
    metrics$smoothing  <- nbmodel$smoothing
    metrics$alpha      <- params$alpha
    metrics$beta       <- params$beta
    metrics$lambda     <- params$lambda
    metrics$mu         <- params$mu
    metrics$recall     <- round(recall, digits=3)
    metrics$precision  <- round(precision, digits=3)
    metrics$f1         <- round(f1, digits=3)
    
    
    # Update performance and number of tests
    performances <<- rbind(performances, data.frame(metrics))    
    performances.table <<- gtable(items = performances,
                                  container = frm.performances,
                                  expand = TRUE)
  }
  
  # Naive Bayes models
  choices.model <- c("bernoulli", "multinomial", "poisson")
  # Smoothing type
  choices.smooth <- c("prior", "interpolation", "laplace")
  # Load the dataset
  # dataset <- loadDataset(dataset)
  # Load the labels of a category
  # labels <- loadLabels(labels)
  
  # Create the window that contains the widgets
  win.ctrls <- gwindow("Two Dimensional Probabilistic Model", expand = TRUE)
  # Group of objects
  grp.all <- ggroup(container = win.ctrls, horizontal = FALSE)
  # Group of control widget
  grp.expand <- ggroup(container = grp.all, expand = TRUE)
  grp.ctrls <- ggroup(container = grp.expand, horizontal = FALSE)
  grp.grph <- ggroup(container = grp.expand, horizontal = FALSE, expand = TRUE)
  
  # Create sliders and layouts index
  list.layouts <- list()
  list.sliders <- list()
  
  # Model Settings
  frm.newmodel <- gframe("Create new model",
                         expand    = FALSE)
  layout.newmodel <- glayout(container = frm.newmodel,
                             anchor   = c(0, 0))
  layout.newmodel[1, 1, anchor = c(0, 0)] <- "Parametric Distribution:"
  layout.newmodel[1, 2] <- (combo.model <- gcombobox(choices.model,
                                                     editable  = FALSE,
                                                     container = layout.newmodel))
  layout.newmodel[2, 1, anchor = c(0, 0)] <- "Smoothing Method:"
  layout.newmodel[2, 2] <- (combo.smooth <- gcombobox(choices.smooth,
                                                      editable  = FALSE,
                                                      container = layout.newmodel))
  layout.newmodel[3, 2] <- gbutton("Create model",
                                   handler = hndleCreateModel)
  
  frm.params <- gframe(expand=FALSE, horizontal=FALSE)
  button.params <- gbutton("Estimate model", container = frm.params,
                           handler = function(...) {
                             fireUpdate()
                           })
  list.layouts$bernprior = glayout(expand = TRUE)
  list.layouts$bernprior[1, 1, anchor = c(0,0)] <- "Alpha:"
  list.layouts$bernprior[1, 2, expand = TRUE] <- (list.sliders$alpha <- gslider(
    from = 0.1,
    to = 1,
    by = 0.1,
    value = 1,
    container = list.layouts$bernprior,
    expand = TRUE
  ))
  # Create beta slider for Beta Prior
  list.layouts$bernprior[2, 1, anchor = c(0,0)] <- "Beta:"
  list.layouts$bernprior[2, 2, expand = TRUE] <- (list.sliders$beta <- gslider(
    from = 0.1,
    to = 700,
    by = 0.1,
    value = 1,
    container = list.layouts$bernprior,
    expand = TRUE,
  ))
  # Create lambda slider for Jelinek-Mercer interpolation
  list.layouts$interp = glayout( expand = TRUE)
  list.layouts$interp[1, 1, anchor = c(0,0)] <- "Lambda:"
  list.layouts$interp[1, 2, expand = TRUE] <- (list.sliders$lambda <- gslider(
    from = 0.01,
    to = 1,
    by = 0.01,
    value = 0.5,
    container = list.layouts$interp,
    expand = TRUE
  ))
  # Create mu slider for Dirichlet Prior
  list.layouts$multiprior = glayout(expand = TRUE)
  list.layouts$multiprior[1, 1, anchor = c(0,0)] <- "Mu:"
  list.layouts$multiprior[1, 2, expand = TRUE] <- (list.sliders$mu <- gslider(
    from = 0.1,
    to = 1,
    by = 0.1,
    value = 1,
    container = list.layouts$multiprior,
    expand = TRUE
  ))
    
  # Change geometries
  frm.geoms <- gframe(
    "Add/remove geoms",
    expand    = FALSE
  )  
  # Add checkbox to plot points
  chk.points <- gcheckbox(
    "Points", 
    checked   = TRUE,
    container = frm.geoms,
    handler   = function(...) { updatePlot() }
  )
  # Add checkbox to smooth points
  chk.smooth <- gcheckbox(
    "Smooth curve",
    checked   = FALSE,
    container = frm.geoms,
    handler   = function(...) { updatePlot() }
  )
  
  # Change coordinates
  frm.coord <- gframe(
    "Zoom coordinates",
    expand    = FALSE
  )
  slider.coord <- gslider(from      = -2000,
                          to        =  -200,
                          by        =   100,
                          value     = -1000,
                          container = frm.coord,
                          expand    = TRUE,
                          handler   = function(h, ...) {
                            p <- last_plot() + coord_cartesian(
                              xlim = c(svalue(h$obj), 0),
                              ylim = c(svalue(h$obj), 0))
                            print(p)
                          })
  
  # Create a statistics frame
  frm.dataset <- gframe("Dataset properties", expand = TRUE)
  layout.dataset <- glayout(container = frm.dataset, expand = TRUE)
  
  # Create a default metrics object
  metrics            <- list()
  metrics$model      <- character()
  metrics$smoothing  <- character()
  metrics$alpha      <- numeric()
  metrics$beta       <- numeric()
  metrics$lambda     <- numeric()
  metrics$mu         <- numeric()
  metrics$recall     <- numeric()
  metrics$precision  <- numeric()
  metrics$f1         <- numeric()
  
  # Create performance list
  performances <- data.frame(metrics, stringsAsFactors=FALSE)
  # Performance frame
  frm.performances   <- gframe("Performance", expand = TRUE)
  performances.table <- gtable(items = performances,
                               container = frm.performances,
                               expand = TRUE)
  
  # Create the dataframe to plot results
  df <- data.frame(x = rep(0, dim(dataset)[1]), y = rep(0, dim(dataset)[1]), labels)
  p <- ggplot(df, aes(x, y, colour = labels)) + 
    geom_abline(colour = "green") + 
    coord_cartesian(xlim = c(-2000, 0), ylim = c(-2000, 0)) +
    theme(panel.grid.major = element_line(colour = "gray"),
         panel.grid.minor = element_blank(),
         panel.background = element_blank())
  
  # Create a status bar
  status.bar <- gstatusbar("")
  
  # Add components to the graphics group
  add(grp.grph, frm.geoms)
  add(grp.grph, frm.coord)
  add(grp.grph, ggraphics())
  
  # Add components to the control group
  add(grp.ctrls, frm.newmodel)
  add(grp.ctrls, frm.params)
  
  # Add performances to all
  add(grp.all, frm.performances)

  # Add status bar
  add(win.ctrls, status.bar)
    
  # Start the first model
  hndleCreateModel()
  
}