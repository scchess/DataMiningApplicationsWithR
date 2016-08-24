rm(list=ls())
library(RODBC)
library(RSQLite)
library(randomForest)
library(varSelRF)

m <- dbDriver("SQLite")
con <- dbConnect(m, dbname = "parsian3.dbms")
customertargetselectedscale2data=dbGetQuery(con, "select * from customertargetselectedscale2")

customertargetselectedscale2data=(subset(customertargetselectedscale2data, select=-ID))
customertargetselectedscale2data$target<- factor(customertargetselectedscale2data$target)



featureselect <- varSelRF(subset(customertargetselectedscale2data, select=-target),customertargetselectedscale2data$target,
                          mtryFactor = 1 , ntree = 2000,ntreeIterat = 1000, 
                          vars.drop.frac = 0.2,whole.range =FALSE, verbose = TRUE)




featureselect
plot(featureselect)


