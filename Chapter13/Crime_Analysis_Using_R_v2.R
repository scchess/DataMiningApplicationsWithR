
##########################################################################################
# File-Name:       DataMiningApplicationsWithR_Crime_Analysis_Using_R_v2                 #
# Date:            30-Nov-2012                                                           #
# Author:          Madhav Kumar                                                          #
# Email:           madhavkumar2005@gmail.com                                             #
# Data Used:       1. Crime data for city of Chicago available from their                #
#                     open data portal at: https://data.cityofchicago.org/               #
#                  2. Shape files for Chicago                                            # 
# Packages Used:   animation, chron, doBy, ggplot2, maps, maptools, MASS, reshape2       #
##########################################################################################

##########################################################################################
# This script downloads, cleans, processes, visualizes, models on crime data of Chicago 
# available from the city of Chicago's open data portal.

# It contents have primarily been written for the book: Data Mining Applications with R
# by Elsevier.
##########################################################################################


##########################################
###########    Section 3    ##############
########### Data extraction ##############
##########################################


# Download one year of crime data from the open data portal of city of Chicago
# NOTE: This may take a while depening on the strength of your internet connection
url.data <- "https://data.cityofchicago.org/api/views/x2n5-8w5q/rows.csv?accessType=DOWNLOAD"
crime.data <- read.csv(url.data, na.strings= '')

# Write data to csv file for further use
write.csv(crime.data, "crime.data.csv", row.names = FALSE)


##########################################
###########    Section 4    ##############
########### Data exploration #############
##########################################


# Understanding the data fields
str(crime.data)

# Summarize the data
summary(crime.data)

# CASE ID is the unique identifier for each crime incident.
# There are multiple instance of the same case id
# These need to be removed
crime.data <- subset(crime.data, !duplicated(crime.data$CASE.))
summary(crime.data)

# Remove NAs
crime.data <- subset(crime.data, !is.na(crime.data$LATITUDE))
crime.data <- subset(crime.data, !is.na(crime.data$WARD))
summary(crime.data)


# Remove observations where CASE. == case#
crime.data <- crime.data[crime.data$CASE. != 'CASE#',]
crime.data <- crime.data[crime.data$PRIMARY.DESCRIPTION != "PRIMARY",]

summary(crime.data)

# Remove observation where all fields are NA
which(is.na(crime.data$LOCATION))
crime.data <- crime.data[-which(is.na(crime.data$LOCATION)), ]

summary(crime.data)

# Save file as rdata file for faster loading
save(crime.data, file= 'crime.data.rdata')


###################### 
### Time dimension ###
######################


# Date of occurance 
head(crime.data$DATE..OF.OCCURRENCE)
tail(crime.data$DATE..OF.OCCURRENCE)

# In case time is the following format: 05:30 PM
crime.data$date <- as.POSIXlt(crime.data$DATE..OF.OCCURRENCE, format= "%m/%d/%Y %I:%M %p")

# In case time is the following format: 1730
# crime.data$Date <- as.POSIXlt(crime.data$DATE..OF.OCCURRENCE, format= "%m/%d/%Y %H:%M")

head(crime.data$date)
tail(crime.data$date)


# Remove timestamp from datetime and place in separate column
library(chron)
crime.data$time <- times(format(crime.data$date, format= "%H:%M:%S"))
head(crime.data$time)

# Create 6 hour time window to analyze crime
time.tag <- chron(times = c("00:00:00", "06:00:00", "12:00:00", "18:00:00", "23:59:00"))
time.tag

# Bin crime times to the 4 hour time-crime windows
crime.data$time.tag <- cut(crime.data$time, breaks=time.tag, 
                           labels=c("00-06","06-12", "12-18", "18-00"), 
                           include.lowest=TRUE)
table(crime.data$time.tag)

# Convert Date.OF.OCCURRENCE to date
crime.data$date <- as.POSIXlt(strptime(crime.data$date, format= "%Y-%m-%d"))
head(crime.data$date)


# Day of the week on which the crime occured
crime.data$day <- weekdays(crime.data$date, abbreviate= TRUE)
table(crime.data$day)

# Month in which the crime occured
crime.data$month <- months(crime.data$date, abbreviate= TRUE)
table(crime.data$month)

# Now that we have processed the date, we can remove the field Date.of.Occurrence
crime.data <- crime.data[,-2]

# Save file
save(crime.data, file= 'crime.data.rdata')

##################### 
## Crime dimension ##
#####################

# Specific crime types
table(crime.data$PRIMARY.DESCRIPTION)

# The data contains 30 crime types; not all of which are mutually exclusive
# Some categories can be combined to reduce this number

crime.data$crime <- as.character(crime.data$PRIMARY.DESCRIPTION)

crime.data$crime <- ifelse(crime.data$crime %in% c("CRIM SEXUAL ASSAULT", "PROSTITUTION", "SEX OFFENSE"), 
                           'SEX', crime.data$crime)

crime.data$crime <- ifelse(crime.data$crime %in% c("MOTOR VEHICLE THEFT"), "MVT", 
                           crime.data$crime)

crime.data$crime <- ifelse(crime.data$crime %in% c("GAMBLING", "INTERFERE WITH PUBLIC OFFICER",
                                                   "INTERFERENCE WITH PUBLIC OFFICER", "INTIMIDATION",
                                                   "LIQUOR LAW VIOLATION", "OBSCENITY", "NON-CRIMINAL",
                                                   "PUBLIC PEACE VIOLATION",  "PUBLIC INDECENCY", 
                                                     "STALKING", "NON-CRIMINAL (SUBJECT SPECIFIED)"), "NONVIO", crime.data$crime)

crime.data$crime <- ifelse(crime.data$crime == "CRIMINAL DAMAGE", "DAMAGE", crime.data$crime)

crime.data$crime <- ifelse(crime.data$crime == "CRIMINAL TRESPASS", "TRESPASS", 
                           crime.data$crime)

crime.data$crime <- ifelse(crime.data$crime %in% c("NARCOTICS", "OTHER NARCOTIC VIOLATION"), "DRUG", 
                           crime.data$crime)

crime.data$crime<- ifelse(crime.data$crime == "DECEPTIVE PRACTICE", "FRAUD", 
                          crime.data$crime)

crime.data$crime <- ifelse(crime.data$crime %in% c("OTHER OFFENSE", "OTHER OFFENSE "), "OTHER", 
                           crime.data$crime)

crime.data$crime <- ifelse(crime.data$crime %in% c("KIDNAPPING", "WEAPONS VIOLATION", 
                                                   "OFFENSE INVOLVING CHILDREN"), 
                           "VIO", crime.data$crime)
table(crime.data$crime)

# Further combination into violent and non-violent crime types
crime.data$crime.type <- ifelse(crime.data$crime %in% c("SEX", "ARSON", "ASSAULT", "HOMICIDE", "VIO", 
                                                        "BATTERY"), 
                                "VIO", ifelse(crime.data$crime %in% c("BURGLARY", "DAMAGE", "DRUG", "FRAUD",
                                                                      "MVT", "NONVIO", "ROBBERY", "THEFT", 
                                                                      "TRESPASS"),
                                              "NONVIO", "Other"))
table(crime.data$crime.type)

# Convert arrest to binary numeric variable
crime.data$ARREST <- ifelse(as.character(crime.data$ARREST) == "Y", 1, 0)

# Keep only the required columns
crime.data <- crime.data[, c('CASE.', 'ARREST', 'DOMESTIC', 'BEAT', 
                             'WARD', 'FBI.CD', 'X.COORDINATE', 'Y.COORDINATE', 'LATITUDE', 
                             'LONGITUDE', 'date', 'time', 'time.tag', 'day', 'month', 'crime', 'crime.type')]


# Save file
save(crime.data, file= 'crime.data.rdata')




################## 
## Simple Plots ##
##################

library(ggplot2)

# Frequency of crime
qplot(crime.data$crime, xlab = "Crimes", main ="Crimes in Chicago") + 
  scale_y_continuous("Number of crimes" )



# Time of day
qplot(crime.data$time.tag, xlab="Time of day", main = "Crimes by time of day") +
  scale_y_continuous("Number of crimes")


# Day of week
crime.data$day <- factor(crime.data$day, levels= c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))

qplot(crime.data$day, xlab= "Day of week", main= "Crimes by day of week") + 
  scale_y_continuous("Number of crimes") 

# Month
crime.data$month <- factor(crime.data$month, levels= c("Jan", "Feb", "Mar", "Apr", 
                                                       "May", "Jun", "Jul", "Aug", 
                                                       "Sep", "Oct", "Nov", "Dec"))

qplot(crime.data$month, xlab= "Month", main= "Crimes by month") + 
  scale_y_continuous("Number of crimes")



################### 
#### Heat Maps ####
###################


# Crime type by time of day
temp <- aggregate(crime.data$crime, by= list(crime.data$crime, crime.data$time.tag), 
                  FUN= length)
names(temp) <- c("crime", "time.tag", "count")

ggplot(temp, aes(x= crime, y= factor(time.tag))) +
  geom_tile(aes(fill= count)) + scale_x_discrete("Crime", expand = c(0,0)) +
  scale_y_discrete("Time of day", expand = c(0,-2)) +
  scale_fill_gradient("Number of crimes", low = "white", high = "steelblue") + 
  theme_bw() + ggtitle("Crimes by time of day") +
  theme(panel.grid.major = element_line(colour = NA), panel.grid.minor = element_line(colour = NA))



# Crimes by day of the week
# Another way of aggregating data is using the ddply function from plyr
library(plyr)
temp <- ddply(crime.data, .(crime, day), summarise, count = length(cASE.))

ggplot(temp, aes(x= crime, y= day, fill= count)) +  
  geom_tile(aes(fill= count)) +  
  scale_x_discrete("Crime", expand = c(0,0)) +  
  scale_y_discrete("Day of week", expand = c(0,-2)) +
  scale_fill_gradient("Number of crimes", low = "white", high = "steelblue")+ 
  theme_bw() + ggtitle("Crimes by day of week") +
  theme(panel.grid.major = element_line(colour = NA), panel.grid.minor = element_line(colour = NA))


# Crimes by month
# A third way of aggregating data is using the summaryBy function from the doBy package
library(doBy)

temp <- summaryBy(CASE. ~ crime + month, data= crime.data, FUN= length)
names(temp)[3] <- 'count'

ggplot(temp, aes(x= crime, y= month, fill= count)) +  
  geom_tile(aes(fill= count)) +  
  scale_x_discrete("Crime", expand = c(0,0)) +  
  scale_y_discrete("Month", expand = c(0,-2)) +
  scale_fill_gradient("Number of crimes", low = "white", high = "steelblue")+ 
  theme_bw() + ggtitle("Crimes by Month") +
  theme(panel.grid.major = element_line(colour = NA), panel.grid.minor = element_line(colour = NA))



###################################### 
### Simple plot on map  of Chicago ###
######################################


# Aggregate data for plotting it on maps
# Use the ddply function from plyr
# Any of the techniques above can be used
crime.agg <- ddply(crime.data, .(crime, crime.type, ARREST, BEAT, WARD, FBI.CD, 
                                             date, X.COORDINATE, 
                                             Y.COORDINATE, time.tag, day, month), 
                               summarise, count = length(date), .progress= 'text')



###########################
# Police beat shape files #
###########################

library(maptools)
beat.shp <- readShapePoly("PoliceBeats/PoliceBeat.shp")

source('fortify.R')
beat.shp.df <- fortify.SpatialPolygons(beat.shp)


##############################
# Police station shape files #
##############################

station.shp <- readShapePoly("Police_20Stations-2/police_stations.shp") 


# Get police station information
getPoliceData <- function(shp.file){
  PoliceData <- data.frame()
  NumOfStations = nrow(shp.file@data)
  for(ii in 1:NumOfStations){
    PoliceData[ii, 1] <- shp.file@data$DESCRIPTIO[ii]
    PoliceData[ii, 2] <- shp.file@polygons[[ii]]@labpt[1]
    PoliceData[ii, 3] <- shp.file@polygons[[ii]]@labpt[2]
  }
  names(PoliceData) <- c("description", "lat", "long")
  return(PoliceData)
}

police.data <- getPoliceData(station.shp)


# convert coordinates to numeric
crime.agg$X.COORDINATE <- as.numeric(crime.agg$X.COORDINATE)
crime.agg$Y.COORDINATE <- as.numeric(crime.agg$Y.COORDINATE)


# simple way
plot(beat.shp)

temp <- subset(crime.agg, date == crime.agg$date[1])
points(temp$X.COORDINATE, temp$Y.COORDINATE, pch= 20, col= "red")

points(PoliceData$lat, PoliceData$long, pch = 18, col = "blue")
pointLabel(PoliceData$lat, PoliceData$long, labels = PoliceData$description, method = "SANN")



# the cool ggplot way

# crimes for one day
back.plot <- ggplot(beat.shp.df, aes(x=long, y=lat)) + geom_path(aes(group = group)) + theme_bw()  +
  geom_point(data= PoliceData, aes(x= lat, y= long))

today <- crime.data[1, 'date']

back.plot <- back.plot + geom_point(data= subset(crime.agg, as.character(crime.agg$date) == today) , 
                                    aes(x=X.COORDINATE, y= Y.COORDINATE, color= crime)) 

back.plot <- back.plot + theme_bw() + 
  ggtitle(paste("Crime in Chicago on", weekdays(today), ",", months(today), substr(today, 9, 12), substr(today, 1, 4))) +
  theme(panel.grid.major= element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(),
       axis.title.x= element_blank(), axis.title.y= element_blank(),
       axis.text.x= element_blank(), axis.text.y= element_blank(), axis.ticks= element_blank())

back.plot


# animation of crimes
back.plot <- ggplot(beat.shp.df, aes(x=long, y=lat)) + geom_path(aes(group = group)) + theme_bw() +
  geom_point(data= PoliceData, aes(x= lat, y= long))

crime.plot <- function(dates){
  for (today in dates){
    date.crime <- crime.agg[as.character(crime.agg$date) == today, ]
    today <- as.Date(today, origin = "1970-01-01")
    
    date.plot <- back.plot + theme_bw() + 
      ggtitle(paste("Crime in Chicago on", weekdays(today), ",", months(today), substr(today, 9, 12), substr(today, 1, 4))) +
      theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(), panel.border= element_blank(),
           axis.title.x= element_blank(), axis.title.y= element_blank(),
           axis.text.x= element_blank(), axis.text.y= element_blank(), axis.ticks=element_blank())
    
    date.plot <- date.plot + geom_point(data= date.crime, aes(x=X.COORDINATE, y= Y.COORDINATE, color= crime)) +
      scale_size(guide= "none", range= c(1.5,2.5)) + 
      scale_alpha(guide= "none", range=c(0.3,0.5)) + scale_colour_discrete(name="Type of Crime") + 
      xlab("") + ylab("")
    
    plot(date.plot)
  }
}

# plot for all the dates in crime.agg
dates <- as.character(unique(crime.agg$date))


library(animation)
saveMovie(crime.plot(dates), movie.name = "chicago_crime_animation.gif", 
          img.name = "Rplot", convert = "convert", 
          cmd.fun = system, clean = TRUE, outdir= getwd(), interval = 1, width = 580, height = 400)




##########################################
###########    Section 4    ##############
########### Predictive Model #############
##########################################


# Create a base data set that contains unique combination of all beats and dates
# The number of rows for the base data will be the product of 
length(unique(crime.agg$BEAT))
# and
length(unique(crime.agg$date))

# Unqiue beats from crime data
beats <- sort(unique(crime.agg$BEAT))

# Unqiue dates from crime data
dates <- sort(as.character(unique(crime.agg$date)))

# Create base data
temp <- expand.grid(beats, dates)
names(temp) <- c("BEAT", "date")

# order base data
library(doBy)
temp <- orderBy(~ BEAT, data= temp)

# Aggregate crime.agg data to get one row for beat-date combination
model.data <- aggregate(crime.agg[, c('count', 'ARREST')], by= list(crime.agg$BEAT, as.character(crime.agg$date)), 
                        FUN= sum)
names(model.data) <- c("BEAT", "date", "count", "ARREST")

# Overlap base data and crime.agg data
# For beat-date combinations missing in incident data, we will get NAs
model.data <- merge(temp, model.data, by= c('BEAT', 'date'), all.x= TRUE)

# Get day of the week from date to incorporate intra-week trends
model.data$day <- weekdays(as.Date(model.data$date), abbreviate= TRUE)

# Get month from date to incorporate trends over a month 
model.data$month <- months(as.Date(model.data$date), abbreviate= TRUE)
model.data$count[is.na(model.data$count)] <- 0
model.data$ARREST[is.na(model.data$ARREST)] <- 0

# Get crime history for each beat

# Funtion to get history of past x days
pastDays <- function(x){
  c(0, rep(1, x))
}


# Yesterday
model.data$past.crime.1 <- ave(model.data$count, model.data$BEAT, 
                               FUN= function(x) filter(x, pastDays(1), sides= 1))

# Past 7 days
model.data$past.crime.7 <- ave(model.data$count, model.data$BEAT, 
                               FUN= function(x) filter(x, pastDays(7), sides= 1))

# Past 30 days
model.data$past.crime.30 <- ave(model.data$count, model.data$BEAT, 
                                FUN= function(x) filter(x, pastDays(30), sides= 1))

# Funtion to calculate mean of variable with missing values
meanNA <- function(x){mean(x, na.rm= TRUE)}

# Replace missing values in each column
model.data$past.crime.1 <- ifelse(is.na(model.data$past.crime.1), meanNA(model.data$past.crime.1), 
                                  model.data$past.crime.1)

model.data$past.crime.7 <- ifelse(is.na(model.data$past.crime.7), meanNA(model.data$past.crime.7), 
                                  model.data$past.crime.7)

model.data$past.crime.30 <- ifelse(is.na(model.data$past.crime.30), meanNA(model.data$past.crime.30), 
                                   model.data$past.crime.30)


# Arrests history

# Yesterday
model.data$past.arrest.1 <- ave(model.data$ARREST, model.data$BEAT, 
                                FUN= function(x) filter(x, pastDays(1), sides= 1))

# Past 7 days
model.data$past.arrest.7 <- ave(model.data$ARREST, model.data$BEAT, 
                                FUN= function(x) filter(x, pastDays(7), sides= 1))

# Past 30 days
model.data$past.arrest.30 <- ave(model.data$ARREST, model.data$BEAT, 
                                 FUN= function(x) filter(x, pastDays(30), sides= 1))

# Replace missing values in each column
model.data$past.arrest.1 <- ifelse(is.na(model.data$past.arrest.1), meanNA(model.data$past.arrest.1), 
                                   model.data$past.arrest.1)

model.data$past.arrest.7 <- ifelse(is.na(model.data$past.arrest.7), meanNA(model.data$past.arrest.7), 
                                   model.data$past.crime.7)

model.data$past.arrest.30 <- ifelse(is.na(model.data$past.arrest.30), meanNA(model.data$past.arrest.30), 
                                    model.data$past.arrest.30)

# correlation between crimes and arrests
cor(model.data$past.crime.30, model.data$past.arrest.30)

# effect of policing
model.data$policing <- ifelse(model.data$past.crime.30 == 0, 0,  
                              model.data$past.arrest.30/model.data$past.crime.30)


# effect of crime trend
model.data$crime.trend <- ifelse(model.data$past.crime.30 == 0, 0, 
                                 model.data$past.crime.7/model.data$past.crime.30)



# Season variable
model.data$season <- as.factor(ifelse(model.data$month %in% c("Mar", "Apr", "May"), "spring", 
                                      ifelse(model.data$month %in% c("Jun", "Jul", "Aug"), "summer",
                                             ifelse(model.data$month %in% c("Sep", "Oct", "Nov"), "fall", "winter"))))


# summary of modeling data set
summary(model.data)

# correlation plot
library(psych)
model.cor <- cor(model.data[, c('count', 'past.crime.1', 'past.crime.7', 
                                'past.crime.30','policing', 'crime.trend')])

png("cor.plot.png", 640, 480)
  op <- par(mar= rep(0,4))
  cor.plot(model.cor)
  par(op)
dev.off()


# Keep 10% of the data for out of time validaton
model.data <- orderBy(~ date, data= model.data)

rows <- c(1:floor(nrow(model.data)*0.9))

test.data <- model.data[-rows, ]
model.data <- model.data[rows, ]


# Negative binomial regression model
library(MASS)
crime.model <- glm.nb(count ~ past.crime.1 + past.crime.7 + past.crime.30 
                     + policing + crime.trend + factor(day)  + season + I(past.crime.30^2)
                     , data= model.data)


summary(crime.model)


##########################################
###########    Section 5    ##############
########### Model Validation #############
##########################################

# predict on test data
crime.model.pred <- predict(crime.model, test.data, type= "response")

# rmse
sqrt(mean((test.data$count - crime.model.pred)^2))

# actual vs predicted graph
validate <- data.frame(test.data$count, crime.model.pred)
names(validate) <- c("actual", "predicted")

# decile predictions
validate$bucket <- with(validate, cut(predicted, breaks= quantile(predicted, probs= seq(0, 1, 0.1)),
                                      include.lowest= TRUE, labels= c(1:10)))

# average values of actual and predicted by decile
validate <- aggregate(validate[, c('actual', 'predicted')], by= list(validate$bucket), FUN = mean)

# plot
plot(validate$predicted, col= "red", type= "l", lwd= 1.5, ylab= "No. of Crashes", 
     xlab= "Predicted Crimes Decile", main= "Actual vs. Predicted")
lines(validate$actual, col= "blue", lwd= 1.5)
legend("topleft", c("Actual", "Predicted"), col= c("blue", "red"), lwd= c(1.5, 1.5), bty= "n", cex= 0.8)