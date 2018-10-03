# These are the solutions that Steve and Fabi found for these problems. They are not the only solutions!

#=============================================
#Math challenge 
#=============================================

# Make a Fibonacci sequence 10 numbers long.

len <- 10
fibvals <- numeric(len)
fibvals[1] <- 1
fibvals[2] <- 1
for (i in 3:len) { 
  fibvals[i] <- fibvals[i-1]+fibvals[i-2]
} 

fibvals

#What is the sum of all the numbers?

sum(fibvals) 

#Plot each numeral against it's inverse, using a line.

plot(fibvals, 1/fibvals, type = "l")

#==========================================
#R psychology challenge
#==========================================

#From a survey of the clerical employees of a large financial organization, the data are aggregated from the questionnaires of the approximately 35 employees for each of 30 (randomly selected) departments. The numbers give the percent proportion of favourable responses to seven questions in each department. In the survey, there was a question designed to measure the overall performance of a supervisor, as well as questions that related to specific activities involving interaction between supervisor and employee.

library("datasets")

data(attitude) ; attitude

#Variables

#overall rating of job being done by supervisor
#handles employee complaints
#does not allow special privileges
#opportunity to learn more things
#raises based on performances
#too critical of poor performances
#rate of advancing to better jobs

# Which variable most strongly predicts a positive overall rating of job being done by supervisor?

A <- lm(rating ~., data = attitude)
summary(A)

B <- lm(rating ~ complaints, data = attitude)

#make a plot of the regression including the linear model
plot(attitude$rating ~ attitude$complaints)
abline(a = B$coefficients[1], b = B$coefficients[2])

#========================================
#Biology example
#========================================

data(CO2) ; head(CO2)

#make a boxplot to explore if there might be a difference in CO2 uptake of the different type of plants.

plot(CO2$uptake ~ CO2$Type)

#what about the different treatments?
plot(CO2$uptake ~ CO2$Treatment)

#Use an ANOVA to test whether any of the plants took up less CO2 when chilled.

summary(aov(CO2$uptake ~ CO2$Type*CO2$Treatment))

#=========================================
# Spatial data
#=========================================

# Load the libraries
if (!require(sp)) install.packages('sp') #Converts  data frames to spatial files
library(sp) 

if (!require(rgdal)) install.packages('rgdal')
library(rgdal) # Geospatial data loading and manipulation (load shapefiles)


# Load the data that has information on geographical location of several Earthquake locations in Fiji
data(quakes) ; quakes

# You got an Excel file with these locations but your collaborators want you to convert the file to a shapefile that will easily be read in any GIS program
Quake.locs<-quakes[,1:2] # Subset the x and y coordinates

Quake.locs.shp<-SpatialPointsDataFrame(Quake.locs,data.frame(dummy=rep(1,1000)))

plot(Quake.locs.shp)

writeOGR(Quake.locs.shp, dsn="tempdir", layer="Quake.locs", driver="ESRI Shapefile")

#=========================================
# Data organization
#=========================================

# You wish to explore a dataset on admissions of graduate students by six departments in the year 1973 at UC Berkeley. The table that would make this easier would show the number of students accepted, grouped by gender in each department. As an example:

# Department      Gender       No.
#
# Biology         Female       60
#                 Male         120


# Install and/or load the package dplyr. This package is useful to explore, organize, and summarize data

if (!require(dplyr)) install.packages('dplyr')
library(dplyr) 

# Load the dataset of admissions UCB
data(UCBAdmissions); UCBAdmissions

# View the dataset 
View(UCBAdmissions)
UCBAdmissions.tbl<-tbl_df(UCBAdmissions) # The table needs to be formatted specifically for dplyr processes

# Summarize your tasks: Number of students accepted-> by Gender -> in each Department

UCBAdmissions.summary<- UCBAdmissions.tbl%>%filter(Admit=="Admitted")%>%select(Dept,Gender,n)

UCBAdmissions.summary
