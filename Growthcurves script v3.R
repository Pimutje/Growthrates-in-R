library(growthrates)
library(ggplot2)
library(tidyr)
library(lubridate)

#importing the data from a .csv file
RawData <- read.csv("C:/Users/Pimut/OneDrive/UvA/Lab/Results/Spent medium growth/Run 2/absorbance log 2021-06-04T15_08_43.csv", header=T,check.names=F)
str(RawData)

#rename the first column to time in hours
names(RawData)[1] <- "Time..h"

#convert the time in the first column to time in hours
RawData$Time..h <- as.numeric(as.POSIXct(RawData$Time..h))
RawData$Time..h<-(RawData$Time..h - RawData$Time..h[1])/3600

#select the columns to be used for the wide data format for ggplot2, excluding Time..h
RawDataColumnArray <- c(colnames(RawData)[2:length(RawData)])

#creating a wide data format for ggplot2
df_wide <- RawData %>% pivot_longer(RawDataColumnArray, names_to = "colname", values_to = "val")

#Making growthcurves
ggplot(df_wide, aes(x = Time..h ,value, y=val, color=colname)) + geom_point()

#defining the time as a variable t
t <- RawData$Time..h

#calculate growthrates for all columns
fitfun <- function(x){coef(fit_easylinear(t, x))[3]}
result <- sapply(RawData[,-1],fitfun)
write.csv(result,"C:\\Users\\Pimut\\OneDrive\\UvA\\Lab\\Results\\Spent medium growth\\growthrates.csv")

#this function needs to loop for all data in the sheet (max 96 columns)
for (i in colnames(RawData)[2:length(RawData)]) {
  
  fit <- fit_easylinear(t, RawData[[i]])
  par(mfrow = c(1, 2))
  plot(fit, log = "y", main=i)
  plot(fit)
  
  ##plot(average.time.h , Amean , type="l" , col="green")##
  ##lines(average.time.h , C1mean , type="l" , col="blue")##
  
  par(mfrow = c(1, 2))
  plot(fit, log = "y", main=i)
  plot(fit)
}

#Saving all the plots
plots.dir.path <- list.files(tempdir(), pattern="rs-graphics", full.names = TRUE); 
plots.png.paths <- list.files(plots.dir.path, pattern=".png", full.names = TRUE)
file.copy(from=plots.png.paths, to="C:\\Users\\Pimut\\OneDrive\\UvA\\Lab\\Results\\Spent medium growth\\rplots")

#specific information per column
fit <- fit_easylinear(t, RawData$X84)
summary(fit)
coef(fit)
rsquared(fit)
deviance(fit)