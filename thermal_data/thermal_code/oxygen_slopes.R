## USAGE: This script takes data from one temperature on multiple days in multiple chambers and calculates the slope 
# filtering samples by day, by channel and then removing the first 30s of data points because some looked wonky (see plots)
#Harmony Martell and Madeleine Abbott
## Load in packages
library(anytime)
library(plyr)
library(dplyr)
library(ggplot2)
library(chron)
library(tidyverse)
library(lubridate)
library(psych)
library(readr)
library(data.table)
library(scales)

####################################
#set working directory
setwd("~/Desktop/UBC/_Projects/xThermalPerformanceCrabbies/DATA_-_Active_Sites/data/") # put the dir of your files here

data = read.csv("22C.csv", header=TRUE, stringsAsFactors=FALSE) # adjust to make this be your filename here
head(data) # examine the data to check it loaded correctly
str(data) # examine at the structure of the variables
names(data) # what are the variable names called in the dataframe?

data$date<-as.Date(data$date, "%Y-%m-%d") # format date to be readable by R

newData <- select(data, date, time, channel, oxygen)  # keep only what we care about
head(newData)
str(newData)

days <- unique(newData$date) # how many unique measurement days there are
length(days)# there are 16 measurement days

control_32<-data3 %>% 
  filter(!is.na(oxygen)) %>%  #remove the NAs
  group_by(date, channel) %>% # groups the channels
  filter(time > 30) %>% # removes the first 30 s to get out any wonky measurements
  summarise(slope = lm(oxygen ~ time)$coefficients[2]) #writes out the slopes
# double check that all slopes are negative. values are in micromols Oxygen
# values are not normalized to anything! 
# must normalize to Chamber Volume (L) and Crabbie Biomass (g FW)
fwrite(TwentyTwo,file="22Cslopes.csv") # write to file :D


# Aug14<-newData %>% 
#   filter(date=="2021-08-14") %>% # filters by date
#   group_by(date, channel)  # groups the channels



#data.r <- recovery
#data.c <- challenge