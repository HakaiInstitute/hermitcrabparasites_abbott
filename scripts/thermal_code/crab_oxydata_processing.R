# hermit crab oxydata processing 
library(dplyr)
library(tidyverse)
library(ggplot2)
require(readr)  # for read_csv()
require(dplyr)  # for mutate()
require(tidyr)  # for unnest()
require(purrr)
 

# see textfile wrangling for how to get all measurements together for each temperature and how to clean the files
#see oxygen_slopes for how to get the compiled measurements file into a slope file
# this file shows how files were graphed, and then cut for quality control processing, as applicable

#graph the data to check where to cut it
#we removed missing and unreasonable measurements 
#also removed the first 30s 



setwd("~/Desktop/hermit crab project/summer_metabolic_data/compiled_data")

data<- read.csv("32C_control.csv")

ggplot(aug_5_32_control, aes(x=time, y= oxygen, colour=channel))+geom_point(size=0.5) +ylab("micromol /L ")+facet_wrap("~date")+theme_classic()

data2<-data %>% 
  filter(time>30)

#where to cut 
#30s in is default
# end cut aug 28 at 300s 
# start aug 30 at 150s


#how to get slopes from the data, this is example code, and code for days/runs that were not the typical case

setwd("~/Desktop/hermit crab project/summer_metabolic_data/compiled_data")

data = read.csv("22C_control.csv", header=TRUE, stringsAsFactors=FALSE) # adjust to make this be your filename here
view(data)

head(data) # examine the data to check it loaded correctly
str(data) # examine at the structure of the variables
names(data) # what are the variable names called in the dataframe?

data$date<-as.Date(data$date, "%Y-%m-%d") # format date to be readable by R

newData <- select(data, date, time, channel, oxygen)  # keep only what we care about
head(newData)
str(newData)

days <- unique(newData$date) # how many unique measurement days there are
length(days)# there are 16 measurement days

view(data)

library(lubridate)
library(hms)
data$Time..HH.MM.SS.<-as_hms(data$Time..HH.MM.SS.)
data$numeric_time<-as.numeric(data$Time..HH.MM.SS.)
view(data)


round_starts<-data %>%
filter(time==0, date=="2021-08-28")
view(round_starts)

#12C_control filter for last round of control

filter_control_12 <- data %>%
filter((date=="2021-08-01" & numeric_time >=41935)|(date=="2021-08-05" & numeric_time >=36222)|(date=="2021-08-06" & numeric_time >= 35404) |(date=="2021-08-08" & numeric_time >=36576)|(date=="2021-08-09" & numeric_time >=38838) | (date=="2021-08-10") | (date=="2021-08-11" & numeric_time >=38071) |(date=="2021-08-14" & numeric_time >=36740)  |(date=="2021-08-15" & numeric_time >=39099) |  (date=="2021-08-25" & numeric_time >=35578) |  (date=="2021-08-28")  |  (date=="2021-08-29" & numeric_time >=34744)
       |  (date=="2021-08-30" & numeric_time >=35910)     |  (date=="2021-08-31" & numeric_time >=36034
)   |  (date=="2021-09-01" & numeric_time >=35651)   |  (date=="2021-09-01" & numeric_time >=35651)       |  (date=="2021-09-02"))
       
view(filter_control_12)
         
view(test_data)

data_5<- data %>%
filter(date=="2021-08-08")

ggplot(aug_5_27_control, aes(x=time, y= oxygen, colour=channel))+geom_point(size=0.5) +ylab("micromol /L ")+theme_classic()+facet_wrap("~date")

#getting the slopes from the compiled data file, if there is nothing unusual about the file. typical case:
slopes_17_control<-data %>% 
  filter(!is.na(oxygen)) %>%  #remove the NAs
  group_by(date, channel) %>% # groups the channels
  filter(time > 30) %>% # removes the first 30 s to get out any wonky measurements
  summarise(slope = lm(oxygen ~ time)$coefficients[2], mean_temp = mean(temp)) 

#12C control used last round. some reason 9 only has 100s c
# end cut aug 28 at 300s 
# start aug 30 at 150s

aug_28_12C_control<-filter_control_12 %>% 
  filter(!is.na(oxygen)) %>% 
  group_by(date, channel) %>% 
  filter(time > 30 & time < 300 & date=="2021-08-28")%>% 
  summarise(slope = lm(oxygen ~ time)$coefficients[2], mean_temp = mean(temp)) 

aug_30_12C_control<-filter_control_12 %>% 
  filter(!is.na(oxygen)) %>% 
  group_by(date, channel) %>% # groups the channels
  filter(time > 150 & date=="2021-08-30")%>% 
  summarise(slope = lm(oxygen ~ time)$coefficients[2], mean_temp = mean(temp)) 
         
#17C editing
aug28_ch1<-data %>% 
  filter(!is.na(oxygen)) %>% 
  group_by(date, channel) %>% # groups the channels
  filter(time > 400 & date=="2021-08-28" & channel=="Ch1")%>% 
  summarise(slope = lm(oxygen ~ time)$coefficients[2], mean_temp = mean(temp)) 
#17C control Aug 5 27 got put in the wrong csv, and then used 2nd attempt for aug 5th
data$Time..HH.MM.SS.<-as_hms(data$Time..HH.MM.SS.)
data$numeric_time<-as.numeric(data$Time..HH.MM.SS.)

aug5_17_control<-data %>% 
  filter(!is.na(oxygen)) %>% 
  group_by(date, channel) %>% 
  filter(time > 30 & date=="2021-08-05" & numeric_time>42870 & temp < 23)  %>%  summarise(slope = lm(oxygen ~ time)$coefficients[2], mean_temp = mean(temp)) 

aug29_17_control<-data %>% 
  filter(!is.na(oxygen)) %>% 
  group_by(date, channel) %>% 
  filter(time > 150 & date=="2021-08-29")  %>%  summarise(slope = lm(oxygen ~ time)$coefficients[2], mean_temp = mean(temp)) 
#22 editing

aug11_ch2_22C<-data %>% 
  filter(!is.na(oxygen)) %>% 
  group_by(date, channel) %>% 
  filter(time > 300 & date=="2021-08-11" & channel=="Ch2")  %>%  summarise(slope = lm(oxygen ~ time)$coefficients[2], mean_temp = mean(temp))

aug28_22C<-data %>% 
  filter(!is.na(oxygen)) %>% 
  group_by(date, channel) %>% 
  filter(time > 200 & date=="2021-08-28" )  %>%  summarise(slope = lm(oxygen ~ time)$coefficients[2], mean_temp = mean(temp)) 

#27C editing 
aug8_27C<-data %>% 
  filter(!is.na(oxygen)) %>% 
  group_by(date, channel) %>% 
  filter(time > 30 & date=="2021-08-08" & numeric_time < 60183)  %>%  summarise(slope = lm(oxygen ~ time)$coefficients[2], mean_temp = mean(temp)) 
#27C control editing   #aug 28 has two rounds, Aug 15 also has two rounds, aug 10 has two rounds as well
#aug 10 remove short round 
# remove first round for Aug 15
#aug 28 remove 22C temp
data$Time..HH.MM.SS.<-as_hms(data$Time..HH.MM.SS.)
data$numeric_time<-as.numeric(data$Time..HH.MM.SS.)

data7<- data %>%
  filter(date=="2021-08-10"|date=="2021-08-15"|date=="2021-08-28")
round_starts<-data7 %>%
  filter(time==0)
view(round_starts)

aug10_27C_control<-data7 %>% 
  filter(!is.na(oxygen)) %>% 
  group_by(date, channel) %>% 
  filter(time > 30 & date=="2021-08-10" & numeric_time> 52465)  %>%  summarise(slope = lm(oxygen ~ time)$coefficients[2], mean_temp = mean(temp)) 

aug15_27C_control<-data7 %>% 
  filter(!is.na(oxygen)) %>% 
  group_by(date, channel) %>% 
  filter(time > 30 & date=="2021-08-15" & numeric_time> 56136)  %>%  summarise(slope = lm(oxygen ~ time)$coefficients[2], mean_temp = mean(temp)) 

aug28_27C_control<-data7 %>% 
  filter(!is.na(oxygen)) %>% 
  group_by(date, channel) %>% 
  filter(time > 30 & date=="2021-08-28" & numeric_time> 53084)  %>%  summarise(slope = lm(oxygen ~ time)$coefficients[2], mean_temp = mean(temp)) 


slopes_27control_edits <- rbind(aug10_27C_control, aug15_27C_control,aug28_27C_control) 


#32C edits
# Aug 5 decided to cut all at 250, even though it was mainly ch1 messed up, the others look possibly affected 
aug5_32C<-data%>% 
  filter(!is.na(oxygen)) %>% 
  group_by(date, channel) %>% 
  filter(time > 250 & date=="2021-08-05")  %>%  summarise(slope = lm(oxygen ~ time)$coefficients[2], mean_temp = mean(temp)) 
#32C control edit
data$Time..HH.MM.SS.<-as_hms(data$Time..HH.MM.SS.)
data$numeric_time<-as.numeric(data$Time..HH.MM.SS.)

aug_5_32_control<-data %>% 
  filter(!is.na(oxygen)) %>% 
  group_by(date, channel) %>% 
  filter(time > 30 & date=="2021-08-05" & numeric_time>66234 )  %>%  summarise(slope = lm(oxygen ~ time)$coefficients[2], mean_temp = mean(temp)) 

# getting 27C aug 5th control 
data$Time..HH.MM.SS.<-as_hms(data$Time..HH.MM.SS.)
data$numeric_time<-as.numeric(data$Time..HH.MM.SS.)

aug_5_27_control<-data %>% 
  filter(!is.na(oxygen)) %>% 
  group_by(date, channel) %>% 
  filter(date=="2021-08-05", numeric_time > 56542, time>175 )   %>% 
  summarise(slope = lm(oxygen ~ time)$coefficients[2], mean_temp = mean(temp)) 


round_starts<- aug_5_27_control %>%
  filter(time==0)
view(round_starts)

ggplot(aug_5_27_control, aes(x=time, y= oxygen, colour=channel))+geom_point(size=0.5) +ylab("micromol /L ")+theme_classic()+facet_wrap("~date")


write.csv(aug_5_27_control, "slope_aug5_27_control.csv")  

#writes out the slopes
# double check that all slopes are negative. values are in micromols Oxygen
# values are not normalized to anything! 
# must normalize to Chamber Volume (L) and Crabbie Biomass (g FW)
 # write to file :D



