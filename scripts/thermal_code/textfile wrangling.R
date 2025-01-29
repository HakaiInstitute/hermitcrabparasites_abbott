
library(dplyr)
library(tidyverse)

require(readr)  # for read_csv()
require(dplyr)  # for mutate()
require(tidyr)  # for unnest()
require(purrr)  # 

#this code takes the textfiles from pyroscience and makes them workable.
#compiles all the crab or control measurements at each temperature into a csv file 
#12C
files <- list.files("12C", pattern="*.txt",full.names=TRUE)
data <- files %>%
  map(read_tsv, skip=19) %>% 
  reduce(rbind)

files2 <- list.files("12C_3", pattern="*.txt",full.names=TRUE)

data2 <- files2 %>%
  map(read_tsv, skip=17) %>% 
  reduce(rbind)

single_data<-read_tsv("12 crabs aug 10     C only.txt", skip =13)

data3 <- rbind(data, data2)

data3 = rename(data3, date= "Date", temp = "Ch 1", time= "Time (s)")
data3 <- data3[ -c(4,10:31) ]

single_data = rename(single_data, date= "Date", temp = "Ch 2", time= "Time (s)")
single_data <- single_data[ -c(4,9,11:31) ]

all_data<-rbind(single_data, data3)

all_data<-transform(all_data, Ch2 = as.numeric(Ch2), 
          Ch3 = as.numeric(Ch3),Ch1 = as.numeric(Ch1), 
          Ch4 = as.numeric(Ch4))

all_data <- all_data %>% pivot_longer(c(Ch1, Ch2, Ch3, Ch4), names_to = "channel", values_to = "oxygen")

view(all_data)

write.csv(all_data, "12C.csv")

#12C control
files <- list.files("12C_control", pattern="*.txt",full.names=TRUE)
data <- files %>%
  map(read_tsv, skip=19) %>% 
  reduce(rbind)

files2 <- list.files("12C_control_3", pattern="*.txt",full.names=TRUE)

data2 <- files2 %>%
  map(read_tsv, skip=17) %>% 
  reduce(rbind)

data3 <- rbind(data, data2)


data3 = rename(data3, date= "Date", temp = "Ch 1", time= "Time (s)")
data3 <- data3[ -c(4,10:31) ]

data3<-transform(data3, Ch2 = as.numeric(Ch2), 
                 Ch3 = as.numeric(Ch3))

data3 <- data3 %>% pivot_longer(c(Ch1, Ch2, Ch3, Ch4), names_to = "channel", values_to = "oxygen")

view(data3)

write.csv(data3, "12C_control.csv")

#17C

files <- list.files("17C", pattern="*.txt",full.names=TRUE)
data <- files %>%
  map(read_tsv, skip=19) %>% 
  reduce(rbind)

files2 <- list.files("17C_3", pattern="*.txt",full.names=TRUE)

data2 <- files2 %>%
  map(read_tsv, skip=17) %>% 
  reduce(rbind)

data3 <- rbind(data, data2)

data3 = rename(data3, date= "Date", temp = "Ch 1", time= "Time (s)")
data3 <- data3[ -c(4,10:31) ]

data3<-transform(data3, Ch2 = as.numeric(Ch2), 
                 Ch3 = as.numeric(Ch3))

data3 <- data3 %>% pivot_longer(c(Ch1, Ch2, Ch3, Ch4), names_to = "channel", values_to = "oxygen")

view(data3)

write.csv(data3, "17C.csv")

#17C control
files <- list.files("17C_control", pattern="*.txt",full.names=TRUE)
data <- files %>%
  map(read_tsv, skip=19) %>% 
  reduce(rbind)

files2 <- list.files("17C_control_3", pattern="*.txt",full.names=TRUE)

data2 <- files2 %>%
  map(read_tsv, skip=17) %>% 
  reduce(rbind)

data3 <- rbind(data, data2)

data3 = rename(data3, date= "Date", temp = "Ch 1", time= "Time (s)")
data3 <- data3[ -c(4,10:31) ]

data3<-transform(data3, Ch2 = as.numeric(Ch2), 
                 Ch3 = as.numeric(Ch3),Ch1 = as.numeric(Ch1), 
                 Ch4 = as.numeric(Ch4))

data3 <- data3 %>% pivot_longer(c(Ch1, Ch2, Ch3, Ch4), names_to = "channel", values_to = "oxygen")

view(data3)

write.csv(data3, "17C_control.csv")


#22C
files <- list.files("22C", pattern="*.txt",full.names=TRUE)
data <- files %>%
  map(read_tsv, skip=19) %>% 
  reduce(rbind)

files2 <- list.files("22C_3", pattern="*.txt",full.names=TRUE)

data2 <- files2 %>%
  map(read_tsv, skip=17) %>% 
  reduce(rbind)
view(data2)

data3 <- rbind(data, data2)

data3 = rename(data3, date= "Date", temp = "Ch 1", time= "Time (s)")
data3 <- data3[ -c(4,10:31) ]

data3<-transform(data3, Ch2 = as.numeric(Ch2), 
                 Ch3 = as.numeric(Ch3),Ch1 = as.numeric(Ch1), 
                 Ch4 = as.numeric(Ch4))

data3 <- data3 %>% pivot_longer(c(Ch1, Ch2, Ch3, Ch4), names_to = "channel", values_to = "oxygen")

view(data3)

write.csv(data3, "22C.csv")

#22C control

files <- list.files("22C_control", pattern="*.txt",full.names=TRUE)
data <- files %>%
  map(read_tsv, skip=19) %>% 
  reduce(rbind)

single_data2<-read_tsv("22C control aug 28.txt", skip = 19)

single_data2<-single_data2[-c(302:319),]

files2 <- list.files("22C_control_3", pattern="*.txt",full.names=TRUE)

data2 <- files2 %>%
  map(read_tsv, skip=17) %>% 
  reduce(rbind)

data3 <- rbind(data, data2, single_data2)

view(data3)
data3 = rename(data3, date= "Date", temp = "Ch 1", time= "Time (s)")
data3 <- data3[ -c(4,10:31) ]

data3<-transform(data3, Ch2 = as.numeric(Ch2), 
                 Ch3 = as.numeric(Ch3),Ch1 = as.numeric(Ch1), 
                 Ch4 = as.numeric(Ch4))

data3 <- data3 %>% pivot_longer(c(Ch1, Ch2, Ch3, Ch4), names_to = "channel", values_to = "oxygen")

view(data3)

write.csv(data3, "22C_control.csv")

#27C

files <- list.files("27C", pattern="*.txt",full.names=TRUE)
data <- files %>%
  map(read_tsv, skip=19) %>% 
  reduce(rbind)

files2 <- list.files("27C_3", pattern="*.txt",full.names=TRUE)

data2 <- files2 %>%
  map(read_tsv, skip=17) %>% 
  reduce(rbind)

data3 <- rbind(data, data2)

data3 = rename(data3, date= "Date", temp = "Ch 1", time= "Time (s)")
data3 <- data3[ -c(4,10:31) ]

data3<-transform(data3, Ch2 = as.numeric(Ch2), 
                 Ch3 = as.numeric(Ch3),Ch1 = as.numeric(Ch1), 
                 Ch4 = as.numeric(Ch4))

data3 <- data3 %>% pivot_longer(c(Ch1, Ch2, Ch3, Ch4), names_to = "channel", values_to = "oxygen")

view(data3)

write.csv(data3, "27C.csv")

#27C control 

files <- list.files("27C_control", pattern="*.txt",full.names=TRUE)
data <- files %>%
  map(read_tsv, skip=19) %>% 
  reduce(rbind)
view(data)

single_data3<-read_tsv("27C control.txt", skip = 19)
single_data3<-single_data2[-c(182:199),]
view(single_data3)

files2 <- list.files("27C_control_3", pattern="*.txt",full.names=TRUE)

data2 <- files2 %>%
  map(read_tsv, skip=17) %>% 
  reduce(rbind)

data3 <- rbind(data, data2, single_data3)

data3 = rename(data3, date= "Date", temp = "Ch 1", time= "Time (s)")
data3 <- data3[ -c(4,10:31) ]

data3<-transform(data3, Ch2 = as.numeric(Ch2), 
                 Ch3 = as.numeric(Ch3),Ch1 = as.numeric(Ch1), 
                 Ch4 = as.numeric(Ch4))

data3 <- data3 %>% pivot_longer(c(Ch1, Ch2, Ch3, Ch4), names_to = "channel", values_to = "oxygen")

view(data3)

write.csv(data3, "27C_control.csv")

#32C 
files <- list.files("32C", pattern="*.txt",full.names=TRUE)
data <- files %>%
  map(read_tsv, skip=19) %>% 
  reduce(rbind)

files2 <- list.files("32C_3", pattern="*.txt",full.names=TRUE)

data2 <- files2 %>%
  map(read_tsv, skip=17) %>% 
  reduce(rbind)

data3 <- rbind(data, data2)

data3 = rename(data3, date= "Date", temp = "Ch 1", time= "Time (s)")
data3 <- data3[ -c(4,10:31) ]

data3<-transform(data3, Ch2 = as.numeric(Ch2), 
                 Ch3 = as.numeric(Ch3),Ch1 = as.numeric(Ch1), 
                 Ch4 = as.numeric(Ch4))

data3 <- data3 %>% pivot_longer(c(Ch1, Ch2, Ch3, Ch4), names_to = "channel", values_to = "oxygen")

view(data3)

write.csv(data3, "32C.csv")

#32C control 
files <- list.files("32C_control", pattern="*.txt",full.names=TRUE)
data <- files %>%
  map(read_tsv, skip=19) %>% 
  reduce(rbind)

files2 <- list.files("32C_control_3", pattern="*.txt",full.names=TRUE)

data2 <- files2 %>%
  map(read_tsv, skip=17) %>% 
  reduce(rbind)

data3 <- rbind(data, data2)

data3 = rename(data3, date= "Date", temp = "Ch 1", time= "Time (s)")
data3 <- data3[ -c(4,10:31) ]

data3<-transform(data3, Ch2 = as.numeric(Ch2), 
                 Ch3 = as.numeric(Ch3),Ch1 = as.numeric(Ch1), 
                 Ch4 = as.numeric(Ch4))

data3 <- data3 %>% pivot_longer(c(Ch1, Ch2, Ch3, Ch4), names_to = "channel", values_to = "oxygen")

view(data3)


library(ggplot2)


ggplot(data3, aes(x=time, y= oxygen, colour=channel))+geom_point(size=0.1) +ylab("micromol /L ")+facet_wrap("~date")+theme_classic()

write.csv(data3, "32C_control.csv")




