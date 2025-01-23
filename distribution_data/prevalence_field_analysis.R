#hermit crab prevalence data analysis and graphing
library(readr)
library(tidyr)
library(plyr)
library(dplyr)
library(MuMIn)
library(visreg)
library(car)
library(lmerTest)
library(RVAideMemoire)
library(multcomp)
library(sjPlot)
library(grid)  
library(ggplot2)
library(lubridate)
library(DHARMa)
library(effects)
#library(DMwR)
library(lme4)
library(numDeriv)
library(RCurl) ## to source() from Github
library(ggplot2)
library(reshape2)
library(plyr)
library(devtools)
library(glmmTMB)
library(lubridate)

library(RColorBrewer)

all_crabs<-prevalence_surveys

all_crabs$date<-as.Date(all_crabs$date)

check_crabs<-all_crabs %>% dplyr::count(site,date)

all_crabs<-all_crabs %>% filter(
  date!="2020-12-13"& date!= "2021-02-22"&date!="2021-05-19"&date!="2021-07-20"&
    date!="2021-09-05" &
    date!= "2020-12-12" &date!="2021-02-24"&
    date!="2021-05-16" &date!= "2021-07-12"&date!="2021-09-06"&
   date!= "2021-08-23" &date!= "2021-07-24"&
    date!="2021-08-18"& date!="2020-09-02"&date!="2020-11-22"&
    date!="2020-12-11" &date!= "2021-02-09"&date!="2021-05-03"&
    date!="2021-07-13"&date!="2021-09-03"&date!="2020-11-09"&
    date!="2021-02-06"&date!="2021-05-11"&date!="2021-07-12"&
    date!="2021-09-04"&date!="2021-06-12"&date!="2021-08-21"&
    date!="2021-05-02"&date!="2020-11-21"&date!="2021-02-07"&
    date!="2021-05-12"&date!="2021-07-11"&date!="2021-09-07"&
    date!="2021-06-12")
all_crabs<-all_crabs %>% filter(site!="tsawwassen_beach" | date!="2021-5-29" )  # removing all but first site visit

all_crabs<-all_crabs %>%  drop_na(salinity) #removing sites that are missing salinity



unique(all_crabs$rhizo)
all_crabs$rhizo<-revalue(all_crabs$rhizo, c("yes"=1))
all_crabs$rhizo<-revalue(all_crabs$rhizo, c("no"=0))
all_crabs$rhizo<-revalue(all_crabs$rhizo, c("no "=0))


unique(all_crabs$gill_isopod)
all_crabs$gill_isopod<-revalue(all_crabs$gill_isopod, c("yes"=1))
all_crabs$gill_isopod<-revalue(all_crabs$gill_isopod, c("no"=0))
all_crabs$gill_isopod<-revalue(all_crabs$gill_isopod, c("no "=0))

all_crabs$rhizo<-as.numeric(all_crabs$rhizo)
all_crabs$X2nd_dactyl<-as.numeric(all_crabs$X2nd_dactyl)

all_crabs$gill_isopod<-as.numeric(all_crabs$gill_isopod)
all_crabs$density<-as.numeric(all_crabs$density)

str(all_crabs)

#changing the date
all_crabs$date<-as.Date(all_crabs$date)

snapshot_survey<-all_crabs

#creates dataframes for model
ready_rhizo<-data.frame(site=as.factor(snapshot_survey$site),rhizo=snapshot_survey$rhizo, dactyl=scale(snapshot_survey$X2nd_dactyl),salinity=scale(snapshot_survey$salinity),date=scale(snapshot_survey$date), density=scale(snapshot_survey$density), lat=snapshot_survey$lat, long=snapshot_survey$long) 

ready_gill<-data.frame(site=as.factor(snapshot_survey$site),gill_isopod=snapshot_survey$gill_isopod,dactyl=scale(snapshot_survey$X2nd_dactyl),salinity=scale(snapshot_survey$salinity),date=scale(snapshot_survey$date), density=scale(snapshot_survey$density),lat=snapshot_survey$lat, long=snapshot_survey$long)

library(visreg)
#models of prevalence
rhizo_model<- glmmTMB(rhizo~salinity+dactyl+density+(1|site)+(1|date),family=binomial, data=ready_rhizo)

gill_model<- glmmTMB(gill_isopod~salinity+dactyl+density+(1|site)+(1|date),family=binomial, data=ready_gill)

#getting model information
vc<-(VarCorr(rhizo_model))
vc
plotresid(rhizo_model)
plotresid(gill_model)
print(vc,comp=c("Variance","Std.Dev."), digits=2)
print(vc, comp=c("Variance"))
summary(rhizo_model)
tab_model(rhizo_model)
summary(gill_model)
tab_model(gill_model)
#looking at models
visreg(rhizo_model)
visreg(gill_model)

#graphs for publication
visreg(rhizo_model, "dactyl", scale="response", overlay=TRUE, ylab="Probability of infection", xlab="Standardized dactyl length")

rhizo_1<-visreg(rhizo_model,line=list(col="black"), "dactyl", 
                scale="response", overlay=TRUE, gg=TRUE, 
          ylab="Probability of infection",
          xlab="Standardized dactyl length")+theme_classic(base_size = 18)

rhizo_1
rhizo_2<-visreg(rhizo_model,line=list(col="black"), "density", scale="response", 
                overlay=TRUE, gg=TRUE, ylab="Probability of infection", xlab="Standardized relative host abundance")+theme_classic(base_size = 20)
rhizo_2

gill<-visreg(gill_model,line=list(col="black"), "salinity", scale="response", overlay=TRUE, gg=TRUE, 
                ylab="Probability of infection", xlab="Standardized salinity")+theme_classic(base_size = 20)
gill
gill2<-visreg(gill_model,line=list(col="black"), "density", scale="response", 
                overlay=TRUE, gg=TRUE, ylab="Probability of infection", 
              xlab="Standardized host relative abundance")+theme_classic(base_size = 20)
gill2

#this dataframe is the same as above but only includes sites that were revisted before and after heat wave, and has region label
#how to make the heat wave graph
seas<-`hermit_crab_data_sep_1_2023...seasonal+island.(3)`
library(plyr)
seas$rhizo<-revalue(seas$rhizo, c("yes"=1))
seas$rhizo<-revalue(seas$rhizo, c("no"=0))
seas$gill_isopod<-revalue(seas$gill_isopod, c("yes"=1))
seas$gill_isopod<-revalue(seas$gill_isopod, c("no"=0))
seas$date<-as.Date(seas$date)
seas$site<-as.character(seas$site)
seas$rhizo<-as.numeric(seas$rhizo)
seas$gill_isopod<-as.numeric(seas$gill_isopod)

library(tidyverse)
seas_df<-seas %>%
  group_by(site, date) %>%
  summarise(region=region, length=length(rhizo), rhizo_prevalence=sum(rhizo, na.rm = T)/length(rhizo), length=length(gill_isopod), gill_prevalence=sum(gill_isopod, na.rm = T)/length(gill_isopod))
#seas_df$region<-revalue(seas_df$region, c("lower_mainland"=2020))
#seas_df$region<-revalue(seas_df$region, c("vancouver_island"=2021))

library(RColorBrewer)
library(ggplot2)
ggplot(seas_df, aes(x=date,y=rhizo_prevalence, fill=site, colour=region))+
  geom_point(size=2)+geom_line(size=0.5,linetype = "dashed")+theme_classic(base_size = 18)+ 
  xlab("Date")+ylab("Prevalence")+
  guides(fill = FALSE)+labs(color='Region')+
  scale_color_brewer(palette = "Dark2", labels=c("Lower Mainland", "Vancouver Island"))+ 
  theme(legend.justification=c(0,1), legend.position=c(0.01,1))+ 
  geom_vline(xintercept = as.numeric(as.Date("2021-06-24")), alpha=0.3)+
  geom_vline(xintercept = as.numeric(as.Date("2021-06-25")),alpha=0.3)+
  geom_vline(xintercept = as.numeric(as.Date("2021-06-26")), alpha=0.3)+
  geom_vline(xintercept = as.numeric(as.Date("2021-06-27")), alpha=0.3)+
  geom_vline(xintercept = as.numeric(as.Date("2021-06-28")), alpha=0.3)+
  geom_vline(xintercept = as.numeric(as.Date("2021-06-29")), alpha=0.3)+
  geom_vline(xintercept = as.numeric(as.Date("2021-06-30")), alpha=0.3)+
  geom_vline(xintercept = as.numeric(as.Date("2021-07-01")), alpha=0.3)+
  geom_vline(xintercept = as.numeric(as.Date("2021-07-02")), alpha=0.3)+
  geom_vline(xintercept = as.numeric(as.Date("2021-07-03")), alpha=0.3)+
  geom_vline(xintercept = as.numeric(as.Date("2021-07-04")), alpha=0.3)+
  geom_vline(xintercept = as.numeric(as.Date("2021-07-05")), alpha=0.3)+
  geom_vline(xintercept = as.numeric(as.Date("2021-07-06")), alpha=0.3)+
  geom_vline(xintercept = as.numeric(as.Date("2021-07-07")), alpha=0.3)

