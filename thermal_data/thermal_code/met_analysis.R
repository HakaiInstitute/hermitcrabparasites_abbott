
#analysis of hermit crab metabolic data
library(ggplot2)
library(dplyr)
library(tidyr)
library(lme4)
library(multcomp)
library(car)
library(lmerTest)
library(visreg)
library(RVAideMemoire)
library(rstatix)

crabs<-lab_crabs_summer_2021_metrate



clean_crabs<- filter(crabs, trial>1 & group!="infected_isopod" & group!="hyper_infected" & met_rate>0 & temperature<33)
#removed the first trial day as equipment was not two point calibrated
#removed individuals that had isopod infections or had a hyperparasite on Peltogaster. 
#removed trials where temperature went too high
#remove individual 2nd appearnace of 46, appears 2x unclear notes and unclear what happened  ^ 
# take out extra things discussing signficnace
clean_crabs<- filter(clean_crabs, individual != 46 | trial != 11)

ggplot(clean_crabs, aes(x=individual, y=met_rate,colour=group))+geom_point()

#preparing data
clean_crabs$temperature<-as.numeric(clean_crabs$temperature)

clean_crabs$met_rate<-as.numeric(clean_crabs$met_rate)

clean_crabs$group<-as.factor(clean_crabs$group)

clean_crabs$crab_biomass<-as.numeric(clean_crabs$crab_biomass)


ggplot(clean_crabs, aes(x=temperature, y=met_rate,colour=group))+geom_point()+geom_smooth()+theme_classic()


#making the model


#m <- lmer(met_rate ~ scale(temperature)+I(scale(temperature)^2)+group + scale(crab_biomass)+ (1|individual), data = clean_crabs)
m <- lmer(met_rate ~ temperature+I(temperature^2)+group + (1|individual), data = clean_crabs)
#checking/looking at model
plot(m)
summary(m)
anova(m)
#put in sum of squares and  can report the F value then p value for the anova type III 
plotresid(m)

#Tukey method to compare groups
summary(multcomp::glht(m, linfct=mcp(group="Tukey")))

#looking at model
visreg(m,"group",by="temperature", overlay=TRUE, type = "contrast")
visreg(m,"group",by="temperature", overlay=TRUE, type = "conditional")
visreg(m,"temperature",by="group", overlay=TRUE, type="conditional")
visreg(m, "temperature", by="group", type="contrast")

#graph for publication
visreg(m,"temperature",by="group", overlay=TRUE, legend=FALSE, type="conditional",
ylab= "Metabolic Rate (nmol/gs)",xlab="Temperature(°C)", gg=TRUE) +theme_classic(base_size=15)+ scale_color_discrete(name="Group",
breaks=c("former_ovigerous", "infected", "nonovigerous"),labels=c("Former Ovigerous", "Infected", "Nonovigerous"))



