#####Script for calculating the growth rate of parasites between each 
##David Clark
##20230409


#####Summary#####

#This code is used to calculate the growth rate between the multiple 

#####Load in packages needed for data wrangling and calulating growthrates#####
#Visualization
library(ggplot2)
#Data wrangling
library(dplyr)
library(plyr)
library(tidyverse)
library(tidylog)

##### Load in data set containing all infection information for calculating the growth rate#####

VIEGrowthRate <- read_csv("2021Feb_VIE_Exp_DRC_V3.csv")

#####Delete all columns not needed for calculating the growth rate of worms#####
#Setting day as a factor
VIEGrowthRate$day <- as.factor(VIEGrowthRate$day)

VIEGrowthRate2<- VIEGrowthRate%>%
  #Filtering down to only fish used in the behavioral experiment portion of the experiment
  filter(Behav == 1)%>%
  #Filtering down to only fish who were infected
  filter(Infection == "I")%>%
  #Selecting only relevant columns for determining the worm growth rate on a fish
  select(fishID, totworm, day)%>%
  #Pivoting wider so we can actually calculate the growth fo worms
  pivot_wider(names_from = day, 
              values_from = totworm)

#####Now calculate the change in worms between these points#####

VIEGrowthRate3<-VIEGrowthRate2%>%
  #Taking the growth of worms between the different counts for each fish
  #Naming convention will be a bit weird but making pivoting longer easier
  mutate(
    Growth_1= ((VIEGrowthRate2$`2` - VIEGrowthRate2$`0`)/2),
    Growth_2= ((VIEGrowthRate2$`4` - VIEGrowthRate2$`2`)/2),
    Growth_3= ((VIEGrowthRate2$`6` - VIEGrowthRate2$`4`)/2),
    Growth_4= ((VIEGrowthRate2$`8` - VIEGrowthRate2$`6`)/2),
    Growth_4= ((VIEGrowthRate2$`9` - VIEGrowthRate2$`6`)/3),
    Growth_5= ((VIEGrowthRate2$`11` - VIEGrowthRate2$`8`)/3),
    Growth_6= ((VIEGrowthRate2$`13` - VIEGrowthRate2$`11`)/2),
    Growth_7= ((VIEGrowthRate2$`15` - VIEGrowthRate2$`13`)/2),
    Growth_8= ((VIEGrowthRate2$`17` - VIEGrowthRate2$`15`)/2),
    Growth_8= ((VIEGrowthRate2$`18` - VIEGrowthRate2$`15`)/2),
    Growth_9= ((VIEGrowthRate2$`20` - VIEGrowthRate2$`17`)/3),
    Growth_9= ((VIEGrowthRate2$`20` - VIEGrowthRate2$`18`)/3),
  )


#View(VIEGrowthRate3)

#Drop old columns and then pivot longer so all growthrates and times are in the same columns in a longer format
VIEGrowthRate3 <- VIEGrowthRate3 %>%
  #Getting rid of old columsn
  select(-c(`0`,`2`,`4`,`6`,`8`,`9`,`11`,`13`,`15`,`17`,`18`,`20`,`22`))%>%
  #Pivoting everything into longer format
  pivot_longer(
    cols = starts_with("Growth"),
    names_to = "Time",
    names_prefix = "Growth_",
    values_to = "GrowthRate"
  )%>%
  drop_na()


#Take a look at the rates to determine recovery of fish
# View(VIEGrowthRate3)

#Just for viewing the total worm numbers for different times
VIEGrowthRateB<- VIEGrowthRate %>%
  filter(Behav==1)%>%
  filter(Infection == "I")%>%
  select(fishID, day, totworm, CountDay, ContInf)

#Setting fishID as a factor
VIEGrowthRateB$fishID<-as.factor(VIEGrowthRateB$fishID)

#Visualizing worm burdens over time to make sure individuals we think are controling infection are actually controling infection
ggplot(VIEGrowthRateB, aes(x=day, y=totworm, group=fishID, color=fishID))+geom_smooth(se=FALSE)+facet_wrap(~ContInf)

#filtering down to just uncontroled infection to make sure all trajectories make sense.

VIEGrowthRateUC <- VIEGrowthRateB%>%
  filter(ContInf == 0)

#Visualizing worm burdens over time to make sure individuals we think are controling infection are actually controling infection
ggplot(VIEGrowthRateUC, aes(x=day, y=totworm, group=fishID, color=fishID))+geom_smooth(se=FALSE)+facet_wrap(~ConInf)

