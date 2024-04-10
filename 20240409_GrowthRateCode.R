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
    one_0to2= ((VIEGrowthRate2$`2` - VIEGrowthRate2$`0`)/2),
    two_2to4= ((VIEGrowthRate2$`4` - VIEGrowthRate2$`2`)/2),
    three_4to6= ((VIEGrowthRate2$`6` - VIEGrowthRate2$`4`)/2),
    four_6to8= ((VIEGrowthRate2$`8` - VIEGrowthRate2$`6`)/2),
    four_6to9= ((VIEGrowthRate2$`9` - VIEGrowthRate2$`6`)/3),
    five_8to11= ((VIEGrowthRate2$`11` - VIEGrowthRate2$`8`)/3),
    six_11to13= ((VIEGrowthRate2$`13` - VIEGrowthRate2$`11`)/2),
    seven_13to15= ((VIEGrowthRate2$`15` - VIEGrowthRate2$`13`)/2),
    eight_15to17= ((VIEGrowthRate2$`17` - VIEGrowthRate2$`15`)/2),
    eight_15to18= ((VIEGrowthRate2$`18` - VIEGrowthRate2$`15`)/2),
    nine_17to20= ((VIEGrowthRate2$`20` - VIEGrowthRate2$`17`)/3),
    nine_18to20= ((VIEGrowthRate2$`20` - VIEGrowthRate2$`18`)/3),
  )


View(VIEGrowthRate)
