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
  filter(Behav == 1)%>%
  filter(Infection == "I")%>%
  select(fishID, totworm, day)%>%
  pivot_wider(names_from = day, 
              values_from = totworm)

#####Now calculate the change in worms between these points#####
VIEGrowthRate3<-VIEGrowthRate2%>%
  mutate(
    one_0to2= ((VIEGrowthRate2$`2` - VIEGrowthRate2$`0`)/2),
    two_2to4= ((VIEGrowthRate2$`4` - VIEGrowthRate2$`2`)/2),
    three_4to6= ((VIEGrowthRate2$`6` - VIEGrowthRate2$`4`)/2),
    
    
  )


View(VIEGrowthRate)
