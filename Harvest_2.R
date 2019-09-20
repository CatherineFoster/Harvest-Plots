####Written by Catherine Foster, August 2019
####fosterc6@cardiff.ac.uk

#clear env.
rm(list=ls())

#######Load tools###################
library(ggplot2) #plotting
library(patternplot) #bar patterns
library(ggthemes) #plotting
library(devtools) #code to R package
library(dplyr) #data manipulation
library(plyr) # as above
library(r2d3) #visualisation
library(tidyr) #layout changing etc.
library(tidyverse)
library(RColorBrewer) #color schemes for plotting
library(lubridate) #dates and times
library(leaflet) #maps
library(dygraphs) #timelines
library(ggmap) #google maps tools
library(car)

###Stats packages
library(latex2exp, warn.conflicts = FALSE)
library(multcomp, warn.conflicts = FALSE)
library(pander, warn.conflicts = FALSE)
library(papeR, warn.conflicts = FALSE)
library(stats) #stats!
library(psych) #more stats stuff
library(rcompanion) #support functions for stats
library(lsmeans)
library(openair)
library(zoo)
library(chron)
library(plyr)
library(matrixStats)
library(Rcapture)
library(ggpubr)
#######################################################################################
##load csv file containing data for Harvest plots
df <- read.csv(file="S:\\MyData\\Harvest_plot_data.csv",
               header=TRUE, sep=",",stringsAsFactors = TRUE)

##Subset to just plot 1 outcome measure
Care_Entry <- df[ which(df$Outcome=='1'), ]

##Make it a data frame - not strictly necessary
df_CE <- data.frame(Care_Entry)

##Order left to right on Harvest plot
df_CE$Effect <- factor(df_CE$Effect, levels = c("Intervention", "No Difference", "Control"))

###Not strictly necessary but reminder of levels present
df_CE$ROB <- factor(df_CE$ROB, levels = c("High", "Moderate", "Low"))

###ggplot with facets for Effect
ggplot(df_CE, aes(x=Number, y=Qual_num, fill=ROB,width=3)) +
  geom_bar(stat="identity",position = position_dodge2(preserve="single", padding=.6,width=1)) +
  geom_hline(yintercept=df$Qual_num, linetype="solid", color = "black") +
  facet_grid(~Effect,drop=FALSE) + 
  xlab('') + ylab('') +
  scale_y_continuous(breaks=NULL) +
  ggtitle("Are Young People More Likely to Enter Care \n Following Shared Decision-Making Meetings than CAU?") +
  scale_fill_manual(values=c("red", "yellow", "green"))+
  theme(plot.title = element_text(hjust = 0.5,face="bold"),
        axis.title.y = element_text(size=11, face="bold"), axis.title.x = element_text(face="bold"),
        axis.ticks = element_blank(),
        strip.text = element_text(face="bold", size=12),
        legend.text=element_text(face="bold", size=12),legend.title = element_text(face="bold", size=14), 
        axis.text.x = element_blank(), 
        axis.text.y = element_text(size=11,face="bold")) +  
  geom_text(aes(label=StudyNumber), position=position_dodge2(padding=.6, width=1), vjust=2,hjust=0.5,fontface="bold") +
  geom_text(aes(label=Asterisk),position=position_dodge2(padding=.6, width=2), vjust=-0.5,hjust=0.5,fontface="bold") +
  guides(fill=guide_legend(title="Risk of Bias")) 





