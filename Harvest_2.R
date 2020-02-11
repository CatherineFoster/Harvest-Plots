####Written by Catherine Foster, Summer 2019
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

df <- read.csv(file="S:\\Research Centres\\CASCADE\\Projects\\513323 Systematic Reviews\\Family Group Conferencing\\Report\\Harvest_Plots\\Harvest_Plot_data_v5.csv",
               header=TRUE, sep=",",stringsAsFactors = TRUE)

df <- read.csv(file="S:\\Research Centres\\CASCADE\\Projects\\513323 Systematic Reviews\\Family Group Conferencing\\Report\\Harvest_Plots\\HP_bytype.csv",
               header=TRUE, sep=",",stringsAsFactors = TRUE)

Care_Entry <- df[ which(df$MeetType=="TDM" & df$Outcome=="3"), ]
#Care_Entry <- df[ which(df$Outcome=="1"), ]
df_CE <- data.frame(Care_Entry)

df_CE$Effect <- factor(df_CE$Effect, levels = c("Intervention", "No Difference", "Control"))
df_CE$ROB <- factor(df_CE$ROB, levels = c("High", "Moderate", "Low"))


ggplot(df_CE, aes(x=Number, y=Qual_num, fill=ROB,width=3.5)) +
  geom_bar(stat="identity",position = position_dodge2(preserve="single", padding=.6,width=1)) +
  geom_hline(yintercept=df$Qual_num, linetype="solid", color = "black") +
  facet_grid(~Effect,drop=FALSE) + 
  xlab('') + ylab('') +
  scale_y_continuous(breaks=NULL) +
  #ggtitle("Which Service (Shared Decision-Making Meetings or Control Services) \n Had Fewer Out of Home Placements?") +
  #ggtitle("Which Service Had Fewer Instances of Care Re-Entry?") +
  #ggtitle("Which Service Resulted in More Children Returning Home?") +
  #ggtitle("Which Service Resulted in Greater Family Empowerment?") +
  #ggtitle("Which Service Resulted in Greater Family Satisfaction \n with Child Welfare Services?") +
  #ggtitle("Which Service Resulted in Fewer Referrals for Child Maltreatment?") +
  #ggtitle("Do Family Team Conferences Reduce Care Entry \n When Compared with Control Services?") +
  ggtitle("Do Team Decision Making Meetings Result in More Children \n Returning Home When Compared with Control Services?") +
  #ggtitle("Do Team Decision Making Meetings Result in Fewer Referrals \n for Child Maltreatment When Compared with Control Services?") +
  scale_fill_manual(values=c( "#ff7057", "#5b9b54","#88d0d9"))+
  theme(plot.title = element_text(hjust = 0.5,face="bold", size=14),
        axis.title.y = element_text(size=11, face="bold"), axis.title.x = element_text(face="bold"),
        axis.ticks = element_blank(),
        strip.text = element_text(face="bold", size=12),
        legend.text=element_text(face="bold", size=12),legend.title = element_text(face="bold", size=14), 
        axis.text.x = element_blank(), 
        axis.text.y = element_text(size=11,face="bold")) +  
  geom_text(aes(label=StudyNumber), position=position_dodge2(padding=.6, width=1), vjust=2,hjust=0.5,fontface="bold",size=3) +
  geom_text(aes(label=Asterisk),position=position_dodge2(padding=.6, width=2), vjust=-0.5,hjust=0.5,fontface="bold", size=3) +
  guides(fill=guide_legend(title="Risk of Bias")) 




#ggplot(df_CE, aes(x=Number, y=Qual_num, fill=ROB,width=6)) +
#geom_bar(stat="identity",position = position_dodge2(preserve="single", padding=.6,width=1)) +
#geom_hline(yintercept=df$Qual_num, linetype="solid", color = "black") +
#facet_grid(~Effect,drop=FALSE) + 
#xlab('') + ylab('') +
#scale_y_continuous(breaks=NULL) +
#ggtitle("Do Family Group Decision Making Meetings Reduce Care Entry When Compared with Control Services?") +
#ggtitle("Do FTC Meetings Result in More Children Returning Home \n When Compared with Control Services?") +
#ggtitle("Do FUM Meetings Result in Fewer Referrals for Maltreatment \n When Compared with Control Services?") +
#ggtitle("Are shared decision-making family meetings effective in reducing \n out-of-home placements in families of children <18?") +
#ggtitle("Are shared decision-making family meetings effective at \n improving family empowerment ?") +
#ggtitle("Are shared decision-making family meetings meetings effective in reducing \n care re-entry in families of children <18?") +
#ggtitle("Are shared decision-making family meetings effective at increasing \n reunification in families of children <18?") +
#ggtitle("Do shared decision-making family meetings result in fewer referrals for child maltreatment?") +
#scale_fill_manual(values=c("red", "yellow", "green"))+
#theme(plot.title = element_text(hjust = 0.5,face="bold"),
#  axis.title.y = element_text(size=11, face="bold"), axis.title.x = element_text(face="bold"),
#  axis.ticks = element_blank(),
#  strip.text = element_text(face="bold", size=12),
#  legend.text=element_text(face="bold", size=12),legend.title = element_text(face="bold", size=14), 
#  axis.text.x = element_blank(), 
#  axis.text.y = element_text(size=11,face="bold")) +  
# geom_text(aes(label=StudyNumber), position=position_dodge2(padding=.6, width=1), vjust=2,hjust=0.5,fontface="bold") +
# geom_text(aes(label=Asterisk),position=position_dodge2(padding=.6, width=2), vjust=-0.5,hjust=0.5,fontface="bold") +
# guides(fill=guide_legend(title="Risk of Bias")) 

#########################################################################################
