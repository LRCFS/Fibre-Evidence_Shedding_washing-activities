# To clean the Global environment
rm(list=ls()) 

#############################################################
#####               Library requirement                 #####
#############################################################
library(plyr)
library(dplyr)
library(ggplot2)
library(extrafont)
library(RColorBrewer)

#############################################################
#####                  Data analysis                    #####
#############################################################

# read .csv file with the transfer fibre count 
FibreCount_transfer_G1 <- read.csv('./Transfer_Fibre_Count.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')

#### GRAPH  ####
pAtr <- ggplot(FibreCount_transfer_G1, aes(x=group, y=value)) +
  geom_boxplot() +
  stat_summary(fun = mean, colour="darkred",
               geom="point",position=position_dodge(width=0.75)) +
  stat_summary(fun = mean, colour="darkred", aes(group=1),
               geom="line", lwd=1, lty=1) +
  labs(x="\nWash number", y="Number of Fibre\n") +
  theme_classic(base_family = "Arial", base_size = 14) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.background = element_rect(fill="grey95",size=1, linetype="solid", colour="grey80"),
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5))
show(pAtr)
ggsave("Fibre Count boxplot_ATr.png", pAtr, width = 6, height = 7, units = "in", dpi=150, path = "Results")

# read .csv file with the shedding fibre count 
FibreCount_Shedding_G1 <- read.csv('./Shedding_Fibre_Count.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')

#### GRAPH  ####
pSH <- ggplot(FibreCount_Shedding_G1, aes(x = factor(Weight, level = c('100g', '200g', '400g','800g','1000g','2000g')),
                                             y= meanArea, fill=Condition))+
  geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label = meanArea), hjust=0.5,vjust = -9.5,position = position_dodge(.9))+
  labs(x="\nWeight", y="Total fibre area (mm\u00b2)\n") +
  theme_bw(base_family = "Arial", base_size = 12) +
  ylim(0,500)+
  scale_fill_grey(start = 0.4,end = 0.9)+
  theme(legend.title = element_blank(),
        strip.background.x = element_rect(colour = "grey", fill = "white"),
        legend.position = "bottom",
        legend.background = element_rect(fill="grey95",size=1, linetype="solid", colour="grey80"),
        axis.text.x = element_text(angle = 0, vjust = 0.95, hjust=0.5))+
  geom_errorbar(aes(ymin=meanArea-SD, ymax=meanArea+SD),width=.2,position=position_dodge(.9))
pSH
ggsave("Shedding_W000-5.png", pSH, width = 10, height = 9, units = "in", dpi=150, path = "Results")
