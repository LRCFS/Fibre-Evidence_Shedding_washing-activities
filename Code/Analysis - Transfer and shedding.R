#################################################################################################
#####                           Fibre Analysis 100% cotton jumper                           #####
#################################################################################################
#########################################################
#####                  Data cleaning                #####
#########################################################
#### Assign a Coder to each wash ####
W000_Dataset$Coder <- "W000"
W001_Dataset$Coder <- "W001"
W002_Dataset$Coder <- "W002"
W003_Dataset$Coder <- "W003"
W004_Dataset$Coder <- "W004"
W005_Dataset$Coder <- "W005"
# W006_Dataset$Coder <- "W006"
# W007_Dataset$Coder <- "W007"
# W008_Dataset$Coder <- "W008"
# W009_Dataset$Coder <- "W009"
# W010_Dataset$Coder <- "W010"
# W011_Dataset$Coder <- "W011"
# W012_Dataset$Coder <- "W012"
# W013_Dataset$Coder <- "W013"
# W014_Dataset$Coder <- "W014"
# W015_Dataset$Coder <- "W015"

# Negative controls
W000negative <- W000_Dataset %>% filter(grepl('negative', Sample))
W001negative <- W001_Dataset %>% filter(grepl('negative', Sample))
W002negative <- W002_Dataset %>% filter(grepl('negative', Sample))
W003negative <- W003_Dataset %>% filter(grepl('negative', Sample))
W004negative <- W004_Dataset %>% filter(grepl('negative', Sample))
W005negative <- W005_Dataset %>% filter(grepl('negative', Sample))
# W006negative <- W006_Dataset %>% filter(grepl('negative', Sample))
# W007negative <- W007_Dataset %>% filter(grepl('negative', Sample))
# W008negative <- W008_Dataset %>% filter(grepl('negative', Sample))
# W009negative <- W009_Dataset %>% filter(grepl('negative', Sample))
# W010negative <- W010_Dataset %>% filter(grepl('negative', Sample))
# W011negative <- W011_Dataset %>% filter(grepl('negative', Sample))
# W012negative <- W012_Dataset %>% filter(grepl('negative', Sample))
# W013negative <- W013_Dataset %>% filter(grepl('negative', Sample))
# W014negative <- W014_Dataset %>% filter(grepl('negative', Sample))
# W015negative <- W015_Dataset %>% filter(grepl('negative', Sample))

# Create a data frame "Negativecontrol" with all the negative controls 
Negativecontrol <- rbind(W000negative,W001negative, W002negative, W003negative, W004negative, W005negative)

# Calculate the number of background fibres
Negativecontrol$Diff <- Negativecontrol$`After transfer` - Negativecontrol$`Before transfer`
# Negativecontrol$Diff :  if value = 0, no difference. if value > 0, fibre there before transfer and not after.
# if value < 0, fibre there after transfer but not before (contamination)
Negativecontrol2 <- aggregate(Negativecontrol$Diff,list(Negativecontrol$Diff), FUN=length)
names(Negativecontrol2) <- c("value","appearance")
Negativecontrol2$percentnodiff <- Negativecontrol2$appearance/(sum(Negativecontrol2$appearance))*100
Negativecontrol2$percentnodiff<-round(Negativecontrol2$percent, digit =2)
Negativecontrol$Sample<- gsub("_negative","",Negativecontrol$Sample)

# plot the negative controls
pNegativecontrol <- ggplot(Negativecontrol, aes(x = Sample, y = Diff)) + 
  geom_bar(stat="identity", position=position_dodge(), width = 0.5) + 
  geom_hline(yintercept=0,linetype="dashed", color = "black")+
  labs(y= "Difference between Before transfer and after transfer\n", x="\nWash")+
  ylim(-3,3)+
  scale_x_discrete(labels = every_n_labeler(0), breaks =every_n_labeler(5)) +
  theme_bw(base_size = 12)+
  theme(panel.grid.major.y = element_blank(),
        legend.position="bottom",
        legend.title = element_blank(),
        legend.text = element_text(size=10),
        legend.box.background = element_rect(color="grey", size=2),
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5))+
  guides(fill=guide_legend(nrow=2, byrow=TRUE))
show(pNegativecontrol)

# Positive controls
W000positive <- W000_Dataset %>% filter(grepl('positive', Sample))
W001positive <- W001_Dataset %>% filter(grepl('positive', Sample))
W002positive <- W002_Dataset %>% filter(grepl('positive', Sample))
W003positive <- W003_Dataset %>% filter(grepl('positive', Sample))
W004positive <- W004_Dataset %>% filter(grepl('positive', Sample))
W005positive <- W005_Dataset %>% filter(grepl('positive', Sample))
# W006positive <- W006_Dataset %>% filter(grepl('positive', Sample))
# W007positive <- W007_Dataset %>% filter(grepl('positive', Sample))
# W008positive <- W008_Dataset %>% filter(grepl('positive', Sample))
# W009positive <- W009_Dataset %>% filter(grepl('positive', Sample))
# W010positive <- W010_Dataset %>% filter(grepl('positive', Sample))
# W011positive <- W011_Dataset %>% filter(grepl('positive', Sample))
# W012positive <- W012_Dataset %>% filter(grepl('positive', Sample))
# W013positive <- W013_Dataset %>% filter(grepl('positive', Sample))
# W014positive <- W014_Dataset %>% filter(grepl('positive', Sample))
# W015positive <- W015_Dataset %>% filter(grepl('positive', Sample))

Positivecontrol <- rbind(W000positive,W001positive, W002positive, W003positive, W004positive, W005positive)
Positivecontrol$Diff <- Positivecontrol$`After transfer` - Positivecontrol$`Before transfer`

# if value = 0, no difference. if value > 0, fibre there before and not after. if value < 0, fibre there not before but after
# Count to number of time the same year is repeated in the "AuthorListIFSMSExtended$Year" and save in a data.frame "Year" 
Positivecontrol2 <- aggregate(Positivecontrol$Diff,list(Positivecontrol$Diff), FUN=length)
names(Positivecontrol2) <- c("value","appearance")
Positivecontrol2$percentnodiff <- Positivecontrol2$appearance/(sum(Positivecontrol2$appearance))*100
Positivecontrol2$percentnodiff<-round(Positivecontrol2$percent, digit =2)

Positivecontrol$Sample<- gsub("_positive","",Positivecontrol$Sample)

ppositivecontrol <- ggplot(Positivecontrol, aes(x = Sample, y = Diff)) + 
  geom_bar(stat="identity", position=position_dodge(), width = 0.5) + 
  geom_hline(yintercept=0,linetype="dashed", color = "black")+
  labs(y= "Difference between Before transfer and after transfer\n", x="\nWash")+
  ylim(-3,3)+
  scale_x_discrete(labels = every_n_labeler(0), breaks =every_n_labeler(5)) +
  theme_bw(base_size = 12)+
  theme(panel.grid.major.y = element_blank(),
        legend.position="bottom",
        legend.title = element_blank(),
        legend.text = element_text(size=10),
        legend.box.background = element_rect(color="grey", size=2),
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5))+
  guides(fill=guide_legend(nrow=2, byrow=TRUE))
show(ppositivecontrol)

# Combined results from positive and negative controls
pControls_pending <- ggarrange(ppositivecontrol+ rremove("ylab") + rremove("xlab"),
                               pNegativecontrol+ rremove("ylab") + rremove("xlab"),
                               labels = c("Positive control","Negative control"),
                               common.legend = TRUE, legend = "bottom",
                               align = "hv",
                               ncol = 1, nrow = 2,
                               font.label = list(size = 12, color = "black", family = "Arial", position = "top"),
                               hjust=-0.3,vjust=2)

pControls <- annotate_figure(pControls_pending, left = textGrob("Number of fibres", rot = 90, vjust = 0.5, hjust = 0.5, gp = gpar(cex =1)),
                               bottom = textGrob("Wash number", vjust = 0.5, hjust = 0.5,gp = gpar(cex = 1)));pControls
# ggsave("Controls.png", pControls, width = 6, height = 6, units = "in", dpi=150, path = "Results")

#### Creating dataset with only column Coder and After transfer after removing the positive and negative control ####
forFibreCount0<- W000_Dataset[!(W000_Dataset$Sample=="MP_W000_G1_positive_B" | W000_Dataset$Sample=="MP_W000_G1_negative_B"),]
forFibreCount1<- W001_Dataset[!(W001_Dataset$Sample=="MP_W001_G1_positive_B" | W001_Dataset$Sample=="MP_W001_G1_negative_B"),]
forFibreCount2<- W002_Dataset[!(W002_Dataset$Sample=="MP_W002_G1_positive_B" | W002_Dataset$Sample=="MP_W002_G1_negative_B"),]
forFibreCount3<- W003_Dataset[!(W003_Dataset$Sample=="MP_W003_G1_positive_B" | W003_Dataset$Sample=="MP_W003_G1_negative_B"),]
forFibreCount4<- W004_Dataset[!(W004_Dataset$Sample=="MP_W004_G1_positive_B" | W004_Dataset$Sample=="MP_W004_G1_negative_B"),]
forFibreCount5<- W005_Dataset[!(W005_Dataset$Sample=="MP_W005_G1_positive_B" | W005_Dataset$Sample=="MP_W005_G1_negative_B"),]

#########################################################
#####           ANALYSE OF THE BACKGROUND           #####
#########################################################
#### dplyr::select the column Before only in each dataframe ####
BackgroundW000 <- forFibreCount0 %>%
  dplyr::select(Coder,`Before transfer`)
BackgroundW001 <- forFibreCount1 %>%
  dplyr::select(Coder,`Before transfer`)
BackgroundW002 <- forFibreCount2 %>%
  dplyr::select(Coder,`Before transfer`)
BackgroundW003 <- forFibreCount3 %>%
  dplyr::select(Coder,`Before transfer`)
BackgroundW004 <- forFibreCount4 %>%
  dplyr::select(Coder,`Before transfer`)
BackgroundW005 <- forFibreCount5 %>%
  dplyr::select(Coder,`Before transfer`)
# BackgroundW006 <- forFibreCount6 %>%
#   dplyr::select(Coder,`Before transfer`)
# BackgroundW007 <- forFibreCount7 %>%
#   dplyr::select(Coder,`Before transfer`)
# BackgroundW008 <- forFibreCount8 %>%
#   dplyr::select(Coder,`Before transfer`)
# BackgroundW009 <- forFibreCount9 %>%
#   dplyr::select(Coder,`Before transfer`)
# BackgroundW010 <- forFibreCount10 %>%
#   dplyr::select(Coder,`Before transfer`)
# BackgroundW011 <- forFibreCount11 %>%
#   dplyr::select(Coder,`Before transfer`)
# BackgroundW012 <- forFibreCount12 %>%
#   dplyr::select(Coder,`Before transfer`)
# BackgroundW013 <- forFibreCount13 %>%
#   dplyr::select(Coder,`Before transfer`)
# BackgroundW014<- forFibreCount14 %>%
#   dplyr::select(Coder,`Before transfer`)
# BackgroundW015<- forFibreCount15 %>%
#   dplyr::select(Coder,`Before transfer`)

#### Combine all the dataframe ####
BackgroundFibreCount <- rbind(BackgroundW000, BackgroundW001, BackgroundW002, BackgroundW003, BackgroundW004,BackgroundW005)
names(BackgroundFibreCount) <- c("group", "value")
# write.table(TransferFibreCount, file = "Fibre Count - Transfer.csv", quote = F, sep = ",", row.names = F)

# Count the number of fibres found on the background images
BackgroundFibreCountfibres <- aggregate(BackgroundFibreCount$group,list(BackgroundFibreCount$value), FUN=length) # W011: NA

#########################################################
#####      Number of fibres following transfer      #####
#########################################################
#### select the column Transfer only in each dataframe ####
TransferW000 <- forFibreCount0 %>%
  dplyr::select(Coder,`After transfer`)
TransferW001 <- forFibreCount1 %>%
  dplyr::select(Coder,`After transfer`)
TransferW002 <- forFibreCount2 %>%
  dplyr::select(Coder,`After transfer`)
TransferW003 <- forFibreCount3 %>%
  dplyr::select(Coder,`After transfer`)
TransferW004 <- forFibreCount4 %>%
  dplyr::select(Coder,`After transfer`)
TransferW005 <- forFibreCount5 %>%
  dplyr::select(Coder,`After transfer`)
# TransferW006 <- forFibreCount6 %>%
#   dplyr::select(Coder,`After transfer`)
# TransferW007 <- forFibreCount7 %>%
#   dplyr::select(Coder,`After transfer`)
# TransferW008 <- forFibreCount8 %>%
#   dplyr::select(Coder,`After transfer`)
# TransferW009 <- forFibreCount9 %>%
#   dplyr::select(Coder,`After transfer`)
# TransferW010 <- forFibreCount10 %>%
#   dplyr::select(Coder,`After transfer`)
# TransferW011 <- forFibreCount11 %>%
#   dplyr::select(Coder,`After transfer`)
# TransferW012 <- forFibreCount12 %>%
#   dplyr::select(Coder,`After transfer`)
# TransferW013 <- forFibreCount13 %>%
#   dplyr::select(Coder,`After transfer`)
# TransferW014<- forFibreCount14 %>%
#   dplyr::select(Coder,`After transfer`)
# TransferW015<- forFibreCount15 %>%
#   dplyr::select(Coder,`After transfer`)

#### Combine all the dataframe ####
TransferFibreCount <- rbind(TransferW000, TransferW001, TransferW002, TransferW003, TransferW004,TransferW005)
names(TransferFibreCount) <- c("group", "value")
# write.table(TransferFibreCount, file = "Fibre Count - Transfer.csv", quote = F, sep = ",", row.names = F)

#### Bar plots of data by group - distribution ####
histogram(~ value | group,data=TransferFibreCount,layout=c(3,3),
          xlab="Number of fibres")

#### Create a table with descriptive statistics ####
meanAtr <- aggregate(value ~  group, TransferFibreCount, function(x) {round(mean(x), digits=2)})
SDAtr <- aggregate(value ~  group, TransferFibreCount, function(x) {round(SD(x), digits=2)})
SD2Atr <- round(sqrt((SDAtr$value^2)+(0.95^2)),digits=2)
medianAtr <- aggregate(value ~  group, TransferFibreCount, median)
datatableAtr <- cbind(meanAtr, medianAtr$value, SDAtr$value, SD2Atr)
names(datatableAtr) <- c("Wash number", "Average", "median", "SD", "SD2")
datatableAtr$Forthesis <- paste(datatableAtr$Average, datatableAtr$SD, sep=" ± ")
#write.table(datatableAtr, file = "Stats_Atr red.csv", quote = F, sep = ",", row.names = F)

#### GRAPH - FIGURE 4-8 ####
pAtr <- ggplot(TransferFibreCount, aes(x=group, y=value)) +
  geom_boxplot() +
  #scale_x_discrete(labels = every_n_labeler(5)) +
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
# ggplotly(pAtr)
ggsave("Fibre Count boxplot_ATr.png", pAtr, width = 6, height = 7, units = "in", dpi=150, path = "Results")

#########################################################
#####                  SHEDDING TEST                #####
#########################################################
Shedding <- read.csv('./Fibre count Summary/SH_W000-W003_Summary.csv', sep="," ,header = T,fileEncoding="UTF-8-BOM")
Shedding$Slice<- gsub(".TIF","",Shedding$Slice)
SheddingExtended <- data.frame(str_split(Shedding$Slice, "_", simplify=TRUE))
names(SheddingExtended) <- c("Project","Wash","Garment","Weight","Repeat","condition")
SheddingTotal <- cbind(SheddingExtended,Area=Shedding$Total.Area)

# Convert Area from inch2 to mm2
# 1 pixel = 1 x10^-5 inch2, so (Area*1)/0.000011 to convert into px
SheddingTotal$Area.px <- (SheddingTotal$Area*1)/0.000011
# 1 mm = 110 pixels, 1mm2 = 12100 px
SheddingTotal$Area.mm2 <- SheddingTotal$Area.px/12100

# Split data per garments
SheddingW000 <- SheddingTotal[SheddingTotal$Wash =='W000',]
SheddingW001 <- SheddingTotal[SheddingTotal$Wash =='W001',]
SheddingW003 <- SheddingTotal[SheddingTotal$Wash =='W003',]

# split per weight - unwashed
DataAreaSheddingW000.100 <- SheddingW000[SheddingW000$Weight =='100g',]
DataAreaSheddingW000.200 <- SheddingW000[SheddingW000$Weight =='200g',]
DataAreaSheddingW000.400 <- SheddingW000[SheddingW000$Weight =='400g',]
DataAreaSheddingW000.800 <- SheddingW000[SheddingW000$Weight =='800g',]
DataAreaSheddingW000.1000 <- SheddingW000[SheddingW000$Weight =='1000g',]
DataAreaSheddingW000.2000 <- SheddingW000[SheddingW000$Weight =='2000g',]

DataAreaSheddingW001.100 <- SheddingW001[SheddingW001$Weight =='100g',]
DataAreaSheddingW001.200 <- SheddingW001[SheddingW001$Weight =='200g',]
DataAreaSheddingW001.400 <- SheddingW001[SheddingW001$Weight =='400g',]
DataAreaSheddingW001.800 <- SheddingW001[SheddingW001$Weight =='800g',]
DataAreaSheddingW001.1000 <- SheddingW001[SheddingW001$Weight =='1000g',]
DataAreaSheddingW001.2000 <- SheddingW001[SheddingW001$Weight =='2000g',]

DataAreaSheddingW003.100 <- SheddingW003[SheddingW003$Weight =='100g',]
DataAreaSheddingW003.200 <- SheddingW003[SheddingW003$Weight =='200g',]
DataAreaSheddingW003.400 <- SheddingW003[SheddingW003$Weight =='400g',]
DataAreaSheddingW003.800 <- SheddingW003[SheddingW003$Weight =='800g',]
DataAreaSheddingW003.1000 <- SheddingW003[SheddingW003$Weight =='1000g',]
DataAreaSheddingW003.2000 <- SheddingW003[SheddingW003$Weight =='2000g',]

# Calculation of mean and SD
meanDataAreaSheddingW000.100<- data.frame(meanArea=round(mean(DataAreaSheddingW000.100$Area.mm2),digits =2 ))
meanDataAreaSheddingW000.100$SD<- round(sd(DataAreaSheddingW000.100$Area.mm2),digits =2 )
meanDataAreaSheddingW000.100$Weight <- "100g"
meanDataAreaSheddingW000.200<- data.frame(meanArea=round(mean(DataAreaSheddingW000.200$Area.mm2),digits =2 ))
meanDataAreaSheddingW000.200$SD<- round(sd(DataAreaSheddingW000.200$Area.mm2),digits =2 )
meanDataAreaSheddingW000.200$Weight <- "200g"
meanDataAreaSheddingW000.400<- data.frame(meanArea=round(mean(DataAreaSheddingW000.400$Area.mm2),digits =2 ))
meanDataAreaSheddingW000.400$SD<- round(sd(DataAreaSheddingW000.400$Area.mm2),digits =2 )
meanDataAreaSheddingW000.400$Weight <- "400g"
meanDataAreaSheddingW000.800<- data.frame(meanArea=round(mean(DataAreaSheddingW000.800$Area.mm2),digits =2 ))
meanDataAreaSheddingW000.800$SD<- round(sd(DataAreaSheddingW000.800$Area.mm2),digits =2 )
meanDataAreaSheddingW000.800$Weight <- "800g"
meanDataAreaSheddingW000.1000<- data.frame(meanArea=round(mean(DataAreaSheddingW000.1000$Area.mm2),digits =2 ))
meanDataAreaSheddingW000.1000$SD<- round(sd(DataAreaSheddingW000.1000$Area.mm2),digits =2 )
meanDataAreaSheddingW000.1000$Weight <- "1000g"
meanDataAreaSheddingW000.2000<- data.frame(meanArea=round(mean(DataAreaSheddingW000.2000$Area.mm2),digits =2 ))
meanDataAreaSheddingW000.2000$SD<- round(sd(DataAreaSheddingW000.2000$Area.mm2),digits =2 )
meanDataAreaSheddingW000.2000$Weight <- "2000g"

meanDataAreaSheddingW001.100<- data.frame(meanArea=round(mean(DataAreaSheddingW001.100$Area.mm2),digits =2 ))
meanDataAreaSheddingW001.100$SD<- round(sd(DataAreaSheddingW001.100$Area.mm2),digits =2 )
meanDataAreaSheddingW001.100$Weight <- "100g"
meanDataAreaSheddingW001.200<- data.frame(meanArea=round(mean(DataAreaSheddingW001.200$Area.mm2),digits =2 ))
meanDataAreaSheddingW001.200$SD<- round(sd(DataAreaSheddingW001.200$Area.mm2),digits =2 )
meanDataAreaSheddingW001.200$Weight <- "200g"
meanDataAreaSheddingW001.400<- data.frame(meanArea=round(mean(DataAreaSheddingW001.400$Area.mm2),digits =2 ))
meanDataAreaSheddingW001.400$SD<- round(sd(DataAreaSheddingW001.400$Area.mm2),digits =2 )
meanDataAreaSheddingW001.400$Weight <- "400g"
meanDataAreaSheddingW001.800<- data.frame(meanArea=round(mean(DataAreaSheddingW001.800$Area.mm2),digits =2 ))
meanDataAreaSheddingW001.800$SD<- round(sd(DataAreaSheddingW001.800$Area.mm2),digits =2 )
meanDataAreaSheddingW001.800$Weight <- "800g"
meanDataAreaSheddingW001.1000<- data.frame(meanArea=round(mean(DataAreaSheddingW001.1000$Area.mm2),digits =2 ))
meanDataAreaSheddingW001.1000$SD<- round(sd(DataAreaSheddingW001.1000$Area.mm2),digits =2 )
meanDataAreaSheddingW001.1000$Weight <- "1000g"
meanDataAreaSheddingW001.2000<- data.frame(meanArea=round(mean(DataAreaSheddingW001.2000$Area.mm2),digits =2 ))
meanDataAreaSheddingW001.2000$SD<- round(sd(DataAreaSheddingW001.2000$Area.mm2),digits =2 )
meanDataAreaSheddingW001.2000$Weight <- "2000g"

meanDataAreaSheddingW003.100<- data.frame(meanArea=round(mean(DataAreaSheddingW003.100$Area.mm2),digits =2 ))
meanDataAreaSheddingW003.100$SD<- round(sd(DataAreaSheddingW003.100$Area.mm2),digits =2 )
meanDataAreaSheddingW003.100$Weight <- "100g"
meanDataAreaSheddingW003.200<- data.frame(meanArea=round(mean(DataAreaSheddingW003.200$Area.mm2),digits =2 ))
meanDataAreaSheddingW003.200$SD<- round(sd(DataAreaSheddingW003.200$Area.mm2),digits =2 )
meanDataAreaSheddingW003.200$Weight <- "200g"
meanDataAreaSheddingW003.400<- data.frame(meanArea=round(mean(DataAreaSheddingW003.400$Area.mm2),digits =2 ))
meanDataAreaSheddingW003.400$SD<- round(sd(DataAreaSheddingW003.400$Area.mm2),digits =2 )
meanDataAreaSheddingW003.400$Weight <- "400g"
meanDataAreaSheddingW003.800<- data.frame(meanArea=round(mean(DataAreaSheddingW003.800$Area.mm2),digits =2 ))
meanDataAreaSheddingW003.800$SD<- round(sd(DataAreaSheddingW003.800$Area.mm2),digits =2 )
meanDataAreaSheddingW003.800$Weight <- "800g"
meanDataAreaSheddingW003.1000<- data.frame(meanArea=round(mean(DataAreaSheddingW003.1000$Area.mm2),digits =2 ))
meanDataAreaSheddingW003.1000$SD<- round(sd(DataAreaSheddingW003.1000$Area.mm2),digits =2 )
meanDataAreaSheddingW003.1000$Weight <- "1000g"
meanDataAreaSheddingW003.2000<- data.frame(meanArea=round(mean(DataAreaSheddingW003.2000$Area.mm2),digits =2 ))
meanDataAreaSheddingW003.2000$SD<- round(sd(DataAreaSheddingW003.2000$Area.mm2),digits =2 )
meanDataAreaSheddingW003.2000$Weight <- "2000g"

# Combined data sets
DataW000total <- rbind(meanDataAreaSheddingW000.100,meanDataAreaSheddingW000.200,meanDataAreaSheddingW000.400,
                              meanDataAreaSheddingW000.800,meanDataAreaSheddingW000.1000,meanDataAreaSheddingW000.2000)
DataW000total$Condition <- "W000"

DataW001total <- rbind(meanDataAreaSheddingW001.100,meanDataAreaSheddingW001.200,meanDataAreaSheddingW001.400,
                       meanDataAreaSheddingW001.800,meanDataAreaSheddingW001.1000,meanDataAreaSheddingW001.2000)
DataW001total$Condition <- "W001"

DataW003total <- rbind(meanDataAreaSheddingW003.100,meanDataAreaSheddingW003.200,meanDataAreaSheddingW003.400,
                       meanDataAreaSheddingW003.800,meanDataAreaSheddingW003.1000,meanDataAreaSheddingW003.2000)
DataW003total$Condition <- "W003"

DataAreaCombined <- rbind(DataW000total,DataW001total, DataW003total)

#### PLOT ####
PlotShedding <- ggplot(DataAreaCombined, aes(x = factor(Weight, level = c('100g', '200g', '400g','800g','1000g','2000g')),
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
PlotShedding
ggsave("Shedding_W000-3.png", PlotShedding, width = 10, height = 9, units = "in", dpi=150, path = "Results")
