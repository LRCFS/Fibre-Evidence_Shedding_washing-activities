#################################################################################################
#####                                FIBRE ANALYSIS GARMENT 1                               #####
#################################################################################################

#### Assign a Coder to each wash ####
W000_G1_Dataset$Coder <- "W000"
W001_G1_Dataset$Coder <- "W001"
W002_G1_Dataset$Coder <- "W002"
W003_G1_Dataset$Coder <- "W003"
W004_G1_Dataset$Coder <- "W004"
W005_G1_Dataset$Coder <- "W005"
W006_G1_Dataset$Coder <- "W006"
W007_G1_Dataset$Coder <- "W007"

#########################################################
#####                     Controls                  #####
#########################################################
# Negative controls
W000negative <- W000_G1_Dataset %>% filter(grepl('negative', Sample))
W001negative <- W001_G1_Dataset %>% filter(grepl('negative', Sample))
W002negative <- W002_G1_Dataset %>% filter(grepl('negative', Sample))
W003negative <- W003_G1_Dataset %>% filter(grepl('negative', Sample))
W004negative <- W004_G1_Dataset %>% filter(grepl('negative', Sample))
W005negative <- W005_G1_Dataset %>% filter(grepl('negative', Sample))
W006negative <- W006_G1_Dataset %>% filter(grepl('negative', Sample))
W007negative <- W007_G1_Dataset %>% filter(grepl('negative', Sample))

# Create a data frame "Negativecontrol" with all the negative controls 
Negativecontrol <- rbind(W000negative,W001negative, W002negative,W003negative,W004negative,W005negative,W006negative,W007negative)

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
W000positive <- W000_G1_Dataset %>% filter(grepl('positive', Sample))
W001positive <- W001_G1_Dataset %>% filter(grepl('positive', Sample))
W002positive <- W002_G1_Dataset %>% filter(grepl('positive', Sample))
W003positive <- W003_G1_Dataset %>% filter(grepl('positive', Sample))
W004positive <- W004_G1_Dataset %>% filter(grepl('positive', Sample))
W005positive <- W005_G1_Dataset %>% filter(grepl('positive', Sample))
W006positive <- W006_G1_Dataset %>% filter(grepl('positive', Sample))
W007positive <- W007_G1_Dataset %>% filter(grepl('positive', Sample))

Positivecontrol <- rbind(W000positive,W001positive, W002positive, W003positive, W004positive, W005positive,W006positive,W007positive)
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

### GRAPH ###
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
forFibreCount0<- W000_G1_Dataset[!(W000_G1_Dataset$Sample=="MP_W000_G1_positive_B" | W000_G1_Dataset$Sample=="MP_W000_G1_negative_B"),]
forFibreCount1<- W001_G1_Dataset[!(W001_G1_Dataset$Sample=="MP_W001_G1_positive_B" | W001_G1_Dataset$Sample=="MP_W001_G1_negative_B"),]
forFibreCount2<- W002_G1_Dataset[!(W002_G1_Dataset$Sample=="MP_W002_G1_positive_B" | W002_G1_Dataset$Sample=="MP_W002_G1_negative_B"),]
forFibreCount3<- W003_G1_Dataset[!(W003_G1_Dataset$Sample=="MP_W003_G1_positive_B" | W003_G1_Dataset$Sample=="MP_W003_G1_negative_B"),]
forFibreCount4<- W004_G1_Dataset[!(W004_G1_Dataset$Sample=="MP_W004_G1_positive_B" | W004_G1_Dataset$Sample=="MP_W004_G1_negative_B"),]
forFibreCount5<- W005_G1_Dataset[!(W005_G1_Dataset$Sample=="MP_W005_G1_positive_B" | W005_G1_Dataset$Sample=="MP_W005_G1_negative_B"),]
forFibreCount6<- W006_G1_Dataset[!(W006_G1_Dataset$Sample=="MP_W006_G1_positive_B" | W006_G1_Dataset$Sample=="MP_W006_G1_negative_B"),]
forFibreCount7<- W007_G1_Dataset[!(W007_G1_Dataset$Sample=="MP_W007_G1_positive_B" | W007_G1_Dataset$Sample=="MP_W007_G1_negative_B"),]

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
BackgroundW006 <- forFibreCount6 %>%
  dplyr::select(Coder,`Before transfer`)
BackgroundW007 <- forFibreCount7 %>%
  dplyr::select(Coder,`Before transfer`)

#### Combine all the dataframe ####
BackgroundFibreCount <- rbind(BackgroundW000, BackgroundW001, BackgroundW002,BackgroundW003,BackgroundW004,
                              BackgroundW005,BackgroundW006,BackgroundW007)
names(BackgroundFibreCount) <- c("group", "value")
# write.table(TransferFibreCount, file = "Fibre Count - Transfer.csv", quote = F, sep = ",", row.names = F)

# Count the number of fibres found on the background images
BackgroundFibreCountfibres <- aggregate(BackgroundFibreCount$group,list(BackgroundFibreCount$value), FUN=length) # W011: NA

#########################################################
#####      NUMBER OF FIBRES FOLLOWING TRANSFER      #####
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
TransferW006 <- forFibreCount6 %>%
  dplyr::select(Coder,`After transfer`)
TransferW007 <- forFibreCount7 %>%
  dplyr::select(Coder,`After transfer`)

#### Combine all the dataframe ####
TransferFibreCount <- rbind(TransferW000, TransferW001, TransferW002,TransferW003,TransferW004,TransferW005,
                            TransferW006,TransferW007)
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

write.table(TransferFibreCount, file = "Transfer_Fibre_Count.csv", quote = F, sep = ",", row.names = F)

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
ggsave("Fibre Count boxplot_ATr_G1.png", pAtr, width = 6, height = 7, units = "in", dpi=150, path = "Results")

#########################################################
#####                  SHEDDING TEST                #####
#########################################################
Shedding <- read.csv('./Fibre count Summary/SH_G1_W000-W007_Summary.csv', sep="," ,header = T,fileEncoding="UTF-8-BOM")
Shedding$Slice<- gsub(".TIF","",Shedding$Slice)
SheddingExtended <- data.frame(str_split(Shedding$Slice, "_", simplify=TRUE))
names(SheddingExtended) <- c("Project","Wash","Garment","Weight","Repeat","condition")
SheddingTotal <- cbind(SheddingExtended,Area=Shedding$Total.Area)

# Convert Area from inch2 to mm2
# 1 pixel = 1 x10^-5 inch2, so (Area*1)/0.000011 to convert into px
SheddingTotal$Area.px <- (SheddingTotal$Area*1)/0.000011
# 1 mm = 112 pixels, 1mm2 = 12544 px
SheddingTotal$Area.mm2 <- SheddingTotal$Area.px/12544

# Split data per washing condition
DataAreaW000 <- SheddingTotal[SheddingTotal$Wash =='W000',]
DataAreaW001 <- SheddingTotal[SheddingTotal$Wash =='W001',]
DataAreaW003 <- SheddingTotal[SheddingTotal$Wash =='W003',]
DataAreaW005 <- SheddingTotal[SheddingTotal$Wash =='W005',]
DataAreaW007 <- SheddingTotal[SheddingTotal$Wash =='W007',]

# split per weight - 
DataAreaW000_G1_100 <- DataAreaW000[DataAreaW000$Weight =='100g',]
DataAreaW000_G1_200 <- DataAreaW000[DataAreaW000$Weight =='200g',]
DataAreaW000_G1_400 <- DataAreaW000[DataAreaW000$Weight =='400g',]
DataAreaW000_G1_800 <- DataAreaW000[DataAreaW000$Weight =='800g',]
DataAreaW000_G1_1000 <- DataAreaW000[DataAreaW000$Weight =='1000g',]
DataAreaW000_G1_2000 <- DataAreaW000[DataAreaW000$Weight =='2000g',]

DataAreaW001_G1_100 <- DataAreaW001[DataAreaW001$Weight =='100g',]
DataAreaW001_G1_200 <- DataAreaW001[DataAreaW001$Weight =='200g',]
DataAreaW001_G1_400 <- DataAreaW001[DataAreaW001$Weight =='400g',]
DataAreaW001_G1_800 <- DataAreaW001[DataAreaW001$Weight =='800g',]
DataAreaW001_G1_1000 <- DataAreaW001[DataAreaW001$Weight =='1000g',]
DataAreaW001_G1_2000 <- DataAreaW001[DataAreaW001$Weight =='2000g',]

DataAreaW003_G1_100 <- DataAreaW003[DataAreaW003$Weight =='100g',]
DataAreaW003_G1_200 <- DataAreaW003[DataAreaW003$Weight =='200g',]
DataAreaW003_G1_400 <- DataAreaW003[DataAreaW003$Weight =='400g',]
DataAreaW003_G1_800 <- DataAreaW003[DataAreaW003$Weight =='800g',]
DataAreaW003_G1_1000 <- DataAreaW003[DataAreaW003$Weight =='1000g',]
DataAreaW003_G1_2000 <- DataAreaW003[DataAreaW003$Weight =='2000g',]

DataAreaW005_G1_100 <- DataAreaW005[DataAreaW005$Weight =='100g',]
DataAreaW005_G1_200 <- DataAreaW005[DataAreaW005$Weight =='200g',]
DataAreaW005_G1_400 <- DataAreaW005[DataAreaW005$Weight =='400g',]
DataAreaW005_G1_800 <- DataAreaW005[DataAreaW005$Weight =='800g',]
DataAreaW005_G1_1000 <- DataAreaW005[DataAreaW005$Weight =='1000g',]
DataAreaW005_G1_2000 <- DataAreaW005[DataAreaW005$Weight =='2000g',]

DataAreaW007_G1_100 <- DataAreaW007[DataAreaW007$Weight =='100g',]
DataAreaW007_G1_200 <- DataAreaW007[DataAreaW007$Weight =='200g',]
DataAreaW007_G1_400 <- DataAreaW007[DataAreaW007$Weight =='400g',]
DataAreaW007_G1_800 <- DataAreaW007[DataAreaW007$Weight =='800g',]
DataAreaW007_G1_1000 <- DataAreaW007[DataAreaW007$Weight =='1000g',]
DataAreaW007_G1_2000 <- DataAreaW007[DataAreaW007$Weight =='2000g',]

# Calculation of mean and SD
meanDataAreaW000_G1_100<- data.frame(meanArea=round(mean(DataAreaW000_G1_100$Area.mm2),digits =2 ))
meanDataAreaW000_G1_100$SD<- round(sd(DataAreaW000_G1_100$Area.mm2),digits =2 )
meanDataAreaW000_G1_100$Weight <- "100g"
meanDataAreaW000_G1_200<- data.frame(meanArea=round(mean(DataAreaW000_G1_200$Area.mm2),digits =2 ))
meanDataAreaW000_G1_200$SD<- round(sd(DataAreaW000_G1_200$Area.mm2),digits =2 )
meanDataAreaW000_G1_200$Weight <- "200g"
meanDataAreaW000_G1_400<- data.frame(meanArea=round(mean(DataAreaW000_G1_400$Area.mm2),digits =2 ))
meanDataAreaW000_G1_400$SD<- round(sd(DataAreaW000_G1_400$Area.mm2),digits =2 )
meanDataAreaW000_G1_400$Weight <- "400g"
meanDataAreaW000_G1_800<- data.frame(meanArea=round(mean(DataAreaW000_G1_800$Area.mm2),digits =2 ))
meanDataAreaW000_G1_800$SD<- round(sd(DataAreaW000_G1_800$Area.mm2),digits =2 )
meanDataAreaW000_G1_800$Weight <- "800g"
meanDataAreaW000_G1_1000<- data.frame(meanArea=round(mean(DataAreaW000_G1_1000$Area.mm2),digits =2 ))
meanDataAreaW000_G1_1000$SD<- round(sd(DataAreaW000_G1_1000$Area.mm2),digits =2 )
meanDataAreaW000_G1_1000$Weight <- "1000g"
meanDataAreaW000_G1_2000<- data.frame(meanArea=round(mean(DataAreaW000_G1_2000$Area.mm2),digits =2 ))
meanDataAreaW000_G1_2000$SD<- round(sd(DataAreaW000_G1_2000$Area.mm2),digits =2 )
meanDataAreaW000_G1_2000$Weight <- "2000g"

meanDataAreaW001_G1_100<- data.frame(meanArea=round(mean(DataAreaW001_G1_100$Area.mm2),digits =2 ))
meanDataAreaW001_G1_100$SD<- round(sd(DataAreaW001_G1_100$Area.mm2),digits =2 )
meanDataAreaW001_G1_100$Weight <- "100g"
meanDataAreaW001_G1_200<- data.frame(meanArea=round(mean(DataAreaW001_G1_200$Area.mm2),digits =2 ))
meanDataAreaW001_G1_200$SD<- round(sd(DataAreaW001_G1_200$Area.mm2),digits =2 )
meanDataAreaW001_G1_200$Weight <- "200g"
meanDataAreaW001_G1_400<- data.frame(meanArea=round(mean(DataAreaW001_G1_400$Area.mm2),digits =2 ))
meanDataAreaW001_G1_400$SD<- round(sd(DataAreaW001_G1_400$Area.mm2),digits =2 )
meanDataAreaW001_G1_400$Weight <- "400g"
meanDataAreaW001_G1_800<- data.frame(meanArea=round(mean(DataAreaW001_G1_800$Area.mm2),digits =2 ))
meanDataAreaW001_G1_800$SD<- round(sd(DataAreaW001_G1_800$Area.mm2),digits =2 )
meanDataAreaW001_G1_800$Weight <- "800g"
meanDataAreaW001_G1_1000<- data.frame(meanArea=round(mean(DataAreaW001_G1_1000$Area.mm2),digits =2 ))
meanDataAreaW001_G1_1000$SD<- round(sd(DataAreaW001_G1_1000$Area.mm2),digits =2 )
meanDataAreaW001_G1_1000$Weight <- "1000g"
meanDataAreaW001_G1_2000<- data.frame(meanArea=round(mean(DataAreaW001_G1_2000$Area.mm2),digits =2 ))
meanDataAreaW001_G1_2000$SD<- round(sd(DataAreaW001_G1_2000$Area.mm2),digits =2 )
meanDataAreaW001_G1_2000$Weight <- "2000g"

meanDataAreaW003_G1_100<- data.frame(meanArea=round(mean(DataAreaW003_G1_100$Area.mm2),digits =2 ))
meanDataAreaW003_G1_100$SD<- round(sd(DataAreaW003_G1_100$Area.mm2),digits =2 )
meanDataAreaW003_G1_100$Weight <- "100g"
meanDataAreaW003_G1_200<- data.frame(meanArea=round(mean(DataAreaW003_G1_200$Area.mm2),digits =2 ))
meanDataAreaW003_G1_200$SD<- round(sd(DataAreaW003_G1_200$Area.mm2),digits =2 )
meanDataAreaW003_G1_200$Weight <- "200g"
meanDataAreaW003_G1_400<- data.frame(meanArea=round(mean(DataAreaW003_G1_400$Area.mm2),digits =2 ))
meanDataAreaW003_G1_400$SD<- round(sd(DataAreaW003_G1_400$Area.mm2),digits =2 )
meanDataAreaW003_G1_400$Weight <- "400g"
meanDataAreaW003_G1_800<- data.frame(meanArea=round(mean(DataAreaW003_G1_800$Area.mm2),digits =2 ))
meanDataAreaW003_G1_800$SD<- round(sd(DataAreaW003_G1_800$Area.mm2),digits =2 )
meanDataAreaW003_G1_800$Weight <- "800g"
meanDataAreaW003_G1_1000<- data.frame(meanArea=round(mean(DataAreaW003_G1_1000$Area.mm2),digits =2 ))
meanDataAreaW003_G1_1000$SD<- round(sd(DataAreaW003_G1_1000$Area.mm2),digits =2 )
meanDataAreaW003_G1_1000$Weight <- "1000g"
meanDataAreaW003_G1_2000<- data.frame(meanArea=round(mean(DataAreaW003_G1_2000$Area.mm2),digits =2 ))
meanDataAreaW003_G1_2000$SD<- round(sd(DataAreaW003_G1_2000$Area.mm2),digits =2 )
meanDataAreaW003_G1_2000$Weight <- "2000g"

meanDataAreaW005_G1_100<- data.frame(meanArea=round(mean(DataAreaW005_G1_100$Area.mm2),digits =2 ))
meanDataAreaW005_G1_100$SD<- round(sd(DataAreaW005_G1_100$Area.mm2),digits =2 )
meanDataAreaW005_G1_100$Weight <- "100g"
meanDataAreaW005_G1_200<- data.frame(meanArea=round(mean(DataAreaW005_G1_200$Area.mm2),digits =2 ))
meanDataAreaW005_G1_200$SD<- round(sd(DataAreaW005_G1_200$Area.mm2),digits =2 )
meanDataAreaW005_G1_200$Weight <- "200g"
meanDataAreaW005_G1_400<- data.frame(meanArea=round(mean(DataAreaW005_G1_400$Area.mm2),digits =2 ))
meanDataAreaW005_G1_400$SD<- round(sd(DataAreaW005_G1_400$Area.mm2),digits =2 )
meanDataAreaW005_G1_400$Weight <- "400g"
meanDataAreaW005_G1_800<- data.frame(meanArea=round(mean(DataAreaW005_G1_800$Area.mm2),digits =2 ))
meanDataAreaW005_G1_800$SD<- round(sd(DataAreaW005_G1_800$Area.mm2),digits =2 )
meanDataAreaW005_G1_800$Weight <- "800g"
meanDataAreaW005_G1_1000<- data.frame(meanArea=round(mean(DataAreaW005_G1_1000$Area.mm2),digits =2 ))
meanDataAreaW005_G1_1000$SD<- round(sd(DataAreaW005_G1_1000$Area.mm2),digits =2 )
meanDataAreaW005_G1_1000$Weight <- "1000g"
meanDataAreaW005_G1_2000<- data.frame(meanArea=round(mean(DataAreaW005_G1_2000$Area.mm2),digits =2 ))
meanDataAreaW005_G1_2000$SD<- round(sd(DataAreaW005_G1_2000$Area.mm2),digits =2 )
meanDataAreaW005_G1_2000$Weight <- "2000g"

meanDataAreaW007_G1_100<- data.frame(meanArea=round(mean(DataAreaW007_G1_100$Area.mm2),digits =2 ))
meanDataAreaW007_G1_100$SD<- round(sd(DataAreaW007_G1_100$Area.mm2),digits =2 )
meanDataAreaW007_G1_100$Weight <- "100g"
meanDataAreaW007_G1_200<- data.frame(meanArea=round(mean(DataAreaW007_G1_200$Area.mm2),digits =2 ))
meanDataAreaW007_G1_200$SD<- round(sd(DataAreaW007_G1_200$Area.mm2),digits =2 )
meanDataAreaW007_G1_200$Weight <- "200g"
meanDataAreaW007_G1_400<- data.frame(meanArea=round(mean(DataAreaW007_G1_400$Area.mm2),digits =2 ))
meanDataAreaW007_G1_400$SD<- round(sd(DataAreaW007_G1_400$Area.mm2),digits =2 )
meanDataAreaW007_G1_400$Weight <- "400g"
meanDataAreaW007_G1_800<- data.frame(meanArea=round(mean(DataAreaW007_G1_800$Area.mm2),digits =2 ))
meanDataAreaW007_G1_800$SD<- round(sd(DataAreaW007_G1_800$Area.mm2),digits =2 )
meanDataAreaW007_G1_800$Weight <- "800g"
meanDataAreaW007_G1_1000<- data.frame(meanArea=round(mean(DataAreaW007_G1_1000$Area.mm2),digits =2 ))
meanDataAreaW007_G1_1000$SD<- round(sd(DataAreaW007_G1_1000$Area.mm2),digits =2 )
meanDataAreaW007_G1_1000$Weight <- "1000g"
meanDataAreaW007_G1_2000<- data.frame(meanArea=round(mean(DataAreaW007_G1_2000$Area.mm2),digits =2 ))
meanDataAreaW007_G1_2000$SD<- round(sd(DataAreaW007_G1_2000$Area.mm2),digits =2 )
meanDataAreaW007_G1_2000$Weight <- "2000g"

# Combined data sets
DataW000total <- rbind(meanDataAreaW000_G1_100,meanDataAreaW000_G1_200,meanDataAreaW000_G1_400,
                              meanDataAreaW000_G1_800,meanDataAreaW000_G1_1000,meanDataAreaW000_G1_2000)
DataW000total$Condition <- "W000"

DataW001total <- rbind(meanDataAreaW001_G1_100,meanDataAreaW001_G1_200,meanDataAreaW001_G1_400,
                              meanDataAreaW001_G1_800,meanDataAreaW001_G1_1000,meanDataAreaW001_G1_2000)
DataW001total$Condition <- "W001"

DataW003total <- rbind(meanDataAreaW003_G1_100,meanDataAreaW003_G1_200,meanDataAreaW003_G1_400,
                              meanDataAreaW003_G1_800,meanDataAreaW003_G1_1000,meanDataAreaW003_G1_2000)
DataW003total$Condition <- "W003"

DataW005total <- rbind(meanDataAreaW005_G1_100,meanDataAreaW005_G1_200,meanDataAreaW005_G1_400,
                              meanDataAreaW005_G1_800,meanDataAreaW005_G1_1000,meanDataAreaW005_G1_2000)
DataW005total$Condition <- "W005"

DataW007total <- rbind(meanDataAreaW007_G1_100,meanDataAreaW007_G1_200,meanDataAreaW007_G1_400,
                       meanDataAreaW007_G1_800,meanDataAreaW007_G1_1000,meanDataAreaW007_G1_2000)
DataW007total$Condition <- "W007"

FibreCount_Shedding_G1 <- rbind(DataW000total,DataW001total,DataW003total,DataW005total,DataW007total)

write.table(FibreCount_Shedding_G1, file = "Shedding_Fibre_Count.csv", quote = F, sep = ",", row.names = F)

#### PLOT ####
# calculation of the percentage difference between washed and unwashed
y = rep(c(125, 150, 175, 200, 225),2)
x = rep(c(1:5), 2)

# plot
pSH <- ggplot(FibreCount_Shedding_G1, aes(x = factor(Weight, level = c('100g', '200g', '400g','800g','1000g','2000g')),
                                          y= meanArea, fill=Condition))+
  geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label = meanArea), hjust=0.5,vjust = -5.5,position = position_dodge(.9))+
  labs(x="\nWeight", y="Total fibre area (mm\u00b2)\n") +
  theme_bw(base_family = "Arial", base_size = 12) +
  ylim(0,500)+
  scale_fill_manual("legend", values = c("W000" = "#FED976", "W001" = "#FEB24C","W003" = "#FD8D3C", "W005" = "#FC4E2A","W007" = "#E31A1C"))+ # to obtain the colour brewer.pal(9, "YlOrRd")
  theme(legend.title = element_blank(),
        strip.background.x = element_rect(colour = "grey", fill = "white"),
        legend.position = "bottom",
        legend.background = element_rect(fill="grey95",size=1, linetype="solid", colour="grey80"),
        axis.text.x = element_text(angle = 0, vjust = 0.95, hjust=0.5))+
  geom_errorbar(aes(ymin=meanArea-SD, ymax=meanArea+SD),width=.2,position=position_dodge(.9))
pSH
ggsave("Shedding_G1_W000-7.png", pSH, width = 10, height = 9, units = "in", dpi=150, path = "Results")

#-------------------------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------------------------
  
#################################################################################################
#####                                FIBRE ANALYSIS GARMENT 2                               #####
#################################################################################################
#### Assign a Coder to each wash ####
W000_G2_Dataset$Coder <- "W000"
W001_G2_Dataset$Coder <- "W001"
W002_G2_Dataset$Coder <- "W002"
# W003_G2_Dataset$Coder <- "W003"
# W004_G2_Dataset$Coder <- "W004"
# W005_G2_Dataset$Coder <- "W005"
# W006_G2_Dataset$Coder <- "W006"
# W007_G2_Dataset$Coder <- "W007"

#########################################################
#####                     Controls                  #####
#########################################################
# Negative controls
W000negative <- W000_G2_Dataset %>% filter(grepl('negative', Sample))
W001negative <- W001_G2_Dataset %>% filter(grepl('negative', Sample))
W002negative <- W002_G2_Dataset %>% filter(grepl('negative', Sample))
# W003negative <- W003_G2_Dataset %>% filter(grepl('negative', Sample))
# W004negative <- W004_G2_Dataset %>% filter(grepl('negative', Sample))
# W005negative <- W005_G2_Dataset %>% filter(grepl('negative', Sample))
# W006negative <- W006_G2_Dataset %>% filter(grepl('negative', Sample))
# W007negative <- W007_G2_Dataset %>% filter(grepl('negative', Sample))

# Create a data frame "Negativecontrol" with all the negative controls 
Negativecontrol <- rbind(W000negative,W001negative, W002negative) #W003negative,W004negative,W005negative,W006negative,W007negative)

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
W000positive <- W000_G2_Dataset %>% filter(grepl('positive', Sample))
W001positive <- W001_G2_Dataset %>% filter(grepl('positive', Sample))
W002positive <- W002_G2_Dataset %>% filter(grepl('positive', Sample))
# W003positive <- W003_G2_Dataset %>% filter(grepl('positive', Sample))
# W004positive <- W004_G2_Dataset %>% filter(grepl('positive', Sample))
# W005positive <- W005_G2_Dataset %>% filter(grepl('positive', Sample))
# W006positive <- W006_G2_Dataset %>% filter(grepl('positive', Sample))
# W007positive <- W007_G2_Dataset %>% filter(grepl('positive', Sample))

Positivecontrol <- rbind(W000positive,W001positive, W002positive) # W003positive, W004positive, W005positive,W006positive,W007positive)
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

### GRAPH ###
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
forFibreCount0<- W000_G2_Dataset[!(W000_G2_Dataset$Sample=="MP_W000_G2_G2_positive_B" | W000_G2_Dataset$Sample=="MP_W000_G2_G2_negative_B"),]
forFibreCount1<- W001_G2_Dataset[!(W001_G2_Dataset$Sample=="MP_W001_G2_G2_positive_B" | W001_G2_Dataset$Sample=="MP_W001_G2_G2_negative_B"),]
forFibreCount2<- W002_G2_Dataset[!(W002_G2_Dataset$Sample=="MP_W002_G2_G2_positive_B" | W002_G2_Dataset$Sample=="MP_W002_G2_G2_negative_B"),]
# forFibreCount3<- W003_G2_Dataset[!(W003_G2_Dataset$Sample=="MP_W003_G2_G2_positive_B" | W003_G2_Dataset$Sample=="MP_W003_G2_G2_negative_B"),]
# forFibreCount4<- W004_G2_Dataset[!(W004_G2_Dataset$Sample=="MP_W004_G2_G2_positive_B" | W004_G2_Dataset$Sample=="MP_W004_G2_G2_negative_B"),]
# forFibreCount5<- W005_G2_Dataset[!(W005_G2_Dataset$Sample=="MP_W005_G2_G2_positive_B" | W005_G2_Dataset$Sample=="MP_W005_G2_G2_negative_B"),]
# forFibreCount6<- W006_G2_Dataset[!(W006_G2_Dataset$Sample=="MP_W006_G2_G2_positive_B" | W006_G2_Dataset$Sample=="MP_W006_G2_G2_negative_B"),]
# forFibreCount7<- W007_G2_Dataset[!(W007_G2_Dataset$Sample=="MP_W007_G2_G2_positive_B" | W007_G2_Dataset$Sample=="MP_W007_G2_G2_negative_B"),]

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
# BackgroundW003 <- forFibreCount3 %>%
#   dplyr::select(Coder,`Before transfer`)
# BackgroundW004 <- forFibreCount4 %>%
#   dplyr::select(Coder,`Before transfer`)
# BackgroundW005 <- forFibreCount5 %>%
#   dplyr::select(Coder,`Before transfer`)
# BackgroundW006 <- forFibreCount6 %>%
#   dplyr::select(Coder,`Before transfer`)
# BackgroundW007 <- forFibreCount7 %>%
#   dplyr::select(Coder,`Before transfer`)

#### Combine all the dataframe ####
BackgroundFibreCount <- rbind(BackgroundW000, BackgroundW001, BackgroundW002) #,BackgroundW003,BackgroundW004,BackgroundW005,BackgroundW006,BackgroundW007)
names(BackgroundFibreCount) <- c("group", "value")
# write.table(TransferFibreCount, file = "Fibre Count - Transfer.csv", quote = F, sep = ",", row.names = F)

# Count the number of fibres found on the background images
BackgroundFibreCountfibres <- aggregate(BackgroundFibreCount$group,list(BackgroundFibreCount$value), FUN=length) # W011: NA

#########################################################
#####      NUMBER OF FIBRES FOLLOWING TRANSFER      #####
#########################################################
#### select the column Transfer only in each dataframe ####
TransferW000 <- forFibreCount0 %>%
  dplyr::select(Coder,`After transfer`)
TransferW001 <- forFibreCount1 %>%
  dplyr::select(Coder,`After transfer`)
TransferW002 <- forFibreCount2 %>%
  dplyr::select(Coder,`After transfer`)
# TransferW003 <- forFibreCount3 %>%
#   dplyr::select(Coder,`After transfer`)
# TransferW004 <- forFibreCount4 %>%
#   dplyr::select(Coder,`After transfer`)
# TransferW005 <- forFibreCount5 %>%
#   dplyr::select(Coder,`After transfer`)
# TransferW006 <- forFibreCount6 %>%
#   dplyr::select(Coder,`After transfer`)
# TransferW007 <- forFibreCount7 %>%
#   dplyr::select(Coder,`After transfer`)

#### Combine all the dataframe ####
TransferFibreCount <- rbind(TransferW000, TransferW001, TransferW002) #TransferW003,TransferW004,TransferW005,TransferW006,TransferW007)
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

write.table(TransferFibreCount, file = "Transfer_Fibre_Count.csv", quote = F, sep = ",", row.names = F)

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
ggsave("Fibre Count boxplot_ATr_G2.png", pAtr, width = 6, height = 7, units = "in", dpi=150, path = "Results")

#########################################################
#####                  SHEDDING TEST                #####
#########################################################
Shedding <- read.csv('./Fibre count Summary/SH_G2_W000-W003_Summary.csv', sep="," ,header = T,fileEncoding="UTF-8-BOM")
Shedding$Slice<- gsub(".TIF","",Shedding$Slice)
SheddingExtended <- data.frame(str_split(Shedding$Slice, "_", simplify=TRUE))
names(SheddingExtended) <- c("Project","Wash","Garment","Weight","Repeat","condition")
SheddingTotal <- cbind(SheddingExtended,Area=Shedding$Total.Area)

# Convert Area from inch2 to mm2
# 1 pixel = 1 x10^-5 inch2, so (Area*1)/0.000011 to convert into px
SheddingTotal$Area.px <- (SheddingTotal$Area*1)/0.000011
# 1 mm = 112 pixels, 1mm2 = 12544 px
SheddingTotal$Area.mm2 <- SheddingTotal$Area.px/12544

# Split data per washing condition
DataAreaW000 <- SheddingTotal[SheddingTotal$Wash =='W000',]
DataAreaW001 <- SheddingTotal[SheddingTotal$Wash =='W001',]
DataAreaW003 <- SheddingTotal[SheddingTotal$Wash =='W003',]
# DataAreaW005 <- SheddingTotal[SheddingTotal$Wash =='W005',]
# DataAreaW007 <- SheddingTotal[SheddingTotal$Wash =='W007',]

# split per weight - 
DataAreaW000_G2_100 <- DataAreaW000[DataAreaW000$Weight =='100g',]
DataAreaW000_G2_200 <- DataAreaW000[DataAreaW000$Weight =='200g',]
DataAreaW000_G2_400 <- DataAreaW000[DataAreaW000$Weight =='400g',]
DataAreaW000_G2_800 <- DataAreaW000[DataAreaW000$Weight =='800g',]
DataAreaW000_G2_1000 <- DataAreaW000[DataAreaW000$Weight =='1000g',]
DataAreaW000_G2_2000 <- DataAreaW000[DataAreaW000$Weight =='2000g',]

DataAreaW001_G2_100 <- DataAreaW001[DataAreaW001$Weight =='100g',]
DataAreaW001_G2_200 <- DataAreaW001[DataAreaW001$Weight =='200g',]
DataAreaW001_G2_400 <- DataAreaW001[DataAreaW001$Weight =='400g',]
DataAreaW001_G2_800 <- DataAreaW001[DataAreaW001$Weight =='800g',]
DataAreaW001_G2_1000 <- DataAreaW001[DataAreaW001$Weight =='1000g',]
DataAreaW001_G2_2000 <- DataAreaW001[DataAreaW001$Weight =='2000g',]

DataAreaW003_G2_100 <- DataAreaW003[DataAreaW003$Weight =='100g',]
DataAreaW003_G2_200 <- DataAreaW003[DataAreaW003$Weight =='200g',]
DataAreaW003_G2_400 <- DataAreaW003[DataAreaW003$Weight =='400g',]
DataAreaW003_G2_800 <- DataAreaW003[DataAreaW003$Weight =='800g',]
DataAreaW003_G2_1000 <- DataAreaW003[DataAreaW003$Weight =='1000g',]
DataAreaW003_G2_2000 <- DataAreaW003[DataAreaW003$Weight =='2000g',]

# DataAreaW005_G2_100 <- DataAreaW005[DataAreaW005$Weight =='100g',]
# DataAreaW005_G2_200 <- DataAreaW005[DataAreaW005$Weight =='200g',]
# DataAreaW005_G2_400 <- DataAreaW005[DataAreaW005$Weight =='400g',]
# DataAreaW005_G2_800 <- DataAreaW005[DataAreaW005$Weight =='800g',]
# DataAreaW005_G2_1000 <- DataAreaW005[DataAreaW005$Weight =='1000g',]
# DataAreaW005_G2_2000 <- DataAreaW005[DataAreaW005$Weight =='2000g',]
# 
# DataAreaW007_G2_100 <- DataAreaW007[DataAreaW007$Weight =='100g',]
# DataAreaW007_G2_200 <- DataAreaW007[DataAreaW007$Weight =='200g',]
# DataAreaW007_G2_400 <- DataAreaW007[DataAreaW007$Weight =='400g',]
# DataAreaW007_G2_800 <- DataAreaW007[DataAreaW007$Weight =='800g',]
# DataAreaW007_G2_1000 <- DataAreaW007[DataAreaW007$Weight =='1000g',]
# DataAreaW007_G2_2000 <- DataAreaW007[DataAreaW007$Weight =='2000g',]

# Calculation of mean and SD
meanDataAreaW000_G2_100<- data.frame(meanArea=round(mean(DataAreaW000_G2_100$Area.mm2),digits =2 ))
meanDataAreaW000_G2_100$SD<- round(sd(DataAreaW000_G2_100$Area.mm2),digits =2 )
meanDataAreaW000_G2_100$Weight <- "100g"
meanDataAreaW000_G2_200<- data.frame(meanArea=round(mean(DataAreaW000_G2_200$Area.mm2),digits =2 ))
meanDataAreaW000_G2_200$SD<- round(sd(DataAreaW000_G2_200$Area.mm2),digits =2 )
meanDataAreaW000_G2_200$Weight <- "200g"
meanDataAreaW000_G2_400<- data.frame(meanArea=round(mean(DataAreaW000_G2_400$Area.mm2),digits =2 ))
meanDataAreaW000_G2_400$SD<- round(sd(DataAreaW000_G2_400$Area.mm2),digits =2 )
meanDataAreaW000_G2_400$Weight <- "400g"
meanDataAreaW000_G2_800<- data.frame(meanArea=round(mean(DataAreaW000_G2_800$Area.mm2),digits =2 ))
meanDataAreaW000_G2_800$SD<- round(sd(DataAreaW000_G2_800$Area.mm2),digits =2 )
meanDataAreaW000_G2_800$Weight <- "800g"
meanDataAreaW000_G2_1000<- data.frame(meanArea=round(mean(DataAreaW000_G2_1000$Area.mm2),digits =2 ))
meanDataAreaW000_G2_1000$SD<- round(sd(DataAreaW000_G2_1000$Area.mm2),digits =2 )
meanDataAreaW000_G2_1000$Weight <- "1000g"
meanDataAreaW000_G2_2000<- data.frame(meanArea=round(mean(DataAreaW000_G2_2000$Area.mm2),digits =2 ))
meanDataAreaW000_G2_2000$SD<- round(sd(DataAreaW000_G2_2000$Area.mm2),digits =2 )
meanDataAreaW000_G2_2000$Weight <- "2000g"

meanDataAreaW001_G2_100<- data.frame(meanArea=round(mean(DataAreaW001_G2_100$Area.mm2),digits =2 ))
meanDataAreaW001_G2_100$SD<- round(sd(DataAreaW001_G2_100$Area.mm2),digits =2 )
meanDataAreaW001_G2_100$Weight <- "100g"
meanDataAreaW001_G2_200<- data.frame(meanArea=round(mean(DataAreaW001_G2_200$Area.mm2),digits =2 ))
meanDataAreaW001_G2_200$SD<- round(sd(DataAreaW001_G2_200$Area.mm2),digits =2 )
meanDataAreaW001_G2_200$Weight <- "200g"
meanDataAreaW001_G2_400<- data.frame(meanArea=round(mean(DataAreaW001_G2_400$Area.mm2),digits =2 ))
meanDataAreaW001_G2_400$SD<- round(sd(DataAreaW001_G2_400$Area.mm2),digits =2 )
meanDataAreaW001_G2_400$Weight <- "400g"
meanDataAreaW001_G2_800<- data.frame(meanArea=round(mean(DataAreaW001_G2_800$Area.mm2),digits =2 ))
meanDataAreaW001_G2_800$SD<- round(sd(DataAreaW001_G2_800$Area.mm2),digits =2 )
meanDataAreaW001_G2_800$Weight <- "800g"
meanDataAreaW001_G2_1000<- data.frame(meanArea=round(mean(DataAreaW001_G2_1000$Area.mm2),digits =2 ))
meanDataAreaW001_G2_1000$SD<- round(sd(DataAreaW001_G2_1000$Area.mm2),digits =2 )
meanDataAreaW001_G2_1000$Weight <- "1000g"
meanDataAreaW001_G2_2000<- data.frame(meanArea=round(mean(DataAreaW001_G2_2000$Area.mm2),digits =2 ))
meanDataAreaW001_G2_2000$SD<- round(sd(DataAreaW001_G2_2000$Area.mm2),digits =2 )
meanDataAreaW001_G2_2000$Weight <- "2000g"

meanDataAreaW003_G2_100<- data.frame(meanArea=round(mean(DataAreaW003_G2_100$Area.mm2),digits =2 ))
meanDataAreaW003_G2_100$SD<- round(sd(DataAreaW003_G2_100$Area.mm2),digits =2 )
meanDataAreaW003_G2_100$Weight <- "100g"
meanDataAreaW003_G2_200<- data.frame(meanArea=round(mean(DataAreaW003_G2_200$Area.mm2),digits =2 ))
meanDataAreaW003_G2_200$SD<- round(sd(DataAreaW003_G2_200$Area.mm2),digits =2 )
meanDataAreaW003_G2_200$Weight <- "200g"
meanDataAreaW003_G2_400<- data.frame(meanArea=round(mean(DataAreaW003_G2_400$Area.mm2),digits =2 ))
meanDataAreaW003_G2_400$SD<- round(sd(DataAreaW003_G2_400$Area.mm2),digits =2 )
meanDataAreaW003_G2_400$Weight <- "400g"
meanDataAreaW003_G2_800<- data.frame(meanArea=round(mean(DataAreaW003_G2_800$Area.mm2),digits =2 ))
meanDataAreaW003_G2_800$SD<- round(sd(DataAreaW003_G2_800$Area.mm2),digits =2 )
meanDataAreaW003_G2_800$Weight <- "800g"
meanDataAreaW003_G2_1000<- data.frame(meanArea=round(mean(DataAreaW003_G2_1000$Area.mm2),digits =2 ))
meanDataAreaW003_G2_1000$SD<- round(sd(DataAreaW003_G2_1000$Area.mm2),digits =2 )
meanDataAreaW003_G2_1000$Weight <- "1000g"
meanDataAreaW003_G2_2000<- data.frame(meanArea=round(mean(DataAreaW003_G2_2000$Area.mm2),digits =2 ))
meanDataAreaW003_G2_2000$SD<- round(sd(DataAreaW003_G2_2000$Area.mm2),digits =2 )
meanDataAreaW003_G2_2000$Weight <- "2000g"
# 
# meanDataAreaW005_G2_100<- data.frame(meanArea=round(mean(DataAreaW005_G2_100$Area.mm2),digits =2 ))
# meanDataAreaW005_G2_100$SD<- round(sd(DataAreaW005_G2_100$Area.mm2),digits =2 )
# meanDataAreaW005_G2_100$Weight <- "100g"
# meanDataAreaW005_G2_200<- data.frame(meanArea=round(mean(DataAreaW005_G2_200$Area.mm2),digits =2 ))
# meanDataAreaW005_G2_200$SD<- round(sd(DataAreaW005_G2_200$Area.mm2),digits =2 )
# meanDataAreaW005_G2_200$Weight <- "200g"
# meanDataAreaW005_G2_400<- data.frame(meanArea=round(mean(DataAreaW005_G2_400$Area.mm2),digits =2 ))
# meanDataAreaW005_G2_400$SD<- round(sd(DataAreaW005_G2_400$Area.mm2),digits =2 )
# meanDataAreaW005_G2_400$Weight <- "400g"
# meanDataAreaW005_G2_800<- data.frame(meanArea=round(mean(DataAreaW005_G2_800$Area.mm2),digits =2 ))
# meanDataAreaW005_G2_800$SD<- round(sd(DataAreaW005_G2_800$Area.mm2),digits =2 )
# meanDataAreaW005_G2_800$Weight <- "800g"
# meanDataAreaW005_G2_1000<- data.frame(meanArea=round(mean(DataAreaW005_G2_1000$Area.mm2),digits =2 ))
# meanDataAreaW005_G2_1000$SD<- round(sd(DataAreaW005_G2_1000$Area.mm2),digits =2 )
# meanDataAreaW005_G2_1000$Weight <- "1000g"
# meanDataAreaW005_G2_2000<- data.frame(meanArea=round(mean(DataAreaW005_G2_2000$Area.mm2),digits =2 ))
# meanDataAreaW005_G2_2000$SD<- round(sd(DataAreaW005_G2_2000$Area.mm2),digits =2 )
# meanDataAreaW005_G2_2000$Weight <- "2000g"
# 
# meanDataAreaW007_G2_100<- data.frame(meanArea=round(mean(DataAreaW007_G2_100$Area.mm2),digits =2 ))
# meanDataAreaW007_G2_100$SD<- round(sd(DataAreaW007_G2_100$Area.mm2),digits =2 )
# meanDataAreaW007_G2_100$Weight <- "100g"
# meanDataAreaW007_G2_200<- data.frame(meanArea=round(mean(DataAreaW007_G2_200$Area.mm2),digits =2 ))
# meanDataAreaW007_G2_200$SD<- round(sd(DataAreaW007_G2_200$Area.mm2),digits =2 )
# meanDataAreaW007_G2_200$Weight <- "200g"
# meanDataAreaW007_G2_400<- data.frame(meanArea=round(mean(DataAreaW007_G2_400$Area.mm2),digits =2 ))
# meanDataAreaW007_G2_400$SD<- round(sd(DataAreaW007_G2_400$Area.mm2),digits =2 )
# meanDataAreaW007_G2_400$Weight <- "400g"
# meanDataAreaW007_G2_800<- data.frame(meanArea=round(mean(DataAreaW007_G2_800$Area.mm2),digits =2 ))
# meanDataAreaW007_G2_800$SD<- round(sd(DataAreaW007_G2_800$Area.mm2),digits =2 )
# meanDataAreaW007_G2_800$Weight <- "800g"
# meanDataAreaW007_G2_1000<- data.frame(meanArea=round(mean(DataAreaW007_G2_1000$Area.mm2),digits =2 ))
# meanDataAreaW007_G2_1000$SD<- round(sd(DataAreaW007_G2_1000$Area.mm2),digits =2 )
# meanDataAreaW007_G2_1000$Weight <- "1000g"
# meanDataAreaW007_G2_2000<- data.frame(meanArea=round(mean(DataAreaW007_G2_2000$Area.mm2),digits =2 ))
# meanDataAreaW007_G2_2000$SD<- round(sd(DataAreaW007_G2_2000$Area.mm2),digits =2 )
# meanDataAreaW007_G2_2000$Weight <- "2000g"

# Combined data sets
DataW000total <- rbind(meanDataAreaW000_G2_100,meanDataAreaW000_G2_200,meanDataAreaW000_G2_400,
                       meanDataAreaW000_G2_800,meanDataAreaW000_G2_1000,meanDataAreaW000_G2_2000)
DataW000total$Condition <- "W000"

DataW001total <- rbind(meanDataAreaW001_G2_100,meanDataAreaW001_G2_200,meanDataAreaW001_G2_400,
                       meanDataAreaW001_G2_800,meanDataAreaW001_G2_1000,meanDataAreaW001_G2_2000)
DataW001total$Condition <- "W001"

DataW003total <- rbind(meanDataAreaW003_G2_100,meanDataAreaW003_G2_200,meanDataAreaW003_G2_400,
                       meanDataAreaW003_G2_800,meanDataAreaW003_G2_1000,meanDataAreaW003_G2_2000)
DataW003total$Condition <- "W003"
# 
# DataW005total <- rbind(meanDataAreaW005_G2_100,meanDataAreaW005_G2_200,meanDataAreaW005_G2_400,
#                        meanDataAreaW005_G2_800,meanDataAreaW005_G2_1000,meanDataAreaW005_G2_2000)
# DataW005total$Condition <- "W005"
# 
# DataW007total <- rbind(meanDataAreaW007_G2_100,meanDataAreaW007_G2_200,meanDataAreaW007_G2_400,
#                        meanDataAreaW007_G2_800,meanDataAreaW007_G2_1000,meanDataAreaW007_G2_2000)
# DataW007total$Condition <- "W007"

FibreCount_Shedding_G1 <- rbind(DataW000total,DataW001total,DataW003total)#,DataW005total,DataW007total)

write.table(FibreCount_Shedding_G1, file = "Shedding_Fibre_Count.csv", quote = F, sep = ",", row.names = F)

#### PLOT ####
# calculation of the percentage difference between washed and unwashed
y = rep(c(125, 150, 175, 200, 225),2)
x = rep(c(1:5), 2)

# plot
pSH <- ggplot(FibreCount_Shedding_G1, aes(x = factor(Weight, level = c('100g', '200g', '400g','800g','1000g','2000g')),
                                          y= meanArea, fill=Condition))+
  geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label = meanArea), hjust=0.5,vjust = -5.5,position = position_dodge(.9))+
  labs(x="\nWeight", y="Total fibre area (mm\u00b2)\n") +
  theme_bw(base_family = "Arial", base_size = 12) +
  ylim(0,500)+
  scale_fill_manual("legend", values = c("W000" = "#A6CEE3", "W001" = "#1F78B4"))+ # to obtain the colour brewer.pal(12, "Paired")
  theme(legend.title = element_blank(),
        strip.background.x = element_rect(colour = "grey", fill = "white"),
        legend.position = "bottom",
        legend.background = element_rect(fill="grey95",size=1, linetype="solid", colour="grey80"),
        axis.text.x = element_text(angle = 0, vjust = 0.95, hjust=0.5))+
  geom_errorbar(aes(ymin=meanArea-SD, ymax=meanArea+SD),width=.2,position=position_dodge(.9))
pSH
ggsave("Shedding_G2_W000-3.png", pSH, width = 10, height = 9, units = "in", dpi=150, path = "Results")
