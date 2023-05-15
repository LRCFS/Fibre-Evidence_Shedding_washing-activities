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
# W003_Dataset$Coder <- "W003"
# W004_Dataset$Coder <- "W004"
# W005_Dataset$Coder <- "W005"
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
# W003negative <- W003_Dataset %>% filter(grepl('negative', Sample))
# W004negative <- W004_Dataset %>% filter(grepl('negative', Sample))
# W005negative <- W005_Dataset %>% filter(grepl('negative', Sample))
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
Negativecontrol <- rbind(W000negative,W001negative, W002negative)

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
# W003positive <- W003_Dataset %>% filter(grepl('positive', Sample))
# W004positive <- W004_Dataset %>% filter(grepl('positive', Sample))
# W005positive <- W005_Dataset %>% filter(grepl('positive', Sample))
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

Positivecontrol <- rbind(W000positive,W001positive, W002positive)
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
forFibreCount0<- W000_Dataset[!(W000_Dataset$Sample=="W000_positive" | W000_Dataset$Sample=="W000_negative"),]
forFibreCount1<- W001_Dataset[!(W001_Dataset$Sample=="W001_positive" | W001_Dataset$Sample=="W001_negative"),]
forFibreCount2<- W002_Dataset[!(W002_Dataset$Sample=="W002_positive" | W002_Dataset$Sample=="W002_negative"),]
# forFibreCount3<- W003_Dataset[!(W003_Dataset$Sample=="W003_positive" | W003_Dataset$Sample=="W003_negative"),]
# forFibreCount4<- W004_Dataset[!(W004_Dataset$Sample=="W004_positive" | W004_Dataset$Sample=="W004_negative"),]
# forFibreCount5<- W005_Dataset[!(W005_Dataset$Sample=="W005_positive" | W005_Dataset$Sample=="W005_negative"),]
# forFibreCount6<- W006_Dataset[!(W006_Dataset$Sample=="W006_positive" | W006_Dataset$Sample=="W006_negative"),]
# forFibreCount7<- W007_Dataset[!(W007_Dataset$Sample=="W007_positive" | W007_Dataset$Sample=="W007_negative"),]
# forFibreCount8<- W008_Dataset[!(W008_Dataset$Sample=="W008_positive" | W008_Dataset$Sample=="W008_negative"),]
# forFibreCount9<- W009_Dataset[!(W009_Dataset$Sample=="W009_positive" | W009_Dataset$Sample=="W009_negative"),]
# forFibreCount10<- W010_Dataset[!(W010_Dataset$Sample=="W010_positive" | W010_Dataset$Sample=="W010_negative"),]
# forFibreCount11<- W011_Dataset[!(W011_Dataset$Sample=="W011_positive" | W011_Dataset$Sample=="W011_negative"),]
# forFibreCount12<- W012_Dataset[!(W012_Dataset$Sample=="W012_positive" | W012_Dataset$Sample=="W012_negative"),]
# forFibreCount13<- W013_Dataset[!(W013_Dataset$Sample=="W013_positive" | W013_Dataset$Sample=="W013_negative"),]
# forFibreCount14<- W014_Dataset[!(W014_Dataset$Sample=="W014_positive" | W014_Dataset$Sample=="W014_negative"),]
# forFibreCount15<- W015_Dataset[!(W015_Dataset$Sample=="W015_positive" | W015_Dataset$Sample=="W015_negative"),]

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
BackgroundFibreCount <- rbind(BackgroundW000, BackgroundW001, BackgroundW002)
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
TransferFibreCount <- rbind(TransferW000, TransferW001, TransferW002)
names(TransferFibreCount) <- c("group", "value")
# write.table(TransferFibreCount, file = "Fibre Count - Transfer.csv", quote = F, sep = ",", row.names = F)

#### Bar plots of data by group - distribution ####
histogram(~ value | group,data=TransferFibreCount,layout=c(3,1),
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
ggsave("Fibre Count boxplot_ATr.png", pAtr, width = 6, height = 7, units = "in", dpi=150, path = "Results")

#########################################################
#####                  SHEDDING TEST                #####
#########################################################
Shedding <- read.csv('./Fibre count Summary/Shedding_Summary.csv', sep="," ,header = T,fileEncoding="UTF-8-BOM")
Shedding$Slice<- gsub(".TIF","",Shedding$Slice)
SheddingExtended <- data.frame(str_split(Shedding$Slice, "_", simplify=TRUE))
names(SheddingExtended) <- c("Garment","Weight","Repeat","condition")
SheddingTotal <- cbind(SheddingExtended,Area=Shedding$Total.Area)

# Convert Area from inch2 to mm2
# 1 pixel = 1 x10^-5 inch2, so (Area*1)/0.000011 to convert into px
SheddingTotal$Area.px <- (SheddingTotal$Area*1)/0.000011
# 1 mm = 112 pixels, 1mm2 = 12544 px
SheddingTotal$Area.mm2 <- SheddingTotal$Area.px/12544

# Split data per garments
SheddingRed <- SheddingTotal[SheddingTotal$Garment =='Red',]
SheddingYellow <- SheddingTotal[SheddingTotal$Garment =='Yellow',]

# Split data per washing condition
DataAreaRedunwashed <- SheddingRed[SheddingRed$condition =='unwashed',]
DataAreaRedwashed <- SheddingRed[SheddingRed$condition =='washed',]
DataAreaYellowunwashed <- SheddingYellow[SheddingYellow$condition =='unwashed',]
DataAreaYellowwashed <- SheddingYellow[SheddingYellow$condition =='washed',]

# for Appendix
write.table(DataAreaRedunwashed, file = "Shedding red garment unwashed.csv", quote = F, sep = ",", row.names = F)
write.table(DataAreaRedwashed, file = "Shedding red garment washed.csv", quote = F, sep = ",", row.names = F)
write.table(DataAreaYellowunwashed, file = "Shedding yellow garment unwashed.csv", quote = F, sep = ",", row.names = F)
write.table(DataAreaYellowwashed, file = "Shedding yellow garment washed.csv", quote = F, sep = ",", row.names = F)

# split per weight - unwashed
DataAreaRedunwashed100 <- DataAreaRedunwashed[DataAreaRedunwashed$Weight =='100g',]
DataAreaRedunwashed200 <- DataAreaRedunwashed[DataAreaRedunwashed$Weight =='200g',]
DataAreaRedunwashed400 <- DataAreaRedunwashed[DataAreaRedunwashed$Weight =='400g',]
DataAreaRedunwashed800 <- DataAreaRedunwashed[DataAreaRedunwashed$Weight =='800g',]
DataAreaRedunwashed1000 <- DataAreaRedunwashed[DataAreaRedunwashed$Weight =='1000g',]

DataAreaYellowunwashed100 <- DataAreaYellowunwashed[DataAreaYellowunwashed$Weight =='100g',]
DataAreaYellowunwashed200 <- DataAreaYellowunwashed[DataAreaYellowunwashed$Weight =='200g',]
DataAreaYellowunwashed400 <- DataAreaYellowunwashed[DataAreaYellowunwashed$Weight =='400g',]
DataAreaYellowunwashed800 <- DataAreaYellowunwashed[DataAreaYellowunwashed$Weight =='800g',]
DataAreaYellowunwashed1000 <- DataAreaYellowunwashed[DataAreaYellowunwashed$Weight =='1000g',]

# split per weight - washed
DataAreaRedwashed100 <- DataAreaRedwashed[DataAreaRedwashed$Weight =='100g',]
DataAreaRedwashed200 <- DataAreaRedwashed[DataAreaRedwashed$Weight =='200g',]
DataAreaRedwashed400 <- DataAreaRedwashed[DataAreaRedwashed$Weight =='400g',]
DataAreaRedwashed800 <- DataAreaRedwashed[DataAreaRedwashed$Weight =='800g',]
DataAreaRedwashed1000 <- DataAreaRedwashed[DataAreaRedwashed$Weight =='1000g',]

DataAreaYellowwashed100 <- DataAreaYellowwashed[DataAreaYellowwashed$Weight =='100g',]
DataAreaYellowwashed200 <- DataAreaYellowwashed[DataAreaYellowwashed$Weight =='200g',]
DataAreaYellowwashed400 <- DataAreaYellowwashed[DataAreaYellowwashed$Weight =='400g',]
DataAreaYellowwashed800 <- DataAreaYellowwashed[DataAreaYellowwashed$Weight =='800g',]
DataAreaYellowwashed1000 <- DataAreaYellowwashed[DataAreaYellowwashed$Weight =='1000g',]

# Calculation of mean and SD
meanDataAreaRedunwashed100<- data.frame(meanArea=round(mean(DataAreaRedunwashed100$Area.mm2),digits =2 ))
meanDataAreaRedunwashed100$SD<- round(sd(DataAreaRedunwashed100$Area.mm2),digits =2 )
meanDataAreaRedunwashed100$Weight <- "100g"
meanDataAreaRedunwashed200<- data.frame(meanArea=round(mean(DataAreaRedunwashed200$Area.mm2),digits =2 ))
meanDataAreaRedunwashed200$SD<- round(sd(DataAreaRedunwashed200$Area.mm2),digits =2 )
meanDataAreaRedunwashed200$Weight <- "200g"
meanDataAreaRedunwashed400<- data.frame(meanArea=round(mean(DataAreaRedunwashed400$Area.mm2),digits =2 ))
meanDataAreaRedunwashed400$SD<- round(sd(DataAreaRedunwashed400$Area.mm2),digits =2 )
meanDataAreaRedunwashed400$Weight <- "400g"
meanDataAreaRedunwashed800<- data.frame(meanArea=round(mean(DataAreaRedunwashed800$Area.mm2),digits =2 ))
meanDataAreaRedunwashed800$SD<- round(sd(DataAreaRedunwashed800$Area.mm2),digits =2 )
meanDataAreaRedunwashed800$Weight <- "800g"
meanDataAreaRedunwashed1000<- data.frame(meanArea=round(mean(DataAreaRedunwashed1000$Area.mm2),digits =2 ))
meanDataAreaRedunwashed1000$SD<- round(sd(DataAreaRedunwashed1000$Area.mm2),digits =2 )
meanDataAreaRedunwashed1000$Weight <- "1000g"

meanDataAreaYellowunwashed100<- data.frame(meanArea=round(mean(DataAreaYellowunwashed100$Area.mm2),digits =2 ))
meanDataAreaYellowunwashed100$SD<- round(sd(DataAreaYellowunwashed100$Area.mm2),digits =2 )
meanDataAreaYellowunwashed100$Weight <- "100g"
meanDataAreaYellowunwashed200<- data.frame(meanArea=round(mean(DataAreaYellowunwashed200$Area.mm2),digits =2 ))
meanDataAreaYellowunwashed200$SD<- round(sd(DataAreaYellowunwashed200$Area.mm2),digits =2 )
meanDataAreaYellowunwashed200$Weight <- "200g"
meanDataAreaYellowunwashed400<- data.frame(meanArea=round(mean(DataAreaYellowunwashed400$Area.mm2),digits =2 ))
meanDataAreaYellowunwashed400$SD<- round(sd(DataAreaYellowunwashed400$Area.mm2),digits =2 )
meanDataAreaYellowunwashed400$Weight <- "400g"
meanDataAreaYellowunwashed800<- data.frame(meanArea=round(mean(DataAreaYellowunwashed800$Area.mm2),digits =2 ))
meanDataAreaYellowunwashed800$SD<- round(sd(DataAreaYellowunwashed800$Area.mm2),digits =2 )
meanDataAreaYellowunwashed800$Weight <- "800g"
meanDataAreaYellowunwashed1000<- data.frame(meanArea=round(mean(DataAreaYellowunwashed1000$Area.mm2),digits =2 ))
meanDataAreaYellowunwashed1000$SD<- round(sd(DataAreaYellowunwashed1000$Area.mm2),digits =2 )
meanDataAreaYellowunwashed1000$Weight <- "1000g"

meanDataAreaRedwashed100<- data.frame(meanArea=round(mean(DataAreaRedwashed100$Area.mm2),digits =2 ))
meanDataAreaRedwashed100$SD<- round(sd(DataAreaRedwashed100$Area.mm2),digits =2 )
meanDataAreaRedwashed100$Weight <- "100g"
meanDataAreaRedwashed200<- data.frame(meanArea=round(mean(DataAreaRedwashed200$Area.mm2),digits =2 ))
meanDataAreaRedwashed200$SD<- round(sd(DataAreaRedwashed200$Area.mm2),digits =2 )
meanDataAreaRedwashed200$Weight <- "200g"
meanDataAreaRedwashed400<- data.frame(meanArea=round(mean(DataAreaRedwashed400$Area.mm2),digits =2 ))
meanDataAreaRedwashed400$SD<- round(sd(DataAreaRedwashed400$Area.mm2),digits =2 )
meanDataAreaRedwashed400$Weight <- "400g"
meanDataAreaRedwashed800<- data.frame(meanArea=round(mean(DataAreaRedwashed800$Area.mm2),digits =2 ))
meanDataAreaRedwashed800$SD<- round(sd(DataAreaRedwashed800$Area.mm2),digits =2 )
meanDataAreaRedwashed800$Weight <- "800g"
meanDataAreaRedwashed1000<- data.frame(meanArea=round(mean(DataAreaRedwashed1000$Area.mm2),digits =2 ))
meanDataAreaRedwashed1000$SD<- round(sd(DataAreaRedwashed1000$Area.mm2),digits =2 )
meanDataAreaRedwashed1000$Weight <- "1000g"

meanDataAreaYellowwashed100<- data.frame(meanArea=round(mean(DataAreaYellowwashed100$Area.mm2),digits =2 ))
meanDataAreaYellowwashed100$SD<- round(sd(DataAreaYellowwashed100$Area.mm2),digits =2 )
meanDataAreaYellowwashed100$Weight <- "100g"
meanDataAreaYellowwashed200<- data.frame(meanArea=round(mean(DataAreaYellowwashed200$Area.mm2),digits =2 ))
meanDataAreaYellowwashed200$SD<- round(sd(DataAreaYellowwashed200$Area.mm2),digits =2 )
meanDataAreaYellowwashed200$Weight <- "200g"
meanDataAreaYellowwashed400<- data.frame(meanArea=round(mean(DataAreaYellowwashed400$Area.mm2),digits =2 ))
meanDataAreaYellowwashed400$SD<- round(sd(DataAreaYellowwashed400$Area.mm2),digits =2 )
meanDataAreaYellowwashed400$Weight <- "400g"
meanDataAreaYellowwashed800<- data.frame(meanArea=round(mean(DataAreaYellowwashed800$Area.mm2),digits =2 ))
meanDataAreaYellowwashed800$SD<- round(sd(DataAreaYellowwashed800$Area.mm2),digits =2 )
meanDataAreaYellowwashed800$Weight <- "800g"
meanDataAreaYellowwashed1000<- data.frame(meanArea=round(mean(DataAreaYellowwashed1000$Area.mm2),digits =2 ))
meanDataAreaYellowwashed1000$SD<- round(sd(DataAreaYellowwashed1000$Area.mm2),digits =2 )
meanDataAreaYellowwashed1000$Weight <- "1000g"

# Combined data sets
DataRedUnwashedtotal <- rbind(meanDataAreaRedunwashed100,meanDataAreaRedunwashed200,meanDataAreaRedunwashed400,
                              meanDataAreaRedunwashed800,meanDataAreaRedunwashed1000)
DataRedUnwashedtotal$Condition <- "unwashed"
DataRedwashedtotal <- rbind(meanDataAreaRedwashed100,meanDataAreaRedwashed200,meanDataAreaRedwashed400,
                            meanDataAreaRedwashed800,meanDataAreaRedwashed1000)
DataRedwashedtotal$Condition <- "washed"
DataAreaRedCombined <- rbind(DataRedwashedtotal,DataRedUnwashedtotal)

DataYellowUnwashedtotal <- rbind(meanDataAreaYellowunwashed100,meanDataAreaYellowunwashed200,meanDataAreaYellowunwashed400,
                                 meanDataAreaYellowunwashed800,meanDataAreaYellowunwashed1000)
DataYellowUnwashedtotal$Condition <- "unwashed"
DataYellowwashedtotal <- rbind(meanDataAreaYellowwashed100,meanDataAreaYellowwashed200,meanDataAreaYellowwashed400,
                               meanDataAreaYellowwashed800,meanDataAreaYellowwashed1000)
DataYellowwashedtotal$Condition <- "washed"
DataAreaYellowCombined <- rbind(DataYellowwashedtotal,DataYellowUnwashedtotal)

#### PLOT ####
# calculation of the percentage difference between washed and unwashed
y = rep(c(150, 175, 175, 200, 225),2)
x = rep(c(1:5), 2)
Red <- data.frame(cbind(DataRedUnwashedtotal$meanArea,DataRedwashedtotal$meanArea))
Red$ratio <- round((Red$X1/Red$X2), digits=2)
label = rep(Red$ratio,2)
# plot
PlotRedShedding <- ggplot(DataAreaRedCombined, aes(x = factor(Weight, level = c('100g', '200g', '400g','800g','1000g')),
                                                   y= meanArea, fill=Condition)) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label = meanArea), hjust=0.5,vjust = -3.5,position = position_dodge(.9))+
  geom_line(aes(y=0,colour='Ratio',group=Weight))+
  labs(x="\nWeight", y="Total fibre area (mm\u00b2)\n") +
  theme_bw(base_family = "Arial", base_size = 14) +
  ylim(0,500)+
  scale_fill_grey(start = 0.4,end = 0.9)+
  theme(legend.title = element_blank(),
        strip.background.x = element_rect(colour = "grey", fill = "white"),
        legend.position = "bottom",
        legend.background = element_rect(fill="grey95",size=1, linetype="solid", colour="grey80"),
        axis.text.x = element_text(angle = 0, vjust = 0.95, hjust=0.5))+
  geom_errorbar(aes(ymin=meanArea-SD, ymax=meanArea+SD),width=.2,position=position_dodge(.9))+
  geom_text(x=x, y=y+2, label=label, colour="#c9101a")+
  geom_errorbarh(aes(xmax = (x + 0.3), xmin = (x - 0.3), y = y-20), height = 15,colour="#c9101a") 
PlotRedShedding
ggsave("Shedding Red.png", PlotRedShedding, width = 7, height =6, units = "in", dpi=600,
       path = "C:/Users/2395804/OneDrive - University of Dundee/Desktop/PhD Results/Transfer project/Labwork analysis/Results")

# calculation of the percentage difference between washed and unwashed
a = rep(c(275, 300, 325, 400, 425),2)
b = rep(c(1:5), 2)
Yellow <- data.frame(cbind(DataYellowUnwashedtotal$meanArea,DataYellowwashedtotal$meanArea))
Yellow$ratio <- round((Yellow$X1/Yellow$X2), digits=2)
label = rep(Yellow$ratio,2)
# Plot
PlotYellowShedding <- ggplot(DataAreaYellowCombined, aes(x = factor(Weight, level = c('100g', '200g', '400g','800g','1000g')),
                                                         y= meanArea, fill=Condition)) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label = meanArea), hjust=0.5,vjust = -5,position = position_dodge(.9))+
  geom_line(aes(y=0,colour='Ratio',group=Weight))+
  labs(x="\nWeight", y="Total fibre area (mm\u00b2)\n") +
  theme_bw(base_family = "Arial", base_size = 12) +
  ylim(0,500)+
  scale_fill_grey(start = 0.4,end = 0.9)+
  theme(legend.position = "bottom",
        legend.background = element_rect(fill="grey95",size=1, linetype="solid", colour="grey80"),
        axis.text.x = element_text(angle = 0, vjust = 0.95, hjust=0.5))+
  geom_errorbar(aes(ymin=meanArea-SD, ymax=meanArea+SD),width=.3,position=position_dodge(.9))+
  geom_text(x=b, y=a+2, label=label,colour="#c9101a")+
  geom_errorbarh(aes(xmax = (b + 0.3), xmin = (b - 0.3), y = a-20), height = 15,colour="#c9101a") 
PlotYellowShedding 
ggsave("Shedding Yellow.png", PlotYellowShedding, width = 7, height = 7, units = "in", dpi=600,
       path = "C:/Users/2395804/OneDrive - University of Dundee/Desktop/PhD Results/Transfer project/Labwork analysis/Results")

pSheddingCombined_pending <- ggarrange(PlotRedShedding+ rremove("ylab") + rremove("xlab"),
                                       PlotYellowShedding+ rremove("ylab") + rremove("xlab"),
                                       labels = c("   100% virgin cotton donor - Red","65/35% recycled/virgin cotton donor - Yellow"),
                                       common.legend = TRUE, legend = "right",
                                       align = "hv",
                                       ncol = 1, nrow = 2,
                                       font.label = list(size = 10, color = "black", family = "Arial", position = "top"),
                                       hjust=-0.2,vjust=2.5)
pSheddingCombined_pending
pSheddingCombined <- annotate_figure(pSheddingCombined_pending, left = textGrob("Total fibre area (mm\u00b2)\n", rot = 90, vjust = 0.5, hjust = 0.5, gp = gpar(cex =1)),
                                     bottom = textGrob("weight", vjust = 0.5, hjust = 0.5,gp = gpar(cex = 1)))
pSheddingCombined
ggsave("Shedding Red VS Yellow.png", pSheddingCombined, width = 7, height = 9, units = "in", dpi=600,
       path = "C:/Users/2395804/OneDrive - University of Dundee/Desktop/PhD Results/Transfer project/Labwork analysis/Results")

#### STATS ####
# Statistical analysis of the effect of the weight on the total area
# qqplot to see the distribution 
ggplot(DataAreaRedunwashed) +
  aes(x = Weight, y = Area.mm2, color = Weight) +
  geom_jitter() +
  theme(legend.position = "none")

ggplot(DataAreaRedwashed) +
  aes(x = Weight, y = Area.mm2, color = Weight) +
  geom_jitter() +
  theme(legend.position = "none")

ggplot(DataAreaYellowunwashed) +
  aes(x = Weight, y = Area.mm2, color = Weight) +
  geom_jitter() +
  theme(legend.position = "none")

ggplot(DataAreaYellowwashed) +
  aes(x = Weight, y = Area.mm2, color = Weight) +
  geom_jitter() +
  theme(legend.position = "none")

# Test of normality
# Red, unwashed
res_aovRedunwashed <- aov(Area.mm2 ~ Weight, data = DataAreaRedunwashed)
par(mfrow = c(1, 2)) # combine plots
# histogram
hist(res_aovRedunwashed$residuals)
# QQ-plot
qqPlot(res_aovRedunwashed$residuals, id = FALSE) # id = FALSE to remove point identification
shapiro.test(res_aovRedunwashed$residuals)
# p-value = 0.03681, data do not follow a normal distribution.

# Red, washed
res_aovRedwashed <- aov(Area.mm2 ~ Weight, data = DataAreaRedwashed)
par(mfrow = c(1, 2)) # combine plots
# histogram
hist(res_aovRedwashed$residuals)
# QQ-plot
qqPlot(res_aovRedwashed$residuals, id = FALSE) # id = FALSE to remove point identification
shapiro.test(res_aovRedwashed$residuals)
# p-value = 0.05321 (> 0.05), data follow a normal distribution.

# Yellow, unwashed
res_aovYellowunwashed <- aov(Area.mm2 ~ Weight, data = DataAreaYellowunwashed)
par(mfrow = c(1, 2)) # combine plots
# histogram
hist(res_aovYellowunwashed$residuals)
# QQ-plot
qqPlot(res_aovYellowunwashed$residuals, id = FALSE) # id = FALSE to remove point identification
shapiro.test(res_aovYellowunwashed$residuals)
# p-value = 0.1894 (> 0.05), data follow a normal distribution.

# Yellow, washed
res_aovYellowwashed <- aov(Area.mm2 ~ Weight, data = DataAreaYellowwashed)
par(mfrow = c(1, 2)) # combine plots
# histogram
hist(res_aovYellowwashed$residuals)
# QQ-plot
qqPlot(res_aovYellowwashed$residuals, id = FALSE) # id = FALSE to remove point identification
shapiro.test(res_aovYellowwashed$residuals)
# p-value = 0.1733 (> 0.05), data follow a normal distribution.

# Equality of variances - homogeneity
bartlett.test(Area.mm2 ~ Weight, data = DataAreaRedunwashed) # p-value = 0.5594, the three groups have similar variances
bartlett.test(Area.mm2 ~ Weight, data = DataAreaRedwashed) # p-value = 0.04187, the three groups have different variances
bartlett.test(Area.mm2 ~ Weight, data = DataAreaYellowunwashed) # p-value = 0.6034, the three groups have similar variances
bartlett.test(Area.mm2 ~ Weight, data = DataAreaYellowwashed) # p-value = 0.5449, the three groups have similar variances

# Choice of test
# for DataAreaRedunwashed : data do not follow a normal distribution and have similar variance: Kruskall- Wallis
# for DataAreaRedwashed : data follow a normal distribution and have different variance: Welch ANOVA
# for DataAreaYellowunwashed : data follow a normal distribution and have similar variance: ANOVA
# for DataAreaYellowwashed : data follow a normal distribution and have similar variance: ANOVA

# Kruskal-Wallis test for DataAreaRedunwashed
#H0: The 3 groups are equal
#H1: At least one group is different from the other 2 groups
kruskal.test(Area.mm2 ~ Weight,data = DataAreaRedunwashed)
# p-value = 0.1606, the groups are equal (p-value = 0.8061).
# Post-hoc test following Kruskal-Wallis
dunnTest(Area.mm2 ~ Weight,data = DataAreaRedunwashed,method = "holm")

### Welch ANOVA for DataAreaRedwashed
# used when variances are unequal. This can be done by replacing var.equal = TRUE by var.equal = FALSE
oneway.test(DataAreaRedwashed$Area.mm2 ~ DataAreaRedwashed$Weight, var.equal = FALSE)
# p-value = 0.08245,  p-value > 0.05,  all means are equal
posthoc.tgh(y=DataAreaRedwashed$Area.mm2, x=DataAreaRedwashed$Weight);

### ANOVA for DataAreaYellowunwashed
DataAreaYellowunwashed$Weight = as.factor(DataAreaYellowunwashed$Weight)
res_aov <- aov(Area.mm2 ~ Weight,data = DataAreaYellowunwashed)
summary(res_aov)
# p-value = 0.00242 **,  we reject the hypothesis that all means are equal
# Therefore, we can conclude that at least one weight is different than the others
# Tukey HSD test:
TukeyHSD(res_aov)

### ANOVA for DataAreaYellowwashed
DataAreaYellowwashed$Weight = as.factor(DataAreaYellowwashed$Weight)
res_aov <- aov(Area.mm2 ~ Weight,data = DataAreaYellowwashed)
summary(res_aov)
# p-value = 0.409, no differences between groups
# Tukey HSD test:
TukeyHSD(res_aov)

# STATS - comparing effect of the wash in the shedding #
# qqplot to see the distribution
# Red
ggplot(SheddingRed) +
  aes(x = condition, y = Area.mm2, color = condition) +
  geom_jitter() +
  theme(legend.position = "none")

# Yellow
ggplot(SheddingYellow) +
  aes(x = condition, y = Area.mm2, color = condition) +
  geom_jitter() +
  theme(legend.position = "none")

# Test of normality
res_aovSheddingRed <- aov(Area.mm2 ~ condition, data = SheddingRed)
par(mfrow = c(1, 2)) # combine plots
# histogram
hist(res_aovSheddingRed$residuals)
# QQ-plot
qqPlot(res_aovSheddingRed$residuals, id = FALSE) # id = FALSE to remove point identification
shapiro.test(res_aovSheddingRed$residuals)
# p-value = 0.0004514, data do not follow a normal distribution.

res_aovSheddingYellow <- aov(Area.mm2 ~ condition, data = SheddingYellow)
par(mfrow = c(1, 2)) # combine plots
# histogram
hist(res_aovSheddingYellow$residuals)
# QQ-plot
qqPlot(res_aovSheddingYellow$residuals, id = FALSE) # id = FALSE to remove point identification
shapiro.test(res_aovSheddingYellow$residuals)
# p-value = 0.06834, data follow a normal distribution.

# Equality of variances - homogeneity
bartlett.test(Area.mm2 ~ condition, data = SheddingRed) # p-value =  0.4728, the three groups have similar variances
bartlett.test(Area.mm2 ~ condition, data = SheddingYellow) # p-value = 1.719e-08, the three groups have similar variances

# Choice of test
# for SheddingRed : data do not follow a normal distribution and have similar variance: Kruskall- Wallis
# for SheddingYellow : data do not follow a normal distribution and have similar variance: Kruskall- Wallis
