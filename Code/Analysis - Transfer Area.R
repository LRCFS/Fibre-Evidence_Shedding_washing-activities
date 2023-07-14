#########################################################
#####         TRANSFER - TOTAL FIBRE AREA           #####
#########################################################
##################
#####   G1   #####
##################
TransferArea_G1 <- read.csv('./Fibre count Summary/TR_G1_W000-W015_Summary.csv', sep="," ,header = T,fileEncoding="UTF-8-BOM")
TransferArea_G1$Slice<- gsub(".TIF","",TransferArea_G1$Slice)
TransferArea_G1Extended <- data.frame(str_split(TransferArea_G1$Slice, "_", simplify=TRUE))
names(TransferArea_G1Extended) <- c("Project","Wash","Garment","Bound","Condition")
TransferArea_G1Total <- cbind(TransferArea_G1Extended,Area=TransferArea_G1$Total.Area)

# Convert Area from inch2 to mm2
# 1 pixel = 1 x10^-5 inch2, so (Area*1)/0.000011 to convert into px
TransferArea_G1Total$Area.px <- (TransferArea_G1Total$Area*1)/0.000011
# 1 mm = 108 pixels, 1mm2 = 11664 px
TransferArea_G1Total$Area.mm2 <- TransferArea_G1Total$Area.px/11664

# Split data per washing condition
DataAreaW000_G1_TR <- TransferArea_G1Total[TransferArea_G1Total$Wash =='W000',]
DataAreaW001_G1_TR <- TransferArea_G1Total[TransferArea_G1Total$Wash =='W001',]
DataAreaW003_G1_TR <- TransferArea_G1Total[TransferArea_G1Total$Wash =='W003',]
DataAreaW005_G1_TR <- TransferArea_G1Total[TransferArea_G1Total$Wash =='W005',]
DataAreaW007_G1_TR <- TransferArea_G1Total[TransferArea_G1Total$Wash =='W007',]
DataAreaW009_G1_TR <- TransferArea_G1Total[TransferArea_G1Total$Wash =='W009',]
DataAreaW011_G1_TR <- TransferArea_G1Total[TransferArea_G1Total$Wash =='W011',]
DataAreaW013_G1_TR <- TransferArea_G1Total[TransferArea_G1Total$Wash =='W013',]
DataAreaW015_G1_TR <- TransferArea_G1Total[TransferArea_G1Total$Wash =='W015',]

# Calculation of mean and SD
meanDataAreaW000_G1_TR<- data.frame(meanArea=round(mean(DataAreaW000_G1_TR$Area.mm2),digits =2 ))
meanDataAreaW000_G1_TR$SD<- round(sd(DataAreaW000_G1_TR$Area.mm2),digits =2 )
meanDataAreaW000_G1_TR$Wash <- "W000"
meanDataAreaW000_G1_TR$Condition <- "W000_G1"

meanDataAreaW001_G1_TR<- data.frame(meanArea=round(mean(DataAreaW001_G1_TR$Area.mm2),digits =2 ))
meanDataAreaW001_G1_TR$SD<- round(sd(DataAreaW001_G1_TR$Area.mm2),digits =2 )
meanDataAreaW001_G1_TR$Wash <- "W001"
meanDataAreaW001_G1_TR$Condition <- "W001_G1"

meanDataAreaW003_G1_TR<- data.frame(meanArea=round(mean(DataAreaW003_G1_TR$Area.mm2),digits =2 ))
meanDataAreaW003_G1_TR$SD<- round(sd(DataAreaW003_G1_TR$Area.mm2),digits =2 )
meanDataAreaW003_G1_TR$Wash <- "W003"
meanDataAreaW003_G1_TR$Condition <- "W003_G1"

meanDataAreaW005_G1_TR<- data.frame(meanArea=round(mean(DataAreaW005_G1_TR$Area.mm2),digits =2 ))
meanDataAreaW005_G1_TR$SD<- round(sd(DataAreaW005_G1_TR$Area.mm2),digits =2 )
meanDataAreaW005_G1_TR$Wash <- "W005"
meanDataAreaW005_G1_TR$Condition <- "W005_G1"

meanDataAreaW007_G1_TR<- data.frame(meanArea=round(mean(DataAreaW007_G1_TR$Area.mm2),digits =2 ))
meanDataAreaW007_G1_TR$SD<- round(sd(DataAreaW007_G1_TR$Area.mm2),digits =2 )
meanDataAreaW007_G1_TR$Wash <- "W007"
meanDataAreaW007_G1_TR$Condition <- "W007_G1"

meanDataAreaW009_G1_TR<- data.frame(meanArea=round(mean(DataAreaW009_G1_TR$Area.mm2),digits =2 ))
meanDataAreaW009_G1_TR$SD<- round(sd(DataAreaW009_G1_TR$Area.mm2),digits =2 )
meanDataAreaW009_G1_TR$Wash <- "W009"
meanDataAreaW009_G1_TR$Condition <- "W009_G1"

meanDataAreaW011_G1_TR<- data.frame(meanArea=round(mean(DataAreaW011_G1_TR$Area.mm2),digits =2 ))
meanDataAreaW011_G1_TR$SD<- round(sd(DataAreaW011_G1_TR$Area.mm2),digits =2 )
meanDataAreaW011_G1_TR$Wash <- "W011"
meanDataAreaW011_G1_TR$Condition <- "W011_G1"

meanDataAreaW013_G1_TR<- data.frame(meanArea=round(mean(DataAreaW013_G1_TR$Area.mm2),digits =2 ))
meanDataAreaW013_G1_TR$SD<- round(sd(DataAreaW013_G1_TR$Area.mm2),digits =2 )
meanDataAreaW013_G1_TR$Wash <- "W013"
meanDataAreaW013_G1_TR$Condition <- "W013_G1"

meanDataAreaW015_G1_TR<- data.frame(meanArea=round(mean(DataAreaW015_G1_TR$Area.mm2),digits =2 ))
meanDataAreaW015_G1_TR$SD<- round(sd(DataAreaW015_G1_TR$Area.mm2),digits =2 )
meanDataAreaW015_G1_TR$Wash <- "W015"
meanDataAreaW015_G1_TR$Condition <- "W015_G1"

FibreCount_TransferArea_G1 <- rbind(meanDataAreaW000_G1_TR, meanDataAreaW001_G1_TR, meanDataAreaW003_G1_TR, meanDataAreaW005_G1_TR, meanDataAreaW007_G1_TR,meanDataAreaW009_G1_TR,meanDataAreaW011_G1_TR,meanDataAreaW013_G1_TR,meanDataAreaW015_G1_TR)

#write.table(FibreCount_TransferArea_G1, file = "Transfer_Area.csv", quote = F, sep = ",", row.names = F)

# plot
pTR_G1 <- ggplot(FibreCount_TransferArea_G1, aes(x = factor(Wash, level = c('W000','W001','W003','W005','W007','W009','W011','W013','W015')),
                                          y= meanArea, fill=Condition))+
  geom_bar(stat="identity", position=position_dodge(),colour="black")+
  geom_text(aes(label = meanArea), hjust=0.5,vjust = -5.5,position = position_dodge(.9))+
  labs(x="\nWash", y="Total fibre area (mm\u00b2)\n") +
  theme_bw(base_family = "Arial", base_size = 12) +
  ylim(0,2.5)+
  scale_fill_manual(values = brewer.pal(9, "Purples")[1:9])+
  theme(legend.title = element_blank(),
        strip.background.x = element_rect(colour = "grey", fill = "white"),
        legend.position = "bottom",
        legend.background = element_rect(fill="grey95",size=1, linetype="solid", colour="grey80"),
        axis.text.x = element_text(angle = 0, vjust = 0.95, hjust=0.5))+
  geom_errorbar(aes(ymin=meanArea-SD, ymax=meanArea+SD),width=.2,position=position_dodge(.9))
pTR_G1
ggsave("TransferArea_G1_W000-15.png", pTR_G1, width = 10, height = 9, units = "in", dpi=150, path = "Results")

# comparison to shedding
SH_meanDataAreaW000_G1_800 <- meanDataAreaW000_G1_800[,!names(meanDataAreaW000_G1_800) %in% c("Weight")]
SH_meanDataAreaW001_G1_800 <- meanDataAreaW001_G1_800[,!names(meanDataAreaW001_G1_800) %in% c("Weight")]
SH_meanDataAreaW003_G1_800 <- meanDataAreaW003_G1_800[,!names(meanDataAreaW003_G1_800) %in% c("Weight")]
SH_meanDataAreaW005_G1_800 <- meanDataAreaW005_G1_800[,!names(meanDataAreaW005_G1_800) %in% c("Weight")]
SH_meanDataAreaW007_G1_800 <- meanDataAreaW007_G1_800[,!names(meanDataAreaW007_G1_800) %in% c("Weight")]
SH_meanDataAreaW009_G1_800 <- meanDataAreaW009_G1_800[,!names(meanDataAreaW009_G1_800) %in% c("Weight")]
SH_meanDataAreaW011_G1_800 <- meanDataAreaW011_G1_800[,!names(meanDataAreaW011_G1_800) %in% c("Weight")]

SH_meanDataAreaW000_G1_800$Wash <- "W000"
SH_meanDataAreaW001_G1_800$Wash <- "W001"
SH_meanDataAreaW003_G1_800$Wash <- "W003"
SH_meanDataAreaW005_G1_800$Wash <- "W005"
SH_meanDataAreaW007_G1_800$Wash <- "W007"
SH_meanDataAreaW009_G1_800$Wash <- "W009"
SH_meanDataAreaW011_G1_800$Wash <- "W011"

SH_meanDataAreaW000_G1_800$Condition <- "W000_G1"
SH_meanDataAreaW001_G1_800$Condition <- "W001_G1"
SH_meanDataAreaW003_G1_800$Condition <- "W003_G1"
SH_meanDataAreaW005_G1_800$Condition <- "W005_G1"
SH_meanDataAreaW007_G1_800$Condition <- "W007_G1"
SH_meanDataAreaW009_G1_800$Condition <- "W009_G1"
SH_meanDataAreaW011_G1_800$Condition <- "W011_G1"

FibreCount_Shedding_800g_G1 <- rbind(SH_meanDataAreaW000_G1_800,SH_meanDataAreaW001_G1_800,SH_meanDataAreaW003_G1_800,SH_meanDataAreaW005_G1_800,SH_meanDataAreaW007_G1_800,SH_meanDataAreaW009_G1_800,SH_meanDataAreaW011_G1_800)

FibreCount_TransferArea_G1$Coder <-"Transfer Experiments"
FibreCount_Shedding_800g_G1$Coder <-"Shedding Experiments"
G1_FibreCount_Total <- rbind(FibreCount_TransferArea_G1,FibreCount_Shedding_800g_G1)

pSHTR_G1 <- ggplot(G1_FibreCount_Total, aes(x = factor(Wash, level = c('W000','W001','W003','W005','W007','W009','W011')),
                                             y= meanArea, fill=Coder))+
  geom_bar(stat="identity", position=position_dodge(),colour="black")+
  geom_text(aes(label = meanArea), hjust=0.5,vjust = -5.5,position = position_dodge(.9))+
  labs(x="\nWeight", y="Total fibre area (mm\u00b2)\n") +
  theme_bw(base_family = "Arial", base_size = 12) +
  ylim(0,290)+
  scale_fill_manual(values = brewer.pal(9, "Set1")[1:9])+
  theme(legend.title = element_blank(),
        strip.background.x = element_rect(colour = "Greys", fill = "white"),
        legend.position = "bottom",
        legend.background = element_rect(fill="grey95",size=1, linetype="solid", colour="grey80"),
        axis.text.x = element_text(angle = 0, vjust = 0.95, hjust=0.5))+
  geom_errorbar(aes(ymin=meanArea-SD, ymax=meanArea+SD),width=.2,position=position_dodge(.9))
pSHTR_G1
#ggsave("Fibre Count boxplot_SHTR_G1.png", pAtr_Total, width = 6, height = 7, units = "in", dpi=150, path = "Results")

##################
#####   G2   #####
##################
TransferArea_G2 <- read.csv('./Fibre count Summary/TR_G2_W000-W015_Summary.csv', sep="," ,header = T,fileEncoding="UTF-8-BOM")
TransferArea_G2$Slice<- gsub(".TIF","",TransferArea_G2$Slice)
TransferArea_G2Extended <- data.frame(str_split(TransferArea_G2$Slice, "_", simplify=TRUE))
names(TransferArea_G2Extended) <- c("Project","Wash","Garment","Bound","Condition")
TransferArea_G2Total <- cbind(TransferArea_G2Extended,Area=TransferArea_G2$Total.Area)

# Convert Area from inch2 to mm2
# 1 pixel = 1 x10^-5 inch2, so (Area*1)/0.000011 to convert into px
TransferArea_G2Total$Area.px <- (TransferArea_G2Total$Area*1)/0.000011
# 1 mm = 108 pixels, 1mm2 = 11664 px
TransferArea_G2Total$Area.mm2 <- TransferArea_G2Total$Area.px/11664

# Split data per washing condition
DataAreaW000_G2_TR <- TransferArea_G2Total[TransferArea_G2Total$Wash =='W000',]
DataAreaW001_G2_TR <- TransferArea_G2Total[TransferArea_G2Total$Wash =='W001',]
DataAreaW003_G2_TR <- TransferArea_G2Total[TransferArea_G2Total$Wash =='W003',]
DataAreaW005_G2_TR <- TransferArea_G2Total[TransferArea_G2Total$Wash =='W005',]
DataAreaW007_G2_TR <- TransferArea_G2Total[TransferArea_G2Total$Wash =='W007',]
DataAreaW009_G2_TR <- TransferArea_G2Total[TransferArea_G2Total$Wash =='W009',]
DataAreaW010_G2_TR <- TransferArea_G2Total[TransferArea_G2Total$Wash =='W010',]
DataAreaW013_G2_TR <- TransferArea_G2Total[TransferArea_G2Total$Wash =='W013',]
DataAreaW015_G2_TR <- TransferArea_G2Total[TransferArea_G2Total$Wash =='W015',]

# Calculation of mean and SD
meanDataAreaW000_G2_TR<- data.frame(meanArea=round(mean(DataAreaW000_G2_TR$Area.mm2),digits =2 ))
meanDataAreaW000_G2_TR$SD<- round(sd(DataAreaW000_G2_TR$Area.mm2),digits =2 )
meanDataAreaW000_G2_TR$Wash <- "W000"
meanDataAreaW000_G2_TR$Condition <- "W000_G2"

meanDataAreaW001_G2_TR<- data.frame(meanArea=round(mean(DataAreaW001_G2_TR$Area.mm2),digits =2 ))
meanDataAreaW001_G2_TR$SD<- round(sd(DataAreaW001_G2_TR$Area.mm2),digits =2 )
meanDataAreaW001_G2_TR$Wash <- "W001"
meanDataAreaW001_G2_TR$Condition <- "W001_G2"

meanDataAreaW003_G2_TR<- data.frame(meanArea=round(mean(DataAreaW003_G2_TR$Area.mm2),digits =2 ))
meanDataAreaW003_G2_TR$SD<- round(sd(DataAreaW003_G2_TR$Area.mm2),digits =2 )
meanDataAreaW003_G2_TR$Wash <- "W003"
meanDataAreaW003_G2_TR$Condition <- "W003_G2"

meanDataAreaW005_G2_TR<- data.frame(meanArea=round(mean(DataAreaW005_G2_TR$Area.mm2),digits =2 ))
meanDataAreaW005_G2_TR$SD<- round(sd(DataAreaW005_G2_TR$Area.mm2),digits =2 )
meanDataAreaW005_G2_TR$Wash <- "W005"
meanDataAreaW005_G2_TR$Condition <- "W005_G2"

meanDataAreaW007_G2_TR<- data.frame(meanArea=round(mean(DataAreaW007_G2_TR$Area.mm2),digits =2 ))
meanDataAreaW007_G2_TR$SD<- round(sd(DataAreaW007_G2_TR$Area.mm2),digits =2 )
meanDataAreaW007_G2_TR$Wash <- "W007"
meanDataAreaW007_G2_TR$Condition <- "W007_G2"

meanDataAreaW009_G2_TR<- data.frame(meanArea=round(mean(DataAreaW009_G2_TR$Area.mm2),digits =2 ))
meanDataAreaW009_G2_TR$SD<- round(sd(DataAreaW009_G2_TR$Area.mm2),digits =2 )
meanDataAreaW009_G2_TR$Wash <- "W009"
meanDataAreaW009_G2_TR$Condition <- "W009_G2"

meanDataAreaW010_G2_TR<- data.frame(meanArea=round(mean(DataAreaW010_G2_TR$Area.mm2),digits =2 ))
meanDataAreaW010_G2_TR$SD<- round(sd(DataAreaW010_G2_TR$Area.mm2),digits =2 )
meanDataAreaW010_G2_TR$Wash <- "W010"
meanDataAreaW010_G2_TR$Condition <- "W010_G2"

meanDataAreaW013_G2_TR<- data.frame(meanArea=round(mean(DataAreaW013_G2_TR$Area.mm2),digits =2 ))
meanDataAreaW013_G2_TR$SD<- round(sd(DataAreaW013_G2_TR$Area.mm2),digits =2 )
meanDataAreaW013_G2_TR$Wash <- "W013"
meanDataAreaW013_G2_TR$Condition <- "W013_G2"

meanDataAreaW015_G2_TR<- data.frame(meanArea=round(mean(DataAreaW015_G2_TR$Area.mm2),digits =2 ))
meanDataAreaW015_G2_TR$SD<- round(sd(DataAreaW015_G2_TR$Area.mm2),digits =2 )
meanDataAreaW015_G2_TR$Wash <- "W015"
meanDataAreaW015_G2_TR$Condition <- "W015_G2"

FibreCount_TransferArea_G2 <- rbind(meanDataAreaW000_G2_TR, meanDataAreaW001_G2_TR, meanDataAreaW003_G2_TR, meanDataAreaW005_G2_TR, meanDataAreaW007_G2_TR,meanDataAreaW009_G2_TR, meanDataAreaW010_G2_TR, meanDataAreaW013_G2_TR,meanDataAreaW015_G2_TR)

#write.table(FibreCount_TransferArea_G2, file = "Transfer_Area.csv", quote = F, sep = ",", row.names = F)

# plot
pTR_G2 <- ggplot(FibreCount_TransferArea_G2, aes(x = factor(Wash, level = c('W000', 'W001','W003','W005','W007','W009','W010','W013','W015')),
                                                 y= meanArea, fill=Condition))+
  geom_bar(stat="identity", position=position_dodge(),colour="black")+
  geom_text(aes(label = meanArea), hjust=0.5,vjust = -5.5,position = position_dodge(.9))+
  labs(x="\nWash", y="Total fibre area (mm\u00b2)\n") +
  theme_bw(base_family = "Arial", base_size = 12) +
  ylim(0,3)+
  scale_fill_manual(values = brewer.pal(9, "Oranges")[1:9])+
  theme(legend.title = element_blank(),
        strip.background.x = element_rect(colour = "grey", fill = "white"),
        legend.position = "bottom",
        legend.background = element_rect(fill="grey95",size=1, linetype="solid", colour="grey80"),
        axis.text.x = element_text(angle = 0, vjust = 0.95, hjust=0.5))+
  geom_errorbar(aes(ymin=meanArea-SD, ymax=meanArea+SD),width=.2,position=position_dodge(.9))
pTR_G2
#ggsave("TransferArea_G2_W000-15.png", pTR_G2, width = 10, height = 9, units = "in", dpi=150, path = "Results")

##################
#####   G3   #####
##################
TransferArea_G3 <- read.csv('./Fibre count Summary/TR_G3_W000-W015_Summary.csv', sep="," ,header = T,fileEncoding="UTF-8-BOM")
TransferArea_G3$Slice<- gsub(".TIF","",TransferArea_G3$Slice)
TransferArea_G3Extended <- data.frame(str_split(TransferArea_G3$Slice, "_", simplify=TRUE))
names(TransferArea_G3Extended) <- c("Project","Wash","Garment","Bound","Condition")
TransferArea_G3Total <- cbind(TransferArea_G3Extended,Area=TransferArea_G3$Total.Area)

# Convert Area from inch2 to mm2
# 1 pixel = 1 x10^-5 inch2, so (Area*1)/0.000011 to convert into px
TransferArea_G3Total$Area.px <- (TransferArea_G3Total$Area*1)/0.000011
# 1 mm = 108 pixels, 1mm2 = 11664 px
TransferArea_G3Total$Area.mm2 <- TransferArea_G3Total$Area.px/11664

# Split data per washing condition
DataAreaW000_G3_TR <- TransferArea_G3Total[TransferArea_G3Total$Wash =='W000',]
DataAreaW001_G3_TR <- TransferArea_G3Total[TransferArea_G3Total$Wash =='W001',]
DataAreaW003_G3_TR <- TransferArea_G3Total[TransferArea_G3Total$Wash =='W003',]
DataAreaW005_G3_TR <- TransferArea_G3Total[TransferArea_G3Total$Wash =='W005',]
DataAreaW007_G3_TR <- TransferArea_G3Total[TransferArea_G3Total$Wash =='W007',]
DataAreaW009_G3_TR <- TransferArea_G3Total[TransferArea_G3Total$Wash =='W009',]
DataAreaW010_G3_TR <- TransferArea_G3Total[TransferArea_G3Total$Wash =='W010',]
DataAreaW013_G3_TR <- TransferArea_G3Total[TransferArea_G3Total$Wash =='W013',]
DataAreaW015_G3_TR <- TransferArea_G3Total[TransferArea_G3Total$Wash =='W015',]

# Calculation of mean and SD
meanDataAreaW000_G3_TR<- data.frame(meanArea=round(mean(DataAreaW000_G3_TR$Area.mm2),digits =2 ))
meanDataAreaW000_G3_TR$SD<- round(sd(DataAreaW000_G3_TR$Area.mm2),digits =2 )
meanDataAreaW000_G3_TR$Wash <- "W000"
meanDataAreaW000_G3_TR$Condition <- "W000_G3"

meanDataAreaW001_G3_TR<- data.frame(meanArea=round(mean(DataAreaW001_G3_TR$Area.mm2),digits =2 ))
meanDataAreaW001_G3_TR$SD<- round(sd(DataAreaW001_G3_TR$Area.mm2),digits =2 )
meanDataAreaW001_G3_TR$Wash <- "W001"
meanDataAreaW001_G3_TR$Condition <- "W001_G3"

meanDataAreaW003_G3_TR<- data.frame(meanArea=round(mean(DataAreaW003_G3_TR$Area.mm2),digits =2 ))
meanDataAreaW003_G3_TR$SD<- round(sd(DataAreaW003_G3_TR$Area.mm2),digits =2 )
meanDataAreaW003_G3_TR$Wash <- "W003"
meanDataAreaW003_G3_TR$Condition <- "W003_G3"

meanDataAreaW005_G3_TR<- data.frame(meanArea=round(mean(DataAreaW005_G3_TR$Area.mm2),digits =2 ))
meanDataAreaW005_G3_TR$SD<- round(sd(DataAreaW005_G3_TR$Area.mm2),digits =2 )
meanDataAreaW005_G3_TR$Wash <- "W005"
meanDataAreaW005_G3_TR$Condition <- "W005_G3"

meanDataAreaW007_G3_TR<- data.frame(meanArea=round(mean(DataAreaW007_G3_TR$Area.mm2),digits =2 ))
meanDataAreaW007_G3_TR$SD<- round(sd(DataAreaW007_G3_TR$Area.mm2),digits =2 )
meanDataAreaW007_G3_TR$Wash <- "W007"
meanDataAreaW007_G3_TR$Condition <- "W007_G3"

meanDataAreaW009_G3_TR<- data.frame(meanArea=round(mean(DataAreaW009_G3_TR$Area.mm2),digits =2 ))
meanDataAreaW009_G3_TR$SD<- round(sd(DataAreaW009_G3_TR$Area.mm2),digits =2 )
meanDataAreaW009_G3_TR$Wash <- "W009"
meanDataAreaW009_G3_TR$Condition <- "W009_G3"

meanDataAreaW010_G3_TR<- data.frame(meanArea=round(mean(DataAreaW010_G3_TR$Area.mm2),digits =2 ))
meanDataAreaW010_G3_TR$SD<- round(sd(DataAreaW010_G3_TR$Area.mm2),digits =2 )
meanDataAreaW010_G3_TR$Wash <- "W010"
meanDataAreaW010_G3_TR$Condition <- "W010_G3"

meanDataAreaW013_G3_TR<- data.frame(meanArea=round(mean(DataAreaW013_G3_TR$Area.mm2),digits =2 ))
meanDataAreaW013_G3_TR$SD<- round(sd(DataAreaW013_G3_TR$Area.mm2),digits =2 )
meanDataAreaW013_G3_TR$Wash <- "W013"
meanDataAreaW013_G3_TR$Condition <- "W013_G3"

meanDataAreaW015_G3_TR<- data.frame(meanArea=round(mean(DataAreaW015_G3_TR$Area.mm2),digits =2 ))
meanDataAreaW015_G3_TR$SD<- round(sd(DataAreaW015_G3_TR$Area.mm2),digits =2 )
meanDataAreaW015_G3_TR$Wash <- "W015"
meanDataAreaW015_G3_TR$Condition <- "W015_G3"

FibreCount_TransferArea_G3 <- rbind(meanDataAreaW000_G3_TR, meanDataAreaW001_G3_TR, meanDataAreaW003_G3_TR, meanDataAreaW005_G3_TR, meanDataAreaW007_G3_TR,meanDataAreaW009_G3_TR, meanDataAreaW010_G3_TR,meanDataAreaW013_G3_TR,meanDataAreaW015_G3_TR)

#write.table(FibreCount_TransferArea_G3, file = "Transfer_Area.csv", quote = F, sep = ",", row.names = F)

# plot
pTR_G3 <- ggplot(FibreCount_TransferArea_G3, aes(x = factor(Wash, level = c('W000','W001','W003','W005','W007','W009','W010','W013','W015')),
                                                 y= meanArea, fill=Condition))+
  geom_bar(stat="identity", position=position_dodge(),colour="black")+
  geom_text(aes(label = meanArea), hjust=0.5,vjust = -5.5,position = position_dodge(.9))+
  labs(x="\nWash", y="Total fibre area (mm\u00b2)\n") +
  theme_bw(base_family = "Arial", base_size = 12) +
  ylim(0,3)+
  scale_fill_manual(values = brewer.pal(9, "Greens")[1:9])+
  theme(legend.title = element_blank(),
        strip.background.x = element_rect(colour = "grey", fill = "white"),
        legend.position = "bottom",
        legend.background = element_rect(fill="grey95",size=1, linetype="solid", colour="grey80"),
        axis.text.x = element_text(angle = 0, vjust = 0.95, hjust=0.5))+
  geom_errorbar(aes(ymin=meanArea-SD, ymax=meanArea+SD),width=.2,position=position_dodge(.9))
pTR_G3
ggsave("TransferArea_G3_W000-15.png", pTR_G3, width = 10, height = 9, units = "in", dpi=150, path = "Results")

###################
#####   G4   #####
###################
TransferArea_G4 <- read.csv('./Fibre count Summary/TR_G4_W000-W011_Summary.csv', sep="," ,header = T,fileEncoding="UTF-8-BOM")
TransferArea_G4$Slice<- gsub(".TIF","",TransferArea_G4$Slice)
TransferArea_G4Extended <- data.frame(str_split(TransferArea_G4$Slice, "_", simplify=TRUE))
names(TransferArea_G4Extended) <- c("Project","Wash","Garment","Bound","Condition")
TransferArea_G4Total <- cbind(TransferArea_G4Extended,Area=TransferArea_G4$Total.Area)

# Convert Area from inch2 to mm2
# 1 pixel = 1 x10^-5 inch2, so (Area*1)/0.000011 to convert into px
TransferArea_G4Total$Area.px <- (TransferArea_G4Total$Area*1)/0.000011
# 1 mm = 108 pixels, 1mm2 = 11664 px
TransferArea_G4Total$Area.mm2 <- TransferArea_G4Total$Area.px/11664

# Split data per washing condition
DataAreaW000_G4_TR <- TransferArea_G4Total[TransferArea_G4Total$Wash =='W000',]
DataAreaW001_G4_TR <- TransferArea_G4Total[TransferArea_G4Total$Wash =='W001',]
DataAreaW003_G4_TR <- TransferArea_G4Total[TransferArea_G4Total$Wash =='W003',]
DataAreaW005_G4_TR <- TransferArea_G4Total[TransferArea_G4Total$Wash =='W005',]
DataAreaW007_G4_TR <- TransferArea_G4Total[TransferArea_G4Total$Wash =='W007',]
DataAreaW009_G4_TR <- TransferArea_G4Total[TransferArea_G4Total$Wash =='W009',]
DataAreaW011_G4_TR <- TransferArea_G4Total[TransferArea_G4Total$Wash =='W011',]
DataAreaW013_G4_TR <- TransferArea_G4Total[TransferArea_G4Total$Wash =='W013',]
DataAreaW015_G4_TR <- TransferArea_G4Total[TransferArea_G4Total$Wash =='W015',]

# Calculation of mean and SD
meanDataAreaW000_G4_TR<- data.frame(meanArea=round(mean(DataAreaW000_G4_TR$Area.mm2),digits =2 ))
meanDataAreaW000_G4_TR$SD<- round(sd(DataAreaW000_G4_TR$Area.mm2),digits =2 )
meanDataAreaW000_G4_TR$Wash <- "W000"
meanDataAreaW000_G4_TR$Condition <- "W000_G4"

meanDataAreaW001_G4_TR<- data.frame(meanArea=round(mean(DataAreaW001_G4_TR$Area.mm2),digits =2 ))
meanDataAreaW001_G4_TR$SD<- round(sd(DataAreaW001_G4_TR$Area.mm2),digits =2 )
meanDataAreaW001_G4_TR$Wash <- "W001"
meanDataAreaW001_G4_TR$Condition <- "W001_G4"

meanDataAreaW003_G4_TR<- data.frame(meanArea=round(mean(DataAreaW003_G4_TR$Area.mm2),digits =2 ))
meanDataAreaW003_G4_TR$SD<- round(sd(DataAreaW003_G4_TR$Area.mm2),digits =2 )
meanDataAreaW003_G4_TR$Wash <- "W003"
meanDataAreaW003_G4_TR$Condition <- "W003_G4"

meanDataAreaW005_G4_TR<- data.frame(meanArea=round(mean(DataAreaW005_G4_TR$Area.mm2),digits =2 ))
meanDataAreaW005_G4_TR$SD<- round(sd(DataAreaW005_G4_TR$Area.mm2),digits =2 )
meanDataAreaW005_G4_TR$Wash <- "W005"
meanDataAreaW005_G4_TR$Condition <- "W005_G4"

meanDataAreaW007_G4_TR<- data.frame(meanArea=round(mean(DataAreaW007_G4_TR$Area.mm2),digits =2 ))
meanDataAreaW007_G4_TR$SD<- round(sd(DataAreaW007_G4_TR$Area.mm2),digits =2 )
meanDataAreaW007_G4_TR$Wash <- "W007"
meanDataAreaW007_G4_TR$Condition <- "W007_G4"

meanDataAreaW009_G4_TR<- data.frame(meanArea=round(mean(DataAreaW009_G4_TR$Area.mm2),digits =2 ))
meanDataAreaW009_G4_TR$SD<- round(sd(DataAreaW009_G4_TR$Area.mm2),digits =2 )
meanDataAreaW009_G4_TR$Wash <- "W009"
meanDataAreaW009_G4_TR$Condition <- "W009_G4"

meanDataAreaW011_G4_TR<- data.frame(meanArea=round(mean(DataAreaW011_G4_TR$Area.mm2),digits =2 ))
meanDataAreaW011_G4_TR$SD<- round(sd(DataAreaW011_G4_TR$Area.mm2),digits =2 )
meanDataAreaW011_G4_TR$Wash <- "W011"
meanDataAreaW011_G4_TR$Condition <- "W011_G4"

meanDataAreaW013_G4_TR<- data.frame(meanArea=round(mean(DataAreaW013_G4_TR$Area.mm2),digits =2 ))
meanDataAreaW013_G4_TR$SD<- round(sd(DataAreaW013_G4_TR$Area.mm2),digits =2 )
meanDataAreaW013_G4_TR$Wash <- "W013"
meanDataAreaW013_G4_TR$Condition <- "W013_G4"

meanDataAreaW015_G4_TR<- data.frame(meanArea=round(mean(DataAreaW015_G4_TR$Area.mm2),digits =2 ))
meanDataAreaW015_G4_TR$SD<- round(sd(DataAreaW015_G4_TR$Area.mm2),digits =2 )
meanDataAreaW015_G4_TR$Wash <- "W015"
meanDataAreaW015_G4_TR$Condition <- "W015_G4"

FibreCount_TransferArea_G4 <- rbind(meanDataAreaW000_G4_TR, meanDataAreaW001_G4_TR, meanDataAreaW003_G4_TR, meanDataAreaW005_G4_TR, meanDataAreaW007_G4_TR,meanDataAreaW009_G4_TR, meanDataAreaW011_G4_TR) #,meanDataAreaW013_G4_TR,meanDataAreaW015_G4_TR)

#write.table(FibreCount_TransferArea_G4, file = "Transfer_Area.csv", quote = F, sep = ",", row.names = F)

# plot
pTR_G4 <- ggplot(FibreCount_TransferArea_G4, aes(x = factor(Wash, level = c('W000','W001','W003','W005','W007','W009','W011','W013','W015')),
                                                 y= meanArea, fill=Condition))+
  geom_bar(stat="identity", position=position_dodge(),colour="black")+
  geom_text(aes(label = meanArea), hjust=0.5,vjust = -5.5,position = position_dodge(.9))+
  labs(x="\nWash", y="Total fibre area (mm\u00b2)\n") +
  theme_bw(base_family = "Arial", base_size = 12) +
  ylim(0,5)+
  scale_fill_manual(values = brewer.pal(9, "YlGn")[1:9])+
  theme(legend.title = element_blank(),
        strip.background.x = element_rect(colour = "grey", fill = "white"),
        legend.position = "bottom",
        legend.background = element_rect(fill="grey95",size=1, linetype="solid", colour="grey80"),
        axis.text.x = element_text(angle = 0, vjust = 0.95, hjust=0.5))+
  geom_errorbar(aes(ymin=meanArea-SD, ymax=meanArea+SD),width=.2,position=position_dodge(.9))
pTR_G4
ggsave("TransferArea_G4_W000-11.png", pTR_G4, width = 10, height = 9, units = "in", dpi=150, path = "Results")


# COMBINED

FibreCount_TransferArea_G1$Coder <-"Garment 1"
FibreCount_TransferArea_G2$Coder <-"Garment 2"
FibreCount_TransferArea_G3$Coder <-"Garment 3"
FibreCount_TransferArea_G4A$Coder <- "Garment 4A"
FibreCount_TransferArea_G4B$Coder <- "Garment 4B"
FibreCount_TransferArea_G4C$Coder <- "Garment 4C"
TR_FibreCount_Total <- rbind(FibreCount_TransferArea_G1,FibreCount_TransferArea_G2,FibreCount_TransferArea_G3,FibreCount_TransferArea_G4A,FibreCount_TransferArea_G4B,FibreCount_TransferArea_G4C)

pTR_comb <- ggplot(TR_FibreCount_Total, aes(x = factor(Wash, level = c('W000','W001','W003','W005','W007','W009','W010','W011','W013','W015')),
                                            y= meanArea, fill=Coder))+
  geom_bar(stat="identity", position=position_dodge(),colour="black")+
  geom_text(aes(label = meanArea), hjust=0.5,vjust = -5.5,position = position_dodge(.9))+
  labs(x="\nWeight", y="Total fibre area (mm\u00b2)\n") +
  theme_bw(base_family = "Arial", base_size = 12) +
  ylim(0,3)+
  scale_fill_manual(values = brewer.pal(9, "Set1")[1:9])+
  theme(legend.title = element_blank(),
        strip.background.x = element_rect(colour = "Greys", fill = "white"),
        legend.position = "bottom",
        legend.background = element_rect(fill="grey95",size=1, linetype="solid", colour="grey80"),
        axis.text.x = element_text(angle = 0, vjust = 0.95, hjust=0.5))+
  geom_errorbar(aes(ymin=meanArea-SD, ymax=meanArea+SD),width=.2,position=position_dodge(.9))
pTR_comb
ggsave("TransferArea_Total.png", TR_FibreCount_Total, width = 6, height = 7, units = "in", dpi=150, path = "Results")
