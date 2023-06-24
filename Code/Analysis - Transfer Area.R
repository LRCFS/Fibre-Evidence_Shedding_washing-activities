#########################################################
#####         TRANSFER - TOTAL FIBRE AREA           #####
#########################################################
##################
#####   G1   #####
##################
TransferArea_G1 <- read.csv('./Fibre count Summary/TR_G1_W000-W009_Summary.csv', sep="," ,header = T,fileEncoding="UTF-8-BOM")
TransferArea_G1$Slice<- gsub(".TIF","",TransferArea_G1$Slice)
TransferArea_G1Extended <- data.frame(str_split(TransferArea_G1$Slice, "_", simplify=TRUE))
names(TransferArea_G1Extended) <- c("Project","Wash","Garment","Bound","Condition")
TransferArea_G1Total <- cbind(TransferArea_G1Extended,Area=TransferArea_G1$Total.Area)

# Convert Area from inch2 to mm2
# 1 pixel = 1 x10^-5 inch2, so (Area*1)/0.000011 to convert into px
TransferArea_G1Total$Area.px <- (TransferArea_G1Total$Area*1)/0.000011
# 1 mm = 112 pixels, 1mm2 = 12544 px
TransferArea_G1Total$Area.mm2 <- TransferArea_G1Total$Area.px/12544

# Split data per washing condition
DataAreaW000_G1_TR <- TransferArea_G1Total[TransferArea_G1Total$Wash =='W000',]
DataAreaW001_G1_TR <- TransferArea_G1Total[TransferArea_G1Total$Wash =='W001',]
DataAreaW002_G1_TR <- TransferArea_G1Total[TransferArea_G1Total$Wash =='W002',]
DataAreaW003_G1_TR <- TransferArea_G1Total[TransferArea_G1Total$Wash =='W003',]
DataAreaW004_G1_TR <- TransferArea_G1Total[TransferArea_G1Total$Wash =='W004',]
DataAreaW005_G1_TR <- TransferArea_G1Total[TransferArea_G1Total$Wash =='W005',]
DataAreaW006_G1_TR <- TransferArea_G1Total[TransferArea_G1Total$Wash =='W006',]
DataAreaW007_G1_TR <- TransferArea_G1Total[TransferArea_G1Total$Wash =='W007',]
DataAreaW009_G1_TR <- TransferArea_G1Total[TransferArea_G1Total$Wash =='W009',]

# Calculation of mean and SD
meanDataAreaW000_G1_TR<- data.frame(meanArea=round(mean(DataAreaW000_G1_TR$Area.mm2),digits =2 ))
meanDataAreaW000_G1_TR$SD<- round(sd(DataAreaW000_G1_TR$Area.mm2),digits =2 )
meanDataAreaW000_G1_TR$Wash <- "W000"
meanDataAreaW000_G1_TR$Condition <- "W000_G1"

meanDataAreaW001_G1_TR<- data.frame(meanArea=round(mean(DataAreaW001_G1_TR$Area.mm2),digits =2 ))
meanDataAreaW001_G1_TR$SD<- round(sd(DataAreaW001_G1_TR$Area.mm2),digits =2 )
meanDataAreaW001_G1_TR$Wash <- "W001"
meanDataAreaW001_G1_TR$Condition <- "W001_G1"

meanDataAreaW002_G1_TR<- data.frame(meanArea=round(mean(DataAreaW002_G1_TR$Area.mm2),digits =2 ))
meanDataAreaW002_G1_TR$SD<- round(sd(DataAreaW002_G1_TR$Area.mm2),digits =2 )
meanDataAreaW002_G1_TR$Wash <- "W002"
meanDataAreaW002_G1_TR$Condition <- "W002_G1"

meanDataAreaW003_G1_TR<- data.frame(meanArea=round(mean(DataAreaW003_G1_TR$Area.mm2),digits =2 ))
meanDataAreaW003_G1_TR$SD<- round(sd(DataAreaW003_G1_TR$Area.mm2),digits =2 )
meanDataAreaW003_G1_TR$Wash <- "W003"
meanDataAreaW003_G1_TR$Condition <- "W003_G1"

meanDataAreaW004_G1_TR<- data.frame(meanArea=round(mean(DataAreaW004_G1_TR$Area.mm2),digits =2 ))
meanDataAreaW004_G1_TR$SD<- round(sd(DataAreaW004_G1_TR$Area.mm2),digits =2 )
meanDataAreaW004_G1_TR$Wash <- "W004"
meanDataAreaW004_G1_TR$Condition <- "W004_G1"

meanDataAreaW005_G1_TR<- data.frame(meanArea=round(mean(DataAreaW005_G1_TR$Area.mm2),digits =2 ))
meanDataAreaW005_G1_TR$SD<- round(sd(DataAreaW005_G1_TR$Area.mm2),digits =2 )
meanDataAreaW005_G1_TR$Wash <- "W005"
meanDataAreaW005_G1_TR$Condition <- "W005_G1"

meanDataAreaW006_G1_TR<- data.frame(meanArea=round(mean(DataAreaW006_G1_TR$Area.mm2),digits =2 ))
meanDataAreaW006_G1_TR$SD<- round(sd(DataAreaW006_G1_TR$Area.mm2),digits =2 )
meanDataAreaW006_G1_TR$Wash <- "W006"
meanDataAreaW006_G1_TR$Condition <- "W006_G1"

meanDataAreaW007_G1_TR<- data.frame(meanArea=round(mean(DataAreaW007_G1_TR$Area.mm2),digits =2 ))
meanDataAreaW007_G1_TR$SD<- round(sd(DataAreaW007_G1_TR$Area.mm2),digits =2 )
meanDataAreaW007_G1_TR$Wash <- "W007"
meanDataAreaW007_G1_TR$Condition <- "W007_G1"

meanDataAreaW009_G1_TR<- data.frame(meanArea=round(mean(DataAreaW009_G1_TR$Area.mm2),digits =2 ))
meanDataAreaW009_G1_TR$SD<- round(sd(DataAreaW009_G1_TR$Area.mm2),digits =2 )
meanDataAreaW009_G1_TR$Wash <- "W009"
meanDataAreaW009_G1_TR$Condition <- "W009_G1"

FibreCount_TransferArea_G1 <- rbind(meanDataAreaW000_G1_TR, meanDataAreaW001_G1_TR, meanDataAreaW002_G1_TR, meanDataAreaW003_G1_TR, meanDataAreaW004_G1_TR, meanDataAreaW005_G1_TR, meanDataAreaW006_G1_TR, meanDataAreaW007_G1_TR,meanDataAreaW009_G1_TR)

#write.table(FibreCount_TransferArea_G1, file = "Transfer_Area.csv", quote = F, sep = ",", row.names = F)

# plot
pSH <- ggplot(FibreCount_TransferArea_G1, aes(x = factor(Wash, level = c('W000', 'W001', 'W002','W003','W004','W005','W006','W007','W009')),
                                          y= meanArea, fill=Condition))+
  geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label = meanArea), hjust=0.5,vjust = -5.5,position = position_dodge(.9))+
  labs(x="\nWash", y="Total fibre area (mm\u00b2)\n") +
  theme_bw(base_family = "Arial", base_size = 12) +
  ylim(0,5)+
  scale_fill_manual("legend", values = c("W000_G1" = "#91d4e3", "W001_G1" = "#4daabf","W002_G1" = "#148199","W003_G1" = "#6daae3","W004_G1" = "#4487c7",
                                         "W005_G1" = "#145796","W006_G1" = "#7b88d4","W007_G1" = "#4457c2", "W009_G1" = "#182da1"))+ # to obtain the colour brewer.pal(9, "YlOrRd")
  theme(legend.title = element_blank(),
        strip.background.x = element_rect(colour = "grey", fill = "white"),
        legend.position = "bottom",
        legend.background = element_rect(fill="grey95",size=1, linetype="solid", colour="grey80"),
        axis.text.x = element_text(angle = 0, vjust = 0.95, hjust=0.5))+
  geom_errorbar(aes(ymin=meanArea-SD, ymax=meanArea+SD),width=.2,position=position_dodge(.9))
pSH
ggsave("TransferArea_G1_W000-9.png", pSH, width = 10, height = 9, units = "in", dpi=150, path = "Results")

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
# 1 mm = 112 pixels, 1mm2 = 12544 px
TransferArea_G2Total$Area.mm2 <- TransferArea_G2Total$Area.px/12544

# Split data per washing condition
DataAreaW000_G2_TR <- TransferArea_G2Total[TransferArea_G2Total$Wash =='W000',]
DataAreaW001_G2_TR <- TransferArea_G2Total[TransferArea_G2Total$Wash =='W001',]
DataAreaW002_G2_TR <- TransferArea_G2Total[TransferArea_G2Total$Wash =='W002',]
DataAreaW003_G2_TR <- TransferArea_G2Total[TransferArea_G2Total$Wash =='W003',]
DataAreaW004_G2_TR <- TransferArea_G2Total[TransferArea_G2Total$Wash =='W004',]
DataAreaW005_G2_TR <- TransferArea_G2Total[TransferArea_G2Total$Wash =='W005',]
DataAreaW006_G2_TR <- TransferArea_G2Total[TransferArea_G2Total$Wash =='W006',]
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

meanDataAreaW002_G2_TR<- data.frame(meanArea=round(mean(DataAreaW002_G2_TR$Area.mm2),digits =2 ))
meanDataAreaW002_G2_TR$SD<- round(sd(DataAreaW002_G2_TR$Area.mm2),digits =2 )
meanDataAreaW002_G2_TR$Wash <- "W002"
meanDataAreaW002_G2_TR$Condition <- "W002_G2"

meanDataAreaW003_G2_TR<- data.frame(meanArea=round(mean(DataAreaW003_G2_TR$Area.mm2),digits =2 ))
meanDataAreaW003_G2_TR$SD<- round(sd(DataAreaW003_G2_TR$Area.mm2),digits =2 )
meanDataAreaW003_G2_TR$Wash <- "W003"
meanDataAreaW003_G2_TR$Condition <- "W003_G2"

meanDataAreaW004_G2_TR<- data.frame(meanArea=round(mean(DataAreaW004_G2_TR$Area.mm2),digits =2 ))
meanDataAreaW004_G2_TR$SD<- round(sd(DataAreaW004_G2_TR$Area.mm2),digits =2 )
meanDataAreaW004_G2_TR$Wash <- "W004"
meanDataAreaW004_G2_TR$Condition <- "W004_G2"

meanDataAreaW005_G2_TR<- data.frame(meanArea=round(mean(DataAreaW005_G2_TR$Area.mm2),digits =2 ))
meanDataAreaW005_G2_TR$SD<- round(sd(DataAreaW005_G2_TR$Area.mm2),digits =2 )
meanDataAreaW005_G2_TR$Wash <- "W005"
meanDataAreaW005_G2_TR$Condition <- "W005_G2"

meanDataAreaW006_G2_TR<- data.frame(meanArea=round(mean(DataAreaW006_G2_TR$Area.mm2),digits =2 ))
meanDataAreaW006_G2_TR$SD<- round(sd(DataAreaW006_G2_TR$Area.mm2),digits =2 )
meanDataAreaW006_G2_TR$Wash <- "W006"
meanDataAreaW006_G2_TR$Condition <- "W006_G2"

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

FibreCount_TransferArea_G2 <- rbind(meanDataAreaW000_G2_TR, meanDataAreaW001_G2_TR, meanDataAreaW002_G2_TR, meanDataAreaW003_G2_TR, meanDataAreaW004_G2_TR, meanDataAreaW005_G2_TR, meanDataAreaW006_G2_TR, meanDataAreaW007_G2_TR,meanDataAreaW009_G2_TR, meanDataAreaW010_G2_TR, meanDataAreaW013_G2_TR,meanDataAreaW015_G2_TR)

#write.table(FibreCount_TransferArea_G2, file = "Transfer_Area.csv", quote = F, sep = ",", row.names = F)

# plot
pSH <- ggplot(FibreCount_TransferArea_G2, aes(x = factor(Wash, level = c('W000', 'W001', 'W002','W003','W004','W005','W006','W007','W009','W010')),
                                              y= meanArea, fill=Condition))+
  geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label = meanArea), hjust=0.5,vjust = -5.5,position = position_dodge(.9))+
  labs(x="\nWash", y="Total fibre area (mm\u00b2)\n") +
  theme_bw(base_family = "Arial", base_size = 12) +
  ylim(0,5)+
  scale_fill_manual("legend", values = c("W000_G2" = "#91d4e3", "W001_G2" = "#4daabf","W002_G2" = "#148199","W003_G2" = "#6daae3","W004_G2" = "#4487c7",
                                         "W005_G2" = "#145796","W006_G2" = "#7b88d4","W007_G2" = "#4457c2", "W009_G2" = "#182da1", "W010_G2" = "#a28ae6", "W013_G2" = "#7357c2", "W015_G2" = "#401ca6"))+ # to obtain the colour brewer.pal(9, "YlOrRd")
  theme(legend.title = element_blank(),
        strip.background.x = element_rect(colour = "grey", fill = "white"),
        legend.position = "bottom",
        legend.background = element_rect(fill="grey95",size=1, linetype="solid", colour="grey80"),
        axis.text.x = element_text(angle = 0, vjust = 0.95, hjust=0.5))+
  geom_errorbar(aes(ymin=meanArea-SD, ymax=meanArea+SD),width=.2,position=position_dodge(.9))
pSH
ggsave("TransferArea_G2_W000-15.png", pSH, width = 10, height = 9, units = "in", dpi=150, path = "Results")

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
# 1 mm = 112 pixels, 1mm2 = 12544 px
TransferArea_G3Total$Area.mm2 <- TransferArea_G3Total$Area.px/12544

# Split data per washing condition
DataAreaW000_G3_TR <- TransferArea_G3Total[TransferArea_G3Total$Wash =='W000',]
DataAreaW001_G3_TR <- TransferArea_G3Total[TransferArea_G3Total$Wash =='W001',]
DataAreaW002_G3_TR <- TransferArea_G3Total[TransferArea_G3Total$Wash =='W002',]
DataAreaW003_G3_TR <- TransferArea_G3Total[TransferArea_G3Total$Wash =='W003',]
DataAreaW004_G3_TR <- TransferArea_G3Total[TransferArea_G3Total$Wash =='W004',]
DataAreaW005_G3_TR <- TransferArea_G3Total[TransferArea_G3Total$Wash =='W005',]
DataAreaW006_G3_TR <- TransferArea_G3Total[TransferArea_G3Total$Wash =='W006',]
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

meanDataAreaW002_G3_TR<- data.frame(meanArea=round(mean(DataAreaW002_G3_TR$Area.mm2),digits =2 ))
meanDataAreaW002_G3_TR$SD<- round(sd(DataAreaW002_G3_TR$Area.mm2),digits =2 )
meanDataAreaW002_G3_TR$Wash <- "W002"
meanDataAreaW002_G3_TR$Condition <- "W002_G3"

meanDataAreaW003_G3_TR<- data.frame(meanArea=round(mean(DataAreaW003_G3_TR$Area.mm2),digits =2 ))
meanDataAreaW003_G3_TR$SD<- round(sd(DataAreaW003_G3_TR$Area.mm2),digits =2 )
meanDataAreaW003_G3_TR$Wash <- "W003"
meanDataAreaW003_G3_TR$Condition <- "W003_G3"

meanDataAreaW004_G3_TR<- data.frame(meanArea=round(mean(DataAreaW004_G3_TR$Area.mm2),digits =2 ))
meanDataAreaW004_G3_TR$SD<- round(sd(DataAreaW004_G3_TR$Area.mm2),digits =2 )
meanDataAreaW004_G3_TR$Wash <- "W004"
meanDataAreaW004_G3_TR$Condition <- "W004_G3"

meanDataAreaW005_G3_TR<- data.frame(meanArea=round(mean(DataAreaW005_G3_TR$Area.mm2),digits =2 ))
meanDataAreaW005_G3_TR$SD<- round(sd(DataAreaW005_G3_TR$Area.mm2),digits =2 )
meanDataAreaW005_G3_TR$Wash <- "W005"
meanDataAreaW005_G3_TR$Condition <- "W005_G3"

meanDataAreaW006_G3_TR<- data.frame(meanArea=round(mean(DataAreaW006_G3_TR$Area.mm2),digits =2 ))
meanDataAreaW006_G3_TR$SD<- round(sd(DataAreaW006_G3_TR$Area.mm2),digits =2 )
meanDataAreaW006_G3_TR$Wash <- "W006"
meanDataAreaW006_G3_TR$Condition <- "W006_G3"

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

FibreCount_TransferArea_G3 <- rbind(meanDataAreaW000_G3_TR, meanDataAreaW001_G3_TR, meanDataAreaW002_G3_TR, meanDataAreaW003_G3_TR, meanDataAreaW004_G3_TR, meanDataAreaW005_G3_TR, meanDataAreaW006_G3_TR, meanDataAreaW007_G3_TR,meanDataAreaW009_G3_TR, meanDataAreaW010_G3_TR, meanDataAreaW013_G3_TR,meanDataAreaW015_G3_TR)

#write.table(FibreCount_TransferArea_G3, file = "Transfer_Area.csv", quote = F, sep = ",", row.names = F)

# plot
pSH <- ggplot(FibreCount_TransferArea_G3, aes(x = factor(Wash, level = c('W000', 'W001', 'W002','W003','W004','W005','W006','W007','W009','W010')),
                                              y= meanArea, fill=Condition))+
  geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label = meanArea), hjust=0.5,vjust = -5.5,position = position_dodge(.9))+
  labs(x="\nWash", y="Total fibre area (mm\u00b2)\n") +
  theme_bw(base_family = "Arial", base_size = 12) +
  ylim(0,5)+
  scale_fill_manual("legend", values = c("W000_G3" = "#91d4e3", "W001_G3" = "#4daabf","W002_G3" = "#148199","W003_G3" = "#6daae3","W004_G3" = "#4487c7",
                                         "W005_G3" = "#145796","W006_G3" = "#7b88d4","W007_G3" = "#4457c2", "W009_G3" = "#182da1", "W010_G3" = "#a28ae6", "W013_G3" = "#7357c2", "W015_G3" = "#401ca6"))+ # to obtain the colour brewer.pal(9, "YlOrRd")
  theme(legend.title = element_blank(),
        strip.background.x = element_rect(colour = "grey", fill = "white"),
        legend.position = "bottom",
        legend.background = element_rect(fill="grey95",size=1, linetype="solid", colour="grey80"),
        axis.text.x = element_text(angle = 0, vjust = 0.95, hjust=0.5))+
  geom_errorbar(aes(ymin=meanArea-SD, ymax=meanArea+SD),width=.2,position=position_dodge(.9))
pSH
ggsave("TransferArea_G3_W000-15.png", pSH, width = 10, height = 9, units = "in", dpi=150, path = "Results")
