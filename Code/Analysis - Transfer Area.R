#########################################################
#####         TRANSFER - TOTAL FIBRE AREA           #####
#########################################################
##################
#####   G1   #####
##################
TransferArea_G1 <- read.csv('./Fibre count Summary/TR_G1_W000-W007_Summary.csv', sep="," ,header = T,fileEncoding="UTF-8-BOM")
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

FibreCount_TransferArea_G1 <- rbind(meanDataAreaW000_G1_TR, meanDataAreaW001_G1_TR, meanDataAreaW002_G1_TR, meanDataAreaW003_G1_TR, meanDataAreaW004_G1_TR, meanDataAreaW005_G1_TR, meanDataAreaW006_G1_TR, meanDataAreaW007_G1_TR)

#write.table(FibreCount_TransferArea_G1, file = "Transfer_Area.csv", quote = F, sep = ",", row.names = F)

# plot
pSH <- ggplot(FibreCount_TransferArea_G1, aes(x = factor(Wash, level = c('W000', 'W001', 'W002','W003','W004','W005','W006','W007')),
                                          y= meanArea, fill=Condition))+
  geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label = meanArea), hjust=0.5,vjust = -5.5,position = position_dodge(.9))+
  labs(x="\nWash", y="Total fibre area (mm\u00b2)\n") +
  theme_bw(base_family = "Arial", base_size = 12) +
  ylim(0,5)+
  scale_fill_manual("legend", values = c("W000_G1" = "#FED976", "W001_G1" = "#FEB24C","W002_G1" = "#FD8D3C","W003_G1" = "#FC4E2A","W004_G1" = "#E31A1C",
                                         "W005_G1" = "#e31a7f","W006_G1" = "#e31acf","W007_G1" = "#be1ae3"))+ # to obtain the colour brewer.pal(9, "YlOrRd")
  theme(legend.title = element_blank(),
        strip.background.x = element_rect(colour = "grey", fill = "white"),
        legend.position = "bottom",
        legend.background = element_rect(fill="grey95",size=1, linetype="solid", colour="grey80"),
        axis.text.x = element_text(angle = 0, vjust = 0.95, hjust=0.5))+
  geom_errorbar(aes(ymin=meanArea-SD, ymax=meanArea+SD),width=.2,position=position_dodge(.9))
pSH
ggsave("TransferArea_G1_W000-7.png", pSH, width = 10, height = 9, units = "in", dpi=150, path = "Results")
