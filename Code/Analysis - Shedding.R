#########################################################
#####                  SHEDDING TEST                #####
#########################################################
##################
#####   G1   #####
##################
Shedding_G1 <- read.csv('./Fibre count Summary/SH_G1_W000-W015_Summary.csv', sep="," ,header = T,fileEncoding="UTF-8-BOM")
Shedding_G1$Slice<- gsub(".TIF","",Shedding_G1$Slice)
Shedding_G1Extended <- data.frame(str_split(Shedding_G1$Slice, "_", simplify=TRUE))
names(Shedding_G1Extended) <- c("Project","Wash","Garment","Weight","Repeat","condition")
Shedding_G1Total <- cbind(Shedding_G1Extended,Area=Shedding_G1$Total.Area)

# Convert Area from inch2 to mm2
# 1 pixel = 1 x10^-5 inch2, so (Area*1)/0.000011 to convert into px
Shedding_G1Total$Area.px <- (Shedding_G1Total$Area*1)/0.000011
# 1 mm = 112 pixels, 1mm2 = 12544 px
Shedding_G1Total$Area.mm2 <- Shedding_G1Total$Area.px/12544

# Split data per washing condition
DataAreaW000_G1 <- Shedding_G1Total[Shedding_G1Total$Wash =='W000',]
DataAreaW001_G1 <- Shedding_G1Total[Shedding_G1Total$Wash =='W001',]
DataAreaW003_G1 <- Shedding_G1Total[Shedding_G1Total$Wash =='W003',]
DataAreaW005_G1 <- Shedding_G1Total[Shedding_G1Total$Wash =='W005',]
DataAreaW007_G1 <- Shedding_G1Total[Shedding_G1Total$Wash =='W007',]
DataAreaW009_G1 <- Shedding_G1Total[Shedding_G1Total$Wash =='W009',]
DataAreaW011_G1 <- Shedding_G1Total[Shedding_G1Total$Wash =='W011',]
DataAreaW013_G1 <- Shedding_G1Total[Shedding_G1Total$Wash =='W013',]
DataAreaW015_G1 <- Shedding_G1Total[Shedding_G1Total$Wash =='W015',]

# split per weight
DataAreaW000_G1_100 <- DataAreaW000_G1[DataAreaW000_G1$Weight =='100g',]
DataAreaW000_G1_200 <- DataAreaW000_G1[DataAreaW000_G1$Weight =='200g',]
DataAreaW000_G1_400 <- DataAreaW000_G1[DataAreaW000_G1$Weight =='400g',]
DataAreaW000_G1_800 <- DataAreaW000_G1[DataAreaW000_G1$Weight =='800g',]
DataAreaW000_G1_1000 <- DataAreaW000_G1[DataAreaW000_G1$Weight =='1000g',]
DataAreaW000_G1_2000 <- DataAreaW000_G1[DataAreaW000_G1$Weight =='2000g',]

DataAreaW001_G1_100 <- DataAreaW001_G1[DataAreaW001_G1$Weight =='100g',]
DataAreaW001_G1_200 <- DataAreaW001_G1[DataAreaW001_G1$Weight =='200g',]
DataAreaW001_G1_400 <- DataAreaW001_G1[DataAreaW001_G1$Weight =='400g',]
DataAreaW001_G1_800 <- DataAreaW001_G1[DataAreaW001_G1$Weight =='800g',]
DataAreaW001_G1_1000 <- DataAreaW001_G1[DataAreaW001_G1$Weight =='1000g',]
DataAreaW001_G1_2000 <- DataAreaW001_G1[DataAreaW001_G1$Weight =='2000g',]

DataAreaW003_G1_100 <- DataAreaW003_G1[DataAreaW003_G1$Weight =='100g',]
DataAreaW003_G1_200 <- DataAreaW003_G1[DataAreaW003_G1$Weight =='200g',]
DataAreaW003_G1_400 <- DataAreaW003_G1[DataAreaW003_G1$Weight =='400g',]
DataAreaW003_G1_800 <- DataAreaW003_G1[DataAreaW003_G1$Weight =='800g',]
DataAreaW003_G1_1000 <- DataAreaW003_G1[DataAreaW003_G1$Weight =='1000g',]
DataAreaW003_G1_2000 <- DataAreaW003_G1[DataAreaW003_G1$Weight =='2000g',]

DataAreaW005_G1_100 <- DataAreaW005_G1[DataAreaW005_G1$Weight =='100g',]
DataAreaW005_G1_200 <- DataAreaW005_G1[DataAreaW005_G1$Weight =='200g',]
DataAreaW005_G1_400 <- DataAreaW005_G1[DataAreaW005_G1$Weight =='400g',]
DataAreaW005_G1_800 <- DataAreaW005_G1[DataAreaW005_G1$Weight =='800g',]
DataAreaW005_G1_1000 <- DataAreaW005_G1[DataAreaW005_G1$Weight =='1000g',]
DataAreaW005_G1_2000 <- DataAreaW005_G1[DataAreaW005_G1$Weight =='2000g',]

DataAreaW007_G1_100 <- DataAreaW007_G1[DataAreaW007_G1$Weight =='100g',]
DataAreaW007_G1_200 <- DataAreaW007_G1[DataAreaW007_G1$Weight =='200g',]
DataAreaW007_G1_400 <- DataAreaW007_G1[DataAreaW007_G1$Weight =='400g',]
DataAreaW007_G1_800 <- DataAreaW007_G1[DataAreaW007_G1$Weight =='800g',]
DataAreaW007_G1_1000 <- DataAreaW007_G1[DataAreaW007_G1$Weight =='1000g',]
DataAreaW007_G1_2000 <- DataAreaW007_G1[DataAreaW007_G1$Weight =='2000g',]

DataAreaW009_G1_100 <- DataAreaW009_G1[DataAreaW009_G1$Weight =='100g',]
DataAreaW009_G1_200 <- DataAreaW009_G1[DataAreaW009_G1$Weight =='200g',]
DataAreaW009_G1_400 <- DataAreaW009_G1[DataAreaW009_G1$Weight =='400g',]
DataAreaW009_G1_800 <- DataAreaW009_G1[DataAreaW009_G1$Weight =='800g',]
DataAreaW009_G1_1000 <- DataAreaW009_G1[DataAreaW009_G1$Weight =='1000g',]
DataAreaW009_G1_2000 <- DataAreaW009_G1[DataAreaW009_G1$Weight =='2000g',]

DataAreaW011_G1_100 <- DataAreaW011_G1[DataAreaW011_G1$Weight =='100g',]
DataAreaW011_G1_200 <- DataAreaW011_G1[DataAreaW011_G1$Weight =='200g',]
DataAreaW011_G1_400 <- DataAreaW011_G1[DataAreaW011_G1$Weight =='400g',]
DataAreaW011_G1_800 <- DataAreaW011_G1[DataAreaW011_G1$Weight =='800g',]
DataAreaW011_G1_1000 <- DataAreaW011_G1[DataAreaW011_G1$Weight =='1000g',]
DataAreaW011_G1_2000 <- DataAreaW011_G1[DataAreaW011_G1$Weight =='2000g',]

DataAreaW013_G1_100 <- DataAreaW013_G1[DataAreaW013_G1$Weight =='100g',]
DataAreaW013_G1_200 <- DataAreaW013_G1[DataAreaW013_G1$Weight =='200g',]
DataAreaW013_G1_400 <- DataAreaW013_G1[DataAreaW013_G1$Weight =='400g',]
DataAreaW013_G1_800 <- DataAreaW013_G1[DataAreaW013_G1$Weight =='800g',]
DataAreaW013_G1_1000 <- DataAreaW013_G1[DataAreaW013_G1$Weight =='1000g',]
DataAreaW013_G1_2000 <- DataAreaW013_G1[DataAreaW013_G1$Weight =='2000g',]

DataAreaW015_G1_100 <- DataAreaW015_G1[DataAreaW015_G1$Weight =='100g',]
DataAreaW015_G1_200 <- DataAreaW015_G1[DataAreaW015_G1$Weight =='200g',]
DataAreaW015_G1_400 <- DataAreaW015_G1[DataAreaW015_G1$Weight =='400g',]
DataAreaW015_G1_800 <- DataAreaW015_G1[DataAreaW015_G1$Weight =='800g',]
DataAreaW015_G1_1000 <- DataAreaW015_G1[DataAreaW015_G1$Weight =='1000g',]
DataAreaW015_G1_2000 <- DataAreaW015_G1[DataAreaW015_G1$Weight =='2000g',]

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

meanDataAreaW009_G1_100<- data.frame(meanArea=round(mean(DataAreaW009_G1_100$Area.mm2),digits =2 ))
meanDataAreaW009_G1_100$SD<- round(sd(DataAreaW009_G1_100$Area.mm2),digits =2 )
meanDataAreaW009_G1_100$Weight <- "100g"
meanDataAreaW009_G1_200<- data.frame(meanArea=round(mean(DataAreaW009_G1_200$Area.mm2),digits =2 ))
meanDataAreaW009_G1_200$SD<- round(sd(DataAreaW009_G1_200$Area.mm2),digits =2 )
meanDataAreaW009_G1_200$Weight <- "200g"
meanDataAreaW009_G1_400<- data.frame(meanArea=round(mean(DataAreaW009_G1_400$Area.mm2),digits =2 ))
meanDataAreaW009_G1_400$SD<- round(sd(DataAreaW009_G1_400$Area.mm2),digits =2 )
meanDataAreaW009_G1_400$Weight <- "400g"
meanDataAreaW009_G1_800<- data.frame(meanArea=round(mean(DataAreaW009_G1_800$Area.mm2),digits =2 ))
meanDataAreaW009_G1_800$SD<- round(sd(DataAreaW009_G1_800$Area.mm2),digits =2 )
meanDataAreaW009_G1_800$Weight <- "800g"
meanDataAreaW009_G1_1000<- data.frame(meanArea=round(mean(DataAreaW009_G1_1000$Area.mm2),digits =2 ))
meanDataAreaW009_G1_1000$SD<- round(sd(DataAreaW009_G1_1000$Area.mm2),digits =2 )
meanDataAreaW009_G1_1000$Weight <- "1000g"
meanDataAreaW009_G1_2000<- data.frame(meanArea=round(mean(DataAreaW009_G1_2000$Area.mm2),digits =2 ))
meanDataAreaW009_G1_2000$SD<- round(sd(DataAreaW009_G1_2000$Area.mm2),digits =2 )
meanDataAreaW009_G1_2000$Weight <- "2000g"

meanDataAreaW011_G1_100<- data.frame(meanArea=round(mean(DataAreaW011_G1_100$Area.mm2),digits =2 ))
meanDataAreaW011_G1_100$SD<- round(sd(DataAreaW011_G1_100$Area.mm2),digits =2 )
meanDataAreaW011_G1_100$Weight <- "100g"
meanDataAreaW011_G1_200<- data.frame(meanArea=round(mean(DataAreaW011_G1_200$Area.mm2),digits =2 ))
meanDataAreaW011_G1_200$SD<- round(sd(DataAreaW011_G1_200$Area.mm2),digits =2 )
meanDataAreaW011_G1_200$Weight <- "200g"
meanDataAreaW011_G1_400<- data.frame(meanArea=round(mean(DataAreaW011_G1_400$Area.mm2),digits =2 ))
meanDataAreaW011_G1_400$SD<- round(sd(DataAreaW011_G1_400$Area.mm2),digits =2 )
meanDataAreaW011_G1_400$Weight <- "400g"
meanDataAreaW011_G1_800<- data.frame(meanArea=round(mean(DataAreaW011_G1_800$Area.mm2),digits =2 ))
meanDataAreaW011_G1_800$SD<- round(sd(DataAreaW011_G1_800$Area.mm2),digits =2 )
meanDataAreaW011_G1_800$Weight <- "800g"
meanDataAreaW011_G1_1000<- data.frame(meanArea=round(mean(DataAreaW011_G1_1000$Area.mm2),digits =2 ))
meanDataAreaW011_G1_1000$SD<- round(sd(DataAreaW011_G1_1000$Area.mm2),digits =2 )
meanDataAreaW011_G1_1000$Weight <- "1000g"
meanDataAreaW011_G1_2000<- data.frame(meanArea=round(mean(DataAreaW011_G1_2000$Area.mm2),digits =2 ))
meanDataAreaW011_G1_2000$SD<- round(sd(DataAreaW011_G1_2000$Area.mm2),digits =2 )
meanDataAreaW011_G1_2000$Weight <- "2000g"

meanDataAreaW013_G1_100<- data.frame(meanArea=round(mean(DataAreaW013_G1_100$Area.mm2),digits =2 ))
meanDataAreaW013_G1_100$SD<- round(sd(DataAreaW013_G1_100$Area.mm2),digits =2 )
meanDataAreaW013_G1_100$Weight <- "100g"
meanDataAreaW013_G1_200<- data.frame(meanArea=round(mean(DataAreaW013_G1_200$Area.mm2),digits =2 ))
meanDataAreaW013_G1_200$SD<- round(sd(DataAreaW013_G1_200$Area.mm2),digits =2 )
meanDataAreaW013_G1_200$Weight <- "200g"
meanDataAreaW013_G1_400<- data.frame(meanArea=round(mean(DataAreaW013_G1_400$Area.mm2),digits =2 ))
meanDataAreaW013_G1_400$SD<- round(sd(DataAreaW013_G1_400$Area.mm2),digits =2 )
meanDataAreaW013_G1_400$Weight <- "400g"
meanDataAreaW013_G1_800<- data.frame(meanArea=round(mean(DataAreaW013_G1_800$Area.mm2),digits =2 ))
meanDataAreaW013_G1_800$SD<- round(sd(DataAreaW013_G1_800$Area.mm2),digits =2 )
meanDataAreaW013_G1_800$Weight <- "800g"
meanDataAreaW013_G1_1000<- data.frame(meanArea=round(mean(DataAreaW013_G1_1000$Area.mm2),digits =2 ))
meanDataAreaW013_G1_1000$SD<- round(sd(DataAreaW013_G1_1000$Area.mm2),digits =2 )
meanDataAreaW013_G1_1000$Weight <- "1000g"
meanDataAreaW013_G1_2000<- data.frame(meanArea=round(mean(DataAreaW013_G1_2000$Area.mm2),digits =2 ))
meanDataAreaW013_G1_2000$SD<- round(sd(DataAreaW013_G1_2000$Area.mm2),digits =2 )
meanDataAreaW013_G1_2000$Weight <- "2000g"

meanDataAreaW015_G1_100<- data.frame(meanArea=round(mean(DataAreaW015_G1_100$Area.mm2),digits =2 ))
meanDataAreaW015_G1_100$SD<- round(sd(DataAreaW015_G1_100$Area.mm2),digits =2 )
meanDataAreaW015_G1_100$Weight <- "100g"
meanDataAreaW015_G1_200<- data.frame(meanArea=round(mean(DataAreaW015_G1_200$Area.mm2),digits =2 ))
meanDataAreaW015_G1_200$SD<- round(sd(DataAreaW015_G1_200$Area.mm2),digits =2 )
meanDataAreaW015_G1_200$Weight <- "200g"
meanDataAreaW015_G1_400<- data.frame(meanArea=round(mean(DataAreaW015_G1_400$Area.mm2),digits =2 ))
meanDataAreaW015_G1_400$SD<- round(sd(DataAreaW015_G1_400$Area.mm2),digits =2 )
meanDataAreaW015_G1_400$Weight <- "400g"
meanDataAreaW015_G1_800<- data.frame(meanArea=round(mean(DataAreaW015_G1_800$Area.mm2),digits =2 ))
meanDataAreaW015_G1_800$SD<- round(sd(DataAreaW015_G1_800$Area.mm2),digits =2 )
meanDataAreaW015_G1_800$Weight <- "800g"
meanDataAreaW015_G1_1000<- data.frame(meanArea=round(mean(DataAreaW015_G1_1000$Area.mm2),digits =2 ))
meanDataAreaW015_G1_1000$SD<- round(sd(DataAreaW015_G1_1000$Area.mm2),digits =2 )
meanDataAreaW015_G1_1000$Weight <- "1000g"
meanDataAreaW015_G1_2000<- data.frame(meanArea=round(mean(DataAreaW015_G1_2000$Area.mm2),digits =2 ))
meanDataAreaW015_G1_2000$SD<- round(sd(DataAreaW015_G1_2000$Area.mm2),digits =2 )
meanDataAreaW015_G1_2000$Weight <- "2000g"

# Combined data sets
DataW000_G1_total <- rbind(meanDataAreaW000_G1_100,meanDataAreaW000_G1_200,meanDataAreaW000_G1_400,
                           meanDataAreaW000_G1_800,meanDataAreaW000_G1_1000,meanDataAreaW000_G1_2000)
DataW000_G1_total$Condition <- "W000_G1"

DataW001_G1_total <- rbind(meanDataAreaW001_G1_100,meanDataAreaW001_G1_200,meanDataAreaW001_G1_400,
                           meanDataAreaW001_G1_800,meanDataAreaW001_G1_1000,meanDataAreaW001_G1_2000)
DataW001_G1_total$Condition <- "W001_G1"

DataW003_G1_total <- rbind(meanDataAreaW003_G1_100,meanDataAreaW003_G1_200,meanDataAreaW003_G1_400,
                           meanDataAreaW003_G1_800,meanDataAreaW003_G1_1000,meanDataAreaW003_G1_2000)
DataW003_G1_total$Condition <- "W003_G1"

DataW005_G1_total <- rbind(meanDataAreaW005_G1_100,meanDataAreaW005_G1_200,meanDataAreaW005_G1_400,
                           meanDataAreaW005_G1_800,meanDataAreaW005_G1_1000,meanDataAreaW005_G1_2000)
DataW005_G1_total$Condition <- "W005_G1"

DataW007_G1_total <- rbind(meanDataAreaW007_G1_100,meanDataAreaW007_G1_200,meanDataAreaW007_G1_400,
                           meanDataAreaW007_G1_800,meanDataAreaW007_G1_1000,meanDataAreaW007_G1_2000)
DataW007_G1_total$Condition <- "W007_G1"

DataW009_G1_total <- rbind(meanDataAreaW009_G1_100,meanDataAreaW009_G1_200,meanDataAreaW009_G1_400,
                           meanDataAreaW009_G1_800,meanDataAreaW009_G1_1000,meanDataAreaW009_G1_2000)
DataW009_G1_total$Condition <- "W009_G1"

DataW011_G1_total <- rbind(meanDataAreaW011_G1_100,meanDataAreaW011_G1_200,meanDataAreaW011_G1_400,
                           meanDataAreaW011_G1_800,meanDataAreaW011_G1_1000,meanDataAreaW011_G1_2000)
DataW011_G1_total$Condition <- "W011_G1"

DataW013_G1_total <- rbind(meanDataAreaW013_G1_100,meanDataAreaW013_G1_200,meanDataAreaW013_G1_400,
                           meanDataAreaW013_G1_800,meanDataAreaW013_G1_1000,meanDataAreaW013_G1_2000)
DataW013_G1_total$Condition <- "W013_G1"

DataW015_G1_total <- rbind(meanDataAreaW015_G1_100,meanDataAreaW015_G1_200,meanDataAreaW015_G1_400,
                           meanDataAreaW015_G1_800,meanDataAreaW015_G1_1000,meanDataAreaW015_G1_2000)
DataW015_G1_total$Condition <- "W015_G1"

FibreCount_Shedding_G1 <- rbind(DataW000_G1_total,DataW001_G1_total,DataW003_G1_total,DataW005_G1_total,DataW007_G1_total,DataW009_G1_total,DataW011_G1_total,DataW013_G1_total,DataW015_G1_total)

write.table(FibreCount_Shedding_G1, file = "Shedding_Fibre_Count.csv", quote = F, sep = ",", row.names = F)

#### PLOT ####
# calculation of the percentage difference between washed and unwashed
y = rep(c(125, 150, 175, 200, 225),2)
x = rep(c(1:5), 2)

# plot
pSH_G1 <- ggplot(FibreCount_Shedding_G1, aes(x = factor(Weight, level = c('100g', '200g', '400g','800g','1000g','2000g')),
                                          y= meanArea, fill=Condition))+
  geom_bar(stat="identity", position=position_dodge(),colour="black")+
  geom_text(aes(label = meanArea), hjust=0.5,vjust = -5.5,position = position_dodge(.9))+
  labs(x="\nWeight", y="Total fibre area (mm\u00b2)\n") +
  theme_bw(base_family = "Arial", base_size = 12) +
  ylim(0,490)+
  scale_fill_manual(values = brewer.pal(9, "Greys")[1:9])+
  theme(legend.title = element_blank(),
        strip.background.x = element_rect(colour = "Greys", fill = "white"),
        legend.position = "bottom",
        legend.background = element_rect(fill="grey95",size=1, linetype="solid", colour="grey80"),
        axis.text.x = element_text(angle = 0, vjust = 0.95, hjust=0.5))+
  geom_errorbar(aes(ymin=meanArea-SD, ymax=meanArea+SD),width=.2,position=position_dodge(.9))
pSH_G1
ggsave("Shedding_G1_W000-15.png", pSH_G1, width = 10, height = 9, units = "in", dpi=150, path = "Results")

# plot - no label
pSH_G1 <- ggplot(FibreCount_Shedding_G1, aes(x = factor(Weight, level = c('100g', '200g', '400g','800g','1000g','2000g')),
                                             y= meanArea, fill=Condition))+
  geom_bar(stat="identity", position=position_dodge(),colour="black")+
  labs(x="\nWeight", y="Total fibre area (mm\u00b2)\n") +
  theme_bw(base_family = "Arial", base_size = 12) +
  ylim(0,480)+
  scale_fill_manual(values = brewer.pal(9, "Greys")[1:9])+
  theme(legend.title = element_blank(),
        strip.background.x = element_rect(colour = "Greys", fill = "white"),
        legend.position = "bottom",
        legend.background = element_rect(fill="grey95",size=1, linetype="solid", colour="grey80"),
        axis.text.x = element_text(angle = 0, vjust = 0.95, hjust=0.5))+
  geom_errorbar(aes(ymin=meanArea-SD, ymax=meanArea+SD),width=.2,position=position_dodge(.9))
pSH_G1
ggsave("Shedding_G1_W000-15_No Mean Label.png", pSH_G1, width = 10, height = 9, units = "in", dpi=150, path = "Results")

##################
#####   G2   #####
##################
Shedding_G2 <- read.csv('./Fibre count Summary/SH_G2_W000-W015_Summary.csv', sep="," ,header = T,fileEncoding="UTF-8-BOM")
Shedding_G2$Slice<- gsub(".TIF","",Shedding_G2$Slice)
Shedding_G2Extended <- data.frame(str_split(Shedding_G2$Slice, "_", simplify=TRUE))
names(Shedding_G2Extended) <- c("Project","Wash","Garment","Weight","Repeat","condition")
Shedding_G2Total <- cbind(Shedding_G2Extended,Area=Shedding_G2$Total.Area)

# Convert Area from inch2 to mm2
# 1 pixel = 1 x10^-5 inch2, so (Area*1)/0.000011 to convert into px
Shedding_G2Total$Area.px <- (Shedding_G2Total$Area*1)/0.000011
# 1 mm = 112 pixels, 1mm2 = 12544 px
Shedding_G2Total$Area.mm2 <- Shedding_G2Total$Area.px/12544

# Split data per washing condition
DataAreaW000_G2 <- Shedding_G2Total[Shedding_G2Total$Wash =='W000',]
DataAreaW001_G2 <- Shedding_G2Total[Shedding_G2Total$Wash =='W001',]
DataAreaW003_G2 <- Shedding_G2Total[Shedding_G2Total$Wash =='W003',]
DataAreaW005_G2 <- Shedding_G2Total[Shedding_G2Total$Wash =='W005',]
DataAreaW007_G2 <- Shedding_G2Total[Shedding_G2Total$Wash =='W007',]
DataAreaW009_G2 <- Shedding_G2Total[Shedding_G2Total$Wash =='W009',]
DataAreaW011_G2 <- Shedding_G2Total[Shedding_G2Total$Wash =='W011',]
DataAreaW013_G2 <- Shedding_G2Total[Shedding_G2Total$Wash =='W013',]
DataAreaW015_G2 <- Shedding_G2Total[Shedding_G2Total$Wash =='W015',]

# split per weight
DataAreaW000_G2_100 <- DataAreaW000_G2[DataAreaW000_G2$Weight =='100g',]
DataAreaW000_G2_200 <- DataAreaW000_G2[DataAreaW000_G2$Weight =='200g',]
DataAreaW000_G2_400 <- DataAreaW000_G2[DataAreaW000_G2$Weight =='400g',]
DataAreaW000_G2_800 <- DataAreaW000_G2[DataAreaW000_G2$Weight =='800g',]
DataAreaW000_G2_1000 <- DataAreaW000_G2[DataAreaW000_G2$Weight =='1000g',]
DataAreaW000_G2_2000 <- DataAreaW000_G2[DataAreaW000_G2$Weight =='2000g',]

DataAreaW001_G2_100 <- DataAreaW001_G2[DataAreaW001_G2$Weight =='100g',]
DataAreaW001_G2_200 <- DataAreaW001_G2[DataAreaW001_G2$Weight =='200g',]
DataAreaW001_G2_400 <- DataAreaW001_G2[DataAreaW001_G2$Weight =='400g',]
DataAreaW001_G2_800 <- DataAreaW001_G2[DataAreaW001_G2$Weight =='800g',]
DataAreaW001_G2_1000 <- DataAreaW001_G2[DataAreaW001_G2$Weight =='1000g',]
DataAreaW001_G2_2000 <- DataAreaW001_G2[DataAreaW001_G2$Weight =='2000g',]

DataAreaW003_G2_100 <- DataAreaW003_G2[DataAreaW003_G2$Weight =='100g',]
DataAreaW003_G2_200 <- DataAreaW003_G2[DataAreaW003_G2$Weight =='200g',]
DataAreaW003_G2_400 <- DataAreaW003_G2[DataAreaW003_G2$Weight =='400g',]
DataAreaW003_G2_800 <- DataAreaW003_G2[DataAreaW003_G2$Weight =='800g',]
DataAreaW003_G2_1000 <- DataAreaW003_G2[DataAreaW003_G2$Weight =='1000g',]
DataAreaW003_G2_2000 <- DataAreaW003_G2[DataAreaW003_G2$Weight =='2000g',]

DataAreaW005_G2_100 <- DataAreaW005_G2[DataAreaW005_G2$Weight =='100g',]
DataAreaW005_G2_200 <- DataAreaW005_G2[DataAreaW005_G2$Weight =='200g',]
DataAreaW005_G2_400 <- DataAreaW005_G2[DataAreaW005_G2$Weight =='400g',]
DataAreaW005_G2_800 <- DataAreaW005_G2[DataAreaW005_G2$Weight =='800g',]
DataAreaW005_G2_1000 <- DataAreaW005_G2[DataAreaW005_G2$Weight =='1000g',]
DataAreaW005_G2_2000 <- DataAreaW005_G2[DataAreaW005_G2$Weight =='2000g',]

DataAreaW007_G2_100 <- DataAreaW007_G2[DataAreaW007_G2$Weight =='100g',]
DataAreaW007_G2_200 <- DataAreaW007_G2[DataAreaW007_G2$Weight =='200g',]
DataAreaW007_G2_400 <- DataAreaW007_G2[DataAreaW007_G2$Weight =='400g',]
DataAreaW007_G2_800 <- DataAreaW007_G2[DataAreaW007_G2$Weight =='800g',]
DataAreaW007_G2_1000 <- DataAreaW007_G2[DataAreaW007_G2$Weight =='1000g',]
DataAreaW007_G2_2000 <- DataAreaW007_G2[DataAreaW007_G2$Weight =='2000g',]

DataAreaW009_G2_100 <- DataAreaW009_G2[DataAreaW009_G2$Weight =='100g',]
DataAreaW009_G2_200 <- DataAreaW009_G2[DataAreaW009_G2$Weight =='200g',]
DataAreaW009_G2_400 <- DataAreaW009_G2[DataAreaW009_G2$Weight =='400g',]
DataAreaW009_G2_800 <- DataAreaW009_G2[DataAreaW009_G2$Weight =='800g',]
DataAreaW009_G2_1000 <- DataAreaW009_G2[DataAreaW009_G2$Weight =='1000g',]
DataAreaW009_G2_2000 <- DataAreaW009_G2[DataAreaW009_G2$Weight =='2000g',]

DataAreaW011_G2_100 <- DataAreaW011_G2[DataAreaW011_G2$Weight =='100g',]
DataAreaW011_G2_200 <- DataAreaW011_G2[DataAreaW011_G2$Weight =='200g',]
DataAreaW011_G2_400 <- DataAreaW011_G2[DataAreaW011_G2$Weight =='400g',]
DataAreaW011_G2_800 <- DataAreaW011_G2[DataAreaW011_G2$Weight =='800g',]
DataAreaW011_G2_1000 <- DataAreaW011_G2[DataAreaW011_G2$Weight =='1000g',]
DataAreaW011_G2_2000 <- DataAreaW011_G2[DataAreaW011_G2$Weight =='2000g',]

DataAreaW013_G2_100 <- DataAreaW013_G2[DataAreaW013_G2$Weight =='100g',]
DataAreaW013_G2_200 <- DataAreaW013_G2[DataAreaW013_G2$Weight =='200g',]
DataAreaW013_G2_400 <- DataAreaW013_G2[DataAreaW013_G2$Weight =='400g',]
DataAreaW013_G2_800 <- DataAreaW013_G2[DataAreaW013_G2$Weight =='800g',]
DataAreaW013_G2_1000 <- DataAreaW013_G2[DataAreaW013_G2$Weight =='1000g',]
DataAreaW013_G2_2000 <- DataAreaW013_G2[DataAreaW013_G2$Weight =='2000g',]

DataAreaW015_G2_100 <- DataAreaW015_G2[DataAreaW015_G2$Weight =='100g',]
DataAreaW015_G2_200 <- DataAreaW015_G2[DataAreaW015_G2$Weight =='200g',]
DataAreaW015_G2_400 <- DataAreaW015_G2[DataAreaW015_G2$Weight =='400g',]
DataAreaW015_G2_800 <- DataAreaW015_G2[DataAreaW015_G2$Weight =='800g',]
DataAreaW015_G2_1000 <- DataAreaW015_G2[DataAreaW015_G2$Weight =='1000g',]
DataAreaW015_G2_2000 <- DataAreaW015_G2[DataAreaW015_G2$Weight =='2000g',]

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
meanDataAreaW005_G2_100<- data.frame(meanArea=round(mean(DataAreaW005_G2_100$Area.mm2),digits =2 ))
meanDataAreaW005_G2_100$SD<- round(sd(DataAreaW005_G2_100$Area.mm2),digits =2 )
meanDataAreaW005_G2_100$Weight <- "100g"
meanDataAreaW005_G2_200<- data.frame(meanArea=round(mean(DataAreaW005_G2_200$Area.mm2),digits =2 ))
meanDataAreaW005_G2_200$SD<- round(sd(DataAreaW005_G2_200$Area.mm2),digits =2 )
meanDataAreaW005_G2_200$Weight <- "200g"
meanDataAreaW005_G2_400<- data.frame(meanArea=round(mean(DataAreaW005_G2_400$Area.mm2),digits =2 ))
meanDataAreaW005_G2_400$SD<- round(sd(DataAreaW005_G2_400$Area.mm2),digits =2 )
meanDataAreaW005_G2_400$Weight <- "400g"
meanDataAreaW005_G2_800<- data.frame(meanArea=round(mean(DataAreaW005_G2_800$Area.mm2),digits =2 ))
meanDataAreaW005_G2_800$SD<- round(sd(DataAreaW005_G2_800$Area.mm2),digits =2 )
meanDataAreaW005_G2_800$Weight <- "800g"
meanDataAreaW005_G2_1000<- data.frame(meanArea=round(mean(DataAreaW005_G2_1000$Area.mm2),digits =2 ))
meanDataAreaW005_G2_1000$SD<- round(sd(DataAreaW005_G2_1000$Area.mm2),digits =2 )
meanDataAreaW005_G2_1000$Weight <- "1000g"
meanDataAreaW005_G2_2000<- data.frame(meanArea=round(mean(DataAreaW005_G2_2000$Area.mm2),digits =2 ))
meanDataAreaW005_G2_2000$SD<- round(sd(DataAreaW005_G2_2000$Area.mm2),digits =2 )
meanDataAreaW005_G2_2000$Weight <- "2000g"

meanDataAreaW007_G2_100<- data.frame(meanArea=round(mean(DataAreaW007_G2_100$Area.mm2),digits =2 ))
meanDataAreaW007_G2_100$SD<- round(sd(DataAreaW007_G2_100$Area.mm2),digits =2 )
meanDataAreaW007_G2_100$Weight <- "100g"
meanDataAreaW007_G2_200<- data.frame(meanArea=round(mean(DataAreaW007_G2_200$Area.mm2),digits =2 ))
meanDataAreaW007_G2_200$SD<- round(sd(DataAreaW007_G2_200$Area.mm2),digits =2 )
meanDataAreaW007_G2_200$Weight <- "200g"
meanDataAreaW007_G2_400<- data.frame(meanArea=round(mean(DataAreaW007_G2_400$Area.mm2),digits =2 ))
meanDataAreaW007_G2_400$SD<- round(sd(DataAreaW007_G2_400$Area.mm2),digits =2 )
meanDataAreaW007_G2_400$Weight <- "400g"
meanDataAreaW007_G2_800<- data.frame(meanArea=round(mean(DataAreaW007_G2_800$Area.mm2),digits =2 ))
meanDataAreaW007_G2_800$SD<- round(sd(DataAreaW007_G2_800$Area.mm2),digits =2 )
meanDataAreaW007_G2_800$Weight <- "800g"
meanDataAreaW007_G2_1000<- data.frame(meanArea=round(mean(DataAreaW007_G2_1000$Area.mm2),digits =2 ))
meanDataAreaW007_G2_1000$SD<- round(sd(DataAreaW007_G2_1000$Area.mm2),digits =2 )
meanDataAreaW007_G2_1000$Weight <- "1000g"
meanDataAreaW007_G2_2000<- data.frame(meanArea=round(mean(DataAreaW007_G2_2000$Area.mm2),digits =2 ))
meanDataAreaW007_G2_2000$SD<- round(sd(DataAreaW007_G2_2000$Area.mm2),digits =2 )
meanDataAreaW007_G2_2000$Weight <- "2000g"

meanDataAreaW009_G2_100<- data.frame(meanArea=round(mean(DataAreaW009_G2_100$Area.mm2),digits =2 ))
meanDataAreaW009_G2_100$SD<- round(sd(DataAreaW009_G2_100$Area.mm2),digits =2 )
meanDataAreaW009_G2_100$Weight <- "100g"
meanDataAreaW009_G2_200<- data.frame(meanArea=round(mean(DataAreaW009_G2_200$Area.mm2),digits =2 ))
meanDataAreaW009_G2_200$SD<- round(sd(DataAreaW009_G2_200$Area.mm2),digits =2 )
meanDataAreaW009_G2_200$Weight <- "200g"
meanDataAreaW009_G2_400<- data.frame(meanArea=round(mean(DataAreaW009_G2_400$Area.mm2),digits =2 ))
meanDataAreaW009_G2_400$SD<- round(sd(DataAreaW009_G2_400$Area.mm2),digits =2 )
meanDataAreaW009_G2_400$Weight <- "400g"
meanDataAreaW009_G2_800<- data.frame(meanArea=round(mean(DataAreaW009_G2_800$Area.mm2),digits =2 ))
meanDataAreaW009_G2_800$SD<- round(sd(DataAreaW009_G2_800$Area.mm2),digits =2 )
meanDataAreaW009_G2_800$Weight <- "800g"
meanDataAreaW009_G2_1000<- data.frame(meanArea=round(mean(DataAreaW009_G2_1000$Area.mm2),digits =2 ))
meanDataAreaW009_G2_1000$SD<- round(sd(DataAreaW009_G2_1000$Area.mm2),digits =2 )
meanDataAreaW009_G2_1000$Weight <- "1000g"
meanDataAreaW009_G2_2000<- data.frame(meanArea=round(mean(DataAreaW009_G2_2000$Area.mm2),digits =2 ))
meanDataAreaW009_G2_2000$SD<- round(sd(DataAreaW009_G2_2000$Area.mm2),digits =2 )
meanDataAreaW009_G2_2000$Weight <- "2000g"

meanDataAreaW011_G2_100<- data.frame(meanArea=round(mean(DataAreaW011_G2_100$Area.mm2),digits =2 ))
meanDataAreaW011_G2_100$SD<- round(sd(DataAreaW011_G2_100$Area.mm2),digits =2 )
meanDataAreaW011_G2_100$Weight <- "100g"
meanDataAreaW011_G2_200<- data.frame(meanArea=round(mean(DataAreaW011_G2_200$Area.mm2),digits =2 ))
meanDataAreaW011_G2_200$SD<- round(sd(DataAreaW011_G2_200$Area.mm2),digits =2 )
meanDataAreaW011_G2_200$Weight <- "200g"
meanDataAreaW011_G2_400<- data.frame(meanArea=round(mean(DataAreaW011_G2_400$Area.mm2),digits =2 ))
meanDataAreaW011_G2_400$SD<- round(sd(DataAreaW011_G2_400$Area.mm2),digits =2 )
meanDataAreaW011_G2_400$Weight <- "400g"
meanDataAreaW011_G2_800<- data.frame(meanArea=round(mean(DataAreaW011_G2_800$Area.mm2),digits =2 ))
meanDataAreaW011_G2_800$SD<- round(sd(DataAreaW011_G2_800$Area.mm2),digits =2 )
meanDataAreaW011_G2_800$Weight <- "800g"
meanDataAreaW011_G2_1000<- data.frame(meanArea=round(mean(DataAreaW011_G2_1000$Area.mm2),digits =2 ))
meanDataAreaW011_G2_1000$SD<- round(sd(DataAreaW011_G2_1000$Area.mm2),digits =2 )
meanDataAreaW011_G2_1000$Weight <- "1000g"
meanDataAreaW011_G2_2000<- data.frame(meanArea=round(mean(DataAreaW011_G2_2000$Area.mm2),digits =2 ))
meanDataAreaW011_G2_2000$SD<- round(sd(DataAreaW011_G2_2000$Area.mm2),digits =2 )
meanDataAreaW011_G2_2000$Weight <- "2000g"

meanDataAreaW013_G2_100<- data.frame(meanArea=round(mean(DataAreaW013_G2_100$Area.mm2),digits =2 ))
meanDataAreaW013_G2_100$SD<- round(sd(DataAreaW013_G2_100$Area.mm2),digits =2 )
meanDataAreaW013_G2_100$Weight <- "100g"
meanDataAreaW013_G2_200<- data.frame(meanArea=round(mean(DataAreaW013_G2_200$Area.mm2),digits =2 ))
meanDataAreaW013_G2_200$SD<- round(sd(DataAreaW013_G2_200$Area.mm2),digits =2 )
meanDataAreaW013_G2_200$Weight <- "200g"
meanDataAreaW013_G2_400<- data.frame(meanArea=round(mean(DataAreaW013_G2_400$Area.mm2),digits =2 ))
meanDataAreaW013_G2_400$SD<- round(sd(DataAreaW013_G2_400$Area.mm2),digits =2 )
meanDataAreaW013_G2_400$Weight <- "400g"
meanDataAreaW013_G2_800<- data.frame(meanArea=round(mean(DataAreaW013_G2_800$Area.mm2),digits =2 ))
meanDataAreaW013_G2_800$SD<- round(sd(DataAreaW013_G2_800$Area.mm2),digits =2 )
meanDataAreaW013_G2_800$Weight <- "800g"
meanDataAreaW013_G2_1000<- data.frame(meanArea=round(mean(DataAreaW013_G2_1000$Area.mm2),digits =2 ))
meanDataAreaW013_G2_1000$SD<- round(sd(DataAreaW013_G2_1000$Area.mm2),digits =2 )
meanDataAreaW013_G2_1000$Weight <- "1000g"
meanDataAreaW013_G2_2000<- data.frame(meanArea=round(mean(DataAreaW013_G2_2000$Area.mm2),digits =2 ))
meanDataAreaW013_G2_2000$SD<- round(sd(DataAreaW013_G2_2000$Area.mm2),digits =2 )
meanDataAreaW013_G2_2000$Weight <- "2000g"

meanDataAreaW015_G2_100<- data.frame(meanArea=round(mean(DataAreaW015_G2_100$Area.mm2),digits =2 ))
meanDataAreaW015_G2_100$SD<- round(sd(DataAreaW015_G2_100$Area.mm2),digits =2 )
meanDataAreaW015_G2_100$Weight <- "100g"
meanDataAreaW015_G2_200<- data.frame(meanArea=round(mean(DataAreaW015_G2_200$Area.mm2),digits =2 ))
meanDataAreaW015_G2_200$SD<- round(sd(DataAreaW015_G2_200$Area.mm2),digits =2 )
meanDataAreaW015_G2_200$Weight <- "200g"
meanDataAreaW015_G2_400<- data.frame(meanArea=round(mean(DataAreaW015_G2_400$Area.mm2),digits =2 ))
meanDataAreaW015_G2_400$SD<- round(sd(DataAreaW015_G2_400$Area.mm2),digits =2 )
meanDataAreaW015_G2_400$Weight <- "400g"
meanDataAreaW015_G2_800<- data.frame(meanArea=round(mean(DataAreaW015_G2_800$Area.mm2),digits =2 ))
meanDataAreaW015_G2_800$SD<- round(sd(DataAreaW015_G2_800$Area.mm2),digits =2 )
meanDataAreaW015_G2_800$Weight <- "800g"
meanDataAreaW015_G2_1000<- data.frame(meanArea=round(mean(DataAreaW015_G2_1000$Area.mm2),digits =2 ))
meanDataAreaW015_G2_1000$SD<- round(sd(DataAreaW015_G2_1000$Area.mm2),digits =2 )
meanDataAreaW015_G2_1000$Weight <- "1000g"
meanDataAreaW015_G2_2000<- data.frame(meanArea=round(mean(DataAreaW015_G2_2000$Area.mm2),digits =2 ))
meanDataAreaW015_G2_2000$SD<- round(sd(DataAreaW015_G2_2000$Area.mm2),digits =2 )
meanDataAreaW015_G2_2000$Weight <- "2000g"

# Combined data sets
DataW000_G2_total <- rbind(meanDataAreaW000_G2_100,meanDataAreaW000_G2_200,meanDataAreaW000_G2_400,
                           meanDataAreaW000_G2_800,meanDataAreaW000_G2_1000,meanDataAreaW000_G2_2000)
DataW000_G2_total$Condition <- "W000_G2"

DataW001_G2_total <- rbind(meanDataAreaW001_G2_100,meanDataAreaW001_G2_200,meanDataAreaW001_G2_400,
                           meanDataAreaW001_G2_800,meanDataAreaW001_G2_1000,meanDataAreaW001_G2_2000)
DataW001_G2_total$Condition <- "W001_G2"

DataW003_G2_total <- rbind(meanDataAreaW003_G2_100,meanDataAreaW003_G2_200,meanDataAreaW003_G2_400,
                           meanDataAreaW003_G2_800,meanDataAreaW003_G2_1000,meanDataAreaW003_G2_2000)
DataW003_G2_total$Condition <- "W003_G2"
 
DataW005total <- rbind(meanDataAreaW005_G2_100,meanDataAreaW005_G2_200,meanDataAreaW005_G2_400,
                        meanDataAreaW005_G2_800,meanDataAreaW005_G2_1000,meanDataAreaW005_G2_2000)
DataW005total$Condition <- "W005_G2"
 
DataW007total <- rbind(meanDataAreaW007_G2_100,meanDataAreaW007_G2_200,meanDataAreaW007_G2_400,
                        meanDataAreaW007_G2_800,meanDataAreaW007_G2_1000,meanDataAreaW007_G2_2000)
DataW007total$Condition <- "W007_G2"

DataW009total <- rbind(meanDataAreaW009_G2_100,meanDataAreaW009_G2_200,meanDataAreaW009_G2_400,
                       meanDataAreaW009_G2_800,meanDataAreaW009_G2_1000,meanDataAreaW009_G2_2000)
DataW009total$Condition <- "W009_G2"

DataW011total <- rbind(meanDataAreaW011_G2_100,meanDataAreaW011_G2_200,meanDataAreaW011_G2_400,
                       meanDataAreaW011_G2_800,meanDataAreaW011_G2_1000,meanDataAreaW011_G2_2000)
DataW011total$Condition <- "W011_G2"

DataW013total <- rbind(meanDataAreaW013_G2_100,meanDataAreaW013_G2_200,meanDataAreaW013_G2_400,
                       meanDataAreaW013_G2_800,meanDataAreaW013_G2_1000,meanDataAreaW013_G2_2000)
DataW013total$Condition <- "W013_G2"

DataW015total <- rbind(meanDataAreaW015_G2_100,meanDataAreaW015_G2_200,meanDataAreaW015_G2_400,
                       meanDataAreaW015_G2_800,meanDataAreaW015_G2_1000,meanDataAreaW015_G2_2000)
DataW015total$Condition <- "W015_G2"

FibreCount_Shedding_G2 <- rbind(DataW000_G2_total,DataW001_G2_total,DataW003_G2_total, DataW005total,DataW007total,DataW009total,DataW011total,DataW013total,DataW015total)

write.table(FibreCount_Shedding_G2, file = "Shedding_Fibre_Count.csv", quote = F, sep = ",", row.names = F)

#### PLOT ####
# calculation of the percentage difference between washed and unwashed
y = rep(c(125, 150, 175, 200, 225),2)
x = rep(c(1:5), 2)

# plot
pSH_G2 <- ggplot(FibreCount_Shedding_G2, aes(x = factor(Weight, level = c('100g', '200g', '400g','800g','1000g','2000g')),
                                          y= meanArea, fill=Condition))+
  geom_bar(stat="identity", position=position_dodge(),colour="black")+
  geom_text(aes(label = meanArea), hjust=0.5,vjust = -5.5,position = position_dodge(.9))+
  labs(x="\nWeight", y="Total fibre area (mm\u00b2)\n") +
  theme_bw(base_family = "Arial", base_size = 12) +
  ylim(0,490)+
  scale_fill_manual(values = brewer.pal(9, "Blues")[1:9])+
  theme(legend.title = element_blank(),
        strip.background.x = element_rect(colour = "grey", fill = "white"),
        legend.position = "bottom",
        legend.background = element_rect(fill="grey95",size=1, linetype="solid", colour="grey80"),
        axis.text.x = element_text(angle = 0, vjust = 0.95, hjust=0.5))+
  geom_errorbar(aes(ymin=meanArea-SD, ymax=meanArea+SD),width=.2,position=position_dodge(.9))
pSH_G2
ggsave("Shedding_G2_W000-15.png", pSH_G2, width = 10, height = 9, units = "in", dpi=150, path = "Results")

# plot - no label
pSH_G2 <- ggplot(FibreCount_Shedding_G2, aes(x = factor(Weight, level = c('100g', '200g', '400g','800g','1000g','2000g')),
                                             y= meanArea, fill=Condition))+
  geom_bar(stat="identity", position=position_dodge(),colour="black")+
  labs(x="\nWeight", y="Total fibre area (mm\u00b2)\n") +
  theme_bw(base_family = "Arial", base_size = 12) +
  ylim(0,375)+
  scale_fill_manual(values = brewer.pal(9, "Blues")[1:9])+
  theme(legend.title = element_blank(),
        strip.background.x = element_rect(colour = "grey", fill = "white"),
        legend.position = "bottom",
        legend.background = element_rect(fill="grey95",size=1, linetype="solid", colour="grey80"),
        axis.text.x = element_text(angle = 0, vjust = 0.95, hjust=0.5))+
  geom_errorbar(aes(ymin=meanArea-SD, ymax=meanArea+SD),width=.2,position=position_dodge(.9))
pSH_G2
ggsave("Shedding_G2_W000-15_No Mean Label.png", pSH_G2, width = 10, height = 9, units = "in", dpi=150, path = "Results")

##################
#####   G3   #####
##################
Shedding_G3 <- read.csv('./Fibre count Summary/SH_G3_W000-W015_Summary.csv', sep="," ,header = T,fileEncoding="UTF-8-BOM")
Shedding_G3$Slice<- gsub(".TIF","",Shedding_G3$Slice)
Shedding_G3Extended <- data.frame(str_split(Shedding_G3$Slice, "_", simplify=TRUE))
names(Shedding_G3Extended) <- c("Project","Wash","Garment","Weight","Repeat","condition")
Shedding_G3Total <- cbind(Shedding_G3Extended,Area=Shedding_G3$Total.Area)

# Convert Area from inch2 to mm2
# 1 pixel = 1 x10^-5 inch2, so (Area*1)/0.000011 to convert into px
Shedding_G3Total$Area.px <- (Shedding_G3Total$Area*1)/0.000011
# 1 mm = 112 pixels, 1mm2 = 12544 px
Shedding_G3Total$Area.mm2 <- Shedding_G3Total$Area.px/12544

# Split data per washing condition
DataAreaW000_G3 <- Shedding_G3Total[Shedding_G3Total$Wash =='W000',]
DataAreaW001_G3 <- Shedding_G3Total[Shedding_G3Total$Wash =='W001',]
DataAreaW003_G3 <- Shedding_G3Total[Shedding_G3Total$Wash =='W003',]
DataAreaW005_G3 <- Shedding_G3Total[Shedding_G3Total$Wash =='W005',]
DataAreaW007_G3 <- Shedding_G3Total[Shedding_G3Total$Wash =='W007',]
DataAreaW009_G3 <- Shedding_G3Total[Shedding_G3Total$Wash =='W009',]
DataAreaW011_G3 <- Shedding_G3Total[Shedding_G3Total$Wash =='W011',]
DataAreaW013_G3 <- Shedding_G3Total[Shedding_G3Total$Wash =='W013',]
DataAreaW015_G3 <- Shedding_G3Total[Shedding_G3Total$Wash =='W015',]

# split per weight
DataAreaW000_G3_100 <- DataAreaW000_G3[DataAreaW000_G3$Weight =='100g',]
DataAreaW000_G3_200 <- DataAreaW000_G3[DataAreaW000_G3$Weight =='200g',]
DataAreaW000_G3_400 <- DataAreaW000_G3[DataAreaW000_G3$Weight =='400g',]
DataAreaW000_G3_800 <- DataAreaW000_G3[DataAreaW000_G3$Weight =='800g',]
DataAreaW000_G3_1000 <- DataAreaW000_G3[DataAreaW000_G3$Weight =='1000g',]
DataAreaW000_G3_2000 <- DataAreaW000_G3[DataAreaW000_G3$Weight =='2000g',]

DataAreaW001_G3_100 <- DataAreaW001_G3[DataAreaW001_G3$Weight =='100g',]
DataAreaW001_G3_200 <- DataAreaW001_G3[DataAreaW001_G3$Weight =='200g',]
DataAreaW001_G3_400 <- DataAreaW001_G3[DataAreaW001_G3$Weight =='400g',]
DataAreaW001_G3_800 <- DataAreaW001_G3[DataAreaW001_G3$Weight =='800g',]
DataAreaW001_G3_1000 <- DataAreaW001_G3[DataAreaW001_G3$Weight =='1000g',]
DataAreaW001_G3_2000 <- DataAreaW001_G3[DataAreaW001_G3$Weight =='2000g',]

DataAreaW003_G3_100 <- DataAreaW003_G3[DataAreaW003_G3$Weight =='100g',]
DataAreaW003_G3_200 <- DataAreaW003_G3[DataAreaW003_G3$Weight =='200g',]
DataAreaW003_G3_400 <- DataAreaW003_G3[DataAreaW003_G3$Weight =='400g',]
DataAreaW003_G3_800 <- DataAreaW003_G3[DataAreaW003_G3$Weight =='800g',]
DataAreaW003_G3_1000 <- DataAreaW003_G3[DataAreaW003_G3$Weight =='1000g',]
DataAreaW003_G3_2000 <- DataAreaW003_G3[DataAreaW003_G3$Weight =='2000g',]

DataAreaW005_G3_100 <- DataAreaW005_G3[DataAreaW005_G3$Weight =='100g',]
DataAreaW005_G3_200 <- DataAreaW005_G3[DataAreaW005_G3$Weight =='200g',]
DataAreaW005_G3_400 <- DataAreaW005_G3[DataAreaW005_G3$Weight =='400g',]
DataAreaW005_G3_800 <- DataAreaW005_G3[DataAreaW005_G3$Weight =='800g',]
DataAreaW005_G3_1000 <- DataAreaW005_G3[DataAreaW005_G3$Weight =='1000g',]
DataAreaW005_G3_2000 <- DataAreaW005_G3[DataAreaW005_G3$Weight =='2000g',]

DataAreaW007_G3_100 <- DataAreaW007_G3[DataAreaW007_G3$Weight =='100g',]
DataAreaW007_G3_200 <- DataAreaW007_G3[DataAreaW007_G3$Weight =='200g',]
DataAreaW007_G3_400 <- DataAreaW007_G3[DataAreaW007_G3$Weight =='400g',]
DataAreaW007_G3_800 <- DataAreaW007_G3[DataAreaW007_G3$Weight =='800g',]
DataAreaW007_G3_1000 <- DataAreaW007_G3[DataAreaW007_G3$Weight =='1000g',]
DataAreaW007_G3_2000 <- DataAreaW007_G3[DataAreaW007_G3$Weight =='2000g',]

DataAreaW009_G3_100 <- DataAreaW009_G3[DataAreaW009_G3$Weight =='100g',]
DataAreaW009_G3_200 <- DataAreaW009_G3[DataAreaW009_G3$Weight =='200g',]
DataAreaW009_G3_400 <- DataAreaW009_G3[DataAreaW009_G3$Weight =='400g',]
DataAreaW009_G3_800 <- DataAreaW009_G3[DataAreaW009_G3$Weight =='800g',]
DataAreaW009_G3_1000 <- DataAreaW009_G3[DataAreaW009_G3$Weight =='1000g',]
DataAreaW009_G3_2000 <- DataAreaW009_G3[DataAreaW009_G3$Weight =='2000g',]

DataAreaW011_G3_100 <- DataAreaW011_G3[DataAreaW011_G3$Weight =='100g',]
DataAreaW011_G3_200 <- DataAreaW011_G3[DataAreaW011_G3$Weight =='200g',]
DataAreaW011_G3_400 <- DataAreaW011_G3[DataAreaW011_G3$Weight =='400g',]
DataAreaW011_G3_800 <- DataAreaW011_G3[DataAreaW011_G3$Weight =='800g',]
DataAreaW011_G3_1000 <- DataAreaW011_G3[DataAreaW011_G3$Weight =='1000g',]
DataAreaW011_G3_2000 <- DataAreaW011_G3[DataAreaW011_G3$Weight =='2000g',]

DataAreaW013_G3_100 <- DataAreaW013_G3[DataAreaW013_G3$Weight =='100g',]
DataAreaW013_G3_200 <- DataAreaW013_G3[DataAreaW013_G3$Weight =='200g',]
DataAreaW013_G3_400 <- DataAreaW013_G3[DataAreaW013_G3$Weight =='400g',]
DataAreaW013_G3_800 <- DataAreaW013_G3[DataAreaW013_G3$Weight =='800g',]
DataAreaW013_G3_1000 <- DataAreaW013_G3[DataAreaW013_G3$Weight =='1000g',]
DataAreaW013_G3_2000 <- DataAreaW013_G3[DataAreaW013_G3$Weight =='2000g',]

DataAreaW015_G3_100 <- DataAreaW015_G3[DataAreaW015_G3$Weight =='100g',]
DataAreaW015_G3_200 <- DataAreaW015_G3[DataAreaW015_G3$Weight =='200g',]
DataAreaW015_G3_400 <- DataAreaW015_G3[DataAreaW015_G3$Weight =='400g',]
DataAreaW015_G3_800 <- DataAreaW015_G3[DataAreaW015_G3$Weight =='800g',]
DataAreaW015_G3_1000 <- DataAreaW015_G3[DataAreaW015_G3$Weight =='1000g',]
DataAreaW015_G3_2000 <- DataAreaW015_G3[DataAreaW015_G3$Weight =='2000g',]

# Calculation of mean and SD
meanDataAreaW000_G3_100<- data.frame(meanArea=round(mean(DataAreaW000_G3_100$Area.mm2),digits =2 ))
meanDataAreaW000_G3_100$SD<- round(sd(DataAreaW000_G3_100$Area.mm2),digits =2 )
meanDataAreaW000_G3_100$Weight <- "100g"
meanDataAreaW000_G3_200<- data.frame(meanArea=round(mean(DataAreaW000_G3_200$Area.mm2),digits =2 ))
meanDataAreaW000_G3_200$SD<- round(sd(DataAreaW000_G3_200$Area.mm2),digits =2 )
meanDataAreaW000_G3_200$Weight <- "200g"
meanDataAreaW000_G3_400<- data.frame(meanArea=round(mean(DataAreaW000_G3_400$Area.mm2),digits =2 ))
meanDataAreaW000_G3_400$SD<- round(sd(DataAreaW000_G3_400$Area.mm2),digits =2 )
meanDataAreaW000_G3_400$Weight <- "400g"
meanDataAreaW000_G3_800<- data.frame(meanArea=round(mean(DataAreaW000_G3_800$Area.mm2),digits =2 ))
meanDataAreaW000_G3_800$SD<- round(sd(DataAreaW000_G3_800$Area.mm2),digits =2 )
meanDataAreaW000_G3_800$Weight <- "800g"
meanDataAreaW000_G3_1000<- data.frame(meanArea=round(mean(DataAreaW000_G3_1000$Area.mm2),digits =2 ))
meanDataAreaW000_G3_1000$SD<- round(sd(DataAreaW000_G3_1000$Area.mm2),digits =2 )
meanDataAreaW000_G3_1000$Weight <- "1000g"
meanDataAreaW000_G3_2000<- data.frame(meanArea=round(mean(DataAreaW000_G3_2000$Area.mm2),digits =2 ))
meanDataAreaW000_G3_2000$SD<- round(sd(DataAreaW000_G3_2000$Area.mm2),digits =2 )
meanDataAreaW000_G3_2000$Weight <- "2000g"

meanDataAreaW001_G3_100<- data.frame(meanArea=round(mean(DataAreaW001_G3_100$Area.mm2),digits =2 ))
meanDataAreaW001_G3_100$SD<- round(sd(DataAreaW001_G3_100$Area.mm2),digits =2 )
meanDataAreaW001_G3_100$Weight <- "100g"
meanDataAreaW001_G3_200<- data.frame(meanArea=round(mean(DataAreaW001_G3_200$Area.mm2),digits =2 ))
meanDataAreaW001_G3_200$SD<- round(sd(DataAreaW001_G3_200$Area.mm2),digits =2 )
meanDataAreaW001_G3_200$Weight <- "200g"
meanDataAreaW001_G3_400<- data.frame(meanArea=round(mean(DataAreaW001_G3_400$Area.mm2),digits =2 ))
meanDataAreaW001_G3_400$SD<- round(sd(DataAreaW001_G3_400$Area.mm2),digits =2 )
meanDataAreaW001_G3_400$Weight <- "400g"
meanDataAreaW001_G3_800<- data.frame(meanArea=round(mean(DataAreaW001_G3_800$Area.mm2),digits =2 ))
meanDataAreaW001_G3_800$SD<- round(sd(DataAreaW001_G3_800$Area.mm2),digits =2 )
meanDataAreaW001_G3_800$Weight <- "800g"
meanDataAreaW001_G3_1000<- data.frame(meanArea=round(mean(DataAreaW001_G3_1000$Area.mm2),digits =2 ))
meanDataAreaW001_G3_1000$SD<- round(sd(DataAreaW001_G3_1000$Area.mm2),digits =2 )
meanDataAreaW001_G3_1000$Weight <- "1000g"
meanDataAreaW001_G3_2000<- data.frame(meanArea=round(mean(DataAreaW001_G3_2000$Area.mm2),digits =2 ))
meanDataAreaW001_G3_2000$SD<- round(sd(DataAreaW001_G3_2000$Area.mm2),digits =2 )
meanDataAreaW001_G3_2000$Weight <- "2000g"

meanDataAreaW003_G3_100<- data.frame(meanArea=round(mean(DataAreaW003_G3_100$Area.mm2),digits =2 ))
meanDataAreaW003_G3_100$SD<- round(sd(DataAreaW003_G3_100$Area.mm2),digits =2 )
meanDataAreaW003_G3_100$Weight <- "100g"
meanDataAreaW003_G3_200<- data.frame(meanArea=round(mean(DataAreaW003_G3_200$Area.mm2),digits =2 ))
meanDataAreaW003_G3_200$SD<- round(sd(DataAreaW003_G3_200$Area.mm2),digits =2 )
meanDataAreaW003_G3_200$Weight <- "200g"
meanDataAreaW003_G3_400<- data.frame(meanArea=round(mean(DataAreaW003_G3_400$Area.mm2),digits =2 ))
meanDataAreaW003_G3_400$SD<- round(sd(DataAreaW003_G3_400$Area.mm2),digits =2 )
meanDataAreaW003_G3_400$Weight <- "400g"
meanDataAreaW003_G3_800<- data.frame(meanArea=round(mean(DataAreaW003_G3_800$Area.mm2),digits =2 ))
meanDataAreaW003_G3_800$SD<- round(sd(DataAreaW003_G3_800$Area.mm2),digits =2 )
meanDataAreaW003_G3_800$Weight <- "800g"
meanDataAreaW003_G3_1000<- data.frame(meanArea=round(mean(DataAreaW003_G3_1000$Area.mm2),digits =2 ))
meanDataAreaW003_G3_1000$SD<- round(sd(DataAreaW003_G3_1000$Area.mm2),digits =2 )
meanDataAreaW003_G3_1000$Weight <- "1000g"
meanDataAreaW003_G3_2000<- data.frame(meanArea=round(mean(DataAreaW003_G3_2000$Area.mm2),digits =2 ))
meanDataAreaW003_G3_2000$SD<- round(sd(DataAreaW003_G3_2000$Area.mm2),digits =2 )
meanDataAreaW003_G3_2000$Weight <- "2000g"

meanDataAreaW005_G3_100<- data.frame(meanArea=round(mean(DataAreaW005_G3_100$Area.mm2),digits =2 ))
meanDataAreaW005_G3_100$SD<- round(sd(DataAreaW005_G3_100$Area.mm2),digits =2 )
meanDataAreaW005_G3_100$Weight <- "100g"
meanDataAreaW005_G3_200<- data.frame(meanArea=round(mean(DataAreaW005_G3_200$Area.mm2),digits =2 ))
meanDataAreaW005_G3_200$SD<- round(sd(DataAreaW005_G3_200$Area.mm2),digits =2 )
meanDataAreaW005_G3_200$Weight <- "200g"
meanDataAreaW005_G3_400<- data.frame(meanArea=round(mean(DataAreaW005_G3_400$Area.mm2),digits =2 ))
meanDataAreaW005_G3_400$SD<- round(sd(DataAreaW005_G3_400$Area.mm2),digits =2 )
meanDataAreaW005_G3_400$Weight <- "400g"
meanDataAreaW005_G3_800<- data.frame(meanArea=round(mean(DataAreaW005_G3_800$Area.mm2),digits =2 ))
meanDataAreaW005_G3_800$SD<- round(sd(DataAreaW005_G3_800$Area.mm2),digits =2 )
meanDataAreaW005_G3_800$Weight <- "800g"
meanDataAreaW005_G3_1000<- data.frame(meanArea=round(mean(DataAreaW005_G3_1000$Area.mm2),digits =2 ))
meanDataAreaW005_G3_1000$SD<- round(sd(DataAreaW005_G3_1000$Area.mm2),digits =2 )
meanDataAreaW005_G3_1000$Weight <- "1000g"
meanDataAreaW005_G3_2000<- data.frame(meanArea=round(mean(DataAreaW005_G3_2000$Area.mm2),digits =2 ))
meanDataAreaW005_G3_2000$SD<- round(sd(DataAreaW005_G3_2000$Area.mm2),digits =2 )
meanDataAreaW005_G3_2000$Weight <- "2000g"

meanDataAreaW007_G3_100<- data.frame(meanArea=round(mean(DataAreaW007_G3_100$Area.mm2),digits =2 ))
meanDataAreaW007_G3_100$SD<- round(sd(DataAreaW007_G3_100$Area.mm2),digits =2 )
meanDataAreaW007_G3_100$Weight <- "100g"
meanDataAreaW007_G3_200<- data.frame(meanArea=round(mean(DataAreaW007_G3_200$Area.mm2),digits =2 ))
meanDataAreaW007_G3_200$SD<- round(sd(DataAreaW007_G3_200$Area.mm2),digits =2 )
meanDataAreaW007_G3_200$Weight <- "200g"
meanDataAreaW007_G3_400<- data.frame(meanArea=round(mean(DataAreaW007_G3_400$Area.mm2),digits =2 ))
meanDataAreaW007_G3_400$SD<- round(sd(DataAreaW007_G3_400$Area.mm2),digits =2 )
meanDataAreaW007_G3_400$Weight <- "400g"
meanDataAreaW007_G3_800<- data.frame(meanArea=round(mean(DataAreaW007_G3_800$Area.mm2),digits =2 ))
meanDataAreaW007_G3_800$SD<- round(sd(DataAreaW007_G3_800$Area.mm2),digits =2 )
meanDataAreaW007_G3_800$Weight <- "800g"
meanDataAreaW007_G3_1000<- data.frame(meanArea=round(mean(DataAreaW007_G3_1000$Area.mm2),digits =2 ))
meanDataAreaW007_G3_1000$SD<- round(sd(DataAreaW007_G3_1000$Area.mm2),digits =2 )
meanDataAreaW007_G3_1000$Weight <- "1000g"
meanDataAreaW007_G3_2000<- data.frame(meanArea=round(mean(DataAreaW007_G3_2000$Area.mm2),digits =2 ))
meanDataAreaW007_G3_2000$SD<- round(sd(DataAreaW007_G3_2000$Area.mm2),digits =2 )
meanDataAreaW007_G3_2000$Weight <- "2000g"

meanDataAreaW009_G3_100<- data.frame(meanArea=round(mean(DataAreaW009_G3_100$Area.mm2),digits =2 ))
meanDataAreaW009_G3_100$SD<- round(sd(DataAreaW009_G3_100$Area.mm2),digits =2 )
meanDataAreaW009_G3_100$Weight <- "100g"
meanDataAreaW009_G3_200<- data.frame(meanArea=round(mean(DataAreaW009_G3_200$Area.mm2),digits =2 ))
meanDataAreaW009_G3_200$SD<- round(sd(DataAreaW009_G3_200$Area.mm2),digits =2 )
meanDataAreaW009_G3_200$Weight <- "200g"
meanDataAreaW009_G3_400<- data.frame(meanArea=round(mean(DataAreaW009_G3_400$Area.mm2),digits =2 ))
meanDataAreaW009_G3_400$SD<- round(sd(DataAreaW009_G3_400$Area.mm2),digits =2 )
meanDataAreaW009_G3_400$Weight <- "400g"
meanDataAreaW009_G3_800<- data.frame(meanArea=round(mean(DataAreaW009_G3_800$Area.mm2),digits =2 ))
meanDataAreaW009_G3_800$SD<- round(sd(DataAreaW009_G3_800$Area.mm2),digits =2 )
meanDataAreaW009_G3_800$Weight <- "800g"
meanDataAreaW009_G3_1000<- data.frame(meanArea=round(mean(DataAreaW009_G3_1000$Area.mm2),digits =2 ))
meanDataAreaW009_G3_1000$SD<- round(sd(DataAreaW009_G3_1000$Area.mm2),digits =2 )
meanDataAreaW009_G3_1000$Weight <- "1000g"
meanDataAreaW009_G3_2000<- data.frame(meanArea=round(mean(DataAreaW009_G3_2000$Area.mm2),digits =2 ))
meanDataAreaW009_G3_2000$SD<- round(sd(DataAreaW009_G3_2000$Area.mm2),digits =2 )
meanDataAreaW009_G3_2000$Weight <- "2000g"

meanDataAreaW011_G3_100<- data.frame(meanArea=round(mean(DataAreaW011_G3_100$Area.mm2),digits =2 ))
meanDataAreaW011_G3_100$SD<- round(sd(DataAreaW011_G3_100$Area.mm2),digits =2 )
meanDataAreaW011_G3_100$Weight <- "100g"
meanDataAreaW011_G3_200<- data.frame(meanArea=round(mean(DataAreaW011_G3_200$Area.mm2),digits =2 ))
meanDataAreaW011_G3_200$SD<- round(sd(DataAreaW011_G3_200$Area.mm2),digits =2 )
meanDataAreaW011_G3_200$Weight <- "200g"
meanDataAreaW011_G3_400<- data.frame(meanArea=round(mean(DataAreaW011_G3_400$Area.mm2),digits =2 ))
meanDataAreaW011_G3_400$SD<- round(sd(DataAreaW011_G3_400$Area.mm2),digits =2 )
meanDataAreaW011_G3_400$Weight <- "400g"
meanDataAreaW011_G3_800<- data.frame(meanArea=round(mean(DataAreaW011_G3_800$Area.mm2),digits =2 ))
meanDataAreaW011_G3_800$SD<- round(sd(DataAreaW011_G3_800$Area.mm2),digits =2 )
meanDataAreaW011_G3_800$Weight <- "800g"
meanDataAreaW011_G3_1000<- data.frame(meanArea=round(mean(DataAreaW011_G3_1000$Area.mm2),digits =2 ))
meanDataAreaW011_G3_1000$SD<- round(sd(DataAreaW011_G3_1000$Area.mm2),digits =2 )
meanDataAreaW011_G3_1000$Weight <- "1000g"
meanDataAreaW011_G3_2000<- data.frame(meanArea=round(mean(DataAreaW011_G3_2000$Area.mm2),digits =2 ))
meanDataAreaW011_G3_2000$SD<- round(sd(DataAreaW011_G3_2000$Area.mm2),digits =2 )
meanDataAreaW011_G3_2000$Weight <- "2000g"

meanDataAreaW013_G3_100<- data.frame(meanArea=round(mean(DataAreaW013_G3_100$Area.mm2),digits =2 ))
meanDataAreaW013_G3_100$SD<- round(sd(DataAreaW013_G3_100$Area.mm2),digits =2 )
meanDataAreaW013_G3_100$Weight <- "100g"
meanDataAreaW013_G3_200<- data.frame(meanArea=round(mean(DataAreaW013_G3_200$Area.mm2),digits =2 ))
meanDataAreaW013_G3_200$SD<- round(sd(DataAreaW013_G3_200$Area.mm2),digits =2 )
meanDataAreaW013_G3_200$Weight <- "200g"
meanDataAreaW013_G3_400<- data.frame(meanArea=round(mean(DataAreaW013_G3_400$Area.mm2),digits =2 ))
meanDataAreaW013_G3_400$SD<- round(sd(DataAreaW013_G3_400$Area.mm2),digits =2 )
meanDataAreaW013_G3_400$Weight <- "400g"
meanDataAreaW013_G3_800<- data.frame(meanArea=round(mean(DataAreaW013_G3_800$Area.mm2),digits =2 ))
meanDataAreaW013_G3_800$SD<- round(sd(DataAreaW013_G3_800$Area.mm2),digits =2 )
meanDataAreaW013_G3_800$Weight <- "800g"
meanDataAreaW013_G3_1000<- data.frame(meanArea=round(mean(DataAreaW013_G3_1000$Area.mm2),digits =2 ))
meanDataAreaW013_G3_1000$SD<- round(sd(DataAreaW013_G3_1000$Area.mm2),digits =2 )
meanDataAreaW013_G3_1000$Weight <- "1000g"
meanDataAreaW013_G3_2000<- data.frame(meanArea=round(mean(DataAreaW013_G3_2000$Area.mm2),digits =2 ))
meanDataAreaW013_G3_2000$SD<- round(sd(DataAreaW013_G3_2000$Area.mm2),digits =2 )
meanDataAreaW013_G3_2000$Weight <- "2000g"

meanDataAreaW015_G3_100<- data.frame(meanArea=round(mean(DataAreaW015_G3_100$Area.mm2),digits =2 ))
meanDataAreaW015_G3_100$SD<- round(sd(DataAreaW015_G3_100$Area.mm2),digits =2 )
meanDataAreaW015_G3_100$Weight <- "100g"
meanDataAreaW015_G3_200<- data.frame(meanArea=round(mean(DataAreaW015_G3_200$Area.mm2),digits =2 ))
meanDataAreaW015_G3_200$SD<- round(sd(DataAreaW015_G3_200$Area.mm2),digits =2 )
meanDataAreaW015_G3_200$Weight <- "200g"
meanDataAreaW015_G3_400<- data.frame(meanArea=round(mean(DataAreaW015_G3_400$Area.mm2),digits =2 ))
meanDataAreaW015_G3_400$SD<- round(sd(DataAreaW015_G3_400$Area.mm2),digits =2 )
meanDataAreaW015_G3_400$Weight <- "400g"
meanDataAreaW015_G3_800<- data.frame(meanArea=round(mean(DataAreaW015_G3_800$Area.mm2),digits =2 ))
meanDataAreaW015_G3_800$SD<- round(sd(DataAreaW015_G3_800$Area.mm2),digits =2 )
meanDataAreaW015_G3_800$Weight <- "800g"
meanDataAreaW015_G3_1000<- data.frame(meanArea=round(mean(DataAreaW015_G3_1000$Area.mm2),digits =2 ))
meanDataAreaW015_G3_1000$SD<- round(sd(DataAreaW015_G3_1000$Area.mm2),digits =2 )
meanDataAreaW015_G3_1000$Weight <- "1000g"
meanDataAreaW015_G3_2000<- data.frame(meanArea=round(mean(DataAreaW015_G3_2000$Area.mm2),digits =2 ))
meanDataAreaW015_G3_2000$SD<- round(sd(DataAreaW015_G3_2000$Area.mm2),digits =2 )
meanDataAreaW015_G3_2000$Weight <- "2000g"

# Combined data sets
DataW000_G3_total <- rbind(meanDataAreaW000_G3_100,meanDataAreaW000_G3_200,meanDataAreaW000_G3_400,
                           meanDataAreaW000_G3_800,meanDataAreaW000_G3_1000,meanDataAreaW000_G3_2000)
DataW000_G3_total$Condition <- "W000_G3"

DataW001_G3_total <- rbind(meanDataAreaW001_G3_100,meanDataAreaW001_G3_200,meanDataAreaW001_G3_400,
                           meanDataAreaW001_G3_800,meanDataAreaW001_G3_1000,meanDataAreaW001_G3_2000)
DataW001_G3_total$Condition <- "W001_G3"

DataW003_G3_total <- rbind(meanDataAreaW003_G3_100,meanDataAreaW003_G3_200,meanDataAreaW003_G3_400,
                           meanDataAreaW003_G3_800,meanDataAreaW003_G3_1000,meanDataAreaW003_G3_2000)
DataW003_G3_total$Condition <- "W003_G3"

DataW005_G3_total <- rbind(meanDataAreaW005_G3_100,meanDataAreaW005_G3_200,meanDataAreaW005_G3_400,
                           meanDataAreaW005_G3_800,meanDataAreaW005_G3_1000,meanDataAreaW005_G3_2000)
DataW005_G3_total$Condition <- "W005_G3"

DataW007_G3_total <- rbind(meanDataAreaW007_G3_100,meanDataAreaW007_G3_200,meanDataAreaW007_G3_400,
                           meanDataAreaW007_G3_800,meanDataAreaW007_G3_1000,meanDataAreaW007_G3_2000)
DataW007_G3_total$Condition <- "W007_G3"

DataW009_G3_total <- rbind(meanDataAreaW009_G3_100,meanDataAreaW009_G3_200,meanDataAreaW009_G3_400,
                           meanDataAreaW009_G3_800,meanDataAreaW009_G3_1000,meanDataAreaW009_G3_2000)
DataW009_G3_total$Condition <- "W009_G3"

DataW011_G3_total <- rbind(meanDataAreaW011_G3_100,meanDataAreaW011_G3_200,meanDataAreaW011_G3_400,
                           meanDataAreaW011_G3_800,meanDataAreaW011_G3_1000,meanDataAreaW011_G3_2000)
DataW011_G3_total$Condition <- "W011_G3"

DataW013_G3_total <- rbind(meanDataAreaW013_G3_100,meanDataAreaW013_G3_200,meanDataAreaW013_G3_400,
                           meanDataAreaW013_G3_800,meanDataAreaW013_G3_1000,meanDataAreaW013_G3_2000)
DataW013_G3_total$Condition <- "W013_G3"

DataW015_G3_total <- rbind(meanDataAreaW015_G3_100,meanDataAreaW015_G3_200,meanDataAreaW015_G3_400,
                           meanDataAreaW015_G3_800,meanDataAreaW015_G3_1000,meanDataAreaW015_G3_2000)
DataW015_G3_total$Condition <- "W015_G3"

FibreCount_Shedding_G3 <- rbind(DataW000_G3_total,DataW001_G3_total,DataW003_G3_total, DataW005_G3_total, DataW007_G3_total,DataW009_G3_total,DataW011_G3_total,DataW013_G3_total,DataW015_G3_total)

write.table(FibreCount_Shedding_G3, file = "Shedding_Fibre_Count.csv", quote = F, sep = ",", row.names = F)

#### PLOT ####
# calculation of the percentage difference between washed and unwashed
y = rep(c(125, 150, 175, 200, 225),2)
x = rep(c(1:5), 2)

# plot
pSH_G3 <- ggplot(FibreCount_Shedding_G3, aes(x = factor(Weight, level = c('100g', '200g', '400g','800g','1000g','2000g')),
                                          y= meanArea, fill=Condition))+
  geom_bar(stat="identity", position=position_dodge(),colour="black")+
  geom_text(aes(label = meanArea), hjust=0.5,vjust = -5.5,position = position_dodge(.9))+
  labs(x="\nWeight", y="Total fibre area (mm\u00b2)\n") +
  theme_bw(base_family = "Arial", base_size = 12) +
  ylim(0,490)+
  scale_fill_manual(values = brewer.pal(9, "Reds")[1:9])+
  theme(legend.title = element_blank(),
        strip.background.x = element_rect(colour = "grey", fill = "white"),
        legend.position = "bottom",
        legend.background = element_rect(fill="grey95",size=1, linetype="solid", colour="grey80"),
        axis.text.x = element_text(angle = 0, vjust = 0.95, hjust=0.5))+
  geom_errorbar(aes(ymin=meanArea-SD, ymax=meanArea+SD),width=.2,position=position_dodge(.9))
pSH_G3
ggsave("Shedding_G3_W000-15.png", pSH_G3, width = 10, height = 9, units = "in", dpi=150, path = "Results")

# plot - no label
pSH_G3 <- ggplot(FibreCount_Shedding_G3, aes(x = factor(Weight, level = c('100g', '200g', '400g','800g','1000g','2000g')),
                                             y= meanArea, fill=Condition))+
  geom_bar(stat="identity", position=position_dodge(),colour="black")+
  labs(x="\nWeight", y="Total fibre area (mm\u00b2)\n") +
  theme_bw(base_family = "Arial", base_size = 12) +
  ylim(0,400)+
  scale_fill_manual(values = brewer.pal(9, "Reds")[1:9])+
  theme(legend.title = element_blank(),
        strip.background.x = element_rect(colour = "grey", fill = "white"),
        legend.position = "bottom",
        legend.background = element_rect(fill="grey95",size=1, linetype="solid", colour="grey80"),
        axis.text.x = element_text(angle = 0, vjust = 0.95, hjust=0.5))+
  geom_errorbar(aes(ymin=meanArea-SD, ymax=meanArea+SD),width=.2,position=position_dodge(.9))
pSH_G3
ggsave("Shedding_G3_W000-15_No Mean Label.png", pSH_G3, width = 10, height = 9, units = "in", dpi=150, path = "Results")


##################
####    G4    ####
##################
Shedding_G4 <- read.csv('./Fibre count Summary/SH_G4_W000-W011_Summary.csv', sep="," ,header = T,fileEncoding="UTF-8-BOM")
Shedding_G4$Slice<- gsub(".TIF","",Shedding_G4$Slice)
Shedding_G4Extended <- data.frame(str_split(Shedding_G4$Slice, "_", simplify=TRUE))
names(Shedding_G4Extended) <- c("Project","Wash","Garment","Weight","Repeat","condition")
Shedding_G4Total <- cbind(Shedding_G4Extended,Area=Shedding_G4$Total.Area)

# Convert Area from inch2 to mm2
# 1 pixel = 1 x10^-5 inch2, so (Area*1)/0.000011 to convert into px
Shedding_G4Total$Area.px <- (Shedding_G4Total$Area*1)/0.000011
# 1 mm = 112 pixels, 1mm2 = 12544 px
Shedding_G4Total$Area.mm2 <- Shedding_G4Total$Area.px/12544

# Split data per washing condition
DataAreaW000_G4 <- Shedding_G4Total[Shedding_G4Total$Wash =='W000',]
DataAreaW001_G4 <- Shedding_G4Total[Shedding_G4Total$Wash =='W001',]
DataAreaW003_G4 <- Shedding_G4Total[Shedding_G4Total$Wash =='W003',]
DataAreaW005_G4 <- Shedding_G4Total[Shedding_G4Total$Wash =='W005',]
DataAreaW007_G4 <- Shedding_G4Total[Shedding_G4Total$Wash =='W007',]
DataAreaW009_G4 <- Shedding_G4Total[Shedding_G4Total$Wash =='W009',]
DataAreaW011_G4 <- Shedding_G4Total[Shedding_G4Total$Wash =='W011',]

# split per weight
DataAreaW000_G4_100 <- DataAreaW000_G4[DataAreaW000_G4$Weight =='100g',]
DataAreaW000_G4_200 <- DataAreaW000_G4[DataAreaW000_G4$Weight =='200g',]
DataAreaW000_G4_400 <- DataAreaW000_G4[DataAreaW000_G4$Weight =='400g',]
DataAreaW000_G4_800 <- DataAreaW000_G4[DataAreaW000_G4$Weight =='800g',]
DataAreaW000_G4_1000 <- DataAreaW000_G4[DataAreaW000_G4$Weight =='1000g',]
DataAreaW000_G4_2000 <- DataAreaW000_G4[DataAreaW000_G4$Weight =='2000g',]

DataAreaW001_G4_100 <- DataAreaW001_G4[DataAreaW001_G4$Weight =='100g',]
DataAreaW001_G4_200 <- DataAreaW001_G4[DataAreaW001_G4$Weight =='200g',]
DataAreaW001_G4_400 <- DataAreaW001_G4[DataAreaW001_G4$Weight =='400g',]
DataAreaW001_G4_800 <- DataAreaW001_G4[DataAreaW001_G4$Weight =='800g',]
DataAreaW001_G4_1000 <- DataAreaW001_G4[DataAreaW001_G4$Weight =='1000g',]
DataAreaW001_G4_2000 <- DataAreaW001_G4[DataAreaW001_G4$Weight =='2000g',]

DataAreaW003_G4_100 <- DataAreaW003_G4[DataAreaW003_G4$Weight =='100g',]
DataAreaW003_G4_200 <- DataAreaW003_G4[DataAreaW003_G4$Weight =='200g',]
DataAreaW003_G4_400 <- DataAreaW003_G4[DataAreaW003_G4$Weight =='400g',]
DataAreaW003_G4_800 <- DataAreaW003_G4[DataAreaW003_G4$Weight =='800g',]
DataAreaW003_G4_1000 <- DataAreaW003_G4[DataAreaW003_G4$Weight =='1000g',]
DataAreaW003_G4_2000 <- DataAreaW003_G4[DataAreaW003_G4$Weight =='2000g',]

DataAreaW005_G4_100 <- DataAreaW005_G4[DataAreaW005_G4$Weight =='100g',]
DataAreaW005_G4_200 <- DataAreaW005_G4[DataAreaW005_G4$Weight =='200g',]
DataAreaW005_G4_400 <- DataAreaW005_G4[DataAreaW005_G4$Weight =='400g',]
DataAreaW005_G4_800 <- DataAreaW005_G4[DataAreaW005_G4$Weight =='800g',]
DataAreaW005_G4_1000 <- DataAreaW005_G4[DataAreaW005_G4$Weight =='1000g',]
DataAreaW005_G4_2000 <- DataAreaW005_G4[DataAreaW005_G4$Weight =='2000g',]

DataAreaW007_G4_100 <- DataAreaW007_G4[DataAreaW007_G4$Weight =='100g',]
DataAreaW007_G4_200 <- DataAreaW007_G4[DataAreaW007_G4$Weight =='200g',]
DataAreaW007_G4_400 <- DataAreaW007_G4[DataAreaW007_G4$Weight =='400g',]
DataAreaW007_G4_800 <- DataAreaW007_G4[DataAreaW007_G4$Weight =='800g',]
DataAreaW007_G4_1000 <- DataAreaW007_G4[DataAreaW007_G4$Weight =='1000g',]
DataAreaW007_G4_2000 <- DataAreaW007_G4[DataAreaW007_G4$Weight =='2000g',]

DataAreaW009_G4_100 <- DataAreaW009_G4[DataAreaW009_G4$Weight =='100g',]
DataAreaW009_G4_200 <- DataAreaW009_G4[DataAreaW009_G4$Weight =='200g',]
DataAreaW009_G4_400 <- DataAreaW009_G4[DataAreaW009_G4$Weight =='400g',]
DataAreaW009_G4_800 <- DataAreaW009_G4[DataAreaW009_G4$Weight =='800g',]
DataAreaW009_G4_1000 <- DataAreaW009_G4[DataAreaW009_G4$Weight =='1000g',]
DataAreaW009_G4_2000 <- DataAreaW009_G4[DataAreaW009_G4$Weight =='2000g',]

DataAreaW011_G4_100 <- DataAreaW011_G4[DataAreaW011_G4$Weight =='100g',]
DataAreaW011_G4_200 <- DataAreaW011_G4[DataAreaW011_G4$Weight =='200g',]
DataAreaW011_G4_400 <- DataAreaW011_G4[DataAreaW011_G4$Weight =='400g',]
DataAreaW011_G4_800 <- DataAreaW011_G4[DataAreaW011_G4$Weight =='800g',]
DataAreaW011_G4_1000 <- DataAreaW011_G4[DataAreaW011_G4$Weight =='1000g',]
DataAreaW011_G4_2000 <- DataAreaW011_G4[DataAreaW011_G4$Weight =='2000g',]

# Calculation of mean and SD
meanDataAreaW000_G4_100<- data.frame(meanArea=round(mean(DataAreaW000_G4_100$Area.mm2),digits =2 ))
meanDataAreaW000_G4_100$SD<- round(sd(DataAreaW000_G4_100$Area.mm2),digits =2 )
meanDataAreaW000_G4_100$Weight <- "100g"
meanDataAreaW000_G4_200<- data.frame(meanArea=round(mean(DataAreaW000_G4_200$Area.mm2),digits =2 ))
meanDataAreaW000_G4_200$SD<- round(sd(DataAreaW000_G4_200$Area.mm2),digits =2 )
meanDataAreaW000_G4_200$Weight <- "200g"
meanDataAreaW000_G4_400<- data.frame(meanArea=round(mean(DataAreaW000_G4_400$Area.mm2),digits =2 ))
meanDataAreaW000_G4_400$SD<- round(sd(DataAreaW000_G4_400$Area.mm2),digits =2 )
meanDataAreaW000_G4_400$Weight <- "400g"
meanDataAreaW000_G4_800<- data.frame(meanArea=round(mean(DataAreaW000_G4_800$Area.mm2),digits =2 ))
meanDataAreaW000_G4_800$SD<- round(sd(DataAreaW000_G4_800$Area.mm2),digits =2 )
meanDataAreaW000_G4_800$Weight <- "800g"
meanDataAreaW000_G4_1000<- data.frame(meanArea=round(mean(DataAreaW000_G4_1000$Area.mm2),digits =2 ))
meanDataAreaW000_G4_1000$SD<- round(sd(DataAreaW000_G4_1000$Area.mm2),digits =2 )
meanDataAreaW000_G4_1000$Weight <- "1000g"
meanDataAreaW000_G4_2000<- data.frame(meanArea=round(mean(DataAreaW000_G4_2000$Area.mm2),digits =2 ))
meanDataAreaW000_G4_2000$SD<- round(sd(DataAreaW000_G4_2000$Area.mm2),digits =2 )
meanDataAreaW000_G4_2000$Weight <- "2000g"

meanDataAreaW001_G4_100<- data.frame(meanArea=round(mean(DataAreaW001_G4_100$Area.mm2),digits =2 ))
meanDataAreaW001_G4_100$SD<- round(sd(DataAreaW001_G4_100$Area.mm2),digits =2 )
meanDataAreaW001_G4_100$Weight <- "100g"
meanDataAreaW001_G4_200<- data.frame(meanArea=round(mean(DataAreaW001_G4_200$Area.mm2),digits =2 ))
meanDataAreaW001_G4_200$SD<- round(sd(DataAreaW001_G4_200$Area.mm2),digits =2 )
meanDataAreaW001_G4_200$Weight <- "200g"
meanDataAreaW001_G4_400<- data.frame(meanArea=round(mean(DataAreaW001_G4_400$Area.mm2),digits =2 ))
meanDataAreaW001_G4_400$SD<- round(sd(DataAreaW001_G4_400$Area.mm2),digits =2 )
meanDataAreaW001_G4_400$Weight <- "400g"
meanDataAreaW001_G4_800<- data.frame(meanArea=round(mean(DataAreaW001_G4_800$Area.mm2),digits =2 ))
meanDataAreaW001_G4_800$SD<- round(sd(DataAreaW001_G4_800$Area.mm2),digits =2 )
meanDataAreaW001_G4_800$Weight <- "800g"
meanDataAreaW001_G4_1000<- data.frame(meanArea=round(mean(DataAreaW001_G4_1000$Area.mm2),digits =2 ))
meanDataAreaW001_G4_1000$SD<- round(sd(DataAreaW001_G4_1000$Area.mm2),digits =2 )
meanDataAreaW001_G4_1000$Weight <- "1000g"
meanDataAreaW001_G4_2000<- data.frame(meanArea=round(mean(DataAreaW001_G4_2000$Area.mm2),digits =2 ))
meanDataAreaW001_G4_2000$SD<- round(sd(DataAreaW001_G4_2000$Area.mm2),digits =2 )
meanDataAreaW001_G4_2000$Weight <- "2000g"

meanDataAreaW003_G4_100<- data.frame(meanArea=round(mean(DataAreaW003_G4_100$Area.mm2),digits =2 ))
meanDataAreaW003_G4_100$SD<- round(sd(DataAreaW003_G4_100$Area.mm2),digits =2 )
meanDataAreaW003_G4_100$Weight <- "100g"
meanDataAreaW003_G4_200<- data.frame(meanArea=round(mean(DataAreaW003_G4_200$Area.mm2),digits =2 ))
meanDataAreaW003_G4_200$SD<- round(sd(DataAreaW003_G4_200$Area.mm2),digits =2 )
meanDataAreaW003_G4_200$Weight <- "200g"
meanDataAreaW003_G4_400<- data.frame(meanArea=round(mean(DataAreaW003_G4_400$Area.mm2),digits =2 ))
meanDataAreaW003_G4_400$SD<- round(sd(DataAreaW003_G4_400$Area.mm2),digits =2 )
meanDataAreaW003_G4_400$Weight <- "400g"
meanDataAreaW003_G4_800<- data.frame(meanArea=round(mean(DataAreaW003_G4_800$Area.mm2),digits =2 ))
meanDataAreaW003_G4_800$SD<- round(sd(DataAreaW003_G4_800$Area.mm2),digits =2 )
meanDataAreaW003_G4_800$Weight <- "800g"
meanDataAreaW003_G4_1000<- data.frame(meanArea=round(mean(DataAreaW003_G4_1000$Area.mm2),digits =2 ))
meanDataAreaW003_G4_1000$SD<- round(sd(DataAreaW003_G4_1000$Area.mm2),digits =2 )
meanDataAreaW003_G4_1000$Weight <- "1000g"
meanDataAreaW003_G4_2000<- data.frame(meanArea=round(mean(DataAreaW003_G4_2000$Area.mm2),digits =2 ))
meanDataAreaW003_G4_2000$SD<- round(sd(DataAreaW003_G4_2000$Area.mm2),digits =2 )
meanDataAreaW003_G4_2000$Weight <- "2000g"

meanDataAreaW005_G4_100<- data.frame(meanArea=round(mean(DataAreaW005_G4_100$Area.mm2),digits =2 ))
meanDataAreaW005_G4_100$SD<- round(sd(DataAreaW005_G4_100$Area.mm2),digits =2 )
meanDataAreaW005_G4_100$Weight <- "100g"
meanDataAreaW005_G4_200<- data.frame(meanArea=round(mean(DataAreaW005_G4_200$Area.mm2),digits =2 ))
meanDataAreaW005_G4_200$SD<- round(sd(DataAreaW005_G4_200$Area.mm2),digits =2 )
meanDataAreaW005_G4_200$Weight <- "200g"
meanDataAreaW005_G4_400<- data.frame(meanArea=round(mean(DataAreaW005_G4_400$Area.mm2),digits =2 ))
meanDataAreaW005_G4_400$SD<- round(sd(DataAreaW005_G4_400$Area.mm2),digits =2 )
meanDataAreaW005_G4_400$Weight <- "400g"
meanDataAreaW005_G4_800<- data.frame(meanArea=round(mean(DataAreaW005_G4_800$Area.mm2),digits =2 ))
meanDataAreaW005_G4_800$SD<- round(sd(DataAreaW005_G4_800$Area.mm2),digits =2 )
meanDataAreaW005_G4_800$Weight <- "800g"
meanDataAreaW005_G4_1000<- data.frame(meanArea=round(mean(DataAreaW005_G4_1000$Area.mm2),digits =2 ))
meanDataAreaW005_G4_1000$SD<- round(sd(DataAreaW005_G4_1000$Area.mm2),digits =2 )
meanDataAreaW005_G4_1000$Weight <- "1000g"
meanDataAreaW005_G4_2000<- data.frame(meanArea=round(mean(DataAreaW005_G4_2000$Area.mm2),digits =2 ))
meanDataAreaW005_G4_2000$SD<- round(sd(DataAreaW005_G4_2000$Area.mm2),digits =2 )
meanDataAreaW005_G4_2000$Weight <- "2000g"

meanDataAreaW007_G4_100<- data.frame(meanArea=round(mean(DataAreaW007_G4_100$Area.mm2),digits =2 ))
meanDataAreaW007_G4_100$SD<- round(sd(DataAreaW007_G4_100$Area.mm2),digits =2 )
meanDataAreaW007_G4_100$Weight <- "100g"
meanDataAreaW007_G4_200<- data.frame(meanArea=round(mean(DataAreaW007_G4_200$Area.mm2),digits =2 ))
meanDataAreaW007_G4_200$SD<- round(sd(DataAreaW007_G4_200$Area.mm2),digits =2 )
meanDataAreaW007_G4_200$Weight <- "200g"
meanDataAreaW007_G4_400<- data.frame(meanArea=round(mean(DataAreaW007_G4_400$Area.mm2),digits =2 ))
meanDataAreaW007_G4_400$SD<- round(sd(DataAreaW007_G4_400$Area.mm2),digits =2 )
meanDataAreaW007_G4_400$Weight <- "400g"
meanDataAreaW007_G4_800<- data.frame(meanArea=round(mean(DataAreaW007_G4_800$Area.mm2),digits =2 ))
meanDataAreaW007_G4_800$SD<- round(sd(DataAreaW007_G4_800$Area.mm2),digits =2 )
meanDataAreaW007_G4_800$Weight <- "800g"
meanDataAreaW007_G4_1000<- data.frame(meanArea=round(mean(DataAreaW007_G4_1000$Area.mm2),digits =2 ))
meanDataAreaW007_G4_1000$SD<- round(sd(DataAreaW007_G4_1000$Area.mm2),digits =2 )
meanDataAreaW007_G4_1000$Weight <- "1000g"
meanDataAreaW007_G4_2000<- data.frame(meanArea=round(mean(DataAreaW007_G4_2000$Area.mm2),digits =2 ))
meanDataAreaW007_G4_2000$SD<- round(sd(DataAreaW007_G4_2000$Area.mm2),digits =2 )
meanDataAreaW007_G4_2000$Weight <- "2000g"

meanDataAreaW009_G4_100<- data.frame(meanArea=round(mean(DataAreaW009_G4_100$Area.mm2),digits =2 ))
meanDataAreaW009_G4_100$SD<- round(sd(DataAreaW009_G4_100$Area.mm2),digits =2 )
meanDataAreaW009_G4_100$Weight <- "100g"
meanDataAreaW009_G4_200<- data.frame(meanArea=round(mean(DataAreaW009_G4_200$Area.mm2),digits =2 ))
meanDataAreaW009_G4_200$SD<- round(sd(DataAreaW009_G4_200$Area.mm2),digits =2 )
meanDataAreaW009_G4_200$Weight <- "200g"
meanDataAreaW009_G4_400<- data.frame(meanArea=round(mean(DataAreaW009_G4_400$Area.mm2),digits =2 ))
meanDataAreaW009_G4_400$SD<- round(sd(DataAreaW009_G4_400$Area.mm2),digits =2 )
meanDataAreaW009_G4_400$Weight <- "400g"
meanDataAreaW009_G4_800<- data.frame(meanArea=round(mean(DataAreaW009_G4_800$Area.mm2),digits =2 ))
meanDataAreaW009_G4_800$SD<- round(sd(DataAreaW009_G4_800$Area.mm2),digits =2 )
meanDataAreaW009_G4_800$Weight <- "800g"
meanDataAreaW009_G4_1000<- data.frame(meanArea=round(mean(DataAreaW009_G4_1000$Area.mm2),digits =2 ))
meanDataAreaW009_G4_1000$SD<- round(sd(DataAreaW009_G4_1000$Area.mm2),digits =2 )
meanDataAreaW009_G4_1000$Weight <- "1000g"
meanDataAreaW009_G4_2000<- data.frame(meanArea=round(mean(DataAreaW009_G4_2000$Area.mm2),digits =2 ))
meanDataAreaW009_G4_2000$SD<- round(sd(DataAreaW009_G4_2000$Area.mm2),digits =2 )
meanDataAreaW009_G4_2000$Weight <- "2000g"

meanDataAreaW011_G4_100<- data.frame(meanArea=round(mean(DataAreaW011_G4_100$Area.mm2),digits =2 ))
meanDataAreaW011_G4_100$SD<- round(sd(DataAreaW011_G4_100$Area.mm2),digits =2 )
meanDataAreaW011_G4_100$Weight <- "100g"
meanDataAreaW011_G4_200<- data.frame(meanArea=round(mean(DataAreaW011_G4_200$Area.mm2),digits =2 ))
meanDataAreaW011_G4_200$SD<- round(sd(DataAreaW011_G4_200$Area.mm2),digits =2 )
meanDataAreaW011_G4_200$Weight <- "200g"
meanDataAreaW011_G4_400<- data.frame(meanArea=round(mean(DataAreaW011_G4_400$Area.mm2),digits =2 ))
meanDataAreaW011_G4_400$SD<- round(sd(DataAreaW011_G4_400$Area.mm2),digits =2 )
meanDataAreaW011_G4_400$Weight <- "400g"
meanDataAreaW011_G4_800<- data.frame(meanArea=round(mean(DataAreaW011_G4_800$Area.mm2),digits =2 ))
meanDataAreaW011_G4_800$SD<- round(sd(DataAreaW011_G4_800$Area.mm2),digits =2 )
meanDataAreaW011_G4_800$Weight <- "800g"
meanDataAreaW011_G4_1000<- data.frame(meanArea=round(mean(DataAreaW011_G4_1000$Area.mm2),digits =2 ))
meanDataAreaW011_G4_1000$SD<- round(sd(DataAreaW011_G4_1000$Area.mm2),digits =2 )
meanDataAreaW011_G4_1000$Weight <- "1000g"
meanDataAreaW011_G4_2000<- data.frame(meanArea=round(mean(DataAreaW011_G4_2000$Area.mm2),digits =2 ))
meanDataAreaW011_G4_2000$SD<- round(sd(DataAreaW011_G4_2000$Area.mm2),digits =2 )
meanDataAreaW011_G4_2000$Weight <- "2000g"

# Combined data sets
DataW000_G4_total <- rbind(meanDataAreaW000_G4_100,meanDataAreaW000_G4_200,meanDataAreaW000_G4_400,
                           meanDataAreaW000_G4_800,meanDataAreaW000_G4_1000,meanDataAreaW000_G4_2000)
DataW000_G4_total$Condition <- "W000_G4"

DataW001_G4_total <- rbind(meanDataAreaW001_G4_100,meanDataAreaW001_G4_200,meanDataAreaW001_G4_400,
                           meanDataAreaW001_G4_800,meanDataAreaW001_G4_1000,meanDataAreaW001_G4_2000)
DataW001_G4_total$Condition <- "W001_G4"

DataW003_G4_total <- rbind(meanDataAreaW003_G4_100,meanDataAreaW003_G4_200,meanDataAreaW003_G4_400,
                           meanDataAreaW003_G4_800,meanDataAreaW003_G4_1000,meanDataAreaW003_G4_2000)
DataW003_G4_total$Condition <- "W003_G4"

DataW005_G4_total <- rbind(meanDataAreaW005_G4_100,meanDataAreaW005_G4_200,meanDataAreaW005_G4_400,
                           meanDataAreaW005_G4_800,meanDataAreaW005_G4_1000,meanDataAreaW005_G4_2000)
DataW005_G4_total$Condition <- "W005_G4"

DataW007_G4_total <- rbind(meanDataAreaW007_G4_100,meanDataAreaW007_G4_200,meanDataAreaW007_G4_400,
                           meanDataAreaW007_G4_800,meanDataAreaW007_G4_1000,meanDataAreaW007_G4_2000)
DataW007_G4_total$Condition <- "W007_G4"

DataW009_G4_total <- rbind(meanDataAreaW009_G4_100,meanDataAreaW009_G4_200,meanDataAreaW009_G4_400,
                           meanDataAreaW009_G4_800,meanDataAreaW009_G4_1000,meanDataAreaW009_G4_2000)
DataW009_G4_total$Condition <- "W009_G4"

DataW011_G4_total <- rbind(meanDataAreaW011_G4_100,meanDataAreaW011_G4_200,meanDataAreaW011_G4_400,
                           meanDataAreaW011_G4_800,meanDataAreaW011_G4_1000,meanDataAreaW011_G4_2000)
DataW011_G4_total$Condition <- "W011_G4"

FibreCount_Shedding_G4 <- rbind(DataW000_G4_total,DataW001_G4_total,DataW003_G4_total,DataW005_G4_total,DataW007_G4_total,DataW009_G4_total,DataW011_G4_total)

write.table(FibreCount_Shedding_G4, file = "Shedding_Fibre_Count.csv", quote = F, sep = ",", row.names = F)

# calculation of the percentage difference between washed and unwashed
y = rep(c(125, 150, 175, 200, 225),2)
x = rep(c(1:5), 2)

# plot
pSH_G4 <- ggplot(FibreCount_Shedding_G4, aes(x = factor(Weight, level = c('100g', '200g', '400g','800g','1000g','2000g')),
                                             y= meanArea, fill=Condition))+
  geom_bar(stat="identity", position=position_dodge(),colour="black")+
  geom_text(aes(label = meanArea), hjust=0.5,vjust = -5.5,position = position_dodge(.9))+
  labs(x="\nWeight", y="Total fibre area (mm\u00b2)\n") +
  theme_bw(base_family = "Arial", base_size = 12) +
  ylim(0,520)+
  scale_fill_manual(values = brewer.pal(9, "YlOrBr")[1:9])+
  theme(legend.title = element_blank(),
        strip.background.x = element_rect(colour = "grey", fill = "white"),
        legend.position = "bottom",
        legend.background = element_rect(fill="grey95",size=1, linetype="solid", colour="grey80"),
        axis.text.x = element_text(angle = 0, vjust = 0.95, hjust=0.5))+
  geom_errorbar(aes(ymin=meanArea-SD, ymax=meanArea+SD),width=.2,position=position_dodge(.9))
pSH_G4
ggsave("Shedding_G4_W000-11.png", pSH_G4, width = 10, height = 9, units = "in", dpi=150, path = "Results")

# plot - no label
pSH_G4 <- ggplot(FibreCount_Shedding_G4, aes(x = factor(Weight, level = c('100g', '200g', '400g','800g','1000g','2000g')),
                                             y= meanArea, fill=Condition))+
  geom_bar(stat="identity", position=position_dodge(),colour="black")+
  labs(x="\nWeight", y="Total fibre area (mm\u00b2)\n") +
  theme_bw(base_family = "Arial", base_size = 12) +
  ylim(0,540)+
  scale_fill_manual(values = brewer.pal(9, "YlOrBr")[1:9])+
  theme(legend.title = element_blank(),
        strip.background.x = element_rect(colour = "grey", fill = "white"),
        legend.position = "bottom",
        legend.background = element_rect(fill="grey95",size=1, linetype="solid", colour="grey80"),
        axis.text.x = element_text(angle = 0, vjust = 0.95, hjust=0.5))+
  geom_errorbar(aes(ymin=meanArea-SD, ymax=meanArea+SD),width=.2,position=position_dodge(.9))
pSH_G4
ggsave("Shedding_G4_W000-11_No Mean Label.png", pSH_G4, width = 10, height = 9, units = "in", dpi=150, path = "Results")


#######################
####   COMBINED    ####
#######################

FibreCount_Shedding_Combined <- rbind(DataW000_G1_total,DataW000_G2_total,DataW000_G3_total,DataW000_G4A_total,DataW000_G4B_total)
FibreCount_Shedding_allGarmentCombined <- aggregate(FibreCount_Shedding_Combined$meanArea,list(FibreCount_Shedding_Combined$Weight), FUN=mean)
names(FibreCount_Shedding_allGarmentCombined) <- c("Weight","meanArea")

#### PLOT ####
# calculation of the percentage difference between washed and unwashed
y = rep(c(125, 150, 175, 200, 225),2)
x = rep(c(1:5), 2)

# plot
pSH <- ggplot(FibreCount_Shedding_Combined, aes(x = factor(Weight, level = c('100g', '200g', '400g','800g','1000g','2000g')),
                                                y= meanArea, fill=Condition))+
  geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label = meanArea), hjust=0.5,vjust = -5.5,position = position_dodge(.9))+
  labs(x="\nWeight", y="Total fibre area (mm\u00b2)\n") +
  theme_bw(base_family = "Arial", base_size = 12) +
  ylim(0,500)+
  scale_fill_manual("legend", values = c("W000_G1" = "#A6CEE3", "W000_G2" = "#1F78B4","W000_G3" = "#B2DF8A",
                                         "W000_G4A" = "#33A02C","W000_G4B" = "#FB9A99"))+ # to obtain the colour brewer.pal(12, "Paired")
  theme(legend.title = element_blank(),
        strip.background.x = element_rect(colour = "grey", fill = "white"),
        legend.position = "bottom",
        legend.background = element_rect(fill="grey95",size=1, linetype="solid", colour="grey80"),
        axis.text.x = element_text(angle = 0, vjust = 0.95, hjust=0.5))+
  geom_errorbar(aes(ymin=meanArea-SD, ymax=meanArea+SD),width=.2,position=position_dodge(.9))
pSH
ggsave("Shedding_W000_all garment.png", pSH, width = 10, height = 9, units = "in", dpi=150, path = "Results")

pSH <- ggplot(FibreCount_Shedding_allGarmentCombined, aes(x = factor(Weight, level = c('100g', '200g', '400g','800g','1000g','2000g')),
                                                y= meanArea))+
  geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label = meanArea), hjust=0.5,vjust = -5.5,position = position_dodge(.9))+
  labs(x="\nWeight", y="Total fibre area (mm\u00b2)\n") +
  theme_bw(base_family = "Arial", base_size = 12) +
  ylim(0,500)+
  theme(legend.title = element_blank(),
        strip.background.x = element_rect(colour = "dark", fill = "white"),
        legend.position = "bottom",
        legend.background = element_rect(fill="grey95",size=1, linetype="solid", colour="grey80"),
        axis.text.x = element_text(angle = 0, vjust = 0.95, hjust=0.5))
pSH
ggsave("Shedding_W000_all garment combined.png", pSH, width = 10, height = 9, units = "in", dpi=150, path = "Results")
