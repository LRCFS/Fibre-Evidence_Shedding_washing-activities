##########################################################################
#####                           Data upload                          #####
##########################################################################

# This R script is the first step to export all the data needed to generate the figure in the article.
# This includes the data from: 
# 1. Impact test, Repeated Contact and Hand pressure
# 2. Shedding data from washed garments
# 3. Transfer experiments 
  # 3-1. First Series - 1 v-cotton garment, no detergent, no softener
  # 3-2. Second Series - 1 v-cotton garment, detergent, no softener
  # 3-3. Third Series - 1 v-cotton garment, detergent, softener
  # 3-4. Fourth Series - 5 v-cotton garments, no detergent, no softener
  # 3-5. Fifth Series - 12 garments, no detergent, no softener
  # 3-6. Sixth Series - 1 r-cotton textile, no detergent, no softener


# ------------------------------------------------------------------------
# Section 1: Impact test, Repeated Contact and Hand pressure
# ------------------------------------------------------------------------
# Read the CSV file containing garment weight data
ImpactTest <- read.csv('./Data/Impact-test_Summary.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')
ImpactTest$Slice<- gsub(".TIF","",ImpactTest$Slice)
ImpactTest_Extended <- data.frame(str_split(ImpactTest$Slice, "_", simplify=TRUE))
names(ImpactTest_Extended) <- c("Project","Experiment","Garment","Height","Repeat")
ImpactTest <- cbind(ImpactTest_Extended,Area=ImpactTest$Total.Area)
rm(ImpactTest_Extended)

RepeatedContact<- read.csv('./Data/Repeated-contact_Summary.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')
RepeatedContact$Slice<- gsub(".TIF","",RepeatedContact$Slice)
RepeatedContact_Extended <- data.frame(str_split(RepeatedContact$Slice, "_", simplify=TRUE))
RepeatedContact <- data.frame(cbind(Contact.Area=RepeatedContact_Extended$X4,Repeat=RepeatedContact_Extended$X5,Area=RepeatedContact$Total.Area))
RepeatedContact$Area <- as.numeric(RepeatedContact$Area)
rm(RepeatedContact_Extended)

Handpressure<- read.csv('./Data/Hand-pressure_Summary.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')
Handpressure$Slice<- gsub(".TIF","",Handpressure$Slice)
Handpressure_Extended <- data.frame(str_split(Handpressure$Slice, "_", simplify=TRUE))
Handpressure <- data.frame(cbind(Operator=Handpressure_Extended$X2,Pressure=Handpressure_Extended$X3,Garment=Handpressure_Extended$X4,Repeat=Handpressure_Extended$X5,Sample=Handpressure_Extended$X6,Area=Handpressure$Total.Area))
Handpressure$Area <- as.numeric(Handpressure$Area)
rm(Handpressure_Extended)

# ------------------------------------------------------------------------
# Section 2: Shedding data from washed garments
# ------------------------------------------------------------------------
# Read the CSV file containing garment weight data
Shedding_Vcotton <- read.csv('./Data/SH_1Vcotton_Summary.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')
Shedding_Vcotton$Slice<- gsub(".TIF","",Shedding_Vcotton$Slice)
Shedding_Vcotton_Extended <- data.frame(str_split(Shedding_Vcotton$Slice, "_", simplify=TRUE))
names(Shedding_Vcotton_Extended) <- c("Project","Wash","Garment","Weight","Repeat")
Shedding_Vcotton <- cbind(Shedding_Vcotton_Extended,Area=Shedding_Vcotton$Total.Area)
rm(Shedding_Vcotton_Extended)

Shedding_VcottonD <- read.csv('./Data/SH_1VcottonD_Summary.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')
Shedding_VcottonD$Slice<- gsub(".TIF","",Shedding_VcottonD$Slice)
Shedding_VcottonD_Extended <- data.frame(str_split(Shedding_VcottonD$Slice, "_", simplify=TRUE))
names(Shedding_VcottonD_Extended) <- c("Project","Wash","Garment","Weight","Repeat")
Shedding_VcottonD <- cbind(Shedding_VcottonD_Extended,Area=Shedding_VcottonD$Total.Area)
rm(Shedding_VcottonD_Extended)

Shedding_VcottonDC <- read.csv('./Data/SH_1VcottonDC_Summary.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')
Shedding_VcottonDC$Slice<- gsub(".TIF","",Shedding_VcottonDC$Slice)
Shedding_VcottonDC_Extended <- data.frame(str_split(Shedding_VcottonDC$Slice, "_", simplify=TRUE))
names(Shedding_VcottonDC_Extended) <- c("Project","Wash","Garment","Weight","Repeat")
Shedding_VcottonDC <- cbind(Shedding_VcottonDC_Extended,Area=Shedding_VcottonDC$Total.Area)
rm(Shedding_VcottonDC_Extended)

Shedding_5_Vcotton <- read.csv('./Data/SH_5Vcotton2nd_Summary.csv', sep="," ,header = T,fileEncoding="UTF-8-BOM")
Shedding_5_Vcotton$Slice<- gsub(".TIF","",Shedding_5_Vcotton$Slice)
Shedding_5_Vcotton_Extended <- data.frame(str_split(Shedding_5_Vcotton$Slice, "_", simplify=TRUE))
names(Shedding_5_Vcotton_Extended) <- c("Project","Wash","Garment","Weight","Repeat")
Shedding_5_Vcotton <- cbind(Shedding_5_Vcotton_Extended,Area=Shedding_5_Vcotton$Total.Area)
rm(Shedding_5_Vcotton_Extended)

Shedding_12_Vcotton <- read.csv('./Data/SH_12Vcotton_Summary.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')
Shedding_12_Vcotton$Slice<- gsub(".TIF","",Shedding_12_Vcotton$Slice)
Shedding_12_Vcotton_Extended <- data.frame(str_split(Shedding_12_Vcotton$Slice, "_", simplify=TRUE))
names(Shedding_12_Vcotton_Extended) <- c("Project","Wash","Garment","Weight","Repeat")
Shedding_12_Vcotton <- cbind(Shedding_12_Vcotton_Extended,Area=Shedding_12_Vcotton$Total.Area)
rm(Shedding_12_Vcotton_Extended)

Shedding_Rcotton <- read.csv('./Data/SH_Rcotton2nd_Summary.csv', sep="," ,header = T,fileEncoding="UTF-8-BOM")
Shedding_Rcotton$Slice<- gsub(".TIF","",Shedding_Rcotton$Slice)
Shedding_Rcotton_Extended <- data.frame(str_split(Shedding_Rcotton$Slice, "_", simplify=TRUE))
names(Shedding_Rcotton_Extended) <- c("Project","Wash","Garment","Weight","Repeat")
Shedding_Rcotton <- cbind(Shedding_Rcotton_Extended,Area=Shedding_Rcotton$Total.Area)
rm(Shedding_Rcotton_Extended)

# ------------------------------------------------------------------------
# Section 3: Transfer experiments
# ------------------------------------------------------------------------
# ------------------------------------------------------------------------
# Subsection 3-1 : First Series - 1 v-cotton garment, no detergent, no softener
# ------------------------------------------------------------------------
Vcotton <- read.csv('./Data/Transfer_1stSeries_Summary.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')

# Remove ".TIF" in Vcotton
Vcotton$Slice<- gsub(".TIF","",Vcotton$Slice)

# Remove "_negative" and "_positive" in Vcotton
text_to_remove <- "_negative"
Vcotton <- Vcotton[!grepl(text_to_remove, Vcotton$Slice), ]
text_to_remove <- "_positive"
Vcotton <- Vcotton[!grepl(text_to_remove, Vcotton$Slice), ]

# Create different dataframes before and after transfer
VcottonB<- Vcotton %>% filter(grepl('_B', Slice))
VcottonAtr <- Vcotton %>% filter(grepl('_Atr', Slice))

# Create table
Vcotton_Dataset_pending <- data.frame(rbind(VcottonB$Slice, VcottonB$Count,VcottonAtr$Count,VcottonB$Total.Area, VcottonAtr$Total.Area))
Vcotton_Dataset<-as.data.frame(t(Vcotton_Dataset_pending))

names(Vcotton_Dataset) <- c("Sample", "Before transfer count", "After transfer count","Before transfer area", "After transfer area")

# Convert factor to numeric in columns 2 to 3
Vcotton_Dataset[,2:5] = apply(Vcotton_Dataset[,2:5], 2, function(x) as.numeric(as.character(x)));

# Remove unused dataframes
rm(Vcotton,VcottonB,VcottonAtr,Vcotton_Dataset_pending)

# ------------------------------------------------------------------------
# Subsection 3-2 : Second Series - 1 v-cotton garment, detergent, no softener
# ------------------------------------------------------------------------
VcottonD <- read.csv('./Data/Transfer_2ndSeries_Summary.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')

# Remove ".TIF" in VcottonD
VcottonD$Slice<- gsub(".TIF","",VcottonD$Slice)

# Remove "_negative" and "_positive" in VcottonD
text_to_remove <- "_negative"
VcottonD <- VcottonD[!grepl(text_to_remove, VcottonD$Slice), ]
text_to_remove <- "_positive"
VcottonD <- VcottonD[!grepl(text_to_remove, VcottonD$Slice), ]

# Create different dataframes before and after transfer
VcottonDB<- VcottonD %>% filter(grepl('_B', Slice))
VcottonDAtr <- VcottonD %>% filter(grepl('_Atr', Slice))

# Create table
VcottonD_Dataset_pending <- data.frame(rbind(VcottonDB$Slice, VcottonDB$Count,VcottonDAtr$Count,VcottonDB$Total.Area, VcottonDAtr$Total.Area))
VcottonD_Dataset<-as.data.frame(t(VcottonD_Dataset_pending))

names(VcottonD_Dataset) <- c("Sample", "Before transfer count", "After transfer count","Before transfer area", "After transfer area")

# Convert factor to numeric in columns 2 to 3
VcottonD_Dataset[,2:5] = apply(VcottonD_Dataset[,2:5], 2, function(x) as.numeric(as.character(x)));

# Remove unused dataframes
rm(VcottonD,VcottonDB,VcottonDAtr,VcottonD_Dataset_pending)

# ------------------------------------------------------------------------
# Subsection 3-3 : Third Series - 1 v-cotton garment, detergent, softener
# ------------------------------------------------------------------------
VcottonDC <- read.csv('./Data/Transfer_3rdSeries_Summary.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')

# Remove ".TIF" in VcottonDC
VcottonDC$Slice<- gsub(".TIF","",VcottonDC$Slice)

# Remove "_negative" and "_positive" in VcottonDC
text_to_remove <- "_negative"
VcottonDC <- VcottonDC[!grepl(text_to_remove, VcottonDC$Slice), ]
text_to_remove <- "_positive"
VcottonDC <- VcottonDC[!grepl(text_to_remove, VcottonDC$Slice), ]

# Create different dataframes before and after transfer
VcottonDCB<- VcottonDC %>% filter(grepl('_B', Slice))
VcottonDCAtr <- VcottonDC %>% filter(grepl('_Atr', Slice))

# Create table
VcottonDC_Dataset_pending <- data.frame(rbind(VcottonDCB$Slice, VcottonDCB$Count,VcottonDCAtr$Count,VcottonDCB$Total.Area, VcottonDCAtr$Total.Area))
VcottonDC_Dataset<-as.data.frame(t(VcottonDC_Dataset_pending))

names(VcottonDC_Dataset) <- c("Sample", "Before transfer count", "After transfer count","Before transfer area", "After transfer area")

# Convert factor to numeric in columns 2 to 3
VcottonDC_Dataset[,2:5] = apply(VcottonDC_Dataset[,2:5], 2, function(x) as.numeric(as.character(x)));

# Remove unused dataframes
rm(VcottonDC,VcottonDCB,VcottonDCAtr,VcottonDC_Dataset_pending)

# ------------------------------------------------------------------------
# Subsection 3-4 : Fourth Series - 5 v-cotton garments, no detergent, no softener
# ------------------------------------------------------------------------
Vcotton5 <- read.csv('./Data/Transfer_5Vcotton_Summary.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')

# Remove ".TIF" in Vcotton5
Vcotton5$Slice<- gsub(".TIF","",Vcotton5$Slice)

# Remove "_negative" and "_positive" in Vcotton5
text_to_remove <- "_negative"
Vcotton5 <- Vcotton5[!grepl(text_to_remove, Vcotton5$Slice), ]
text_to_remove <- "_positive"
Vcotton5 <- Vcotton5[!grepl(text_to_remove, Vcotton5$Slice), ]

# Create different dataframes before and after transfer
Vcotton5B<- Vcotton5 %>% filter(grepl('_B', Slice))
Vcotton5Atr <- Vcotton5 %>% filter(grepl('_Atr', Slice))

# Create table
Vcotton5_Dataset_pending <- data.frame(rbind(Vcotton5B$Slice, Vcotton5B$Count,Vcotton5Atr$Count,Vcotton5B$Total.Area, Vcotton5Atr$Total.Area))
Vcotton5_Dataset<-as.data.frame(t(Vcotton5_Dataset_pending))

names(Vcotton5_Dataset) <- c("Sample", "Before transfer count", "After transfer count","Before transfer area", "After transfer area")

# Convert factor to numeric in columns 2 to 3
Vcotton5_Dataset[,2:5] = apply(Vcotton5_Dataset[,2:5], 2, function(x) as.numeric(as.character(x)));

# Remove unused dataframes
rm(Vcotton5,Vcotton5B,Vcotton5Atr,Vcotton5_Dataset_pending)

# ------------------------------------------------------------------------
# Subsection 3-5 : Fifth Series - 12 garments, no detergent, no softener
# ------------------------------------------------------------------------
Vcotton12 <- read.csv('./Data/Transfer_12Vcotton_Summary.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')

# Remove ".TIF" in Vcotton12
Vcotton12$Slice<- gsub(".TIF","",Vcotton12$Slice)

# Remove "_negative" and "_positive" in Vcotton12
text_to_remove <- "_negative"
Vcotton12 <- Vcotton12[!grepl(text_to_remove, Vcotton12$Slice), ]
text_to_remove <- "_positive"
Vcotton12 <- Vcotton12[!grepl(text_to_remove, Vcotton12$Slice), ]

# Create different dataframes before and after transfer
Vcotton12B<- Vcotton12 %>% filter(grepl('_B', Slice))
Vcotton12Atr <- Vcotton12 %>% filter(grepl('_Atr', Slice))

# Create table
Vcotton12_Dataset_pending <- data.frame(rbind(Vcotton12B$Slice, Vcotton12B$Count,Vcotton12Atr$Count,Vcotton12B$Total.Area, Vcotton12Atr$Total.Area))
Vcotton12_Dataset<-as.data.frame(t(Vcotton12_Dataset_pending))

names(Vcotton12_Dataset) <- c("Sample", "Before transfer count", "After transfer count","Before transfer area", "After transfer area")

# Convert factor to numeric in columns 2 to 3
Vcotton12_Dataset[,2:5] = apply(Vcotton12_Dataset[,2:5], 2, function(x) as.numeric(as.character(x)));

# Remove unused dataframes
rm(Vcotton12,Vcotton12B,Vcotton12Atr,Vcotton12_Dataset_pending)

# ------------------------------------------------------------------------
# Subsection 3-6 : Sixth Series - 1 r-cotton textile, no detergent, no softener
# ------------------------------------------------------------------------
Rcotton <- read.csv('./Data/Transfer_Rcotton_Summary.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')

# Remove ".TIF" in Rcotton
Rcotton$Slice<- gsub(".TIF","",Rcotton$Slice)

# Remove "_negative" and "_positive" in Rcotton
text_to_remove <- "_negative"
Rcotton <- Rcotton[!grepl(text_to_remove, Rcotton$Slice), ]
text_to_remove <- "_positive"
Rcotton <- Rcotton[!grepl(text_to_remove, Rcotton$Slice), ]

# Create different dataframes before and after transfer
RcottonB<- Rcotton %>% filter(grepl('_B', Slice))
RcottonAtr <- Rcotton %>% filter(grepl('_Atr', Slice))

# Create table
Rcotton_Dataset_pending <- data.frame(rbind(RcottonB$Slice, RcottonB$Count,RcottonAtr$Count,RcottonB$Total.Area, RcottonAtr$Total.Area))
Rcotton_Dataset<-as.data.frame(t(Rcotton_Dataset_pending))

names(Rcotton_Dataset) <- c("Sample", "Before transfer count", "After transfer count","Before transfer area", "After transfer area")

# Convert factor to numeric in columns 2 to 3
Rcotton_Dataset[,2:5] = apply(Rcotton_Dataset[,2:5], 2, function(x) as.numeric(as.character(x)));

# Remove unused dataframes
rm(Rcotton,RcottonB,RcottonAtr,Rcotton_Dataset_pending)

