################################################
######       COMPARISON OF WEIGHTS        ######
################################################

# W000_G1 Kruskal-Wallis Test
kruskal.test(Area.mm2 ~ Weight, data = DataAreaW000_G1)

# W000_G1 Dunn's Test with Bonferroni correction for p-values
G1_W000_Weight_Dunn <- dunnTest(Area.mm2 ~ Weight, data=DataAreaW000_G1, method="bonferroni")
write.table(G1_W000_Weight_Dunn$res, file = "Statistics/G1_W000_Weight_Dunn Test.csv", quote = F, sep = ",", row.names = F)

# W001_G1 Kruskal-Wallis Test
kruskal.test(Area.mm2 ~ Weight, data = DataAreaW001_G1)

# W001_G1 Dunn's Test with Bonferroni correction for p-values
G1_W001_Weight_Dunn <- dunnTest(Area.mm2 ~ Weight, data=DataAreaW001_G1, method="bonferroni")
write.table(G1_W001_Weight_Dunn$res, file = "Statistics/G1_W001_Weight_Dunn Test.csv", quote = F, sep = ",", row.names = F)

# W007_G1 Kruskal-Wallis Test
kruskal.test(Area.mm2 ~ Weight, data = DataAreaW007_G1)

# W007_G1 Dunn's Test with Bonferroni correction for p-values
G1_W007_Weight_Dunn <- dunnTest(Area.mm2 ~ Weight, data=DataAreaW007_G1, method="bonferroni")
write.table(G1_W007_Weight_Dunn$res, file = "Statistics/G1_W007_Weight_Dunn Test.csv", quote = F, sep = ",", row.names = F)

# W015_G1 Kruskal-Wallis Test
kruskal.test(Area.mm2 ~ Weight, data = DataAreaW015_G1)

# W015_G1 Dunn's Test with Bonferroni correction for p-values
G1_W015_Weight_Dunn <- dunnTest(Area.mm2 ~ Weight, data=DataAreaW015_G1, method="bonferroni")
write.table(G1_W015_Weight_Dunn$res, file = "Statistics/G1_W015_Weight_Dunn Test.csv", quote = F, sep = ",", row.names = F)


# W000_G2 Kruskal-Wallis Test
kruskal.test(Area.mm2 ~ Weight, data = DataAreaW000_G2)

# W000_G2 Dunn's Test with Bonferroni correction for p-values
G2_W000_Weight_Dunn <- dunnTest(Area.mm2 ~ Weight, data=DataAreaW000_G2, method="bonferroni")
write.table(G2_W000_Weight_Dunn$res, file = "Statistics/G2_W000_Weight_Dunn Test.csv", quote = F, sep = ",", row.names = F)

# W001_G2 Kruskal-Wallis Test
kruskal.test(Area.mm2 ~ Weight, data = DataAreaW001_G2)

# W001_G2 Dunn's Test with Bonferroni correction for p-values
G2_W001_Weight_Dunn <- dunnTest(Area.mm2 ~ Weight, data=DataAreaW001_G2, method="bonferroni")
write.table(G2_W001_Weight_Dunn$res, file = "Statistics/G2_W001_Weight_Dunn Test.csv", quote = F, sep = ",", row.names = F)

# W007_G2 Kruskal-Wallis Test
kruskal.test(Area.mm2 ~ Weight, data = DataAreaW007_G2)

# W007_G2 Dunn's Test with Bonferroni correction for p-values
G2_W007_Weight_Dunn <- dunnTest(Area.mm2 ~ Weight, data=DataAreaW007_G2, method="bonferroni")
write.table(G2_W007_Weight_Dunn$res, file = "Statistics/G2_W007_Weight_Dunn Test.csv", quote = F, sep = ",", row.names = F)

# W015_G2 Kruskal-Wallis Test
kruskal.test(Area.mm2 ~ Weight, data = DataAreaW015_G2)

# W015_G2 Dunn's Test with Bonferroni correction for p-values
G2_W015_Weight_Dunn <- dunnTest(Area.mm2 ~ Weight, data=DataAreaW015_G2, method="bonferroni")
write.table(G2_W015_Weight_Dunn$res, file = "Statistics/G2_W015_Weight_Dunn Test.csv", quote = F, sep = ",", row.names = F)


# W000_G3 Kruskal-Wallis Test
kruskal.test(Area.mm2 ~ Weight, data = DataAreaW000_G3)

# W000_G3 Dunn's Test with Bonferroni correction for p-values
G3_W000_Weight_Dunn <- dunnTest(Area.mm2 ~ Weight, data=DataAreaW000_G3, method="bonferroni")
write.table(G3_W000_Weight_Dunn$res, file = "Statistics/G3_W000_Weight_Dunn Test.csv", quote = F, sep = ",", row.names = F)

# W001_G3 Kruskal-Wallis Test
kruskal.test(Area.mm2 ~ Weight, data = DataAreaW001_G3)

# W001_G3 Dunn's Test with Bonferroni correction for p-values
G3_W001_Weight_Dunn <- dunnTest(Area.mm2 ~ Weight, data=DataAreaW001_G3, method="bonferroni")
write.table(G3_W001_Weight_Dunn$res, file = "Statistics/G3_W001_Weight_Dunn Test.csv", quote = F, sep = ",", row.names = F)

# W007_G3 Kruskal-Wallis Test
kruskal.test(Area.mm2 ~ Weight, data = DataAreaW007_G3)

# W007_G3 Dunn's Test with Bonferroni correction for p-values
G3_W007_Weight_Dunn <- dunnTest(Area.mm2 ~ Weight, data=DataAreaW007_G3, method="bonferroni")
write.table(G3_W007_Weight_Dunn$res, file = "Statistics/G3_W007_Weight_Dunn Test.csv", quote = F, sep = ",", row.names = F)

# W015_G3 Kruskal-Wallis Test
kruskal.test(Area.mm2 ~ Weight, data = DataAreaW015_G3)

# W015_G3 Dunn's Test with Bonferroni correction for p-values
G3_W015_Weight_Dunn <- dunnTest(Area.mm2 ~ Weight, data=DataAreaW015_G3, method="bonferroni")
write.table(G3_W015_Weight_Dunn$res, file = "Statistics/G3_W015_Weight_Dunn Test.csv", quote = F, sep = ",", row.names = F)


# W000_G4 Kruskal-Wallis Test
kruskal.test(Area.mm2 ~ Weight, data = DataAreaW000_G4)

# W000_G4 Dunn's Test with Bonferroni correction for p-values
G4_W000_Weight_Dunn <- dunnTest(Area.mm2 ~ Weight, data=DataAreaW000_G4, method="bonferroni")
write.table(G4_W000_Weight_Dunn$res, file = "Statistics/G4_W000_Weight_Dunn Test.csv", quote = F, sep = ",", row.names = F)

# W001_G4 Kruskal-Wallis Test
kruskal.test(Area.mm2 ~ Weight, data = DataAreaW001_G4)

# W001_G4 Dunn's Test with Bonferroni correction for p-values
G4_W001_Weight_Dunn <- dunnTest(Area.mm2 ~ Weight, data=DataAreaW001_G4, method="bonferroni")
write.table(G4_W001_Weight_Dunn$res, file = "Statistics/G4_W001_Weight_Dunn Test.csv", quote = F, sep = ",", row.names = F)

# W007_G4 Kruskal-Wallis Test
kruskal.test(Area.mm2 ~ Weight, data = DataAreaW007_G4)

# W007_G4 Dunn's Test with Bonferroni correction for p-values
G4_W007_Weight_Dunn <- dunnTest(Area.mm2 ~ Weight, data=DataAreaW007_G4, method="bonferroni")
write.table(G4_W007_Weight_Dunn$res, file = "Statistics/G4_W007_Weight_Dunn Test.csv", quote = F, sep = ",", row.names = F)

# W015_G4 Kruskal-Wallis Test
kruskal.test(Area.mm2 ~ Weight, data = DataAreaW015_G4)

# W015_G4 Dunn's Test with Bonferroni correction for p-values
G4_W015_Weight_Dunn <- dunnTest(Area.mm2 ~ Weight, data=DataAreaW015_G4, method="bonferroni")
write.table(G4_W015_Weight_Dunn$res, file = "Statistics/G4_W015_Weight_Dunn Test.csv", quote = F, sep = ",", row.names = F)


######################################################################
######       COMPARISON OF WASHING CONDITIONS - SHEDDING        ######
######################################################################
DataAreaW000_G4_800$Garment <- "G4"
DataAreaW001_G4_800$Garment <- "G4"
DataAreaW007_G4_800$Garment <- "G4"
DataAreaW015_G4_800$Garment <- "G4"

W000_800g_Shedding_Combined <- rbind(DataAreaW000_G1_800,DataAreaW000_G2_800,DataAreaW000_G3_800,DataAreaW000_G4_800)
W001_800g_Shedding_Combined <- rbind(DataAreaW001_G1_800,DataAreaW001_G2_800,DataAreaW001_G3_800,DataAreaW001_G4_800)
W007_800g_Shedding_Combined <- rbind(DataAreaW007_G1_800,DataAreaW007_G2_800,DataAreaW007_G3_800,DataAreaW007_G4_800)
W015_800g_Shedding_Combined <- rbind(DataAreaW015_G1_800,DataAreaW015_G2_800,DataAreaW015_G3_800,DataAreaW015_G4_800)

kruskal.test(Area.mm2 ~ Garment, data = W000_800g_Shedding_Combined)
Shedding_W000_WashingCondition_Dunn <- dunnTest(Area.mm2 ~ Garment, data=W000_800g_Shedding_Combined, method="bonferroni")
write.table(Shedding_W000_WashingCondition_Dunn$res, file = "Statistics/Shedding_W000_Washing Condition_Dunn Test.csv", quote = F, sep = ",", row.names = F)

kruskal.test(Area.mm2 ~ Garment, data = W001_800g_Shedding_Combined)
Shedding_W001_WashingCondition_Dunn <- dunnTest(Area.mm2 ~ Garment, data=W001_800g_Shedding_Combined, method="bonferroni")
write.table(Shedding_W001_WashingCondition_Dunn$res, file = "Statistics/Shedding_W001_Washing Condition_Dunn Test.csv", quote = F, sep = ",", row.names = F)

kruskal.test(Area.mm2 ~ Garment, data = W007_800g_Shedding_Combined)
Shedding_W007_WashingCondition_Dunn <- dunnTest(Area.mm2 ~ Garment, data=W007_800g_Shedding_Combined, method="bonferroni")
write.table(Shedding_W007_WashingCondition_Dunn$res, file = "Statistics/Shedding_W007_Washing Condition_Dunn Test.csv", quote = F, sep = ",", row.names = F)

kruskal.test(Area.mm2 ~ Garment, data = W015_800g_Shedding_Combined)
Shedding_W015_WashingCondition_Dunn <- dunnTest(Area.mm2 ~ Garment, data=W015_800g_Shedding_Combined, method="bonferroni")
write.table(Shedding_W015_WashingCondition_Dunn$res, file = "Statistics/Shedding_W015_Washing Condition_Dunn Test.csv", quote = F, sep = ",", row.names = F)


######################################################################
######       COMPARISON OF WASHING CONDITIONS - TRANSFER        ######
######################################################################
TransferFibreCount_G1$Garment <- "G1"
TransferFibreCount_G2$Garment <- "G2"
TransferFibreCount_G3$Garment <- "G3"
TransferFibreCount_G4$Garment <- "G4"

W000_Transfer_Combined <- rbind(TransferFibreCount_G1[TransferFibreCount_G1$group == 'W000',],TransferFibreCount_G2[TransferFibreCount_G2$group == 'W000',],TransferFibreCount_G3[TransferFibreCount_G3$group == 'W000',],TransferFibreCount_G4[TransferFibreCount_G4$group == 'W000',])
W001_Transfer_Combined <- rbind(TransferFibreCount_G1[TransferFibreCount_G1$group == 'W001',],TransferFibreCount_G2[TransferFibreCount_G2$group == 'W001',],TransferFibreCount_G3[TransferFibreCount_G3$group == 'W001',],TransferFibreCount_G4[TransferFibreCount_G4$group == 'W001',])
W007_Transfer_Combined <- rbind(TransferFibreCount_G1[TransferFibreCount_G1$group == 'W007',],TransferFibreCount_G2[TransferFibreCount_G2$group == 'W007',],TransferFibreCount_G3[TransferFibreCount_G3$group == 'W007',],TransferFibreCount_G4[TransferFibreCount_G4$group == 'W007',])
W015_Transfer_Combined <- rbind(TransferFibreCount_G1[TransferFibreCount_G1$group == 'W015',],TransferFibreCount_G2[TransferFibreCount_G2$group == 'W015',],TransferFibreCount_G3[TransferFibreCount_G3$group == 'W015',],TransferFibreCount_G4[TransferFibreCount_G4$group == 'W015',])

kruskal.test(value ~ Garment, data = W000_Transfer_Combined)
Transfer_W000_WashingCondition_Dunn <- dunnTest(value ~ Garment, data=W000_Transfer_Combined, method="bonferroni")
write.table(Transfer_W000_WashingCondition_Dunn$res, file = "Statistics/Transfer_W000_Washing Condition_Dunn Test.csv", quote = F, sep = ",", row.names = F)

kruskal.test(value ~ Garment, data = W001_Transfer_Combined)
Transfer_W001_WashingCondition_Dunn <- dunnTest(value ~ Garment, data=W001_Transfer_Combined, method="bonferroni")
write.table(Transfer_W001_WashingCondition_Dunn$res, file = "Statistics/Transfer_W001_Washing Condition_Dunn Test.csv", quote = F, sep = ",", row.names = F)

kruskal.test(value ~ Garment, data = W007_Transfer_Combined)
Transfer_W007_WashingCondition_Dunn <- dunnTest(value ~ Garment, data=W007_Transfer_Combined, method="bonferroni")
write.table(Transfer_W007_WashingCondition_Dunn$res, file = "Statistics/Transfer_W007_Washing Condition_Dunn Test.csv", quote = F, sep = ",", row.names = F)

kruskal.test(value ~ Garment, data = W015_Transfer_Combined)
Transfer_W015_WashingCondition_Dunn <- dunnTest(value ~ Garment, data=W015_Transfer_Combined, method="bonferroni")
write.table(Transfer_W015_WashingCondition_Dunn$res, file = "Statistics/Transfer_W015_Washing Condition_Dunn Test.csv", quote = F, sep = ",", row.names = F)
