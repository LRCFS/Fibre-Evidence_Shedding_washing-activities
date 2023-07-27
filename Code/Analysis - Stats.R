################################################
######       COMPARISON OF WEIGHTS        ######
################################################
G1_W000_SH_Combined <- rbind(DataAreaW000_G1_100,DataAreaW000_G1_800,DataAreaW000_G1_2000)
G1_W001_SH_Combined <- rbind(DataAreaW001_G1_100,DataAreaW001_G1_800,DataAreaW001_G1_2000)
G1_W007_SH_Combined <- rbind(DataAreaW007_G1_100,DataAreaW007_G1_800,DataAreaW007_G1_2000)
G1_W015_SH_Combined <- rbind(DataAreaW015_G1_100,DataAreaW015_G1_800,DataAreaW015_G1_2000)

# W000_G1 Kruskal-Wallis Test
kruskal.test(Area.mm2 ~ Weight, data = G1_W000_SH_Combined)

# W000_G1 Dunn's Test with Bonferroni correction for p-values
G1_W000_Weight_Dunn <- dunnTest(Area.mm2 ~ Weight, data=G1_W000_SH_Combined, method="bonferroni")
write.table(G1_W000_Weight_Dunn$res, file = "Statistics/G1_W000_Weight_Dunn Test.csv", quote = F, sep = ",", row.names = F)

# W001_G1 Kruskal-Wallis Test
kruskal.test(Area.mm2 ~ Weight, data = G1_W001_SH_Combined)

# W001_G1 Dunn's Test with Bonferroni correction for p-values
G1_W001_Weight_Dunn <- dunnTest(Area.mm2 ~ Weight, data=G1_W001_SH_Combined, method="bonferroni")
write.table(G1_W001_Weight_Dunn$res, file = "Statistics/G1_W001_Weight_Dunn Test.csv", quote = F, sep = ",", row.names = F)

# W007_G1 Kruskal-Wallis Test
kruskal.test(Area.mm2 ~ Weight, data = G1_W007_SH_Combined)

# W007_G1 Dunn's Test with Bonferroni correction for p-values
G1_W007_Weight_Dunn <- dunnTest(Area.mm2 ~ Weight, data=G1_W007_SH_Combined, method="bonferroni")
write.table(G1_W007_Weight_Dunn$res, file = "Statistics/G1_W007_Weight_Dunn Test.csv", quote = F, sep = ",", row.names = F)

# W015_G1 Kruskal-Wallis Test
kruskal.test(Area.mm2 ~ Weight, data = G1_W015_SH_Combined)

# W015_G1 Dunn's Test with Bonferroni correction for p-values
G1_W015_Weight_Dunn <- dunnTest(Area.mm2 ~ Weight, data=G1_W015_SH_Combined, method="bonferroni")
write.table(G1_W015_Weight_Dunn$res, file = "Statistics/G1_W015_Weight_Dunn Test.csv", quote = F, sep = ",", row.names = F)


G2_W000_SH_Combined <- rbind(DataAreaW000_G2_100,DataAreaW000_G2_800,DataAreaW000_G2_2000)
G2_W001_SH_Combined <- rbind(DataAreaW001_G2_100,DataAreaW001_G2_800,DataAreaW001_G2_2000)
G2_W007_SH_Combined <- rbind(DataAreaW007_G2_100,DataAreaW007_G2_800,DataAreaW007_G2_2000)
G2_W015_SH_Combined <- rbind(DataAreaW015_G2_100,DataAreaW015_G2_800,DataAreaW015_G2_2000)

# W000_G2 Kruskal-Wallis Test
kruskal.test(Area.mm2 ~ Weight, data = G2_W000_SH_Combined)

# W000_G2 Dunn's Test with Bonferroni correction for p-values
G2_W000_Weight_Dunn <- dunnTest(Area.mm2 ~ Weight, data=G2_W000_SH_Combined, method="bonferroni")
write.table(G2_W000_Weight_Dunn$res, file = "Statistics/G2_W000_Weight_Dunn Test.csv", quote = F, sep = ",", row.names = F)

# W001_G2 Kruskal-Wallis Test
kruskal.test(Area.mm2 ~ Weight, data = G2_W001_SH_Combined)

# W001_G2 Dunn's Test with Bonferroni correction for p-values
G2_W001_Weight_Dunn <- dunnTest(Area.mm2 ~ Weight, data=G2_W001_SH_Combined, method="bonferroni")
write.table(G2_W001_Weight_Dunn$res, file = "Statistics/G2_W001_Weight_Dunn Test.csv", quote = F, sep = ",", row.names = F)

# W007_G2 Kruskal-Wallis Test
kruskal.test(Area.mm2 ~ Weight, data = G2_W007_SH_Combined)

# W007_G2 Dunn's Test with Bonferroni correction for p-values
G2_W007_Weight_Dunn <- dunnTest(Area.mm2 ~ Weight, data=G2_W007_SH_Combined, method="bonferroni")
write.table(G2_W007_Weight_Dunn$res, file = "Statistics/G2_W007_Weight_Dunn Test.csv", quote = F, sep = ",", row.names = F)

# W015_G2 Kruskal-Wallis Test
kruskal.test(Area.mm2 ~ Weight, data = G2_W015_SH_Combined)

# W015_G2 Dunn's Test with Bonferroni correction for p-values
G2_W015_Weight_Dunn <- dunnTest(Area.mm2 ~ Weight, data=G2_W015_SH_Combined, method="bonferroni")
write.table(G2_W015_Weight_Dunn$res, file = "Statistics/G2_W015_Weight_Dunn Test.csv", quote = F, sep = ",", row.names = F)


G3_W000_SH_Combined <- rbind(DataAreaW000_G3_100,DataAreaW000_G3_800,DataAreaW000_G3_2000)
G3_W001_SH_Combined <- rbind(DataAreaW001_G3_100,DataAreaW001_G3_800,DataAreaW001_G3_2000)
G3_W007_SH_Combined <- rbind(DataAreaW007_G3_100,DataAreaW007_G3_800,DataAreaW007_G3_2000)
G3_W015_SH_Combined <- rbind(DataAreaW015_G3_100,DataAreaW015_G3_800,DataAreaW015_G3_2000)

# W000_G3 Kruskal-Wallis Test
kruskal.test(Area.mm2 ~ Weight, data = G3_W000_SH_Combined)

# W000_G3 Dunn's Test with Bonferroni correction for p-values
G3_W000_Weight_Dunn <- dunnTest(Area.mm2 ~ Weight, data=G3_W000_SH_Combined, method="bonferroni")
write.table(G3_W000_Weight_Dunn$res, file = "Statistics/G3_W000_Weight_Dunn Test.csv", quote = F, sep = ",", row.names = F)

# W001_G3 Kruskal-Wallis Test
kruskal.test(Area.mm2 ~ Weight, data = G3_W001_SH_Combined)

# W001_G3 Dunn's Test with Bonferroni correction for p-values
G3_W001_Weight_Dunn <- dunnTest(Area.mm2 ~ Weight, data=G3_W001_SH_Combined, method="bonferroni")
write.table(G3_W001_Weight_Dunn$res, file = "Statistics/G3_W001_Weight_Dunn Test.csv", quote = F, sep = ",", row.names = F)

# W007_G3 Kruskal-Wallis Test
kruskal.test(Area.mm2 ~ Weight, data = G3_W007_SH_Combined)

# W007_G3 Dunn's Test with Bonferroni correction for p-values
G3_W007_Weight_Dunn <- dunnTest(Area.mm2 ~ Weight, data=G3_W007_SH_Combined, method="bonferroni")
write.table(G3_W007_Weight_Dunn$res, file = "Statistics/G3_W007_Weight_Dunn Test.csv", quote = F, sep = ",", row.names = F)

# W015_G3 Kruskal-Wallis Test
kruskal.test(Area.mm2 ~ Weight, data = G3_W015_SH_Combined)

# W015_G3 Dunn's Test with Bonferroni correction for p-values
G3_W015_Weight_Dunn <- dunnTest(Area.mm2 ~ Weight, data=G3_W015_SH_Combined, method="bonferroni")
write.table(G3_W015_Weight_Dunn$res, file = "Statistics/G3_W015_Weight_Dunn Test.csv", quote = F, sep = ",", row.names = F)


G4_W000_SH_Combined <- rbind(DataAreaW000_G4_100,DataAreaW000_G4_800,DataAreaW000_G4_2000)
G4_W001_SH_Combined <- rbind(DataAreaW001_G4_100,DataAreaW001_G4_800,DataAreaW001_G4_2000)
G4_W007_SH_Combined <- rbind(DataAreaW007_G4_100,DataAreaW007_G4_800,DataAreaW007_G4_2000)
G4_W015_SH_Combined <- rbind(DataAreaW015_G4_100,DataAreaW015_G4_800,DataAreaW015_G4_2000)

# W000_G4 Kruskal-Wallis Test
kruskal.test(Area.mm2 ~ Weight, data = G4_W000_SH_Combined)

# W000_G4 Dunn's Test with Bonferroni correction for p-values
G4_W000_Weight_Dunn <- dunnTest(Area.mm2 ~ Weight, data=G4_W000_SH_Combined, method="bonferroni")
write.table(G4_W000_Weight_Dunn$res, file = "Statistics/G4_W000_Weight_Dunn Test.csv", quote = F, sep = ",", row.names = F)

# W001_G4 Kruskal-Wallis Test
kruskal.test(Area.mm2 ~ Weight, data = G4_W001_SH_Combined)

# W001_G4 Dunn's Test with Bonferroni correction for p-values
G4_W001_Weight_Dunn <- dunnTest(Area.mm2 ~ Weight, data=G4_W001_SH_Combined, method="bonferroni")
write.table(G4_W001_Weight_Dunn$res, file = "Statistics/G4_W001_Weight_Dunn Test.csv", quote = F, sep = ",", row.names = F)

# W007_G4 Kruskal-Wallis Test
kruskal.test(Area.mm2 ~ Weight, data = G4_W007_SH_Combined)

# W007_G4 Dunn's Test with Bonferroni correction for p-values
G4_W007_Weight_Dunn <- dunnTest(Area.mm2 ~ Weight, data=G4_W007_SH_Combined, method="bonferroni")
write.table(G4_W007_Weight_Dunn$res, file = "Statistics/G4_W007_Weight_Dunn Test.csv", quote = F, sep = ",", row.names = F)

# W015_G4 Kruskal-Wallis Test
kruskal.test(Area.mm2 ~ Weight, data = G4_W015_SH_Combined)

# W015_G4 Dunn's Test with Bonferroni correction for p-values
G4_W015_Weight_Dunn <- dunnTest(Area.mm2 ~ Weight, data=G4_W015_SH_Combined, method="bonferroni")
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

##############################################################################
######       RATIO OF SHEDDING FIBRE AREA TO TRANSFER FIBRE AREA        ######
##############################################################################
### G1 ###
FC_Shedding_800g_G1 <- rbind(meanDataAreaW000_G1_800,meanDataAreaW001_G1_800,meanDataAreaW003_G1_800,meanDataAreaW005_G1_800,meanDataAreaW007_G1_800,meanDataAreaW009_G1_800,meanDataAreaW011_G1_800,meanDataAreaW013_G1_800,meanDataAreaW015_G1_800)
Wash = c("W000","W001","W003","W005","W007","W009","W011","W013","W015")
SHTR_G1 <- data.frame(Wash)
SHTR_G1$Shedding <- FC_Shedding_800g_G1$meanArea
SHTR_G1$Transfer <- FibreCount_TransferArea_G1$meanArea

# Scatter Plot
pSHTR_G1 <- ggplot(SHTR_G1, aes(x = Shedding, y = Transfer)) +
  geom_point(size = 3) +
  labs(x = "Shedding average fibre area (mm\u00b2)", y = "Transfer average fibre area (mm\u00b2)") +
  geom_smooth(method=lm, se=FALSE)
pSHTR_G1
ggsave("SheddingTransfer_G1.png", pSHTR_G1, width = 10, height = 9, units = "in", dpi=150, path = "Statistics")

# Correlation Analysis
correlation <- cor(SHTR_G1$Shedding, SHTR_G1$Transfer)
print(paste("Correlation Coefficient:", correlation))


### G2 ###
FC_Shedding_800g_G2 <- rbind(meanDataAreaW000_G2_800,meanDataAreaW001_G2_800,meanDataAreaW003_G2_800,meanDataAreaW005_G2_800,meanDataAreaW007_G2_800,meanDataAreaW009_G2_800,meanDataAreaW011_G2_800,meanDataAreaW013_G2_800,meanDataAreaW015_G2_800)
Wash = c("W000","W001","W003","W005","W007","W009","W011","W013","W015")
SHTR_G2_df <- data.frame(Wash)
SHTR_G2_df$Shedding <- FC_Shedding_800g_G2$meanArea
SHTR_G2_df$Transfer <- FibreCount_TransferArea_G2$meanArea
SHTR_G2 <- SHTR_G2_df[-7,]

# Scatter Plot
pSHTR_G2 <- ggplot(SHTR_G2, aes(x = Shedding, y = Transfer)) +
  geom_point(size = 3) +
  labs(x = "Shedding average fibre area (mm\u00b2)", y = "Transfer average fibre area (mm\u00b2)") +
  geom_smooth(method=lm, se=FALSE)
pSHTR_G2
ggsave("SheddingTransfer_G2.png", pSHTR_G2, width = 10, height = 9, units = "in", dpi=150, path = "Statistics")

# Correlation Analysis
correlation <- cor(SHTR_G2$Shedding, SHTR_G2$Transfer)
print(paste("Correlation Coefficient:", correlation))


### G3 ###
FC_Shedding_800g_G3 <- rbind(meanDataAreaW000_G3_800,meanDataAreaW001_G3_800,meanDataAreaW003_G3_800,meanDataAreaW005_G3_800,meanDataAreaW007_G3_800,meanDataAreaW009_G3_800,meanDataAreaW011_G3_800,meanDataAreaW013_G3_800,meanDataAreaW015_G3_800)
Wash = c("W000","W001","W003","W005","W007","W009","W011","W013","W015")
SHTR_G3_df <- data.frame(Wash)
SHTR_G3_df$Shedding <- FC_Shedding_800g_G3$meanArea
SHTR_G3_df$Transfer <- FibreCount_TransferArea_G3$meanArea
SHTR_G3 <- SHTR_G3_df[-7,]

# Scatter Plot
pSHTR_G3 <- ggplot(SHTR_G3, aes(x = Shedding, y = Transfer)) +
  geom_point(size = 3) +
  labs(x = "Shedding average fibre area (mm\u00b2)", y = "Transfer average fibre area (mm\u00b2)") +
  geom_smooth(method=lm, se=FALSE)
pSHTR_G3
ggsave("SheddingTransfer_G3.png", pSHTR_G3, width = 10, height = 9, units = "in", dpi=150, path = "Statistics")

# Correlation Analysis
correlation <- cor(SHTR_G3$Shedding, SHTR_G3$Transfer)
print(paste("Correlation Coefficient:", correlation))


### G4 ###
FC_Shedding_800g_G4 <- rbind(meanDataAreaW000_G4_800,meanDataAreaW001_G4_800,meanDataAreaW003_G4_800,meanDataAreaW005_G4_800,meanDataAreaW007_G4_800,meanDataAreaW009_G4_800,meanDataAreaW011_G4_800,meanDataAreaW013_G4_800,meanDataAreaW015_G4_800)
Wash = c("W000","W001","W003","W005","W007","W009","W011","W013","W015")
SHTR_G4 <- data.frame(Wash)
SHTR_G4$Shedding <- FC_Shedding_800g_G4$meanArea
SHTR_G4$Transfer <- FibreCount_TransferArea_G4$meanArea

# Scatter Plot
pSHTR_G4 <- ggplot(SHTR_G4, aes(x = Shedding, y = Transfer)) +
  geom_point(size = 3) +
  labs(x = "Shedding average fibre area (mm\u00b2)", y = "Transfer average fibre area (mm\u00b2)") +
  geom_smooth(method=lm, se=FALSE)
pSHTR_G4
ggsave("SheddingTransfer_G4.png", pSHTR_G4, width = 10, height = 9, units = "in", dpi=150, path = "Statistics")

# Correlation Analysis
correlation <- cor(SHTR_G4$Shedding, SHTR_G4$Transfer)
print(paste("Correlation Coefficient:", correlation))

# Combined results - figure 6
pSHTR_figure6_pending <- ggarrange(pSHTR_G1+ rremove("ylab") + rremove("xlab"),
                                 pSHTR_G4+ rremove("ylab") + rremove("xlab"),
                                 labels = c("A", "B"),
                                 common.legend = F, legend = "right",
                                 align = "hv",
                                 ncol = 1, nrow = 2,
                                 font.label = list(size = 12, color = "black", family = NULL, position = "top"))+
  theme(plot.margin = margin(0,1.5,0,0, "cm")) # in order (Top,left,bottom,right)
pSHTR_figure6_pending

ppSHTR_combined <- annotate_figure(pSHTR_figure6_pending, left = textGrob("Transfer total fibre area (mm\u00b2)\n", rot = 90, vjust = 0.5, hjust = 0.5, gp = gpar(cex =1)),
                                 bottom = textGrob("Shedding total fibre area (mm\u00b2)\n", vjust = 0.5, hjust = 0.5,gp = gpar(cex = 1)));ppSHTR_combined

ggsave("ppSHTR_figure6.png", ppSHTR_combined, width = 7, height = 6, units = "in", dpi=300, path = "Results")

# Combined results - figure 7
pSHTR_figure7_pending <- ggarrange(pSHTR_G2+ rremove("ylab") + rremove("xlab"),
                                   pSHTR_G3+ rremove("ylab") + rremove("xlab"),
                                   labels = c("A", "B"),
                                   common.legend = F, legend = "right",
                                   align = "hv",
                                   ncol = 1, nrow = 2,
                                   font.label = list(size = 12, color = "black", family = NULL, position = "top"))+
  theme(plot.margin = margin(0,1.5,0,0, "cm")) # in order (Top,left,bottom,right)
pSHTR_figure7_pending

ppSHTR_combined_2 <- annotate_figure(pSHTR_figure7_pending, left = textGrob("Transfer total fibre area (mm\u00b2)\n", rot = 90, vjust = 0.5, hjust = 0.5, gp = gpar(cex =1)),
                                   bottom = textGrob("Shedding total fibre area (mm\u00b2)\n", vjust = 0.5, hjust = 0.5,gp = gpar(cex = 1)));ppSHTR_combined_2

ggsave("ppSHTR_figure7.png", ppSHTR_combined_2, width = 7, height = 6, units = "in", dpi=300, path = "Results")
