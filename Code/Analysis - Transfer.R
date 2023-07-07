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
W009_G1_Dataset$Coder <- "W009"
W011_G1_Dataset$Coder <- "W011"
W013_G1_Dataset$Coder <- "W013"

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
W009negative <- W009_G1_Dataset %>% filter(grepl('negative', Sample))
W011negative <- W011_G1_Dataset %>% filter(grepl('negative', Sample))
W013negative <- W013_G1_Dataset %>% filter(grepl('negative', Sample))

# Create a data frame "Negativecontrol" with all the negative controls 
Negativecontrol <- rbind(W000negative,W001negative, W002negative,W003negative,W004negative,W005negative,W006negative,W007negative,W009negative,W011negative,W013negative)

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
W009positive <- W009_G1_Dataset %>% filter(grepl('positive', Sample))
W011positive <- W011_G1_Dataset %>% filter(grepl('positive', Sample))
W013positive <- W013_G1_Dataset %>% filter(grepl('positive', Sample))

Positivecontrol <- rbind(W000positive,W001positive, W002positive, W003positive, W004positive, W005positive,W006positive,W007positive,W009positive,W011positive,W013positive)
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
forFibreCount9<- W009_G1_Dataset[!(W009_G1_Dataset$Sample=="MP_W009_G1_positive_B" | W009_G1_Dataset$Sample=="MP_W009_G1_negative_B"),]
forFibreCount11<- W011_G1_Dataset[!(W011_G1_Dataset$Sample=="MP_W011_G1_positive_B" | W011_G1_Dataset$Sample=="MP_W011_G1_negative_B"),]
forFibreCount13<- W013_G1_Dataset[!(W013_G1_Dataset$Sample=="MP_W013_G1_positive_B" | W013_G1_Dataset$Sample=="MP_W013_G1_negative_B"),]

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
BackgroundW009 <- forFibreCount9 %>%
  dplyr::select(Coder,`Before transfer`)
BackgroundW011 <- forFibreCount11 %>%
  dplyr::select(Coder,`Before transfer`)
BackgroundW013 <- forFibreCount11 %>%
  dplyr::select(Coder,`Before transfer`)

#### Combine all the dataframe ####
BackgroundFibreCount <- rbind(BackgroundW000, BackgroundW001, BackgroundW002,BackgroundW003,BackgroundW004,
                              BackgroundW005,BackgroundW006,BackgroundW007,BackgroundW009,BackgroundW011,BackgroundW013)
names(BackgroundFibreCount) <- c("group", "value")
# write.table(TransferFibreCount_G1, file = "Fibre Count - Transfer.csv", quote = F, sep = ",", row.names = F)

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
TransferW009 <- forFibreCount9 %>%
  dplyr::select(Coder,`After transfer`)
TransferW011 <- forFibreCount11 %>%
  dplyr::select(Coder,`After transfer`)
TransferW013 <- forFibreCount13 %>%
  dplyr::select(Coder,`After transfer`)

#### Combine all the dataframe ####
TransferFibreCount_G1 <- rbind(TransferW000, TransferW001, TransferW002,TransferW003,TransferW004,TransferW005,
                            TransferW006,TransferW007,TransferW009,TransferW011,TransferW013)
names(TransferFibreCount_G1) <- c("group", "value")
# write.table(TransferFibreCount_G1, file = "Fibre Count - Transfer.csv", quote = F, sep = ",", row.names = F)

#### Bar plots of data by group - distribution ####
histogram(~ value | group,data=TransferFibreCount_G1,layout=c(3,3),
          xlab="Number of fibres")

#### Create a table with descriptive statistics ####
meanAtr_G1 <- aggregate(value ~  group, TransferFibreCount_G1, function(x) {round(mean(x), digits=2)})
SDAtr_G1 <- aggregate(value ~  group, TransferFibreCount_G1, function(x) {round(SD(x), digits=2)})
SD2Atr_G1 <- round(sqrt((SDAtr_G1$value^2)+(0.95^2)),digits=2)
medianAtr_G1 <- aggregate(value ~  group, TransferFibreCount_G1, median)
datatableAtr_G1 <- cbind(meanAtr_G1, medianAtr_G1$value, SDAtr_G1$value, SD2Atr_G1)
names(datatableAtr_G1) <- c("Wash number", "Average", "median", "SD", "SD2")
datatableAtr_G1$Forthesis <- paste(datatableAtr_G1$Average, datatableAtr_G1$SD, sep=" ± ")
#write.table(datatableAtr_G, file = "Stats_Atr red.csv", quote = F, sep = ",", row.names = F)

write.table(TransferFibreCount_G1, file = "Transfer_Fibre_Count.csv", quote = F, sep = ",", row.names = F)

#### GRAPH - FIGURE 4-8 ####
pAtr_G1 <- ggplot(TransferFibreCount_G1, aes(x=group, y=value)) +
  geom_boxplot() +
  stat_summary(fun = mean, colour="darkred",
               geom="point",position=position_dodge(width=0.75)) +
  stat_summary(fun = mean, colour="darkred", aes(group=1),
               geom="line", lwd=1, lty=1) +
  ylim(0,22)+
  labs(x="\nWash number", y="Number of Fibre\n") +
  theme_classic(base_family = "Arial", base_size = 14) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.background = element_rect(fill="grey95",size=1, linetype="solid", colour="grey80"),
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5))
show(pAtr_G1)
ggsave("Fibre Count boxplot_ATr_G1.png", pAtr_G1, width = 6, height = 7, units = "in", dpi=150, path = "Results")

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
W003_G2_Dataset$Coder <- "W003"
W004_G2_Dataset$Coder <- "W004"
W005_G2_Dataset$Coder <- "W005"
W006_G2_Dataset$Coder <- "W006"
W007_G2_Dataset$Coder <- "W007"
W008_G2_Dataset$Coder <- "W008"
W009_G2_Dataset$Coder <- "W009"
W010_G2_Dataset$Coder <- "W010"
W013_G2_Dataset$Coder <- "W013"
W015_G2_Dataset$Coder <- "W015"

#########################################################
#####                     Controls                  #####
#########################################################
# Negative controls
W000negative <- W000_G2_Dataset %>% filter(grepl('negative', Sample))
W001negative <- W001_G2_Dataset %>% filter(grepl('negative', Sample))
W002negative <- W002_G2_Dataset %>% filter(grepl('negative', Sample))
W003negative <- W003_G2_Dataset %>% filter(grepl('negative', Sample))
W004negative <- W004_G2_Dataset %>% filter(grepl('negative', Sample))
W005negative <- W005_G2_Dataset %>% filter(grepl('negative', Sample))
W006negative <- W006_G2_Dataset %>% filter(grepl('negative', Sample))
W007negative <- W007_G2_Dataset %>% filter(grepl('negative', Sample))
W008negative <- W008_G2_Dataset %>% filter(grepl('negative', Sample))
W009negative <- W009_G2_Dataset %>% filter(grepl('negative', Sample))
W010negative <- W010_G2_Dataset %>% filter(grepl('negative', Sample))
W013negative <- W013_G2_Dataset %>% filter(grepl('negative', Sample))
W015negative <- W015_G2_Dataset %>% filter(grepl('negative', Sample))

# Create a data frame "Negativecontrol" with all the negative controls 
Negativecontrol <- rbind(W000negative,W001negative, W002negative,W003negative, W004negative, W005negative, W006negative, W007negative,W008negative,W009negative,W010negative,W013negative,W015negative)

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
W003positive <- W003_G2_Dataset %>% filter(grepl('positive', Sample))
W004positive <- W004_G2_Dataset %>% filter(grepl('positive', Sample))
W005positive <- W005_G2_Dataset %>% filter(grepl('positive', Sample))
W006positive <- W006_G2_Dataset %>% filter(grepl('positive', Sample))
W007positive <- W007_G2_Dataset %>% filter(grepl('positive', Sample))
W008positive <- W008_G2_Dataset %>% filter(grepl('positive', Sample))
W009positive <- W009_G2_Dataset %>% filter(grepl('positive', Sample))
W010positive <- W010_G2_Dataset %>% filter(grepl('positive', Sample))
W013positive <- W013_G2_Dataset %>% filter(grepl('positive', Sample))
W015positive <- W015_G2_Dataset %>% filter(grepl('positive', Sample))

Positivecontrol <- rbind(W000positive,W001positive, W002positive,W003positive, W004positive, W005positive, W006positive,W007positive,W008positive,W009positive,W010positive,W013positive,W015positive)
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
forFibreCount0<- W000_G2_Dataset[!(W000_G2_Dataset$Sample=="MP_W000_G2_positive_B" | W000_G2_Dataset$Sample=="MP_W000_G2_negative_B"),]
forFibreCount1<- W001_G2_Dataset[!(W001_G2_Dataset$Sample=="MP_W001_G2_positive_B" | W001_G2_Dataset$Sample=="MP_W001_G2_negative_B"),]
forFibreCount2<- W002_G2_Dataset[!(W002_G2_Dataset$Sample=="MP_W002_G2_positive_B" | W002_G2_Dataset$Sample=="MP_W002_G2_negative_B"),]
forFibreCount3<- W003_G2_Dataset[!(W003_G2_Dataset$Sample=="MP_W003_G2_positive_B" | W003_G2_Dataset$Sample=="MP_W003_G2_negative_B"),]
forFibreCount4<- W004_G2_Dataset[!(W004_G2_Dataset$Sample=="MP_W004_G2_positive_B" | W004_G2_Dataset$Sample=="MP_W004_G2_negative_B"),]
forFibreCount5<- W005_G2_Dataset[!(W005_G2_Dataset$Sample=="MP_W005_G2_positive_B" | W005_G2_Dataset$Sample=="MP_W005_G2_negative_B"),]
forFibreCount6<- W006_G2_Dataset[!(W006_G2_Dataset$Sample=="MP_W006_G2_positive_B" | W006_G2_Dataset$Sample=="MP_W006_G2_negative_B"),]
forFibreCount7<- W007_G2_Dataset[!(W007_G2_Dataset$Sample=="MP_W007_G2_positive_B" | W007_G2_Dataset$Sample=="MP_W007_G2_negative_B"),]
forFibreCount8<- W008_G2_Dataset[!(W008_G2_Dataset$Sample=="MP_W008_G2_positive_B" | W008_G2_Dataset$Sample=="MP_W008_G2_negative_B"),]
forFibreCount9<- W009_G2_Dataset[!(W009_G2_Dataset$Sample=="MP_W009_G2_positive_B" | W009_G2_Dataset$Sample=="MP_W009_G2_negative_B"),]
forFibreCount10<- W010_G2_Dataset[!(W010_G2_Dataset$Sample=="MP_W010_G2_positive_B" | W010_G2_Dataset$Sample=="MP_W010_G2_negative_B"),]
forFibreCount13<- W013_G2_Dataset[!(W013_G2_Dataset$Sample=="MP_W013_G2_positive_B" | W013_G2_Dataset$Sample=="MP_W013_G2_negative_B"),]
forFibreCount15<- W015_G2_Dataset[!(W015_G2_Dataset$Sample=="MP_W015_G2_positive_B" | W015_G2_Dataset$Sample=="MP_W015_G2_negative_B"),]

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
BackgroundW008 <- forFibreCount8 %>%
  dplyr::select(Coder,`Before transfer`)
BackgroundW009 <- forFibreCount9 %>%
  dplyr::select(Coder,`Before transfer`)
BackgroundW010 <- forFibreCount10 %>%
  dplyr::select(Coder,`Before transfer`)
BackgroundW013 <- forFibreCount13 %>%
  dplyr::select(Coder,`Before transfer`)
BackgroundW015 <- forFibreCount15 %>%
  dplyr::select(Coder,`Before transfer`)

#### Combine all the dataframe ####
BackgroundFibreCount <- rbind(BackgroundW000, BackgroundW001, BackgroundW002,BackgroundW003, BackgroundW004, BackgroundW005, BackgroundW006,BackgroundW007,BackgroundW008,BackgroundW009,BackgroundW010,BackgroundW013,BackgroundW015)
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
TransferW008 <- forFibreCount8 %>%
  dplyr::select(Coder,`After transfer`)
TransferW009 <- forFibreCount9 %>%
  dplyr::select(Coder,`After transfer`)
TransferW010 <- forFibreCount10 %>%
  dplyr::select(Coder,`After transfer`)
TransferW013 <- forFibreCount13 %>%
  dplyr::select(Coder,`After transfer`)
TransferW015 <- forFibreCount15 %>%
  dplyr::select(Coder,`After transfer`)

#### Combine all the dataframe ####
TransferFibreCount_G2 <- rbind(TransferW000, TransferW001, TransferW002,TransferW003, TransferW004, TransferW005, TransferW006,TransferW007,TransferW008,TransferW009,TransferW010,TransferW013,TransferW015)
names(TransferFibreCount_G2) <- c("group", "value")
# write.table(TransferFibreCount_G2, file = "Fibre Count - Transfer.csv", quote = F, sep = ",", row.names = F)

#### Bar plots of data by group - distribution ####
histogram(~ value | group,data=TransferFibreCount_G2,layout=c(3,3),
          xlab="Number of fibres")

#### Create a table with descriptive statistics ####
meanAtr_G2 <- aggregate(value ~  group, TransferFibreCount_G2, function(x) {round(mean(x), digits=2)})
SDAtr_G2 <- aggregate(value ~  group, TransferFibreCount_G2, function(x) {round(SD(x), digits=2)})
SD2Atr_G2 <- round(sqrt((SDAtr_G2$value^2)+(0.95^2)),digits=2)
medianAtr_G2 <- aggregate(value ~  group, TransferFibreCount_G2, median)
datatableAtr_G2 <- cbind(meanAtr_G2, medianAtr_G2$value, SDAtr_G2$value, SD2Atr_G2)
names(datatableAtr_G2) <- c("Wash number", "Average", "median", "SD", "SD2")
datatableAtr_G2$Forthesis <- paste(datatableAtr_G2$Average, datatableAtr_G2$SD, sep=" ± ")
#write.table(datatableAtr_G2, file = "Stats_Atr red_G2.csv", quote = F, sep = ",", row.names = F)

write.table(TransferFibreCount_G2, file = "Transfer_Fibre_Count.csv", quote = F, sep = ",", row.names = F)

#### GRAPH - FIGURE 4-8 ####
pAtr_G2 <- ggplot(TransferFibreCount_G2, aes(x=group, y=value)) +
  geom_boxplot() +
  #scale_x_discrete(labels = every_n_labeler(5)) +
  stat_summary(fun = mean, colour="darkred",
               geom="point",position=position_dodge(width=0.75)) +
  stat_summary(fun = mean, colour="darkred", aes(group=1),
               geom="line", lwd=1, lty=1) +
  ylim(0,22)+
  labs(x="\nWash number", y="Number of Fibre\n") +
  theme_classic(base_family = "Arial", base_size = 14) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.background = element_rect(fill="grey95",size=1, linetype="solid", colour="grey80"),
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5))
show(pAtr_G2)
ggsave("Fibre Count boxplot_ATr_G2.png", pAtr_G2, width = 6, height = 7, units = "in", dpi=150, path = "Results")

#-------------------------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------------------------

#################################################################################################
#####                                FIBRE ANALYSIS GARMENT 3                               #####
#################################################################################################
#### Assign a Coder to each wash ####
W000_G3_Dataset$Coder <- "W000"
W001_G3_Dataset$Coder <- "W001"
W002_G3_Dataset$Coder <- "W002"
W003_G3_Dataset$Coder <- "W003"
W004_G3_Dataset$Coder <- "W004"
W005_G3_Dataset$Coder <- "W005"
W006_G3_Dataset$Coder <- "W006"
W007_G3_Dataset$Coder <- "W007"
W008_G3_Dataset$Coder <- "W008"
W009_G3_Dataset$Coder <- "W009"
W010_G3_Dataset$Coder <- "W010"
W013_G3_Dataset$Coder <- "W013"
W015_G3_Dataset$Coder <- "W015"

#########################################################
#####                     Controls                  #####
#########################################################
# Negative controls
W000negative <- W000_G3_Dataset %>% filter(grepl('negative', Sample))
W001negative <- W001_G3_Dataset %>% filter(grepl('negative', Sample))
W002negative <- W002_G3_Dataset %>% filter(grepl('negative', Sample))
W003negative <- W003_G3_Dataset %>% filter(grepl('negative', Sample))
W004negative <- W004_G3_Dataset %>% filter(grepl('negative', Sample))
W005negative <- W005_G3_Dataset %>% filter(grepl('negative', Sample))
W006negative <- W006_G3_Dataset %>% filter(grepl('negative', Sample))
W007negative <- W007_G3_Dataset %>% filter(grepl('negative', Sample))
W008negative <- W008_G3_Dataset %>% filter(grepl('negative', Sample))
W009negative <- W009_G3_Dataset %>% filter(grepl('negative', Sample))
W010negative <- W010_G3_Dataset %>% filter(grepl('negative', Sample))
W013negative <- W013_G3_Dataset %>% filter(grepl('negative', Sample))
W015negative <- W015_G3_Dataset %>% filter(grepl('negative', Sample))

# Create a data frame "Negativecontrol" with all the negative controls 
Negativecontrol <- rbind(W000negative,W001negative,W002negative,W003negative,W004negative,W005negative,W006negative,W007negative,W008negative,W009negative,W010negative,W013negative,W015negative)

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
W000positive <- W000_G3_Dataset %>% filter(grepl('positive', Sample))
W001positive <- W001_G3_Dataset %>% filter(grepl('positive', Sample))
W002positive <- W002_G3_Dataset %>% filter(grepl('positive', Sample))
W003positive <- W003_G3_Dataset %>% filter(grepl('positive', Sample))
W004positive <- W004_G3_Dataset %>% filter(grepl('positive', Sample))
W005positive <- W005_G3_Dataset %>% filter(grepl('positive', Sample))
W006positive <- W006_G3_Dataset %>% filter(grepl('positive', Sample))
W007positive <- W007_G3_Dataset %>% filter(grepl('positive', Sample))
W008positive <- W008_G3_Dataset %>% filter(grepl('positive', Sample))
W009positive <- W009_G3_Dataset %>% filter(grepl('positive', Sample))
W010positive <- W010_G3_Dataset %>% filter(grepl('positive', Sample))
W013positive <- W013_G3_Dataset %>% filter(grepl('positive', Sample))
W015positive <- W015_G3_Dataset %>% filter(grepl('positive', Sample))

Positivecontrol <- rbind(W000positive,W001positive,W002positive,W003positive,W004positive,W005positive,W006positive,W007positive,W008positive,W009positive,W010positive,W013positive,W015positive)
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
forFibreCount0<- W000_G3_Dataset[!(W000_G3_Dataset$Sample=="MP_W000_G3_positive_B" | W000_G3_Dataset$Sample=="MP_W000_G3_negative_B"),]
forFibreCount1<- W001_G3_Dataset[!(W001_G3_Dataset$Sample=="MP_W001_G3_positive_B" | W001_G3_Dataset$Sample=="MP_W001_G3_negative_B"),]
forFibreCount2<- W002_G3_Dataset[!(W002_G3_Dataset$Sample=="MP_W002_G3_positive_B" | W002_G3_Dataset$Sample=="MP_W002_G3_negative_B"),]
forFibreCount3<- W003_G3_Dataset[!(W003_G3_Dataset$Sample=="MP_W003_G3_positive_B" | W003_G3_Dataset$Sample=="MP_W003_G3_negative_B"),]
forFibreCount4<- W004_G3_Dataset[!(W004_G3_Dataset$Sample=="MP_W004_G3_positive_B" | W004_G3_Dataset$Sample=="MP_W004_G3_negative_B"),]
forFibreCount5<- W005_G3_Dataset[!(W005_G3_Dataset$Sample=="MP_W005_G3_positive_B" | W005_G3_Dataset$Sample=="MP_W005_G3_negative_B"),]
forFibreCount6<- W006_G3_Dataset[!(W006_G3_Dataset$Sample=="MP_W006_G3_positive_B" | W006_G3_Dataset$Sample=="MP_W006_G3_negative_B"),]
forFibreCount7<- W007_G3_Dataset[!(W007_G3_Dataset$Sample=="MP_W007_G3_positive_B" | W007_G3_Dataset$Sample=="MP_W007_G3_negative_B"),]
forFibreCount8<- W008_G3_Dataset[!(W008_G3_Dataset$Sample=="MP_W008_G3_positive_B" | W008_G3_Dataset$Sample=="MP_W008_G3_negative_B"),]
forFibreCount9<- W009_G3_Dataset[!(W009_G3_Dataset$Sample=="MP_W009_G3_positive_B" | W009_G3_Dataset$Sample=="MP_W009_G3_negative_B"),]
forFibreCount10<- W010_G3_Dataset[!(W010_G3_Dataset$Sample=="MP_W010_G3_positive_B" | W010_G3_Dataset$Sample=="MP_W010_G3_negative_B"),]
forFibreCount13<- W013_G3_Dataset[!(W013_G3_Dataset$Sample=="MP_W013_G3_positive_B" | W013_G3_Dataset$Sample=="MP_W013_G3_negative_B"),]
forFibreCount15<- W015_G3_Dataset[!(W015_G3_Dataset$Sample=="MP_W015_G3_positive_B" | W015_G3_Dataset$Sample=="MP_W015_G3_negative_B"),]

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
BackgroundW008 <- forFibreCount8 %>%
  dplyr::select(Coder,`Before transfer`)
BackgroundW009 <- forFibreCount9 %>%
  dplyr::select(Coder,`Before transfer`)
BackgroundW010 <- forFibreCount10 %>%
  dplyr::select(Coder,`Before transfer`)
BackgroundW013 <- forFibreCount13 %>%
  dplyr::select(Coder,`Before transfer`)
BackgroundW015 <- forFibreCount15 %>%
  dplyr::select(Coder,`Before transfer`)

#### Combine all the dataframe ####
BackgroundFibreCount <- rbind(BackgroundW000,BackgroundW001,BackgroundW002,BackgroundW003,BackgroundW004,BackgroundW005,BackgroundW006,BackgroundW007,BackgroundW008,BackgroundW009,BackgroundW010,BackgroundW013,BackgroundW015)
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
TransferW008 <- forFibreCount8 %>%
  dplyr::select(Coder,`After transfer`)
TransferW009 <- forFibreCount9 %>%
  dplyr::select(Coder,`After transfer`)
TransferW010 <- forFibreCount10 %>%
  dplyr::select(Coder,`After transfer`)
TransferW013 <- forFibreCount13 %>%
  dplyr::select(Coder,`After transfer`)
TransferW015 <- forFibreCount15 %>%
  dplyr::select(Coder,`After transfer`)

#### Combine all the dataframe ####
TransferFibreCount_G3 <- rbind(TransferW000, TransferW001,TransferW002,TransferW003,TransferW004,TransferW005,TransferW006,TransferW007,TransferW008,TransferW009,TransferW010,TransferW013,TransferW015)
names(TransferFibreCount_G3) <- c("group", "value")
# write.table(TransferFibreCount_G3, file = "Fibre Count - Transfer.csv", quote = F, sep = ",", row.names = F)

#### Bar plots of data by group - distribution ####
histogram(~ value | group,data=TransferFibreCount_G3,layout=c(3,3),
          xlab="Number of fibres")

#### Create a table with descriptive statistics ####
meanAtr_G3 <- aggregate(value ~  group, TransferFibreCount_G3, function(x) {round(mean(x), digits=2)})
SDAtr_G3 <- aggregate(value ~  group, TransferFibreCount_G3, function(x) {round(SD(x), digits=2)})
SD2Atr_G3 <- round(sqrt((SDAtr_G3$value^2)+(0.95^2)),digits=2)
medianAtr_G3 <- aggregate(value ~  group, TransferFibreCount_G3, median)
datatableAtr_G3 <- cbind(meanAtr_G3, medianAtr_G3$value, SDAtr_G3$value, SD2Atr_G3)
names(datatableAtr_G3) <- c("Wash number", "Average", "median", "SD", "SD2")
datatableAtr_G3$Forthesis <- paste(datatableAtr_G3$Average, datatableAtr_G3$SD, sep=" ± ")
#write.table(datatableAtr_G3, file = "Stats_Atr red_G3.csv", quote = F, sep = ",", row.names = F)

write.table(TransferFibreCount_G3, file = "Transfer_Fibre_Count.csv", quote = F, sep = ",", row.names = F)

#### GRAPH ####
pAtr_G3 <- ggplot(TransferFibreCount_G3, aes(x=group, y=value)) +
  geom_boxplot() +
  #scale_x_discrete(labels = every_n_labeler(5)) +
  stat_summary(fun = mean, colour="darkred",
               geom="point",position=position_dodge(width=0.75)) +
  stat_summary(fun = mean, colour="darkred", aes(group=1),
               geom="line", lwd=1, lty=1) +
  ylim(0,40)+
  labs(x="\nWash number", y="Number of Fibre\n") +
  theme_classic(base_family = "Arial", base_size = 14) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.background = element_rect(fill="grey95",size=1, linetype="solid", colour="grey80"),
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5))
show(pAtr_G3)
ggsave("Fibre Count boxplot_ATr_G3.png", pAtr_G3, width = 6, height = 7, units = "in", dpi=150, path = "Results")

#-------------------------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------------------------

#################################################################################################
#####                               FIBRE ANALYSIS GARMENT 4A                               #####
#################################################################################################
#### Assign a Coder to each wash ####
W000_G4A_Dataset$Coder <- "W000"
W001_G4A_Dataset$Coder <- "W001"
W002_G4A_Dataset$Coder <- "W002"
W003_G4A_Dataset$Coder <- "W003"
W004_G4A_Dataset$Coder <- "W004"
W005_G4A_Dataset$Coder <- "W005"
W006_G4A_Dataset$Coder <- "W006"
W007_G4A_Dataset$Coder <- "W007"
W008_G4A_Dataset$Coder <- "W008"
#W009_G4A_Dataset$Coder <- "W009"
#W010_G4A_Dataset$Coder <- "W010"

#########################################################
#####                     Controls                  #####
#########################################################
# Negative controls
W000negative <- W000_G4A_Dataset %>% filter(grepl('negative', Sample))
W001negative <- W001_G4A_Dataset %>% filter(grepl('negative', Sample))
W002negative <- W002_G4A_Dataset %>% filter(grepl('negative', Sample))
W003negative <- W003_G4A_Dataset %>% filter(grepl('negative', Sample))
W004negative <- W004_G4A_Dataset %>% filter(grepl('negative', Sample))
W005negative <- W005_G4A_Dataset %>% filter(grepl('negative', Sample))
W006negative <- W006_G4A_Dataset %>% filter(grepl('negative', Sample))
W007negative <- W007_G4A_Dataset %>% filter(grepl('negative', Sample))
W008negative <- W008_G4A_Dataset %>% filter(grepl('negative', Sample))
#W009negative <- W009_G4A_Dataset %>% filter(grepl('negative', Sample))
#W010negative <- W010_G4A_Dataset %>% filter(grepl('negative', Sample))

# Create a data frame "Negativecontrol" with all the negative controls 
Negativecontrol <- rbind(W000negative,W001negative,W002negative,W003negative,W004negative,W005negative,W006negative,W007negative,W008negative) #,W009negative) #,W010negative)

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
W000positive <- W000_G4A_Dataset %>% filter(grepl('positive', Sample))
W001positive <- W001_G4A_Dataset %>% filter(grepl('positive', Sample))
W002positive <- W002_G4A_Dataset %>% filter(grepl('positive', Sample))
W003positive <- W003_G4A_Dataset %>% filter(grepl('positive', Sample))
W004positive <- W004_G4A_Dataset %>% filter(grepl('positive', Sample))
W005positive <- W005_G4A_Dataset %>% filter(grepl('positive', Sample))
W006positive <- W006_G4A_Dataset %>% filter(grepl('positive', Sample))
W007positive <- W007_G4A_Dataset %>% filter(grepl('positive', Sample))
W008positive <- W008_G4A_Dataset %>% filter(grepl('positive', Sample))
#W009positive <- W009_G4A_Dataset %>% filter(grepl('positive', Sample))
#W010positive <- W010_G4A_Dataset %>% filter(grepl('positive', Sample))

Positivecontrol <- rbind(W000positive,W001positive,W002positive,W003positive,W004positive,W005positive,W006positive,W007positive,W008positive) #,W009positive) #,W010positive)
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
forFibreCount0<- W000_G4A_Dataset[!(W000_G4A_Dataset$Sample=="MP_W000_G4A_positive_B" | W000_G4A_Dataset$Sample=="MP_W000_G4A_negative_B"),]
forFibreCount1<- W001_G4A_Dataset[!(W001_G4A_Dataset$Sample=="MP_W001_G4A_positive_B" | W001_G4A_Dataset$Sample=="MP_W001_G4A_negative_B"),]
forFibreCount2<- W002_G4A_Dataset[!(W002_G4A_Dataset$Sample=="MP_W002_G4A_positive_B" | W002_G4A_Dataset$Sample=="MP_W002_G4A_negative_B"),]
forFibreCount3<- W003_G4A_Dataset[!(W003_G4A_Dataset$Sample=="MP_W003_G4A_positive_B" | W003_G4A_Dataset$Sample=="MP_W003_G4A_negative_B"),]
forFibreCount4<- W004_G4A_Dataset[!(W004_G4A_Dataset$Sample=="MP_W004_G4A_positive_B" | W004_G4A_Dataset$Sample=="MP_W004_G4A_negative_B"),]
forFibreCount5<- W005_G4A_Dataset[!(W005_G4A_Dataset$Sample=="MP_W005_G4A_positive_B" | W005_G4A_Dataset$Sample=="MP_W005_G4A_negative_B"),]
forFibreCount6<- W006_G4A_Dataset[!(W006_G4A_Dataset$Sample=="MP_W006_G4A_positive_B" | W006_G4A_Dataset$Sample=="MP_W006_G4A_negative_B"),]
forFibreCount7<- W007_G4A_Dataset[!(W007_G4A_Dataset$Sample=="MP_W007_G4A_positive_B" | W007_G4A_Dataset$Sample=="MP_W007_G4A_negative_B"),]
forFibreCount8<- W008_G4A_Dataset[!(W008_G4A_Dataset$Sample=="MP_W008_G4A_positive_B" | W008_G4A_Dataset$Sample=="MP_W008_G4A_negative_B"),]
#forFibreCount9<- W009_G4A_Dataset[!(W009_G4A_Dataset$Sample=="MP_W009_G4A_positive_B" | W009_G4A_Dataset$Sample=="MP_W009_G4A_negative_B"),]
#forFibreCount10<- W010_G4A_Dataset[!(W010_G4A_Dataset$Sample=="MP_W010_G4A_positive_B" | W010_G4A_Dataset$Sample=="MP_W010_G4A_negative_B"),]

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
BackgroundW008 <- forFibreCount8 %>%
  dplyr::select(Coder,`Before transfer`)
#BackgroundW009 <- forFibreCount9 %>%
#  dplyr::select(Coder,`Before transfer`)
#BackgroundW010 <- forFibreCount10 %>%
#  dplyr::select(Coder,`Before transfer`)

#### Combine all the dataframe ####
BackgroundFibreCount <- rbind(BackgroundW000,BackgroundW001,BackgroundW002,BackgroundW003,BackgroundW004,BackgroundW005,BackgroundW006,BackgroundW007,BackgroundW008) #,BackgroundW009) #,BackgroundW010)
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
TransferW008 <- forFibreCount8 %>%
  dplyr::select(Coder,`After transfer`)
#TransferW009 <- forFibreCount9 %>%
#  dplyr::select(Coder,`After transfer`)
#TransferW010 <- forFibreCount10 %>%
#  dplyr::select(Coder,`After transfer`)

#### Combine all the dataframe ####
TransferFibreCount_G4A <- rbind(TransferW000,TransferW001,TransferW002,TransferW003,TransferW004,TransferW005,TransferW006,TransferW007,TransferW008) #,TransferW009) #,TransferW010)
names(TransferFibreCount_G4A) <- c("group", "value")
# write.table(TransferFibreCount_G4A, file = "Fibre Count - Transfer.csv", quote = F, sep = ",", row.names = F)

#### Bar plots of data by group - distribution ####
histogram(~ value | group,data=TransferFibreCount_G4A,layout=c(3,3),
          xlab="Number of fibres")

#### Create a table with descriptive statistics ####
meanAtr_G4A <- aggregate(value ~  group, TransferFibreCount_G4A, function(x) {round(mean(x), digits=2)})
SDAtr_G4A <- aggregate(value ~  group, TransferFibreCount_G4A, function(x) {round(SD(x), digits=2)})
SD2Atr_G4A <- round(sqrt((SDAtr_G4A$value^2)+(0.95^2)),digits=2)
medianAtr_G4A <- aggregate(value ~  group, TransferFibreCount_G4A, median)
datatableAtr_G4A <- cbind(meanAtr_G4A, medianAtr_G4A$value, SDAtr_G4A$value, SD2Atr_G4A)
names(datatableAtr_G4A) <- c("Wash number", "Average", "median", "SD", "SD2")
datatableAtr_G4A$Forthesis <- paste(datatableAtr_G4A$Average, datatableAtr_G4A$SD, sep=" ± ")
#write.table(datatableAtr_G4A, file = "Stats_Atr red.csv", quote = F, sep = ",", row.names = F)

write.table(TransferFibreCount_G4A, file = "Transfer_Fibre_Count.csv", quote = F, sep = ",", row.names = F)

#### GRAPH - FIGURE 4-8 ####
pAtr_G4A <- ggplot(TransferFibreCount_G4A, aes(x=group, y=value)) +
  geom_boxplot() +
  #scale_x_discrete(labels = every_n_labeler(5)) +
  stat_summary(fun = mean, colour="darkred",
               geom="point",position=position_dodge(width=0.75)) +
  stat_summary(fun = mean, colour="darkred", aes(group=1),
               geom="line", lwd=1, lty=1) +
  ylim(0,30)+
  labs(x="\nWash number", y="Number of Fibre\n") +
  theme_classic(base_family = "Arial", base_size = 14) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.background = element_rect(fill="grey95",size=1, linetype="solid", colour="grey80"),
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5))
show(pAtr_G4A)
#ggsave("Fibre Count boxplot_ATr_G4A.png", pAtr_G4A, width = 6, height = 7, units = "in", dpi=150, path = "Results")

#################################################################################################
#####                               FIBRE ANALYSIS GARMENT 4B                               #####
#################################################################################################
#### Assign a Coder to each wash ####
W000_G4B_Dataset$Coder <- "W000"
W001_G4B_Dataset$Coder <- "W001"
W002_G4B_Dataset$Coder <- "W002"
W003_G4B_Dataset$Coder <- "W003"
W004_G4B_Dataset$Coder <- "W004"
W005_G4B_Dataset$Coder <- "W005"
W006_G4B_Dataset$Coder <- "W006"
W007_G4B_Dataset$Coder <- "W007"
W008_G4B_Dataset$Coder <- "W008"
#W009_G4B_Dataset$Coder <- "W009"
#W010_G4B_Dataset$Coder <- "W010"

#########################################################
#####                     Controls                  #####
#########################################################
# Negative controls
W000negative <- W000_G4B_Dataset %>% filter(grepl('negative', Sample))
W001negative <- W001_G4B_Dataset %>% filter(grepl('negative', Sample))
W002negative <- W002_G4B_Dataset %>% filter(grepl('negative', Sample))
W003negative <- W003_G4B_Dataset %>% filter(grepl('negative', Sample))
W004negative <- W004_G4B_Dataset %>% filter(grepl('negative', Sample))
W005negative <- W005_G4B_Dataset %>% filter(grepl('negative', Sample))
W006negative <- W006_G4B_Dataset %>% filter(grepl('negative', Sample))
W007negative <- W007_G4B_Dataset %>% filter(grepl('negative', Sample))
W008negative <- W008_G4B_Dataset %>% filter(grepl('negative', Sample))
#W009negative <- W009_G4B_Dataset %>% filter(grepl('negative', Sample))
#W010negative <- W010_G4B_Dataset %>% filter(grepl('negative', Sample))

# Create a data frame "Negativecontrol" with all the negative controls 
Negativecontrol <- rbind(W000negative,W001negative,W002negative,W003negative,W004negative,W005negative,W006negative,W007negative,W008negative) #,W009negative) #,W010negative)

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
W000positive <- W000_G4B_Dataset %>% filter(grepl('positive', Sample))
W001positive <- W001_G4B_Dataset %>% filter(grepl('positive', Sample))
W002positive <- W002_G4B_Dataset %>% filter(grepl('positive', Sample))
W003positive <- W003_G4B_Dataset %>% filter(grepl('positive', Sample))
W004positive <- W004_G4B_Dataset %>% filter(grepl('positive', Sample))
W005positive <- W005_G4B_Dataset %>% filter(grepl('positive', Sample))
W006positive <- W006_G4B_Dataset %>% filter(grepl('positive', Sample))
W007positive <- W007_G4B_Dataset %>% filter(grepl('positive', Sample))
W008positive <- W008_G4B_Dataset %>% filter(grepl('positive', Sample))
#W009positive <- W009_G4B_Dataset %>% filter(grepl('positive', Sample))
#W010positive <- W010_G4B_Dataset %>% filter(grepl('positive', Sample))

Positivecontrol <- rbind(W000positive,W001positive,W002positive,W003positive,W004positive,W005positive,W006positive,W007positive,W008positive) #,W009positive) #,W010positive)
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
forFibreCount0<- W000_G4B_Dataset[!(W000_G4B_Dataset$Sample=="MP_W000_G4B_positive_B" | W000_G4B_Dataset$Sample=="MP_W000_G4B_negative_B"),]
forFibreCount1<- W001_G4B_Dataset[!(W001_G4B_Dataset$Sample=="MP_W001_G4B_positive_B" | W001_G4B_Dataset$Sample=="MP_W001_G4B_negative_B"),]
forFibreCount2<- W002_G4B_Dataset[!(W002_G4B_Dataset$Sample=="MP_W002_G4B_positive_B" | W002_G4B_Dataset$Sample=="MP_W002_G4B_negative_B"),]
forFibreCount3<- W003_G4B_Dataset[!(W003_G4B_Dataset$Sample=="MP_W003_G4B_positive_B" | W003_G4B_Dataset$Sample=="MP_W003_G4B_negative_B"),]
forFibreCount4<- W004_G4B_Dataset[!(W004_G4B_Dataset$Sample=="MP_W004_G4B_positive_B" | W004_G4B_Dataset$Sample=="MP_W004_G4B_negative_B"),]
forFibreCount5<- W005_G4B_Dataset[!(W005_G4B_Dataset$Sample=="MP_W005_G4B_positive_B" | W005_G4B_Dataset$Sample=="MP_W005_G4B_negative_B"),]
forFibreCount6<- W006_G4B_Dataset[!(W006_G4B_Dataset$Sample=="MP_W006_G4B_positive_B" | W006_G4B_Dataset$Sample=="MP_W006_G4B_negative_B"),]
forFibreCount7<- W007_G4B_Dataset[!(W007_G4B_Dataset$Sample=="MP_W007_G4B_positive_B" | W007_G4B_Dataset$Sample=="MP_W007_G4B_negative_B"),]
forFibreCount8<- W008_G4B_Dataset[!(W008_G4B_Dataset$Sample=="MP_W008_G4B_positive_B" | W008_G4B_Dataset$Sample=="MP_W008_G4B_negative_B"),]
#forFibreCount9<- W009_G4B_Dataset[!(W009_G4B_Dataset$Sample=="MP_W009_G4B_positive_B" | W009_G4B_Dataset$Sample=="MP_W009_G4B_negative_B"),]
#forFibreCount10<- W010_G4B_Dataset[!(W010_G4B_Dataset$Sample=="MP_W010_G4B_positive_B" | W010_G4B_Dataset$Sample=="MP_W010_G4B_negative_B"),]

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
BackgroundW008 <- forFibreCount8 %>%
  dplyr::select(Coder,`Before transfer`)
#BackgroundW009 <- forFibreCount9 %>%
#  dplyr::select(Coder,`Before transfer`)
#BackgroundW010 <- forFibreCount10 %>%
#  dplyr::select(Coder,`Before transfer`)

#### Combine all the dataframe ####
BackgroundFibreCount <- rbind(BackgroundW000,BackgroundW001,BackgroundW002,BackgroundW003,BackgroundW004,BackgroundW005,BackgroundW006,BackgroundW007,BackgroundW008) #,BackgroundW009) #,BackgroundW010)
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
TransferW008 <- forFibreCount8 %>%
  dplyr::select(Coder,`After transfer`)
#TransferW009 <- forFibreCount9 %>%
#  dplyr::select(Coder,`After transfer`)
#TransferW010 <- forFibreCount10 %>%
#  dplyr::select(Coder,`After transfer`)

#### Combine all the dataframe ####
TransferFibreCount_G4B <- rbind(TransferW000,TransferW001,TransferW002,TransferW003,TransferW004,TransferW005,TransferW006,TransferW007,TransferW008) #,TransferW009) #,TransferW010)
names(TransferFibreCount_G4B) <- c("group", "value")
# write.table(TransferFibreCount_G4B, file = "Fibre Count - Transfer.csv", quote = F, sep = ",", row.names = F)

#### Bar plots of data by group - distribution ####
histogram(~ value | group,data=TransferFibreCount_G4B,layout=c(3,3),
          xlab="Number of fibres")

#### Create a table with descriptive statistics ####
meanAtr_G4B <- aggregate(value ~  group, TransferFibreCount_G4B, function(x) {round(mean(x), digits=2)})
SDAtr_G4B <- aggregate(value ~  group, TransferFibreCount_G4B, function(x) {round(SD(x), digits=2)})
SD2Atr_G4B <- round(sqrt((SDAtr_G4B$value^2)+(0.95^2)),digits=2)
medianAtr_G4B <- aggregate(value ~  group, TransferFibreCount_G4B, median)
datatableAtr_G4B <- cbind(meanAtr_G4B, medianAtr_G4B$value, SDAtr_G4B$value, SD2Atr_G4B)
names(datatableAtr_G4B) <- c("Wash number", "Average", "median", "SD", "SD2")
datatableAtr_G4B$Forthesis <- paste(datatableAtr_G4B$Average, datatableAtr_G4B$SD, sep=" ± ")
#write.table(datatableAtr_G4B, file = "Stats_Atr red.csv", quote = F, sep = ",", row.names = F)

write.table(TransferFibreCount_G4B, file = "Transfer_Fibre_Count.csv", quote = F, sep = ",", row.names = F)

#### GRAPH - FIGURE 4-8 ####
pAtr_G4B <- ggplot(TransferFibreCount_G4B, aes(x=group, y=value)) +
  geom_boxplot() +
  #scale_x_discrete(labels = every_n_labeler(5)) +
  stat_summary(fun = mean, colour="darkred",
               geom="point",position=position_dodge(width=0.75)) +
  stat_summary(fun = mean, colour="darkred", aes(group=1),
               geom="line", lwd=1, lty=1) +
  ylim(0,30)+
  labs(x="\nWash number", y="Number of Fibre\n") +
  theme_classic(base_family = "Arial", base_size = 14) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.background = element_rect(fill="grey95",size=1, linetype="solid", colour="grey80"),
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5))
show(pAtr_G4B)
#ggsave("Fibre Count boxplot_ATr_G4B.png", pAtr_G4B, width = 6, height = 7, units = "in", dpi=150, path = "Results")

#################################################################################################
#####                               FIBRE ANALYSIS GARMENT 4C                               #####
#################################################################################################
#### Assign a Coder to each wash ####
W000_G4C_Dataset$Coder <- "W000"
W001_G4C_Dataset$Coder <- "W001"
W002_G4C_Dataset$Coder <- "W002"
W003_G4C_Dataset$Coder <- "W003"
W004_G4C_Dataset$Coder <- "W004"
W005_G4C_Dataset$Coder <- "W005"
W006_G4C_Dataset$Coder <- "W006"
W007_G4C_Dataset$Coder <- "W007"
W008_G4C_Dataset$Coder <- "W008"
#W009_G4C_Dataset$Coder <- "W009"
#W010_G4C_Dataset$Coder <- "W010"

#########################################################
#####                     Controls                  #####
#########################################################
# Negative controls
W000negative <- W000_G4C_Dataset %>% filter(grepl('negative', Sample))
W001negative <- W001_G4C_Dataset %>% filter(grepl('negative', Sample))
W002negative <- W002_G4C_Dataset %>% filter(grepl('negative', Sample))
W003negative <- W003_G4C_Dataset %>% filter(grepl('negative', Sample))
W004negative <- W004_G4C_Dataset %>% filter(grepl('negative', Sample))
W005negative <- W005_G4C_Dataset %>% filter(grepl('negative', Sample))
W006negative <- W006_G4C_Dataset %>% filter(grepl('negative', Sample))
W007negative <- W007_G4C_Dataset %>% filter(grepl('negative', Sample))
W008negative <- W008_G4C_Dataset %>% filter(grepl('negative', Sample))
#W009negative <- W009_G4C_Dataset %>% filter(grepl('negative', Sample))
#W010negative <- W010_G4C_Dataset %>% filter(grepl('negative', Sample))

# Create a data frame "Negativecontrol" with all the negative controls 
Negativecontrol <- rbind(W000negative,W001negative,W002negative,W003negative,W004negative,W005negative,W006negative,W007negative,W008negative) #,W009negative) #,W010negative)

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
W000positive <- W000_G4C_Dataset %>% filter(grepl('positive', Sample))
W001positive <- W001_G4C_Dataset %>% filter(grepl('positive', Sample))
W002positive <- W002_G4C_Dataset %>% filter(grepl('positive', Sample))
W003positive <- W003_G4C_Dataset %>% filter(grepl('positive', Sample))
W004positive <- W004_G4C_Dataset %>% filter(grepl('positive', Sample))
W005positive <- W005_G4C_Dataset %>% filter(grepl('positive', Sample))
W006positive <- W006_G4C_Dataset %>% filter(grepl('positive', Sample))
W007positive <- W007_G4C_Dataset %>% filter(grepl('positive', Sample))
W008positive <- W008_G4C_Dataset %>% filter(grepl('positive', Sample))
#W009positive <- W009_G4C_Dataset %>% filter(grepl('positive', Sample))
#W010positive <- W010_G4C_Dataset %>% filter(grepl('positive', Sample))

Positivecontrol <- rbind(W000positive,W001positive,W002positive,W003positive,W004positive,W005positive,W006positive,W007positive,W008positive) #,W009positive) #,W010positive)
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
forFibreCount0<- W000_G4C_Dataset[!(W000_G4C_Dataset$Sample=="MP_W000_G4C_positive_B" | W000_G4C_Dataset$Sample=="MP_W000_G4C_negative_B"),]
forFibreCount1<- W001_G4C_Dataset[!(W001_G4C_Dataset$Sample=="MP_W001_G4C_positive_B" | W001_G4C_Dataset$Sample=="MP_W001_G4C_negative_B"),]
forFibreCount2<- W002_G4C_Dataset[!(W002_G4C_Dataset$Sample=="MP_W002_G4C_positive_B" | W002_G4C_Dataset$Sample=="MP_W002_G4C_negative_B"),]
forFibreCount3<- W003_G4C_Dataset[!(W003_G4C_Dataset$Sample=="MP_W003_G4C_positive_B" | W003_G4C_Dataset$Sample=="MP_W003_G4C_negative_B"),]
forFibreCount4<- W004_G4C_Dataset[!(W004_G4C_Dataset$Sample=="MP_W004_G4C_positive_B" | W004_G4C_Dataset$Sample=="MP_W004_G4C_negative_B"),]
forFibreCount5<- W005_G4C_Dataset[!(W005_G4C_Dataset$Sample=="MP_W005_G4C_positive_B" | W005_G4C_Dataset$Sample=="MP_W005_G4C_negative_B"),]
forFibreCount6<- W006_G4C_Dataset[!(W006_G4C_Dataset$Sample=="MP_W006_G4C_positive_B" | W006_G4C_Dataset$Sample=="MP_W006_G4C_negative_B"),]
forFibreCount7<- W007_G4C_Dataset[!(W007_G4C_Dataset$Sample=="MP_W007_G4C_positive_B" | W007_G4C_Dataset$Sample=="MP_W007_G4C_negative_B"),]
forFibreCount8<- W008_G4C_Dataset[!(W008_G4C_Dataset$Sample=="MP_W008_G4C_positive_B" | W008_G4C_Dataset$Sample=="MP_W008_G4C_negative_B"),]
#forFibreCount9<- W009_G4C_Dataset[!(W009_G4C_Dataset$Sample=="MP_W009_G4C_positive_B" | W009_G4C_Dataset$Sample=="MP_W009_G4C_negative_B"),]
#forFibreCount10<- W010_G4C_Dataset[!(W010_G4C_Dataset$Sample=="MP_W010_G4C_positive_B" | W010_G4C_Dataset$Sample=="MP_W010_G4C_negative_B"),]

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
BackgroundW008 <- forFibreCount8 %>%
  dplyr::select(Coder,`Before transfer`)
#BackgroundW009 <- forFibreCount9 %>%
#  dplyr::select(Coder,`Before transfer`)
#BackgroundW010 <- forFibreCount10 %>%
#  dplyr::select(Coder,`Before transfer`)

#### Combine all the dataframe ####
BackgroundFibreCount <- rbind(BackgroundW000,BackgroundW001,BackgroundW002,BackgroundW003,BackgroundW004,BackgroundW005,BackgroundW006,BackgroundW007,BackgroundW008) #,BackgroundW009) #,BackgroundW010)
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
TransferW008 <- forFibreCount8 %>%
  dplyr::select(Coder,`After transfer`)
#TransferW009 <- forFibreCount9 %>%
#  dplyr::select(Coder,`After transfer`)
#TransferW010 <- forFibreCount10 %>%
#  dplyr::select(Coder,`After transfer`)

#### Combine all the dataframe ####
TransferFibreCount_G4C <- rbind(TransferW000,TransferW001,TransferW002,TransferW003,TransferW004,TransferW005,TransferW006,TransferW007,TransferW008) #,TransferW009) #,TransferW010)
names(TransferFibreCount_G4C) <- c("group", "value")
# write.table(TransferFibreCount_G4C, file = "Fibre Count - Transfer.csv", quote = F, sep = ",", row.names = F)

#### Bar plots of data by group - distribution ####
histogram(~ value | group,data=TransferFibreCount_G4C,layout=c(3,3),
          xlab="Number of fibres")

#### Create a table with descriptive statistics ####
meanAtr_G4C <- aggregate(value ~  group, TransferFibreCount_G4C, function(x) {round(mean(x), digits=2)})
SDAtr_G4C <- aggregate(value ~  group, TransferFibreCount_G4C, function(x) {round(SD(x), digits=2)})
SD2Atr_G4C <- round(sqrt((SDAtr_G4C$value^2)+(0.95^2)),digits=2)
medianAtr_G4C <- aggregate(value ~  group, TransferFibreCount_G4C, median)
datatableAtr_G4C <- cbind(meanAtr_G4C, medianAtr_G4C$value, SDAtr_G4C$value, SD2Atr_G4C)
names(datatableAtr_G4C) <- c("Wash number", "Average", "median", "SD", "SD2")
datatableAtr_G4C$Forthesis <- paste(datatableAtr_G4C$Average, datatableAtr_G4C$SD, sep=" ± ")
#write.table(datatableAtr_G4C, file = "Stats_Atr red.csv", quote = F, sep = ",", row.names = F)

write.table(TransferFibreCount_G4C, file = "Transfer_Fibre_Count.csv", quote = F, sep = ",", row.names = F)

#### GRAPH - FIGURE 4-8 ####
pAtr_G4C <- ggplot(TransferFibreCount_G4C, aes(x=group, y=value)) +
  geom_boxplot() +
  #scale_x_discrete(labels = every_n_labeler(5)) +
  stat_summary(fun = mean, colour="darkred",
               geom="point",position=position_dodge(width=0.75)) +
  stat_summary(fun = mean, colour="darkred", aes(group=1),
               geom="line", lwd=1, lty=1) +
  ylim(0,30)+
  labs(x="\nWash number", y="Number of Fibre\n") +
  theme_classic(base_family = "Arial", base_size = 14) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.background = element_rect(fill="grey95",size=1, linetype="solid", colour="grey80"),
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5))
show(pAtr_G4C)
#ggsave("Fibre Count boxplot_ATr_G4C.png", pAtr_G4C, width = 6, height = 7, units = "in", dpi=150, path = "Results")

#########################################################
#####               GARMENT 4 COMBINED              #####
#########################################################
TransferFibreCount_G4A$Coder <-"Garment 4A"
TransferFibreCount_G4B$Coder <-"Garment 4B"
TransferFibreCount_G4C$Coder <-"Garment 4C"
G4_TransferFibreCount_Total <- rbind(TransferFibreCount_G4A, TransferFibreCount_G4B, TransferFibreCount_G4C)

pAtr_G4Total <- ggplot(G4_TransferFibreCount_Total, aes(x=group, y=value,fill=Coder)) +
  geom_boxplot() +
  labs(x="\nWash number", y="Number of Fibre\n") +
  scale_fill_brewer(palette = "Reds")+
  theme_classic(base_family = "Arial", base_size = 14) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.background = element_rect(fill="grey95",size=1, linetype="solid", colour="grey80"),
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5))
show(pAtr_G4Total)
ggsave("Fibre Count boxplot_ATr_G4.png", pAtr_G4Total, width = 6, height = 7, units = "in", dpi=150, path = "Results")

pAtr_G4Total_2nd <- ggplot(G4_TransferFibreCount_Total, aes(x=group, y=value)) +
  geom_boxplot() +
  #scale_x_discrete(labels = every_n_labeler(5)) +
  stat_summary(fun = mean, colour="darkred",
               geom="point",position=position_dodge(width=0.75)) +
  stat_summary(fun = mean, colour="darkred", aes(group=1),
               geom="line", lwd=1, lty=1) +
  ylim(0,40)+
  labs(x="\nWash number", y="Number of Fibre\n") +
  theme_classic(base_family = "Arial", base_size = 14) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.background = element_rect(fill="grey95",size=1, linetype="solid", colour="grey80"),
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5))
show(pAtr_G4Total_2nd)
ggsave("Fibre Count boxplot_ATr_G4.png", pAtr_G4Total_2nd, width = 6, height = 7, units = "in", dpi=150, path = "Results")


#################################################################################################
#####                             FIBRE ANALYSIS GARMENT COMBINED                           #####
#################################################################################################
TransferFibreCount_PhDG <- read.csv('./Transfer_Fibre_Count_red jumpers_PhD.csv', sep="," ,header = T,fileEncoding="UTF-8-BOM")
pAtr_PhD <- ggplot(TransferFibreCount_PhDG, aes(x=group, y=value)) +
  geom_boxplot() +
  scale_x_discrete(labels = every_n_labeler(5)) +
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
show(pAtr_PhD)

TransferFibreCount_PhDG <- TransferFibreCount_PhDG[is.element(TransferFibreCount_PhDG$group, c('W000','W001','W002','W003','W004','W005','W006','W007','W008','W009','W010')),]
TransferFibreCount_PhDG$Coder <-"5 garments - no detergent"
TransferFibreCount_G1$Coder <-"1 garment - no detergent"
TransferFibreCount_G2$Coder <-"1 garment -  detergent"
TransferFibreCount_G3$Coder <-"1 garment -  detergent + softener"
G4_TransferFibreCount_Total$Coder <-"12 garments - no detergent"
TransferFibreCount_Total <- rbind(TransferFibreCount_G1, TransferFibreCount_G2, TransferFibreCount_G3, G4_TransferFibreCount_Total) #,TransferFibreCount_PhDG)

pAtr_Total <- ggplot(TransferFibreCount_Total, aes(x=group, y=value,fill=Coder)) +
  geom_boxplot() +
  labs(x="\nWash number", y="Number of Fibre\n") +
  scale_fill_brewer(palette = "Reds")+
  theme_classic(base_family = "Arial", base_size = 14) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.background = element_rect(fill="grey95",size=1, linetype="solid", colour="grey80"),
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5))
show(pAtr_Total)
ggsave("Fibre Count boxplot_ATr_Total.png", pAtr_Total, width = 6, height = 7, units = "in", dpi=150, path = "Results")

pAtr_Total_2nd <- ggplot(TransferFibreCount_Total, aes(x=group, y=value,fill=Coder)) +
  geom_boxplot() +
  facet_wrap(~Coder)+
  stat_summary(fun = mean, colour="black",
               geom="point",position=position_dodge(width=0.75)) +
  stat_summary(fun = mean, colour="black", aes(group=1),
               geom="line", lwd=1, lty=1) +
  labs(x="\nWash number", y="Number of Fibre\n") +
  scale_fill_brewer(palette = "Reds")+
  theme_classic(base_family = "Arial", base_size = 14) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.background = element_rect(fill="grey95",size=1, linetype="solid", colour="grey80"),
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5))
show(pAtr_Total_2nd)
ggsave("Fibre Count boxplot_ATr_Total_2nd.png", pAtr_Total_2nd, width = 6, height = 7, units = "in", dpi=150, path = "Results")

# Combined results
pAtr_combined_pending <- ggarrange(pAtr_PhD+ rremove("ylab") + rremove("xlab"), vjust = 0.8, hjust = 0.8,                                                # First row with scatter plot
                                   ggarrange(pAtr_G1+ rremove("ylab") + rremove("xlab"),
                                             pAtr_G4Total_2nd+ rremove("ylab") + rremove("xlab"),
                                             ncol = 2, labels = c("B", "C"),vjust = 0.8, hjust = 0.8), # Second row with box and dot plots
                                   nrow = 2,
                                   labels = "A"                                        # Labels of the scatter plot
) 
ppAtr_combined <- annotate_figure(pAtr_combined_pending, left = textGrob("Number of fibres\n", rot = 90, vjust = 0.5, hjust = 0.5, gp = gpar(cex =1)),
                             bottom = textGrob("\nWash number", vjust = 0.5, hjust = 0.5,gp = gpar(cex = 1)));ppAtr_combined

ggsave("ppAtr_combined.png", ppAtr_combined, width = 6, height = 6, units = "in", dpi=300, path = "Results")
