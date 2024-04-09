#########################################################
#####             SHEDDING VS TRANSFER              #####
#########################################################

# ------------------------------------------------------------------------
# Section 1: Pearson correlation
# ------------------------------------------------------------------------
#### Intermediate Data Visualisation ####
# Prepare the data
wash_codes <- c("W000", "W001", "W003","W005","W007","W009","W011","W013","W015")
# Loop through each wash code, filter the dataframe, and create a new variable in the global environment
for (wash_code in wash_codes) {
  filtered_df <- meanAtrG1 %>% filter(grepl(wash_code, wash))
  assign(paste("results_Transfer_G1", wash_code, sep = "_"), filtered_df)
}

for (wash_code in wash_codes) {
  filtered_df <- meanAtrG2 %>% filter(grepl(wash_code, wash))
  assign(paste("results_Transfer_G2", wash_code, sep = "_"), filtered_df)
}

for (wash_code in wash_codes) {
  filtered_df <- meanAtrG3 %>% filter(grepl(wash_code, wash))
  assign(paste("results_Transfer_G3", wash_code, sep = "_"), filtered_df)
}

# for (wash_code in wash_codes) {
#   filtered_df <- meanAtrG4 %>% filter(grepl(wash_code, wash))
#   assign(paste("results_Transfer_G4", wash_code, sep = "_"), filtered_df)
# }
# 
# for (wash_code in wash_codes) {
#   filtered_df <- meanAtrG5 %>% filter(grepl(wash_code, wash))
#   assign(paste("results_Transfer_G5", wash_code, sep = "_"), filtered_df)
# }
# 
# for (wash_code in wash_codes) {
#   filtered_df <- meanAtrG6 %>% filter(grepl(wash_code, wash))
#   assign(paste("results_Transfer_G6", wash_code, sep = "_"), filtered_df)
# }

#combining results for statistic analysis
results_G1_T <- rbind(results_Transfer_G1_W000,results_Transfer_G1_W001,results_Transfer_G1_W003,results_Transfer_G1_W005,
                      results_Transfer_G1_W007,results_Transfer_G1_W009,results_Transfer_G1_W011,results_Transfer_G1_W013,results_Transfer_G1_W015)
results_G2_T <- rbind(results_Transfer_G2_W000,results_Transfer_G2_W001,results_Transfer_G2_W003,results_Transfer_G2_W005,
                      results_Transfer_G2_W007,results_Transfer_G2_W009,results_Transfer_G2_W011,results_Transfer_G2_W013,results_Transfer_G2_W015)
results_G3_T <- rbind(results_Transfer_G3_W000,results_Transfer_G3_W001,results_Transfer_G3_W003,results_Transfer_G3_W005,
                      results_Transfer_G3_W007,results_Transfer_G3_W009,results_Transfer_G3_W011,results_Transfer_G3_W013,results_Transfer_G3_W015)

results_G1_S<- results_shedding_G1 %>% filter(grepl('800g', Weight))
results_G2_S<- results_shedding_G2 %>% filter(grepl('800g', Weight))
results_G2_S<- results_G2_S %>% filter(!grepl('W011', Wash))
results_G3_S<- results_shedding_G3 %>% filter(grepl('800g', Weight))
results_G3_S<- results_G3_S %>% filter(!grepl('W011', Wash))

# Ensure the data is ordered consistently by weight category if necessary
results_G1_S <- results_G1_S[order(results_G1_S$Wash, results_G1_S$Weight), ]
results_G2_S <- results_G2_S[order(results_G2_S$Wash, results_G2_S$Weight), ]
results_G3_S <- results_G3_S[order(results_G3_S$Wash, results_G3_S$Weight), ]

PearsonG1 <- cbind(Wash=results_G1_T,results_G1_S$Mean_Area)
PearsonG1$ratio <- PearsonG1$`results_G1_S$Mean_Area`/PearsonG1$Wash.value
PearsonG1$coder <- c("1st series")
names(PearsonG1)<- c("Wash", "Transferred", "Shed","Ratio", "Wash series")
PearsonG2 <- cbind(Wash=results_G2_T,results_G2_S$Mean_Area)
PearsonG2$ratio <- PearsonG2$`results_G2_S$Mean_Area`/PearsonG2$Wash.value
PearsonG2$coder <- c("2nd series")
names(PearsonG2)<- c("Wash", "Transferred", "Shed","Ratio", "Wash series")
PearsonG3 <- cbind(Wash=results_G3_T,results_G3_S$Mean_Area)
PearsonG3$ratio <- PearsonG3$`results_G3_S$Mean_Area`/PearsonG3$Wash.value
PearsonG3$coder <- c("3rd series")
names(PearsonG3)<- c("Wash", "Transferred", "Shed","Ratio", "Wash series")
Table_pearson_result <-rbind(PearsonG1,PearsonG2,PearsonG3)
write.table(Table_pearson_result, file = "Results/Statistics/Table_pearson_result.csv", quote = F, sep = ",", row.names = F)

# Null hypothesis – There is no significant correlation between the transferred and shed fibres
# The alternative hypothesis – There is a significant correlation between the transferred and shed fibres
# set alpha level to 0.05
#Visualize data using scatter plots Fibres VS wash number
# obtained with 1 garment
PearsonFW_G1 <- ggscatter(PearsonG1, x = "Shed", y = "Transferred",
                          add = "reg.line",
                          xlab = "Shed fibre area", ylab = "Transferred fibre area",
                          xlim = c(18, 240),
                          ylim = c(0, 0.25),
                          cor.coef = TRUE,
                          cor.coeff.args = list(method = "pearson", label.x = 20,label.y = 0.22, label.sep = "\n"))
PearsonFW_G1

# obtained with 5 garments
PearsonFW_G2 <- ggscatter(PearsonG2, x = "Shed", y = "Transferred",
                          add = "reg.line",
                          xlab = "Shed fibre area", ylab = "Transferred fibre area",
                          xlim = c(18, 240),
                          ylim = c(0, 0.25),
                          cor.coef = TRUE,
                          cor.coeff.args = list(method = "pearson", label.x = 20,label.y = 0.22, label.sep = "\n"))
PearsonFW_G2   

# obtained with 12 garments
PearsonFW_G3 <- ggscatter(PearsonG3, x = "Shed", y = "Transferred",
                          add = "reg.line",
                          xlab = "Shed fibre area", ylab = "Transferred fibre area",
                          xlim = c(18, 240),
                          ylim = c(0, 0.25),
                          cor.coef = TRUE,
                          cor.coeff.args = list(method = "pearson", label.x = 20,label.y = 0.22, label.sep = "\n"))
PearsonFW_G3                

#### Final graph - Figure XXX ####
PearsonFW_G1 <- PearsonFW_G1 + theme(plot.margin = margin(t = 20, l = 20, r = 20, unit = "pt"))
PearsonFW_G2 <- PearsonFW_G2 + theme(plot.margin = margin(t = 20, l = 20, r = 20, unit = "pt"))
PearsonFW_G3 <- PearsonFW_G3 + theme(plot.margin = margin(t = 20, l = 20, r = 20, unit = "pt"))

pPearson_combined_pending <- ggarrange(PearsonFW_G1+ rremove("ylab") + rremove("xlab"),
                                       PearsonFW_G2+ rremove("ylab") + rremove("xlab"),
                                       PearsonFW_G3+ rremove("ylab") + rremove("xlab"),
                                       nrow = 3, labels = c("A", "B", "C"),
                                       vjust = 0.9, hjust = 0.9)+
  theme(plot.margin = margin(0.5,0,0,0, "cm")) # in order (Top,left,bottom,right)

pPearson_combined <- annotate_figure(pPearson_combined_pending, left = textGrob("Transferred fibre area\n", rot = 90, vjust = 0.5, hjust = 0.5, gp = gpar(cex =1)),
                                     bottom = textGrob("\nShed fibre area", vjust = 0.5, hjust = 0.5,gp = gpar(cex = 1)));pPearson_combined
pPearson_combined

# To save the graph
ggsave("Figure XXX - Transfer VS Shedding.png", pPearson_combined, width = 8, height = 10, units = "in", dpi=600, path = "Results")

