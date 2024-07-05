#########################################################
#####             SHEDDING VS TRANSFER              #####
#########################################################

# ------------------------------------------------------------------------
# Section 1: Pearson correlation
# ------------------------------------------------------------------------
#### Intermediate Data Visualisation ####
# Prepare the data
forplotTotVcotton$Transfer <- paste0("W", sprintf("%03d", forplotTotVcotton$Transfer))
forplotTotVcottonD$Transfer <- paste0("W", sprintf("%03d", forplotTotVcottonD$Transfer))
forplotTotVcottonDC$Transfer <- paste0("W", sprintf("%03d", forplotTotVcottonDC$Transfer))
forplotTot5_Vcotton$Transfer <- paste0("W", sprintf("%03d", forplotTot5_Vcotton$Transfer))
forplotTot12_Vcotton$Transfer <- paste0("W", sprintf("%03d", forplotTot12_Vcotton$Transfer))
forplotTotRcotton$Transfer <- paste0("W", sprintf("%03d", forplotTotRcotton$Transfer))

Wash_codes <- c("W000", "W001", "W003","W005","W007","W009","W011","W013","W015")

# Loop through each Wash code, filter the dataframe, and create a new variable in the global environment
for (Wash_code in Wash_codes) {
  filtered_df <- forplotTotVcotton %>% filter(grepl(Wash_code, Transfer))
  assign(paste("results_Transfer_Vcotton", Wash_code, sep = "_"), filtered_df)
}

for (Wash_code in Wash_codes) {
  filtered_df <- forplotTotVcottonD %>% filter(grepl(Wash_code, Transfer))
  assign(paste("results_Transfer_VcottonD", Wash_code, sep = "_"), filtered_df)
}

for (Wash_code in Wash_codes) {
  filtered_df <- forplotTotVcottonDC %>% filter(grepl(Wash_code, Transfer))
  assign(paste("results_Transfer_VcottonDC", Wash_code, sep = "_"), filtered_df)
}

Wash_codes <- c("W000","W051")
for (Wash_code in Wash_codes) {
  filtered_df <- forplotTot5_Vcotton %>% filter(grepl(Wash_code, Transfer))
  assign(paste("results_Transfer_5_Vcotton", Wash_code, sep = "_"), filtered_df)
}

Wash_codes <- c("W000","W041")
for (Wash_code in Wash_codes) {
  filtered_df <- forplotTot12_Vcotton %>% filter(grepl(Wash_code, Transfer))
  assign(paste("results_Transfer_12_Vcotton", Wash_code, sep = "_"), filtered_df)
}

# Wash_codes <- c("W000","W015")
# for (Wash_code in Wash_codes) {
#   filtered_df <- forplotTotRcotton %>% filter(grepl(Wash_code, Transfer))
#   assign(paste("results_Transfer_Rcotton", Wash_code, sep = "_"), filtered_df)
# }

#combining results for statistic analysis
results_Vcotton_T <- rbind(results_Transfer_Vcotton_W000,results_Transfer_Vcotton_W001,results_Transfer_Vcotton_W003,results_Transfer_Vcotton_W005,
                      results_Transfer_Vcotton_W007,results_Transfer_Vcotton_W009,results_Transfer_Vcotton_W011,results_Transfer_Vcotton_W013,results_Transfer_Vcotton_W015)
results_VcottonD_T <- rbind(results_Transfer_VcottonD_W000,results_Transfer_VcottonD_W001,results_Transfer_VcottonD_W003,results_Transfer_VcottonD_W005,
                      results_Transfer_VcottonD_W007,results_Transfer_VcottonD_W009,results_Transfer_VcottonD_W011,results_Transfer_VcottonD_W013,results_Transfer_VcottonD_W015)
results_VcottonDC_T <- rbind(results_Transfer_VcottonDC_W000,results_Transfer_VcottonDC_W001,results_Transfer_VcottonDC_W003,results_Transfer_VcottonDC_W005,
                      results_Transfer_VcottonDC_W007,results_Transfer_VcottonDC_W009,results_Transfer_VcottonDC_W011,results_Transfer_VcottonDC_W013,results_Transfer_VcottonDC_W015)
results_5_Vcotton_T <- rbind(results_Transfer_5_Vcotton_W000,results_Transfer_5_Vcotton_W051)
results_12_Vcotton_T <- rbind(results_Transfer_12_Vcotton_W000,results_Transfer_12_Vcotton_W041)

results_Vcotton_S<- results_shedding_Vcotton %>% filter(grepl('800g', Weight))
results_VcottonD_S<- results_Shedding_VcottonD %>% filter(grepl('800g', Weight))
results_VcottonD_S<- results_VcottonD_S %>% filter(!grepl('W011', Wash))
results_VcottonDC_S<- results_Shedding_VcottonDC %>% filter(grepl('800g', Weight))
results_VcottonDC_S<- results_VcottonDC_S %>% filter(!grepl('W011', Wash))

results_5_Vcotton_S <- results_shedding_5_Vcotton
results_12_Vcotton_S <-results_shedding_12_Vcotton

# Ensure the data is ordered consistently by weight category if necessary
results_Vcotton_S <- results_Vcotton_S[order(results_Vcotton_S$Wash, results_Vcotton_S$Weight), ]
results_VcottonD_S <- results_VcottonD_S[order(results_VcottonD_S$Wash, results_VcottonD_S$Weight), ]
results_VcottonDC_S <- results_VcottonDC_S[order(results_VcottonDC_S$Wash, results_VcottonDC_S$Weight), ]

PearsonVcotton <- data.frame(
  Wash = results_Vcotton_T$Transfer,
  Transferred = as.numeric(round(results_Vcotton_T$value, digits=3)),
  Shed = as.numeric(round(results_Vcotton_S$Mean_Area, digits=2)),
  coder = "1st series"
)
PearsonVcotton$ratio <- round(pmax(PearsonVcotton$Transferred, PearsonVcotton$Shed) / 
                                pmin(PearsonVcotton$Transferred, PearsonVcotton$Shed), digits = 2)

PearsonVcottonD <- data.frame(
  Wash = results_VcottonD_T$Transfer,
  Transferred = as.numeric(round(results_VcottonD_T$value, digits=3)),
  Shed = as.numeric(round(results_VcottonD_S$Mean_Area, digits=2)),
  coder = "2nd series"
)
PearsonVcottonD$ratio <- round(pmax(PearsonVcottonD$Transferred, PearsonVcottonD$Shed) / 
                                pmin(PearsonVcottonD$Transferred, PearsonVcottonD$Shed), digits = 2)

PearsonVcottonDC <- data.frame(
  Wash = results_VcottonDC_T$Transfer,
  Transferred = as.numeric(round(results_VcottonDC_T$value, digits=3)),
  Shed = as.numeric(round(results_VcottonDC_S$Mean_Area, digits=2)),
  coder = "3rd series"
)
PearsonVcottonDC$ratio <- round(pmax(PearsonVcottonDC$Transferred, PearsonVcottonDC$Shed) / 
                                 pmin(PearsonVcottonDC$Transferred, PearsonVcottonDC$Shed), digits = 2)

Table_pearson_result <-rbind(PearsonVcotton,PearsonVcottonD,PearsonVcottonDC)
write.table(Table_pearson_result, file = "Results/Statistics/Table_pearson_result.csv", quote = F, sep = ",", row.names = F)

# Null hypothesis – There is no significant correlation between the transferred and shed fibres
# The alternative hypothesis – There is a significant correlation between the transferred and shed fibres
# set alpha level to 0.05
#Visualize data using scatter plots Fibres VS Wash number
# obtained with 1 garment
PearsonFW_Vcotton <- ggscatter(PearsonVcotton, x = "Shed", y = "Transferred",
                          add = "reg.line",
                          xlab = "Shed fibre area", ylab = "Transferred fibre area",
                          xlim = c(18, 240),
                          ylim = c(0, 0.25),
                          cor.coef = TRUE,
                          cor.coeff.args = list(method = "pearson", label.x = 20,label.y = 0.22, label.sep = "\n"))
PearsonFW_Vcotton

# obtained with 5 garments
PearsonFW_VcottonD <- ggscatter(PearsonVcottonD, x = "Shed", y = "Transferred",
                          add = "reg.line",
                          xlab = "Shed fibre area", ylab = "Transferred fibre area",
                          xlim = c(18, 240),
                          ylim = c(0, 0.25),
                          cor.coef = TRUE,
                          cor.coeff.args = list(method = "pearson", label.x = 20,label.y = 0.22, label.sep = "\n"))
PearsonFW_VcottonD   

# obtained with 12 garments
PearsonFW_VcottonDC <- ggscatter(PearsonVcottonDC, x = "Shed", y = "Transferred",
                          add = "reg.line",
                          xlab = "Shed fibre area", ylab = "Transferred fibre area",
                          xlim = c(18, 240),
                          ylim = c(0, 0.25),
                          cor.coef = TRUE,
                          cor.coeff.args = list(method = "pearson", label.x = 20,label.y = 0.22, label.sep = "\n"))
PearsonFW_VcottonDC                

#### Final graph - Figure XXX ####
PearsonFW_Vcotton <- PearsonFW_Vcotton + theme(plot.margin = margin(t = 20, l = 20, r = 20, unit = "pt"))
PearsonFW_VcottonD <- PearsonFW_VcottonD + theme(plot.margin = margin(t = 20, l = 20, r = 20, unit = "pt"))
PearsonFW_VcottonDC <- PearsonFW_VcottonDC + theme(plot.margin = margin(t = 20, l = 20, r = 20, unit = "pt"))

pPearson_combined_pending <- ggarrange(PearsonFW_Vcotton+ rremove("ylab") + rremove("xlab"),
                                       PearsonFW_VcottonD+ rremove("ylab") + rremove("xlab"),
                                       PearsonFW_VcottonDC+ rremove("ylab") + rremove("xlab"),
                                       nrow = 3, labels = c("A", "B", "C"),
                                       vjust = 0.9, hjust = 0.9)+
  theme(plot.margin = margin(0.5,0,0,0, "cm")) # in order (Top,left,bottom,right)

pPearson_combined <- annotate_figure(pPearson_combined_pending, left = textGrob("Transferred fibre area (mm\u00b2)\n", rot = 90, vjust = 0.5, hjust = 0.5, gp = gpar(cex =1)),
                                     bottom = textGrob("\nShed fibre area (mm\u00b2)", vjust = 0.5, hjust = 0.5,gp = gpar(cex = 1)));pPearson_combined
pPearson_combined

# To save the graph
ggsave("Figure 14 - Transfer VS Shedding.png", pPearson_combined, width = 8, height = 10, units = "in", dpi=600, path = "Results")

# #### Final graph - Figure XXX ####
# results_Vcotton_S$coder <- c("Shed")
# results_Vcotton_S <-results_Vcotton_S %>% select("Wash", "Mean_Area","SD_Area", "coder")
# names(results_Vcotton_S)<-c("Wash", "value", "sd", "coder")
# results_Vcotton_T$coder <- c("Transferred")
# results_Vcotton_T <-results_Vcotton_T %>% select("Transfer", "value","sd", "coder")
# names(results_Vcotton_T)<-c("Wash", "value", "sd", "coder")
# forplotVcotton <- rbind(results_Vcotton_S,results_Vcotton_T)
# 
# a = rep(c(100, 125, 150, 175, 200,225),2)
# b = rep(c(1:6), 2)
# plotVcotton <- ggplot(forplotVcotton, aes(x = factor(Wash, level = c('W000', 'W001', 'W003', 'W005', 'W007', 'W009', 'W011', 'W013', 'W015')),y= value, fill=coder))+
#   geom_bar(stat="identity", position=position_dodge(),colour="black")+
#   labs(x="\nWeight", y="Total fibre area (mm\u00b2)\n") +
#   theme_bw(base_family = "Arial", base_size = 12) +
#   ylim(-1,250)+
#   scale_fill_manual(values = c(brewer.pal(9, "Greys")[c(1, 9)])) +
#   theme(plot.title = element_text(hjust = 0.5),
#         legend.title = element_blank(),
#         legend.position = "bottom",
#         axis.text.x = element_text(angle = 0, vjust = 0.95, hjust=0.5))+
#   geom_errorbar(aes(ymin=value-sd, ymax=value+sd),width=.2,position=position_dodge(.9))
# # ggsave("Shedding_5_Vcotton.png", pSH_5_Vcotton, width = 10, height = 9, units = "in", dpi=300, path = "Results")
# plotVcotton
