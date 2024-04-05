#############################################################
#####          Analysis - Transfer experiments          #####
#############################################################

# This R script is to generate the figures related to the Transfer experiments:

# ------------------------------------------------------------------------
# Section 1: Transfer - Washing series
# ------------------------------------------------------------------------
#### Data Cleaning and Processing ####
# Split the column based on "_"
Transfer_G1 <- separate(G1_Dataset, Sample, into = c("date", "exp", "wash","garment", "band", "sample"), sep = "_")
Transfer_G2 <- separate(G2_Dataset, Sample, into = c("date", "exp", "wash","garment", "band", "sample"), sep = "_")
Transfer_G3 <- separate(G3_Dataset, Sample, into = c("date", "exp", "wash","garment", "band", "sample"), sep = "_")
Transfer_G4 <- separate(G4_Dataset, Sample, into = c("wash","garment", "band","orientation", "sample"), sep = "_")
Transfer_G5 <- separate(G5_Dataset, Sample, into = c("wash","garment", "band","orientation", "sample"), sep = "_")
Transfer_G6 <- separate(G6_Dataset, Sample, into = c("date", "exp", "wash","garment", "band", "sample"), sep = "_")

# substract background
Transfer_G1$value <-  Transfer_G1$`After transfer` -  Transfer_G1$`Before transfer`
Transfer_G2$value <-  Transfer_G2$`After transfer` -  Transfer_G2$`Before transfer`
Transfer_G3$value <-  Transfer_G3$`After transfer` -  Transfer_G3$`Before transfer`
Transfer_G4$value <-  Transfer_G4$`After transfer` -  Transfer_G4$`Before transfer`
Transfer_G4$value <-  Transfer_G4$`After transfer` -  Transfer_G4$`Before transfer`
Transfer_G5$value <-  Transfer_G5$`After transfer` -  Transfer_G5$`Before transfer`
Transfer_G6$value <-  Transfer_G6$`After transfer` -  Transfer_G6$`Before transfer`

# Rename garment name in Transfer_G6 for more comprehension
Transfer_G6$garment<- gsub("G4A","G1",Transfer_G6$garment)
Transfer_G6$garment<- gsub("G4B","G2",Transfer_G6$garment)
Transfer_G6$garment<- gsub("G4C","G3",Transfer_G6$garment)

# to export 
Transfer_G1 <- Transfer_G1 %>% dplyr::select(wash, garment,`Before transfer`,`After transfer`,value)
Transfer_G2 <- Transfer_G2 %>% dplyr::select(wash, garment,`Before transfer`,`After transfer`,value)
Transfer_G3 <- Transfer_G3 %>% dplyr::select(wash, garment,`Before transfer`,`After transfer`,value)
Transfer_G4 <- Transfer_G4 %>% dplyr::select(wash, garment,`Before transfer`,`After transfer`,value)
Transfer_G5 <- Transfer_G5 %>% dplyr::select(wash, garment,`Before transfer`,`After transfer`,value)
Transfer_G6 <- Transfer_G6 %>% dplyr::select(wash, garment,`Before transfer`,`After transfer`,value)

# Select important column
Transfer_G1 <-  Transfer_G1 %>% dplyr::select(wash, garment,value)
Transfer_G2 <-  Transfer_G2 %>% dplyr::select(wash, garment,value)
Transfer_G3 <-  Transfer_G3 %>% dplyr::select(wash, garment,value)
Transfer_G4 <-  Transfer_G4 %>% dplyr::select(wash, garment,value)
Transfer_G5 <-  Transfer_G5 %>% dplyr::select(wash, garment,value)
Transfer_G6 <-  Transfer_G6 %>% dplyr::select(wash, garment,value)

# Create a list of dataframes
dataframes <- list(Transfer_G1,Transfer_G2,Transfer_G3, Transfer_G4, Transfer_G5, Transfer_G6)

# Create a function to compute statistics and write to CSV
compute_and_write_stats <- function(df, filename) {
  mean_df <- aggregate(value ~ wash, df, function(x) round(mean(x), digits = 2))
  sd_df <- aggregate(value ~ wash, df, function(x) round(sd(x), digits = 2))
  median_df <- aggregate(value ~ wash, df, function(x) round(median(x), digits = 2))
  
  datatable <- data.frame(
    "Wash number" = mean_df$wash,
    "Average" = mean_df$value,
    "Median" = median_df$value,
    "SD" = sd_df$value
  )
  
  write.table(datatable, file = filename, sep = ",", fileEncoding = "UTF-8")
}

# Loop through the dataframes and compute/write statistics
for (i in 1:length(dataframes)) {
  compute_and_write_stats(dataframes[[i]], paste("Results/Descriptive statistics - Garment ", c(1, 2, 3, 4, 5, 6)[i], ".csv", sep = ""))
}

#### Intermediate Data Visualisation ####
Transfer_G1[61,] <- c("W008", NA, NA)
Transfer_G1[62,] <- c("W010", NA, NA)
Transfer_G1[63,] <- c("W012", NA, NA)
Transfer_G1[64,] <- c("W014", NA, NA)
pAtr_G1 <- ggplot(Transfer_G1, aes(x=as.factor(wash), y=as.numeric(value))) +
  geom_boxplot() +
  #scale_x_discrete(labels = every_n_labeler(5)) +
  ylim(-5,40)+
  labs(x="\nWash number", y="Number of Fibres\n") +
  theme_bw(base_family = "Arial", base_size = 14) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.background = element_rect(fill="grey95",size=1, linetype="solid", colour="grey80"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5))

Transfer_G2[61,] <- c("W011", NA, NA)
Transfer_G2[62,] <- c("W012", NA, NA)
Transfer_G2[63,] <- c("W014", NA, NA)
pAtr_G2 <- ggplot(Transfer_G2, aes(x=as.factor(wash), y=as.numeric(value))) +
  geom_boxplot() +
  #scale_x_discrete(labels = every_n_labeler(5))+
  ylim(-5,40)+
  labs(x="\nWash number", y="Number of Fibres\n") +
  theme_bw(base_family = "Arial", base_size = 14) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.background = element_rect(fill="grey95",size=1, linetype="solid", colour="grey80"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5))

Transfer_G3[61,] <- c("W011", NA, NA)
Transfer_G3[62,] <- c("W012", NA, NA)
Transfer_G3[63,] <- c("W014", NA, NA)
pAtr_G3 <- ggplot(Transfer_G3, aes(x=as.factor(wash), y=as.numeric(value))) +
  geom_boxplot() +
  #scale_x_discrete(labels = every_n_labeler(5)) +
  ylim(-5,40)+
  labs(x="\nWash number", y="Number of Fibres\n") +
  theme_bw(base_family = "Arial", base_size = 14) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.background = element_rect(fill="grey95",size=1, linetype="solid", colour="grey80"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5))

Transfer_G4[61,] <- c("W006", NA, NA)
Transfer_G4[62,] <- c("W007", NA, NA)
Transfer_G4[63,] <- c("W008", NA, NA)
Transfer_G4[64,] <- c("W009", NA, NA)
Transfer_G4[65,] <- c("W011", NA, NA)
Transfer_G4[66,] <- c("W012", NA, NA)
Transfer_G4[67,] <- c("W013", NA, NA)
Transfer_G4[68,] <- c("W014", NA, NA)
pAtr_G4 <- ggplot(Transfer_G4, aes(x=as.factor(wash), y=as.numeric(value))) +
  geom_boxplot() +
  scale_x_discrete(labels = every_n_labeler(5)) +
  ylim(-5,40)+
  labs(x="\nWash number", y="Number of Fibres\n") +
  theme_bw(base_family = "Arial", base_size = 14) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.background = element_rect(fill="grey95",size=1, linetype="solid", colour="grey80"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5))

pAtr_G5 <- ggplot(Transfer_G5, aes(x=wash, y=value)) +
  geom_boxplot() +
  scale_x_discrete(labels = every_n_labeler(5)) +
  ylim(-5,40)+
  labs(x="\nWash number", y="Number of Fibres\n") +
  theme_bw(base_family = "Arial", base_size = 14) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.background = element_rect(fill="grey95",size=1, linetype="solid", colour="grey80"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5))

Transfer_G6[156,] <- c("W016", NA, NA)
Transfer_G6[157,] <- c("W017", NA, NA)
Transfer_G6[158,] <- c("W024", NA, NA)
Transfer_G6[159,] <- c("W026", NA, NA)
Transfer_G6[160,] <- c("W028", NA, NA)
Transfer_G6[161,] <- c("W030", NA, NA)
Transfer_G6[162,] <- c("W032", NA, NA)
Transfer_G6[163,] <- c("W034", NA, NA)
Transfer_G6[164,] <- c("W036", NA, NA)
Transfer_G6[165,] <- c("W038", NA, NA)
Transfer_G6[166,] <- c("W040", NA, NA)
pAtr_G6 <- ggplot(Transfer_G6, aes(x=as.factor(wash), y=as.numeric(value))) +
  geom_boxplot() +
  scale_x_discrete(labels = every_n_labeler(5)) +
  ylim(-5,40)+
  labs(x="\nWash number", y="Number of Fibres\n") +
  theme_bw(base_family = "Arial", base_size = 14) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.background = element_rect(fill="grey95",size=1, linetype="solid", colour="grey80"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5))
show(pAtr_G6)

#### Final graph - Figure XXX #### 
pAtr_G1 <- pAtr_G1 + theme(plot.margin = margin(t = 20, l = 20, unit = "pt"))
pAtr_G2 <- pAtr_G2 + theme(plot.margin = margin(t = 20, l = 20, unit = "pt"))
pAtr_G3 <- pAtr_G3 + theme(plot.margin = margin(t = 20, l = 20, unit = "pt"))

# Now, using ggarrange with labels
pCombined_pending <- ggarrange(
  pAtr_G1 + rremove("ylab") + rremove("xlab"),
  pAtr_G2 + rremove("ylab") + rremove("xlab"),
  pAtr_G3 + rremove("ylab") + rremove("xlab"),
  labels = c("A", "B", "C"),
  common.legend = TRUE, legend = "bottom",
  align = "hv",
  ncol = 1, nrow = 3,
  font.label = list(size = 12, color = "black", family = NULL, position = "top")
)

# Adding external annotations for titles and axis labels
pCombined <- annotate_figure(pCombined_pending, 
                             left = textGrob("Number of fibres", rot = 90, vjust = 0.5, hjust = 0.5, gp = gpar(cex =1)),
                             bottom = textGrob("Wash number", vjust = 0.5, hjust = 0.5, gp = gpar(cex = 1)))

print(pCombined)

# to save the graph
ggsave("Figure XXX - Transfer all garments1.png", pCombined, width =7, height = 8, units = "in", dpi=600,path = "Results")

#### Final graph - Figure XXX ####
pAtr_G4 <- pAtr_G4 + theme(plot.margin = margin(t = 20, l = 20, unit = "pt"))
pAtr_G5 <- pAtr_G5 + theme(plot.margin = margin(t = 20, l = 20, unit = "pt"))
pAtr_G6 <- pAtr_G6 + theme(plot.margin = margin(t = 20, l = 20, unit = "pt"))

# Now, using ggarrange with labels
pCombined_pending2 <- ggarrange(
  pAtr_G4 + rremove("ylab") + rremove("xlab"),
  pAtr_G5 + rremove("ylab") + rremove("xlab"),
  pAtr_G6 + rremove("ylab") + rremove("xlab"),
  labels = c("A", "B", "C"),
  common.legend = TRUE, legend = "bottom",
  align = "hv",
  ncol = 1, nrow = 3,
  font.label = list(size = 12, color = "black", family = NULL, position = "top")
)

# Adding external annotations for titles and axis labels
pCombined2 <- annotate_figure(pCombined_pending2, 
                             left = textGrob("Number of fibres", rot = 90, vjust = 0.5, hjust = 0.5, gp = gpar(cex =1)),
                             bottom = textGrob("Wash number", vjust = 0.5, hjust = 0.5, gp = gpar(cex = 1)))

print(pCombined2)

# to save the graph
ggsave("Figure XXX - Transfer all garments2.png", pCombined2, width =7, height = 8, units = "in", dpi=600,path = "Results")

#### Final graph - Figure XXX #### 
# data from the series involving washing a single donor garment
n=15
numS <- data.frame(setdiff(0:n, c(8,10,12,14)))
Transfer_G1 <- Transfer_G1[!is.na(Transfer_G1$value), ]
Transfer_G1$value <- as.numeric(Transfer_G1$value)
meanAtrG1 <- aggregate(value ~ wash, Transfer_G1, function(x) round(mean(x), digits = 2))
forplotTotG1 <- data.frame(cbind(numS, value =meanAtrG1$value))
names(forplotTotG1) <- c("Transfer", "value")
forplotTotG1$group <- c("No detergent, no conditioner")

# data from the series involving washing a single donor garment with detergent
n=15
numS <- data.frame(setdiff(0:n, c(11,12,14)))
Transfer_G2 <- Transfer_G2[!is.na(Transfer_G2$value), ]
Transfer_G2$value <- as.numeric(Transfer_G2$value)
meanAtrG2 <- aggregate(value ~ wash, Transfer_G2, function(x) round(mean(x), digits = 2))
forplotTotG2 <- data.frame(cbind(numS, value =meanAtrG2$value))
names(forplotTotG2) <- c("Transfer", "value")
forplotTotG2$group <- c("Detergent, no conditioner")

# data from the series involving washing a single donor garment with detergent and softener
n=15
numS <- data.frame(setdiff(0:n, c(11,12,14)))
Transfer_G3 <- Transfer_G3[!is.na(Transfer_G3$value), ]
Transfer_G3$value <- as.numeric(Transfer_G3$value)
meanAtrG3 <- aggregate(value ~ wash, Transfer_G3, function(x) round(mean(x), digits = 2))
forplotTotG3 <- data.frame(cbind(numS, value =meanAtrG3$value))
names(forplotTotG3) <- c("Transfer", "value")
forplotTotG3$group <- c("Detergent and conditioner")

# Combined all data
Toplot <- rbind(forplotTotG1,forplotTotG2,forplotTotG3)

# # Convert specific columns to different format
Toplot$Transfer <- as.numeric(Toplot$Transfer)
Toplot$value <- as.numeric(Toplot$value)
Toplot$group <- as.factor(Toplot$group)

# find the best visual fit
# for the first series involving washing a single donor garment
fitG1 <- lm(value~poly(Transfer,9,raw=TRUE), data=forplotTotG1)
a <- summary(fitG1)$adj.r.squared;a

# for the second series involving washing 5 donor garments
fitG2 <- lm(value~poly(Transfer,9,raw=TRUE), data=forplotTotG2)
b <- summary(fitG2)$adj.r.squared;b

# for the second series involving washing 5 donor garments
fitG3 <- lm(value~poly(Transfer,9,raw=TRUE), data=forplotTotG3)
c <- summary(fitG3)$adj.r.squared;c

# Set up a PNG device
png("./Results/Figure XXX - Transfer.png", width = 2400, height = 2100, res = 300)

# Adjust the margins (bottom, left, top, right)
par(mar = c(5, 4, 1, 2))  # Adjust the top margin to be smaller

# Plotting symbols and line types for each group
pch_values <- c(1, 2, 3)  # Different shapes: 1 = circle, 2 = triangle, 3 = plus
lty_values <- c(1, 2, 3)  # Different line types: 1 = solid, 2 = dashed, 3 = dotted

# Initial plot with first group only, to establish plot parameters
plot(Toplot$Transfer[Toplot$group == "No detergent, no conditioner"], Toplot$value[Toplot$group == "No detergent, no conditioner"],
     col="black", pch=pch_values[1],
     xlab='\nWash number',
     ylab='Average number of transferred fibres',
     xlim=range(Toplot$Transfer), 
     ylim=range(Toplot$value),
     xaxt='n',
     cex.axis = 1.2,
     cex.lab = 1.2,
     main="")

# Add other groups to the plot with points
points(Toplot$Transfer[Toplot$group == "Detergent, no conditioner"], Toplot$value[Toplot$group == "Detergent, no conditioner"], col="black", pch=pch_values[2])
points(Toplot$Transfer[Toplot$group == "Detergent and conditioner"], Toplot$value[Toplot$group == "Detergent and conditioner"], col="black", pch=pch_values[3])

# Define axis separately
axis(1, at=seq(0, 15, by=1), las=1, cex.axis = 1.2)

# Add legend
legend("topright", legend=c("No detergent, no conditioner", "Detergent, no conditioner", "Detergent and conditioner"),
       col="black", pch=pch_values, lty=lty_values, cex=1.2)

# Predict and plot smooth curves with different line types
splineG1 <- spline(Toplot$Transfer[Toplot$group == "No detergent, no conditioner"], Toplot$value[Toplot$group == "No detergent, no conditioner"], n = 200)
lines(splineG1$x, splineG1$y, col = 'black', lty=lty_values[1])

splineG2 <- spline(Toplot$Transfer[Toplot$group == "Detergent, no conditioner"], Toplot$value[Toplot$group == "Detergent, no conditioner"], n = 200)
lines(splineG2$x, splineG2$y, col = 'black', lty=lty_values[2])

splineG3 <- spline(Toplot$Transfer[Toplot$group == "Detergent and conditioner"], Toplot$value[Toplot$group == "Detergent and conditioner"], n = 200)
lines(splineG3$x, splineG3$y, col = 'black', lty=lty_values[3])

# End the plot device
dev.off()

#### Final graph - Figure XXX #### 
# data from the series involving washing a single r-cotton donor garment
n=15
numS <- data.frame(setdiff(0:n, c(6,7,8,9,11,12,13,14)))
Transfer_G4 <- Transfer_G4[!is.na(Transfer_G4$value), ]
Transfer_G4$value <- as.numeric(Transfer_G4$value)
meanAtrG4 <- aggregate(value ~ wash, Transfer_G4, function(x) round(mean(x), digits = 2))
forplotTotG4 <- data.frame(cbind(numS, value =meanAtrG4$value))
names(forplotTotG4) <- c("Transfer", "value")
forplotTotG4$group <- c("R-cotton textile")

# data from the series involving washing a single donor garment with detergent and softener
n=51
numS <- data.frame(setdiff(0:n, c()))
Transfer_G5 <- Transfer_G5[!is.na(Transfer_G5$value), ]
Transfer_G5$value <- as.numeric(Transfer_G5$value)
meanAtrG5 <- aggregate(value ~ wash, Transfer_G5, function(x) round(mean(x), digits = 2))
forplotTotG5 <- data.frame(cbind(numS, value =meanAtrG5$value))
names(forplotTotG5) <- c("Transfer", "value")
forplotTotG5$group <- c("5 v-cotton garment")

# data from the series involving washing a single donor garment with detergent
n=41
numS <- data.frame(setdiff(0:n, c(16,17,24,26,28,30,32,34,36,38,40)))
Transfer_G6 <- Transfer_G6[!is.na(Transfer_G6$value), ]
Transfer_G6$value <- as.numeric(Transfer_G6$value)
meanAtrG6 <- aggregate(value ~ wash, Transfer_G6, function(x) round(mean(x), digits = 2))
forplotTotG6 <- data.frame(cbind(numS, value =meanAtrG6$value))
names(forplotTotG6) <- c("Transfer", "value")
forplotTotG6$group <- c("12 v-cotton garment")

# Combined all data
Toplot <- rbind(forplotTotG4,forplotTotG5,forplotTotG6)

# Convert specific columns to different format
Toplot$Transfer <- as.numeric(Toplot$Transfer)
Toplot$value <- as.numeric(Toplot$value)
Toplot$group <- as.factor(Toplot$group)

# find the best visual fit
# for the first series involving washing a single donor garment
fitG4 <- lm(value~poly(Transfer,9,raw=TRUE), data=forplotTotG4)
a <- summary(fitG4)$adj.r.squared;a

# for the second series involving washing 5 donor garments
fitG5 <- lm(value~poly(Transfer,9,raw=TRUE), data=forplotTotG5)
b <- summary(fitG5)$adj.r.squared;b

# for the second series involving washing 5 donor garments
fitG6 <- lm(value~poly(Transfer,9,raw=TRUE), data=forplotTotG6)
c <- summary(fitG6)$adj.r.squared;c

#### Final graph - Figure XXX ####
# Set up a PNG device
png("./Results/Figure XXX - Transfer2.png", width = 2400, height = 2100, res = 300)

# Adjust the margins (bottom, left, top, right)
par(mar = c(5, 4, 1, 2))  # Adjust the top margin to be smaller

# Plotting symbols and line types for each group
pch_values <- c(1, 2, 3)  # Different shapes: 1 = circle, 2 = triangle, 3 = plus
lty_values <- c(1, 2, 3)  # Different line types: 1 = solid, 2 = dashed, 3 = dotted

# Initial plot with first group only, to establish plot parameters
plot(Toplot$Transfer[Toplot$group == "R-cotton textile"], Toplot$value[Toplot$group == "R-cotton textile"],
     col="black", pch=pch_values[1],
     xlab='\nWash number',
     ylab='Average number of transferred fibres',
     xlim=range(Toplot$Transfer), 
     ylim=range(Toplot$value),
     xaxt='n',
     cex.axis = 1.2,
     cex.lab = 1.2,
     main="")

# Add other groups to the plot with points
points(Toplot$Transfer[Toplot$group == "5 v-cotton garment"], Toplot$value[Toplot$group == "5 v-cotton garment"], col="black", pch=pch_values[2])
points(Toplot$Transfer[Toplot$group == "12 v-cotton garment"], Toplot$value[Toplot$group == "12 v-cotton garment"], col="black", pch=pch_values[3])

# Define axis separately
axis(1, at=seq(0, max(Toplot$Transfer), by=1), las=1, cex.axis = 1.2)

# Add legend
legend("topright", legend=c("R-cotton textile","5 v-cotton garment", "12 v-cotton garment"),
       col="black", pch=pch_values, lty=lty_values, cex=1.2)

# Predict and plot smooth curves with different line types
splineG4 <- spline(Toplot$Transfer[Toplot$group == "R-cotton textile"], Toplot$value[Toplot$group == "R-cotton textile"], n = 200)
lines(splineG4$x, splineG4$y, col = 'black', lty=lty_values[1])

splineG5 <- spline(Toplot$Transfer[Toplot$group == "5 v-cotton garment"], Toplot$value[Toplot$group == "5 v-cotton garment"], n = 200)
lines(splineG5$x, splineG5$y, col = 'black', lty=lty_values[2])

splineG6 <- spline(Toplot$Transfer[Toplot$group == "12 v-cotton garment"], Toplot$value[Toplot$group == "12 v-cotton garment"], n = 200)
lines(splineG6$x, splineG6$y, col = 'black', lty=lty_values[3])

# End the plot device
dev.off()

# ------------------------------------------------------------------------
# Section 1: Transfer - statistics
# ------------------------------------------------------------------------
## Create different dataframes with each wash
wash_codes <- c("W000", "W001", "W015")
# Loop through each wash code, filter the dataframe, and create a new variable in the global environment
for (wash_code in wash_codes) {
  filtered_df <- Transfer_G1 %>% filter(grepl(wash_code, wash))
  
  # Dynamically create variable names and assign dataframes to them
  assign(paste("results_Transfer_G1", wash_code, sep = "_"), filtered_df)
}

for (wash_code in wash_codes) {
  filtered_df <- Transfer_G2 %>% filter(grepl(wash_code, wash))
  
  # Dynamically create variable names and assign dataframes to them
  assign(paste("results_Transfer_G2", wash_code, sep = "_"), filtered_df)
}

for (wash_code in wash_codes) {
  filtered_df <- Transfer_G3 %>% filter(grepl(wash_code, wash))
  
  # Dynamically create variable names and assign dataframes to them
  assign(paste("results_Transfer_G3", wash_code, sep = "_"), filtered_df)
}

#combining results for statistic analysis
results_Transfer_W000 <- rbind(results_Transfer_G1_W000,results_Transfer_G2_W000,results_Transfer_G3_W000)
results_Transfer_W001 <- rbind(results_Transfer_G1_W001,results_Transfer_G2_W001,results_Transfer_G3_W001)
results_Transfer_W015 <- rbind(results_Transfer_G1_W015,results_Transfer_G2_W015,results_Transfer_G3_W000)

# Data visualisation
X<-ggline(results_Transfer_W000, x = "garment", y = "value",
       add = c("mean_se", "jitter"),
       order = c("G1", "G2", "G3"),
       ylab = "Number of transferred fibres", xlab = "Washing condition")
show(X)

X2<-ggline(results_Transfer_W001, x = "garment", y = "value",
          add = c("mean_se", "jitter"),
          order = c("G1", "G2", "G3"),
          ylab = "Number of transferred fibres", xlab = "Washing condition")
show(X2)

X3<-ggline(results_Transfer_W015, x = "garment", y = "value",
          add = c("mean_se", "jitter"),
          order = c("G1", "G2", "G3"),
          ylab = "Number of transferred fibres", xlab = "Washing condition")
show(X3)

# Test of normality - shapiro test
#H0: data are normally distributed
#H1: data are not normally distributed
shapiro.test(results_Transfer_W000$value) # p-value = 0.0003768 - not normally distributed
shapiro.test(results_Transfer_W001$value) # p-value = 0.6357 - Normally distributed
shapiro.test(results_Transfer_W015$value) # p-value = 3.149e-05 - not normally distributed

# Analysis for the non normally distributed wash
results_Transfer_W000$garment <- as.factor(results_Transfer_W000$garment)
kruskal.test(value ~ garment, data = results_Transfer_W000)#  p-value = 0.2365 - NS

results_Transfer_W015$garment <- as.factor(results_Transfer_W015$garment)
kruskal.test(value ~ garment, data = results_Transfer_W015)#  p-value = 0.0006181

results_Transfer_W015_Dunn <- dunnTest(value ~ garment, data=results_Transfer_W015, method="bonferroni");results_Transfer_W015_Dunn

# Analysis for the normally distributed wash
# sphericity test - Levene’s Test
#H0: All sample variances are equal
#H1: At least one group has a variance that is not equal to the rest.
leveneTest(value ~ garment, results_Transfer_W001) # p-value = 0.3509,  equal variances

# ANOVA
res_aovW001 <- aov(value ~ garment, results_Transfer_W001)
summary(res_aovW001) #  p-value = 0.0692- NS

# Export the data
write.table(results_Transfer_W015_Dunn$res, file = "Results/Statistics/Transfer_W015.csv", quote = F, sep = ",", row.names = F)
