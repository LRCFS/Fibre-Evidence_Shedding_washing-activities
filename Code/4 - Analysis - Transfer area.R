#############################################################
#####          Analysis - Transfer experiments          #####
#############################################################

# This R script is to generate the figures related to the Transfer experiments:

# ------------------------------------------------------------------------
# Section 1: Transfer - Washing series
# ------------------------------------------------------------------------
#### Data Cleaning and Processing ####
# Split the column based on "_"
Transfer_Vcotton <- separate(Vcotton_Dataset, Sample, into = c("date", "exp", "Wash","garment", "band", "sample"), sep = "_")
Transfer_VcottonD <- separate(VcottonD_Dataset, Sample, into = c("date", "exp", "Wash","garment", "band", "sample"), sep = "_")
Transfer_VcottonDC <- separate(VcottonDC_Dataset, Sample, into = c("date", "exp", "Wash","garment", "band", "sample"), sep = "_")
Transfer_5_Vcotton <- separate(Vcotton5_Dataset, Sample, into = c("Wash","garment", "band","orientation", "sample"), sep = "_")
Transfer_12_Vcotton <- separate(Vcotton12_Dataset, Sample, into = c("date", "exp", "Wash","garment", "band", "sample"), sep = "_")
Transfer_Rcotton <- separate(Rcotton_Dataset, Sample, into = c("Wash","garment", "band","orientation", "sample"), sep = "_")

# substract background
Transfer_Vcotton$value <-  Transfer_Vcotton$`After transfer area` -  Transfer_Vcotton$`Before transfer area`
Transfer_VcottonD$value <-  Transfer_VcottonD$`After transfer area` -  Transfer_VcottonD$`Before transfer area`
Transfer_VcottonDC$value <-  Transfer_VcottonDC$`After transfer area` -  Transfer_VcottonDC$`Before transfer area`
Transfer_5_Vcotton$value <-  Transfer_5_Vcotton$`After transfer area` -  Transfer_5_Vcotton$`Before transfer area`
Transfer_5_Vcotton$value <-  Transfer_5_Vcotton$`After transfer area` -  Transfer_5_Vcotton$`Before transfer area`
Transfer_12_Vcotton$value <-  Transfer_12_Vcotton$`After transfer area` -  Transfer_12_Vcotton$`Before transfer area`
Transfer_Rcotton$value <-  Transfer_Rcotton$`After transfer area` -  Transfer_Rcotton$`Before transfer area`

# Rename garment name in Transfer_Rcotton for more comprehension
Transfer_Rcotton$garment<- gsub("5_VcottonA","Vcotton",Transfer_Rcotton$garment)
Transfer_Rcotton$garment<- gsub("5_VcottonB","VcottonD",Transfer_Rcotton$garment)
Transfer_Rcotton$garment<- gsub("5_VcottonC","VcottonDC",Transfer_Rcotton$garment)

# to export 
Transfer_Vcotton <- Transfer_Vcotton %>% dplyr::select(Wash, garment,`Before transfer area`,`After transfer area`,value)
Transfer_VcottonD <- Transfer_VcottonD %>% dplyr::select(Wash, garment,`Before transfer area`,`After transfer area`,value)
Transfer_VcottonDC <- Transfer_VcottonDC %>% dplyr::select(Wash, garment,`Before transfer area`,`After transfer area`,value)
Transfer_5_Vcotton <- Transfer_5_Vcotton %>% dplyr::select(Wash, garment,`Before transfer area`,`After transfer area`,value)
Transfer_12_Vcotton <- Transfer_12_Vcotton %>% dplyr::select(Wash, garment,`Before transfer area`,`After transfer area`,value)
Transfer_Rcotton <- Transfer_Rcotton %>% dplyr::select(Wash, garment,`Before transfer area`,`After transfer area`,value)

# Select important column
Transfer_Vcotton <-  Transfer_Vcotton %>% dplyr::select(Wash, garment,value)
Transfer_VcottonD <-  Transfer_VcottonD %>% dplyr::select(Wash, garment,value)
Transfer_VcottonDC <-  Transfer_VcottonDC %>% dplyr::select(Wash, garment,value)
Transfer_5_Vcotton <-  Transfer_5_Vcotton %>% dplyr::select(Wash, garment,value)
Transfer_12_Vcotton <-  Transfer_12_Vcotton %>% dplyr::select(Wash, garment,value)
Transfer_Rcotton <-  Transfer_Rcotton %>% dplyr::select(Wash, garment,value)

# Create a list of dataframes
dataframes <- list(Transfer_Vcotton,Transfer_VcottonD,Transfer_VcottonDC, Transfer_5_Vcotton, Transfer_12_Vcotton, Transfer_Rcotton)

# Create a function to compute statistics and write to CSV
compute_and_write_stats <- function(df, filename) {
  mean_df <- aggregate(value ~ Wash, df, function(x) round(mean(x), digits = 3))
  sd_df <- aggregate(value ~ Wash, df, function(x) round(sd(x), digits = 2))
  median_df <- aggregate(value ~ Wash, df, function(x) round(median(x), digits = 2))
  
  datatable <- data.frame(
    "Wash number" = mean_df$Wash,
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

#### Final graph - Figure XXX #### 
# data from the series involving Washing a single donor garment
n=15
numS <- data.frame(setdiff(0:n, c(8,10,12,14)))
Transfer_Vcotton <- Transfer_Vcotton[!is.na(Transfer_Vcotton$value), ]
Transfer_Vcotton$value <- as.numeric(Transfer_Vcotton$value)
meanAtrVcotton <- aggregate(value ~ Wash, Transfer_Vcotton, function(x) round(mean(x), digits = 3))
SDAtrVcotton <- aggregate(value ~ Wash, Transfer_Vcotton, function(x) round(sd(x), digits = 3))
forplotTotVcotton <- data.frame(cbind(numS, value =meanAtrVcotton$value))
names(forplotTotVcotton) <- c("Transfer", "value")
forplotTotVcotton$group <- c("No detergent, no conditioner")

# data from the series involving Washing a single donor garment with detergent
n=15
numS <- data.frame(setdiff(0:n, c(11,12,14)))
Transfer_VcottonD <- Transfer_VcottonD[!is.na(Transfer_VcottonD$value), ]
Transfer_VcottonD$value <- as.numeric(Transfer_VcottonD$value)
meanAtrVcottonD <- aggregate(value ~ Wash, Transfer_VcottonD, function(x) round(mean(x), digits = 3))
SDAtrVcottonD <- aggregate(value ~ Wash, Transfer_VcottonD, function(x) round(sd(x), digits = 3))
forplotTotVcottonD <- data.frame(cbind(numS, value =meanAtrVcottonD$value))
names(forplotTotVcottonD) <- c("Transfer", "value")
forplotTotVcottonD$group <- c("Detergent, no conditioner")

# data from the series involving Washing a single donor garment with detergent and softener
n=15
numS <- data.frame(setdiff(0:n, c(11,12,14)))
Transfer_VcottonDC <- Transfer_VcottonDC[!is.na(Transfer_VcottonDC$value), ]
Transfer_VcottonDC$value <- as.numeric(Transfer_VcottonDC$value)
meanAtrVcottonDC <- aggregate(value ~ Wash, Transfer_VcottonDC, function(x) round(mean(x), digits = 3))
SDAtrVcottonDC <- aggregate(value ~ Wash, Transfer_VcottonDC, function(x) round(sd(x), digits = 3))
forplotTotVcottonDC <- data.frame(cbind(numS, value =meanAtrVcottonDC$value))
names(forplotTotVcottonDC) <- c("Transfer", "value")
forplotTotVcottonDC$group <- c("Detergent and conditioner")

# Add standard deviation columns to each data frame
forplotTotVcotton$sd <- SDAtrVcotton$value
forplotTotVcottonD$sd <- SDAtrVcottonD$value
forplotTotVcottonDC$sd <- SDAtrVcottonDC$value

# Combined all data
Toplot <- rbind(forplotTotVcotton,forplotTotVcottonD,forplotTotVcottonDC)

# # Convert specific columns to different format
Toplot$Transfer <- as.numeric(Toplot$Transfer)
Toplot$value <- as.numeric(Toplot$value)
Toplot$group <- as.factor(Toplot$group)

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
     ylab='Average area of transferred fibres',
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
splineVcotton <- spline(Toplot$Transfer[Toplot$group == "No detergent, no conditioner"], Toplot$value[Toplot$group == "No detergent, no conditioner"], n = 200)
lines(splineVcotton$x, splineVcotton$y, col = 'black', lty=lty_values[1])

splineVcottonD <- spline(Toplot$Transfer[Toplot$group == "Detergent, no conditioner"], Toplot$value[Toplot$group == "Detergent, no conditioner"], n = 200)
lines(splineVcottonD$x, splineVcottonD$y, col = 'black', lty=lty_values[2])

splineVcottonDC <- spline(Toplot$Transfer[Toplot$group == "Detergent and conditioner"], Toplot$value[Toplot$group == "Detergent and conditioner"], n = 200)
lines(splineVcottonDC$x, splineVcottonDC$y, col = 'black', lty=lty_values[3])

# # Error bars for "No detergent, no conditioner"
# with(subset(Toplot, group == "No detergent, no conditioner"), {
#   arrows(Transfer, value - sd, Transfer, value + sd, angle = 90, code = 3, length = 0.05, col = "black")
# })
# 
# # Error bars for "Detergent, no conditioner"
# with(subset(Toplot, group == "Detergent, no conditioner"), {
#   arrows(Transfer, value - sd, Transfer, value + sd, angle = 90, code = 3, length = 0.05, col = "black")
# })
# 
# # Error bars for "Detergent and conditioner"
# with(subset(Toplot, group == "Detergent and conditioner"), {
#   arrows(Transfer, value - sd, Transfer, value + sd, angle = 90, code = 3, length = 0.05, col = "black")
# })

# End the plot device
dev.off()

#### Final graph - Figure XXX #### 
# data from the series involving Washing a single r-cotton donor garment
n=51
numS <- data.frame(setdiff(0:n, c()))
Transfer_5_Vcotton <- Transfer_5_Vcotton[!is.na(Transfer_5_Vcotton$value), ]
Transfer_5_Vcotton$value <- as.numeric(Transfer_5_Vcotton$value)
meanAtr5_Vcotton <- aggregate(value ~ Wash, Transfer_5_Vcotton, function(x) round(mean(x), digits = 3))
SDAtr5_Vcotton <- aggregate(value ~ Wash, Transfer_5_Vcotton, function(x) round(sd(x), digits = 3))
forplotTot5_Vcotton <- data.frame(cbind(numS, value =meanAtr5_Vcotton$value))
names(forplotTot5_Vcotton) <- c("Transfer", "value")
forplotTot5_Vcotton$group <- c("R-cotton textile")

# data from the series involving Washing a single donor garment with detergent and softener
n=41
numS <- data.frame(setdiff(0:n, c(16,17,24,26,28,30,32,34,36,38,40)))
Transfer_12_Vcotton <- Transfer_12_Vcotton[!is.na(Transfer_12_Vcotton$value), ]
Transfer_12_Vcotton$value <- as.numeric(Transfer_12_Vcotton$value)
meanAtr12_Vcotton <- aggregate(value ~ Wash, Transfer_12_Vcotton, function(x) round(mean(x), digits = 3))
SDAtr12_Vcotton <- aggregate(value ~ Wash, Transfer_12_Vcotton, function(x) round(sd(x), digits = 3))
forplotTot12_Vcotton <- data.frame(cbind(numS, value =meanAtr12_Vcotton$value))
names(forplotTot12_Vcotton) <- c("Transfer", "value")
forplotTot12_Vcotton$group <- c("5 v-cotton garment")

# data from the series involving Washing a single donor garment with detergent
n=15
numS <- data.frame(setdiff(0:n, c(6,7,8,9,11,12,13,14)))
Transfer_Rcotton <- Transfer_Rcotton[!is.na(Transfer_Rcotton$value), ]
Transfer_Rcotton$value <- as.numeric(Transfer_Rcotton$value)
meanAtrRcotton <- aggregate(value ~ Wash, Transfer_Rcotton, function(x) round(mean(x), digits = 3))
SDAtrRcotton <- aggregate(value ~ Wash, Transfer_Rcotton, function(x) round(sd(x), digits = 3))
forplotTotRcotton <- data.frame(cbind(numS, value =meanAtrRcotton$value))
names(forplotTotRcotton) <- c("Transfer", "value")
forplotTotRcotton$group <- c("12 v-cotton garment")

# Combined all data
Toplot <- rbind(forplotTot5_Vcotton,forplotTot12_Vcotton,forplotTotRcotton)

# Add standard deviation columns to each data frame
forplotTotVcotton$sd <- SDAtrVcotton$value
forplotTotVcottonD$sd <- SDAtrVcottonD$value
forplotTotVcottonDC$sd <- SDAtrVcottonDC$value

# Convert specific columns to different format
Toplot$Transfer <- as.numeric(Toplot$Transfer)
Toplot$value <- as.numeric(Toplot$value)
Toplot$group <- as.factor(Toplot$group)

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
     ylab='Average area of transferred fibres',
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
spline5_Vcotton <- spline(Toplot$Transfer[Toplot$group == "R-cotton textile"], Toplot$value[Toplot$group == "R-cotton textile"], n = 200)
lines(spline5_Vcotton$x, spline5_Vcotton$y, col = 'black', lty=lty_values[1])

spline12_Vcotton <- spline(Toplot$Transfer[Toplot$group == "5 v-cotton garment"], Toplot$value[Toplot$group == "5 v-cotton garment"], n = 200)
lines(spline12_Vcotton$x, spline12_Vcotton$y, col = 'black', lty=lty_values[2])

splineRcotton <- spline(Toplot$Transfer[Toplot$group == "12 v-cotton garment"], Toplot$value[Toplot$group == "12 v-cotton garment"], n = 200)
lines(splineRcotton$x, splineRcotton$y, col = 'black', lty=lty_values[3])

# End the plot device
dev.off()

# # ------------------------------------------------------------------------
# # Section 2: Transfer - statistics
# # ------------------------------------------------------------------------
# ## Create different dataframes with each Wash
# Wash_codes <- c("W000", "W001", "W015")
# # Loop through each Wash code, filter the dataframe, and create a new variable in the global environment
# for (Wash_code in Wash_codes) {
#   filtered_df <- Transfer_Vcotton %>% filter(grepl(Wash_code, Wash))
#   
#   # Dynamically create variable names and assign dataframes to them
#   assign(paste("results_Transfer_Vcotton", Wash_code, sep = "_"), filtered_df)
# }
# 
# for (Wash_code in Wash_codes) {
#   filtered_df <- Transfer_VcottonD %>% filter(grepl(Wash_code, Wash))
#   
#   # Dynamically create variable names and assign dataframes to them
#   assign(paste("results_Transfer_VcottonD", Wash_code, sep = "_"), filtered_df)
# }
# 
# for (Wash_code in Wash_codes) {
#   filtered_df <- Transfer_VcottonDC %>% filter(grepl(Wash_code, Wash))
#   
#   # Dynamically create variable names and assign dataframes to them
#   assign(paste("results_Transfer_VcottonDC", Wash_code, sep = "_"), filtered_df)
# }
# 
# #combining results for statistic analysis
# results_Transfer_W000 <- rbind(results_Transfer_Vcotton_W000,results_Transfer_VcottonD_W000,results_Transfer_VcottonDC_W000)
# results_Transfer_W001 <- rbind(results_Transfer_Vcotton_W001,results_Transfer_VcottonD_W001,results_Transfer_VcottonDC_W001)
# results_Transfer_W015 <- rbind(results_Transfer_Vcotton_W015,results_Transfer_VcottonD_W015,results_Transfer_VcottonDC_W000)
# 
# # Data visualisation
# X<-ggline(results_Transfer_W000, x = "garment", y = "value",
#        add = c("mean_se", "jitter"),
#        order = c("Vcotton", "VcottonD", "VcottonDC"),
#        ylab = "Number of transferred fibres", xlab = "Washing condition")
# show(X)
# 
# X2<-ggline(results_Transfer_W001, x = "garment", y = "value",
#           add = c("mean_se", "jitter"),
#           order = c("Vcotton", "VcottonD", "VcottonDC"),
#           ylab = "Number of transferred fibres", xlab = "Washing condition")
# show(X2)
# 
# X3<-ggline(results_Transfer_W015, x = "garment", y = "value",
#           add = c("mean_se", "jitter"),
#           order = c("Vcotton", "VcottonD", "VcottonDC"),
#           ylab = "Number of transferred fibres", xlab = "Washing condition")
# show(X3)
# 
# # Test of normality - shapiro test
# #H0: data are normally distributed
# #H1: data are not normally distributed
# shapiro.test(results_Transfer_W000$value) # p-value = 0.09684 - Normally distributed
# shapiro.test(results_Transfer_W001$value) # p-value = 0.7346 - Normally distributed
# shapiro.test(results_Transfer_W015$value) # p-value = 7.067e-06 - not normally distributed
# 
# # Analysis for the non normally distributed Wash
# results_Transfer_W015$garment <- as.factor(results_Transfer_W015$garment)
# kruskal.test(value ~ garment, data = results_Transfer_W015)#  p-value = 0.0003758
# 
# results_Transfer_W015_Dunn <- dunnTest(value ~ garment, data=results_Transfer_W015, method="bonferroni");results_Transfer_W015_Dunn
# 
# # Analysis for the normally distributed Wash
# # sphericity test - Levene’s Test
# #H0: All sample variances are equal
# #H1: At least one group has a variance that is not equal to the rest.
# results_Transfer_W000$garment <- as.factor(results_Transfer_W000$garment)
# results_Transfer_W001$garment <- as.factor(results_Transfer_W001$garment)
# leveneTest(value ~ garment, results_Transfer_W000) # p-value = 0.5937,  equal variances
# leveneTest(value ~ garment, results_Transfer_W001) # p-value = 0.3155,  equal variances
# 
# # ANOVA
# res_aovW000 <- aov(value ~ garment, results_Transfer_W000)
# summary(res_aovW000) #  p-value = 0.174- NS
# 
# res_aovW001 <- aov(value ~ garment, results_Transfer_W001)
# summary(res_aovW001) #  p-value = 0.147- NS
# 
# # Export the data
# write.table(results_Transfer_W015_Dunn$res, file = "Results/Statistics/Transfer_W015.csv", quote = F, sep = ",", row.names = F)
