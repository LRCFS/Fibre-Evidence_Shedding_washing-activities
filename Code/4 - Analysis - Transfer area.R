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
  compute_and_write_stats(dataframes[[i]], paste("Results/Statistics/Transfer Descriptive statistics - Series ", c(1, 2, 3, 4, 5, 6)[i], ".csv", sep = ""))
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
png("./Results/Figure 12 - Transfer 1st to 3rd series.png", width = 2400, height = 2100, res = 300)

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
forplotTot5_Vcotton$group <- c("5 v-cotton garment")

# data from the series involving Washing a single donor garment with detergent and softener
n=41
numS <- data.frame(setdiff(0:n, c(16,17,24,26,28,30,32,34,36,38,40)))
Transfer_12_Vcotton <- Transfer_12_Vcotton[!is.na(Transfer_12_Vcotton$value), ]
Transfer_12_Vcotton$value <- as.numeric(Transfer_12_Vcotton$value)
meanAtr12_Vcotton <- aggregate(value ~ Wash, Transfer_12_Vcotton, function(x) round(mean(x), digits = 3))
SDAtr12_Vcotton <- aggregate(value ~ Wash, Transfer_12_Vcotton, function(x) round(sd(x), digits = 3))
forplotTot12_Vcotton <- data.frame(cbind(numS, value =meanAtr12_Vcotton$value))
names(forplotTot12_Vcotton) <- c("Transfer", "value")
forplotTot12_Vcotton$group <- c("12 v-cotton garment")

# data from the series involving Washing a single donor garment with detergent
n=15
numS <- data.frame(setdiff(0:n, c(6,7,8,9,11,12,13,14)))
Transfer_Rcotton <- Transfer_Rcotton[!is.na(Transfer_Rcotton$value), ]
Transfer_Rcotton$value <- as.numeric(Transfer_Rcotton$value)
meanAtrRcotton <- aggregate(value ~ Wash, Transfer_Rcotton, function(x) round(mean(x), digits = 3))
SDAtrRcotton <- aggregate(value ~ Wash, Transfer_Rcotton, function(x) round(sd(x), digits = 3))
forplotTotRcotton <- data.frame(cbind(numS, value =meanAtrRcotton$value))
names(forplotTotRcotton) <- c("Transfer", "value")
forplotTotRcotton$group <- c("R-cotton textile")

# Combined all data
Toplot <- rbind(forplotTot5_Vcotton, forplotTot12_Vcotton, forplotTotRcotton)

# Convert specific columns to different format
Toplot$Transfer <- as.numeric(Toplot$Transfer)
Toplot$value <- as.numeric(Toplot$value)
Toplot$group <- as.factor(Toplot$group)

#### Final graph - Figure XXX ####
# Set up a PNG device
png("./Results/Figure 13 - Transfer 4th to 6th.png", width = 2400, height = 2100, res = 300)

# Adjust the margins (bottom, left, top, right)
par(mar = c(5, 4, 1, 2))  # Adjust the top margin to be smaller

# Plotting symbols and line types for each group
pch_values <- c(1, 2, 3)  # Different shapes: 1 = circle, 2 = triangle, 3 = plus
lty_values <- c(1, 2, 3)  # Different line types: 1 = solid, 2 = dashed, 3 = dotted

# Initial plot with first group only, to establish plot parameters
plot(Toplot$Transfer[Toplot$group == "R-cotton textile"], Toplot$value[Toplot$group == "R-cotton textile"],
     col="black", pch=pch_values[3],
     xlab='\nWash number',
     ylab='Average area of transferred fibres',
     xlim=range(Toplot$Transfer), 
     ylim=range(Toplot$value),
     xaxt='n',
     cex.axis = 1.2,
     cex.lab = 1.2,
     main="")

# Add other groups to the plot with points
points(Toplot$Transfer[Toplot$group == "5 v-cotton garment"], Toplot$value[Toplot$group == "5 v-cotton garment"], col="black", pch=pch_values[1])
points(Toplot$Transfer[Toplot$group == "12 v-cotton garment"], Toplot$value[Toplot$group == "12 v-cotton garment"], col="black", pch=pch_values[2])

# Define axis separately
axis(1, at=seq(0, max(Toplot$Transfer), by=1), las=1, cex.axis = 1.2)

# Add legend
legend("topright", legend=c("5 v-cotton garment", "12 v-cotton garment","R-cotton textile"),
       col="black", pch=pch_values, lty=lty_values, cex=1.2)

# Predict and plot smooth curves with different line types
spline5_Vcotton <- spline(Toplot$Transfer[Toplot$group == "R-cotton textile"], Toplot$value[Toplot$group == "R-cotton textile"], n = 200)
lines(spline5_Vcotton$x, spline5_Vcotton$y, col = 'black', lty=lty_values[3])

spline12_Vcotton <- spline(Toplot$Transfer[Toplot$group == "5 v-cotton garment"], Toplot$value[Toplot$group == "5 v-cotton garment"], n = 200)
lines(spline12_Vcotton$x, spline12_Vcotton$y, col = 'black', lty=lty_values[1])

splineRcotton <- spline(Toplot$Transfer[Toplot$group == "12 v-cotton garment"], Toplot$value[Toplot$group == "12 v-cotton garment"], n = 200)
lines(splineRcotton$x, splineRcotton$y, col = 'black', lty=lty_values[2])

# End the plot device
dev.off()


# ------------------------------------------------------------------------
# Section 2: Transfer - statistics
# ------------------------------------------------------------------------
# Load the data
first_series <- Transfer_Vcotton
second_series <- Transfer_VcottonD
third_series <- Transfer_VcottonDC

# Add a column to each dataset to indicate the series
first_series$Series <- 'First'
second_series$Series <- 'Second'
third_series$Series <- 'Third'

# Combine the datasets
combined_data <- bind_rows(first_series, second_series, third_series)

# Identify common wash cycles across all three series
common_washes <- intersect(intersect(first_series$Wash, second_series$Wash), third_series$Wash)

# Filter the data to include only the common wash cycles
filtered_data <- combined_data %>% filter(Wash %in% common_washes)

# Aggregate the data by taking the mean value for each wash cycle and series combination
aggregated_data <- filtered_data %>%
  group_by(Wash, Series) %>%
  summarise(value = mean(value)) %>%
  pivot_wider(names_from = Series, values_from = value)

# Test for normality for each series
shapiro_first <- shapiro.test(aggregated_data$First)
shapiro_second <- shapiro.test(aggregated_data$Second)
shapiro_third <- shapiro.test(aggregated_data$Third)

print(shapiro_first)
print(shapiro_second)
print(shapiro_third)

# Perform the Friedman test
friedman_test_result <- friedman.test(as.matrix(aggregated_data[, -1]))
print(friedman_test_result)

# Prepare data for the Nemenyi post hoc test in long format
nemenyi_data <- aggregated_data %>%
  pivot_longer(cols = c(First, Second, Third), names_to = "Series", values_to = "Value") %>%
  mutate(Wash = factor(Wash), Series = factor(Series))

# Perform the Nemenyi post hoc test
nemenyi_result <- frdAllPairsNemenyiTest(Value ~ Series | Wash, data = nemenyi_data)
print(summary(nemenyi_result))


# Load the data
fourth_series <- Transfer_5_Vcotton
fifth_series <- Transfer_12_Vcotton
sixth_series <- Transfer_Rcotton

# Add a column to each dataset to indicate the series
fourth_series$Series <- 'fourth'
fifth_series$Series <- 'fifth'
sixth_series$Series <- 'sixth'

# Combine the datasets
combined_data <- bind_rows(fourth_series, fifth_series, sixth_series)

# Identify common wash cycles across all three series
common_washes <- intersect(intersect(fourth_series$Wash, fifth_series$Wash), sixth_series$Wash)

# Filter the data to include only the common wash cycles
filtered_data <- combined_data %>% filter(Wash %in% common_washes)

# Aggregate the data by taking the mean value for each wash cycle and series combination
aggregated_data <- filtered_data %>%
  group_by(Wash, Series) %>%
  summarise(value = mean(value)) %>%
  pivot_wider(names_from = Series, values_from = value)

# Test for normality for each series
shapiro_fourth <- shapiro.test(aggregated_data$fourth)
shapiro_fifth <- shapiro.test(aggregated_data$fifth)
shapiro_sixth <- shapiro.test(aggregated_data$sixth)

print(shapiro_fourth)
print(shapiro_fifth)
print(shapiro_sixth)

# Perform the Friedman test
friedman_test_result <- friedman.test(as.matrix(aggregated_data[, -1]))
print(friedman_test_result)

# Save the aggregated data to verify the wash cycles
#write.csv(aggregated_data, "aggregated_data.csv", row.names = FALSE)

# Prepare data for the Nemenyi post hoc test in long format
nemenyi_data <- aggregated_data %>%
  pivot_longer(cols = c(fourth, fifth, sixth), names_to = "Series", values_to = "Value") %>%
  mutate(Wash = factor(Wash), Series = factor(Series))

# Perform the Nemenyi post hoc test
nemenyi_result <- frdAllPairsNemenyiTest(Value ~ Series | Wash, data = nemenyi_data)
print(summary(nemenyi_result))

### Difference between garments
#Levene’s Test
#H0: All sample variances are equal
#H1: At least one group has a variance that is not equal to the rest.
forstats_gentle_operators$Garment <- as.factor(forstats_gentle_operators$Garment)
leveneTest(Area.mm2 ~ Garment, forstats_gentle_operators) # p-value = 0.9778 equal variances

# Kruskal-Wallis test 
kruskal_test_result <- kruskal.test(Area.mm2 ~ Garment, data = forstats_gentle_operators)
print(kruskal_test_result) # p-value = 0.5951, no differences between groups

# firm pressure
# Shapiro test
forstats_firm_garment1 <- filter(forstats_firm_operators, Garment == "G1")
forstats_firm_garment2 <- filter(forstats_firm_operators, Garment == "G2")
forstats_firm_garment1 %>%
  shapiro_test(Area.mm2) # p-value = 0.000271,  non-parametric distribution
forstats_firm_garment2 %>%
  shapiro_test(Area.mm2) # p-value = 0.000221, non-parametric distribution

#Levene’s Test
#H0: All sample variances are equal
#H1: At least one group has a variance that is not equal to the rest.
forstats_firm_operators$Garment <- as.factor(forstats_firm_operators$Garment)
leveneTest(Area.mm2 ~ Garment, forstats_firm_operators) # p-value = 0.7416 equal variances

# Kruskal-Wallis test 
kruskal_test_result <- kruskal.test(Area.mm2 ~ Garment, data = forstats_firm_operators)
print(kruskal_test_result) # p-value = 0.2557, no differences between groups