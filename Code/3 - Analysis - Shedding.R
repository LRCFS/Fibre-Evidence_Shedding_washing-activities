#########################################################
#####                    SHEDDING                   #####
#########################################################

# ------------------------------------------------------------------------
# Section 1: Garment 1
# ------------------------------------------------------------------------
#### Data Cleaning and Processing ####
# Convert Area from inch2 to mm2
# 1 pixel = 1 x10^-5 inch2, so (Area*1)/0.000011 to convert into px
Shedding_G1$Area.px <- (Shedding_G1$Area*1)/0.000011
# scale of the image: 1 mm = 110 pixels, 1mm2 = 12544 px
Shedding_G1$Area.mm2 <- Shedding_G1$Area.px/12100

# Create different dataframes for each wash
unique_washes <- unique(Shedding_G1$Wash)
shedding_by_wash <- lapply(unique_washes, function(wash) {
  Shedding_G1[Shedding_G1$Wash == wash, ]
})
names(shedding_by_wash) <- paste("Shedding_G1_", unique_washes, sep="")

## Create different dataframes for each weight
unique_weights <- unique(Shedding_G1$Weight)
shedding_by_wash_and_weight <- lapply(shedding_by_wash, function(df) {
  lapply(unique_weights, function(weight) {
    df[df$Weight == weight, ]
  })
})

# Naming the inner lists with weights
for (wash in names(shedding_by_wash_and_weight)) {
  names(shedding_by_wash_and_weight[[wash]]) <- paste(wash, unique_weights, sep="_")
}

# Adjusting the code to calculate mean and SD for "Area.mm2" column
results_shedding_G1 <- do.call(rbind, lapply(names(shedding_by_wash_and_weight), function(wash) {
  lapply(names(shedding_by_wash_and_weight[[wash]]), function(weight) {
    data_subset <- shedding_by_wash_and_weight[[wash]][[weight]]
    if (nrow(data_subset) > 0) {
      mean_area <- mean(data_subset$`Area.mm2`, na.rm = TRUE) # Calculate mean for "Area.mm2"
      sd_area <- sd(data_subset$`Area.mm2`, na.rm = TRUE) # Calculate SD for "Area.mm2"
      return(data.frame(WashWeight = paste(wash, weight, sep = "_"), Mean_Area = mean_area, SD_Area = sd_area))
    }
  })
})) 

# Remove NULL elements (if any) from the list before row-binding
results_shedding_G1 <- do.call(rbind, lapply(results_shedding_G1, function(x) x[!sapply(x, is.null)]))

# Combined data sets
results_shedding_G1_extended <- data.frame(str_split(results_shedding_G1$WashWeight, "_", simplify=TRUE))
results_shedding_G1_extended <- results_shedding_G1_extended %>%
  select("X2", "X3", "X7")
names(results_shedding_G1_extended) <- c("Garment","Wash","Weight")
results_shedding_G1 <- cbind(results_shedding_G1_extended,Mean_Area=results_shedding_G1$Mean_Area,SD_Area=results_shedding_G1$SD_Area)
rm(results_shedding_G1_extended)
results_shedding_G1$check <- (results_shedding_G1$SD_Area/results_shedding_G1$Mean_Area)

#### Intermediate Data Visualisation ####
pSH_G1 <- ggplot(results_shedding_G1, aes(x = factor(Weight, level = c('100g', '200g', '400g','800g','1000g','2000g')),
                                          y= Mean_Area, fill=Wash))+
  geom_bar(stat="identity", position=position_dodge(),colour="black")+
  labs(x="\nWeight", y="Total fibre area (mm\u00b2)\n") +
  theme_bw(base_family = "Arial", base_size = 12) +
  ylim(0,450)+
  scale_fill_manual(values = brewer.pal(9, "Paired")[1:9])+
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 0, vjust = 0.95, hjust=0.5))+
  geom_errorbar(aes(ymin=Mean_Area-SD_Area, ymax=Mean_Area+SD_Area),width=.2,position=position_dodge(.9))
ggsave("Shedding_G1.png", pSH_G1, width = 10, height = 9, units = "in", dpi=300, path = "Results")

#### STATS ####
## Create different dataframes with each wash
wash_codes <- c("W000", "W001", "W003", "W005", "W007", "W009", "W011", "W013", "W015")
# Loop through each wash code, filter the dataframe, and create a new variable in the global environment
for (wash_code in wash_codes) {
  filtered_df <- Shedding_G1 %>% filter(grepl(wash_code, Wash))
  
  # Dynamically create variable names and assign dataframes to them
  assign(paste("results_shedding_G1", wash_code, sep = "_"), filtered_df)
}

# Residuals VS Fitted and and Q-Q plots
# Combine dataframes into one, adding a 'Wash' column to identify the group
# Adjust the plotting area to accommodate all plots
OP <- par(mfrow=c(3,2))
# Loop through each wash code and plot the linear model
for (wash_code in wash_codes) {
  dataframe_name <- paste("results_shedding_G1", wash_code, sep = "_")
  # Dynamically retrieve the dataframe using get()
  current_df <- get(dataframe_name)
  # Plot the linear model
  plot(lm(Area.mm2 ~ Weight, data = current_df), 1:2, main = wash_code)
}
# Reset to original plotting parameters
par(OP)

# Test of normality - shapiro test
# List of dataframe names
dataframe_names <- c("results_shedding_G1_W000", "results_shedding_G1_W001", "results_shedding_G1_W003","results_shedding_G1_W005", "results_shedding_G1_W007", "results_shedding_G1_W009","results_shedding_G1_W011", "results_shedding_G1_W013", "results_shedding_G1_W015")
# Function to perform Shapiro-Wilk test and interpret results
perform_shapiro_test <- function(df_name) {
  dataframe <- get(df_name)
  test_result <- shapiro.test(dataframe$Area.mm2)
  interpretation <- ifelse(test_result$p.value < 0.05, "Not normally distributed", "Normally distributed")
  message(paste(df_name, "- p-value:", test_result$p.value, interpretation))
}
# Apply the function to each dataframe name
lapply(dataframe_names, perform_shapiro_test)

# Analysis for the normally distributed wash
### sphericity test
#Levene’s Test
#H0: All sample variances are equal
#H1: At least one group has a variance that is not equal to the rest.
results_shedding_G1_W000$Weight <- as.factor(results_shedding_G1_W000$Weight)
leveneTest(Area.mm2 ~ Weight, results_shedding_G1_W000) # p-value = 0.5255,  equal variances
results_shedding_G1_W001$Weight <- as.factor(results_shedding_G1_W001$Weight)
leveneTest(Area.mm2 ~ Weight, results_shedding_G1_W001) # p-value = 0.2146,  equal variances
results_shedding_G1_W009$Weight <- as.factor(results_shedding_G1_W009$Weight)
leveneTest(Area.mm2 ~ Weight, results_shedding_G1_W009) # p-value = 0.8318,  equal variances

# ANOVA
res_aovW000 <- aov(Area.mm2 ~ Weight, data = results_shedding_G1_W000)
summary(res_aovW000) #  p-value = 5.15e-07
res_aovW001 <- aov(Area.mm2 ~ Weight, data = results_shedding_G1_W001)
summary(res_aovW001) #  p-value = 2.33e-11
res_aovW009 <- aov(Area.mm2 ~ Weight, data = results_shedding_G1_W009)
summary(res_aovW009) #  p-value = 2.41e-05

# Tukey HSD test:
post_testW000 <- glht(res_aovW000, linfct = mcp(Weight = "Tukey"))
summary_post_testW000 <- summary(post_testW000);summary_post_testW000
post_testW001 <- glht(res_aovW001, linfct = mcp(Weight = "Tukey"))
summary_post_testW001 <- summary(post_testW001);summary_post_testW001
post_testW009 <- glht(res_aovW009, linfct = mcp(Weight = "Tukey"))
summary_post_testW009 <- summary(post_testW009);summary_post_testW009

# Analysis for the non normally distributed wash
results_shedding_G1_W003$Weight <- as.factor(results_shedding_G1_W003$Weight)
kruskal.test(Area.mm2 ~ Weight, data = results_shedding_G1_W003)#  p-value = 0.0001559

results_shedding_G1_W005$Weight <- as.factor(results_shedding_G1_W005$Weight)
kruskal.test(Area.mm2 ~ Weight, data = results_shedding_G1_W005)#  p-value = 0.00055

results_shedding_G1_W007$Weight <- as.factor(results_shedding_G1_W007$Weight)
kruskal.test(Area.mm2 ~ Weight, data = results_shedding_G1_W007)#  p-value = 8.87e-05

results_shedding_G1_W011$Weight <- as.factor(results_shedding_G1_W011$Weight)
kruskal.test(Area.mm2 ~ Weight, data = results_shedding_G1_W011)#  p-value = 8.728e-05

results_shedding_G1_W013$Weight <- as.factor(results_shedding_G1_W013$Weight)
kruskal.test(Area.mm2 ~ Weight, data = results_shedding_G1_W013)#  p-value = 5.016e-05

results_shedding_G1_W015$Weight <- as.factor(results_shedding_G1_W015$Weight)
kruskal.test(Area.mm2 ~ Weight, data = results_shedding_G1_W015)#  p-value = 0.002379

results_shedding_G1_W003_Dunn <- dunnTest(Area.mm2 ~ Weight, data=results_shedding_G1_W003, method="bonferroni");results_shedding_G1_W003_Dunn
results_shedding_G1_W005_Dunn <- dunnTest(Area.mm2 ~ Weight, data=results_shedding_G1_W005, method="bonferroni");results_shedding_G1_W005_Dunn
results_shedding_G1_W007_Dunn <- dunnTest(Area.mm2 ~ Weight, data=results_shedding_G1_W007, method="bonferroni");results_shedding_G1_W007_Dunn
results_shedding_G1_W011_Dunn <- dunnTest(Area.mm2 ~ Weight, data=results_shedding_G1_W011, method="bonferroni");results_shedding_G1_W011_Dunn
results_shedding_G1_W013_Dunn <- dunnTest(Area.mm2 ~ Weight, data=results_shedding_G1_W013, method="bonferroni");results_shedding_G1_W013_Dunn
results_shedding_G1_W015_Dunn <- dunnTest(Area.mm2 ~ Weight, data=results_shedding_G1_W015, method="bonferroni");results_shedding_G1_W015_Dunn


write.table(results_shedding_G1_W003_Dunn$res, file = "Results/Statistics/Shedding_G1_W003.csv", quote = F, sep = ",", row.names = F)
write.table(results_shedding_G1_W005_Dunn$res, file = "Results/Statistics/Shedding_G1_W005.csv", quote = F, sep = ",", row.names = F)
write.table(results_shedding_G1_W007_Dunn$res, file = "Results/Statistics/Shedding_G1_W007.csv", quote = F, sep = ",", row.names = F)
write.table(results_shedding_G1_W011_Dunn$res, file = "Results/Statistics/Shedding_G1_W011.csv", quote = F, sep = ",", row.names = F)
write.table(results_shedding_G1_W013_Dunn$res, file = "Results/Statistics/Shedding_G1_W013.csv", quote = F, sep = ",", row.names = F)
write.table(results_shedding_G1_W015_Dunn$res, file = "Results/Statistics/Shedding_G1_W015.csv", quote = F, sep = ",", row.names = F)

dataframes <- list(summary_post_testW000,summary_post_testW001,summary_post_testW009)

# Create a function to compute statistics and write to CSV
compute_and_write_stats <- function(df, filename) {
  coefficients <- df$test$coefficients
  std_errors <- df$test$sigma
  t_values <- df$test$tstat
  p_values <- df$test$pvalues
  
  # Convert p-values to non-scientific notation
  formatted_p_values <- formatC(p_values, format = "f", digits = 5)
  
  # Assign significance levels with stars
  significance_stars <- ifelse(p_values < 0.001, "***", ifelse(p_values < 0.01, "**", ifelse(p_values < 0.05, "*", "NS")))
  
  datatable <- data.frame(
    'Pairwise Comparison' = names(coefficients),
    Estimate = coefficients,
    'T-Value' = t_values,
    'p-Value' = formatted_p_values,
    'Significance' = significance_stars  # Add the new column for significance stars
  )
  
  # Ensure the Results directory exists
  if(!dir.exists(dirname(filename))) {
    dir.create(dirname(filename), recursive = TRUE)
  }
  
  write.table(datatable, file = filename, sep = ",", row.names = FALSE, col.names = TRUE, quote = FALSE, fileEncoding = "UTF-8")
}


# Assuming dataframes is a list of summary objects from glht tests
dataframes <- list(summary_post_testW000 = summary_post_testW000, summary_post_testW001 = summary_post_testW001,summary_post_testW009 = summary_post_testW009)

# Loop through the dataframes and compute/write statistics
for (i in seq_along(dataframes)) {
  compute_and_write_stats(dataframes[[i]], paste0("Results/Statistics/Shedding_G1_", i, ".csv"))
}

# ------------------------------------------------------------------------
# Section 2: Garment 2
# ------------------------------------------------------------------------
#### Data Cleaning and Processing ####
# Convert Area from inch2 to mm2
# 1 pixel = 1 x10^-5 inch2, so (Area*1)/0.000011 to convert into px
Shedding_G2$Area.px <- (Shedding_G2$Area*1)/0.000011
# scale of the image: 1 mm = 110 pixels, 1mm2 = 12544 px
Shedding_G2$Area.mm2 <- Shedding_G2$Area.px/12100

# Create different dataframes for each wash
unique_washes <- unique(Shedding_G2$Wash)
shedding_by_wash <- lapply(unique_washes, function(wash) {
  Shedding_G2[Shedding_G2$Wash == wash, ]
})
names(shedding_by_wash) <- paste("Shedding_G2_", unique_washes, sep="")

## Create different dataframes for each weight
unique_weights <- unique(Shedding_G2$Weight)
shedding_by_wash_and_weight <- lapply(shedding_by_wash, function(df) {
  lapply(unique_weights, function(weight) {
    df[df$Weight == weight, ]
  })
})

# Naming the inner lists with weights
for (wash in names(shedding_by_wash_and_weight)) {
  names(shedding_by_wash_and_weight[[wash]]) <- paste(wash, unique_weights, sep="_")
}

# Adjusting the code to calculate mean and SD for "Area.mm2" column
results_shedding_G2 <- do.call(rbind, lapply(names(shedding_by_wash_and_weight), function(wash) {
  lapply(names(shedding_by_wash_and_weight[[wash]]), function(weight) {
    data_subset <- shedding_by_wash_and_weight[[wash]][[weight]]
    if (nrow(data_subset) > 0) {
      mean_area <- mean(data_subset$`Area.mm2`, na.rm = TRUE) # Calculate mean for "Area.mm2"
      sd_area <- sd(data_subset$`Area.mm2`, na.rm = TRUE) # Calculate SD for "Area.mm2"
      return(data.frame(WashWeight = paste(wash, weight, sep = "_"), Mean_Area = mean_area, SD_Area = sd_area))
    }
  })
})) 

# Remove NULL elements (if any) from the list before row-binding
results_shedding_G2 <- do.call(rbind, lapply(results_shedding_G2, function(x) x[!sapply(x, is.null)]))

# Combined data sets
results_shedding_G2_extended <- data.frame(str_split(results_shedding_G2$WashWeight, "_", simplify=TRUE))
results_shedding_G2_extended <- results_shedding_G2_extended %>%
  select("X2", "X3", "X7")
names(results_shedding_G2_extended) <- c("Garment","Wash","Weight")
results_shedding_G2 <- cbind(results_shedding_G2_extended,Mean_Area=results_shedding_G2$Mean_Area,SD_Area=results_shedding_G2$SD_Area)
rm(results_shedding_G2_extended)
results_shedding_G2$check <- (results_shedding_G2$SD_Area/results_shedding_G2$Mean_Area)

#### Intermediate Data Visualisation ####
pSH_G2 <- ggplot(results_shedding_G2, aes(x = factor(Weight, level = c('100g', '200g', '400g','800g','1000g','2000g')),
                                 y= Mean_Area, fill=Wash))+
  geom_bar(stat="identity", position=position_dodge(),colour="black")+
  labs(x="\nWeight", y="Total fibre area (mm\u00b2)\n") +
  theme_bw(base_family = "Arial", base_size = 12) +
  ylim(0,450)+
  scale_fill_manual(values = brewer.pal(9, "Paired")[1:9])+
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 0, vjust = 0.95, hjust=0.5))+
  geom_errorbar(aes(ymin=Mean_Area-SD_Area, ymax=Mean_Area+SD_Area),width=.2,position=position_dodge(.9))
ggsave("Shedding_G2.png", pSH_G2, width = 10, height = 9, units = "in", dpi=300, path = "Results")

# ------------------------------------------------------------------------
# Section 3: Garment 3
# ------------------------------------------------------------------------
#### Data Cleaning and Processing ####
# Convert Area from inch2 to mm2
# 1 pixel = 1 x10^-5 inch2, so (Area*1)/0.000011 to convert into px
Shedding_G3$Area.px <- (Shedding_G3$Area*1)/0.000011
# scale of the image: 1 mm = 110 pixels, 1mm2 = 12544 px
Shedding_G3$Area.mm2 <- Shedding_G3$Area.px/12100

# Create different dataframes for each wash
unique_washes <- unique(Shedding_G3$Wash)
shedding_by_wash <- lapply(unique_washes, function(wash) {
  Shedding_G3[Shedding_G3$Wash == wash, ]
})
names(shedding_by_wash) <- paste("Shedding_G3_", unique_washes, sep="")

## Create different dataframes for each weight
unique_weights <- unique(Shedding_G3$Weight)
shedding_by_wash_and_weight <- lapply(shedding_by_wash, function(df) {
  lapply(unique_weights, function(weight) {
    df[df$Weight == weight, ]
  })
})

# Naming the inner lists with weights
for (wash in names(shedding_by_wash_and_weight)) {
  names(shedding_by_wash_and_weight[[wash]]) <- paste(wash, unique_weights, sep="_")
}

# Adjusting the code to calculate mean and SD for "Area.mm2" column
results_shedding_G3 <- do.call(rbind, lapply(names(shedding_by_wash_and_weight), function(wash) {
  lapply(names(shedding_by_wash_and_weight[[wash]]), function(weight) {
    data_subset <- shedding_by_wash_and_weight[[wash]][[weight]]
    if (nrow(data_subset) > 0) {
      mean_area <- mean(data_subset$`Area.mm2`, na.rm = TRUE) # Calculate mean for "Area.mm2"
      sd_area <- sd(data_subset$`Area.mm2`, na.rm = TRUE) # Calculate SD for "Area.mm2"
      return(data.frame(WashWeight = paste(wash, weight, sep = "_"), Mean_Area = mean_area, SD_Area = sd_area))
    }
  })
})) 

# Remove NULL elements (if any) from the list before row-binding
results_shedding_G3 <- do.call(rbind, lapply(results_shedding_G3, function(x) x[!sapply(x, is.null)]))

# Combined data sets
results_shedding_G3_extended <- data.frame(str_split(results_shedding_G3$WashWeight, "_", simplify=TRUE))
results_shedding_G3_extended <- results_shedding_G3_extended %>%
  select("X2", "X3", "X7")
names(results_shedding_G3_extended) <- c("Garment","Wash","Weight")
results_shedding_G3 <- cbind(results_shedding_G3_extended,Mean_Area=results_shedding_G3$Mean_Area,SD_Area=results_shedding_G3$SD_Area)
rm(results_shedding_G3_extended)
results_shedding_G3$check <- (results_shedding_G3$SD_Area/results_shedding_G3$Mean_Area)

#### Intermediate Data Visualisation ####
pSH_G3 <- ggplot(results_shedding_G3, aes(x = factor(Weight, level = c('100g', '200g', '400g','800g','1000g','2000g')),
                                 y= Mean_Area, fill=Wash))+
  geom_bar(stat="identity", position=position_dodge(),colour="black")+
  labs(x="\nWeight", y="Total fibre area (mm\u00b2)\n") +
  theme_bw(base_family = "Arial", base_size = 12) +
  ylim(0,450)+
  scale_fill_manual(values = brewer.pal(9, "Paired")[1:9])+
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 0, vjust = 0.95, hjust=0.5))+
  geom_errorbar(aes(ymin=Mean_Area-SD_Area, ymax=Mean_Area+SD_Area),width=.2,position=position_dodge(.9))
ggsave("Shedding_G3.png", pSH_G3, width = 10, height = 9, units = "in", dpi=300, path = "Results")

# ------------------------------------------------------------------------
# Section 4: 4th series 
# ------------------------------------------------------------------------
#### Data Cleaning and Processing ####
# Convert Area from inch2 to mm2
# 1 pixel = 1 x10^-5 inch2, so (Area*1)/0.000011 to convert into px
Shedding_G4$Area.px <- (Shedding_G4$Area*1)/0.000011
# 1 mm = 110 pixels, 1mm2 = 12100 px
Shedding_G4$Area.mm2 <- Shedding_G4$Area.px/12100

# Create different dataframes for each wash
unique_washes <- unique(Shedding_G4$Wash)
shedding_by_wash <- lapply(unique_washes, function(wash) {
  Shedding_G4[Shedding_G4$Wash == wash, ]
})
names(shedding_by_wash) <- paste("Shedding_G4_", unique_washes, sep="")

## Create different dataframes for each weight
unique_weights <- unique(Shedding_G4$Weight)
shedding_by_wash_and_weight <- lapply(shedding_by_wash, function(df) {
  lapply(unique_weights, function(weight) {
    df[df$Weight == weight, ]
  })
})

# Naming the inner lists with weights
for (wash in names(shedding_by_wash_and_weight)) {
  names(shedding_by_wash_and_weight[[wash]]) <- paste(wash, unique_weights, sep="_")
}

# Adjusting the code to calculate mean and SD for "Area.mm2" column
results_shedding_G4 <- do.call(rbind, lapply(names(shedding_by_wash_and_weight), function(wash) {
  lapply(names(shedding_by_wash_and_weight[[wash]]), function(weight) {
    data_subset <- shedding_by_wash_and_weight[[wash]][[weight]]
    if (nrow(data_subset) > 0) {
      mean_area <- mean(data_subset$`Area.mm2`, na.rm = TRUE) # Calculate mean for "Area.mm2"
      sd_area <- sd(data_subset$`Area.mm2`, na.rm = TRUE) # Calculate SD for "Area.mm2"
      return(data.frame(WashWeight = paste(wash, weight, sep = "_"), Mean_Area = mean_area, SD_Area = sd_area))
    }
  })
})) 

# Remove NULL elements (if any) from the list before row-binding
results_shedding_G4 <- do.call(rbind, lapply(results_shedding_G4, function(x) x[!sapply(x, is.null)]))

# Combined data sets
results_shedding_G4_extended <- data.frame(str_split(results_shedding_G4$WashWeight, "_", simplify=TRUE))
results_shedding_G4_extended <- results_shedding_G4_extended %>%
  select("X2", "X3", "X7")
names(results_shedding_G4_extended) <- c("Garment","Wash","Weight")
results_shedding_G4 <- cbind(results_shedding_G4_extended,Mean_Area=results_shedding_G4$Mean_Area,SD_Area=results_shedding_G4$SD_Area)
rm(results_shedding_G4_extended)
results_shedding_G4$check <- (results_shedding_G4$SD_Area/results_shedding_G4$Mean_Area)

# calculation of the percentage difference between washed and unwashed
results_shedding_G4_W000 <- results_shedding_G4 %>% filter(Wash == "W000")
results_shedding_G4_W051 <- results_shedding_G4 %>% filter(Wash == "W051")

# Remove the 'g' from the Weight column and convert it to numeric
results_shedding_G4_W000$Weight <- as.numeric(sub("g", "", results_shedding_G4_W000$Weight))
results_shedding_G4_W051$Weight <- as.numeric(sub("g", "", results_shedding_G4_W051$Weight))
# Order the dataframe by the numeric Weight column
results_shedding_G4_W000 <- results_shedding_G4_W000[order(results_shedding_G4_W000$Weight), ]
results_shedding_G4_W051 <- results_shedding_G4_W051[order(results_shedding_G4_W051$Weight), ]
# convert it back after ordering
results_shedding_G4_W000$Weight <- paste0(results_shedding_G4_W000$Weight, "g")
results_shedding_G4_W051$Weight <- paste0(results_shedding_G4_W051$Weight, "g")

results_shedding_G4_ratio <- data.frame(cbind(results_shedding_G4_W000$Mean_Area,results_shedding_G4_W051$Mean_Area))
results_shedding_G4_ratio$ratio <- round(abs(results_shedding_G4_ratio$X1 - results_shedding_G4_ratio$X2) / 
  ((results_shedding_G4_ratio$X1 + results_shedding_G4_ratio$X2)/2)*100, digits=2)
label = rep(results_shedding_G4_ratio$ratio,2)

#### Intermediate Data Visualisation ####
a = rep(c(100, 125, 150, 175, 200,225),2)
b = rep(c(1:6), 2)
pSH_G4 <- ggplot(results_shedding_G4, aes(x = factor(Weight, level = c('100g', '200g', '400g','800g','1000g','2000g')),
                                 y= Mean_Area, fill=Wash))+
  geom_bar(stat="identity", position=position_dodge(),colour="black")+
  labs(x="\nWeight", y="Total fibre area (mm\u00b2)\n") +
  theme_bw(base_family = "Arial", base_size = 12) +
  ylim(0,375)+
  scale_fill_manual(values = c(brewer.pal(9, "Greys")[c(1, 9)])) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 0, vjust = 0.95, hjust=0.5))+
  geom_errorbar(aes(ymin=Mean_Area-SD_Area, ymax=Mean_Area+SD_Area),width=.2,position=position_dodge(.9))+
  geom_text(x=b, y=a+2, label=label,colour="#c9101a")+
  geom_errorbarh(aes(xmax = (b + 0.3), xmin = (b - 0.3), y = a-20), height = 15,colour="#c9101a") 
ggsave("Shedding_G4.png", pSH_G4, width = 10, height = 9, units = "in", dpi=300, path = "Results")
pSH_G4

# ------------------------------------------------------------------------
# Section 5: Garment 5
# ------------------------------------------------------------------------
#### Data Cleaning and Processing ####
# Convert Area from inch2 to mm2
# 1 pixel = 1 x10^-5 inch2, so (Area*1)/0.000011 to convert into px
Shedding_G5$Area.px <- (Shedding_G5$Area*1)/0.000011
# scale of the image: 1 mm = 110 pixels, 1mm2 = 12544 px
Shedding_G5$Area.mm2 <- Shedding_G5$Area.px/12100

# Create different dataframes for each wash
unique_washes <- unique(Shedding_G5$Wash)
shedding_by_wash <- lapply(unique_washes, function(wash) {
  Shedding_G5[Shedding_G5$Wash == wash, ]
})
names(shedding_by_wash) <- paste("Shedding_G5_", unique_washes, sep="")

## Create different dataframes for each weight
unique_weights <- unique(Shedding_G5$Weight)
shedding_by_wash_and_weight <- lapply(shedding_by_wash, function(df) {
  lapply(unique_weights, function(weight) {
    df[df$Weight == weight, ]
  })
})

# Naming the inner lists with weights
for (wash in names(shedding_by_wash_and_weight)) {
  names(shedding_by_wash_and_weight[[wash]]) <- paste(wash, unique_weights, sep="_")
}

# Adjusting the code to calculate mean and SD for "Area.mm2" column
results_shedding_G5 <- do.call(rbind, lapply(names(shedding_by_wash_and_weight), function(wash) {
  lapply(names(shedding_by_wash_and_weight[[wash]]), function(weight) {
    data_subset <- shedding_by_wash_and_weight[[wash]][[weight]]
    if (nrow(data_subset) > 0) {
      mean_area <- mean(data_subset$`Area.mm2`, na.rm = TRUE) # Calculate mean for "Area.mm2"
      sd_area <- sd(data_subset$`Area.mm2`, na.rm = TRUE) # Calculate SD for "Area.mm2"
      return(data.frame(WashWeight = paste(wash, weight, sep = "_"), Mean_Area = mean_area, SD_Area = sd_area))
    }
  })
})) 

# Remove NULL elements (if any) from the list before row-binding
results_shedding_G5 <- do.call(rbind, lapply(results_shedding_G5, function(x) x[!sapply(x, is.null)]))

# Combined data sets
results_shedding_G5_extended <- data.frame(str_split(results_shedding_G5$WashWeight, "_", simplify=TRUE))
results_shedding_G5_extended <- results_shedding_G5_extended %>%
  select("X2", "X3", "X7")
names(results_shedding_G5_extended) <- c("Garment","Wash","Weight")
results_shedding_G5 <- cbind(results_shedding_G5_extended,Mean_Area=results_shedding_G5$Mean_Area,SD_Area=results_shedding_G5$SD_Area)
rm(results_shedding_G5_extended)
results_shedding_G5$check <- (results_shedding_G5$SD_Area/results_shedding_G5$Mean_Area)

#### Intermediate Data Visualisation ####
results_shedding_G5_bis <- results_shedding_G5 %>% filter(Wash == "W000" | Wash == "W015")

# calculation of the percentage difference between washed and unwashed
results_shedding_G5_bis_W000 <- results_shedding_G5 %>% filter(Wash == "W000")
results_shedding_G5_bis_W015 <- results_shedding_G5 %>% filter(Wash == "W015")

# Remove the 'g' from the Weight column and convert it to numeric
results_shedding_G5_bis_W000$Weight <- as.numeric(sub("g", "", results_shedding_G5_bis_W000$Weight))
results_shedding_G5_bis_W015$Weight <- as.numeric(sub("g", "", results_shedding_G5_bis_W015$Weight))
# Order the dataframe by the numeric Weight column
results_shedding_G5_bis_W000 <- results_shedding_G5_bis_W000[order(results_shedding_G5_bis_W000$Weight), ]
results_shedding_G5_bis_W015 <- results_shedding_G5_bis_W015[order(results_shedding_G5_bis_W015$Weight), ]
# convert it back after ordering
results_shedding_G5_bis_W000$Weight <- paste0(results_shedding_G5_bis_W000$Weight, "g")
results_shedding_G5_bis_W015$Weight <- paste0(results_shedding_G5_bis_W015$Weight, "g")

results_shedding_G5_bis_ratio <- data.frame(cbind(results_shedding_G5_bis_W000$Mean_Area,results_shedding_G5_bis_W015$Mean_Area))
results_shedding_G5_bis_ratio$ratio <- round(abs(results_shedding_G5_bis_ratio$X1 - results_shedding_G5_bis_ratio$X2) / 
                                                 ((results_shedding_G5_bis_ratio$X1 + results_shedding_G5_bis_ratio$X2)/2)*100, digits=2)

label = rep(results_shedding_G5_bis_ratio$ratio,2)

c = rep(c(175, 200, 225, 250, 275, 300),2)
d = rep(c(1:6), 2)
pSH_G5 <- ggplot(results_shedding_G5_bis, aes(x = factor(Weight, level = c('100g', '200g', '400g','800g','1000g','2000g')),
                                              y= Mean_Area, fill=Wash))+
  geom_bar(stat="identity", position=position_dodge(),colour="black")+
  labs(x="\nWeight", y="Total fibre area (mm\u00b2)\n") +
  theme_bw(base_family = "Arial", base_size = 12) +
  ylim(0,375)+
  scale_fill_manual(values = c(brewer.pal(9, "Greys")[c(1, 9)])) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 0, vjust = 0.95, hjust=0.5))+
  geom_errorbar(aes(ymin=Mean_Area-SD_Area, ymax=Mean_Area+SD_Area),width=.2,position=position_dodge(.9))+
  geom_text(x=d, y=c+2, label=label,colour="#c9101a")+
  geom_errorbarh(aes(xmax = (d + 0.3), xmin = (d - 0.3), y = c-20), height = 15,colour="#c9101a") 
ggsave("Shedding_G5.png", pSH_G5, width = 10, height = 9, units = "in", dpi=300, path = "Results")
pSH_G5

# ------------------------------------------------------------------------
# Section 6: 6th series 
# ------------------------------------------------------------------------
#### Data Cleaning and Processing ####
# Convert Area from inch2 to mm2
# 1 pixel = 1 x10^-5 inch2, so (Area*1)/0.000011 to convert into px
Shedding_G6$Area.px <- (Shedding_G6$Area*1)/0.000011
# 1 mm = 110 pixels, 1mm2 = 12100 px
Shedding_G6$Area.mm2 <- Shedding_G6$Area.px/12100

# Create different dataframes for each wash
unique_washes <- unique(Shedding_G6$Wash)
shedding_by_wash <- lapply(unique_washes, function(wash) {
  Shedding_G6[Shedding_G6$Wash == wash, ]
})
names(shedding_by_wash) <- paste("Shedding_G6_", unique_washes, sep="")

## Create different dataframes for each weight
unique_weights <- unique(Shedding_G6$Weight)
shedding_by_wash_and_weight <- lapply(shedding_by_wash, function(df) {
  lapply(unique_weights, function(weight) {
    df[df$Weight == weight, ]
  })
})

# Naming the inner lists with weights
for (wash in names(shedding_by_wash_and_weight)) {
  names(shedding_by_wash_and_weight[[wash]]) <- paste(wash, unique_weights, sep="_")
}

# Adjusting the code to calculate mean and SD for "Area.mm2" column
results_shedding_G6 <- do.call(rbind, lapply(names(shedding_by_wash_and_weight), function(wash) {
  lapply(names(shedding_by_wash_and_weight[[wash]]), function(weight) {
    data_subset <- shedding_by_wash_and_weight[[wash]][[weight]]
    if (nrow(data_subset) > 0) {
      mean_area <- mean(data_subset$`Area.mm2`, na.rm = TRUE) # Calculate mean for "Area.mm2"
      sd_area <- sd(data_subset$`Area.mm2`, na.rm = TRUE) # Calculate SD for "Area.mm2"
      return(data.frame(WashWeight = paste(wash, weight, sep = "_"), Mean_Area = mean_area, SD_Area = sd_area))
    }
  })
})) 

# Remove NULL elements (if any) from the list before row-binding
results_shedding_G6 <- do.call(rbind, lapply(results_shedding_G6, function(x) x[!sapply(x, is.null)]))

# Combined data sets
results_shedding_G6_extended <- data.frame(str_split(results_shedding_G6$WashWeight, "_", simplify=TRUE))
results_shedding_G6_extended <- results_shedding_G6_extended %>%
  select("X2", "X3", "X7")
names(results_shedding_G6_extended) <- c("Garment","Wash","Weight")
results_shedding_G6 <- cbind(results_shedding_G6_extended,Mean_Area=results_shedding_G6$Mean_Area,SD_Area=results_shedding_G6$SD_Area)
rm(results_shedding_G6_extended)
results_shedding_G6$check <- (results_shedding_G6$SD_Area/results_shedding_G6$Mean_Area)

# calculation of the percentage difference between washed and unwashed
results_shedding_G6_W000 <- results_shedding_G6 %>% filter(Wash == "W000")
results_shedding_G6_W025 <- results_shedding_G6 %>% filter(Wash == "W025")

# Remove the 'g' from the Weight column and convert it to numeric
results_shedding_G6_W000$Weight <- as.numeric(sub("g", "", results_shedding_G6_W000$Weight))
results_shedding_G6_W025$Weight <- as.numeric(sub("g", "", results_shedding_G6_W025$Weight))
# Order the dataframe by the numeric Weight column
results_shedding_G6_W000 <- results_shedding_G6_W000[order(results_shedding_G6_W000$Weight), ]
results_shedding_G6_W025 <- results_shedding_G6_W025[order(results_shedding_G6_W025$Weight), ]
# convert it back after ordering
results_shedding_G6_W000$Weight <- paste0(results_shedding_G6_W000$Weight, "g")
results_shedding_G6_W025$Weight <- paste0(results_shedding_G6_W025$Weight, "g")

results_shedding_G6_ratio <- data.frame(cbind(results_shedding_G6_W000$Mean_Area,results_shedding_G6_W025$Mean_Area))
results_shedding_G6_ratio$ratio <- round(abs(results_shedding_G6_ratio$X1 - results_shedding_G6_ratio$X2) / 
  ((results_shedding_G6_ratio$X1 + results_shedding_G6_ratio$X2)/2)*100, digits=2)
label = rep(results_shedding_G6_ratio$ratio,2)

#### Intermediate Data Visualisation ####
e = rep(c(175, 215, 255, 295, 335, 375),2)
f = rep(c(1:6), 2)
pSH_G6 <- ggplot(results_shedding_G6, aes(x = factor(Weight, level = c('100g', '200g', '400g','800g','1000g','2000g')),
                                 y= Mean_Area, fill=Wash))+
  geom_bar(stat="identity", position=position_dodge(),colour="black")+
  labs(x="\nWeight", y="Total fibre area (mm\u00b2)\n") +
  theme_bw(base_family = "Arial", base_size = 12) +
  ylim(0,375)+
  scale_fill_manual(values = c(brewer.pal(9, "Greys")[c(1, 9)])) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 0, vjust = 0.95, hjust=0.5))+
  geom_errorbar(aes(ymin=Mean_Area-SD_Area, ymax=Mean_Area+SD_Area),width=.2,position=position_dodge(.9))+
  geom_text(x=f, y=e+2, label=label,colour="#c9101a")+
  geom_errorbarh(aes(xmax = (f + 0.3), xmin = (f - 0.3), y = e-20), height = 15,colour="#c9101a") 
ggsave("Shedding_G6.png", pSH_G6, width = 10, height = 9, units = "in", dpi=300, path = "Results")
pSH_G6

# ------------------------------------------------------------------------
# Section 5: Comparisons between washing conditions
# ------------------------------------------------------------------------
# Modify each pSH_GX plot to adjust legend key size and labels
pSH_G1 <- pSH_G1 + theme(legend.text = element_text(size = 12)  # Adjust the horizontal space between legend keys
)
pSH_G2 <- pSH_G2 + theme(legend.text = element_text(size = 12) # Adjust the horizontal space between legend keys
)
pSH_G3 <- pSH_G3 + theme(legend.text = element_text(size = 12) # Adjust the horizontal space between legend keys
)
# pSH_G4 <- pSH_G4 + theme(legend.text = element_text(size = 12)  # Adjust the horizontal space between legend keys
# )
#### Final graph - Figure XXX #### 
pCombinedSH_pending <- ggarrange(pSH_G1+ rremove("ylab") + rremove("xlab"),
                                 pSH_G2+ rremove("ylab") + rremove("xlab"),
                                 pSH_G3+ rremove("ylab") + rremove("xlab"),
                                 # pSH_G4+ rremove("ylab") + rremove("xlab"),
                                 labels = c("A", "B", "C", "D"),
                                 common.legend = TRUE, legend = "right",
                                 align = "hv",
                                 ncol = 1, nrow = 3,
                                 font.label = list(size = 12, color = "black", family = NULL, position = "top"))+
  theme(plot.margin = margin(0,1,0,0, "cm")) # in order (Top,right,bottom,left)

pCombinedSH <- annotate_figure(pCombinedSH_pending, left = textGrob("Shed fibre area (mm\u00b2)\n", rot = 90, vjust = 0.5, hjust = 0.5, gp = gpar(cex =1)),
                               bottom = textGrob("weight", vjust = 0.5, hjust = 0.5,gp = gpar(cex = 1)))
pCombinedSH

# to save the graph
ggsave("Figure XXX - Shedding all garments.png", pCombinedSH, width =10, height = 13, units = "in", dpi=600,path = "Results")


#### Final graph - Figure XXX #### 
pCombinedSH_pending2 <- ggarrange(pSH_G4+ rremove("ylab") + rremove("xlab"),
                                  pSH_G5+ rremove("ylab") + rremove("xlab"),
                                 pSH_G6+ rremove("ylab") + rremove("xlab"),
                                 labels = c("A", "B", "C"),
                                 common.legend = F, legend = "right",
                                 align = "hv",
                                 ncol = 1, nrow = 3,
                                 font.label = list(size = 12, color = "black", family = NULL, position = "top"))+
  theme(plot.margin = margin(0,1,0,0, "cm")) # in order (Top,right,bottom,left)

pCombinedSH2 <- annotate_figure(pCombinedSH_pending2, left = textGrob("Shed fibre area (mm\u00b2)\n", rot = 90, vjust = 0.5, hjust = 0.5, gp = gpar(cex =1)),
                               bottom = textGrob("weight", vjust = 0.5, hjust = 0.5,gp = gpar(cex = 1)))
pCombinedSH2

# to save the graph
ggsave("Figure XXX - Shedding 4-6 garments.png", pCombinedSH2, width =7, height = 10, units = "in", dpi=600,path = "Results")

