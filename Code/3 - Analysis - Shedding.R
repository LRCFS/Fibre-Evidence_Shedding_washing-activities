#########################################################
#####                    SHEDDING                   #####
#########################################################

# ------------------------------------------------------------------------
# Section 1: Garment 1
# ------------------------------------------------------------------------
#### Data Cleaning and Processing ####
# Convert Area from inch2 to mm2
# 1 pixel = 1 x10^-5 inch2, so (Area*1)/0.000011 to convert into px
Shedding_Vcotton$Area.px <- (Shedding_Vcotton$Area*1)/0.000011
# scale of the image: 1 mm = 110 pixels, 1mm2 = 12544 px
Shedding_Vcotton$Area.mm2 <- Shedding_Vcotton$Area.px/12100

# Create different dataframes for each wash
unique_washes <- unique(Shedding_Vcotton$Wash)
shedding_by_wash <- lapply(unique_washes, function(wash) {
  Shedding_Vcotton[Shedding_Vcotton$Wash == wash, ]
})
names(shedding_by_wash) <- paste("Shedding_1Vcotton_", unique_washes, sep="")

## Create different dataframes for each weight
unique_weights <- unique(Shedding_Vcotton$Weight)
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
results_shedding_Vcotton <- do.call(rbind, lapply(names(shedding_by_wash_and_weight), function(wash) {
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
results_shedding_Vcotton <- do.call(rbind, lapply(results_shedding_Vcotton, function(x) x[!sapply(x, is.null)]))

# Combined data sets
results_shedding_Vcotton_extended <- data.frame(str_split(results_shedding_Vcotton$WashWeight, "_", simplify=TRUE))
results_shedding_Vcotton_extended <- results_shedding_Vcotton_extended %>%
  select("X2", "X3", "X7")
names(results_shedding_Vcotton_extended) <- c("Garment","Wash","Weight")
results_shedding_Vcotton <- cbind(results_shedding_Vcotton_extended,Mean_Area=results_shedding_Vcotton$Mean_Area,SD_Area=results_shedding_Vcotton$SD_Area)
rm(results_shedding_Vcotton_extended)
results_shedding_Vcotton$check <- (results_shedding_Vcotton$SD_Area/results_shedding_Vcotton$Mean_Area)

#### Intermediate Data Visualisation ####
pSH_Vcotton <- ggplot(results_shedding_Vcotton, aes(x = factor(Weight, level = c('100g', '200g', '400g','800g','1000g','2000g')),
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
ggsave("Shedding_Vcotton.png", pSH_Vcotton, width = 10, height = 9, units = "in", dpi=300, path = "Results")

#### STATS ####
## Create different dataframes with each wash
wash_codes <- c("W000", "W001", "W003", "W005", "W007", "W009", "W011", "W013", "W015")
# Loop through each wash code, filter the dataframe, and create a new variable in the global environment
for (wash_code in wash_codes) {
  filtered_df <- Shedding_Vcotton %>% filter(grepl(wash_code, Wash))
  
  # Dynamically create variable names and assign dataframes to them
  assign(paste("results_shedding_Vcotton", wash_code, sep = "_"), filtered_df)
}

# Residuals VS Fitted and and Q-Q plots
# Combine dataframes into one, adding a 'Wash' column to identify the group
# Adjust the plotting area to accommodate all plots
OP <- par(mfrow=c(3,2))
# Loop through each wash code and plot the linear model
for (wash_code in wash_codes) {
  dataframe_name <- paste("results_shedding_Vcotton", wash_code, sep = "_")
  # Dynamically retrieve the dataframe using get()
  current_df <- get(dataframe_name)
  # Plot the linear model
  plot(lm(Area.mm2 ~ Weight, data = current_df), 1:2, main = wash_code)
}
# Reset to original plotting parameters
par(OP)

# Test of normality - shapiro test
# List of dataframe names
dataframe_names <- c("results_shedding_Vcotton_W000", "results_shedding_Vcotton_W001", "results_shedding_Vcotton_W003","results_shedding_Vcotton_W005", "results_shedding_Vcotton_W007", "results_shedding_Vcotton_W009","results_shedding_Vcotton_W011", "results_shedding_Vcotton_W013", "results_shedding_Vcotton_W015")
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
results_shedding_Vcotton_W000$Weight <- as.factor(results_shedding_Vcotton_W000$Weight)
leveneTest(Area.mm2 ~ Weight, results_shedding_Vcotton_W000) # p-value = 0.5255,  equal variances
results_shedding_Vcotton_W001$Weight <- as.factor(results_shedding_Vcotton_W001$Weight)
leveneTest(Area.mm2 ~ Weight, results_shedding_Vcotton_W001) # p-value = 0.2146,  equal variances
results_shedding_Vcotton_W009$Weight <- as.factor(results_shedding_Vcotton_W009$Weight)
leveneTest(Area.mm2 ~ Weight, results_shedding_Vcotton_W009) # p-value = 0.8318,  equal variances

# ANOVA
res_aovW000 <- aov(Area.mm2 ~ Weight, data = results_shedding_Vcotton_W000)
summary(res_aovW000) #  p-value = 5.15e-07
res_aovW001 <- aov(Area.mm2 ~ Weight, data = results_shedding_Vcotton_W001)
summary(res_aovW001) #  p-value = 2.33e-11
res_aovW009 <- aov(Area.mm2 ~ Weight, data = results_shedding_Vcotton_W009)
summary(res_aovW009) #  p-value = 2.41e-05

# Tukey HSD test:
post_testW000 <- glht(res_aovW000, linfct = mcp(Weight = "Tukey"))
summary_post_testW000 <- summary(post_testW000);summary_post_testW000
post_testW001 <- glht(res_aovW001, linfct = mcp(Weight = "Tukey"))
summary_post_testW001 <- summary(post_testW001);summary_post_testW001
post_testW009 <- glht(res_aovW009, linfct = mcp(Weight = "Tukey"))
summary_post_testW009 <- summary(post_testW009);summary_post_testW009

# Analysis for the non normally distributed wash
results_shedding_Vcotton_W003$Weight <- as.factor(results_shedding_Vcotton_W003$Weight)
kruskal.test(Area.mm2 ~ Weight, data = results_shedding_Vcotton_W003)#  p-value = 0.0001559

results_shedding_Vcotton_W005$Weight <- as.factor(results_shedding_Vcotton_W005$Weight)
kruskal.test(Area.mm2 ~ Weight, data = results_shedding_Vcotton_W005)#  p-value = 0.00055

results_shedding_Vcotton_W007$Weight <- as.factor(results_shedding_Vcotton_W007$Weight)
kruskal.test(Area.mm2 ~ Weight, data = results_shedding_Vcotton_W007)#  p-value = 8.87e-05

results_shedding_Vcotton_W011$Weight <- as.factor(results_shedding_Vcotton_W011$Weight)
kruskal.test(Area.mm2 ~ Weight, data = results_shedding_Vcotton_W011)#  p-value = 8.728e-05

results_shedding_Vcotton_W013$Weight <- as.factor(results_shedding_Vcotton_W013$Weight)
kruskal.test(Area.mm2 ~ Weight, data = results_shedding_Vcotton_W013)#  p-value = 5.016e-05

results_shedding_Vcotton_W015$Weight <- as.factor(results_shedding_Vcotton_W015$Weight)
kruskal.test(Area.mm2 ~ Weight, data = results_shedding_Vcotton_W015)#  p-value = 0.002379

results_shedding_Vcotton_W003_Dunn <- dunnTest(Area.mm2 ~ Weight, data=results_shedding_Vcotton_W003, method="bonferroni");results_shedding_Vcotton_W003_Dunn
results_shedding_Vcotton_W005_Dunn <- dunnTest(Area.mm2 ~ Weight, data=results_shedding_Vcotton_W005, method="bonferroni");results_shedding_Vcotton_W005_Dunn
results_shedding_Vcotton_W007_Dunn <- dunnTest(Area.mm2 ~ Weight, data=results_shedding_Vcotton_W007, method="bonferroni");results_shedding_Vcotton_W007_Dunn
results_shedding_Vcotton_W011_Dunn <- dunnTest(Area.mm2 ~ Weight, data=results_shedding_Vcotton_W011, method="bonferroni");results_shedding_Vcotton_W011_Dunn
results_shedding_Vcotton_W013_Dunn <- dunnTest(Area.mm2 ~ Weight, data=results_shedding_Vcotton_W013, method="bonferroni");results_shedding_Vcotton_W013_Dunn
results_shedding_Vcotton_W015_Dunn <- dunnTest(Area.mm2 ~ Weight, data=results_shedding_Vcotton_W015, method="bonferroni");results_shedding_Vcotton_W015_Dunn


write.table(results_shedding_Vcotton_W003_Dunn$res, file = "Results/Statistics/Shedding_Vcotton_W003.csv", quote = F, sep = ",", row.names = F)
write.table(results_shedding_Vcotton_W005_Dunn$res, file = "Results/Statistics/Shedding_Vcotton_W005.csv", quote = F, sep = ",", row.names = F)
write.table(results_shedding_Vcotton_W007_Dunn$res, file = "Results/Statistics/Shedding_Vcotton_W007.csv", quote = F, sep = ",", row.names = F)
write.table(results_shedding_Vcotton_W011_Dunn$res, file = "Results/Statistics/Shedding_Vcotton_W011.csv", quote = F, sep = ",", row.names = F)
write.table(results_shedding_Vcotton_W013_Dunn$res, file = "Results/Statistics/Shedding_Vcotton_W013.csv", quote = F, sep = ",", row.names = F)
write.table(results_shedding_Vcotton_W015_Dunn$res, file = "Results/Statistics/Shedding_Vcotton_W015.csv", quote = F, sep = ",", row.names = F)

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
  compute_and_write_stats(dataframes[[i]], paste0("Results/Statistics/Shedding_Vcotton_", i, ".csv"))
}

# ------------------------------------------------------------------------
# Section 2: Garment 2
# ------------------------------------------------------------------------
#### Data Cleaning and Processing ####
# Convert Area from inch2 to mm2
# 1 pixel = 1 x10^-5 inch2, so (Area*1)/0.000011 to convert into px
Shedding_VcottonD$Area.px <- (Shedding_VcottonD$Area*1)/0.000011
# scale of the image: 1 mm = 110 pixels, 1mm2 = 12544 px
Shedding_VcottonD$Area.mm2 <- Shedding_VcottonD$Area.px/12100

# Create different dataframes for each wash
unique_washes <- unique(Shedding_VcottonD$Wash)
shedding_by_wash <- lapply(unique_washes, function(wash) {
  Shedding_VcottonD[Shedding_VcottonD$Wash == wash, ]
})
names(shedding_by_wash) <- paste("Shedding_1VcottonD_", unique_washes, sep="")

## Create different dataframes for each weight
unique_weights <- unique(Shedding_VcottonD$Weight)
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
results_Shedding_VcottonD <- do.call(rbind, lapply(names(shedding_by_wash_and_weight), function(wash) {
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
results_Shedding_VcottonD <- do.call(rbind, lapply(results_Shedding_VcottonD, function(x) x[!sapply(x, is.null)]))

# Combined data sets
results_Shedding_VcottonD_extended <- data.frame(str_split(results_Shedding_VcottonD$WashWeight, "_", simplify=TRUE))
results_Shedding_VcottonD_extended <- results_Shedding_VcottonD_extended %>%
  select("X2", "X3", "X7")
names(results_Shedding_VcottonD_extended) <- c("Garment","Wash","Weight")
results_Shedding_VcottonD <- cbind(results_Shedding_VcottonD_extended,Mean_Area=results_Shedding_VcottonD$Mean_Area,SD_Area=results_Shedding_VcottonD$SD_Area)
rm(results_Shedding_VcottonD_extended)
results_Shedding_VcottonD$check <- (results_Shedding_VcottonD$SD_Area/results_Shedding_VcottonD$Mean_Area)

#### Intermediate Data Visualisation ####
pSH_VcottonD <- ggplot(results_Shedding_VcottonD, aes(x = factor(Weight, level = c('100g', '200g', '400g','800g','1000g','2000g')),
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
ggsave("Shedding_VcottonD.png", pSH_VcottonD, width = 10, height = 9, units = "in", dpi=300, path = "Results")

# ------------------------------------------------------------------------
# Section 3: Garment 3
# ------------------------------------------------------------------------
#### Data Cleaning and Processing ####
# Convert Area from inch2 to mm2
# 1 pixel = 1 x10^-5 inch2, so (Area*1)/0.000011 to convert into px
Shedding_VcottonDC$Area.px <- (Shedding_VcottonDC$Area*1)/0.000011
# scale of the image: 1 mm = 110 pixels, 1mm2 = 12544 px
Shedding_VcottonDC$Area.mm2 <- Shedding_VcottonDC$Area.px/12100

# Create different dataframes for each wash
unique_washes <- unique(Shedding_VcottonDC$Wash)
shedding_by_wash <- lapply(unique_washes, function(wash) {
  Shedding_VcottonDC[Shedding_VcottonDC$Wash == wash, ]
})
names(shedding_by_wash) <- paste("Shedding_1VcottonDC_", unique_washes, sep="")

## Create different dataframes for each weight
unique_weights <- unique(Shedding_VcottonDC$Weight)
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
results_Shedding_VcottonDC <- do.call(rbind, lapply(names(shedding_by_wash_and_weight), function(wash) {
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
results_Shedding_VcottonDC <- do.call(rbind, lapply(results_Shedding_VcottonDC, function(x) x[!sapply(x, is.null)]))

# Combined data sets
results_shedding_VcottonDC_extended <- data.frame(str_split(results_Shedding_VcottonDC$WashWeight, "_", simplify=TRUE))
results_shedding_VcottonDC_extended <- results_shedding_VcottonDC_extended %>%
  select("X2", "X3", "X7")
names(results_shedding_VcottonDC_extended) <- c("Garment","Wash","Weight")
results_Shedding_VcottonDC <- cbind(results_shedding_VcottonDC_extended,Mean_Area=results_Shedding_VcottonDC$Mean_Area,SD_Area=results_Shedding_VcottonDC$SD_Area)
rm(results_shedding_VcottonDC_extended)
results_Shedding_VcottonDC$check <- (results_Shedding_VcottonDC$SD_Area/results_Shedding_VcottonDC$Mean_Area)

#### Intermediate Data Visualisation ####
pSH_VcottonDC <- ggplot(results_Shedding_VcottonDC, aes(x = factor(Weight, level = c('100g', '200g', '400g','800g','1000g','2000g')),
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
ggsave("Shedding_VcottonDC.png", pSH_VcottonDC, width = 10, height = 9, units = "in", dpi=300, path = "Results")

# ------------------------------------------------------------------------
# Section 4: 4th series 
# ------------------------------------------------------------------------
#### Data Cleaning and Processing ####
# Convert Area from inch2 to mm2
# 1 pixel = 1 x10^-5 inch2, so (Area*1)/0.000011 to convert into px
Shedding_5_Vcotton$Area.px <- (Shedding_5_Vcotton$Area*1)/0.000011
# 1 mm = 110 pixels, 1mm2 = 12100 px
Shedding_5_Vcotton$Area.mm2 <- Shedding_5_Vcotton$Area.px/12100

# Create different dataframes for each wash
unique_washes <- unique(Shedding_5_Vcotton$Wash)
shedding_by_wash <- lapply(unique_washes, function(wash) {
  Shedding_5_Vcotton[Shedding_5_Vcotton$Wash == wash, ]
})
names(shedding_by_wash) <- paste("Shedding_5Vcotton_", unique_washes, sep="")

## Create different dataframes for each weight
unique_weights <- unique(Shedding_5_Vcotton$Weight)
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
results_shedding_5_Vcotton <- do.call(rbind, lapply(names(shedding_by_wash_and_weight), function(wash) {
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
results_shedding_5_Vcotton <- do.call(rbind, lapply(results_shedding_5_Vcotton, function(x) x[!sapply(x, is.null)]))

# Combined data sets
results_shedding_5_Vcotton_extended <- data.frame(str_split(results_shedding_5_Vcotton$WashWeight, "_", simplify=TRUE))
results_shedding_5_Vcotton_extended <- results_shedding_5_Vcotton_extended %>%
  select("X2", "X3", "X7")
names(results_shedding_5_Vcotton_extended) <- c("Garment","Wash","Weight")
results_shedding_5_Vcotton <- cbind(results_shedding_5_Vcotton_extended,Mean_Area=results_shedding_5_Vcotton$Mean_Area,SD_Area=results_shedding_5_Vcotton$SD_Area)
rm(results_shedding_5_Vcotton_extended)
results_shedding_5_Vcotton$check <- (results_shedding_5_Vcotton$SD_Area/results_shedding_5_Vcotton$Mean_Area)

# calculation of the percentage difference between washed and unwashed
results_shedding_5_Vcotton_W000 <- results_shedding_5_Vcotton %>% filter(Wash == "W000")
results_shedding_5_Vcotton_W051 <- results_shedding_5_Vcotton %>% filter(Wash == "W051")

# Remove the 'g' from the Weight column and convert it to numeric
results_shedding_5_Vcotton_W000$Weight <- as.numeric(sub("g", "", results_shedding_5_Vcotton_W000$Weight))
results_shedding_5_Vcotton_W051$Weight <- as.numeric(sub("g", "", results_shedding_5_Vcotton_W051$Weight))
# Order the dataframe by the numeric Weight column
results_shedding_5_Vcotton_W000 <- results_shedding_5_Vcotton_W000[order(results_shedding_5_Vcotton_W000$Weight), ]
results_shedding_5_Vcotton_W051 <- results_shedding_5_Vcotton_W051[order(results_shedding_5_Vcotton_W051$Weight), ]
# convert it back after ordering
results_shedding_5_Vcotton_W000$Weight <- paste0(results_shedding_5_Vcotton_W000$Weight, "g")
results_shedding_5_Vcotton_W051$Weight <- paste0(results_shedding_5_Vcotton_W051$Weight, "g")

results_shedding_5_Vcotton_percentagediff <- data.frame(cbind(results_shedding_5_Vcotton_W000$Mean_Area,results_shedding_5_Vcotton_W051$Mean_Area))
results_shedding_5_Vcotton_percentagediff$percentagediff <- round(abs(results_shedding_5_Vcotton_percentagediff$X1 - results_shedding_5_Vcotton_percentagediff$X2) / 
  ((results_shedding_5_Vcotton_percentagediff$X1 + results_shedding_5_Vcotton_percentagediff$X2)/2)*100, digits=2)
results_shedding_5_Vcotton_percentagediff$ratio <- round(abs(results_shedding_5_Vcotton_percentagediff$X1/results_shedding_5_Vcotton_percentagediff$X2),digit=2)
label = rep(results_shedding_5_Vcotton_percentagediff$ratio,2)

#### Intermediate Data Visualisation ####
a = rep(c(100, 125, 150, 175, 200,225),2)
b = rep(c(1:6), 2)
pSH_5_Vcotton <- ggplot(results_shedding_5_Vcotton, aes(x = factor(Weight, level = c('100g', '200g', '400g','800g','1000g','2000g')),
                                 y= Mean_Area, fill=Wash))+
  geom_bar(stat="identity", position=position_dodge(),colour="black")+
  labs(x="\nWeight", y="Total fibre area (mm\u00b2)\n") +
  theme_bw(base_family = "Arial", base_size = 12) +
  ylim(0,325)+
  scale_fill_manual(values = c(brewer.pal(9, "Greys")[c(1, 9)])) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 0, vjust = 0.95, hjust=0.5))+
  geom_errorbar(aes(ymin=Mean_Area-SD_Area, ymax=Mean_Area+SD_Area),width=.2,position=position_dodge(.9))+
  geom_text(x=b, y=a+2, label=label,colour="#c9101a")+
  geom_errorbarh(aes(xmax = (b + 0.3), xmin = (b - 0.3), y = a-20), height = 15,colour="#c9101a") 
ggsave("Shedding_5_Vcotton.png", pSH_5_Vcotton, width = 10, height = 9, units = "in", dpi=300, path = "Results")
pSH_5_Vcotton
# ------------------------------------------------------------------------
# Section 5: Garment 5
# ------------------------------------------------------------------------
#### Data Cleaning and Processing ####
# Convert Area from inch2 to mm2
# 1 pixel = 1 x10^-5 inch2, so (Area*1)/0.000011 to convert into px
Shedding_12_Vcotton$Area.px <- (Shedding_12_Vcotton$Area*1)/0.000011
# scale of the image: 1 mm = 110 pixels, 1mm2 = 12544 px
Shedding_12_Vcotton$Area.mm2 <- Shedding_12_Vcotton$Area.px/12100

# Create different dataframes for each wash
unique_washes <- unique(Shedding_12_Vcotton$Wash)
shedding_by_wash <- lapply(unique_washes, function(wash) {
  Shedding_12_Vcotton[Shedding_12_Vcotton$Wash == wash, ]
})
names(shedding_by_wash) <- paste("Shedding_12Vcotton_", unique_washes, sep="")

## Create different dataframes for each weight
unique_weights <- unique(Shedding_12_Vcotton$Weight)
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
results_shedding_12_Vcotton <- do.call(rbind, lapply(names(shedding_by_wash_and_weight), function(wash) {
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
results_shedding_12_Vcotton <- do.call(rbind, lapply(results_shedding_12_Vcotton, function(x) x[!sapply(x, is.null)]))

# Combined data sets
results_shedding_12_Vcotton_extended <- data.frame(str_split(results_shedding_12_Vcotton$WashWeight, "_", simplify=TRUE))
results_shedding_12_Vcotton_extended <- results_shedding_12_Vcotton_extended %>%
  select("X2", "X3", "X7")
names(results_shedding_12_Vcotton_extended) <- c("Garment","Wash","Weight")
results_shedding_12_Vcotton <- cbind(results_shedding_12_Vcotton_extended,Mean_Area=results_shedding_12_Vcotton$Mean_Area,SD_Area=results_shedding_12_Vcotton$SD_Area)
rm(results_shedding_12_Vcotton_extended)
results_shedding_12_Vcotton$check <- (results_shedding_12_Vcotton$SD_Area/results_shedding_12_Vcotton$Mean_Area)

#### Intermediate Data Visualisation ####
results_shedding_12_Vcotton_bis <- results_shedding_12_Vcotton %>% filter(Wash == "W000" | Wash == "W041")

# calculation of the percentage difference between washed and unwashed
results_shedding_12_Vcotton_bis_W000 <- results_shedding_12_Vcotton %>% filter(Wash == "W000")
results_shedding_12_Vcotton_bis_W041 <- results_shedding_12_Vcotton %>% filter(Wash == "W041")

# Remove the 'g' from the Weight column and convert it to numeric
results_shedding_12_Vcotton_bis_W000$Weight <- as.numeric(sub("g", "", results_shedding_12_Vcotton_bis_W000$Weight))
results_shedding_12_Vcotton_bis_W041$Weight <- as.numeric(sub("g", "", results_shedding_12_Vcotton_bis_W041$Weight))
# Order the dataframe by the numeric Weight column
results_shedding_12_Vcotton_bis_W000 <- results_shedding_12_Vcotton_bis_W000[order(results_shedding_12_Vcotton_bis_W000$Weight), ]
results_shedding_12_Vcotton_bis_W041 <- results_shedding_12_Vcotton_bis_W041[order(results_shedding_12_Vcotton_bis_W041$Weight), ]
# convert it back after ordering
results_shedding_12_Vcotton_bis_W000$Weight <- paste0(results_shedding_12_Vcotton_bis_W000$Weight, "g")
results_shedding_12_Vcotton_bis_W041$Weight <- paste0(results_shedding_12_Vcotton_bis_W041$Weight, "g")

results_shedding_12_Vcotton_bis_percentagediff <- data.frame(cbind(results_shedding_12_Vcotton_bis_W000$Mean_Area,results_shedding_12_Vcotton_bis_W041$Mean_Area))
results_shedding_12_Vcotton_bis_percentagediff$percentagediff <- round(abs(results_shedding_12_Vcotton_bis_percentagediff$X1 - results_shedding_12_Vcotton_bis_percentagediff$X2) / 
                                                 ((results_shedding_12_Vcotton_bis_percentagediff$X1 + results_shedding_12_Vcotton_bis_percentagediff$X2)/2)*100, digits=2)
results_shedding_12_Vcotton_bis_percentagediff$ratio <- round(abs(results_shedding_12_Vcotton_bis_percentagediff$X1/results_shedding_12_Vcotton_bis_percentagediff$X2),digit=2)
label = rep(results_shedding_12_Vcotton_bis_percentagediff$ratio,2)


c = rep(c(100, 125, 150, 175, 200, 225),2)
d = rep(c(1:6), 2)
pSH_12_Vcotton <- ggplot(results_shedding_12_Vcotton_bis, aes(x = factor(Weight, level = c('100g', '200g', '400g','800g','1000g','2000g')),
                                              y= Mean_Area, fill=Wash))+
  geom_bar(stat="identity", position=position_dodge(),colour="black")+
  labs(x="\nWeight", y="Total fibre area (mm\u00b2)\n") +
  theme_bw(base_family = "Arial", base_size = 12) +
  ylim(0,325)+
  scale_fill_manual(values = c(brewer.pal(9, "Greys")[c(1, 9)])) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 0, vjust = 0.95, hjust=0.5))+
  geom_errorbar(aes(ymin=Mean_Area-SD_Area, ymax=Mean_Area+SD_Area),width=.2,position=position_dodge(.9))+
  geom_text(x=d, y=c+2, label=label,colour="#c9101a")+
  geom_errorbarh(aes(xmax = (d + 0.3), xmin = (d - 0.3), y = c-20), height = 15,colour="#c9101a") 
ggsave("Shedding_12_Vcotton.png", pSH_12_Vcotton, width = 10, height = 9, units = "in", dpi=300, path = "Results")
pSH_12_Vcotton

# ------------------------------------------------------------------------
# Section 6: 6th series 
# ------------------------------------------------------------------------
#### Data Cleaning and Processing ####
# Convert Area from inch2 to mm2
# 1 pixel = 1 x10^-5 inch2, so (Area*1)/0.000011 to convert into px
Shedding_Rcotton$Area.px <- (Shedding_Rcotton$Area*1)/0.000011
# 1 mm = 110 pixels, 1mm2 = 12100 px
Shedding_Rcotton$Area.mm2 <- Shedding_Rcotton$Area.px/12100

# Create different dataframes for each wash
unique_washes <- unique(Shedding_Rcotton$Wash)
shedding_by_wash <- lapply(unique_washes, function(wash) {
  Shedding_Rcotton[Shedding_Rcotton$Wash == wash, ]
})
names(shedding_by_wash) <- paste("Shedding_Rcotton_", unique_washes, sep="")

## Create different dataframes for each weight
unique_weights <- unique(Shedding_Rcotton$Weight)
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
results_shedding_Rcotton <- do.call(rbind, lapply(names(shedding_by_wash_and_weight), function(wash) {
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
results_shedding_Rcotton <- do.call(rbind, lapply(results_shedding_Rcotton, function(x) x[!sapply(x, is.null)]))

# Combined data sets
results_shedding_Rcotton_extended <- data.frame(str_split(results_shedding_Rcotton$WashWeight, "_", simplify=TRUE))
results_shedding_Rcotton_extended <- results_shedding_Rcotton_extended %>%
  select("X2", "X3", "X7")
names(results_shedding_Rcotton_extended) <- c("Garment","Wash","Weight")
results_shedding_Rcotton <- cbind(results_shedding_Rcotton_extended,Mean_Area=results_shedding_Rcotton$Mean_Area,SD_Area=results_shedding_Rcotton$SD_Area)
rm(results_shedding_Rcotton_extended)
results_shedding_Rcotton$check <- (results_shedding_Rcotton$SD_Area/results_shedding_Rcotton$Mean_Area)

# calculation of the percentage difference between washed and unwashed
results_shedding_Rcotton_W000 <- results_shedding_Rcotton %>% filter(Wash == "W000")
results_shedding_Rcotton_W025 <- results_shedding_Rcotton %>% filter(Wash == "W025")

# Remove the 'g' from the Weight column and convert it to numeric
results_shedding_Rcotton_W000$Weight <- as.numeric(sub("g", "", results_shedding_Rcotton_W000$Weight))
results_shedding_Rcotton_W025$Weight <- as.numeric(sub("g", "", results_shedding_Rcotton_W025$Weight))
# Order the dataframe by the numeric Weight column
results_shedding_Rcotton_W000 <- results_shedding_Rcotton_W000[order(results_shedding_Rcotton_W000$Weight), ]
results_shedding_Rcotton_W025 <- results_shedding_Rcotton_W025[order(results_shedding_Rcotton_W025$Weight), ]
# convert it back after ordering
results_shedding_Rcotton_W000$Weight <- paste0(results_shedding_Rcotton_W000$Weight, "g")
results_shedding_Rcotton_W025$Weight <- paste0(results_shedding_Rcotton_W025$Weight, "g")

results_shedding_Rcotton_percentagediff <- data.frame(cbind(results_shedding_Rcotton_W000$Mean_Area,results_shedding_Rcotton_W025$Mean_Area))
results_shedding_Rcotton_percentagediff$percentagediff <- round(abs(results_shedding_Rcotton_percentagediff$X1 - results_shedding_Rcotton_percentagediff$X2) / 
  ((results_shedding_Rcotton_percentagediff$X1 + results_shedding_Rcotton_percentagediff$X2)/2)*100, digits=2)
results_shedding_Rcotton_percentagediff$ratio <- round(abs(results_shedding_Rcotton_percentagediff$X1/results_shedding_Rcotton_percentagediff$X2),digit=2)
label = rep(results_shedding_Rcotton_percentagediff$ratio,2)

#### Intermediate Data Visualisation ####
e = rep(c(175, 215, 255, 295, 310, 320),2)
f = rep(c(1:6), 2)
pSH_Rcotton <- ggplot(results_shedding_Rcotton, aes(x = factor(Weight, level = c('100g', '200g', '400g','800g','1000g','2000g')),
                                 y= Mean_Area, fill=Wash))+
  geom_bar(stat="identity", position=position_dodge(),colour="black")+
  labs(x="\nWeight", y="Total fibre area (mm\u00b2)\n") +
  theme_bw(base_family = "Arial", base_size = 12) +
  ylim(0,500)+
  scale_fill_manual(values = c(brewer.pal(9, "Greys")[c(1, 9)])) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 0, vjust = 0.95, hjust=0.5))+
  geom_errorbar(aes(ymin=Mean_Area-SD_Area, ymax=Mean_Area+SD_Area),width=.2,position=position_dodge(.9))+
  geom_text(x=f, y=e+2, label=label,colour="#c9101a")+
  geom_errorbarh(aes(xmax = (f + 0.3), xmin = (f - 0.3), y = e-20), height = 15,colour="#c9101a") 
ggsave("Shedding_Rcotton.png", pSH_Rcotton, width = 10, height = 9, units = "in", dpi=300, path = "Results")
pSH_Rcotton

# ------------------------------------------------------------------------
# Section 5: Comparisons between washing conditions
# ------------------------------------------------------------------------
# Modify each pSH_GX plot to adjust legend key size and labels
pSH_Vcotton <- pSH_Vcotton + theme(legend.text = element_text(size = 12)  # Adjust the horizontal space between legend keys
)
pSH_VcottonD <- pSH_VcottonD + theme(legend.text = element_text(size = 12) # Adjust the horizontal space between legend keys
)
pSH_VcottonDC <- pSH_VcottonDC + theme(legend.text = element_text(size = 12) # Adjust the horizontal space between legend keys
)
# pSH_5_Vcotton <- pSH_5_Vcotton + theme(legend.text = element_text(size = 12)  # Adjust the horizontal space between legend keys
# )
#### Final graph - Figure XXX #### 
pCombinedSH_pending <- ggarrange(pSH_Vcotton+ rremove("ylab") + rremove("xlab"),
                                 pSH_VcottonD+ rremove("ylab") + rremove("xlab"),
                                 pSH_VcottonDC+ rremove("ylab") + rremove("xlab"),
                                 # pSH_5_Vcotton+ rremove("ylab") + rremove("xlab"),
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
ggsave("Figure XXX - Shedding all garments.png", pCombinedSH, width =8, height = 10, units = "in", dpi=600,path = "Results")


#### Final graph - Figure XXX #### 
pSH_5_Vcotton <- pSH_5_Vcotton + theme(plot.title = element_text(hjust = 0.5))
pSH_12_Vcotton <- pSH_12_Vcotton + theme(plot.title = element_text(hjust = 0.5))
pSH_Rcotton <- pSH_Rcotton + theme(plot.title = element_text(hjust = 0.5))

pCombinedSH_pendinVcottonD <- ggarrange(pSH_5_Vcotton+ rremove("ylab") + rremove("xlab"),
                                  pSH_12_Vcotton+ rremove("ylab") + rremove("xlab"),
                                  pSH_Rcotton+ rremove("ylab") + rremove("xlab"),
                                 labels = c("A              V-cotton - 5 garments","B             V-cotton - 12 garments","C           R-cotton       "),
                                 common.legend = F, legend = "right",
                                 align = "hv",
                                 ncol = 1, nrow = 3,
                                 font.label = list(size = 12, color = "black", family = NULL, position = "top"),
                                 hjust=0.05,vjust=2.45)+
  theme(plot.margin = margin(0,1,0,0, "cm")) # in order (Top,right,bottom,left)

pCombinedSH2 <- annotate_figure(pCombinedSH_pendinVcottonD, left = textGrob("Shed fibre area (mm\u00b2)\n", rot = 90, vjust = 0.5, hjust = 0.5, gp = gpar(cex =1)),
                               bottom = textGrob("weight", vjust = 0.5, hjust = 0.5,gp = gpar(cex = 1)))
pCombinedSH2

# to save the graph
ggsave("Figure XXX - Shedding 4-6 garments.png", pCombinedSH2, width =7, height = 10, units = "in", dpi=600,path = "Results")

#### EXPORT TABLE FOR ARTICLE ####
# Create a list of dataframes
dataframes <- list(Shedding_Vcotton,Shedding_VcottonD,Shedding_VcottonDC)

# Create a function to compute statistics and write to CSV
compute_and_write_stats <- function(df, filename) {
  # Assuming the 'Wash' column needs to be included in the aggregation
  mean_df <- aggregate(Area.mm2 ~ Wash + Weight, df, function(x) round(mean(x), digits = 2))
  sd_df <- aggregate(Area.mm2 ~ Wash + Weight, df, function(x) round(sd(x), digits = 2))
  median_df <- aggregate(Area.mm2 ~ Wash + Weight, df, function(x) round(median(x), digits = 2))
  
  # Merge the dataframes by 'Wash' and 'Weight' to ensure they align
  datatable <- merge(merge(mean_df, median_df, by = c("Wash", "Weight")), sd_df, by = c("Wash", "Weight"))
  
  # Rename the columns appropriately
  names(datatable) <- c("Wash", "Weight", "Average", "Median", "SD")
  
  # Write the datatable to a CSV file
  write.table(datatable, file = filename, sep = ",", row.names = FALSE, col.names = TRUE, quote = FALSE, fileEncoding = "UTF-8")
}

# Loop through the dataframes and compute/write statistics
for (i in 1:length(dataframes)) {
  compute_and_write_stats(dataframes[[i]], paste("Results/Statistics/Shedding Descriptive statistics - Series ", c(1, 2, 3)[i], ".csv", sep = ""))
}

# Create a list of dataframes
dataframes <- list(Shedding_5_Vcotton,Shedding_12_Vcotton,Shedding_Rcotton)

# Create a function to compute statistics and write to CSV
compute_and_write_stats <- function(df, filename) {
  # Assuming the 'Wash' column needs to be included in the aggregation
  mean_df <- aggregate(Area.mm2 ~ Wash + Weight, df, function(x) round(mean(x), digits = 2))
  sd_df <- aggregate(Area.mm2 ~ Wash + Weight, df, function(x) round(sd(x), digits = 2))
  median_df <- aggregate(Area.mm2 ~ Wash + Weight, df, function(x) round(median(x), digits = 2))
  
  # Merge the dataframes by 'Wash' and 'Weight' to ensure they align
  datatable <- merge(merge(mean_df, median_df, by = c("Wash", "Weight")), sd_df, by = c("Wash", "Weight"))
  
  # Rename the columns appropriately
  names(datatable) <- c("Wash", "Weight", "Average", "Median", "SD")
  
  # Write the datatable to a CSV file
  write.table(datatable, file = filename, sep = ",", row.names = FALSE, col.names = TRUE, quote = FALSE, fileEncoding = "UTF-8")
}

# Loop through the dataframes and compute/write statistics
for (i in 1:length(dataframes)) {
  compute_and_write_stats(dataframes[[i]], paste("Results/Statistics/Shedding Descriptive statistics - Series ", c(4, 5, 6)[i], ".csv", sep = ""))
}

# Table for comparisons in discussion
# Define the weights you want to process
weights <- c("100g", "200g", "400g", "800g", "1000g", "2000g")

# Loop over each weight
for (weight in weights) {
  # Filter each dataset by the current weight and fixed wash condition
  Shedding_Vcotton_table <- filter(Shedding_Vcotton, Weight == weight & Wash == "W000")
  Shedding_VcottonD_table <- filter(Shedding_VcottonD, Weight == weight & Wash == "W000")
  Shedding_VcottonDC_table <- filter(Shedding_VcottonDC, Weight == weight & Wash == "W000")
  
  # Combine the tables
  Shedding_table <- rbind(Shedding_Vcotton_table, Shedding_VcottonD_table, Shedding_VcottonDC_table)
  
  # Construct file name based on the weight
  file_name <- sprintf("Results/Statistics/Shedding_table_%s.csv", gsub("g", "", weight))
  
  # Write the combined table to a CSV file
  write.table(Shedding_table, file = file_name, quote = FALSE, sep = ",", row.names = FALSE)
}
