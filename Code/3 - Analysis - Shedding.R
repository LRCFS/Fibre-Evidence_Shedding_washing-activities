#########################################################
#####                    SHEDDING                   #####
#########################################################

# This R script is to generate the figures related to:
# 1. Shedding data
  # 1-1. First Series - 1 v-cotton garment, no detergent, no softener
  # 1-2. Second Series - 1 v-cotton garment, detergent, no softener
  # 1-3. Third Series - 1 v-cotton garment, detergent, softener
  # 1-4. Fourth Series - 5 v-cotton garments, no detergent, no softener
  # 1-5. Fifth Series - 12 garments, no detergent, no softener
  # 1-6. Sixth Series - 1 r-cotton textile, no detergent, no softener
# 2.Comparisons between washing conditions
# 3. Statistics
# ------------------------------------------------------------------------
# Section 1: Shedding data
# ------------------------------------------------------------------------
# ------------------------------------------------------------------------
# Section 1-1: First Series - 1 v-cotton garment, no detergent, no softener
# ------------------------------------------------------------------------
#### Data Cleaning and Processing ####
# Convert Area from inch2 to mm2
# 1 pixel = 1 x10^-5 inch2, so (Area*1)/0.000011 to convert into px
Shedding_Vcotton$Area.px <- (Shedding_Vcotton$Area*1)/0.000011
# scale of the image: 1 mm = 110 pixels, 1mm2 = 12100 px
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
# ggsave("Shedding_Vcotton.png", pSH_Vcotton, width = 10, height = 9, units = "in", dpi=300, path = "Results")

# ------------------------------------------------------------------------
# Section 1-2: Second Series - 1 v-cotton garment, detergent, no softener
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
# ggsave("Shedding_VcottonD.png", pSH_VcottonD, width = 10, height = 9, units = "in", dpi=300, path = "Results")

# ------------------------------------------------------------------------
# Section 1-3: Third Series - 1 v-cotton garment, detergent, softener
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
# ggsave("Shedding_VcottonDC.png", pSH_VcottonDC, width = 10, height = 9, units = "in", dpi=300, path = "Results")

# ------------------------------------------------------------------------
# Section 1-4: Fourth Series - 5 v-cotton garments, no detergent, no softener
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
#ggsave("Shedding_5_Vcotton.png", pSH_5_Vcotton, width = 10, height = 9, units = "in", dpi=300, path = "Results")
pSH_5_Vcotton
# ------------------------------------------------------------------------
# Section 1-5: Fifth Series - 12 garments, no detergent, no softener
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
#ggsave("Shedding_12_Vcotton.png", pSH_12_Vcotton, width = 10, height = 9, units = "in", dpi=300, path = "Results")
pSH_12_Vcotton

# ------------------------------------------------------------------------
# Section 1-6: Sixth Series - 1 r-cotton textile, no detergent, no softener
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
# ggsave("Shedding_Rcotton.png", pSH_Rcotton, width = 10, height = 9, units = "in", dpi=300, path = "Results")
pSH_Rcotton

# ------------------------------------------------------------------------
# Section 2: Comparisons between washing conditions
# ------------------------------------------------------------------------
#### Data Cleaning and Processing ####
# Modify each pSH_GX plot to adjust legend key size and labels
pSH_Vcotton <- pSH_Vcotton + theme(legend.text = element_text(size = 12)  # Adjust the horizontal space between legend keys
)
pSH_VcottonD <- pSH_VcottonD + theme(legend.text = element_text(size = 12) # Adjust the horizontal space between legend keys
)
pSH_VcottonDC <- pSH_VcottonDC + theme(legend.text = element_text(size = 12) # Adjust the horizontal space between legend keys
)
# pSH_5_Vcotton <- pSH_5_Vcotton + theme(legend.text = element_text(size = 12)  # Adjust the horizontal space between legend keys
# )
#### Final graph - Figure 11 #### 
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
ggsave("Figure 11 - Shedding 1st to 3rd series.png", pCombinedSH, width =8, height = 10, units = "in", dpi=600,path = "Results")

#### Final graph - Figure 12 #### 
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
ggsave("Figure 12 - Shedding 4th to 6th series.png", pCombinedSH2, width =7, height = 10, units = "in", dpi=600,path = "Results")

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
  Shedding_5_Vcotton_table <- filter(Shedding_5_Vcotton, Weight == weight & Wash == "W000")
  Shedding_12_Vcotton_table <- filter(Shedding_12_Vcotton, Weight == weight & Wash == "W000")

  # Combine the tables
  Shedding_table <- rbind(Shedding_Vcotton_table, Shedding_VcottonD_table, Shedding_VcottonDC_table,Shedding_5_Vcotton_table,Shedding_12_Vcotton_table)

  # Construct file name based on the weight
  file_name <- sprintf("Results/Statistics/Shedding_table_%s.csv", gsub("g", "", weight))

  # Write the combined table to a CSV file
  write.table(Shedding_table, file = file_name, quote = FALSE, sep = ",", row.names = FALSE)
}

# ------------------------------------------------------------------------
# Section 3: Statistics
# ------------------------------------------------------------------------
#### Create the combined dataframe for statistical analysis ####
For_Stats <- rbind(Shedding_Vcotton, Shedding_VcottonD, Shedding_VcottonDC)

# Function to create a subset of For_Stats data based on weight pattern and garment
createTransfer_Subset <- function(For_Stats, weightPattern, garment) {
  subset <- For_Stats %>%
    filter(Weight == weightPattern & Garment == garment)
  
  subset$weightCoder <- weightPattern
  
  return(subset)
}

# Define the weights and garments based on unique values from the dataframe
weights <- c('100g','200g','400g','800g','1000g','2000g')
garments <- c('1Vcotton','1VcottonD','1VcottonDC')

# Create subsets of the data for each combination of garment and weight
for (garment in garments) {
  for (weight in weights) {
    assign(paste0("For_Stats_Shedding", "_", weight, "_", garment), 
           createTransfer_Subset(For_Stats, weight, garment))
  }
}

# Combine all the subsets for Red with 180 rotation into one dataframe
For_Stats_Shedding_100g <- rbind(For_Stats_Shedding_100g_1Vcotton, For_Stats_Shedding_100g_1VcottonD,For_Stats_Shedding_100g_1VcottonDC)
For_Stats_Shedding_200g <- rbind(For_Stats_Shedding_200g_1Vcotton, For_Stats_Shedding_200g_1VcottonD,For_Stats_Shedding_200g_1VcottonDC)
For_Stats_Shedding_400g <- rbind(For_Stats_Shedding_400g_1Vcotton, For_Stats_Shedding_400g_1VcottonD,For_Stats_Shedding_400g_1VcottonDC)
For_Stats_Shedding_800g <- rbind(For_Stats_Shedding_800g_1Vcotton, For_Stats_Shedding_800g_1VcottonD,For_Stats_Shedding_800g_1VcottonDC)
For_Stats_Shedding_1000g <- rbind(For_Stats_Shedding_1000g_1Vcotton, For_Stats_Shedding_1000g_1VcottonD,For_Stats_Shedding_1000g_1VcottonDC)
For_Stats_Shedding_2000g <- rbind(For_Stats_Shedding_2000g_1Vcotton, For_Stats_Shedding_2000g_1VcottonD,For_Stats_Shedding_2000g_1VcottonDC)

#### Friedman test - Shedding 1st to 3rd series ####
# List of weights to be processed
weights <- c(100, 200, 400, 800, 1000, 2000)

# Initialize an empty list to store Friedman test results
friedman_results <- list()

# Loop through each weight
for (weight in weights) {
  # Construct the variable name for the current weight's data
  data_var <- get(paste0("For_Stats_Shedding_", weight, "g"))
  
  # Aggregate the data to get the mean Area.mm2 for each Wash and Garment combination
  result <- aggregate(Area.mm2 ~ Wash + Garment, data = data_var, FUN = mean)
  
  # Get summary statistics for the aggregated data
  result %>%
    group_by(Garment) %>%
    get_summary_stats(Area.mm2, type = "common")
  
  # Create a box plot with jittered points for the current weight's data
  ggboxplot(result, x = "Garment", y = "Area.mm2", add = "jitter")
  
  # Perform the Friedman test on the aggregated data
  res.fried <- result %>% friedman_test(Area.mm2 ~ Garment | Wash)
  
  # Store the Friedman test result in the list, using the weight as the key
  friedman_results[[as.character(weight)]] <- res.fried
}

# Combine all Friedman test results into a single data frame
For_Stats_Shedding_Friedman_Table <- do.call(rbind, friedman_results)

# Add a column for the weights to the combined data frame
For_Stats_Shedding_Friedman_Table$Weight <- names(friedman_results)

# Select relevant columns for the final table
For_Stats_Shedding_Friedman_Table <- For_Stats_Shedding_Friedman_Table %>%
  select(Weight, statistic, p)

# Rename columns for better readability
names(For_Stats_Shedding_Friedman_Table) <- c("Weight", "Friedman_statistic", "p-value")

# Print the final Friedman test results table
For_Stats_Shedding_Friedman_Table

#### ANOVA test and Kruskal-Wallis Test- Shedding 4th to 6th series ####
### Difference between garments
# Function to filter data by wash and weight
filter_data <- function(data, wash, weight) {
  data_filtered <- filter(data, Wash == wash)
  if (!missing(weight)) {
    data_filtered <- filter(data_filtered, Weight == weight)
  }
  return(data_filtered)
}

# List of datasets
datasets <- list(Shedding_Vcotton, Shedding_VcottonD, Shedding_VcottonDC, Shedding_5_Vcotton, Shedding_12_Vcotton)

# Names for the datasets
dataset_names <- c("Vcotton", "VcottonD", "VcottonDC", "5Vcotton", "12Vcotton")

# Filter datasets by wash "W000"
filtered_datasets <- lapply(datasets, filter_data, wash = "W000")

# Filter datasets by weight
weights <- c("100g", "200g", "400g", "800g", "1000g", "2000g")
filtered_by_weight <- lapply(weights, function(wt) {
  lapply(filtered_datasets, filter_data, wash = "W000", weight = wt)
})

# Combine filtered datasets by weight
combined_data <- lapply(filtered_by_weight, function(weight_list) {
  do.call(rbind, weight_list)
})

# Perform Shapiro test
shapiro_results <- lapply(combined_data, function(data) {
  shapiro_test(data$Area.mm2)
})

# Assign names to results for clarity
names(shapiro_results) <- paste("forstats_W000", weights, sep = "_")

# Display results
shapiro_results

# New data frames
df_names <- c("forstats_W000_100", "forstats_W000_200", "forstats_W000_400", 
              "forstats_W000_800", "forstats_W000_1000", "forstats_W000_2000")

# Iterate through filtered_by_weight, bind rows of the list elements and assign them as dataframes
for (i in seq_along(filtered_by_weight)) {
  # Combine the list of data frames into one data frame
  combined_df <- do.call(rbind, filtered_by_weight[[i]])
  
  # Assign the combined data frame to the respective variable name
  assign(df_names[i], combined_df)
}

forstats_W000_100$Garment <- as.factor(forstats_W000_100$Garment)
forstats_W000_200$Garment <- as.factor(forstats_W000_200$Garment)
forstats_W000_400$Garment <- as.factor(forstats_W000_400$Garment)
forstats_W000_800$Garment <- as.factor(forstats_W000_800$Garment)
forstats_W000_1000$Garment <- as.factor(forstats_W000_1000$Garment)
forstats_W000_2000$Garment <- as.factor(forstats_W000_2000$Garment)

# Kruskal-Wallis test (800g and 1000g)
kruskal_test_result_800 <- kruskal.test(Area.mm2 ~ Garment, data = forstats_W000_800)
print(kruskal_test_result_800) # p-value = 0.003707, differences between groups

kruskal_test_result_1000 <- kruskal.test(Area.mm2 ~ Garment, data = forstats_W000_1000)
print(kruskal_test_result_1000) # p-value = 0.01467, differences between groups

# Perform Dunn's test for multiple comparisons
dunn_test_result_800 <- dunnTest(Area.mm2 ~ Garment,data = forstats_W000_800,method = "bonferroni")
print(dunn_test_result_800)

dunn_test_result_1000 <- dunnTest(Area.mm2 ~ Garment,data = forstats_W000_1000,method = "bonferroni")
print(dunn_test_result_1000)

Table_dunn_result_800 <-dunn_test_result_800$res
Table_dunn_result_1000 <-dunn_test_result_1000$res
Table_dunn_results <- rbind(Table_dunn_result_800,Table_dunn_result_1000)
write.table(Table_dunn_results, file = "Results/Statistics/Table_dunn_results_shedding differences between garments.csv", quote = F, sep = ",", row.names = F)

# ANOVA
res_aov100 <- aov(Area.mm2 ~ Garment, data = forstats_W000_100)
summary(res_aov100) #  p-value = 0.00347 , indicating that there are statistically significant differences

res_aov200 <- aov(Area.mm2 ~ Garment, data = forstats_W000_200)
summary(res_aov200) #  p-value = 0.00307 , indicating that there are statistically significant differences

res_aov400 <- aov(Area.mm2 ~ Garment, data = forstats_W000_400)
summary(res_aov400) #  p-value = 0.00147 , indicating that there are statistically significant differences

res_aov2000 <- aov(Area.mm2 ~ Garment, data = forstats_W000_2000)
summary(res_aov2000) #  p-value = 0.0135   , indicating that there are statistically significant differences

# Tukey HSD test:
post_test100 <- glht(res_aov100, linfct = mcp(Garment = "Tukey"))
summary_post_test100 <- summary(post_test100);summary_post_test100

post_test200 <- glht(res_aov200, linfct = mcp(Garment = "Tukey"))
summary_post_test200 <- summary(post_test200);summary_post_test200

post_test400 <- glht(res_aov400, linfct = mcp(Garment = "Tukey"))
summary_post_test400 <- summary(post_test400);summary_post_test400

post_test2000 <- glht(res_aov2000, linfct = mcp(Garment = "Tukey"))
summary_post_test2000 <- summary(post_test2000);summary_post_test2000

# Save statistics tables
dataframes <- list(
  summary_post_test100 = summary_post_test100, 
  summary_post_test200 = summary_post_test200,
  summary_post_test400 = summary_post_test400, 
  summary_post_test2000 = summary_post_test2000
)

# Create a function to compute statistics
compute_stats <- function(df) {
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
  
  return(datatable)
}

# Compute statistics for each dataframe and add a column for the source
combined_stats <- do.call(rbind, lapply(names(dataframes), function(name) {
  df <- compute_stats(dataframes[[name]])
  df$Source <- name
  return(df)
}))

# Define the desired filename
filename <- "Results/Statistics/Statistics_ANOVA_Shedding_difference between garments.csv"

# Ensure the Results directory exists
if (!dir.exists(dirname(filename))) {
  dir.create(dirname(filename), recursive = TRUE)
}

# Write the combined dataframe to a CSV file
write.table(combined_stats, file = filename, sep = ",", row.names = FALSE, col.names = TRUE, quote = FALSE, fileEncoding = "UTF-8")