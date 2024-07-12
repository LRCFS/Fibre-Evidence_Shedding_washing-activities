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
# ggsave("Shedding_Vcotton.png", pSH_Vcotton, width = 10, height = 9, units = "in", dpi=300, path = "Results")

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
# ggsave("Shedding_VcottonD.png", pSH_VcottonD, width = 10, height = 9, units = "in", dpi=300, path = "Results")

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
# ggsave("Shedding_VcottonDC.png", pSH_VcottonDC, width = 10, height = 9, units = "in", dpi=300, path = "Results")

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
#ggsave("Shedding_5_Vcotton.png", pSH_5_Vcotton, width = 10, height = 9, units = "in", dpi=300, path = "Results")
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
#ggsave("Shedding_12_Vcotton.png", pSH_12_Vcotton, width = 10, height = 9, units = "in", dpi=300, path = "Results")
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
# ggsave("Shedding_Rcotton.png", pSH_Rcotton, width = 10, height = 9, units = "in", dpi=300, path = "Results")
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
ggsave("Figure 10 - Shedding 1st to 3rd series.png", pCombinedSH, width =8, height = 10, units = "in", dpi=600,path = "Results")


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
ggsave("Figure 11 - Shedding 4th to 6th series.png", pCombinedSH2, width =7, height = 10, units = "in", dpi=600,path = "Results")

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

# # Table for comparisons in discussion
# # Define the weights you want to process
# weights <- c("100g", "200g", "400g", "800g", "1000g", "2000g")
# 
# # Loop over each weight
# for (weight in weights) {
#   # Filter each dataset by the current weight and fixed wash condition
#   Shedding_Vcotton_table <- filter(Shedding_Vcotton, Weight == weight & Wash == "W000")
#   Shedding_VcottonD_table <- filter(Shedding_VcottonD, Weight == weight & Wash == "W000")
#   Shedding_VcottonDC_table <- filter(Shedding_VcottonDC, Weight == weight & Wash == "W000")
#   Shedding_5_Vcotton_table <- filter(Shedding_5_Vcotton, Weight == weight & Wash == "W000")
#   Shedding_12_Vcotton_table <- filter(Shedding_12_Vcotton, Weight == weight & Wash == "W000")
#   
#   # Combine the tables
#   Shedding_table <- rbind(Shedding_Vcotton_table, Shedding_VcottonD_table, Shedding_VcottonDC_table,Shedding_5_Vcotton_table,Shedding_12_Vcotton_table)
#   
#   # Construct file name based on the weight
#   file_name <- sprintf("Results/Statistics/Shedding_table_%s.csv", gsub("g", "", weight))
#   
#   # Write the combined table to a CSV file
#   write.table(Shedding_table, file = file_name, quote = FALSE, sep = ",", row.names = FALSE)
# }

#### STATS ####
# Create the combined dataframe for statistical analysis
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

# Test of normality - shapiro test
# List of dataframe names
dataframe_names <- c("For_Stats_Shedding_100g","For_Stats_Shedding_200g","For_Stats_Shedding_400g",
                     "For_Stats_Shedding_800g","For_Stats_Shedding_1000g","For_Stats_Shedding_2000g")
# Function to perform Shapiro-Wilk test and interpret results
perform_shapiro_test <- function(df_name) {
  dataframe <- get(df_name)
  test_result <- shapiro.test(dataframe$Area.mm2)
  interpretation <- ifelse(test_result$p.value < 0.05, "Not normally distributed", "Normally distributed")
  message(paste(df_name, "- p-value:", test_result$p.value, interpretation))
}
# Apply the function to each dataframe name
lapply(dataframe_names, perform_shapiro_test)
# For_Stats_Shedding_100g - p-value: 1.02873994007177e-11 Not normally distributed
# For_Stats_Shedding_200g - p-value: 8.34126968552537e-11 Not normally distributed
# For_Stats_Shedding_400g - p-value: 5.14663283980105e-10 Not normally distributed
# For_Stats_Shedding_800g - p-value: 2.76486168097498e-08 Not normally distributed
# For_Stats_Shedding_1000g - p-value: 4.44185544889641e-09 Not normally distributed
# For_Stats_Shedding_2000g - p-value: 1.30458853700905e-07 Not normally distributed

# Analysis for the normally distributed wash
### sphericity test
#Levene’s Test
#H0: All sample variances are equal
#H1: At least one group has a variance that is not equal to the rest.
For_Stats_Shedding_100g$Garment <- as.factor(For_Stats_Shedding_100g$Garment)
leveneTest(Area.mm2 ~ Garment, For_Stats_Shedding_100g) # p-value = 0.2216,  equal variances

For_Stats_Shedding_200g$Garment <- as.factor(For_Stats_Shedding_200g$Garment)
leveneTest(Area.mm2 ~ Garment, For_Stats_Shedding_200g) # p-value = 0.1686,  equal variances

For_Stats_Shedding_400g$Garment <- as.factor(For_Stats_Shedding_400g$Garment)
leveneTest(Area.mm2 ~ Garment, For_Stats_Shedding_400g) # p-value = 0.8188,  equal variances

For_Stats_Shedding_800g$Garment <- as.factor(For_Stats_Shedding_800g$Garment)
leveneTest(Area.mm2 ~ Garment, For_Stats_Shedding_800g) # p-value = 0.6499,  equal variances

For_Stats_Shedding_1000g$Garment <- as.factor(For_Stats_Shedding_1000g$Garment)
leveneTest(Area.mm2 ~ Garment, For_Stats_Shedding_1000g) # p-value = 0.5355,  equal variances

For_Stats_Shedding_2000g$Garment <- as.factor(For_Stats_Shedding_2000g$Garment)
leveneTest(Area.mm2 ~ Garment, For_Stats_Shedding_2000g) # p-value = 0.7381,  equal variances

# Analysis for the non normally distributed data
For_Stats_Shedding_100g$Garment <- as.factor(For_Stats_Shedding_100g$Garment)
kruskal.test(Area.mm2 ~ Garment, data = For_Stats_Shedding_100g)#  p-value = 0.07137 NS

For_Stats_Shedding_200g$Garment <- as.factor(For_Stats_Shedding_200g$Garment)
kruskal.test(Area.mm2 ~ Garment, data = For_Stats_Shedding_200g)#  p-value = 0.022 *

For_Stats_Shedding_400g$Garment <- as.factor(For_Stats_Shedding_400g$Garment)
kruskal.test(Area.mm2 ~ Garment, data = For_Stats_Shedding_400g)#  p-value = 0.02152 *

For_Stats_Shedding_800g$Garment <- as.factor(For_Stats_Shedding_800g$Garment)
kruskal.test(Area.mm2 ~ Garment, data = For_Stats_Shedding_800g)#  p-value = 0.1474 NS

For_Stats_Shedding_1000g$Garment <- as.factor(For_Stats_Shedding_1000g$Garment)
kruskal.test(Area.mm2 ~ Garment, data = For_Stats_Shedding_1000g)#  p-value = 0.05226 NS

For_Stats_Shedding_2000g$Garment <- as.factor(For_Stats_Shedding_2000g$Garment)
kruskal.test(Area.mm2 ~ Garment, data = For_Stats_Shedding_2000g)#  p-value = 0.04425 *

For_Stats_Shedding_100g_Dunn <- dunnTest(Area.mm2 ~ Garment, data=For_Stats_Shedding_100g, method="bonferroni");For_Stats_Shedding_100g_Dunn
For_Stats_Shedding_200g_Dunn <- dunnTest(Area.mm2 ~ Garment, data=For_Stats_Shedding_200g, method="bonferroni");For_Stats_Shedding_200g_Dunn
For_Stats_Shedding_400g_Dunn <- dunnTest(Area.mm2 ~ Garment, data=For_Stats_Shedding_400g, method="bonferroni");For_Stats_Shedding_400g_Dunn
For_Stats_Shedding_800g_Dunn <- dunnTest(Area.mm2 ~ Garment, data=For_Stats_Shedding_800g, method="bonferroni");For_Stats_Shedding_800g_Dunn
For_Stats_Shedding_1000g_Dunn <- dunnTest(Area.mm2 ~ Garment, data=For_Stats_Shedding_1000g, method="bonferroni");For_Stats_Shedding_1000g_Dunn
For_Stats_Shedding_2000g_Dunn <- dunnTest(Area.mm2 ~ Garment, data=For_Stats_Shedding_2000g, method="bonferroni");For_Stats_Shedding_2000g_Dunn

For_Stats_Shedding_Dunn_Table <- cbind(For_Stats_Shedding_100g_Dunn$res,For_Stats_Shedding_200g_Dunn$res,For_Stats_Shedding_400g_Dunn$res,
                                       For_Stats_Shedding_800g_Dunn$res,For_Stats_Shedding_1000g_Dunn$res,For_Stats_Shedding_2000g_Dunn$res)
write.table(For_Stats_Shedding_Dunn_Table, file = "Results/Statistics/Shedding_Stats_raw.csv", quote = F, sep = ",", row.names = F)
