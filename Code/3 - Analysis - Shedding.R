#########################################################
#####                  SHEDDING TEST                #####
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
results_G1 <- do.call(rbind, lapply(names(shedding_by_wash_and_weight), function(wash) {
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
results_G1 <- do.call(rbind, lapply(results_G1, function(x) x[!sapply(x, is.null)]))

# Combined data sets
results_G1_extended <- data.frame(str_split(results_G1$WashWeight, "_", simplify=TRUE))
results_G1_extended <- results_G1_extended %>%
  select("X2", "X3", "X7")
names(results_G1_extended) <- c("Garment","Wash","Weight")
results_G1 <- cbind(results_G1_extended,Mean_Area=results_G1$Mean_Area,SD_Area=results_G1$SD_Area)
rm(results_G1_extended)

#### Intermediate Data Visualisation ####
pSH_G1 <- ggplot(results_G1, aes(x = factor(Weight, level = c('100g', '200g', '400g','800g','1000g','2000g')),
                                          y= Mean_Area, fill=Wash))+
  geom_bar(stat="identity", position=position_dodge(),colour="black")+
  labs(x="\nWeight", y="Total fibre area (mm\u00b2)\n") +
  theme_bw(base_family = "Arial", base_size = 12) +
  ylim(0,550)+
  scale_fill_manual(values = brewer.pal(9, "Greys")[1:9])+
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 0, vjust = 0.95, hjust=0.5))+
  geom_errorbar(aes(ymin=Mean_Area-SD_Area, ymax=Mean_Area+SD_Area),width=.2,position=position_dodge(.9))
ggsave("Shedding_G1.png", pSH_G1, width = 10, height = 9, units = "in", dpi=300, path = "Results")

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
results_G2 <- do.call(rbind, lapply(names(shedding_by_wash_and_weight), function(wash) {
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
results_G2 <- do.call(rbind, lapply(results_G2, function(x) x[!sapply(x, is.null)]))

# Combined data sets
results_G2_extended <- data.frame(str_split(results_G2$WashWeight, "_", simplify=TRUE))
results_G2_extended <- results_G2_extended %>%
  select("X2", "X3", "X7")
names(results_G2_extended) <- c("Garment","Wash","Weight")
results_G2 <- cbind(results_G2_extended,Mean_Area=results_G2$Mean_Area,SD_Area=results_G2$SD_Area)
rm(results_G2_extended)

#### Intermediate Data Visualisation ####
pSH_G2 <- ggplot(results_G2, aes(x = factor(Weight, level = c('100g', '200g', '400g','800g','1000g','2000g')),
                                 y= Mean_Area, fill=Wash))+
  geom_bar(stat="identity", position=position_dodge(),colour="black")+
  labs(x="\nWeight", y="Total fibre area (mm\u00b2)\n") +
  theme_bw(base_family = "Arial", base_size = 12) +
  ylim(0,550)+
  scale_fill_manual(values = brewer.pal(9, "Greys")[1:9])+
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
results_G3 <- do.call(rbind, lapply(names(shedding_by_wash_and_weight), function(wash) {
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
results_G3 <- do.call(rbind, lapply(results_G3, function(x) x[!sapply(x, is.null)]))

# Combined data sets
results_G3_extended <- data.frame(str_split(results_G3$WashWeight, "_", simplify=TRUE))
results_G3_extended <- results_G3_extended %>%
  select("X2", "X3", "X7")
names(results_G3_extended) <- c("Garment","Wash","Weight")
results_G3 <- cbind(results_G3_extended,Mean_Area=results_G3$Mean_Area,SD_Area=results_G3$SD_Area)
rm(results_G3_extended)

#### Intermediate Data Visualisation ####
pSH_G3 <- ggplot(results_G3, aes(x = factor(Weight, level = c('100g', '200g', '400g','800g','1000g','2000g')),
                                 y= Mean_Area, fill=Wash))+
  geom_bar(stat="identity", position=position_dodge(),colour="black")+
  labs(x="\nWeight", y="Total fibre area (mm\u00b2)\n") +
  theme_bw(base_family = "Arial", base_size = 12) +
  ylim(0,550)+
  scale_fill_manual(values = brewer.pal(9, "Greys")[1:9])+
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 0, vjust = 0.95, hjust=0.5))+
  geom_errorbar(aes(ymin=Mean_Area-SD_Area, ymax=Mean_Area+SD_Area),width=.2,position=position_dodge(.9))
ggsave("Shedding_G3.png", pSH_G3, width = 10, height = 9, units = "in", dpi=300, path = "Results")

# ------------------------------------------------------------------------
# Section 4: Garment 4
# ------------------------------------------------------------------------
#### Data Cleaning and Processing ####
# Convert Area from inch2 to mm2
# 1 pixel = 1 x10^-5 inch2, so (Area*1)/0.000011 to convert into px
Shedding_G4$Area.px <- (Shedding_G4$Area*1)/0.000011
# scale of the image: 1 mm = 110 pixels, 1mm2 = 12544 px
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
results_G4 <- do.call(rbind, lapply(names(shedding_by_wash_and_weight), function(wash) {
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
results_G4 <- do.call(rbind, lapply(results_G4, function(x) x[!sapply(x, is.null)]))

# Combined data sets
results_G4_extended <- data.frame(str_split(results_G4$WashWeight, "_", simplify=TRUE))
results_G4_extended <- results_G4_extended %>%
  select("X2", "X3", "X7")
names(results_G4_extended) <- c("Garment","Wash","Weight")
results_G4 <- cbind(results_G4_extended,Mean_Area=results_G4$Mean_Area,SD_Area=results_G4$SD_Area)
rm(results_G4_extended)

#### Intermediate Data Visualisation ####
pSH_G4 <- ggplot(results_G4, aes(x = factor(Weight, level = c('100g', '200g', '400g','800g','1000g','2000g')),
                                 y= Mean_Area, fill=Wash))+
  geom_bar(stat="identity", position=position_dodge(),colour="black")+
  labs(x="\nWeight", y="Total fibre area (mm\u00b2)\n") +
  theme_bw(base_family = "Arial", base_size = 12) +
  ylim(0,550)+
  scale_fill_manual(values = brewer.pal(9, "Greys")[1:9])+
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 0, vjust = 0.95, hjust=0.5))+
  geom_errorbar(aes(ymin=Mean_Area-SD_Area, ymax=Mean_Area+SD_Area),width=.2,position=position_dodge(.9))
ggsave("Shedding_G4.png", pSH_G4, width = 10, height = 9, units = "in", dpi=300, path = "Results")

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
pSH_G4 <- pSH_G4 + theme(legend.text = element_text(size = 12)  # Adjust the horizontal space between legend keys
)
#### Final graph - Figure XXX #### 
pCombinedSH_pending <- ggarrange(pSH_G1+ rremove("ylab") + rremove("xlab"),
                                 pSH_G2+ rremove("ylab") + rremove("xlab"),
                                 pSH_G3+ rremove("ylab") + rremove("xlab"),
                                 pSH_G4+ rremove("ylab") + rremove("xlab"),
                                 labels = c("A", "B", "C", "D"),
                                 common.legend = TRUE, legend = "right",
                                 align = "hv",
                                 ncol = 1, nrow = 4,
                                 font.label = list(size = 12, color = "black", family = NULL, position = "top"))+
  theme(plot.margin = margin(0,1,0,0, "cm")) # in order (Top,right,bottom,left)

pCombinedSH <- annotate_figure(pCombinedSH_pending, left = textGrob("Total fibre area (mm\u00b2)\n", rot = 90, vjust = 0.5, hjust = 0.5, gp = gpar(cex =1)),
                               bottom = textGrob("weight", vjust = 0.5, hjust = 0.5,gp = gpar(cex = 1)))
pCombinedSH

# to save the graph
ggsave("Figure XXX - Shedding all garments.png", pCombinedSH, width =8, height = 9, units = "in", dpi=600,path = "Results")


# Ensure the data is ordered consistently by weight category if necessary
results_G1 <- results_G1[order(results_G1$Wash, results_G1$Weight), ]
results_G2 <- results_G2[order(results_G2$Wash, results_G2$Weight), ]

# Calculate percentage change for each weight category
percentage_change <- (results_G2$Mean_Area - results_G1$Mean_Area) / results_G1$Mean_Area * 100

# Combine the results into a data frame
results <- data.frame(
  Wash = results_G1$Wash,
  Weight = results_G1$Weight,
  results_G1 = results_G1$Mean_Area,
  results_G2 = results_G2$Mean_Area,
  Percentage_Change = percentage_change
)

# plot
pSH_G1vsG2 <- ggplot(results, aes(x = factor(Weight, level = c('100g', '200g', '400g','800g','1000g','2000g')),
                                 y= percentage_change, fill=Wash))+
  geom_bar(stat="identity", position=position_dodge(),colour="black")+
  labs(x="\nWeight", y="Total fibre area (mm\u00b2)\n") +
  theme_bw(base_family = "Arial", base_size = 12) +
  ylim(-100,900)+
  scale_fill_manual(values = brewer.pal(9, "Greys")[1:9])+
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 0, vjust = 0.95, hjust=0.5))
pSH_G1vsG2
ggsave("Shedding_G1.png", pSH_G1, width = 10, height = 9, units = "in", dpi=300, path = "Results")
