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
Transfer_G12 <- separate(G12_Dataset, Sample, into = c("date", "exp", "wash","garment", "band", "sample"), sep = "_")

# substract background
Transfer_G1$value <-  Transfer_G1$`After transfer` -  Transfer_G1$`Before transfer`
Transfer_G2$value <-  Transfer_G2$`After transfer` -  Transfer_G2$`Before transfer`
Transfer_G3$value <-  Transfer_G3$`After transfer` -  Transfer_G3$`Before transfer`
Transfer_G12$value <-  Transfer_G12$`After transfer` -  Transfer_G12$`Before transfer`

# Rename garment name in Transfer_G12 for more comprehension
Transfer_G12$garment<- gsub("G4A","G1",Transfer_G12$garment)
Transfer_G12$garment<- gsub("G4B","G2",Transfer_G12$garment)
Transfer_G12$garment<- gsub("G4C","G3",Transfer_G12$garment)

# to export 
Transfer_G1 <- Transfer_G1 %>% dplyr::select(wash, garment,`Before transfer`,`After transfer`,value,band)
Transfer_G2 <- Transfer_G2 %>% dplyr::select(wash, garment,`Before transfer`,`After transfer`,value,band)
Transfer_G3 <- Transfer_G3 %>% dplyr::select(wash, garment,`Before transfer`,`After transfer`,value,band)
Transfer_G12 <- Transfer_G12 %>% dplyr::select(wash, garment,`Before transfer`,`After transfer`,value,band)

# Select important column
Transfer_G1 <-  Transfer_G1 %>% dplyr::select(wash, garment,value,band)
Transfer_G2 <-  Transfer_G2 %>% dplyr::select(wash, garment,value,band)
Transfer_G3 <-  Transfer_G3 %>% dplyr::select(wash, garment,value,band)
Transfer_G12 <-  Transfer_G12 %>% dplyr::select(wash, garment,value,band)

# Create a list of dataframes
dataframes <- list(Transfer_G1,Transfer_G2,Transfer_G3, Transfer_G12)

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
  compute_and_write_stats(dataframes[[i]], paste("Results/Descriptive statistics - Garment ", c(1, 2, 3, 12)[i], ".csv", sep = ""))
}

#### Intermediate Data Visualisation ####
pAtr_G1 <- ggplot(Transfer_G1, aes(x=wash, y=value)) +
  geom_boxplot() +
  stat_summary(fun = mean, colour="darkred",
               geom="point",position=position_dodge(width=0.75)) +
  stat_summary(fun = mean, colour="darkred", aes(group=1),
               geom="line", lwd=1, lty=1) +
  ylim(-5,35)+
  labs(x="\nWash number", y="Number of Fibres\n") +
  theme_bw(base_family = "Arial", base_size = 14) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.background = element_rect(fill="grey95",size=1, linetype="solid", colour="grey80"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5))
show(pAtr_G1)

pAtr_G2 <- ggplot(Transfer_G2, aes(x=wash, y=value)) +
  geom_boxplot() +
  stat_summary(fun = mean, colour="darkred",
               geom="point",position=position_dodge(width=0.75)) +
  stat_summary(fun = mean, colour="darkred", aes(group=1),
               geom="line", lwd=1, lty=1) +
  ylim(-5,35)+
  labs(x="\nWash number", y="Number of Fibres\n") +
  theme_bw(base_family = "Arial", base_size = 14) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.background = element_rect(fill="grey95",size=1, linetype="solid", colour="grey80"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5))
show(pAtr_G2)

pAtr_G3 <- ggplot(Transfer_G3, aes(x=wash, y=value)) +
  geom_boxplot() +
  stat_summary(fun = mean, colour="darkred",
               geom="point",position=position_dodge(width=0.75)) +
  stat_summary(fun = mean, colour="darkred", aes(group=1),
               geom="line", lwd=1, lty=1) +
  ylim(-5,35)+
  labs(x="\nWash number", y="Number of Fibres\n") +
  theme_bw(base_family = "Arial", base_size = 14) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.background = element_rect(fill="grey95",size=1, linetype="solid", colour="grey80"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5))
show(pAtr_G3)

pAtr_G12 <- ggplot(Transfer_G12, aes(x=wash, y=value)) +
  geom_boxplot() +
  stat_summary(fun = mean, colour="darkred",
               geom="point",position=position_dodge(width=0.75)) +
  stat_summary(fun = mean, colour="darkred", aes(group=1),
               geom="line", lwd=1, lty=1) +
  ylim(-5,35)+
  labs(x="\nWash number", y="Number of Fibres\n") +
  theme_bw(base_family = "Arial", base_size = 14) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.background = element_rect(fill="grey95",size=1, linetype="solid", colour="grey80"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5))
show(pAtr_G12)

#### Final graph - Figure XXX #### 
pCombined_pending <- ggarrange(pAtr_G1+ rremove("ylab") + rremove("xlab"),
                               pAtr_G2+ rremove("ylab") + rremove("xlab"),
                               pAtr_G3+ rremove("ylab") + rremove("xlab"),
                               pAtr_G12+ rremove("ylab") + rremove("xlab"),
                                 labels = c("A", "B", "C", "D"),
                                 common.legend = TRUE, legend = "bottom",
                                 align = "hv",
                                 ncol = 1, nrow = 4,
                                 font.label = list(size = 12, color = "black", family = NULL, position = "top"))
pCombined <- annotate_figure(pCombined_pending, left = textGrob("Number of fibres", rot = 90, vjust = 0.5, hjust = 0.5, gp = gpar(cex =1)),
                               bottom = textGrob("Wash number", vjust = 0.5, hjust = 0.5,gp = gpar(cex = 1)))
pCombined

# to save the graph
ggsave("Figure XXX - Transfer all garments.png", pCombined, width =8, height = 12, units = "in", dpi=300,path = "Results")


# # Create dataframe for the final plot
# # data from the series involving washing a single donor garment
# n=15
# numS <- data.frame(setdiff(0:n, c(8,10,12,14)))
# Transfer_G1 <- Transfer_G1[!is.na(Transfer_G1$value), ]
# Transfer_G1$value <- as.numeric(Transfer_G1$value)
# meanAtrG1 <- aggregate(value ~ wash, Transfer_G1, function(x) round(mean(x), digits = 2))
# forplotTotG1 <- data.frame(cbind(numS, value =meanAtrG1$value))
# names(forplotTotG1) <- c("Transfer", "value")
# forplotTotG1$group <- c("G1")
# 
# # data from the series involving washing a single donor garment with detergent
# n=15
# numS <- data.frame(setdiff(0:n, c(11,12,14)))
# Transfer_G2 <- Transfer_G2[!is.na(Transfer_G2$value), ]
# Transfer_G2$value <- as.numeric(Transfer_G2$value)
# meanAtrG2 <- aggregate(value ~ wash, Transfer_G2, function(x) round(mean(x), digits = 2))
# forplotTotG2 <- data.frame(cbind(numS, value =meanAtrG2$value))
# names(forplotTotG2) <- c("Transfer", "value")
# forplotTotG2$group <- c("G2")
# 
# # data from the series involving washing a single donor garment with detergent and softener
# n=15
# numS <- data.frame(setdiff(0:n, c(10,11,12,14)))
# Transfer_G3 <- Transfer_G3[!is.na(Transfer_G3$value), ]
# Transfer_G3$value <- as.numeric(Transfer_G3$value)
# meanAtrG3 <- aggregate(value ~ wash, Transfer_G3, function(x) round(mean(x), digits = 2))
# forplotTotG3 <- data.frame(cbind(numS, value =meanAtrG3$value))
# names(forplotTotG3) <- c("Transfer", "value")
# forplotTotG3$group <- c("G3")
# 
# # data from the series involving washing 12 donor garments
# n=41
# numS <- data.frame(setdiff(0:n, c(16,17,24,26,28,30,32,34,36,38,40)))
# Transfer_G12 <- Transfer_G12[!is.na(Transfer_G12$value), ]
# Transfer_G12$value <- as.numeric(Transfer_G12$value)
# meanAtrG12 <- aggregate(value ~ wash, Transfer_G12, function(x) round(mean(x), digits = 2))
# forplotTotG12 <- data.frame(cbind(numS, value =meanAtrG12$value))
# names(forplotTotG12) <- c("Transfer", "value")
# forplotTotG12$group <- c("G12")
# 
# # Combined all data 
# Toplot <- rbind(forplotTotG1,forplotTotG2,forplotTotG3,forplotTotG12)
# 
# # # Convert specific columns to different format
# Toplot$Transfer <- as.numeric(Toplot$Transfer)
# Toplot$value <- as.numeric(Toplot$value)
# Toplot$group <- as.factor(Toplot$group)
# 
# # find the best visual fit
# # for the first series involving washing a single donor garment
# fitG1 <- lm(value~poly(Transfer,5,raw=TRUE), data=forplotTotG1)
# a <- summary(fitG1)$adj.r.squared;a
# 
# # for the second series involving washing 5 donor garments
# fitG2 <- lm(value~poly(Transfer,8,raw=TRUE), data=forplotTotG2)
# b <- summary(fitG2)$adj.r.squared;b
# 
# # for the second series involving washing 5 donor garments
# fitG3 <- lm(value~poly(Transfer,8,raw=TRUE), data=forplotTotG3)
# c <- summary(fitG3)$adj.r.squared;c
# 
# # for the third series involving washing 12 donor garments
# fitG12 <- lm(value~poly(Transfer,9,raw=TRUE), data=forplotTotG12)
# d <- summary(fitG12)$adj.r.squared;d
# 
# #### Final graph - Figure 9 ####
# # Set up a PNG device
# png("./Results/Figure XXX - Transfer.png", width = 2700, height = 2400, res = 300)
# 
# # Adjust the margins (bottom, left, top, right)
# par(mar = c(5, 4, 1, 2))  # Adjust the top margin to be smaller
# 
# # Your plotting code here...
# colors <- c("blue",
#             "darkred",
#             "#000000",
#             "darkgreen")
# 
# plot(Toplot$Transfer, Toplot$value, col=colors[Toplot$group],
#      pch=19,
#      xlab='\nWash number',
#      ylab='Average number of transferred fibres',
#      xaxt='n',
#      cex.axis = 1.2,
#      cex.lab = 1.2)
# axis(1, xaxp=c(0, 51, 51), las=1,cex.axis = 1.2) +theme_bw()
# # add legend
# legend(28, 15, legend=c("G1","G2", "G3", "G12"),
#        col=c("blue", "#000000","darkgreen","darkred"), lty=1:1, cex=1.2)
# 
# #fit polynomial regression models up to degree 5
# fitG1<- lm(value ~ poly(Transfer, 3, raw = TRUE), data = forplotTotG1)
# fitG2 <- lm(value ~ poly(Transfer, 4, raw = TRUE), data = forplotTotG2)
# fitG3 <- lm(value ~ poly(Transfer, 3, raw = TRUE), data = forplotTotG3)
# fitG12 <- lm(value ~ poly(Transfer, 7, raw = TRUE), data = forplotTotG12)
# 
# #define x-axis values
# x_range_G1 <- seq(1, 15, length = 51)  # Adjust the range as needed
# predictions_G1 <- predict(fitG1, data.frame(Transfer = x_range_G1))
# lines(x_range_G1, predictions_G1, col = 'blue')
# 
# x_range_G2 <- seq(1, 15, length = 51)  # Adjust the range as needed
# predictions_G2 <- predict(fitG2, data.frame(Transfer = x_range_G2))
# lines(x_range_G2, predictions_G2, col = '#000000')
# 
# x_range_G3 <- seq(1, 15, length = 51)  # Adjust the range as needed
# predictions_G3 <- predict(fitG3, data.frame(Transfer = x_range_G3))
# lines(x_range_G3, predictions_G3, col = 'darkgreen')
# 
# x_range <- seq(1, 41, length = 51)  # Adjust the range as needed
# predictions <- predict(fitG12, data.frame(Transfer = x_range))
# lines(x_range, predictions, col = 'darkred')
# 
# # End the plot device
# dev.off()

