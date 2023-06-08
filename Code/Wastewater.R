# This code is to upload the data related to the number of fibres released in the wastewater and the volume of water used each wash

# The .csv file to upload for the volume of wastewater will need to have the following columns:
# Washnumber; Wash; Rinse; Total;
# The data are available in Appendix 9 for the red donor
# The data are available in Appendix 18 for the yellow donor

# The .csv file to upload for the fibres released in the wastewater will need to have the following columns:
# Filter; Date; Experiment; Filtration; mass1; mass2; mass3; mass4; mass5;
# The data are available in Appendix 10 for the red donor
# The data are available in Appendix 19 for the yellow donor

#############################################################
#####                     to load                       #####
#############################################################

Wastewaterfibres_G1 <- read.csv('./Data_filtration_fibre weight_G1.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')
Wastewaterfibres_G1$Date<-as.factor(Wastewaterfibres_G1$Date)
Wastewatervolume_G1 <- read.csv('./Data_filtration_water volume_G1.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')

Wastewaterfibres_G2 <- read.csv('./Data_filtration_fibre weight_G2.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')
Wastewaterfibres_G2$Date<-as.factor(Wastewaterfibres_G2$Date)
Wastewatervolume_G2 <- read.csv('./Data_filtration_water volume_G2.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')

Wastewaterfibres_PhD <- read.csv('./Data_filtration_fibre weight_PhD.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')
Wastewatervolume_PhD <- read.csv('./Data_filtration_water volume_PhD.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')

#############################################################
#####                    GARMENT 1                      #####
#############################################################

###### Wastewater fibres ######
# mean for each rows (selected columns)
Wastewaterfibres_G1$meanValue <- rowMeans(subset(Wastewaterfibres_G1, select = c(mass1: mass5)), na.rm = TRUE)

# standard deviation for each row (selected columns)
Wastewaterfibres_G1$standardDeviation <- rowSds(as.matrix(Wastewaterfibres_G1[,c(5,6,7,8,9)]))

# number of replicate measurements
Wastewaterfibres_G1$Number <- rowSums( !is.na(Wastewaterfibres_G1[,5:9]))

# SEM per row
Wastewaterfibres_G1$SEM <- Wastewaterfibres_G1$standardDeviation/sqrt(Wastewaterfibres_G1$Number)

# create an identifier
Wastewaterfibres_G1$ID <- Wastewaterfibres_G1$Filter

# split out each set using the ID
p1 = Wastewaterfibres_G1 %>% filter(ID == "FA") %>% dplyr::select(Experiment,meanValue,SEM)
p2 = Wastewaterfibres_G1 %>% filter(ID == "FN") %>% dplyr::select(Experiment,meanValue,SEM)

# and merge
merge.dat = data.frame(p2[,c("Experiment")], p1$meanValue, p2$meanValue, p1$SEM, p2$SEM)
names(merge.dat) = c("Experiment","P1","P2","SEM1", "SEM2")

# add the filter ID
merge.dat <- cbind(Wastewaterfibres_G1$Filter,merge.dat)
merge.dat <- merge.dat[1:28,]
names(merge.dat)[names(merge.dat) == 'Wastewaterfibres_G1$Filter'] <- 'Filter'

# Calculate difference and U.F
Wastewaterfibres_G12 <- merge.dat %>% mutate(Diff =  P2 -P1, U.F = sqrt((SEM1*SEM1)+(SEM2*SEM2)))

# split out each set by reshaping
Wastewaterfibres_G1p3 <- Wastewaterfibres_G12 %>%
  dplyr::select(Filter, Experiment, Diff, U.F)
Wastewaterfibres_G1p4 <- reshape(Wastewaterfibres_G1p3, idvar = "Experiment", timevar = "Filter", direction = "wide")
Wastewaterfibres_G1p4 <- data.frame(Wastewaterfibres_G1p4)

# Calculate difference and U.F
Wastewaterfibres_G1p4$Wf <- round(Wastewaterfibres_G1p4$Diff.FN-Wastewaterfibres_G1p4$Diff.FA, digit=2)
Wastewaterfibres_G1p4$U.C <- sqrt((Wastewaterfibres_G1p4$U.F.FA*Wastewaterfibres_G1p4$U.F.FA)+ (Wastewaterfibres_G1p4$U.F.FN*Wastewaterfibres_G1p4$U.F.FN))
Wastewaterfibres_G1p4$U.C3 <- round(Wastewaterfibres_G1p4$U.C*3, digit=2)

max(Wastewaterfibres_G1p4$Diff.FN)
min(Wastewaterfibres_G1p4$Diff.FN)
mean(Wastewaterfibres_G1p4$Diff.FN)
SD(Wastewaterfibres_G1p4$Diff.FN)
#write.table(Wastewaterfibres_G1p4, file = "Data_filtration_fibre weight_analysed_red.csv", quote = F, sep = ",", row.names = F)

###### Wastewater volume ######
  # 1) Uncertainty related to the graduation of the barrel (calibration)
# To calibrate the barrel, a 500 ml burette was filled up to the 500 ml graduation and pour into the barrel.
# At the first calibration line on the barrel, the uncertainty on the position of the line corresponds to the uncertainty...
#... on the measured volume (standard uncertainty of reading): Uread = (1 graduation/√12)
# The burette used was graduated every 10 mL
Uread <- 10/sqrt(12)

#The second graduation was obtained by adding again 500 ml of water using the same burette, and so on until the Nth graduation
# the uncertainty is therefore calculated as: Ucalibration = √N*Uread
Wastewatervolume_G1$Ucalibration <- sqrt(Wastewatervolume_G1$Total)*Uread

  #2)	Uncertainty related to the reading of the volume in the barrel
# The barrel was not graduated with consistent precision and therefore if a reading was done between graduations N and (N+1)
# the standard reading uncertainty was calculated as: Ureadbarrel = (1 graduation)/√12
# The barrel was  graduated every 500 mL, therefore:
Ureadbarrel <- 500/sqrt(12)

  # 3) Considering calibration and reading uncertainty
#To consider these uncertainties (calibration and reading) on the estimation of the volume V, between V(N+1) and V(N):
# U(V) = √(Ucalibration)^2+ (Ureadbarrel)^2 )
Wastewatervolume_G1$U.V <- (sqrt((Wastewatervolume_G1$Ucalibration)^2 +(Ureadbarrel)^2))/1000 #the division by 1000 is to convert ml to L
  # 4) Confidence interval
# One single measure of the volume of water in the barrel was done each wash.
# the Guide to the expression of Uncertainty in Measurement (GUM) recommends to calculate the confidence interval from the standard reading uncertainty (U(V))...
#...taking into account the uniform distribution associated with this type of uncertainty (uncertainty of B type), as follow : I=[-2U(V)  ; + 2U(V)]
# In our particular situation, Wastewatervolume_G1$U.V2 is used for the interval
Wastewatervolume_G1$U.V2 <-Wastewatervolume_G1$U.V*2
#write.table(Wastewatervolume_G1, file = "Data_filtration_Volume_analysed_red.csv", quote = F, sep = ",", row.names = F)

#############################################################
#####                   Data anaysis                    #####
#############################################################
### GRAPH - Volume
pVolume_G1 <- ggplot(data = Wastewatervolume_G1, aes(x =Washnumber, y = Total)) +
  geom_line(colour = "steelblue2")+
  labs(x="Wash number", y="Volume of water (L)")+
  scale_y_continuous(breaks = seq(0, 35, by = 1), limits = c(18, 29),expand = c(0,0))+
  scale_x_continuous(breaks = seq(1, 8, by = 2), limits = c(1, 8),expand = c(0.01,0))+
  theme_bw(base_size = 12) +
  theme(legend.position = "bottom",
        legend.background = element_rect(fill="grey95",size=1, linetype="solid", colour="grey80"),
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)))+ # top, right,bottom,left
  geom_errorbar(aes(ymin=Total-U.V2, ymax=Total+U.V2), width=0.5)+
  geom_smooth(formula = y ~ x,method='lm', se=F,color="black", linetype="dashed", size=0.5)
show(pVolume_G1)
ggsave("Wastewater volume_G1.png", pVolume_G1, width = 7, height = 4, units = "in", dpi=600, path = "Results")

### GRAPH - Fibre
lm(Diff.FN~Experiment, data=Wastewaterfibres_G1p4)
pfibres_G1 <- ggplot(data = Wastewaterfibres_G1p4, aes(x =Experiment, y = Diff.FN)) +
  geom_line(colour = "Tomato")+
  labs(x="Wash number", y="Fibres (mg)")+
  scale_y_continuous(breaks = seq(0, 80, by = 10), limits = c(20, 80),expand = c(0,0))+
  scale_x_continuous(breaks = seq(1, 8, by = 2), limits = c(1, 8),expand = c(0.01,0))+
  theme_bw( base_size = 12) +
  theme(legend.position = "bottom",
        legend.background = element_rect(fill="grey95",size=1, linetype="solid", colour="grey80"),
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)))+ # top, right,bottom,left
  geom_errorbar(aes(ymin=Diff.FN-U.C3, ymax=Diff.FN+U.C3), width=0.5)+
  geom_smooth(formula = y ~ x,method='lm', se=F,color="black", linetype="dashed", size=0.5)+
  annotate(geom="text", x=7, y=25, label="y = 61.386 - 1.844 x", # values obtained with lm(Diff.FN~Experiment, data=Wastewaterfibres_G1p4) above
               color="black")
show(pfibres_G1)
ggsave("Wastewater fibres_G1.png", pfibres_G1, width = 7, height = 4, units = "in", dpi=600, path = "Results")

### GRAPH - Pearson correlation
# Null hypothesis – There is no significant correlation between the volume and the wash number
# The alternative hypothesis – There is a significant correlation between the volume and the wash number
# set alpha level to 0.05

#Visualize data using scatter plots Volume VS wash number
PearsonVW_G1 <- ggscatter(Wastewatervolume_G1, x = "Washnumber", y = "Total",
                       add = "reg.line",
                       xlab = "Wash number", ylab = "Volume of water (L)")+
  stat_cor(method = "pearson", label.x = 3, label.y = 25)# Customize reg. line
PearsonVW_G1                 
ggsave("Pearson volume VS wash number_G1.png", PearsonVW_G1, width = 7, height = 4, units = "in", dpi=600, path = "Results")

#Visualize data using scatter plots Fibres VS Volume
Wastewatervolume_fibre_G1 <- data.frame(cbind(Wastewatervolume_G1,Fibre=Wastewaterfibres_G1p4$Diff.FN))
PearsonVf_G1 <- ggscatter(Wastewatervolume_fibre_G1, x = "Fibre", y = "Total",
                       add = "reg.line",
                       xlab = "Fibre (mg)", ylab = "Volume (L)")+
  stat_cor(method = "pearson", label.x = 70, label.y = 25)
PearsonVf_G1   
ggsave("Pearson fibre VS wash number_G1.png", PearsonVf_G1, width = 7, height = 4, units = "in", dpi=600, path = "Results")

#############################################################
#####                    GARMENT 2                      #####
#############################################################
###### Wastewater fibres ######
# mean for each rows (selected columns)
Wastewaterfibres_G2$meanValue <- rowMeans(subset(Wastewaterfibres_G2, select = c(mass1: mass5)), na.rm = TRUE)

# standard deviation for each row (selected columns)
Wastewaterfibres_G2$standardDeviation <- rowSds(as.matrix(Wastewaterfibres_G2[,c(5,6,7,8,9)]))

# number of replicate measurements
Wastewaterfibres_G2$Number <- rowSums( !is.na(Wastewaterfibres_G2[,5:9]))

# SEM per row
Wastewaterfibres_G2$SEM <- Wastewaterfibres_G2$standardDeviation/sqrt(Wastewaterfibres_G2$Number)

# create an identifier
Wastewaterfibres_G2$ID <- Wastewaterfibres_G2$Filter

# split out each set using the ID
p1 = Wastewaterfibres_G2 %>% filter(ID == "FA") %>% dplyr::select(Experiment,meanValue,SEM)
p2 = Wastewaterfibres_G2 %>% filter(ID == "FN") %>% dplyr::select(Experiment,meanValue,SEM)

# and merge
merge.dat = data.frame(p2[,c("Experiment")], p1$meanValue, p2$meanValue, p1$SEM, p2$SEM)
names(merge.dat) = c("Experiment","P1","P2","SEM1", "SEM2")

# add the filter ID
merge.dat <- cbind(Wastewaterfibres_G2$Filter,merge.dat)
merge.dat <- merge.dat[1:14,]
names(merge.dat)[names(merge.dat) == 'Wastewaterfibres_G2$Filter'] <- 'Filter'

# Calculate difference and U.F
Wastewaterfibres_G22 <- merge.dat %>% mutate(Diff =  P2 -P1, U.F = sqrt((SEM1*SEM1)+(SEM2*SEM2)))

# split out each set by reshaping
Wastewaterfibres_G2p3 <- Wastewaterfibres_G22 %>%
  dplyr::select(Filter, Experiment, Diff, U.F)
Wastewaterfibres_G2p4 <- reshape(Wastewaterfibres_G2p3, idvar = "Experiment", timevar = "Filter", direction = "wide")
Wastewaterfibres_G2p4 <- data.frame(Wastewaterfibres_G2p4)

# Calculate difference and U.F
Wastewaterfibres_G2p4$Wf <- round(Wastewaterfibres_G2p4$Diff.FN-Wastewaterfibres_G2p4$Diff.FA, digit=2)
Wastewaterfibres_G2p4$U.C <- sqrt((Wastewaterfibres_G2p4$U.F.FA*Wastewaterfibres_G2p4$U.F.FA)+ (Wastewaterfibres_G2p4$U.F.FN*Wastewaterfibres_G2p4$U.F.FN))
Wastewaterfibres_G2p4$U.C3 <- round(Wastewaterfibres_G2p4$U.C*3, digit=2)

max(Wastewaterfibres_G2p4$Diff.FN)
min(Wastewaterfibres_G2p4$Diff.FN)
mean(Wastewaterfibres_G2p4$Diff.FN)
SD(Wastewaterfibres_G2p4$Diff.FN)
#write.table(Wastewaterfibres_G2p4, file = "Data_filtration_fibre weight_analysed_red.csv", quote = F, sep = ",", row.names = F)

###### Wastewater volume ######
# 1) Uncertainty related to the graduation of the barrel (calibration)
# To calibrate the barrel, a 500 ml burette was filled up to the 500 ml graduation and pour into the barrel.
# At the first calibration line on the barrel, the uncertainty on the position of the line corresponds to the uncertainty...
#... on the measured volume (standard uncertainty of reading): Uread = (1 graduation/√12)
# The burette used was graduated every 10 mL
Uread <- 10/sqrt(12)

#The second graduation was obtained by adding again 500 ml of water using the same burette, and so on until the Nth graduation
# the uncertainty is therefore calculated as: Ucalibration = √N*Uread
Wastewatervolume_G2$Ucalibration <- sqrt(Wastewatervolume_G2$Total)*Uread

#2)	Uncertainty related to the reading of the volume in the barrel
# The barrel was not graduated with consistent precision and therefore if a reading was done between graduations N and (N+1)
# the standard reading uncertainty was calculated as: Ureadbarrel = (1 graduation)/√12
# The barrel was  graduated every 500 mL, therefore:
Ureadbarrel <- 500/sqrt(12)

# 3) Considering calibration and reading uncertainty
#To consider these uncertainties (calibration and reading) on the estimation of the volume V, between V(N+1) and V(N):
# U(V) = √(Ucalibration)^2+ (Ureadbarrel)^2 )
Wastewatervolume_G2$U.V <- (sqrt((Wastewatervolume_G2$Ucalibration)^2 +(Ureadbarrel)^2))/1000 #the division by 1000 is to convert ml to L
# 4) Confidence interval
# One single measure of the volume of water in the barrel was done each wash.
# the Guide to the expression of Uncertainty in Measurement (GUM) recommends to calculate the confidence interval from the standard reading uncertainty (U(V))...
#...taking into account the uniform distribution associated with this type of uncertainty (uncertainty of B type), as follow : I=[-2U(V)  ; + 2U(V)]
# In our particular situation, Wastewatervolume_G2$U.V2 is used for the interval
Wastewatervolume_G2$U.V2 <-Wastewatervolume_G2$U.V*2
#write.table(Wastewatervolume_G2, file = "Data_filtration_Volume_analysed_red.csv", quote = F, sep = ",", row.names = F)

#############################################################
#####                   Data anaysis                    #####
#############################################################
### GRAPH - Volume
pVolume_G2 <- ggplot(data = Wastewatervolume_G2, aes(x =Washnumber, y = Total)) +
  geom_line(colour = "steelblue2")+
  labs(x="Wash number", y="Volume of water (L)")+
  scale_y_continuous(breaks = seq(0, 35, by = 1), limits = c(18, 29),expand = c(0,0))+
  scale_x_continuous(breaks = seq(1, 8, by = 2), limits = c(1, 8),expand = c(0.01,0))+
  theme_bw(base_size = 12) +
  theme(legend.position = "bottom",
        legend.background = element_rect(fill="grey95",size=1, linetype="solid", colour="grey80"),
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)))+ # top, right,bottom,left
  geom_errorbar(aes(ymin=Total-U.V2, ymax=Total+U.V2), width=0.5)+
  geom_smooth(formula = y ~ x,method='lm', se=F,color="black", linetype="dashed", size=0.5)
show(pVolume_G2)
ggsave("Wastewater volume_G2.png", pVolume_G2, width = 7, height = 4, units = "in", dpi=600, path = "Results")

### GRAPH - Fibre
lm(Diff.FN~Experiment, data=Wastewaterfibres_G2p4)
pfibres_G2 <- ggplot(data = Wastewaterfibres_G2p4, aes(x =Experiment, y = Diff.FN)) +
  geom_line(colour = "Tomato")+
  labs(x="Wash number", y="Fibres (mg)")+
  scale_y_continuous(breaks = seq(0, 100, by = 10), limits = c(20, 100),expand = c(0,0))+
  scale_x_continuous(breaks = seq(1, 8, by = 2), limits = c(1, 8),expand = c(0.01,0))+
  theme_bw( base_size = 12) +
  theme(legend.position = "bottom",
        legend.background = element_rect(fill="grey95",size=1, linetype="solid", colour="grey80"),
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)))+ # top, right,bottom,left
  geom_errorbar(aes(ymin=Diff.FN-U.C3, ymax=Diff.FN+U.C3), width=0.5)+
  geom_smooth(formula = y ~ x,method='lm', se=F,color="black", linetype="dashed", size=0.5)+
  annotate(geom="text", x=7, y=25, label="y = 73.677 - 3.516 x", # values obtained with lm(Diff.FN~Experiment, data=Wastewaterfibres_G2p4) above
           color="black")
show(pfibres_G2)
ggsave("Wastewater fibres_G2.png", pfibres_G2, width = 7, height = 4, units = "in", dpi=600, path = "Results")

### GRAPH - Pearson correlation
# Null hypothesis – There is no significant correlation between the volume and the wash number
# The alternative hypothesis – There is a significant correlation between the volume and the wash number
# set alpha level to 0.05

#Visualize data using scatter plots Volume VS wash number
PearsonVW_G2 <- ggscatter(Wastewatervolume_G2, x = "Washnumber", y = "Total",
                          add = "reg.line",
                          xlab = "Wash number", ylab = "Volume of water (L)")+
  stat_cor(method = "pearson", label.x = 3, label.y = 25)# Customize reg. line
PearsonVW_G2                 
ggsave("Pearson volume VS wash number_G2.png", PearsonVW_G2, width = 7, height = 4, units = "in", dpi=600, path = "Results")

#Visualize data using scatter plots Fibres VS Volume
Wastewatervolume_fibre_G2 <- data.frame(cbind(Wastewatervolume_G2,Fibre=Wastewaterfibres_G2p4$Diff.FN))
PearsonVf_G2 <- ggscatter(Wastewatervolume_fibre_G2, x = "Fibre", y = "Total",
                          add = "reg.line",
                          xlab = "Fibre (mg)", ylab = "Volume (L)")+
  stat_cor(method = "pearson", label.x = 70, label.y = 25)
PearsonVf_G2   
ggsave("Pearson fibre VS wash number_G2.png", PearsonVf_G2, width = 7, height = 4, units = "in", dpi=600, path = "Results")

### Combined graph ###
# First exclude wash 34 from the dataframe due to error during experiments
Wastewatervolume_PhD <- subset(Wastewatervolume_PhD, Washnumber!="34")
Wastewatervolume_PhD$Coder <- "5 garments, no detergent"
Wastewatervolume_G1$Coder <- "1 garment, no detergent"
Wastewatervolume_G2$Coder <- "1 garment, detergent"
TotalWastewatervolume <-rbind(Wastewatervolume_PhD,Wastewatervolume_G1,Wastewatervolume_G2)

pVolume_Total <- ggplot(data = TotalWastewatervolume, aes(x =Washnumber, y = Total, color=Coder, group=Coder)) +
  geom_line(aes(linetype=Coder), size=0.7)+
  labs(x="\nWash number", y="Volume of water (L)\n")+
  scale_colour_manual(values=c("tomato","black","black"))+
  scale_linetype_manual(values=c("solid","solid","dashed"))+
  scale_y_continuous(breaks = seq(0, 35, by = 1), limits = c(18, 29),expand = c(0,0))+
  scale_x_continuous(breaks = seq(1, 51, by = 2), limits = c(1, 51),expand = c(0.01,0))+
  theme_bw(base_family = "Arial", base_size = 16) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.background = element_rect(fill="grey95",size=1, linetype="solid", colour="grey80"),
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5))+
  geom_errorbar(aes(ymin=Total-U.V, ymax=Total+U.V), width=0.5, colour="black")
show(pVolume_Total)
ggsave("Wastewater volume-Total.png", pVolume_Total, width = 8, height = 5, units = "in", dpi=600, path = "Results")

Wastewaterfibres_PhD$Coder <- "5 garments, no detergent"
Wastewaterfibres_G1p4$Coder <- "1 garment, no detergent"
Wastewaterfibres_G2p4$Coder <- "1 garment, detergent"
TotalWastewaterfibres <-rbind(Wastewaterfibres_PhD,Wastewaterfibres_G1p4,Wastewaterfibres_G2p4)

pfibre_Total <- ggplot(data = TotalWastewaterfibres, aes(x =Experiment, y = Diff.FN, color=Coder, group=Coder)) +
  geom_line(aes(linetype=Coder), size=0.7)+
  labs(x="\nWash number", y="Fibres (mg)\n")+
  scale_colour_manual(values=c("tomato","black","black"))+
  scale_linetype_manual(values=c("solid","solid","dashed"))+
  scale_y_continuous(breaks = seq(0, 600, by = 50), limits = c(0, 200),expand = c(0,0))+
  scale_x_continuous(breaks = seq(1, 51, by = 2), limits = c(1, 51),expand = c(0.01,0))+
  theme_bw(base_family = "Arial", base_size = 16) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.background = element_rect(fill="grey95",size=1, linetype="solid", colour="grey80"),
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5))+
  geom_errorbar(aes(ymin=Diff.FN-U.C3, ymax=Diff.FN+U.C3), width=0.5, colour="black")
show(pfibre_Total)
ggsave("Wastewater fibres-Total.png", pfibre_Total, width = 8, height = 5, units = "in", dpi=600, path = "Results")

### STATS FOR ARTICLE ###
### Volume of water
meanVolume_Total <- aggregate(Total ~  Coder, TotalWastewatervolume, function(x) {round(mean(x), digits=2)})
SDVolume_Total <- aggregate(Total ~  Coder, TotalWastewatervolume, function(x) {round(SD(x), digits=2)})
medianVolume_Total <- aggregate(Total ~  Coder, TotalWastewatervolume, median)
Table_Volume_Total <- cbind(meanVolume_Total, SDVolume_Total$Total, medianVolume_Total$Total)

### Fibre released in the wastewater
meanFibre_Total <- aggregate(Diff.FN ~  Coder, TotalWastewaterfibres, function(x) {round(mean(x), digits=2)})
SDFibre_Total <- aggregate(Diff.FN ~  Coder, TotalWastewaterfibres, function(x) {round(SD(x), digits=2)})
medianFibre_Total <- aggregate(Diff.FN ~  Coder, TotalWastewaterfibres, median)
Table_Fibre_Total <- cbind(meanFibre_Total, SDFibre_Total$Diff.FN, medianFibre_Total$Diff.FN)
