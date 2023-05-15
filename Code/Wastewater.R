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

Wastewaterfibres <- read.csv('./Data_filtration_fibre weight.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')
Wastewaterfibres$Date<-as.factor(Wastewaterfibres$Date)
Wastewatervolume <- read.csv('./Data_filtration_water volume.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')

#############################################################
#####                 Data processing                   #####
#############################################################

###### Wastewater fibres ######
# mean for each rows (selected columns)
Wastewaterfibres$meanValue <- rowMeans(subset(Wastewaterfibres, select = c(mass1: mass5)), na.rm = TRUE)

# standard deviation for each row (selected columns)
Wastewaterfibres$standardDeviation <- rowSds(as.matrix(Wastewaterfibres[,c(5,6,7,8,9)]))

# number of replicate measurements
Wastewaterfibres$Number <- rowSums( !is.na(Wastewaterfibres[,5:9]))

# SEM per row
Wastewaterfibres$SEM <- Wastewaterfibres$standardDeviation/sqrt(Wastewaterfibres$Number)

# create an identifier
Wastewaterfibres$ID <- Wastewaterfibres$Filter

# split out each set using the ID
p1 = Wastewaterfibres %>% filter(ID == "FA") %>% dplyr::select(Experiment,meanValue,SEM)
p2 = Wastewaterfibres %>% filter(ID == "FN") %>% dplyr::select(Experiment,meanValue,SEM)

# and merge
merge.dat = data.frame(p2[,c("Experiment")], p1$meanValue, p2$meanValue, p1$SEM, p2$SEM)
names(merge.dat) = c("Experiment","P1","P2","SEM1", "SEM2")

# add the filter ID
merge.dat <- cbind(Wastewaterfibres$Filter,merge.dat)
merge.dat <- merge.dat[1:100,]
names(merge.dat)[names(merge.dat) == 'Wastewaterfibres$Filter'] <- 'Filter'

# Calculate difference and U.F
Wastewaterfibres2 <- merge.dat %>% mutate(Diff =  P2 -P1, U.F = sqrt((SEM1*SEM1)+(SEM2*SEM2)))

# split out each set by reshaping
Wastewaterfibresp3 <- Wastewaterfibres2 %>%
  dplyr::select(Filter, Experiment, Diff, U.F)
Wastewaterfibresp4 <- reshape(Wastewaterfibresp3, idvar = "Experiment", timevar = "Filter", direction = "wide")
Wastewaterfibresp4 <- data.frame(Wastewaterfibresp4)

# Calculate difference and U.F
Wastewaterfibresp4$Wf <- round(Wastewaterfibresp4$Diff.FN-Wastewaterfibresp4$Diff.FA, digit=2)
Wastewaterfibresp4$U.C <- sqrt((Wastewaterfibresp4$U.F.FA*Wastewaterfibresp4$U.F.FA)+ (Wastewaterfibresp4$U.F.FN*Wastewaterfibresp4$U.F.FN))
Wastewaterfibresp4$U.C3 <- round(Wastewaterfibresp4$U.C*3, digit=2)

max(Wastewaterfibresp4$Diff.FN)
min(Wastewaterfibresp4$Diff.FN)
mean(Wastewaterfibresp4$Diff.FN)
SD(Wastewaterfibresp4$Diff.FN)
write.table(Wastewaterfibresp4, file = "Data_filtration_fibre weight_analysed_red.csv", quote = F, sep = ",", row.names = F)

###### Wastewater volume ######
  # 1) Uncertainty related to the graduation of the barrel (calibration)
# To calibrate the barrel, a 500 ml burette was filled up to the 500 ml graduation and pour into the barrel.
# At the first calibration line on the barrel, the uncertainty on the position of the line corresponds to the uncertainty...
#... on the measured volume (standard uncertainty of reading): Uread = (1 graduation/√12)
# The burette used was graduated every 10 mL
Uread <- 10/sqrt(12)

#The second graduation was obtained by adding again 500 ml of water using the same burette, and so on until the Nth graduation
# the uncertainty is therefore calculated as: Ucalibration = √N*Uread
Wastewatervolume$Ucalibration <- sqrt(Wastewatervolume$Total)*Uread

  #2)	Uncertainty related to the reading of the volume in the barrel
# The barrel was not graduated with consistent precision and therefore if a reading was done between graduations N and (N+1)
# the standard reading uncertainty was calculated as: Ureadbarrel = (1 graduation)/√12
# The barrel was  graduated every 500 mL, therefore:
Ureadbarrel <- 500/sqrt(12)

  # 3) Considering calibration and reading uncertainty
#To consider these uncertainties (calibration and reading) on the estimation of the volume V, between V(N+1) and V(N):
# U(V) = √(Ucalibration)^2+ (Ureadbarrel)^2 )
Wastewatervolume$U.V <- (sqrt((Wastewatervolume$Ucalibration)^2 +(Ureadbarrel)^2))/1000 #the division by 1000 is to convert ml to L
  # 4) Confidence interval
# One single measure of the volume of water in the barrel was done each wash.
# the Guide to the expression of Uncertainty in Measurement (GUM) recommends to calculate the confidence interval from the standard reading uncertainty (U(V))...
#...taking into account the uniform distribution associated with this type of uncertainty (uncertainty of B type), as follow : I=[-2U(V)  ; + 2U(V)]
# In our particular situation, Wastewatervolume$U.V2 is used for the interval
Wastewatervolume$U.V2 <-Wastewatervolume$U.V*2
write.table(Wastewatervolume, file = "Data_filtration_Volume_analysed_red.csv", quote = F, sep = ",", row.names = F)

#############################################################
#####                   Data anaysis                    #####
#############################################################

# Bar plots of data by group
ggplot(Wastewaterfibresp4, aes(x=Diff.FN)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white", bins=50)+
  geom_density(alpha=.2, fill="#FF6666") 
ggqqplot(Wastewaterfibresp4$Diff.FN)
shapiro.test(Wastewaterfibresp4$Diff.FN)

ggplot(Wastewatervolume, aes(x=Total)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white", bins=50)+
  geom_density(alpha=.2, fill="#FF6666") 
ggqqplot(Wastewatervolume$Total)
shapiro.test(Wastewatervolume$Total)

### GRAPH - Volume
# First exclude wash 34 from the dataframe due to error during experiments
Wastewatervolume <- subset(Wastewatervolume, Washnumber!="34")

pVolume <- ggplot(data = Wastewatervolume, aes(x =Washnumber, y = Total)) +
  geom_line(colour = "steelblue2")+
  labs(x="Wash number", y="Volume of water (L)")+
  scale_y_continuous(breaks = seq(0, 35, by = 1), limits = c(18, 29),expand = c(0,0))+
  scale_x_continuous(breaks = seq(1, 51, by = 2), limits = c(1, 51),expand = c(0.01,0))+
  theme_bw(base_size = 12) +
  theme(legend.position = "bottom",
        legend.background = element_rect(fill="grey95",size=1, linetype="solid", colour="grey80"),
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)))+ # top, right,bottom,left
  geom_errorbar(aes(ymin=Total-U.V2, ymax=Total+U.V2), width=0.5)+
  geom_smooth(formula = y ~ x,method='lm', se=F,color="black", linetype="dashed", size=0.5)+
  stat_regline_equation(label.y = 19.25,label.x = 39.25) #this means at 9th unit regresion line equation will be shown
show(pVolume)
ggsave("Wastewater volume.png", pVolume, width = 7, height = 4, units = "in", dpi=600, path = "Results")

### GRAPH - Fibre
# First exclude wash 34 from the dataframe due to error during experiments
lm(Diff.FN~Experiment, data=Wastewaterfibresp4)
pfibres <- ggplot(data = Wastewaterfibresp4, aes(x =Experiment, y = Diff.FN)) +
  geom_line(colour = "Tomato")+
  labs(x="Wash number", y="Fibres (mg)")+
  scale_y_continuous(breaks = seq(0, 160, by = 10), limits = c(50, 160),expand = c(0,0))+
  scale_x_continuous(breaks = seq(1, 51, by = 2), limits = c(1, 51),expand = c(0.01,0))+
  theme_bw( base_size = 12) +
  theme(legend.position = "bottom",
        legend.background = element_rect(fill="grey95",size=1, linetype="solid", colour="grey80"),
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)))+ # top, right,bottom,left
  geom_errorbar(aes(ymin=Diff.FN-U.C3, ymax=Diff.FN+U.C3), width=0.5)+
  geom_smooth(formula = y ~ x,method='lm', se=F,color="black", linetype="dashed", size=0.5)+
  annotate(geom="text", x=45, y=60, label="y = 114 - 0.15 x", # values obtained with lm(Diff.FN~Experiment, data=Wastewaterfibresp4) above
               color="black")
show(pfibres)
ggsave("Wastewater fibres.png", pfibres, width = 7, height = 4, units = "in", dpi=600, path = "Results")

### GRAPH - Pearson correlation
# Null hypothesis – There is no significant correlation between the volume and the wash number
# The alternative hypothesis – There is a significant correlation between the volume and the wash number
# set alpha level to 0.05

#Visualize data using scatter plots Volume VS wash number
PearsonVW <- ggscatter(Wastewatervolume, x = "Washnumber", y = "Total",
                       add = "reg.line",
                       xlab = "Wash number", ylab = "Volume of water (L)")+
  stat_cor(method = "pearson", label.x = 3, label.y = 30)# Customize reg. line
PearsonVW                 
ggsave("Pearson volume VS wash number - red.png", PearsonVW, width = 7, height = 4, units = "in", dpi=600, path = "Results")

# # First exclude wash 34 from both dataframe due to   experiments
# Wastewaterfibresp4 <- subset(Wastewaterfibresp4, Experiment!="34")
# Wastewatervolume <- subset(Wastewatervolume, Washnumber!="34")
# Wastewatervolumetest <- data.frame(cbind(Wastewatervolume,Fibre=Wastewaterfibresp4$Diff.FN))

#Visualize data using scatter plots Fibres VS Volume
PearsonVf <- ggscatter(Wastewatervolumetest, x = "Fibre", y = "Total",
                       add = "reg.line",
                       xlab = "Fibre (mg)", ylab = "Volume (L)")+
  stat_cor(method = "pearson", label.x = 65, label.y = 30)
PearsonVf   
ggsave("Pearson fibre VS wash number - red.png", PearsonVf, width = 7, height = 4, units = "in", dpi=600, path = "Results")