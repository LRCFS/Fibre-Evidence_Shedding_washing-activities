##########################################################################
#####                  Analysis - Shedding Part 1                    #####
##########################################################################
# This R script is to generate the figures related to the primarily test

# 1: Hand pressure test
# 2: Impact test
# 3: Repeated Contact
# 4: save statistic tables

# ------------------------------------------------------------------------
# Section 1: Hand pressure test
# ------------------------------------------------------------------------
# Convert Area from inch2 to mm2
# 1 pixel = 1 x10^-5 inch2, so (Area*1)/0.000011 to convert into px
Handpressure$Area.px <- (Handpressure$Area*1)/0.000011
# scale of the image: 1 mm = 110 pixels, 1mm2 = 12100 px
Handpressure$Area.mm2 <- Handpressure$Area.px/12100

write.table(Handpressure, file = "Results/For Pat/Handpressure.csv", quote = F, sep = ",", row.names = F)

## Create different dataframes for each participant
Handpressure_O1 <- Handpressure %>% filter(Operator == "Operator 1")
Handpressure_O2 <- Handpressure %>% filter(Operator == "Operator 2")
Handpressure_O3 <- Handpressure %>% filter(Operator == "Operator 3")
Handpressure_O4 <- Handpressure %>% filter(Operator == "Operator 4")
Handpressure_O5 <- Handpressure %>% filter(Operator == "Operator 5")

# Descriptive statistics per operator
meanHandpressure_O1 <- ddply(Handpressure_O1, .(Repeat, Garment), summarise, Mean = round(mean(Area.mm2),digits = 2));meanHandpressure_O1
SDmeanHandpressure <- ddply(Handpressure_O1, .(Repeat, Garment), summarise, sd = round(sd(Area.mm2),digits = 2));SDmeanHandpressure
stats <- Handpressure_O1 %>%
  group_by(Repeat, Garment) %>%
  summarise(Mean = mean(Area.mm2), SD = sd(Area.mm2));stats

meanHandpressure_O2 <- ddply(Handpressure_O2, .(Repeat, Garment), summarise, Mean = round(mean(Area.mm2),digits = 2));meanHandpressure_O2
SDmeanHandpressure <- ddply(Handpressure_O2, .(Repeat, Garment), summarise, sd = round(sd(Area.mm2),digits = 2));SDmeanHandpressure
stats <- Handpressure_O2 %>%
  group_by(Repeat, Garment) %>%
  summarise(Mean = mean(Area.mm2), SD = sd(Area.mm2));stats

meanHandpressure_O3 <- ddply(Handpressure_O3, .(Repeat, Garment), summarise, Mean = round(mean(Area.mm2),digits = 2));meanHandpressure_O3
SDmeanHandpressure <- ddply(Handpressure_O3, .(Repeat, Garment), summarise, sd = round(sd(Area.mm2),digits = 2));SDmeanHandpressure
stats <- Handpressure_O3 %>%
  group_by(Repeat, Garment) %>%
  summarise(Mean = mean(Area.mm2), SD = sd(Area.mm2));stats

meanHandpressure_O4 <- ddply(Handpressure_O4, .(Repeat, Garment), summarise, Mean = round(mean(Area.mm2),digits = 2));meanHandpressure_O4
SDmeanHandpressure <- ddply(Handpressure_O4, .(Repeat, Garment), summarise, sd = round(sd(Area.mm2),digits = 2));SDmeanHandpressure
stats <- Handpressure_O4 %>%
  group_by(Repeat, Garment) %>%
  summarise(Mean = mean(Area.mm2), SD = sd(Area.mm2));stats

Handpressure_O5_gentle <- Handpressure_O5 %>% filter(Pressure == "gentle")
Handpressure_O5_firm <- Handpressure_O5 %>% filter(Pressure == "firm")

meanHandpressure_O5_gentle <- ddply(Handpressure_O5_gentle, .(Repeat, Garment), summarise, Mean = round(mean(Area.mm2),digits = 2));meanHandpressure_O5_gentle
SDmeanHandpressure_gentle <- ddply(Handpressure_O5_gentle, .(Repeat, Garment), summarise, sd = round(sd(Area.mm2),digits = 2));SDmeanHandpressure_gentle
stats <- Handpressure_O5_gentle %>%
  group_by(Repeat, Garment) %>%
  summarise(Mean = mean(Area.mm2), SD = sd(Area.mm2));stats

meanHandpressure_O5_firm<- ddply(Handpressure_O5_firm, .(Repeat, Garment), summarise, Mean = round(mean(Area.mm2),digits = 2));meanHandpressure_O5_firm
SDmeanHandpressure_firm <- ddply(Handpressure_O5_firm, .(Repeat, Garment), summarise, sd = round(sd(Area.mm2),digits = 2));SDmeanHandpressure_firm
stats <- Handpressure_O5_firm %>%
  group_by(Repeat, Garment) %>%
  summarise(Mean = mean(Area.mm2), SD = sd(Area.mm2));stats

#### Intermediate Data Visualisation ####
Handpressure_gentle <- rbind(Handpressure_O1,Handpressure_O4,Handpressure_O5_gentle)
Handpressure_gentle_table <- Handpressure_gentle %>%
  select("Operator", "Pressure", "Garment","Repeat","Sample","Area.mm2")
# write.table(Handpressure_gentle_table, file = "Results/Handpressure_gentle.csv", quote = F, sep = ",", row.names = F)

# Calculate the overall mean and SD for each operator
overall_stats <- Handpressure_gentle %>%
  filter(Pressure == "gentle") %>%
  group_by(Operator) %>%
  summarise(Overall_Mean = mean(Area.mm2),
            SD = sd(Area.mm2))# Create the plot

pgentle <- ggplot(subset(Handpressure_gentle, Pressure == "gentle"), aes(x = factor(Repeat), y = Area.mm2, fill = Garment)) +
  geom_boxplot(position = position_dodge(width = 0.75), color = "black") +
  #geom_point(stat = "summary", fun = mean, shape = 20, size = 3, color = "darkred", position = position_dodge(width = 0.75)) +
  labs(x = "Repeat", y = expression("Shed fibre area (mm"^2*")")) +
  coord_cartesian(ylim = c(0, 140)) +
  theme_bw(base_family = "Arial", base_size = 12) +
  theme(legend.position = "none") +
  facet_wrap(~Operator) + # Creates a panel for each Operator
  scale_fill_manual(values = c("G1" = "white", "G2" = "grey"))
# Add overall mean and SD text to each facet, adjusted for both Operator and Garment
pgentle <- pgentle + geom_text(data = overall_stats, aes(x = 0, y = Inf, label = sprintf("%.2f ± %.2f", Overall_Mean, SD)),
                               position = position_dodge(width = 0.75),
                               hjust =0, vjust = 2, size = 3.5, color = "navy", inherit.aes = FALSE)
pgentle

Handpressure_firm <- rbind(Handpressure_O2,Handpressure_O3,Handpressure_O5_firm)
Handpressure_firm_table <- Handpressure_firm %>%
  select("Operator", "Pressure", "Garment","Repeat","Sample","Area.mm2")
# write.table(Handpressure_firm_table, file = "Results/Handpressure_firm.csv", quote = F, sep = ",", row.names = F)

# Calculate the overall mean and SD for each operator
overall_stats2 <- Handpressure_firm %>%
  filter(Pressure == "firm") %>%
  group_by(Operator) %>%
  summarise(Overall_Mean = mean(Area.mm2),
            SD = sd(Area.mm2))# Create the plot

# Firm pressure graph
pfirm <- ggplot(subset(Handpressure_firm, Pressure == "firm"), aes(x = factor(Repeat), y = Area.mm2,fill = Garment)) +
  geom_boxplot(position = position_dodge(width = 0.75), color = "black") +
  #geom_point(stat = "summary", fun = mean, shape = 20, size = 3, color = "darkred", position = position_dodge(width = 0.75)) +
  labs(x = "Repeat", y = expression("Shed fibre area (mm"^2*")")) +
  coord_cartesian(ylim = c(0, 140)) +
  theme_bw(base_family = "Arial", base_size = 12) +
  theme(legend.position = "none") +
  facet_wrap(~Operator) + # Creates a panel for each Operator
  scale_fill_manual(values = c("G1" = "white", "G2" = "grey"))

# Add overall mean and SD text to each facet
pfirm <- pfirm + geom_text(data = overall_stats2, aes(x = 0, y = Inf, label = sprintf("%.2f \u00B1 %.2f", Overall_Mean, SD)),
                           hjust = -0.3, vjust = 2, size = 3.5, color = "navy", inherit.aes = FALSE)
pfirm

#### Final graph - Figure 6 #### 
# Modify each pSH_GX plot to adjust legend key size and labels
pgentle <- pgentle + theme(plot.title = element_text(hjust = 0.5))
pfirm <- pfirm + theme(plot.title = element_text(hjust = 0.5))

pCombinedHP_pending <- ggarrange(pgentle+ rremove("ylab") + rremove("xlab"),
                                 pfirm+ rremove("ylab") + rremove("xlab"),
                                 labels = c("A  ",
                                            "B  "),
                                 common.legend = T,legend = "right",
                                 align = "hv",
                                 ncol = 1, nrow = 2,
                                 font.label = list(size = 12, color = "black", family = NULL, position = "top"),
                                 hjust=0.05,vjust=2)+
  theme(plot.margin = margin(0,1,0,0, "cm")) # in order (Top,right,bottom,left)

pCombinedHP <- annotate_figure(pCombinedHP_pending, left = textGrob("Shed fibre area (mm\u00b2)\n", rot = 90, vjust = 0.5, hjust = 0.5, gp = gpar(cex =1)),
                               bottom = textGrob("Repeat", vjust = 0.5, hjust = 0.5,gp = gpar(cex = 1)))
pCombinedHP

# to save the graph
ggsave("Figure 6 - Shedding with operators.png", pCombinedHP, width =8, height = 7, units = "in", dpi=600,path = "Results")

#### STATS ####
### Difference between garments
# Gentle pressure
data_gentle <- Handpressure_gentle_table
Operator2_data_gentle <- filter(data_gentle,Operator == "Operator 1")
Operator2_data_gentle <- Operator2_data_gentle %>%
  select("Garment","Area.mm2")
Operator4_data_gentle <- filter(data_gentle,Operator == "Operator 4")
Operator4_data_gentle <- Operator4_data_gentle %>%
  select("Garment","Area.mm2")
Operator5_data_gentle <- filter(data_gentle,Operator == "Operator 5")
Operator5_data_gentle <- Operator5_data_gentle %>%
  select("Garment","Area.mm2")

Operator2_data_gentle %>%
  group_by(Garment) %>%
  shapiro_test(Area.mm2)

Operator4_data_gentle %>%
  group_by(Garment) %>%
  shapiro_test(Area.mm2)

Operator5_data_gentle %>%
  group_by(Garment) %>%
  shapiro_test(Area.mm2)

### sphericity test
#Levene’s Test
#H0: All sample variances are equal
#H1: At least one group has a variance that is not equal to the rest.
Operator2_data_gentle$Garment <- as.factor(Operator2_data_gentle$Garment)
leveneTest(Area.mm2 ~ Garment, Operator2_data_gentle) # p-value = 0.8402, equal variances
Operator4_data_gentle$Garment <- as.factor(Operator4_data_gentle$Garment)
leveneTest(Area.mm2 ~ Garment, Operator4_data_gentle) # p-value = 0.2897, equal variances
Operator5_data_gentle$Garment <- as.factor(Operator5_data_gentle$Garment)
leveneTest(Area.mm2 ~ Garment, Operator5_data_gentle) # p-value = 0.5335, equal variances

# Choice of test
# for Operator2_data_gentle : data follow a normal distribution and have equal variance: Two Sample t-test
# for Operator4_data_gentle : data do not follow a normal distribution and have equal variance:  Mann-Whitney U test
# for Operator5_data_gentle : data follow a normal distribution and have equal variance: Two Sample t-test

# Perform the paired T-test
stat.test_Operator2 <- t.test(Area.mm2 ~ Garment, data = Operator2_data_gentle, subset = Garment %in% c("G1", "G2"),var.equal = TRUE)
print(stat.test_Operator2) # p-value = 0.723 - NS
stat.test_Operator5 <- t.test(Area.mm2 ~ Garment, data = Operator5_data_gentle, subset = Garment %in% c("G1", "G2"),var.equal = TRUE)
print(stat.test_Operator5) # p-value = 0.5329 - NS

# Perform the Mann-Whitney U test
stat.test_Operator4 <- wilcox.test(Area.mm2 ~ Garment, data = Operator4_data_gentle, subset = Garment %in% c("G1", "G2"))
print(stat.test_Operator4) # p-value = 0.181 - NS

# Firm pressure
data_firm <- Handpressure_firm_table
Operator2_data_firm <- filter(data_firm,Operator == "Operator 2")
Operator2_data_firm <- Operator2_data_firm %>%
  select("Garment","Area.mm2")
Operator3_data_firm <- filter(data_firm,Operator == "Operator 3")
Operator3_data_firm <- Operator3_data_firm %>%
  select("Garment","Area.mm2")
Operator5_data_firm <- filter(data_firm,Operator == "Operator 5")
Operator5_data_firm <- Operator5_data_firm %>%
  select("Garment","Area.mm2")

Operator2_data_firm %>%
  group_by(Garment) %>%
  shapiro_test(Area.mm2)

Operator3_data_firm %>%
  group_by(Garment) %>%
  shapiro_test(Area.mm2)

Operator5_data_firm %>%
  group_by(Garment) %>%
  shapiro_test(Area.mm2)

### sphericity test
#Levene’s Test
#H0: All sample variances are equal
#H1: At least one group has a variance that is not equal to the rest.
Operator2_data_firm$Garment <- as.factor(Operator2_data_firm$Garment)
leveneTest(Area.mm2 ~ Garment, Operator2_data_firm) # p-value = 0.006145 , unequal variances
Operator3_data_firm$Garment <- as.factor(Operator3_data_firm$Garment)
leveneTest(Area.mm2 ~ Garment, Operator3_data_firm) # p-value = 0.4807, equal variances
Operator5_data_firm$Garment <- as.factor(Operator5_data_firm$Garment)
leveneTest(Area.mm2 ~ Garment, Operator5_data_firm) # p-value = 0.2079, equal variances

# Choice of test
# for Operator2_data_firm : data follow a normal distribution and have equal variance: Welch Two Sample t-test
# for Operator3_data_firm : data follow a normal distribution and have equal variance: Two Sample t-test
# for Operator5_data_firm : data do not follow a normal distribution and have equal variance:  Mann-Whitney U test

# Perform the paired T-test
stat.test_Operator2 <- t.test(Area.mm2 ~ Garment, data = Operator2_data_firm, subset = Garment %in% c("G1", "G2"))
print(stat.test_Operator2) # p-value = 8.537e-05 - ***
stat.test_Operator3 <- t.test(Area.mm2 ~ Garment, data = Operator3_data_firm, subset = Garment %in% c("G1", "G2"),var.equal = TRUE)
print(stat.test_Operator5) # p-value = 0.1648 - NS

# Perform the Mann-Whitney U test
stat.test_Operator5 <- wilcox.test(Area.mm2 ~ Garment, data = Operator5_data_firm, subset = Garment %in% c("G1", "G2"))
print(stat.test_Operator3) # p-value = 0.09861 - NS

### Difference between operators
# Gentle pressure
Operator2_data_gentle %>%
  shapiro_test(Area.mm2) # p-value = 0.196, normal distribution
Operator4_data_gentle %>%
  shapiro_test(Area.mm2) # p-value = 0.000109, non-parametric distribution
Operator5_data_gentle %>%
  shapiro_test(Area.mm2) # p-value = 0.191, normal distribution

forstats_gentle_operators <- filter(data_gentle, Pressure == "gentle")
forstats_gentle_operators <- forstats_gentle_operators %>%
  select("Operator","Area.mm2")

### sphericity test
#Levene’s Test
#H0: All sample variances are equal
#H1: At least one group has a variance that is not equal to the rest.
forstats_gentle_operators$Operator <- as.factor(forstats_gentle_operators$Operator)
leveneTest(Area.mm2 ~ Operator, forstats_gentle_operators) # p-value = 0.08089 , equal variances

# Choice of test
# for forstats_gentle_operators : data do not follow a normal distribution and have equal variance:   Kruskal-Wallis test 

# Kruskal-Wallis test 
kruskal_test_result <- kruskal.test(Area.mm2 ~ Operator, data = forstats_gentle_operators)
print(kruskal_test_result) # p-value = 1.505e-07, differences between groups

# Perform Dunn's test for multiple comparisons
dunn_test_result <- dunnTest(Area.mm2 ~ Operator,data = forstats_gentle_operators,method = "bonferroni")
print(dunn_test_result)

# Firm pressure
Operator2_data_firm %>%
  shapiro_test(Area.mm2)# p-value = 0.00374, non-parametric distribution

Operator3_data_firm %>%
  shapiro_test(Area.mm2)# p-value = 0.0102, non-parametric distribution

Operator5_data_firm %>%
  shapiro_test(Area.mm2)# p-value = 0.0000265, non-parametric distribution

forstats_firm_operators <- filter(data_firm, Pressure == "firm")
forstats_firm_operators <- forstats_firm_operators %>%
  select("Operator","Area.mm2")

### sphericity test
#Levene’s Test
#H0: All sample variances are equal
#H1: At least one group has a variance that is not equal to the rest.
forstats_firm_operators$Operator <- as.factor(forstats_firm_operators$Operator)
leveneTest(Area.mm2 ~ Operator, forstats_firm_operators) # p-value = 1.455e-06 *** , unequal variances

# Choice of test
# for forstats_firm_operators : data do not follow a normal distribution and have unequal variance:   Kruskal-Wallis tes

# Kruskal-Wallis test 
kruskal_test_result <- kruskal.test(Area.mm2 ~ Operator, data = forstats_firm_operators)
print(kruskal_test_result) # p-value = 7.374e-16, differences between groups

# Perform Dunn's test for multiple comparisons
dunn_test_result <- dunnTest(Area.mm2 ~ Operator,data = forstats_firm_operators,method = "bonferroni")
print(dunn_test_result)

# ------------------------------------------------------------------------
# Section 2: Impact test
# ------------------------------------------------------------------------
# Convert Area from inch2 to mm2
# 1 pixel = 1 x10^-5 inch2, so (Area*1)/0.000011 to convert into px
ImpactTest$Area.px <- (ImpactTest$Area*1)/0.000011
# scale of the image: 1 mm = 112 pixels, 1mm2 = 12544 px
ImpactTest$Area.mm2 <- ImpactTest$Area.px/12544

## Create different dataframes for each height
ImpactTest_5mm <- ImpactTest %>% filter(Height == "5mm")
ImpactTest_10mm <- ImpactTest %>% filter(Height == "10mm")
ImpactTest_15mm <- ImpactTest %>% filter(Height == "15mm")
ImpactTest_20mm <- ImpactTest %>% filter(Height == "20mm")

# STATS
ggplot(ImpactTest) +
  aes(x = Height, y = Area.mm2, color = Height) +
  geom_jitter() +
  theme(legend.position = "none")

# Test of normality - shapiro test
ggqqplot(ImpactTest_5mm, "Area.mm2", facet.by = "Height")
ggqqplot(ImpactTest_10mm, "Area.mm2", facet.by = "Height")
ggqqplot(ImpactTest_15mm, "Area.mm2", facet.by = "Height")
ggqqplot(ImpactTest_20mm, "Area.mm2", facet.by = "Height")

# H0: the population is normally distributed, i.e if the p-value is greater than 0.05, then the null hypothesis is accepted
# H1: the population is not normally distributed, i.e if the p-value is less than or equal to 0.05, then the null hypothesis is rejected
# ggqqplot(Transfer_Red_5cm, "value", facet.by = "weight")
shapiro.test(ImpactTest_5mm$Area.mm2) # p-value > 0.05 (p-value = 0.3532) = normally distributed
shapiro.test(ImpactTest_10mm$Area.mm2) # p-value > 0.05 (p-value = 0.1473) = normally distributed
shapiro.test(ImpactTest_15mm$Area.mm2) # p-value > 0.05 (p-value = 0.6364) = normally distributed
shapiro.test(ImpactTest_20mm$Area.mm2) # p-value > 0.05 (p-value = 0.3696) = normally distributed

### sphericity test
#Levene’s Test
#H0: All sample variances are equal
#H1: At least one group has a variance that is not equal to the rest.
ImpactTest$Height <- as.factor(ImpactTest$Height)
leveneTest(Area.mm2 ~ Height, ImpactTest)
# p-value = 0.2234, indicating that there is not enough evidence to reject the hypothesis of equal variances...
#... across the different height groups.

# Choice of test
# for ImpactTest: data follow a normal distribution but do not have similar variance: ANOVA

### ANOVA for ImpactTest
res_aov <- aov(Area.mm2 ~ Height, data = ImpactTest)
summary(res_aov)
#  p-value = 0.205, indicating that there no are statistically significant differences in the mean Area.mm2 across the different height categories...
#... at the conventional alpha level of 0.05. This suggests that the height from which the impact test is conducted does not affects the area measured.
# Tukey HSD test:
post_test <- glht(res_aov, linfct = mcp(Height = "Tukey"))
summary_post_test1 <- summary(post_test);summary_post_test1


# descriptive statistics
meanImpactTest <- ddply(ImpactTest, .(Height), summarise, Mean = round(mean(Area.mm2),digits = 2));meanImpactTest
SDImpactTest <- ddply(ImpactTest, .(Height), summarise, sd = round(sd(Area.mm2),digits = 2));SDImpactTest
stats <- ImpactTest %>%
  group_by(Height) %>%
  summarise(Mean = mean(Area.mm2), SD = sd(Area.mm2));stats

#removing outliers
iqr <- IQR(ImpactTest$Area.mm2)
Q <- quantile(ImpactTest$Area.mm2, probs=c(.25, .75), na.rm = FALSE)
up <-  Q[2]+1.5*iqr # Upper Range  
low<- Q[1]-1.5*iqr # Lower Range
eliminated_ImpactTest<- subset(ImpactTest, ImpactTest$Area.mm2 > (Q[1] - 1.5*iqr) & ImpactTest$Area.mm2 < (Q[2]+1.5*iqr))

# adding label_offset column
meanImpactTest$label_offset <- c(-2, 2, 5, -3)

pImpactTest <- ggplot(eliminated_ImpactTest, aes(x = factor(Height, levels = c('5mm', '10mm', '15mm', '20mm')), y = Area.mm2)) +
  geom_boxplot() +
  geom_text(data = meanImpactTest, aes(x = Height, y = Mean + label_offset, label = Mean), size = 3, vjust = -0.5, colour = "blue") +
  labs(x = "\nHeight of impact", y = expression("Shed fibre area (mm"^2*")")) +
  theme_bw(base_family = "Arial", base_size = 12) +
  ylim(20, 90) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.background = element_rect(fill="grey95", size=1, linetype="solid", colour="grey80"),
        axis.text.x = element_text(angle = 0, vjust = 0.95, hjust=0.5))
pImpactTest

# to save the graph
ggsave("Figure 7 - Impact test.png", pImpactTest, width = 6, height = 5, units = "in", dpi=600, path = "Results/")

# ------------------------------------------------------------------------
# Section 3: Repeated Contact
# ------------------------------------------------------------------------
# Convert Area from inch2 to mm2
# 1 pixel = 1 x10^-5 inch2, so (Area*1)/0.000011 to convert into px
RepeatedContact$Area.px <- (RepeatedContact$Area*1)/0.000011
# scale of the image: 1 mm = 112 pixels, 1mm2 = 12544 px
RepeatedContact$Area.mm2 <- RepeatedContact$Area.px/12544

## Create different dataframes for each height
RepeatedContact_R1 <- RepeatedContact %>% filter(Repeat == "R1")
RepeatedContact_R2 <- RepeatedContact %>% filter(Repeat == "R2")
RepeatedContact_R3 <- RepeatedContact %>% filter(Repeat == "R3")
RepeatedContact_R4 <- RepeatedContact %>% filter(Repeat == "R4")
RepeatedContact_R5 <- RepeatedContact %>% filter(Repeat == "R5")
RepeatedContact_R6 <- RepeatedContact %>% filter(Repeat == "R6")

# STATS
ggplot(RepeatedContact) +
  aes(x = Repeat, y = Area.mm2, color = Repeat) +
  geom_jitter() +
  theme(legend.position = "none")

# Test of normality - shapiro test
ggqqplot(RepeatedContact_R1, "Area.mm2", facet.by = "Repeat")
ggqqplot(RepeatedContact_R2, "Area.mm2", facet.by = "Repeat")
ggqqplot(RepeatedContact_R3, "Area.mm2", facet.by = "Repeat")
ggqqplot(RepeatedContact_R4, "Area.mm2", facet.by = "Repeat")
ggqqplot(RepeatedContact_R5, "Area.mm2", facet.by = "Repeat")
ggqqplot(RepeatedContact_R6, "Area.mm2", facet.by = "Repeat")

# H0: the population is normally distributed, i.e if the p-value is greater than 0.05, then the null hypothesis is accepted
# H1: the population is not normally distributed, i.e if the p-value is less than or equal to 0.05, then the null hypothesis is rejected
# ggqqplot(Transfer_Red_5cm, "value", facet.by = "weight")
shapiro.test(RepeatedContact_R1$Area.mm2) # p-value > 0.05 (p-value = 0.08486) = normally distributed
shapiro.test(RepeatedContact_R2$Area.mm2) # p-value > 0.05 (p-value = 0.6017) = normally distributed
shapiro.test(RepeatedContact_R3$Area.mm2) # p-value > 0.05 (p-value = 0.2871) = normally distributed
shapiro.test(RepeatedContact_R4$Area.mm2) # p-value > 0.05 (p-value = 0.8708) = normally distributed
shapiro.test(RepeatedContact_R5$Area.mm2) # p-value > 0.05 (p-value = 0.3022) = normally distributed
shapiro.test(RepeatedContact_R6$Area.mm2) # p-value > 0.05 (p-value = 0.4958) = normally distributed

### sphericity test
#Levene’s Test
#H0: All sample variances are equal
#H1: At least one group has a variance that is not equal to the rest.
RepeatedContact$Repeat <- as.factor(RepeatedContact$Repeat)
leveneTest(Area.mm2 ~ Repeat, RepeatedContact)
# p-value = 0.8457, indicating that there is not enough evidence to reject the hypothesis of equal variances...
#... across the different Repeat groups.

# Choice of test
# for RepeatedContact: data follow a normal distribution but do not have similar variance: ANOVA

### ANOVA for RepeatedContact
res_aov <- aov(Area.mm2 ~ Repeat, data = RepeatedContact)
summary(res_aov)
#  p-value = 0.205, indicating that there no are statistically significant differences in the mean Area.mm2 across the different Repeat categories...
#... at the conventional alpha level of 0.05. This suggests that the Repeat from which the impact test is conducted does not affects the area measured.
# Tukey HSD test:
post_test <- glht(res_aov, linfct = mcp(Repeat = "Tukey"))
summary_post_test2 <- summary(post_test);summary_post_test2

# descriptive statistics
meanRepeatedContact <- ddply(RepeatedContact, .(Repeat), summarise, Mean = round(mean(Area.mm2),digits = 2));meanRepeatedContact
SDRepeatedContact <- ddply(RepeatedContact, .(Repeat), summarise, sd = round(sd(Area.mm2),digits = 2));SDRepeatedContact
stats <- RepeatedContact %>%
  group_by(Repeat) %>%
  summarise(Mean = mean(Area.mm2), SD = sd(Area.mm2));stats

# removing outliers
iqr <- IQR(RepeatedContact$Area.mm2)
Q <- quantile(RepeatedContact$Area.mm2, probs=c(.25, .75), na.rm = FALSE)
up <-  Q[2]+1.5*iqr # Upper Range  
low<- Q[1]-1.5*iqr # Lower Range
eliminated_RepeatedContact<- subset(RepeatedContact, RepeatedContact$Area.mm2 > (Q[1] - 1.5*iqr) & RepeatedContact$Area.mm2 < (Q[2]+1.5*iqr))

# adding label_offset column
meanRepeatedContact$label_offset <- c(0,1,-2,0,-2,-1);meanRepeatedContact

pRepeatedContact <- ggplot(eliminated_RepeatedContact, aes(x = factor(Repeat, levels = c('R1','R2','R3','R4','R5','R6')), y = Area.mm2)) +
  geom_boxplot() +
  geom_text(data = meanRepeatedContact, aes(x = Repeat, y = Mean + label_offset, label = Mean), size = 3, vjust = -0.5, colour = "blue") +
  labs(x = "\nRepetition of contact", y = expression("Shed fibre area (mm"^2*")")) +
  theme_bw(base_family = "Arial", base_size = 12) +
  ylim(20, 90) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.background = element_rect(fill="grey95", size=1, linetype="solid", colour="grey80"),
        axis.text.x = element_text(angle = 0, vjust = 0.95, hjust=0.5))
pRepeatedContact

# to save the graph
ggsave("Figure 8 - Repeated contact test.png", pRepeatedContact, width = 6, height = 5, units = "in", dpi=600, path = "Results/")

# ------------------------------------------------------------------------
# Section 4: save statistic tables
# ------------------------------------------------------------------------
dataframes <- list(summary_post_test1 = summary_post_test1, summary_post_test2 = summary_post_test2)

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

# Define the desired filenames
filenames <- c("Results/Statistics/Statistic - Impact test.csv", "Results/Statistics/Statistic - Repeated contact.csv")

# Loop through the dataframes and compute/write statistics
for (i in seq_along(dataframes)) {
  compute_and_write_stats(dataframes[[i]], filenames[i])
}

