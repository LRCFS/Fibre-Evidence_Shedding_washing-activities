##########################################################################
#####                     Analysis - Impact Test                     #####
##########################################################################
# This R script is to generate the figures related to XXX

# ------------------------------------------------------------------------
# Section 1: Impact test
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
summary(post_test)

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
  labs(x = "\nHeight of impact", y = expression("Average fibre Area (mm"^2*")")) +
  theme_bw(base_family = "Arial", base_size = 12) +
  ylim(20, 90) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.background = element_rect(fill="grey95", size=1, linetype="solid", colour="grey80"),
        axis.text.x = element_text(angle = 0, vjust = 0.95, hjust=0.5))
pImpactTest

# to save the graph
ggsave("Figure X - Impact test.png", pImpactTest, width = 6, height = 5, units = "in", dpi=600, path = "Results/")

# ------------------------------------------------------------------------
# Section : Repeated Contact
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
  labs(x = "\nRepetition of contact", y = expression("Average fibre Area (mm"^2*")")) +
  theme_bw(base_family = "Arial", base_size = 12) +
  ylim(20, 90) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.background = element_rect(fill="grey95", size=1, linetype="solid", colour="grey80"),
        axis.text.x = element_text(angle = 0, vjust = 0.95, hjust=0.5))
pRepeatedContact

# to save the graph
ggsave("Figure X - Repeated contact test.png", pRepeatedContact, width = 6, height = 5, units = "in", dpi=600, path = "Results/")

