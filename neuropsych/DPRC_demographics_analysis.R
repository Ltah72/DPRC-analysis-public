#Analysing demographics information from the DPRC cohort. Here, we will be looking at the age and sex of the participants 
#and the clinical sites (Auckland, Christchurch and Dunedin) of where the participant MRI scans took place. We have 5 
#participant groups of interest, which are: Controls (C), subjective cognitive decline (SCD), amnestic mild cognitive 
#impairment (aMCI), multiple-domain mild cognitive impairment (mMCI), and Alzheimer's disease (AD).   


#Author: Lenore Tahara-Eckl
#Email: Ltah262@aucklanduni.ac.nz
#Date: 1/12/20


#load libraries via pacman
pacman::p_load(dplyr, ggplot2, psych, car, lsr, BayesFactor, RColorBrewer)

#add any necessary sources: 
source("https://raw.githubusercontent.com/datavizpyr/data/master/half_flat_violinplot.R") #for raincloud graph

#load in pathway for functions, such as the insertRow.r function (if needed)
source('path/tofile/here.R')
#e.g., source('H:/ltah262/PhD/Diffusion/script/dprc/neuropsych/insertRow.R')

#set up pathway
setwd('/yourpathway/')
#e.g., setwd('H:/ltah262/PhD/ExecutiveFunction/NeuroPsychAssessment/data/')



#######----------------- Cross-sectional (F0) analysis -----------------########

#read in excel files (participant file) - choose cross-sectional or longitudinal analysis
your_file_name <- read.csv("your_file_name.csv") #cross-sectional
#e.g., DPRC_demographics <- read.csv("cross-sectional_DPRC_neuropsych_data_lined_up_valid_participants.csv")

#rename first column 
colnames(DPRC_demographics)[1] <-'ParticipantID'

#convert categorical variables to a factor
DPRC_demographics$Group <- as.factor(DPRC_demographics$Group)
DPRC_demographics$Sex_binary <- as.factor(DPRC_demographics$Sex_binary)

#convert variables to numeric 
DPRC_demographics$Age<- as.numeric(DPRC_demographics$Age)
DPRC_demographics$ACE<- as.numeric(DPRC_demographics$ACE)

#add in trend Group variable
Trend_Group <- as.numeric(DPRC_demographics$Group)
#Trend group with contrasts that sum to zero
Trend_Group_equate_zero_contrast<- vector(mode='numeric',length=length(Trend_Group))
for (i in seq(Trend_Group)) {
    if ((Trend_Group[i] >= 1) && (Trend_Group[i]<= 1)) {
        Trend_Group_equate_zero_contrast[i] <- 2
    }  else if   ((Trend_Group[i] >= 2) && (Trend_Group[i]<= 2)) {
        Trend_Group_equate_zero_contrast[i] <- 1
    }  else if   ((Trend_Group[i] >= 3) && (Trend_Group[i]<= 3)) {
        Trend_Group_equate_zero_contrast[i] <- 0
    }  else if   ((Trend_Group[i] >= 4) && (Trend_Group[i]<= 4)) {
        Trend_Group_equate_zero_contrast[i] <- -1
    }  else if   ((Trend_Group[i] >= 5) && (Trend_Group[i]<= 5)) {
        Trend_Group_equate_zero_contrast[i] <- -2
    }
}

#look at descriptive statistics
age_descrip <- describeBy(DPRC_demographics$Age, DPRC_demographics$Group)
ACE_descrip <- describeBy(DPRC_demographics$ACE, DPRC_demographics$Group)
gender_descrip <- by(DPRC_demographics$Group, DPRC_demographics$Sex, summary)
gender_descrip_detail <- describeBy(DPRC_demographics ~ Sex_binary + Group, skew=FALSE, ranges=FALSE)
clinsite_descrip <- by(DPRC_demographics$Group, DPRC_demographics$Clinical_site, summary)

#clinsite_descrip <- describeBy(DPRC_demographics ~ Sex_binary + Group, skew=FALSE, ranges=FALSE)

#find mean & SD from total sample:
#Age
mean(DPRC_demographics$Age)
sd(DPRC_demographics$Age)
#ACE
all_ACE <- DPRC_demographics$ACE
noNAsACE <- na.omit(all_ACE)
mean(noNAsACE)
sd(noNAsACE)


####---------------------plot & analyse the data to visualise-------------------------------#####
#Examples of plotting data
#plot age (as a violin plot)
ggplot(DPRC_demographics, aes(x = Group, y = Age)) + 
    geom_boxplot(width = 0.1, fill = "white", outlier.size = 1, aes(colour = Group)) + 
    stat_summary(fun = mean, geom = "point", shape = 19, size = 2, aes(colour = Group)) + 
    ylim(50, 95) +
    xlab("Group") + 
    ylab("Age") +
    scale_x_discrete(labels = c("1" = "Control", "2" = "SCD", "3" = "aMCI", "4" = "mMCI", "5" = "AD")) + 
    theme_classic() +
    theme(legend.position = "none") +
    geom_violin(trim = FALSE, alpha = .5, aes(fill = Group, colour = Group), size = 1)

#plot age (as a raincloud plot)
ggplot(DPRC_demographics, aes(x = Group, y = Age, fill = Group)) + 
    geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
    geom_point(aes(y = Age, color = Group), position = position_jitter(width = .15), size = .5, alpha = 0.8) +
    geom_boxplot(width = 0.1, fill = "white", outlier.size = 1, aes(colour = Group)) + 
    stat_summary(fun = mean, geom = "point", shape = 19, size = 2, aes(colour = Group)) + 
    ylim(50, 95) +
    xlab("Group") + 
    ylab("Age") +
    scale_x_discrete(labels = c("1" = "Control", "2" = "SCD", "3" = "aMCI", "4" = "mMCI", "5" = "AD")) + 
    theme_classic() +
    theme(legend.position = "none") +
    coord_flip()

#plot gender
gender_data <- data.frame(table(DPRC_demographics$Classification, DPRC_demographics$Sex))
names(gender_data) <- c("Group", "Sex", "Count")
Group_order <- c("C", "SCD", "aMCI", "mMCI", "AD")
gender_data <- gender_data %>% arrange(factor(Group, levels=Group_order))
gender_data$Group <- factor(gender_data$Group, levels=c("C", "SCD", "aMCI", "mMCI", "AD"))

ggplot(data=gender_data, aes(x=Group, y=Count, fill=Sex)) +
    geom_bar(stat="identity") + 
    theme_bw() + 
    theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

#linear trend example plot
#DPRC_demographics$Group <- as.numeric(DPRC_demographics$Group)
ggplot(DPRC_demographics, aes(x = Group, y = ACE, fill = Group)) + 
    geom_point(aes(y = ACE, color = factor(Group))) +
    geom_smooth(method = lm, se = TRUE)+ 
    xlab("Group") + 
    ylab("ACE") +
    theme_classic() +
    theme(legend.position = "none")
#DPRC_demographics$Group <- as.factor(DPRC_demographics$Group)





#check for significant difference in age between groups example
age_mod <- lm(Age ~ Classification, data = DPRC_demographics)
#age_mod <- lm(Age ~ 0 + Classification, data = DPRC_demographics) #test against y-intercept
anova(age_mod)
#Bayesian (put into a new dataset b/c can't have any NA values)
For_Bay_data <- dplyr::select(DPRC_demographics, ParticipantID, Group, Age, ACE)
summary(For_Bay_data)
For_Bay_data_noNas <- na.omit(For_Bay_data)
anovaBF(Age ~ Group, data = For_Bay_data_noNas) 
lmBF(Age ~ Group, data = For_Bay_data_noNas)
#calculate the effect size (eta-squared)
etaSquared(age_mod)
#conduct power analysis for age
age_group_means <- c(age_descrip$`1`$mean, age_descrip$`2`$mean, age_descrip$`3`$mean, age_descrip$`4`$mean, age_descrip$`5`$mean)
power_age_n <- power.anova.test(groups = length(age_group_means), between.var = anova(age_mod)$`Sum Sq`[1], within.var = anova(age_mod)$`Sum Sq`[2], power = .8, sig.level = 0.05)
power_age_power<- power.anova.test(groups = length(ACE_group_means), between.var = anova(age_mod)$`Sum Sq`[1], within.var = anova(age_mod)$`Sum Sq`[2], n = 41, sig.level = 0.05)


#check for significant difference in gender between groups example. 
#reformat data for chi-square test
#note that you will need to manually enter the number of females and males per each group you have
gender_data_chisq <- rbind(c(27,38,24,27,7), c(8,22,31,25,20))
colnames(gender_data_chisq) <- c("C", "SCD", "aMCI", "mMCI", "AD")
rownames(gender_data_chisq) <- c("F", "M")
#run chi-square test
gender_chi_test <- chisq.test(gender_data_chisq)
contingencyTableBF(gender_data_chisq, sampleType = "jointMulti")
#a good resource on running Bayesian chi-squared tests: https://stats.libretexts.org/Bookshelves/Applied_Statistics/Book%3A_Learning_Statistics_with_R_-_A_tutorial_for_Psychology_Students_and_other_Beginners_(Navarro)/17%3A_Bayesian_Statistics/17.06%3A_Bayesian_Analysis_of_Contingency_Tables
cramersV(gender_data_chisq)

#check for homogeneity of variance, example with age variable 
leveneTest(ACE~Group, data=DPRC_demographics) #violation
#check for significant difference in ACE between groups 
ACE_mod <- lm(ACE ~ Group, data = DPRC_demographics)
anova(ACE_mod)
summary(ACE_mod) #for linear trend analysis
anovaBF(ACE ~ Group, data = For_Bay_data_noNas) 
etaSquared(ACE_mod)

#conduct power analysis, example with ACE variable 
ACE_group_means <- c(ACE_descrip$`1`$mean, ACE_descrip$`2`$mean, ACE_descrip$`3`$mean, ACE_descrip$`4`$mean, ACE_descrip$`5`$mean)
#power_ACE <- power.anova.test(groups = length(ACE_group_means), between.var = var(ACE_group_means), within.var = 7850.6, power = .8, sig.level = 0.05)
power_ACE_n <- power.anova.test(groups = length(ACE_group_means), between.var = anova(ACE_mod)$`Sum Sq`[1], within.var = anova(ACE_mod)$`Sum Sq`[2], power = .8, sig.level = 0.05)
power_ACE_power<- power.anova.test(groups = length(ACE_group_means), between.var = anova(ACE_mod)$`Sum Sq`[1], within.var = anova(ACE_mod)$`Sum Sq`[2], n = 41, sig.level = 0.05)

#add in Linear Trend Analysis in Linear Regression
ACE_LinTrend_mod <- lm(ACE ~ Trend_Group + Group, data = DPRC_demographics)
anova(ACE_LinTrend_mod)
summary(ACE_LinTrend_mod) 

#Try with the linear contrasts summing to zero
ACE_LinTrendZeroCont_mod <- lm(ACE ~ Trend_Group_equate_zero_contrast + Group, data = DPRC_demographics)
anova(ACE_LinTrendZeroCont_mod)
summary(ACE_LinTrendZeroCont_mod)

# #check for significant difference in FBA metrics between groups 
# FD_mod <- lm(Mean_FD ~ Group, data = covariates_data)
# anova(FD_mod)
# FC_mod <- lm(Mean_FC ~ Group, data = covariates_data)
# anova(FC_mod)
# FDC_mod <- lm(Mean_FDC ~ Group, data = covariates_data)
# anova(FDC_mod)
# 

#quick view of the data
plot(age_mod)


#Perform Levene's Test for homogenity of variances
leveneTest(Age ~ Group, data = DPRC_demographics)


# leveneTest(Mean_FD ~ Group, data = covariates_data)
# leveneTest(Mean_FD ~ Sex, data = covariates_data)

#Perform a Shapiro-Wilk test for normality of residuals
shapiro.test(age_mod$residuals)


# #check to see if variables have a linear relationship (e.g. age vs. white matter integrity metrics)
# #for Mean FD
# ggplot(subset(covariates_data, Group %in% c("1", "2", "3", "4", "5")), aes(x = Age, y = Mean_FD, shape = Group, color = Group)) +
#     geom_point() +
#     geom_smooth(method="lm", formula= y~x, se=FALSE) +
#     ggtitle("Age vs. Fibre Density (FD)") +
#     theme(plot.title = element_text(hjust=0.5))
#     #scale_x_discrete(labels = c("1"="Control", "2"="SCD", "3"="aMCI", "4"="mMCI", "5"="AD")) 
#     
# 
# #for Mean FC
# ggplot(subset(covariates_data, Group %in% c("1", "2", "3", "4", "5")), aes(x = Age, y = Mean_FC, shape = Group, color = Group)) +
#     geom_point() +
#     geom_smooth(method="lm", formula= y~x, se=FALSE) +
#     ggtitle("Age vs. Fibre Cross-section (FC)") +
#     theme(plot.title = element_text(hjust=0.5))
# 
# 
# #for Mean FDC
# ggplot(subset(covariates_data, Group %in% c("1", "2", "3", "4", "5")), aes(x = Age, y = Mean_FDC, shape = Group, color = Group)) +
#     geom_point() +
#     geom_smooth(method="lm", formula= y~x, se=FALSE) +
#     ggtitle("Age vs. Fibre Density Cross-section (FDC)") +
#     theme(plot.title = element_text(hjust=0.5))
# 
# 
# 
# 
# #all 3 metrics (FD, FC, FDC)
# ggplot(subset(covariates_data, Group %in% c("1", "2", "3", "4", "5")), aes(x = Age)) +
#     geom_point(aes(y=Mean_FD, color="Mean_FD")) + 
#     geom_smooth(aes(y=Mean_FD, color="Mean_FD"), method="lm", formula= y~x, se=FALSE) +
#     geom_point(aes(y=Mean_FC, color="Mean_FC")) + 
#     geom_smooth(aes(y=Mean_FC, color="Mean_FC"), method="lm", formula= y~x, se=FALSE) +
#     geom_point(aes(y=Mean_FDC, color="Mean_FDC")) + 
#     geom_smooth(aes(y=Mean_FDC, color="Mean_FDC"), method="lm", formula= y~x, se=FALSE) +
#     ylab("Fixel-Based Analysis (FBA) metrics") +
#     ggtitle("Age vs. all 3 metrics (FD, FC, & FDC)") +
#     theme(plot.title = element_text(hjust=0.5))
# 
# 
# 
# plot(covariates_data$Age, covariates_data$Mean_FD)
# 
# 
# 
# #check for significance with age and between age x group and white matter integrity (FBA metrics)
# #run correlation + simple linear regression between age and white matter metrics
# #for FD
# Age_FBA_FD_cor <- cor.test(covariates_data$Age, covariates_data$Mean_FD)
# Age_FBA_FD_mod <- lm(Mean_FD ~ Age, data = covariates_data)
# summary(Age_FBA_FD_mod)
# anova(Age_FBA_FD_mod)
# #for FC
# Age_FBA_FC_cor <- cor.test(covariates_data$Age, covariates_data$Mean_FC)
# Age_FBA_FC_mod <- lm(Mean_FC ~ Age, data = covariates_data)
# summary(Age_FBA_FC_mod)
# #for FDC
# Age_FBA_FDC_cor <- cor.test(covariates_data$Age, covariates_data$Mean_FDC)
# Age_FBA_FDC_mod <- lm(Mean_FDC ~ Age, data = covariates_data)
# summary(Age_FBA_FDC_mod)
# 
# #run multiple linear regression on age x group and white matter integrity (FBA metrics)
# #for FD
# AgexGroup_FD_mult_mod <- lm(Mean_FD ~ Age + Group + Age:Group, data = covariates_data)
# summary(AgexGroup_FD_mult_mod)
# anova(AgexGroup_FD_mult_mod)
# #for FC
# AgexGroup_FC_mult_mod <- lm(Mean_FC ~ Age + Group + Age:Group, data = covariates_data)
# summary(AgexGroup_FC_mult_mod)
# anova(AgexGroup_FC_mult_mod)
# #for FDC
# AgexGroup_FDC_mult_mod <- lm(Mean_FDC ~ Age + Group + Age:Group, data = covariates_data)
# summary(AgexGroup_FDC_mult_mod)
# anova(AgexGroup_FDC_mult_mod)
# 
# 
# 
# Sex_Group_FD_mod <- lm(Mean_FD ~ Group + Sex + Group:Sex, data = covariates_data)
# 
# 
# 
# #plot gender against white matter integrity metrics (FBA)
# #for FD
# ggplot(covariates_data, aes(x = Sex, y = Mean_FD)) + 
#     geom_boxplot(width = 0.5, fill = "white", outlier.size = 1, aes(colour = Sex)) +
#     stat_summary(fun = mean, geom = "point", shape = 19, size = 2, aes(colour = Sex)) +
#     xlab("Sex") + 
#     ylab("Mean Fibre Density (FD)")
# #for FC
# ggplot(covariates_data, aes(x = Sex, y = Mean_FC)) + 
#     geom_boxplot(width = 0.5, fill = "white", outlier.size = 1, aes(colour = Sex)) +
#     stat_summary(fun = mean, geom = "point", shape = 19, size = 2, aes(colour = Sex)) +
#     xlab("Sex") + 
#     ylab("Mean Fibre Cross-section (FC)")
# #for FDC
# ggplot(covariates_data, aes(x = Sex, y = Mean_FDC)) + 
#     geom_boxplot(width = 0.5, fill = "white", outlier.size = 1, aes(colour = Sex)) +
#     stat_summary(fun = mean, geom = "point", shape = 19, size = 2, aes(colour = Sex)) +
#     xlab("Sex") + 
#     ylab("Mean Fibre Density Cross-section (FDC)")
#      
#     
# #check if there's a difference between the white matter integrity metrics (FBA) between gender
# #for FD
# t.test(covariates_data$Mean_FD ~ covariates_data$Sex, var.equal = TRUE)
# #for FC
# t.test(covariates_data$Mean_FC ~ covariates_data$Sex, var.equal = TRUE)
# #for FDC
# t.test(covariates_data$Mean_FDC ~ covariates_data$Sex, var.equal = TRUE)
# 
# 
# 
# #plot group status and gender against white matter integrity metrics (FBA) - 2x5 factorial ANOVA
# #for FD
# ggplot(covariates_data, aes(x = Group, y = Mean_FD, color = Sex)) + 
#     geom_boxplot(width = 0.5, fill = "white", outlier.size = 1, aes(colour = Sex)) +
#     stat_summary(fun = mean, geom = "point", position = position_dodge(.5), shape = 19, size = 2, aes(colour = Sex)) +
#     xlab("Group") + 
#     ylab("Mean Fibre Density (FD)")+
#     scale_x_discrete(labels = c("1" = "Control", "2" = "SCD", "3" = "aMCI", "4" = "mMCI", "5" = "AD")) 
# #for FC
# ggplot(covariates_data, aes(x = Group, y = Mean_FC, color = Sex)) + 
#     geom_boxplot(width = 0.5, fill = "white", outlier.size = 1, aes(colour = Sex)) +
#     stat_summary(fun = mean, geom = "point", position = position_dodge(.5), shape = 19, size = 2, aes(colour = Sex)) +
#     xlab("Group") + 
#     ylab("Mean Fibre Cross-section (FC)")+
#     scale_x_discrete(labels = c("1" = "Control", "2" = "SCD", "3" = "aMCI", "4" = "mMCI", "5" = "AD")) 
# #for FDC
# ggplot(covariates_data, aes(x = Group, y = Mean_FDC, color = Sex)) + 
#     geom_boxplot(width = 0.5, fill = "white", outlier.size = 1, aes(colour = Sex)) +
#     stat_summary(fun = mean, geom = "point", position = position_dodge(.5), shape = 19, size = 2, aes(colour = Sex)) +
#     xlab("Group") + 
#     ylab("Mean Fibre Density Cross-section (FDC)")+
#     scale_x_discrete(labels = c("1" = "Control", "2" = "SCD", "3" = "aMCI", "4" = "mMCI", "5" = "AD")) 
# 
# 
# 
# #check for significant difference in white matter integrity between groups, gender, and groups x gender
# # 2x5 factorial ANOVA
# #for FD
# Sex_Group_FD_mod <- lm(Mean_FD ~ Group + Sex + Group:Sex, data = covariates_data)
# anova(Sex_Group_FD_mod)
# #for FC
# Sex_Group_FC_mod <- lm(Mean_FC ~ Group + Sex + Group:Sex, data = covariates_data)
# anova(Sex_Group_FC_mod)
# #for FDC
# Sex_Group_FDC_mod <- lm(Mean_FDC ~ Group + Sex + Group:Sex, data = covariates_data)
# anova(Sex_Group_FDC_mod)
# 
# 
# 
# 
# 
# #plot clinical site against white matter integrity metrics (FBA)
# #for FD
# ggplot(covariates_data, aes(x = Clinical_site, y = Mean_FD)) + 
#     geom_boxplot(width = 0.5, fill = "white", outlier.size = 1, aes(colour = Clinical_site)) +
#     stat_summary(fun = mean, geom = "point", shape = 19, size = 2, aes(colour = Clinical_site)) +
#     xlab("Clinical Site") + 
#     ylab("Mean Fibre Density (FD)")
# #for FC
# ggplot(covariates_data, aes(x = Clinical_site, y = Mean_FC)) + 
#     geom_boxplot(width = 0.5, fill = "white", outlier.size = 1, aes(colour = Clinical_site)) +
#     stat_summary(fun = mean, geom = "point", shape = 19, size = 2, aes(colour = Clinical_site)) +
#     xlab("Clinical Site") + 
#     ylab("Mean Fibre Cross-section")
# #for FDC
# ggplot(covariates_data, aes(x = Clinical_site, y = Mean_FDC)) + 
#     geom_boxplot(width = 0.5, fill = "white", outlier.size = 1, aes(colour = Clinical_site)) +
#     stat_summary(fun = mean, geom = "point", shape = 19, size = 2, aes(colour = Clinical_site)) +
#     xlab("Clinical Site") + 
#     ylab("Mean Fibre Density Cross-section (FDC)")
# 
# 
# #check for significant difference in white matter integrity between clinical sites
# #for FD
# Clinical_site_FD_mod <- lm(Mean_FD ~ Clinical_site, data = covariates_data)
# anova(Clinical_site_FD_mod)
# #for FC
# Clinical_site_FC_mod <- lm(Mean_FC ~ Clinical_site, data = covariates_data)
# anova(Clinical_site_FC_mod)
# #for FDC
# Clinical_site_FDC_mod <- lm(Mean_FDC ~ Clinical_site, data = covariates_data)
# anova(Clinical_site_FDC_mod)

#plot groups in a pie chart
group_number <- c(35,60,55,52,27)
myPalette <- brewer.pal(5,"Purples")
#par(bg="transparent")
pie(group_number, labels = c("C","SCD","aMCI","mMCI","AD"), border="white", col=myPalette)

#plot groups in a circle plot (cross-sectional)
group_names <- factor(group_names, levels = c("C","SCD","aMCI","mMCI","AD"))
group_number <- c(35,60,55,52,27)
df_circle_plot <- data.frame(group_names, group_number)
ggplot(df_circle_plot, aes(x = group_names, y = group_number)) +
  geom_col(aes(fill = group_names), color = NA) +
  labs(x = "", y = "") +
  coord_polar() +
  guides(fill = FALSE)+
  theme_classic()+
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())

#plot groups in a circle plot (longitudinal)
group_names <- factor(group_names, levels = c("C","SCD","aMCI","mMCI","AD"))
group_number <- c(22,40,30,21,11)
df_circle_plot <- data.frame(group_names, group_number)
ggplot(df_circle_plot, aes(x = group_names, y = group_number)) +
  geom_col(aes(fill = group_names), color = NA) +
  labs(x = "", y = "") +
  coord_polar() +
  guides(fill = FALSE)+
  theme_classic()+
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())




#######--------------- Longitudinal (F0 vs. F2) analysis ---------------########


#read in excel files (participant file) - choose cross-sectional or longitudinal analysis (n = 124)
DPRC_demographics <- read.csv("longitudinal_DPRC_neuropsych_data_lined_up_valid_participants.csv") #longitudinal

#for functional-diffusion connectome, longitudinal study (n = 119)
#DPRC_demographics <- read.csv("fMRI_dMRI_longitudinal_DPRC_neuropsych_data_lined_up_valid_participants.csv")

#rename first column
colnames(DPRC_demographics)[1] <-'ParticipantID'

#convert categorical variables to a factor
DPRC_demographics$Group <- as.factor(DPRC_demographics$Group)
DPRC_demographics$Sex_binary <- as.factor(DPRC_demographics$Sex_binary)
DPRC_demographics$Timepoint <- as.factor(DPRC_demographics$Timepoint)

#convert variables to numeric 
DPRC_demographics$Age<- as.numeric(DPRC_demographics$Age)
DPRC_demographics$ACE<- as.numeric(DPRC_demographics$ACE)

#look at descriptive statistics
age_descrip <- describeBy(DPRC_demographics$Age, list(DPRC_demographics$Group, DPRC_demographics$Timepoint))
ACE_descrip <- describeBy(DPRC_demographics$ACE, list(DPRC_demographics$Group, DPRC_demographics$Timepoint))
gender_descrip <- by(DPRC_demographics$Group, list(DPRC_demographics$Sex, DPRC_demographics$Timepoint), summary)
clinsite_descrip <- by(DPRC_demographics$Group, list(DPRC_demographics$Clinical_site, DPRC_demographics$Timepoint), summary)


####---------------------plot & analyse the data to visualise-------------------------------####

#take a subset of the DPRC data - only baseline data (F0) to display in graphs
baseline_DPRC_demographics <- DPRC_demographics[ which(DPRC_demographics$Timepoint=='F0'), ]

#whole sample descriptives
#Age
mean(baseline_DPRC_demographics$Age)
sd(baseline_DPRC_demographics$Age)
#ACE
mean(baseline_DPRC_demographics$ACE)
sd(baseline_DPRC_demographics$ACE)

#plot age (violin plot) - only for baseline (F0)
ggplot(baseline_DPRC_demographics, aes(x = Group, y = Age)) + 
    geom_boxplot(width = 0.1, fill = "white", outlier.size = 1, aes(colour = Group)) + 
    stat_summary(fun = mean, geom = "point", shape = 19, size = 2, aes(colour = Group)) + 
    ylim(50, 95) +
    xlab("Group") + 
    ylab("Age") +
    scale_x_discrete(labels = c("1" = "Control", "2" = "SCD", "3" = "aMCI", "4" = "mMCI", "5" = "AD")) + 
    theme_classic() +
    theme(legend.position = "none") +
    geom_violin(trim = FALSE, alpha = .5, aes(fill = Group, colour = Group), size = 1)

#plot age (raincloud plot)
ggplot(baseline_DPRC_demographics, aes(x = Group, y = Age, fill = Group)) + 
    geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
    geom_point(aes(y = Age, color = Group), position = position_jitter(width = .15), size = .5, alpha = 0.8) +
    geom_boxplot(width = 0.1, fill = "white", outlier.size = 1, aes(colour = Group)) + 
    stat_summary(fun = mean, geom = "point", shape = 19, size = 2, aes(colour = Group)) + 
    ylim(50, 95) +
    xlab("Group") + 
    ylab("Age") +
    scale_x_discrete(labels = c("1" = "Control", "2" = "SCD", "3" = "aMCI", "4" = "mMCI", "5" = "AD")) + 
    theme_classic() +
    theme(legend.position = "none") +
    coord_flip()

#plot gender
gender_data <- data.frame(table(baseline_DPRC_demographics$Classification, baseline_DPRC_demographics$Sex))
names(gender_data) <- c("Group", "Sex", "Count")
Group_order <- c("C", "SCD", "aMCI", "mMCI", "AD")
gender_data <- gender_data %>% arrange(factor(Group, levels=Group_order))
gender_data$Group <- factor(gender_data$Group, levels=c("C", "SCD", "aMCI", "mMCI", "AD"))

ggplot(data=gender_data, aes(x=Group, y=Count, fill=Sex)) +
    geom_bar(stat="identity") + 
    theme_bw() + 
    theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

#plot clinical site
location_data <- data.frame(table(baseline_DPRC_demographics$Classification, baseline_DPRC_demographics$Clinical_site))
names(location_data) <- c("Group", "Clinical_site", "Count")
location_data <- location_data %>% arrange(factor(Group, levels=Group_order))
location_data$Group <- factor(location_data$Group, levels=c("C", "SCD", "aMCI", "mMCI", "AD"))

ggplot(data=location_data, aes(x=Group, y=Count, fill=Clinical_site)) +
    geom_bar(stat="identity") +
    scale_fill_discrete(name = "Clinical Site") + 
    theme_bw() + 
    theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

#plot ACE (violin plot) - for F0
ggplot(baseline_DPRC_demographics, aes(x = Group, y = ACE)) + 
    geom_boxplot(width = 0.1, fill = "white", outlier.size = 1, aes(colour = Group)) + 
    stat_summary(fun = mean, geom = "point", shape = 19, size = 2, aes(colour = Group)) + 
    xlab("Group") + 
    ylab("ACE Score") +
    scale_x_discrete(labels = c("1" = "Control", "2" = "SCD", "3" = "aMCI", "4" = "mMCI", "5" = "AD")) + 
    theme_classic() +
    theme(legend.position = "none") +
    geom_violin(trim = FALSE, alpha = .5, aes(fill = Group, colour = Group), size = 1)
#plot ACE (violin plot) - for F0 vs. F2
ggplot(DPRC_demographics, aes(x = Group, y = ACE, fill = Timepoint)) + 
    geom_boxplot(width = 0.1, fill = "white", outlier.size = 1, aes(colour = Timepoint), position = position_dodge(.9)) + 
    stat_summary(fun = mean, geom = "point", shape = 19, size = 2, aes(colour = Timepoint), position = position_dodge(.9)) + 
    xlab("Group") + 
    ylab("ACE Score") +
    scale_x_discrete(labels = c("1" = "Control", "2" = "SCD", "3" = "aMCI", "4" = "mMCI", "5" = "AD")) + 
    theme_classic() +
    geom_violin(trim = FALSE, alpha = .5, aes(fill = Timepoint, colour = Timepoint), size = 1)
#colour by group
ggplot(DPRC_demographics, aes(x = Group, y = ACE, group=interaction(Group, Timepoint))) + 
    geom_boxplot(width = 0.1, fill = "white", outlier.size = 1, aes(colour = Group), position = position_dodge(.9)) + 
    stat_summary(fun = mean, geom = "point", shape = 19, size = 2, aes(colour = Group), position = position_dodge(.9)) + 
    xlab("Group") + 
    ylab("ACE Score") +
    scale_x_discrete(labels = c("1" = "Control", "2" = "SCD", "3" = "aMCI", "4" = "mMCI", "5" = "AD")) + 
    theme_classic() +
    theme(legend.position = "none") +
    geom_violin(trim = FALSE, alpha = .5, aes(fill = Group, colour = Group), size = 1)
#plot ACE (raincloud plot) - for F0
ggplot(baseline_DPRC_demographics, aes(x = Group, y = ACE, fill = Group)) + 
    geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
    geom_point(aes(y = ACE, color = Group), position = position_jitter(width = .15), size = .5, alpha = 0.8) +
    geom_boxplot(width = 0.1, fill = "white", outlier.size = 1, aes(colour = Group)) + 
    stat_summary(fun = mean, geom = "point", shape = 19, size = 2, aes(colour = Group)) + 
    xlab("Group") + 
    ylab("ACE Score") +
    scale_x_discrete(labels = c("1" = "Control", "2" = "SCD", "3" = "aMCI", "4" = "mMCI", "5" = "AD")) + 
    theme_classic() +
    theme(legend.position = "none") +
    coord_flip()
#plot ACE (raincloud plot) - for F0 vs.F2
ggplot(DPRC_demographics, aes(x = Group, y = ACE, fill = Timepoint)) + 
    geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
    geom_point(aes(y = ACE, color = Timepoint), position = position_jitter(width = .15), size = .5, alpha = 0.8) +
    geom_boxplot(width = 0.1, fill = "white", outlier.size = 1, aes(colour = Timepoint)) + 
    stat_summary(fun = mean, geom = "point", shape = 19, size = 2, aes(colour = Timepoint)) + 
    xlab("Group") + 
    ylab("ACE Score") +
    scale_x_discrete(labels = c("1" = "Control", "2" = "SCD", "3" = "aMCI", "4" = "mMCI", "5" = "AD")) + 
    theme_classic() +
    coord_flip()
#colour by group
ggplot(DPRC_demographics, aes(x = Group, y = ACE, group=interaction(Group, Timepoint))) + 
    geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
    geom_point(aes(y = ACE, color = Group), position = position_jitter(width = .15), size = .5, alpha = 0.8) +
    geom_boxplot(width = 0.1, fill = "white", outlier.size = 1, aes(colour = Group)) + 
    stat_summary(fun = mean, geom = "point", shape = 19, size = 2, aes(colour = Group)) + 
    xlab("Group") + 
    ylab("ACE Score") +
    scale_x_discrete(labels = c("1" = "Control", "2" = "SCD", "3" = "aMCI", "4" = "mMCI", "5" = "AD")) + 
    theme(legend.position = "none") +
    theme_classic() +
    coord_flip()

#check for significant difference in age between groups 
age_mod <- lm(Age ~ Classification, data = baseline_DPRC_demographics)
#age_mod <- lm(Age ~ 0 + Classification, data = DPRC_demographics) #test against y-intercept
anova(age_mod)
#Bayesian (put into a new dataset b/c can't have any NA values)
For_Bay_data <- dplyr::select(baseline_DPRC_demographics, ParticipantID, Group, Age, ACE)
summary(For_Bay_data)
For_Bay_data_noNas <- na.omit(For_Bay_data)
anovaBF(Age ~ Group, data = For_Bay_data_noNas) 
lmBF(Age ~ Group, data = For_Bay_data_noNas)
#calculate the effect size (eta-squared)
etaSquared(age_mod)
#conduct power analysis for age
age_group_means <- c(age_descrip$`1`$mean, age_descrip$`2`$mean, age_descrip$`3`$mean, age_descrip$`4`$mean, age_descrip$`5`$mean)
power_age_n <- power.anova.test(groups = length(age_group_means), between.var = anova(age_mod)$`Sum Sq`[1], within.var = anova(age_mod)$`Sum Sq`[2], power = .8, sig.level = 0.05)
power_age_power<- power.anova.test(groups = length(ACE_group_means), between.var = anova(age_mod)$`Sum Sq`[1], within.var = anova(age_mod)$`Sum Sq`[2], n = 41, sig.level = 0.05)


#check for significant difference in gender between groups 
#reformat data for chi-square test
gender_data_chisq <- rbind(c(18,25,14,13,3), c(4,15,16,8,8))
#for dMRI-fMRI study (n = 119)
#gender_data_chisq <- rbind(c(18,25,13,13,3), c(3,15,16,8,7))
colnames(gender_data_chisq) <- c("C", "SCD", "aMCI", "mMCI", "AD")
rownames(gender_data_chisq) <- c("F", "M")
#run chi-square test
gender_chi_test <- chisq.test(gender_data_chisq)
gender_chi_test
#Bayesian version:
contingencyTableBF(gender_data_chisq, sampleType = "jointMulti")
#a good resource on running Bayesian chi-squared tests: https://stats.libretexts.org/Bookshelves/Applied_Statistics/Book%3A_Learning_Statistics_with_R_-_A_tutorial_for_Psychology_Students_and_other_Beginners_(Navarro)/17%3A_Bayesian_Statistics/17.06%3A_Bayesian_Analysis_of_Contingency_Tables
cramersV(gender_data_chisq)


#check for significant difference in clinical site between groups 
#reformat data for chi-square test
#location_data_chisq <- rbind(c(21,36,21,18,10), c(1,3,4,3,0), c(0,1,5,0,1))
#for dMRI-fMRI study (n = 119)
location_data_chisq <- rbind(c(21,36,21,16,9), c(0,3,4,3,0), c(0,1,4,0,1))
colnames(location_data_chisq) <- c("C", "SCD", "aMCI", "mMCI", "AD")
rownames(location_data_chisq) <- c("Auckland", "Christchurch", "Dunedin")
#run chi-square test
location_chi_test <- chisq.test(location_data_chisq)
location_chi_test
contingencyTableBF(location_data_chisq, sampleType = "jointMulti")
cramersV(location_data_chisq)
#if expected values in cells are too small, simulate more p-values:
#location_chi_test <- chisq.test(location_data_chisq, simulate.p.value = TRUE)


#check for significant difference in ACE between groups 
ACE_mod <- lm(ACE ~ Group, data = baseline_DPRC_demographics)
anova(ACE_mod)
anovaBF(ACE ~ Group, data = For_Bay_data_noNas) 
etaSquared(ACE_mod)
#whole sample - ACE descriptives
mean(For_Bay_data_noNas$ACE)
sd(For_Bay_data_noNas$ACE)


#conduct power analysis for ACE
ACE_group_means <- c(ACE_descrip$`1`$mean, ACE_descrip$`2`$mean, ACE_descrip$`3`$mean, ACE_descrip$`4`$mean, ACE_descrip$`5`$mean)
#power_ACE <- power.anova.test(groups = length(ACE_group_means), between.var = var(ACE_group_means), within.var = 7850.6, power = .8, sig.level = 0.05)
power_ACE_n <- power.anova.test(groups = length(ACE_group_means), between.var = anova(ACE_mod)$`Sum Sq`[1], within.var = anova(ACE_mod)$`Sum Sq`[2], power = .8, sig.level = 0.05)
power_ACE_power<- power.anova.test(groups = length(ACE_group_means), between.var = anova(ACE_mod)$`Sum Sq`[1], within.var = anova(ACE_mod)$`Sum Sq`[2], n = 41, sig.level = 0.05)



#quick view of the data
plot(age_mod)


#Perform Levene's Test for homogenity of variances
leveneTest(Age ~ Group, data = baseline_DPRC_demographics)


#Perform a Shapiro-Wilk test for normality of residuals
shapiro.test(age_mod$residuals)















