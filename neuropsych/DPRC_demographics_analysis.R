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
#e.g., your_file_name <- read.csv("cross-sectional_DPRC_neuropsych_data_lined_up_valid_participants.csv")

#rename first column 
colnames(your_file_name)[1] <-'ParticipantID'

#convert categorical variables to a factor
your_file_name$Group <- as.factor(your_file_name$Group)
your_file_name$Sex_binary <- as.factor(your_file_name$Sex_binary)

#convert variables to numeric 
your_file_name$Age<- as.numeric(your_file_name$Age)
your_file_name$ACE<- as.numeric(your_file_name$ACE)

#add in trend Group variable
Trend_Group <- as.numeric(your_file_name$Group)
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
age_descrip <- describeBy(your_file_name$Age, your_file_name$Group)
ACE_descrip <- describeBy(your_file_name$ACE, your_file_name$Group)
gender_descrip <- by(your_file_name$Group, your_file_name$Sex, summary)
gender_descrip_detail <- describeBy(your_file_name ~ Sex_binary + Group, skew=FALSE, ranges=FALSE)
clinsite_descrip <- by(your_file_name$Group, your_file_name$Clinical_site, summary)

#find mean & SD from total sample:
#Age
mean(your_file_name$Age)
sd(your_file_name$Age)
#ACE
all_ACE <- your_file_name$ACE
noNAsACE <- na.omit(all_ACE)
mean(noNAsACE)
sd(noNAsACE)

####---------------------plot & analyse the data to visualise-------------------------------#####
#Examples of plotting data with different types of plots
#plot age (as a violin plot)
ggplot(your_file_name, aes(x = Group, y = Age)) + 
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
ggplot(your_file_name, aes(x = Group, y = Age, fill = Group)) + 
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
gender_data <- data.frame(table(your_file_name$Classification, your_file_name$Sex))
names(gender_data) <- c("Group", "Sex", "Count")
Group_order <- c("C", "SCD", "aMCI", "mMCI", "AD")
gender_data <- gender_data %>% arrange(factor(Group, levels=Group_order))
gender_data$Group <- factor(gender_data$Group, levels=c("C", "SCD", "aMCI", "mMCI", "AD"))

ggplot(data=gender_data, aes(x=Group, y=Count, fill=Sex)) +
    geom_bar(stat="identity") + 
    theme_bw() + 
    theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

#linear trend example plot
#your_file_name$Group <- as.numeric(your_file_name$Group)
ggplot(your_file_name, aes(x = Group, y = ACE, fill = Group)) + 
    geom_point(aes(y = ACE, color = factor(Group))) +
    geom_smooth(method = lm, se = TRUE)+ 
    xlab("Group") + 
    ylab("ACE") +
    theme_classic() +
    theme(legend.position = "none")
#your_file_name$Group <- as.factor(your_file_name$Group)

#check for significant difference in age between groups example
age_mod <- lm(Age ~ Classification, data = your_file_name)
#age_mod <- lm(Age ~ 0 + Classification, data = your_file_name) #test against y-intercept
anova(age_mod)
#Bayesian (put into a new dataset b/c can't have any NA values)
For_Bay_data <- dplyr::select(your_file_name, ParticipantID, Group, Age, ACE)
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

#check for homogeneity of variance, example with ACE variable 
leveneTest(ACE~Group, data=your_file_name) #violation
#check for significant difference in ACE between groups 
ACE_mod <- lm(ACE ~ Group, data = your_file_name)
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
ACE_LinTrend_mod <- lm(ACE ~ Trend_Group + Group, data = your_file_name)
anova(ACE_LinTrend_mod)
summary(ACE_LinTrend_mod) 

#Try with the linear contrasts summing to zero
ACE_LinTrendZeroCont_mod <- lm(ACE ~ Trend_Group_equate_zero_contrast + Group, data = your_file_name)
anova(ACE_LinTrendZeroCont_mod)
summary(ACE_LinTrendZeroCont_mod)

#quick view of the data, for the age variable 
plot(age_mod)
#Perform Levene's Test for homogenity of variances 
leveneTest(Age ~ Group, data = your_file_name)
#Perform a Shapiro-Wilk test for normality of residuals
shapiro.test(age_mod$residuals)

#plot groups in a pie chart
group_number <- c(35,60,55,52,27) #will need to modify this
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

#read in excel files (participant data file)
your_file_name <- read.csv("your_data_file_longitudinal.csv") #longitudinal
#e.g., your_file_name <- read.csv("longitudinal_DPRC_neuropsych_data_lined_up_valid_participants.csv") #longitudinal

#rename first column
colnames(your_file_name)[1] <-'ParticipantID'

#convert categorical variables to a factor
your_file_name$Group <- as.factor(your_file_name$Group)
your_file_name$Sex_binary <- as.factor(your_file_name$Sex_binary)
your_file_name$Timepoint <- as.factor(your_file_name$Timepoint)

#convert variables to numeric 
your_file_name$Age<- as.numeric(your_file_name$Age)
your_file_name$ACE<- as.numeric(your_file_name$ACE)

#look at descriptive statistics
age_descrip <- describeBy(your_file_name$Age, list(your_file_name$Group, your_file_name$Timepoint))
ACE_descrip <- describeBy(your_file_name$ACE, list(your_file_name$Group, your_file_name$Timepoint))
gender_descrip <- by(your_file_name$Group, list(your_file_name$Sex, your_file_name$Timepoint), summary)
clinsite_descrip <- by(your_file_name$Group, list(your_file_name$Clinical_site, your_file_name$Timepoint), summary)

####---------------------plot & analyse the data to visualise-------------------------------####

#take a subset of the DPRC data - only baseline data (F0) to display in graphs
baseline_your_file_name <- your_file_name[ which(your_file_name$Timepoint=='F0'), ]

#whole sample descriptives
#ACE
mean(baseline_your_file_name$ACE)
sd(baseline_your_file_name$ACE)

#plot ACE (violin plot) - for F0, baseline, example
ggplot(baseline_your_file_name, aes(x = Group, y = ACE)) + 
    geom_boxplot(width = 0.1, fill = "white", outlier.size = 1, aes(colour = Group)) + 
    stat_summary(fun = mean, geom = "point", shape = 19, size = 2, aes(colour = Group)) + 
    xlab("Group") + 
    ylab("ACE Score") +
    scale_x_discrete(labels = c("1" = "Control", "2" = "SCD", "3" = "aMCI", "4" = "mMCI", "5" = "AD")) + 
    theme_classic() +
    theme(legend.position = "none") +
    geom_violin(trim = FALSE, alpha = .5, aes(fill = Group, colour = Group), size = 1)
#plot ACE (violin plot) - for F0 vs. F2, longitudinal example
ggplot(your_file_name, aes(x = Group, y = ACE, fill = Timepoint)) + 
    geom_boxplot(width = 0.1, fill = "white", outlier.size = 1, aes(colour = Timepoint), position = position_dodge(.9)) + 
    stat_summary(fun = mean, geom = "point", shape = 19, size = 2, aes(colour = Timepoint), position = position_dodge(.9)) + 
    xlab("Group") + 
    ylab("ACE Score") +
    scale_x_discrete(labels = c("1" = "Control", "2" = "SCD", "3" = "aMCI", "4" = "mMCI", "5" = "AD")) + 
    theme_classic() +
    geom_violin(trim = FALSE, alpha = .5, aes(fill = Timepoint, colour = Timepoint), size = 1)
#colour by group
ggplot(your_file_name, aes(x = Group, y = ACE, group=interaction(Group, Timepoint))) + 
    geom_boxplot(width = 0.1, fill = "white", outlier.size = 1, aes(colour = Group), position = position_dodge(.9)) + 
    stat_summary(fun = mean, geom = "point", shape = 19, size = 2, aes(colour = Group), position = position_dodge(.9)) + 
    xlab("Group") + 
    ylab("ACE Score") +
    scale_x_discrete(labels = c("1" = "Control", "2" = "SCD", "3" = "aMCI", "4" = "mMCI", "5" = "AD")) + 
    theme_classic() +
    theme(legend.position = "none") +
    geom_violin(trim = FALSE, alpha = .5, aes(fill = Group, colour = Group), size = 1)
#plot ACE (raincloud plot) - for F0
ggplot(baseline_your_file_name, aes(x = Group, y = ACE, fill = Group)) + 
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
ggplot(your_file_name, aes(x = Group, y = ACE, fill = Timepoint)) + 
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
ggplot(your_file_name, aes(x = Group, y = ACE, group=interaction(Group, Timepoint))) + 
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

#check for significant difference in age between groups, as an example
age_mod <- lm(Age ~ Classification, data = baseline_your_file_name)
#age_mod <- lm(Age ~ 0 + Classification, data = your_file_name) #test against y-intercept
anova(age_mod)
#Bayesian (put into a new dataset b/c can't have any NA values)
For_Bay_data <- dplyr::select(baseline_your_file_name, ParticipantID, Group, Age, ACE)
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

