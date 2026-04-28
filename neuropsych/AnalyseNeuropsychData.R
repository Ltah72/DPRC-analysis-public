#This script will analyse the DPRC neuropsychological assessment data. Will be 
#looking at executive function / cognitive control data. Statistical tests that 
#have been run are an exploratory factor analysis (EFA), multivariate analysis 
#of variance (MANOVAs), and analysis of variance (ANOVAs).

#To deal with unbalanced ANOVA designs, you can use the anova function from the car package. (https://rpubs.com/mhanauer/300976)

#Author: Lenore Tahara-Eckl
#Email: Ltah262@aucklanduni.ac.nz
#Date: 20/06/21

#load libraries via pacman
pacman::p_load(dplyr, ggplot2, psych, car, multcomp, lsr, BayesFactor, tidyr, GPArotation, corrplot, lsmeans, TukeyC, lme4, lmerTest, emmeans, effectsize, nlme, rstatix, sjstats, EMAtools, phia, compute.es, Compositional, MASS,ggforce)
#add any necessary sources: 
source("https://raw.githubusercontent.com/datavizpyr/data/master/half_flat_violinplot.R") #for raincloud graph

#set up pathway
setwd('/yourpathway/')
#file.choose function
#e.g., setwd('H:/ltah262/PhD/ExecutiveFunction/NeuroPsychAssessment/data/')



#######----------------- Cross-sectional (F0) analysis -----------------########

#read in csv files (participant file)
your_data_file <- read.csv("your_data_file_name.csv")
#for connectome data (n = 227)
#e.g., your_data_file <- read.csv(""cross-sectional_your_data_file_lined_up_valid_participants.csv")

#rename first column 
colnames(your_data_file)[1] <-'ParticipantID'

#convert variables
your_data_file$Group <- as.factor(your_data_file$Group)
your_data_file$Sex <- as.factor(your_data_filea$Sex)
your_data_file$Sex_binary <- as.factor(your_data_file$Sex_binary)

#add in trend Group variable
Trend_Group <- as.numeric(your_data_file$Group)
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

#----------------plot data to visualise & run stat tests ----------------------#


####--------------------------------EFA-------------------------------------####
#view a correlation matrix of the variables
#for raw data
#need to remove the columns without continuous values - should only include the 
#neuropsych test values for evaluation. 
your_data_file_raw_vars <- subset(your_data_file, select = c(neuropsych_var1, neuropsych_var2...etc))

#if needed, omit NA values from the dataframe
your_data_file_raw_vars_noNAs<- na.omit(your_data_file_raw_vars)

exec_raw_cor_matrix <- cor(your_data_file_raw_vars_noNAs)
exec_raw_cor_matrix

#View the correlation matrix as a 'heatmap': 
corrplot(exec_raw_cor_matrix, type = "lower", method = "color", tl.col = "black")

#Run the **Kaiser Meyer Olkin (KMO) test**  to see if our data seems suitable 
#for factor analysis.The statistic shows the proportion of variance among the 
#variables that is "common variance" (might be explained by factors).
KMO(exec_raw_cor_matrix)

#Now, let's view a **scree plot** to see how much variance in the data is 
#explained by each component. This is the **eigen value** of each component.

# Calculate the eigen values
eigen_exec_raw <- eigen(exec_raw_cor_matrix)

# Create the scree plot
plot(eigen_exec_raw$values, xlab = "Component", ylab = "Eigen Values", type = "b")

# Check out variances as percentages
exec_raw_var_prop <- eigen_exec_raw$values/sum(eigen_exec_raw$values)
exec_raw_var_prop

#Run an EFA using the fa() function, extracting just 1 factor.
efa1_raw <- fa(exec_raw_cor_matrix, nfactors = 1, rotate = "oblimin")
efa1_raw
#path diagram
fa.diagram(efa1_raw, digits = 2)

#Run an EFA using the fa() function, extracting with 2 factors.
efa2_raw <- fa(exec_raw_cor_matrix, nfactors = 2, rotate = "oblimin")
efa2_raw
#path diagram
fa.diagram(efa2_raw, digits = 2)

#Run an EFA using the fa() function, extracting with 3 factors.
efa3_raw <- fa(exec_raw_cor_matrix, nfactors = 3, rotate = "oblimin")
efa3_raw
#path diagram
fa.diagram(efa3_raw, digits = 2)

#Simplify the output
print(efa3_raw$loadings, cutoff = 0.3)

#calculate factor scores from efa to run further analysis on
fscores_exec_raw <- factor.scores(your_data_file_raw_vars_noNAs, efa3_raw)

#run ANOVA for the factor scores
exec_raw_data <- subset(your_data_file, select = c(ParticipantID,
                                                         Age,
                                                         Classification,
                                                         Group,
                                                         Sex,
                                                         Sex_binary,
                                                         Clinical_site,
                                                         neuropsych_var1, 
                                                         neuropsych_var2...etc))

#need to omit NA values from the dataframe
exec_raw_data_noNAs<- na.omit(exec_raw_data)
exec_raw_fs_data_noNAs <- cbind(exec_raw_data_noNAs, fscores_exec_raw$scores)
#run ANOVA with the factor 1
exec_raw_fs1_mod <- lm(MR1 ~ Group, data = exec_raw_fs_data_noNAs)
anova(exec_raw_fs1_mod)
#run ANOVA with the factor 2
exec_raw_fs2_mod <- lm(MR2 ~ Group, data = exec_raw_fs_data_noNAs)
anova(exec_raw_fs2_mod)
#run ANOVA with the factor 3
exec_raw_fs3_mod <- lm(MR3 ~ Group, data = exec_raw_fs_data_noNAs)
anova(exec_raw_fs3_mod)

#plot exec raw factor score 1
ggplot(subset(exec_raw_fs_data_noNAs, Group %in% c("1", "2", "3", "4", "5")), aes(x = Group, y = MR1, fill = Group)) + 
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(aes(y = MR1, color = Group), position = position_jitter(width = .15), size = .5, alpha = 0.8) +
  geom_boxplot(width = 0.1, fill = "white", outlier.size = 1, aes(colour = Group)) + 
  stat_summary(fun = mean, geom = "point", shape = 19, size = 2, aes(colour = Group)) + 
  xlab("Group") + 
  ylab("MR1 (factor score 1)") +
  scale_x_discrete(labels = c("1" = "Control", "2" = "SCD", "3" = "aMCI", "4" = "mMCI", "5" = "AD")) + 
  theme_classic() +
  theme(legend.position = "none") +
  coord_flip()
#plot exec raw factor score 2
ggplot(subset(exec_raw_fs_data_noNAs, Group %in% c("1", "2", "3", "4", "5")), aes(x = Group, y = MR2, fill = Group)) + 
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(aes(y = MR2, color = Group), position = position_jitter(width = .15), size = .5, alpha = 0.8) +
  geom_boxplot(width = 0.1, fill = "white", outlier.size = 1, aes(colour = Group)) + 
  stat_summary(fun = mean, geom = "point", shape = 19, size = 2, aes(colour = Group)) + 
  xlab("Group") + 
  ylab("MR2 (factor score 2)") +
  scale_x_discrete(labels = c("1" = "Control", "2" = "SCD", "3" = "aMCI", "4" = "mMCI", "5" = "AD")) + 
  theme_classic() +
  theme(legend.position = "none") +
  coord_flip()
#plot exec raw factor score 3
ggplot(subset(exec_raw_fs_data_noNAs, Group %in% c("1", "2", "3", "4", "5")), aes(x = Group, y = MR3, fill = Group)) + 
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(aes(y = MR3, color = Group), position = position_jitter(width = .15), size = .5, alpha = 0.8) +
  geom_boxplot(width = 0.1, fill = "white", outlier.size = 1, aes(colour = Group)) + 
  stat_summary(fun = mean, geom = "point", shape = 19, size = 2, aes(colour = Group)) + 
  xlab("Group") + 
  ylab("MR3 (factor score 3)") +
  scale_x_discrete(labels = c("1" = "Control", "2" = "SCD", "3" = "aMCI", "4" = "mMCI", "5" = "AD")) + 
  theme_classic() +
  theme(legend.position = "none") +
  coord_flip()


####-----------------------------MANOVAs------------------------------------####
#for raw data
manova_exec_func_raw_mod <- manova(cbind(neuropsych_var1,
                                        neuropsych_var2...etc) ~ Group, data = your_data_file) 
#overall model
summary(manova_exec_func_raw_mod)
#anova outputs
summary.aov(manova_exec_func_raw_mod)

#for z-scores
manova_exec_func_z_mod <- manova(cbind(neuropsych_zscore_var1,
                                        neuropsych_zscore_var2...etc) ~ Group, data = your_data_file) 
#overall model
summary(manova_exec_func_z_mod)
#anova outputs
summary.aov(manova_exec_func_z_mod)

#plot the manova data using the z-scores of the variables, so that it is all on the same scale. 
#put exec func variable z-scores onto a new dataset, as long format
exec_func_zscores_data <- dplyr::select(your_data_file, 
                                        ParticipantID,
                                        Group,
                                        neuropsych_zscore_var1,
                                        neuropsych_zscore_var2...etc)
#put into long format
exec_func_zscores_data_long <- gather(exec_func_zscores_data, 
                                      "Test",
                                      "Z_scores", 
                                      neuropsych_zscore_var1,
                                        neuropsych_zscore_var2...etc)

#plot data - z-score for overall 
ggplot(exec_func_zscores_data_long, aes(x = Group, y = Z_scores, fill = Group)) + 
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(aes(y = Z_scores, color = Group), position = position_jitter(width = .15), size = .5, alpha = 0.8) +
  geom_boxplot(width = 0.1, fill = "white", outlier.size = 1, aes(colour = Group)) + 
  stat_summary(fun = mean, geom = "point", shape = 19, size = 2, aes(colour = Group)) + 
  xlab("Group") + 
  ylab("Executive Functioning (Z-scores)") +
  scale_x_discrete(labels = c("1" = "Control", "2" = "SCD", "3" = "aMCI", "4" = "mMCI", "5" = "AD")) + 
  theme_classic() +
  theme(legend.position = "none") +
  coord_flip()


#for raw data - processing speeds variables
manova_proc_speed_raw_mod <- manova(cbind(neuropsych_var1
                                         neuropsych_var2...etc) ~ Group, data = your_data_file) 

#overall model
summary(manova_proc_speed_raw_mod)
#anova outputs
summary.aov(manova_proc_speed_raw_mod)

#for z-scores
manova_proc_speed_z_mod <- manova(cbind(neuropsych_zscore_var1,
                                        neuropsych_zscore_var2...etc) ~ Group, data = your_data_file) 


#overall model
summary(manova_proc_speed_z_mod, 'Wilks')
#anova outputs
summary.aov(manova_proc_speed_z_mod)
#effect size MANOVA
effectsize::eta_squared(manova_proc_speed_z_mod)
#posthoc for MANOVA
#need to make separate vectors for this
proc_speed_tests_combined <- cbind(your_data_file$neuropsych_zscore_var1, your_data_file$neuropsych_zscore_var2..etc) 
classification_group_var <- your_data_file$Group
df_lda_proc_speed <- na.omit(data.frame(your_data_file$neuropsych_zscore_var1, your_data_file$neuropsych_zscore_var2..etc)) 
#posthoc_manova_proc_speed_z_mod <- lda(df_lda_proc_speed$Group  ~ proc_speed_tests_combined, CV=F)
posthoc_manova_proc_speed_z_mod <- lda(classification_group_var ~ proc_speed_tests_combined, CV=F)
posthoc_manova_proc_speed_z_mod 
#to evaluate whether there are sig. difference between groups, you must visualise it on a plot (no value for this)
# plot 
plot_lda_proc_speed <- data.frame(df_lda_proc_speed$your_data_file.Group, lda = predict(posthoc_manova_proc_speed_z_mod)$x)
ggplot(plot_lda_proc_speed) + 
  geom_point(aes(x = lda.LD1, y = lda.LD2, colour = df_lda_proc_speed.your_data_file.Group), size = 4)+
  xlab("LD1") + 
  ylab("LD2") +
  labs(colour="Groups")+
  theme_classic()+
  theme(legend.position="none")+
  theme(axis.title.x = element_text(size = 22),axis.title.y = element_text(size = 22),axis.text.x = element_text(size = 18),axis.text.y = element_text(size = 18))
  
#scale_x_discrete(labels = c("1" = "Control", "2" = "SCD", "3" = "aMCI", "4" = "mMCI", "5" = "AD")) + #don't work
#scale_fill_discrete(labels = c("Control","SCD","aMCI","mMCI","AD")) #don't work

#make LDA plot with outlined ellipses by groups for better visibility
plot_lda_proc_speed <- data.frame(df_lda_proc_speed$your_data_file.Group, lda = predict(posthoc_manova_proc_speed_z_mod)$x)
ggplot(plot_lda_proc_speed) + 
  geom_mark_ellipse(aes(x = lda.LD1, y = lda.LD2, fill=df_lda_proc_speed.your_data_file.Group),expand=unit(0.5,"mm"),label.buffer=unit(-5,"mm"))+
  geom_point(aes(x = lda.LD1, y = lda.LD2, colour = df_lda_proc_speed.your_data_file.Group), size = 4)+
  xlab("LD1") + 
  ylab("LD2") +
  labs(colour="Groups")+
  theme_classic()+
  theme(legend.position="none")+
  theme(axis.title.x = element_text(size = 22),axis.title.y = element_text(size = 22),axis.text.x = element_text(size = 18),axis.text.y = element_text(size = 18))


#plot the manova data using the z-scores of the variables, so that it is all on the same scale. 
#put proc speed variable z-scores onto a new dataset, as long format
proc_speed_zscores_data <- dplyr::select(your_data_file, 
                                        ParticipantID,
                                        Group,
                                        neuropsych_zscore_var1
                                        neuropsych_zscore_var2...etc)
#put into long format
proc_speed_zscores_data_long <- gather(proc_speed_zscores_data, 
                                      "Processing_Speeds",
                                      "Z_scores", 
                                       neuropsych_zscore_var1
                                       neuropsych_zscore_var2...etc)


#plot data - z-score for processing speeds 
ggplot(proc_speed_zscores_data_long, aes(x = Group, y = Z_scores, fill = Group)) + 
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(aes(y = Z_scores, color = Group), position = position_jitter(width = .15), size = .5, alpha = 0.8) +
  geom_boxplot(width = 0.1, fill = "white", outlier.size = 1, aes(colour = Group)) + 
  stat_summary(fun = mean, geom = "point", shape = 19, size = 2, aes(colour = Group)) + 
  xlab("Group") + 
  ylab("Processing Speeds (Z-scores)") +
  scale_x_discrete(labels = c("1" = "Control", "2" = "SCD", "3" = "aMCI", "4" = "mMCI", "5" = "AD")) + 
  theme_classic() +
  theme(legend.position = "none") +
  theme(axis.text=element_text(size=18), axis.title=element_text(size=20))+
  coord_flip()

#get descriptives of the collated z-scores - e.g. 'processing speeds score' mean beta values
proc_speed_z_descrip <- describeBy((proc_speed_zscores_data$neuropsych_zscore_var1 + 
                                      proc_speed_zscores_data$neuropsych_zscore_var2 + ..etc
                                        ) / (length(proc_speed_zscores_data) - 2), proc_speed_zscores_data$Group)

#find mean & SD from total sample: 
all_proc_speed_zscores_data <- proc_speed_zscores_data[,3:6]
noNAsproc_speed_zscores_data <- na.omit(all_proc_speed_zscores_data)
mean(as.matrix(noNAsproc_speed_zscores_data))
sd(as.matrix(noNAsproc_speed_zscores_data))

#for inhibition variables (with example names of the neuropsych tests used for this)
manova_inhibition_raw_mod <- manova(cbind(TrailsB.Raw, 
                                         Inhibition.Raw, 
                                         Switching.Raw, 
                                         HayBTime2.Raw, 
                                         HayBCatA.Raw, 
                                         HayBCatB.Raw) ~ Group, data = your_data_file) 
#overall model
summary(manova_inhibition_raw_mod)
#anova outputs
summary.aov(manova_inhibition_raw_mod)

#for z-scores
manova_inhibition_z_mod <- manova(cbind(TrailsB.Z, 
                                       Inhibition.Z, 
                                       Switching.z, 
                                       HayBTime2.z, 
                                       HayBCatA.z, 
                                       HayBCatB.z) ~ Group, data = your_data_file) 
#overall model
summary(manova_inhibition_z_mod,'Wilks')
#anova outputs
summary.aov(manova_inhibition_z_mod)
#effect size MANOVA
effectsize::eta_squared(manova_inhibition_z_mod)
#posthoc for MANOVA
#need to make separate vectors for this
inhibition_tests_combined <- cbind(your_data_file$TrailsB.Z, your_data_file$Inhibition.Z, your_data_file$Switching.z, your_data_file$HayBTime2.z, your_data_file$HayBCatA.z, your_data_file$HayBCatB.z) 
classification_group_var <- your_data_file$Group
df_lda_inhibition <- na.omit(data.frame(your_data_file$Group,your_data_file$TrailsB.Z, your_data_file$Inhibition.Z, your_data_file$Switching.z, your_data_file$HayBTime2.z, your_data_file$HayBCatA.z, your_data_file$HayBCatB.z)) 
#posthoc_manova_inhibition_z_mod <- lda(df_lda_proc_speed$Group  ~ inhibition_tests_combined, CV=F)
posthoc_manova_inhibition_z_mod <- lda(classification_group_var ~ inhibition_tests_combined, CV=F)
posthoc_manova_inhibition_z_mod 
#to evaluate whether there are sig. difference between groups, you must visualise it on a plot (no value for this)
# plot 
plot_lda_inhibition <- data.frame(df_lda_inhibition$your_data_file.Group, lda = predict(posthoc_manova_inhibition_z_mod)$x)
ggplot(plot_lda_inhibition) + 
  geom_point(aes(x = lda.LD1, y = lda.LD2, colour = df_lda_inhibition.your_data_file.Group), size = 4)+
  xlab("LD1") + 
  ylab("LD2") +
  labs(colour="Groups")+
  theme_classic()+
  theme(legend.position="none")+
  theme(axis.title.x = element_text(size = 22),axis.title.y = element_text(size = 22),axis.text.x = element_text(size = 18),axis.text.y = element_text(size = 18))

#make LDA plot with outlined ellipses by groups for better visibility
plot_lda_inhibition <- data.frame(df_lda_inhibition$your_data_file.Group, lda = predict(posthoc_manova_inhibition_z_mod)$x)
ggplot(plot_lda_inhibition) + 
  geom_mark_ellipse(aes(x = lda.LD1, y = lda.LD2, fill=df_lda_inhibition$your_data_file.Group),expand=unit(0.5,"mm"),label.buffer=unit(-5,"mm"))+
  geom_point(aes(x = lda.LD1, y = lda.LD2, colour = df_lda_inhibition.your_data_file.Group), size = 4)+
  xlab("LD1") + 
  ylab("LD2") +
  labs(colour="Groups")+
  theme_classic()+
  theme(legend.position="none")+
  theme(axis.title.x = element_text(size = 22),axis.title.y = element_text(size = 22),axis.text.x = element_text(size = 18),axis.text.y = element_text(size = 18))


#plot the manova data using the z-scores of the variables, so that it is all on the same scale. 
#put exec func variable z-scores onto a new dataset, as long format
inhibition_zscores_data <- dplyr::select(your_data_file, 
                                        ParticipantID,
                                        Group,
                                        TrailsB.Z, 
                                        Inhibition.Z, 
                                        Switching.z,
                                        HayBTime2.z,
                                        HayBCatA.z,
                                        HayBCatB.z)
#put into long format
inhibition_zscores_data_long <- gather(inhibition_zscores_data, 
                                      "Inhibition",
                                      "Z_scores", 
                                      TrailsB.Z, 
                                      Inhibition.Z, 
                                      Switching.z, 
                                      HayBTime2.z, 
                                      HayBCatA.z, 
                                      HayBCatB.z)

#plot data - z-score for overall 
ggplot(inhibition_zscores_data_long, aes(x = Group, y = Z_scores, fill = Group)) + 
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(aes(y = Z_scores, color = Group), position = position_jitter(width = .15), size = .5, alpha = 0.8) +
  geom_boxplot(width = 0.1, fill = "white", outlier.size = 1, aes(colour = Group)) + 
  stat_summary(fun = mean, geom = "point", shape = 19, size = 2, aes(colour = Group)) + 
  xlab("Group") + 
  ylab("Inhibition (Z-scores)") +
  scale_x_discrete(labels = c("1" = "Control", "2" = "SCD", "3" = "aMCI", "4" = "mMCI", "5" = "AD")) + 
  theme_classic() +
  theme(legend.position = "none") +
  theme(axis.text=element_text(size=18), axis.title=element_text(size=20))+
  coord_flip()

#get descriptives of the collated z-scores - e.g. 'executive function score' mean beta values
inhibition_z_descrip <- describeBy((inhibition_zscores_data$TrailsB.Z + 
                                    inhibition_zscores_data$Inhibition.Z +
                                    inhibition_zscores_data$Switching.z +
                                    inhibition_zscores_data$HayBTime2.z +
                                    inhibition_zscores_data$HayBCatA.z +
                                    inhibition_zscores_data$HayBCatB.z) / (length(inhibition_zscores_data) - 2), inhibition_zscores_data$Group)
#find mean & SD from total sample: 
all_inhibition_zscores_data <- inhibition_zscores_data[,3:8]
noNAsinhibition_zscores_data <- na.omit(all_inhibition_zscores_data)
mean(as.matrix(noNAsinhibition_zscores_data))
sd(as.matrix(noNAsinhibition_zscores_data))


####---------------------------------ANOVAs---------------------------------####
#run ANOVAs on your data:

# Example from D-KEFS Stroop Task ---------------------------------------------------------#
#plot ColorNaming.Raw (raincloud plot)
ggplot(your_data_file, aes(x = Group, y = ColorNaming.Raw, fill = Group)) + 
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(aes(y = ColorNaming.Raw, color = Group), position = position_jitter(width = .15), size = .5, alpha = 0.8) +
  geom_boxplot(width = 0.1, fill = "white", outlier.size = 1, aes(colour = Group)) + 
  stat_summary(fun = mean, geom = "point", shape = 19, size = 2, aes(colour = Group)) + 
  xlab("Group") + 
  ylab("Stroop - Color Naming Raw (seconds)") +
  scale_x_discrete(labels = c("1" = "Control", "2" = "SCD", "3" = "aMCI", "4" = "mMCI", "5" = "AD")) + 
  theme_classic() +
  theme(legend.position = "none") +
  coord_flip()
#run ANOVA for ColorNaming.Raw
ColorNaming.Raw_mod <- lm(ColorNaming.Raw ~ Group, data = your_data_file)
anova(ColorNaming.Raw_mod)
#effect size omnibus ANOVA
etaSquared(ColorNaming.Raw_mod)
#run pairwise comparisons (post-hoc Tukey), given that the F-test was significant. 
post_hoc_ColorNaming.Raw_mod <- glht(ColorNaming.Raw_mod, linfct = mcp(Group = "Tukey"))
summary(post_hoc_ColorNaming.Raw_mod)
confint(post_hoc_ColorNaming.Raw_mod)
#check descriptive statistics per each group
ColorNaming.Raw_descrip <- describeBy(your_data_file$ColorNaming.Raw, your_data_file$Group)
#find mean & SD from total sample:
all_ColorNamingRaw <- your_data_file$ColorNaming.Raw
noNAsColorNamingRaw <- na.omit(all_ColorNamingRaw)
mean(noNAsColorNamingRaw)
sd(noNAsColorNamingRaw)
#effect size for sig. post hoc tests
  #for C vs. mMCI 
  your_data_file_CvmMCI_ColorNaming.Raw <- subset(your_data_file, your_data_file$Group == 1 | your_data_file$Group == 4)
  your_data_file_CvmMCI_ColorNaming.Raw$Group <- droplevels(your_data_file_CvmMCI_ColorNaming.Raw$Group)
  cohensD(ColorNaming.Raw ~ Group, data = your_data_file_CvmMCI_ColorNaming.Raw) #this looks like Hedges' g? 
  #for C vs. AD 
  your_data_file_CvAD_ColorNaming.Raw <- subset(your_data_file, your_data_file$Group == 1 | your_data_file$Group == 5)
  your_data_file_CvAD_ColorNaming.Raw$Group <- droplevels(your_data_file_CvAD_ColorNaming.Raw$Group)
  cohensD(ColorNaming.Raw ~ Group, data = your_data_file_CvAD_ColorNaming.Raw) #this looks like Hedges' g? 
  #for SCD vs. mMCI 
  your_data_file_SCDvmMCI_ColorNaming.Raw <- subset(your_data_file, your_data_file$Group == 2 | your_data_file$Group == 4)
  your_data_file_SCDvmMCI_ColorNaming.Raw$Group <- droplevels(your_data_file_SCDvmMCI_ColorNaming.Raw$Group)
  cohensD(ColorNaming.Raw ~ Group, data = your_data_file_SCDvmMCI_ColorNaming.Raw) #this looks like Hedges' g? 
  #for SCD vs. AD 
  your_data_file_SCDvAD_ColorNaming.Raw <- subset(your_data_file, your_data_file$Group == 2 | your_data_file$Group == 5)
  your_data_file_SCDvAD_ColorNaming.Raw$Group <- droplevels(your_data_file_SCDvAD_ColorNaming.Raw$Group)
  cohensD(ColorNaming.Raw ~ Group, data = your_data_file_SCDvAD_ColorNaming.Raw) #this looks like Hedges' g? 
  #for aMCI vs. AD 
  your_data_file_aMCIvAD_ColorNaming.Raw <- subset(your_data_file, your_data_file$Group == 3 | your_data_file$Group == 5)
  your_data_file_aMCIvAD_ColorNaming.Raw$Group <- droplevels(your_data_file_aMCIvAD_ColorNaming.Raw$Group)
  cohensD(ColorNaming.Raw ~ Group, data = your_data_file_aMCIvAD_ColorNaming.Raw) #this looks like Hedges' g? 
#run Bayesian ANOVA
For_Bay_data_noNas_neuropsych_ColorNaming.Raw <- dplyr::select(your_data_file, ParticipantID, Group, ColorNaming.Raw)
For_Bay_data_noNas_neuropsych_ColorNaming.Raw <- na.omit(For_Bay_data_noNas_neuropsych_ColorNaming.Raw)
anovaBF(ColorNaming.Raw ~ Group, data = For_Bay_data_noNas_neuropsych_ColorNaming.Raw) 
#add in Linear Trend Analysis in Linear Regression
ColorNamingRaw_LinTrend_mod <- lm(ColorNaming.Raw ~ Trend_Group + Group, data = your_data_file)
anova(ColorNamingRaw_LinTrend_mod)
summary(ColorNamingRaw_LinTrend_mod) 

#####----------------ANCOVA - test with covariates (age)---------------------########
#Example from D-KEFS Stroop Task ----------------------------------------------------------------#
#run ANCOVA for ColourNaming.Raw
ColourNaming.Raw_covar_mod <- lm(ColorNaming.Raw ~ Group+Age, data = your_data_file)
anova(ColourNaming.Raw_covar_mod)
#effect size omnibus ANOVA
etaSquared(ColourNaming.Raw_covar_mod)
#run pairwise comparisons (post-hoc Tukey), given that the F-test was significant. 
post_hoc_ColourNaming.Raw_covar_mod <- glht(ColourNaming.Raw_covar_mod, linfct = mcp(Group = "Tukey"))
summary(post_hoc_ColourNaming.Raw_covar_mod) 
confint(post_hoc_ColourNaming.Raw_covar_mod)
t_value_effect_size <- summary(post_hoc_ColourNaming.Raw_covar_mod) 
#effect size with covariate
  #for C vs. AD
  your_data_file_CvAD_ColorNaming.Raw <- subset(your_data_file, your_data_file$Group == 1 | your_data_file$Group == 5)
  your_data_file_CvAD_ColorNaming.Raw$Group <- droplevels(your_data_file_CvAD_ColorNaming.Raw$Group)
  group_number <-dplyr::count(your_data_file_CvAD_ColorNaming.Raw, Group) #count number of participants per group
  r_value <- cor.test(your_data_file_CvAD_ColorNaming.Raw$ColorNaming.Raw,your_data_file_CvAD_ColorNaming.Raw$Age) #find correlation value (r) between dependent variable
  a.tes(t=t_value_effect_size$test$tstat['5 - 1'],n.1=group_number['1','n'],n.2=group_number['2','n'],R=r_value$estimate,q=1) #calculate Cohen's D with the covariate of age
  #for SCD vs. AD
  your_data_file_SCDvAD_ColorNaming.Raw <- subset(your_data_file, your_data_file$Group == 2 | your_data_file$Group == 5)
  your_data_file_SCDvAD_ColorNaming.Raw$Group <- droplevels(your_data_file_SCDvAD_ColorNaming.Raw$Group)
  group_number <-dplyr::count(your_data_file_SCDvAD_ColorNaming.Raw, Group) #count number of participants per group
  r_value <- cor.test(your_data_file_SCDvAD_ColorNaming.Raw$ColorNaming.Raw,your_data_file_SCDvAD_ColorNaming.Raw$Age) #find correlation value (r) between dependent variable
  a.tes(t=t_value_effect_size$test$tstat['5 - 2'],n.1=group_number['1','n'],n.2=group_number['2','n'],R=r_value$estimate,q=1) #calculate Cohen's D with the covariate of age
#add in Linear Trend Analysis in Linear Regression
ColorNamingRaw_LinTrend_covar_mod <- lm(ColorNaming.Raw ~ Trend_Group + Group + Age, data = your_data_file)
anova(ColorNamingRaw_LinTrend_covar_mod)
summary(ColorNamingRaw_LinTrend_covar_mod) 



#----------ANCOVA - test with covariates (age & sex)---------------------------------#####
#Example from D-KEFS Stroop Test ----------------------------------------------------------------#
#run ANCOVA for ColourNaming.Raw
ColourNaming.Raw_2covar_mod <- lm(ColorNaming.Raw ~ Group+Age+Sex, data = your_data_file)
anova(ColourNaming.Raw_2covar_mod)
#effect size omnibus ANOVA
etaSquared(ColourNaming.Raw_2covar_mod)
#run pairwise comparisons (post-hoc Tukey), given that the F-test was significant. 
post_hoc_ColourNaming.Raw_2covar_mod <- glht(ColourNaming.Raw_2covar_mod, linfct = mcp(Group = "Tukey"))
summary(post_hoc_ColourNaming.Raw_2covar_mod) 
confint(post_hoc_ColourNaming.Raw_2covar_mod)
t_value_effect_size <- summary(post_hoc_ColourNaming.Raw_2covar_mod) 
#effect size with covariate
  #for SCD vs. AD
  your_data_file_SCDvAD_ColourNaming.Raw <- subset(your_data_file, your_data_file$Group == 2 | your_data_file$Group == 5)
  your_data_file_SCDvAD_ColourNaming.Raw$Group <- droplevels(your_data_file_SCDvAD_ColourNaming.Raw$Group)
  group_number <-dplyr::count(your_data_file_SCDvAD_ColourNaming.Raw, Group) #count number of participants per group
  mult.r_value_2covar_mod<-summary(lm(ColorNaming.Raw ~ Age + Sex, data = your_data_file_SCDvAD_ColourNaming.Raw)) #create multiple regression between age, sex, and y-var, and get square root of mult-r squared as the r-value
  r_value <- sqrt(mult.r_value_2covar_mod$r.squared) #find correlation value (r) between dependent variable
  a.tes(t=t_value_effect_size$test$tstat['5 - 2'],n.1=group_number['1','n'],n.2=group_number['2','n'],R=r_value,q=2) #calculate Cohen's D with the covariate of age & sex
#add in Linear Trend Analysis in Linear Regression
ColorNamingRaw_LinTrend_2covar_mod <- lm(ColorNaming.Raw ~ Trend_Group + Group+Age+Sex, data = your_data_file)
anova(ColorNamingRaw_LinTrend_2covar_mod)
summary(ColorNamingRaw_LinTrend_2covar_mod) 














#######----------------- Longitudinal (F0 vs. F2) analysis -----------------########

#read in csv files (participant file)
your_data_file <- read.csv("longitudinal_your_data_file.csv")

#rename first column 
colnames(your_data_file)[1] <-'ParticipantID'

#Add in longitudinal values for participants
Individual_number <- c(1:124, 1:124)
#Individual_number <- c(1:119, 1:119)
your_data_file$Individual_number <- Individual_number

#convert variables
your_data_file$ParticipantID <- as.factor(your_data_file$ParticipantID)
your_data_file$Group <- as.factor(your_data_file$Group)
your_data_file$Sex <- as.factor(your_data_file$Sex)
your_data_file$Sex_binary <- as.factor(your_data_file$Sex_binary)
your_data_file$Timepoint <- as.factor(your_data_file$Timepoint)
your_data_file$Individual_number <- as.factor(your_data_file$Individual_number)


#----------------plot data to visualise & run stat tests ----------------------#

#longitudinal example for age (F0 vs. F2)
ggplot(your_data_file, aes(x = Group, y = Age, group=interaction(Group, Timepoint))) + 
  geom_boxplot(width = 0.1, fill = "white", outlier.size = 1, aes(colour = Group), position = position_dodge(.9)) + 
  stat_summary(fun = mean, geom = "point", shape = 19, size = 2, aes(colour = Group), position = position_dodge(.9)) + 
  ylim(50, 95) +
  xlab("Group") + 
  ylab("Age") +
  scale_x_discrete(labels = c("1" = "Control", "2" = "SCD", "3" = "aMCI", "4" = "mMCI", "5" = "AD")) + 
  theme_classic() +
  theme(legend.position = "none") +
  geom_violin(trim = FALSE, alpha = .5, aes(fill = Group, colour = Group), size = 1)

#take a subset of the DPRC data - only baseline data (F0) for certain graphs or analysis
baseline_DPRC_neuropsych <- your_data_file[ which(your_data_file$Timepoint=='F0'), ]


##---- Longitudinal visualisation: ------------------------------------------## 
exec_func_long_zscores_data <- dplyr::select(your_data_file, 
                                        ParticipantID,
                                        Group,
                                        Age,
                                        Timepoint,
                                 neuropsych_zscore1,
                                 neuropsych_zscore2...etc)

#for processing speed -- put into long format
proc_speed_long_zscores_data_long <- gather(exec_func_long_zscores_data, 
                                       "Processing_Speeds",
                                       "Z_scores", 
                                       TrailsA.Z, 
                                       ColorNaming.Z, 
                                       WordReading.Z, 
                                       HayBTime1.z)

#plot data - z-score for processing speeds 
ggplot(proc_speed_long_zscores_data_long, aes(x = Group, y = Z_scores, fill = Timepoint)) + 
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(aes(y = Z_scores, color = Timepoint), position = position_jitter(width = .15), size = .5, alpha = 0.8) +
  geom_boxplot(width = 0.1, fill = "white", outlier.size = 1, aes(colour = Timepoint)) + 
  stat_summary(fun = mean, geom = "point", shape = 19, size = 2, aes(colour = Timepoint)) + 
  xlab("Group") + 
  ylab("Processing Speeds (Z-scores)") +
  scale_x_discrete(labels = c("1" = "Control", "2" = "SCD", "3" = "aMCI", "4" = "mMCI", "5" = "AD")) + 
  theme_classic() +
  theme(legend.position = "none") +
  coord_flip()

#Run MANCOVA for processing speed - corrected for age
mancova_proc_speed_long_z_mod <- manova(cbind(TrailsA.Z, 
                                        ColorNaming.Z, 
                                        WordReading.Z, 
                                        HayBTime1.z) ~ Group*Timepoint+Age+Error(Individual_number/Timepoint), data = your_data_file) 
#overall model
summary(mancova_proc_speed_long_z_mod)

# #another way, but doesn't take individual_number into account; also doesn't seem like combination of group and timepoint is correct for 'ina' argument
# maov_proc_speed_zscores <- maov(na.omit(cbind(your_data_file$TrailsA.Z, 
#                                       your_data_file$ColorNaming.Z, 
#                                       your_data_file$WordReading.Z, 
#                                       your_data_file$HayBTime1.z)), ina = cbind(your_data_file$Group,your_data_file$Timepoint))

#for inhibition: put into long format
inhibition_zscores_data_long <- gather(exec_func_long_zscores_data, 
                                       "Inhibition",
                                       "Z_scores", 
                                       TrailsB.Z, 
                                       Inhibition.Z, 
                                       Switching.z, 
                                       HayBTime2.z, 
                                       HayBCatA.z, 
                                       HayBCatB.z)
#plot data - z-score for overall 
ggplot(inhibition_zscores_data_long, aes(x = Group, y = Z_scores, fill = Timepoint)) + 
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(aes(y = Z_scores, color = Timepoint), position = position_jitter(width = .15), size = .5, alpha = 0.8) +
  geom_boxplot(width = 0.1, fill = "white", outlier.size = 1, aes(colour = Timepoint)) + 
  stat_summary(fun = mean, geom = "point", shape = 19, size = 2, aes(colour = Timepoint)) + 
  xlab("Group") + 
  ylab("Inhibition (Z-scores)") +
  scale_x_discrete(labels = c("1" = "Control", "2" = "SCD", "3" = "aMCI", "4" = "mMCI", "5" = "AD")) + 
  theme_classic() +
  theme(legend.position = "none") +
  coord_flip()

#Run MANCOVA for inhibition - corrected for age
mancova_inhibition_long_z_mod <- manova(cbind(TrailsB.Z, 
                                              Inhibition.Z, 
                                              Switching.z, 
                                              HayBTime2.z, 
                                              HayBCatA.z, 
                                              HayBCatB.z) ~ Group*Timepoint+Age+Error(Individual_number/Timepoint), data = your_data_file) 
#overall model
summary(mancova_inhibition_long_z_mod)




####---------------------------------ANOVAs---------------------------------####
#run ANOVAs on your data:
#1.Test of premorbid functioning (TOPF) ---------------------------------------#
#plot TOPF.Raw (raincloud plot) - only for baseline
ggplot(baseline_DPRC_neuropsych, aes(x = Group, y = TOPF.Raw, fill = Group)) + 
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(aes(y = TOPF.Raw, color = Group), position = position_jitter(width = .15), size = .5, alpha = 0.8) +
  geom_boxplot(width = 0.1, fill = "white", outlier.size = 1, aes(colour = Group)) + 
  stat_summary(fun = mean, geom = "point", shape = 19, size = 2, aes(colour = Group)) + 
  xlab("Group") + 
  ylab("TOPF Score (Raw)") +
  scale_x_discrete(labels = c("1" = "Control", "2" = "SCD", "3" = "aMCI", "4" = "mMCI", "5" = "AD")) + 
  theme_classic() +
  theme(legend.position = "none") +
  coord_flip()
#run ANOVA for TOPF.Raw
TOPF.Raw_mod <- lm(TOPF.Raw ~ Group, data = baseline_DPRC_neuropsych)
anova(TOPF.Raw_mod)
#check descriptive statistics per each group
TOPF.Raw_descrip <- describeBy(baseline_DPRC_neuropsych$TOPF.Raw, baseline_DPRC_neuropsych$Group)
#whole sample descriptives
noNAs_TOPF.Raw_baseline_DPRC_neuropsych <- na.omit(baseline_DPRC_neuropsych$TOPF.Raw)
mean(noNAs_TOPF.Raw_baseline_DPRC_neuropsych)
sd(noNAs_TOPF.Raw_baseline_DPRC_neuropsych)

#plot TOPF.Z (raincloud plot)
ggplot(baseline_DPRC_neuropsych, aes(x = Group, y = TOPF.Z, fill = Group)) + 
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(aes(y = TOPF.Z, color = Group), position = position_jitter(width = .15), size = .5, alpha = 0.8) +
  geom_boxplot(width = 0.1, fill = "white", outlier.size = 1, aes(colour = Group)) + 
  stat_summary(fun = mean, geom = "point", shape = 19, size = 2, aes(colour = Group)) + 
  xlab("Group") + 
  ylab("TOPF Score (z-scores)") +
  scale_x_discrete(labels = c("1" = "Control", "2" = "SCD", "3" = "aMCI", "4" = "mMCI", "5" = "AD")) + 
  theme_classic() +
  theme(legend.position = "none") +
  coord_flip()
#run ANOVA for TOPF.Z
TOPF.Z_mod <- lm(TOPF.Z ~ Group, data = baseline_DPRC_neuropsych)
anova(TOPF.Z_mod)

#2.Hayling Sentence completion test -------------------------------------------#
#plot HayBTime1.Raw (raincloud plot)
ggplot(your_data_file, aes(x = Group, y = HayBTime1.Raw, fill = Timepoint)) + 
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(aes(y = HayBTime1.Raw, color = Timepoint), position = position_jitter(width = .15), size = .5, alpha = 0.8) +
  geom_boxplot(width = 0.1, fill = "white", outlier.size = 1, aes(colour = Timepoint)) + 
  stat_summary(fun = mean, geom = "point", shape = 19, size = 2, aes(colour = Timepoint)) + 
  xlab("Group") + 
  ylab("Hayling Sentence Set 1 Time Raw (seconds)") +
  scale_x_discrete(labels = c("1" = "Control", "2" = "SCD", "3" = "aMCI", "4" = "mMCI", "5" = "AD")) + 
  theme_classic() +
  coord_flip()

#run mixed design, 2 x 5 ANOVA for HayBTime1.Raw
#aov_HayBTime1Raw <- aov(HayBTime1.Raw ~ Group*Timepoint + Error(ParticipantID/Timepoint), data = your_data_file)
#summary(aov_HayBTime1Raw)
#aov_HayBTime1Raw <- Anova(lm(HayBTime1.Raw ~ Group*Timepoint, data=your_data_file), type = "III") #use this anova test to account for unbalanced designs/sample sizes
#aov_HayBTime1Raw
aov_HayBTime1Raw <- anova_test(data=your_data_file, dv=HayBTime1.Raw, wid=Individual_number, between=Group, within=Timepoint, effect.size = "pes")
get_anova_table(aov_HayBTime1Raw)
#effect size
#eta_squared(aov_HayBTime1Raw)
#check descriptive statistics per each group, per each timepoint
HayBTime1.Raw_descrip <- describeBy(your_data_file$HayBTime1.Raw, list(your_data_file$Group, your_data_file$Timepoint))
#find mean & SD from total sample in F0 and F2 timepoints:
F0_HayBTime1Raw <- your_data_file[your_data_file[, "Timepoint"] == "F0",]
noNAsF0_HayBTime1Raw <- na.omit(F0_HayBTime1Raw$HayBTime1.Raw)
mean(noNAsF0_HayBTime1Raw)
sd(noNAsF0_HayBTime1Raw)
#F2
F2_HayBTime1Raw <- your_data_file[your_data_file[, "Timepoint"] == "F2",]
noNAsF2_HayBTime1Raw <- na.omit(F2_HayBTime1Raw$HayBTime1.Raw)
mean(noNAsF2_HayBTime1Raw)
sd(noNAsF2_HayBTime1Raw)
#post hoc test
#compare main effects - Group
#GroupME_HayBTime1 <- emmeans(aov_HayBTime1Raw, ~ Group)
#pairs(GroupME_HayBTime1ME)
#compare main effects - Timepoint
#TimepointME_HayBTime1 <- emmeans(aov_HayBTime1Raw, ~ Timepoint)
#pairs(TimepointME_HayBTime1ME)
#check mean values
#lsmeans(aov_HayBTime1Raw, pairwise ~ Group | Timepoint)
#lsmeans(aov_HayBTime1Raw, pairwise ~ Timepoint | Group)
#lsmeans(aov_HayBTime1Raw, c("Group", "Timepoint"))
#run linear mixed modelling version of ANOVA & posthoc
#lmeHayBTime1mod <- lmer(HayBTime1.Raw ~ Group + (1|Timepoint), data = your_data_file, REML=TRUE)
#anova(lmeHayBTime1mod)
#Group lme posthoc
#posthoc_Group_lmeHayBTime1mod <- glht(lmeHayBTime1mod, linfct=mcp(Group ="Tukey"))
#summary(posthoc_Group_lmeHayBTime1mod)
#remove NAs from dataset for given variable
#noNAs_HayBTime1Raw <- your_data_file[,c("ParticipantID","Group","Timepoint","Age","Sex","HayBTime1.Raw")]
#noNAs_HayBTime1Raw <- noNAs_HayBTime1Raw[complete.cases(noNAs_HayBTime1Raw), ]
#run post hoc w/ Tukey correction
#post_hoc_aov_HayBTime1Raw_mod <- lme(HayBTime1.Raw ~ Group*Timepoint, random = ~1 | ParticipantID/Timepoint, data=noNAs_HayBTime1Raw)
#summary(glht(post_hoc_aov_HayBTime1Raw_mod, linfct=mcp(Timepoint="Tukey")))
#nlme::intervals(post_hoc_aov_HayBTime1Raw_mod, level=0.95) #not working
#summary(glht(post_hoc_aov_HayBTime1Raw_mod, linfct=mcp(Group="Tukey")))
#non-sig. interaction - test by Group (FDR)
# your_data_file %>%
#   pairwise_t_test(
#     HayBTime1.Raw ~ Group, 
#     p.adjust.method = "fdr"
#   )
#non-sig. interaction & Timepoint, Test by Group (Tukey test)
aov(HayBTime1.Raw ~ Group, data = your_data_file) %>% tukey_hsd()
#effect size for Groups
your_data_file%>%cohens_d(HayBTime1.Raw~Group)

#plot HayBTime1.z (raincloud plot)
ggplot(your_data_file, aes(x = Group, y = HayBTime1.z, fill = Timepoint)) + 
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(aes(y = HayBTime1.z, color = Timepoint), position = position_jitter(width = .15), size = .5, alpha = 0.8) +
  geom_boxplot(width = 0.1, fill = "white", outlier.size = 1, aes(colour = Timepoint)) + 
  stat_summary(fun = mean, geom = "point", shape = 19, size = 2, aes(colour = Timepoint)) + 
  xlab("Group") + 
  ylab("Hayling Sentence Set 1 Z-scores") +
  scale_x_discrete(labels = c("1" = "Control", "2" = "SCD", "3" = "aMCI", "4" = "mMCI", "5" = "AD")) + 
  theme_classic() +
  coord_flip()
#run mixed design, 2 x 5 ANOVA for HayBTime1.z
aov_HayBTime1z <- anova_test(data=your_data_file, dv=HayBTime1.z, wid=Individual_number, between=Group, within=Timepoint, effect.size = "pes")
get_anova_table(aov_HayBTime1z)
#descriptives
HayBTime1.z_descrip <- describeBy(your_data_file$HayBTime1.z, list(your_data_file$Group, your_data_file$Timepoint))
#find mean & SD from total sample in F0 and F2 timepoints:
F0_HayBTime1z <- your_data_file[your_data_file[, "Timepoint"] == "F0",]
noNAsF0_HayBTime1z <- na.omit(F0_HayBTime1z$HayBTime1.z)
mean(noNAsF0_HayBTime1z)
sd(noNAsF0_HayBTime1z)
#F2
F2_HayBTime1z <- your_data_file[your_data_file[, "Timepoint"] == "F2",]
noNAsF2_HayBTime1z <- na.omit(F2_HayBTime1z$HayBTime1.z)
mean(noNAsF2_HayBTime1z)
sd(noNAsF2_HayBTime1z)
#non-sig. interaction & Timepoint, Test by Group (Tukey test)
aov(HayBTime1.z ~ Group, data = your_data_file) %>% tukey_hsd()
#effect size for Groups
your_data_file%>%cohens_d(HayBTime1.z~Group)

#plot HayBTime2.Raw (raincloud plot)
ggplot(your_data_file, aes(x = Group, y = HayBTime2.Raw, fill = Timepoint)) + 
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(aes(y = HayBTime2.Raw, color = Timepoint), position = position_jitter(width = .15), size = .5, alpha = 0.8) +
  geom_boxplot(width = 0.1, fill = "white", outlier.size = 1, aes(colour = Timepoint)) + 
  stat_summary(fun = mean, geom = "point", shape = 19, size = 2, aes(colour = Timepoint)) + 
  xlab("Group") + 
  ylab("Hayling Sentence Set 2 Time Raw (seconds)") +
  scale_x_discrete(labels = c("1" = "Control", "2" = "SCD", "3" = "aMCI", "4" = "mMCI", "5" = "AD")) + 
  theme_classic() +
  coord_flip()
#run mixed design, 2 x 5 ANOVA for HayBTime2.Raw
aov_HayBTime2Raw <- anova_test(data=your_data_file, dv=HayBTime2.Raw, wid=Individual_number, between=Group, within=Timepoint, effect.size = "pes")
get_anova_table(aov_HayBTime2Raw)
#check descriptive statistics per each group, per each timepoint
HayBTime2.Raw_descrip <- describeBy(your_data_file$HayBTime2.Raw, list(your_data_file$Group, your_data_file$Timepoint))
#find mean & SD from total sample in F0 and F2 timepoints:
F0_HayBTime2Raw <- your_data_file[your_data_file[, "Timepoint"] == "F0",]
noNAsF0_HayBTime2Raw <- na.omit(F0_HayBTime2Raw$HayBTime2.Raw)
mean(noNAsF0_HayBTime2Raw)
sd(noNAsF0_HayBTime2Raw)
#F2
F2_HayBTime2Raw <- your_data_file[your_data_file[, "Timepoint"] == "F2",]
noNAsF2_HayBTime2Raw <- na.omit(F2_HayBTime2Raw$HayBTime2.Raw)
mean(noNAsF2_HayBTime2Raw)
sd(noNAsF2_HayBTime2Raw)
#Post hoc tests
#non-sig. interaction & Timepoint, Test by Group (Tukey test)
aov(HayBTime2.Raw ~ Group, data = your_data_file) %>% tukey_hsd()
#effect size for Groups
your_data_file%>%cohens_d(HayBTime2.Raw~Group)
#plot interaction 
#remove NAs for HayBTime2.Raw variable
noNAs_HayBTime2.Raw <- your_data_file[,c("ParticipantID","Individual_number","Group","Timepoint","Age","Sex","HayBTime2.Raw")]
noNAs_HayBTime2.Raw<- noNAs_HayBTime2.Raw[complete.cases(noNAs_HayBTime2.Raw), ]
#view interaction plot - by Group
noNAs_HayBTime2.Raw%>%
  group_by(Group,Timepoint) %>%
  summarise(s_mean=mean(HayBTime2.Raw)) %>%
  ggplot(aes(y=s_mean,x=Group,colour=Timepoint,group=Timepoint))+
  geom_point()+geom_line()+
  scale_x_discrete(labels = c("1" = "C", "2" = "SCD", "3" = "aMCI", "4" = "mMCI", "5" = "AD")) + 
  ylab("Hayling Incongruent Sentence Set 2 Time Raw (seconds)") +
  theme_classic()
#view interaction plot - by Timepoint
noNAs_HayBTime2.Raw %>%
  group_by(Group,Timepoint) %>%
  summarise(s_mean=mean(HayBTime2.Raw)) %>%
  ggplot(aes(y=s_mean,x=Timepoint,colour=Group,group=Group))+
  geom_point()+geom_line()+
  scale_color_discrete(labels = c("1" = "C", "2" = "SCD", "3" = "aMCI", "4" = "mMCI", "5" = "AD")) + 
  ylab("Hayling Incongruent Sentence Set 2 Time Raw (seconds)") +
  theme_classic()
#note that if these interaction plots are not working, just try restarting R (just exit and open again)
#interaction f/u tests -- HayBTime2
#subtract differences in F2 - F0 for HayBTime2 
HayBTime2.Raw_diff_data<-your_data_file[,c("ParticipantID","Individual_number","Group","Timepoint","Age","Sex","HayBTime2.Raw")]
F0_HayBTime2.Raw_data<-HayBTime2.Raw_diff_data[HayBTime2.Raw_diff_data$Timepoint=='F0', ]
F2_HayBTime2.Raw_data<-HayBTime2.Raw_diff_data[HayBTime2.Raw_diff_data$Timepoint=='F2', ]
HayBTime2.RawDiff<-F2_HayBTime2.Raw_data$HayBTime2.Raw - F0_HayBTime2.Raw_data$HayBTime2.Raw
F0_HayBTime2.Raw_data$HayBTime2.RawDiff<-HayBTime2.RawDiff
#run one-way ANOVA on the F2-F0 differences in HayTime2.HayTime1_minus between groups
HayBTime2.RawDiff_mod <- lm(HayBTime2.RawDiff ~ Group, data = F0_HayBTime2.Raw_data)
anova(HayBTime2.RawDiff_mod) #sig. difference
#effect size omnibus ANOVA
etaSquared(HayBTime2.RawDiff_mod)
#post hoc f/u test
#run pairwise comparisons (post-hoc Tukey), given that the F-test was significant. 
post_hoc_HayBTime2.RawDiff_mod <- glht(HayBTime2.RawDiff_mod, linfct = mcp(Group = "Tukey"))
summary(post_hoc_HayBTime2.RawDiff_mod)
confint(post_hoc_HayBTime2.RawDiff_mod)
#effect size for Interaction
  #create dataframe
  #HayBTime2.Raw_diff_data_124 <- your_data_file[1:124,c("ParticipantID","Individual_number","Group","Timepoint","Age","Sex","HayBTime2.Raw")]
  HayBTime2.Raw_diff_data_124 <- your_data_file[1:119,c("ParticipantID","Individual_number","Group","Timepoint","Age","Sex","HayBTime2.Raw")]
  HayBTime2.Raw_diff_data_124$HayBTime2.RawDiff <- HayBTime2.RawDiff 
  #calculate effect size
  HayBTime2.Raw_diff_data_124%>%cohens_d(HayBTime2.RawDiff~Group)

#plot HayBTime2.z (raincloud plot)
ggplot(your_data_file, aes(x = Group, y = HayBTime2.z, fill = Timepoint)) + 
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(aes(y = HayBTime2.z, color = Timepoint), position = position_jitter(width = .15), size = .5, alpha = 0.8) +
  geom_boxplot(width = 0.1, fill = "white", outlier.size = 1, aes(colour = Timepoint)) + 
  stat_summary(fun = mean, geom = "point", shape = 19, size = 2, aes(colour = Timepoint)) + 
  xlab("Group") + 
  ylab("Hayling Sentence Set 2 Z-scores") +
  scale_x_discrete(labels = c("1" = "Control", "2" = "SCD", "3" = "aMCI", "4" = "mMCI", "5" = "AD")) + 
  theme_classic() +
  coord_flip()
#run mixed design, 2 x 5 ANOVA for HayBTime2.z
aov_HayBTime2z <- anova_test(data=your_data_file, dv=HayBTime2.z, wid=Individual_number, between=Group, within=Timepoint, effect.size = "pes")
get_anova_table(aov_HayBTime2z)
#descriptives
HayBTime2.z_descrip <- describeBy(your_data_file$HayBTime2.z, list(your_data_file$Group, your_data_file$Timepoint))
#find mean & SD from total sample in F0 and F2 timepoints:
F0_HayBTime2z <- your_data_file[your_data_file[, "Timepoint"] == "F0",]
noNAsF0_HayBTime2z <- na.omit(F0_HayBTime2z$HayBTime2.z)
mean(noNAsF0_HayBTime2z)
sd(noNAsF0_HayBTime2z)
#F2
F2_HayBTime2z <- your_data_file[your_data_file[, "Timepoint"] == "F2",]
noNAsF2_HayBTime2z <- na.omit(F2_HayBTime2z$HayBTime2.z)
mean(noNAsF2_HayBTime2z)
sd(noNAsF2_HayBTime2z)
#non-sig. interaction & Timepoint, Test by Group (Tukey test)
aov(HayBTime2.z ~ Group, data = your_data_file) %>% tukey_hsd()
#effect size for Groups
your_data_file%>%cohens_d(HayBTime2.z~Group)
#interaction f/u tests -- HayBTime2
#subtract differences in F2 - F0 for HayBTime2
HayBTime2.z_diff_data<-your_data_file[,c("ParticipantID","Individual_number","Group","Timepoint","Age","Sex","HayBTime2.z")]
F0_HayBTime2.z_data<-HayBTime2.z_diff_data[HayBTime2.z_diff_data$Timepoint=='F0', ]
F2_HayBTime2.z_data<-HayBTime2.z_diff_data[HayBTime2.z_diff_data$Timepoint=='F2', ]
HayBTime2.zDiff<-F2_HayBTime2.z_data$HayBTime2.z - F0_HayBTime2.z_data$HayBTime2.z
F0_HayBTime2.z_data$HayBTime2.zDiff<-HayBTime2.zDiff
#run one-way ANOVA on the F2-F0 differences in HayTime2.HayTime1_minus between groups
HayBTime2.zDiff_mod <- lm(HayBTime2.zDiff ~ Group, data = F0_HayBTime2.z_data)
anova(HayBTime2.zDiff_mod) #sig. difference
#effect size omnibus ANOVA
etaSquared(HayBTime2.zDiff_mod)
#post hoc f/u test
#run pairwise comparisons (post-hoc Tukey), given that the F-test was significant.
post_hoc_HayBTime2.zDiff_mod <- glht(HayBTime2.zDiff_mod, linfct = mcp(Group = "Tukey"))
summary(post_hoc_HayBTime2.zDiff_mod)
confint(post_hoc_HayBTime2.zDiff_mod)
#effect size for Interaction
  #create dataframe
  #HayBTime2.z_diff_data_124 <- your_data_file[1:124,c("ParticipantID","Individual_number","Group","Timepoint","Age","Sex","HayBTime2.z")]
  HayBTime2.z_diff_data_124 <- your_data_file[1:119,c("ParticipantID","Individual_number","Group","Timepoint","Age","Sex","HayBTime2.z")]
  HayBTime2.z_diff_data_124$HayBTime2.zDiff <- HayBTime2.zDiff
  #calculate effect size
  HayBTime2.z_diff_data_124%>%cohens_d(HayBTime2.zDiff~Group)

#plot HayTime2.HayTime1_minus (raincloud plot)
ggplot(your_data_file, aes(x = Group, y = HayTime2.HayTime1_minus, fill = Timepoint)) + 
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(aes(y = HayTime2.HayTime1_minus, color = Timepoint), position = position_jitter(width = .15), size = .5, alpha = 0.8) +
  geom_boxplot(width = 0.1, fill = "white", outlier.size = 1, aes(colour = Timepoint)) + 
  stat_summary(fun = mean, geom = "point", shape = 19, size = 2, aes(colour = Timepoint)) + 
  xlab("Group") + 
  ylab("Hayling Sentence Set 2 Time Minus Hayling Sentence Set 1 (seconds)") +
  scale_x_discrete(labels = c("1" = "Control", "2" = "SCD", "3" = "aMCI", "4" = "mMCI", "5" = "AD")) + 
  theme_classic() +
  coord_flip()
#run mixed design, 2 x 5 ANOVA for HayTime2.HayTime1_minus
aov_HayTime2.HayTime1_minus <- anova_test(data=your_data_file, dv=HayTime2.HayTime1_minus, wid=Individual_number, between=Group, within=Timepoint, effect.size = "pes")
get_anova_table(aov_HayTime2.HayTime1_minus)
#descriptives
HayTime2.HayTime1_minus_descrip <- describeBy(your_data_file$HayTime2.HayTime1_minus, list(your_data_file$Group, your_data_file$Timepoint))
#find mean & SD from total sample in F0 and F2 timepoints:
F0_HayTime2.HayTime1_minus <- your_data_file[your_data_file[, "Timepoint"] == "F0",]
noNAsF0_HayTime2.HayTime1_minus <- na.omit(F0_HayTime2.HayTime1_minus$HayTime2.HayTime1_minus)
mean(noNAsF0_HayTime2.HayTime1_minus)
sd(noNAsF0_HayTime2.HayTime1_minus)
#F2
F2_HayTime2.HayTime1_minus <- your_data_file[your_data_file[, "Timepoint"] == "F2",]
noNAsF2_HayTime2.HayTime1_minus <- na.omit(F2_HayTime2.HayTime1_minus$HayTime2.HayTime1_minus)
mean(noNAsF2_HayTime2.HayTime1_minus)
sd(noNAsF2_HayTime2.HayTime1_minus)
#Post hoc tests
#non-sig. interaction & Timepoint, Test by Group (Tukey test)
aov(HayTime2.HayTime1_minus ~ Group, data = your_data_file) %>% tukey_hsd()
#effect size for Groups
your_data_file%>%cohens_d(HayTime2.HayTime1_minus~Group)
#plot interaction 
#remove NAs for HayTime2.HayTime1_minus variable
noNAs_HayTime2.HayTime1_minus <- your_data_file[,c("ParticipantID","Individual_number","Group","Timepoint","Age","Sex","HayTime2.HayTime1_minus")]
noNAs_HayTime2.HayTime1_minus<- noNAs_HayTime2.HayTime1_minus[complete.cases(noNAs_HayTime2.HayTime1_minus), ]
#view interaction plot - by Group
noNAs_HayTime2.HayTime1_minus%>%
  group_by(Group,Timepoint) %>%
  summarise(s_mean=mean(HayTime2.HayTime1_minus)) %>%
  ggplot(aes(y=s_mean,x=Group,colour=Timepoint,group=Timepoint))+
  geom_point()+geom_line()+
  scale_x_discrete(labels = c("1" = "C", "2" = "SCD", "3" = "aMCI", "4" = "mMCI", "5" = "AD")) + 
  ylab("Hayling Sentence 2 Minus Hayling Sentence 1 (seconds)") +
  theme_classic()
#view interaction plot - by Timepoint
noNAs_HayTime2.HayTime1_minus %>%
  group_by(Group,Timepoint) %>%
  summarise(s_mean=mean(HayTime2.HayTime1_minus)) %>%
  ggplot(aes(y=s_mean,x=Timepoint,colour=Group,group=Group))+
  geom_point()+geom_line()+
  scale_color_discrete(labels = c("1" = "C", "2" = "SCD", "3" = "aMCI", "4" = "mMCI", "5" = "AD")) + 
  ylab("Hayling Sentence 2 Minus Hayling Sentence 1 (seconds)") +
  theme_classic()
#interaction f/u tests -- HayBTime2
#subtract differences in F2 - F0 for HayBTime2 
HayTime2.HayTime1_minus_diff_data<-your_data_file[,c("ParticipantID","Individual_number","Group","Timepoint","Age","Sex","HayTime2.HayTime1_minus")]
F0_HayTime2.HayTime1_minus_data<-HayTime2.HayTime1_minus_diff_data[HayTime2.HayTime1_minus_diff_data$Timepoint=='F0', ]
F2_HayTime2.HayTime1_minus_data<-HayTime2.HayTime1_minus_diff_data[HayTime2.HayTime1_minus_diff_data$Timepoint=='F2', ]
HayTime2.HayTime1_minusDiff<-F2_HayTime2.HayTime1_minus_data$HayTime2.HayTime1_minus - F0_HayTime2.HayTime1_minus_data$HayTime2.HayTime1_minus
F0_HayTime2.HayTime1_minus_data$HayTime2.HayTime1_minusDiff<-HayTime2.HayTime1_minusDiff
#run one-way ANOVA on the F2-F0 differences in HayTime2.HayTime1_minus between groups
HayTime2.HayTime1_minusDiff_mod <- lm(HayTime2.HayTime1_minusDiff ~ Group, data = F0_HayTime2.HayTime1_minus_data)
anova(HayTime2.HayTime1_minusDiff_mod) #sig. difference
#effect size omnibus ANOVA
etaSquared(HayTime2.HayTime1_minusDiff_mod)
#post hoc f/u test
#run pairwise comparisons (post-hoc Tukey), given that the F-test was significant. 
post_hoc_HayTime2.HayTime1_minusDiff_mod <- glht(HayTime2.HayTime1_minusDiff_mod, linfct = mcp(Group = "Tukey"))
summary(post_hoc_HayTime2.HayTime1_minusDiff_mod)
confint(post_hoc_HayTime2.HayTime1_minusDiff_mod)
#effect size for Interaction
  #create dataframe
  #HayTime2.HayTime1_minus_diff_data_124 <- your_data_file[1:124,c("ParticipantID","Individual_number","Group","Timepoint","Age","Sex","HayTime2.HayTime1_minus")]
  HayTime2.HayTime1_minus_diff_data_124 <- your_data_file[1:119,c("ParticipantID","Individual_number","Group","Timepoint","Age","Sex","HayTime2.HayTime1_minus")]
  HayTime2.HayTime1_minus_diff_data_124$HayTime2.HayTime1_minusDiff <- HayTime2.HayTime1_minusDiff 
  #calculate effect size
  HayTime2.HayTime1_minus_diff_data_124%>%cohens_d(HayTime2.HayTime1_minusDiff~Group)

#plot HayTime2.HayTime1.z_minus.calc. (raincloud plot)
ggplot(your_data_file, aes(x = Group, y = HayTime2.HayTime1.z_minus.calc., fill = Timepoint)) + 
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(aes(y = HayTime2.HayTime1.z_minus.calc., color = Timepoint), position = position_jitter(width = .15), size = .5, alpha = 0.8) +
  geom_boxplot(width = 0.1, fill = "white", outlier.size = 1, aes(colour = Timepoint)) + 
  stat_summary(fun = mean, geom = "point", shape = 19, size = 2, aes(colour = Timepoint)) + 
  xlab("Group") + 
  ylab("Hayling Sentence Set 2 Time Minus Hayling Sentence Set 1 (z-scores)") +
  scale_x_discrete(labels = c("1" = "Control", "2" = "SCD", "3" = "aMCI", "4" = "mMCI", "5" = "AD")) + 
  theme_classic() +
  coord_flip()
#run mixed design, 2 x 5 ANOVA for HayTime2.HayTime1_minus
aov_HayTime2.HayTime1.z_minus.calc. <- anova_test(data=your_data_file, dv=HayTime2.HayTime1.z_minus.calc., wid=Individual_number, between=Group, within=Timepoint, effect.size = "pes")
get_anova_table(aov_HayTime2.HayTime1.z_minus.calc.)
#descriptives
HayTime2.HayTime1.z_minus.calc._descrip <- describeBy(your_data_file$HayTime2.HayTime1.z_minus.calc., list(your_data_file$Group, your_data_file$Timepoint))
#find mean & SD from total sample in F0 and F2 timepoints:
F0_HayTime2.HayTime1.z_minus.calc. <- your_data_file[your_data_file[, "Timepoint"] == "F0",]
noNAsF0_HayTime2.HayTime1.z_minus.calc. <- na.omit(F0_HayTime2.HayTime1.z_minus.calc.$HayTime2.HayTime1.z_minus.calc.)
mean(noNAsF0_HayTime2.HayTime1.z_minus.calc.)
sd(noNAsF0_HayTime2.HayTime1.z_minus.calc.)
#F2
F2_HayTime2.HayTime1.z_minus.calc. <- your_data_file[your_data_file[, "Timepoint"] == "F2",]
noNAsF2_HayTime2.HayTime1.z_minus.calc. <- na.omit(F2_HayTime2.HayTime1.z_minus.calc.$HayTime2.HayTime1.z_minus.calc.)
mean(noNAsF2_HayTime2.HayTime1.z_minus.calc.)
sd(noNAsF2_HayTime2.HayTime1.z_minus.calc.)
#Post hoc tests
#non-sig. interaction & Timepoint, Test by Group (Tukey test)
aov(HayTime2.HayTime1.z_minus.calc. ~ Group, data = your_data_file) %>% tukey_hsd()
#effect size for Groups
your_data_file%>%cohens_d(HayTime2.HayTime1.z_minus.calc.~Group)
#interaction f/u tests -- HayTime2.HayTime1_minus (z-scores)
#subtract differences in F2 - F0 for HayTime2.HayTime1_minus (z-scores)
HayTime2.HayTime1.z_minus.calc._diff_data<-your_data_file[,c("ParticipantID","Individual_number","Group","Timepoint","Age","Sex","HayTime2.HayTime1.z_minus.calc.")]
F0_HayTime2.HayTime1.z_minus.calc._data<-HayTime2.HayTime1.z_minus.calc._diff_data[HayTime2.HayTime1.z_minus.calc._diff_data$Timepoint=='F0', ]
F2_HayTime2.HayTime1.z_minus.calc._data<-HayTime2.HayTime1.z_minus.calc._diff_data[HayTime2.HayTime1.z_minus.calc._diff_data$Timepoint=='F2', ]
HayTime2.HayTime1.z_minus.calc.Diff<-F2_HayTime2.HayTime1.z_minus.calc._data$HayTime2.HayTime1.z_minus.calc. - F0_HayTime2.HayTime1.z_minus.calc._data$HayTime2.HayTime1.z_minus.calc.
F0_HayTime2.HayTime1.z_minus.calc._data$HayTime2.HayTime1.z_minus.calc.Diff<-HayTime2.HayTime1.z_minus.calc.Diff
#run one-way ANOVA on the F2-F0 differences in HayTime2.HayTime1.z_minus.calc. between groups
HayTime2.HayTime1.z_minus.calc.Diff_mod <- lm(HayTime2.HayTime1.z_minus.calc.Diff ~ Group, data = F0_HayTime2.HayTime1.z_minus.calc._data)
anova(HayTime2.HayTime1.z_minus.calc.Diff_mod) #sig. difference
#effect size omnibus ANOVA
etaSquared(HayTime2.HayTime1.z_minus.calc.Diff_mod)
#post hoc f/u test
#run pairwise comparisons (post-hoc Tukey), given that the F-test was significant. 
post_hoc_HayTime2.HayTime1.z_minus.calc.Diff_mod <- glht(HayTime2.HayTime1.z_minus.calc.Diff_mod, linfct = mcp(Group = "Tukey"))
summary(post_hoc_HayTime2.HayTime1.z_minus.calc.Diff_mod)
confint(post_hoc_HayTime2.HayTime1.z_minus.calc.Diff_mod)
#effect size for Interaction
  #create dataframe
  #HayTime2.HayTime1.z_minus.calc._diff_data_124 <- your_data_file[1:124,c("ParticipantID","Individual_number","Group","Timepoint","Age","Sex","HayTime2.HayTime1.z_minus.calc.")]
  HayTime2.HayTime1.z_minus.calc._diff_data_124 <- your_data_file[1:119,c("ParticipantID","Individual_number","Group","Timepoint","Age","Sex","HayTime2.HayTime1.z_minus.calc.")]
  HayTime2.HayTime1.z_minus.calc._diff_data_124$HayTime2.HayTime1.z_minus.calc.Diff <- HayTime2.HayTime1.z_minus.calc.Diff 
  #calculate effect size
  HayTime2.HayTime1.z_minus.calc._diff_data_124%>%cohens_d(HayTime2.HayTime1.z_minus.calc.Diff~Group)

#plot HayBCatA.Raw (raincloud plot)
ggplot(your_data_file, aes(x = Group, y = HayBCatA.Raw, fill = Timepoint)) + 
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(aes(y = HayBCatA.Raw, color = Timepoint), position = position_jitter(width = .15), size = .5, alpha = 0.8) +
  geom_boxplot(width = 0.1, fill = "white", outlier.size = 1, aes(colour = Timepoint)) + 
  stat_summary(fun = mean, geom = "point", shape = 19, size = 2, aes(colour = Timepoint)) + 
  xlab("Group") + 
  ylab("Hayling Sentence Set 2 Errors - Category A (total number of incorrect 'plausible' responses)") +
  scale_x_discrete(labels = c("1" = "Control", "2" = "SCD", "3" = "aMCI", "4" = "mMCI", "5" = "AD")) + 
  theme_classic() +
  coord_flip()
#run mixed design, 2 x 5 ANOVA for HayBCatA.Raw
aov_HayBCatARaw <- anova_test(data=your_data_file, dv=HayBCatA.Raw, wid=Individual_number, between=Group, within=Timepoint, effect.size = "pes")
get_anova_table(aov_HayBCatARaw)
#check descriptive statistics per each group, per each timepoint
HayBCatA.Raw_descrip <- describeBy(your_data_file$HayBCatA.Raw, list(your_data_file$Group, your_data_file$Timepoint))
#find mean & SD from total sample in F0 and F2 timepoints:
F0_HayBCatARaw <- your_data_file[your_data_file[, "Timepoint"] == "F0",]
noNAsF0_HayBCatARaw <- na.omit(F0_HayBCatARaw$HayBCatA.Raw)
mean(noNAsF0_HayBCatARaw)
sd(noNAsF0_HayBCatARaw)
#F2
F2_HayBCatARaw <- your_data_file[your_data_file[, "Timepoint"] == "F2",]
noNAsF2_HayBCatARaw <- na.omit(F2_HayBCatARaw$HayBCatA.Raw)
mean(noNAsF2_HayBCatARaw)
sd(noNAsF2_HayBCatARaw)
#Post hoc tests
#non-sig. interaction & Timepoint, Test by Group (Tukey test)
aov(HayBCatA.Raw ~ Group, data = your_data_file) %>% tukey_hsd()
#effect size for Groups
your_data_file%>%cohens_d(HayBCatA.Raw~Group)

#plot HayBCatA.z (raincloud plot)
ggplot(your_data_file, aes(x = Group, y = HayBCatA.z, fill = Timepoint)) + 
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(aes(y = HayBCatA.z, color = Timepoint), position = position_jitter(width = .15), size = .5, alpha = 0.8) +
  geom_boxplot(width = 0.1, fill = "white", outlier.size = 1, aes(colour = Timepoint)) + 
  stat_summary(fun = mean, geom = "point", shape = 19, size = 2, aes(colour = Timepoint)) + 
  xlab("Group") + 
  ylab("Hayling Sentence Set 2 Errors - Category A (total number of incorrect 'plausible' responses) Z-scores") +
  scale_x_discrete(labels = c("1" = "Control", "2" = "SCD", "3" = "aMCI", "4" = "mMCI", "5" = "AD")) + 
  theme_classic() +
  coord_flip()
#run mixed design, 2 x 5 ANOVA for HayBCatA.z
aov_HayBCatAz <- anova_test(data=your_data_file, dv=HayBCatA.z, wid=Individual_number, between=Group, within=Timepoint, effect.size = "pes")
get_anova_table(aov_HayBCatAz)
#check descriptive statistics per each group, per each timepoint
HayBCatA.z_descrip <- describeBy(your_data_file$HayBCatA.z, list(your_data_file$Group, your_data_file$Timepoint))
#find mean & SD from total sample in F0 and F2 timepoints:
F0_HayBCatAz <- your_data_file[your_data_file[, "Timepoint"] == "F0",]
noNAsF0_HayBCatAz <- na.omit(F0_HayBCatAz$HayBCatA.z)
mean(noNAsF0_HayBCatAz)
sd(noNAsF0_HayBCatAz)
#F2
F2_HayBCatAz <- your_data_file[your_data_file[, "Timepoint"] == "F2",]
noNAsF2_HayBCatAz <- na.omit(F2_HayBCatAz$HayBCatA.z)
mean(noNAsF2_HayBCatAz)
sd(noNAsF2_HayBCatAz)
#Post hoc tests
#non-sig. interaction & Timepoint, Test by Group (Tukey test)
aov(HayBCatA.z ~ Group, data = your_data_file) %>% tukey_hsd()
#effect size for Groups
your_data_file%>%cohens_d(HayBCatA.z~Group)

#plot HayBCatB.Raw (raincloud plot)
ggplot(your_data_file, aes(x = Group, y = HayBCatB.Raw, fill = Timepoint)) + 
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(aes(y = HayBCatB.Raw, color = Timepoint), position = position_jitter(width = .15), size = .5, alpha = 0.8) +
  geom_boxplot(width = 0.1, fill = "white", outlier.size = 1, aes(colour = Timepoint)) + 
  stat_summary(fun = mean, geom = "point", shape = 19, size = 2, aes(colour = Timepoint)) + 
  xlab("Group") + 
  ylab("Hayling Sentence Set 2 Errors - Category B (total number of incorrect 'somewhat plausible' responses)") +
  scale_x_discrete(labels = c("1" = "Control", "2" = "SCD", "3" = "aMCI", "4" = "mMCI", "5" = "AD")) + 
  theme_classic() +
  coord_flip()
#run mixed design, 2 x 5 ANOVA for HayBTime1.Raw
aov_HayBCatBRaw <- anova_test(data=your_data_file, dv=HayBCatB.Raw, wid=Individual_number, between=Group, within=Timepoint, effect.size = "pes")
get_anova_table(aov_HayBCatBRaw)
#check descriptive statistics per each group, per each timepoint
HayBCatB.Raw_descrip <- describeBy(your_data_file$HayBCatB.Raw, list(your_data_file$Group, your_data_file$Timepoint))
#find mean & SD from total sample in F0 and F2 timepoints:
F0_HayBCatBRaw <- your_data_file[your_data_file[, "Timepoint"] == "F0",]
noNAsF0_HayBCatBRaw <- na.omit(F0_HayBCatBRaw$HayBCatB.Raw)
mean(noNAsF0_HayBCatBRaw)
sd(noNAsF0_HayBCatBRaw)
#F2
F2_HayBCatBRaw <- your_data_file[your_data_file[, "Timepoint"] == "F2",]
noNAsF2_HayBCatBRaw <- na.omit(F2_HayBCatBRaw$HayBCatB.Raw)
mean(noNAsF2_HayBCatBRaw)
sd(noNAsF2_HayBCatBRaw)
#Post hoc tests
#non-sig. interaction & Timepoint, Test by Group (Tukey test)
aov(HayBCatB.Raw ~ Group, data = your_data_file) %>% tukey_hsd()
#effect size for Groups
your_data_file%>%cohens_d(HayBCatB.Raw~Group)

#plot HayBCatB.z (raincloud plot)
ggplot(your_data_file, aes(x = Group, y = HayBCatB.z, fill = Timepoint)) + 
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(aes(y = HayBCatB.z, color = Timepoint), position = position_jitter(width = .15), size = .5, alpha = 0.8) +
  geom_boxplot(width = 0.1, fill = "white", outlier.size = 1, aes(colour = Timepoint)) + 
  stat_summary(fun = mean, geom = "point", shape = 19, size = 2, aes(colour = Timepoint)) + 
  xlab("Group") + 
  ylab("Hayling Sentence Set 2 Errors - Category B (total number of incorrect 'somewhat plausible' responses) Z-scores") +
  scale_x_discrete(labels = c("1" = "Control", "2" = "SCD", "3" = "aMCI", "4" = "mMCI", "5" = "AD")) + 
  theme_classic() +
  coord_flip()
#run mixed design, 2 x 5 ANOVA for HayBTime1.z
aov_HayBCatBz <- anova_test(data=your_data_file, dv=HayBCatB.z, wid=Individual_number, between=Group, within=Timepoint, effect.size = "pes")
get_anova_table(aov_HayBCatBz)
#check descriptive statistics per each group, per each timepoint
HayBCatB.z_descrip <- describeBy(your_data_file$HayBCatB.z, list(your_data_file$Group, your_data_file$Timepoint))
#find mean & SD from total sample in F0 and F2 timepoints:
F0_HayBCatBz <- your_data_file[your_data_file[, "Timepoint"] == "F0",]
noNAsF0_HayBCatBz <- na.omit(F0_HayBCatBz$HayBCatB.z)
mean(noNAsF0_HayBCatBz)
sd(noNAsF0_HayBCatBz)
#F2
F2_HayBCatBz <- your_data_file[your_data_file[, "Timepoint"] == "F2",]
noNAsF2_HayBCatBz <- na.omit(F2_HayBCatBz$HayBCatB.z)
mean(noNAsF2_HayBCatBz)
sd(noNAsF2_HayBCatBz)
#Post hoc tests
#non-sig. interaction & Timepoint, Test by Group (Tukey test)
aov(HayBCatB.z ~ Group, data = your_data_file) %>% tukey_hsd()
#effect size for Groups
your_data_file%>%cohens_d(HayBCatB.z~Group)
#interaction f/u tests -- HayTime2.HayTime1_minus (z-scores)
#subtract differences in F2 - F0 for HayTime2.HayTime1_minus (z-scores)
HayBCatB.z_diff_data<-your_data_file[,c("ParticipantID","Individual_number","Group","Timepoint","Age","Sex","HayBCatB.z")]
F0_HayBCatB.z_data<-HayBCatB.z_diff_data[HayBCatB.z_diff_data$Timepoint=='F0', ]
F2_HayBCatB.z_data<-HayBCatB.z_diff_data[HayBCatB.z_diff_data$Timepoint=='F2', ]
HayBCatB.zDiff<-F2_HayBCatB.z_data$HayBCatB.z - F0_HayBCatB.z_data$HayBCatB.z
F0_HayBCatB.z_data$HayBCatB.zDiff<-HayBCatB.zDiff
#run one-way ANOVA on the F2-F0 differences in HayTime2.HayTime1.z_minus.calc. between groups
HayBCatB.zDiff_mod <- lm(HayBCatB.zDiff ~ Group, data = F0_HayBCatB.z_data)
anova(HayBCatB.zDiff_mod) #sig. difference
#effect size omnibus ANOVA
etaSquared(HayBCatB.zDiff_mod)
#post hoc f/u test
#run pairwise comparisons (post-hoc Tukey), given that the F-test was significant. 
post_hoc_HayBCatB.zDiff_mod <- glht(HayBCatB.zDiff_mod, linfct = mcp(Group = "Tukey"))
summary(post_hoc_HayBCatB.zDiff_mod)
confint(post_hoc_HayBCatB.zDiff_mod)
#effect size for Interaction
#create dataframe
#HayBCatB.z_diff_data_124 <- your_data_file[1:124,c("ParticipantID","Individual_number","Group","Timepoint","Age","Sex","HayBCatB.z")]
HayBCatB.z_diff_data_124 <- your_data_file[1:119,c("ParticipantID","Individual_number","Group","Timepoint","Age","Sex","HayBCatB.z")]
HayBCatB.z_diff_data_124$HayBCatB.zDiff <- HayBCatB.zDiff 
#calculate effect size
HayBCatB.z_diff_data_124%>%cohens_d(HayBCatB.zDiff~Group)

#plot HayCatTotalError.Raw (raincloud plot)
ggplot(your_data_file, aes(x = Group, y = HayCatTotalError.Raw, fill = Timepoint)) + 
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(aes(y = HayCatTotalError.Raw, color = Timepoint), position = position_jitter(width = .15), size = .5, alpha = 0.8) +
  geom_boxplot(width = 0.1, fill = "white", outlier.size = 1, aes(colour = Timepoint)) + 
  stat_summary(fun = mean, geom = "point", shape = 19, size = 2, aes(colour = Timepoint)) + 
  xlab("Group") + 
  ylab("Hayling Sentence Total Errors (HayCatA + HayCatB Errors)") +
  scale_x_discrete(labels = c("1" = "Control", "2" = "SCD", "3" = "aMCI", "4" = "mMCI", "5" = "AD")) + 
  theme_classic() +
  coord_flip()
#run mixed design, 2 x 5 ANOVA for HayCatTotalError.Raw
aov_HayCatTotalError.Raw <- anova_test(data=your_data_file, dv=HayCatTotalError.Raw, wid=Individual_number, between=Group, within=Timepoint, effect.size = "pes")
get_anova_table(aov_HayCatTotalError.Raw)
#check descriptive statistics per each group, per each timepoint
HayCatTotalError.Raw_descrip <- describeBy(your_data_file$HayCatTotalError.Raw, list(your_data_file$Group, your_data_file$Timepoint))
#find mean & SD from total sample in F0 and F2 timepoints:
F0_HayCatTotalError.Raw <- your_data_file[your_data_file[, "Timepoint"] == "F0",]
noNAsF0_HayCatTotalError.Raw <- na.omit(F0_HayCatTotalError.Raw$HayCatTotalError.Raw)
mean(noNAsF0_HayCatTotalError.Raw)
sd(noNAsF0_HayCatTotalError.Raw)
#F2
F2_HayCatTotalError.Raw <- your_data_file[your_data_file[, "Timepoint"] == "F2",]
noNAsF2_HayCatTotalError.Raw <- na.omit(F2_HayCatTotalError.Raw$HayCatTotalError.Raw)
mean(noNAsF2_HayCatTotalError.Raw)
sd(noNAsF2_HayCatTotalError.Raw)
#Post hoc tests
#non-sig. interaction & Timepoint, Test by Group (Tukey test)
aov(HayCatTotalError.Raw ~ Group, data = your_data_file) %>% tukey_hsd()
#effect size for Groups
your_data_file%>%cohens_d(HayCatTotalError.Raw~Group)

#plot HayCatTotalError.z (raincloud plot)
ggplot(your_data_file, aes(x = Group, y = HayCatTotalError.z, fill = Timepoint)) + 
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(aes(y = HayCatTotalError.z, color = Timepoint), position = position_jitter(width = .15), size = .5, alpha = 0.8) +
  geom_boxplot(width = 0.1, fill = "white", outlier.size = 1, aes(colour = Timepoint)) + 
  stat_summary(fun = mean, geom = "point", shape = 19, size = 2, aes(colour = Timepoint)) + 
  xlab("Group") + 
  ylab("Hayling Sentence Total Errors (HayCatA + HayCatB Errors)") +
  scale_x_discrete(labels = c("1" = "Control", "2" = "SCD", "3" = "aMCI", "4" = "mMCI", "5" = "AD")) + 
  theme_classic() +
  coord_flip()
#run mixed design, 2 x 5 ANOVA for HayCatTotalError.z
aov_HayCatTotalError.z <- anova_test(data=your_data_file, dv=HayCatTotalError.z, wid=Individual_number, between=Group, within=Timepoint, effect.size = "pes")
get_anova_table(aov_HayCatTotalError.z)
#check descriptive statistics per each group, per each timepoint
HayCatTotalError.z_descrip <- describeBy(your_data_file$HayCatTotalError.z, list(your_data_file$Group, your_data_file$Timepoint))
#find mean & SD from total sample in F0 and F2 timepoints:
F0_HayCatTotalError.z <- your_data_file[your_data_file[, "Timepoint"] == "F0",]
noNAsF0_HayCatTotalError.z <- na.omit(F0_HayCatTotalError.z$HayCatTotalError.z)
mean(noNAsF0_HayCatTotalError.z)
sd(noNAsF0_HayCatTotalError.z)
#F2
F2_HayCatTotalError.z <- your_data_file[your_data_file[, "Timepoint"] == "F2",]
noNAsF2_HayCatTotalError.z <- na.omit(F2_HayCatTotalError.z$HayCatTotalError.z)
mean(noNAsF2_HayCatTotalError.z)
sd(noNAsF2_HayCatTotalError.z)
#Post hoc tests
#non-sig. interaction & Timepoint, Test by Group (Tukey test)
aov(HayCatTotalError.z ~ Group, data = your_data_file) %>% tukey_hsd()
#effect size for Groups
your_data_file%>%cohens_d(HayCatTotalError.z~Group)

#3.D-KEFS Stroop Task ---------------------------------------------------------#
#plot ColorNaming.Raw (raincloud plot)
ggplot(your_data_file, aes(x = Group, y = ColorNaming.Raw, fill = Timepoint)) + 
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(aes(y = ColorNaming.Raw, color = Timepoint), position = position_jitter(width = .15), size = .5, alpha = 0.8) +
  geom_boxplot(width = 0.1, fill = "white", outlier.size = 1, aes(colour = Timepoint)) + 
  stat_summary(fun = mean, geom = "point", shape = 19, size = 2, aes(colour = Timepoint)) + 
  xlab("Group") + 
  ylab("Stroop - Color Naming Raw (seconds)") +
  scale_x_discrete(labels = c("1" = "Control", "2" = "SCD", "3" = "aMCI", "4" = "mMCI", "5" = "AD")) + 
  theme_classic() +
  coord_flip()
#run mixed design, 2 x 5 ANOVA for ColorNaming.Raw
aov_ColorNamingRaw <- anova_test(data=your_data_file, dv=ColorNaming.Raw, wid=Individual_number, between=Group, within=Timepoint, effect.size = "pes")
get_anova_table(aov_ColorNamingRaw)
#check descriptive statistics per each group, per each timepoint
ColorNaming.Raw_descrip <- describeBy(your_data_file$ColorNaming.Raw, list(your_data_file$Group, your_data_file$Timepoint))
#find mean & SD from total sample in F0 and F2 timepoints:
F0_ColorNamingRaw <- your_data_file[your_data_file[, "Timepoint"] == "F0",]
noNAsF0_ColorNamingRaw <- na.omit(F0_ColorNamingRaw$ColorNaming.Raw)
mean(noNAsF0_ColorNamingRaw)
sd(noNAsF0_ColorNamingRaw)
#F2
F2_ColorNamingRaw <- your_data_file[your_data_file[, "Timepoint"] == "F2",]
noNAsF2_ColorNamingRaw <- na.omit(F2_ColorNamingRaw$ColorNaming.Raw)
mean(noNAsF2_ColorNamingRaw)
sd(noNAsF2_ColorNamingRaw)
#Post hoc tests
#Main Effect Test by Group (Tukey test)
aov(ColorNaming.Raw ~ Group, data = your_data_file) %>% tukey_hsd()
#effect size for Groups
your_data_file%>%cohens_d(ColorNaming.Raw~Group)
#non-sig. interaction - test by Time Point
# your_data_file %>%
#   pairwise_t_test(
#     ColorNaming.Raw ~ Timepoint, paired = TRUE,
#     p.adjust.method = "fdr"
#   )
#Main Effect Test by Timepoint (Tukey test)
aov(ColorNaming.Raw ~ Timepoint, data = your_data_file) %>% tukey_hsd()
#effect size for Time Point
your_data_file%>%cohens_d(ColorNaming.Raw~Timepoint, paired=TRUE)
#Sig. Interaction testing
#Simple main effect (aka simple effects tests) w/ interaction - for Group
# posthoc_ME_Group_ColorNaming <- your_data_file %>%
#   group_by(Timepoint) %>%
#   anova_test(dv=ColorNaming.Raw,wid=Individual_number,between=Group) %>%
#   tukey_hsd() #doesn't work
# posthoc_ME_Group_ColorNaming ##within each timepoint (F0 & F2), there are Group differences##
posthoc_ME_Group_ColorNaming <- your_data_file %>%
  group_by(Timepoint) %>%
  anova_test(dv=ColorNaming.Raw,wid=Individual_number,between=Group) %>%
  adjust_pvalue(method="fdr")
posthoc_ME_Group_ColorNaming ##within each timepoint (F0 & F2), there are Group differences##
#Run Pairwise comparison between groups levels if simple main effects (above) is sig.
posthoc_pairwise_Group_ColorNaming <- your_data_file %>%
  group_by(Timepoint) %>%
  tukey_hsd(ColorNaming.Raw ~ Group)
posthoc_pairwise_Group_ColorNaming ##examines all Group contrasts within each timepoint (F0 & F2) ##
# posthoc_pairwise_Group_ColorNaming <- your_data_file %>%
#   group_by(Timepoint) %>%
#   pairwise_t_test(ColorNaming.Raw ~ Group, p.adjust.method = "fdr")
# posthoc_pairwise_Group_ColorNaming ##examines all Group contrasts within each timepoint (F0 & F2) ##
#Simple main effect (aka simple effects tests) w/ interaction - for Timepoint
# posthoc_ME_Timepoint_ColorNaming <- your_data_file %>%
#   group_by(Group) %>%
#   anova_test(dv=ColorNaming.Raw,wid=Individual_number,within=Timepoint,effect.size = "pes") %>%
#   get_anova_table() %>%
#   tukey_hsd() #doesn't work 
# posthoc_ME_Timepoint_ColorNaming ## Across each Group, where were there sig. differences between timepoints (F0 vs. F2)? ##
posthoc_ME_Timepoint_ColorNaming <- your_data_file %>%
  group_by(Group) %>%
  anova_test(dv=ColorNaming.Raw,wid=Individual_number,within=Timepoint,effect.size = "pes") %>%
  get_anova_table() %>%
  adjust_pvalue(method="fdr")
posthoc_ME_Timepoint_ColorNaming ## Across each Group, where were there sig. differences between timepoints (F0 vs. F2)? ##
#Pairwise comparison between groups levels if simple main effects (above) is sig.
posthoc_pairwise_Timepoint_ColorNaming <- your_data_file %>%
  group_by(Group) %>%
  tukey_hsd(ColorNaming.Raw ~ Timepoint, paired = TRUE)
posthoc_pairwise_Timepoint_ColorNaming ##examines all Timepoint contrasts between each Group (1,2,3,4,5) ##
# posthoc_pairwise_Timepoint_ColorNaming <- your_data_file %>%
#   group_by(Group) %>%
#   pairwise_t_test(
#     ColorNaming.Raw ~ Timepoint, paired = TRUE,
#     p.adjust.methods = "fdr") %>%
#   select(-df, -statistic, -p) # Remove details
# posthoc_pairwise_Timepoint_ColorNaming  ##examines all Timepoint contrasts between each Group (1,2,3,4,5) ##
#all pairwise comparisons, Tukey test - from: https://online.stat.psu.edu/stat485/lesson/12/12.7
summary(lm(ColorNaming.Raw~Group+Timepoint+Group:Timepoint,data=your_data_file)) 
TukeyHSD(aov(ColorNaming.Raw~Group+Timepoint+Group:Timepoint,data=your_data_file)) 
#effect size for interaction (aMCI Group and Time Point)
your_data_file_aMCI_ColorNaming_long <- subset(your_data_file, your_data_file$Group == 3)
your_data_file_aMCI_ColorNaming_long$Group <- droplevels(your_data_file_aMCI_ColorNaming_long$Group)
your_data_file_aMCI_ColorNaming_long%>%cohens_d(ColorNaming.Raw~Timepoint, paired=TRUE)
#plot interaction 
#remove NAs for ColorNaming variable
noNAs_ColorNamingRaw <- your_data_file[,c("ParticipantID","Individual_number","Group","Timepoint","Age","Sex","ColorNaming.Raw")]
noNAs_ColorNamingRaw <- noNAs_ColorNamingRaw[complete.cases(noNAs_ColorNamingRaw), ]
#view interaction plot - by Group
noNAs_ColorNamingRaw%>%
  group_by(Group,Timepoint) %>%
  summarise(s_mean=mean(ColorNaming.Raw)) %>%
  ggplot(aes(y=s_mean,x=Group,colour=Timepoint,group=Timepoint))+
  geom_point()+geom_line()+
  scale_x_discrete(labels = c("1" = "C", "2" = "SCD", "3" = "aMCI", "4" = "mMCI", "5" = "AD")) + 
  ylab("Stroop - Colour Naming Raw (seconds)") +
  theme_classic()
#view interaction plot - by Timepoint
noNAs_ColorNamingRaw %>%
  group_by(Group,Timepoint) %>%
  summarise(s_mean=mean(ColorNaming.Raw)) %>%
  ggplot(aes(y=s_mean,x=Timepoint,colour=Group,group=Group))+
  geom_point()+geom_line()+
  scale_color_discrete(labels = c("1" = "C", "2" = "SCD", "3" = "aMCI", "4" = "mMCI", "5" = "AD")) + 
  ylab("Stroop - Colour Naming Raw (seconds)") +
  theme_classic()
#interaction f/u tests -- Colour Naming
#subtract differences in F2 - F0 for Colour Naming 
ColorNaming.Raw_diff_data<-your_data_file[,c("ParticipantID","Individual_number","Group","Timepoint","Age","Sex","ColorNaming.Raw")]
F0_ColorNaming.Raw_data<-ColorNaming.Raw_diff_data[ColorNaming.Raw_diff_data$Timepoint=='F0', ]
F2_ColorNaming.Raw_data<-ColorNaming.Raw_diff_data[ColorNaming.Raw_diff_data$Timepoint=='F2', ]
ColorNaming.RawDiff<-F2_ColorNaming.Raw_data$ColorNaming.Raw - F0_ColorNaming.Raw_data$ColorNaming.Raw
F0_ColorNaming.Raw_data$ColorNaming.RawDiff<-ColorNaming.RawDiff
#run one-way ANOVA on the F2-F0 differences in ColorNaming.Raw between groups
ColorNaming.RawDiff_mod <- lm(ColorNaming.RawDiff ~ Group, data = F0_ColorNaming.Raw_data)
anova(ColorNaming.RawDiff_mod) #sig. difference
#effect size omnibus ANOVA
etaSquared(ColorNaming.RawDiff_mod)
#post hoc f/u test
#run pairwise comparisons (post-hoc Tukey), given that the F-test was significant. 
post_hoc_ColorNaming.RawDiff_mod <- glht(ColorNaming.RawDiff_mod, linfct = mcp(Group = "Tukey"))
summary(post_hoc_ColorNaming.RawDiff_mod)
confint(post_hoc_ColorNaming.RawDiff_mod)
#effect size for Interaction
#create dataframe
  #ColorNaming.Raw_diff_data_124 <- your_data_file[1:124,c("ParticipantID","Individual_number","Group","Timepoint","Age","Sex","ColorNaming.Raw")]
  ColorNaming.Raw_diff_data_124 <- your_data_file[1:119,c("ParticipantID","Individual_number","Group","Timepoint","Age","Sex","ColorNaming.Raw")]
  ColorNaming.Raw_diff_data_124$ColorNaming.RawDiff <- ColorNaming.RawDiff 
  #calculate effect size
  ColorNaming.Raw_diff_data_124%>%cohens_d(ColorNaming.RawDiff~Group)
#test if there is a difference among differences (e.g., is the aMCI group difference for color naming between the time points 
#(F0 - F2) sig. greater compared to the other groups?)
#subtract differences in F2 - F0 for color naming
ColorNaming_diff_data<-your_data_file[,c("ParticipantID","Individual_number","Group","Timepoint","Age","Sex","ColorNaming.Raw")]
F0_ColorNaming_data<-ColorNaming_diff_data[ColorNaming_diff_data$Timepoint=='F0', ]
F2_ColorNaming_data<-ColorNaming_diff_data[ColorNaming_diff_data$Timepoint=='F2', ]
ColorNamingDiff<-F2_ColorNaming_data$ColorNaming.Raw - F0_ColorNaming_data$ColorNaming.Raw
F0_ColorNaming_data$ColorNamingDiff<-ColorNamingDiff
#run one-way ANOVA on the F2-F0 differences in color naming between groups
ColorNamingDiff_mod <- lm(ColorNamingDiff ~ Group, data = F0_ColorNaming_data)
anova(ColorNamingDiff_mod) #sig. difference
#effect size omnibus ANOVA
etaSquared(ColorNamingDiff_mod)
#post hoc f/u test
#run pairwise comparisons (post-hoc Tukey), given that the F-test was significant. 
post_hoc_ColorNamingDiff_mod <- glht(ColorNamingDiff_mod, linfct = mcp(Group = "Tukey"))
summary(post_hoc_ColorNamingDiff_mod)
confint(post_hoc_ColorNamingDiff_mod)
#effect size for sig. post hoc tests
  #for SCD vs. aMCI 
  SCDvaMCI_ColorNamingDiff <- subset(F0_ColorNaming_data, F0_ColorNaming_data$Group == 2 | F0_ColorNaming_data$Group == 3)
  SCDvaMCI_ColorNamingDiff$Group <- droplevels(SCDvaMCI_ColorNamingDiff$Group)
  cohensD(ColorNamingDiff ~ Group, data = SCDvaMCI_ColorNamingDiff) #this looks like Hedges' g? 
#plot Color Naming differences (raincloud plot)
ggplot(F0_ColorNaming_data, aes(x = Group, y = ColorNamingDiff, fill = Group)) + 
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(aes(y = ColorNamingDiff, color = Group), position = position_jitter(width = .15), size = .5, alpha = 0.8) +
  geom_boxplot(width = 0.1, fill = "white", outlier.size = 1, aes(colour = Group)) + 
  stat_summary(fun = mean, geom = "point", shape = 19, size = 2, aes(colour = Group)) + 
  xlab("Group") + 
  ylab("Color Naming Difference (F2 - F0)") +
  scale_x_discrete(labels = c("1" = "Control", "2" = "SCD", "3" = "aMCI", "4" = "mMCI", "5" = "AD")) + 
  theme_classic() +
  theme(legend.position = "none")+
  coord_flip()

#plot ColorNaming.Z (raincloud plot)
ggplot(your_data_file, aes(x = Group, y = ColorNaming.Z, fill = Timepoint)) + 
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(aes(y = ColorNaming.Z, color = Timepoint), position = position_jitter(width = .15), size = .5, alpha = 0.8) +
  geom_boxplot(width = 0.1, fill = "white", outlier.size = 1, aes(colour = Timepoint)) + 
  stat_summary(fun = mean, geom = "point", shape = 19, size = 2, aes(colour = Timepoint)) + 
  xlab("Group") + 
  ylab("Stroop - Color Naming Z-scores") +
  scale_x_discrete(labels = c("1" = "Control", "2" = "SCD", "3" = "aMCI", "4" = "mMCI", "5" = "AD")) + 
  theme_classic() +
  coord_flip()
#run mixed design, 2 x 5 ANOVA for ColorNaming.Z
aov_ColorNamingZ <- anova_test(data=your_data_file, dv=ColorNaming.Z, wid=Individual_number, between=Group, within=Timepoint, effect.size = "pes")
get_anova_table(aov_ColorNamingZ)
#check descriptive statistics per each group, per each timepoint
ColorNaming.Z_descrip <- describeBy(your_data_file$ColorNaming.Z, list(your_data_file$Group, your_data_file$Timepoint))
#find mean & SD from total sample in F0 and F2 timepoints:
F0_ColorNamingZ <- your_data_file[your_data_file[, "Timepoint"] == "F0",]
noNAsF0_ColorNamingZ <- na.omit(F0_ColorNamingZ$ColorNaming.Z)
mean(noNAsF0_ColorNamingZ)
sd(noNAsF0_ColorNamingZ)
#F2
F2_ColorNamingZ <- your_data_file[your_data_file[, "Timepoint"] == "F2",]
noNAsF2_ColorNamingZ <- na.omit(F2_ColorNamingZ$ColorNaming.Z)
mean(noNAsF2_ColorNamingZ)
sd(noNAsF2_ColorNamingZ)
#Post hoc tests
#Main Effect Test by Group (Tukey test)
aov(ColorNaming.Z ~ Group, data = your_data_file) %>% tukey_hsd()
#effect size for Groups
your_data_file%>%cohens_d(ColorNaming.Z~Group)
#interaction f/u tests -- Colour Naming (z-scores)
#subtract differences in F2 - F0 for Colour Naming 
ColorNaming.Z_diff_data<-your_data_file[,c("ParticipantID","Individual_number","Group","Timepoint","Age","Sex","ColorNaming.Z")]
F0_ColorNaming.Z_data<-ColorNaming.Z_diff_data[ColorNaming.Z_diff_data$Timepoint=='F0', ]
F2_ColorNaming.Z_data<-ColorNaming.Z_diff_data[ColorNaming.Z_diff_data$Timepoint=='F2', ]
ColorNaming.ZDiff<-F2_ColorNaming.Z_data$ColorNaming.Z - F0_ColorNaming.Z_data$ColorNaming.Z
F0_ColorNaming.Z_data$ColorNaming.ZDiff<-ColorNaming.ZDiff
#run one-way ANOVA on the F2-F0 differences in ColorNaming.Z between groups
ColorNaming.ZDiff_mod <- lm(ColorNaming.ZDiff ~ Group, data = F0_ColorNaming.Z_data)
anova(ColorNaming.ZDiff_mod) #sig. difference
#effect size omnibus ANOVA
etaSquared(ColorNaming.ZDiff_mod)
#post hoc f/u test
#run pairwise comparisons (post-hoc Tukey), given that the F-test was significant. 
post_hoc_ColorNaming.ZDiff_mod <- glht(ColorNaming.ZDiff_mod, linfct = mcp(Group = "Tukey"))
summary(post_hoc_ColorNaming.ZDiff_mod)
confint(post_hoc_ColorNaming.ZDiff_mod)
#effect size for Interaction
  #create dataframe
  #ColorNaming.Z_diff_data_124 <- your_data_file[1:124,c("ParticipantID","Individual_number","Group","Timepoint","Age","Sex","ColorNaming.Z")]
  ColorNaming.Z_diff_data_124 <- your_data_file[1:119,c("ParticipantID","Individual_number","Group","Timepoint","Age","Sex","ColorNaming.Z")]
  ColorNaming.Z_diff_data_124$ColorNaming.ZDiff <- ColorNaming.ZDiff 
  #calculate effect size
  ColorNaming.Z_diff_data_124%>%cohens_d(ColorNaming.ZDiff~Group)

#plot WordReading.Raw (raincloud plot)
ggplot(your_data_file, aes(x = Group, y = WordReading.Raw, fill = Timepoint)) + 
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(aes(y = WordReading.Raw, color = Timepoint), position = position_jitter(width = .15), size = .5, alpha = 0.8) +
  geom_boxplot(width = 0.1, fill = "white", outlier.size = 1, aes(colour = Timepoint)) + 
  stat_summary(fun = mean, geom = "point", shape = 19, size = 2, aes(colour = Timepoint)) + 
  xlab("Group") + 
  ylab("Stroop - Word Reading Raw (seconds)") +
  scale_x_discrete(labels = c("1" = "Control", "2" = "SCD", "3" = "aMCI", "4" = "mMCI", "5" = "AD")) + 
  theme_classic() +
  coord_flip()
#run mixed design, 2 x 5 ANOVA for WordReading.Raw
aov_WordReadingRaw <- anova_test(data=your_data_file, dv=WordReading.Raw, wid=Individual_number, between=Group, within=Timepoint, effect.size = "pes")
get_anova_table(aov_WordReadingRaw)
#check descriptive statistics per each group, per each timepoint
WordReading.Raw_descrip <- describeBy(your_data_file$WordReading.Raw, list(your_data_file$Group, your_data_file$Timepoint))
#find mean & SD from total sample in F0 and F2 timepoints:
F0_WordReadingRaw <- your_data_file[your_data_file[, "Timepoint"] == "F0",]
noNAsF0_WordReadingRaw <- na.omit(F0_WordReadingRaw$WordReading.Raw)
mean(noNAsF0_WordReadingRaw)
sd(noNAsF0_WordReadingRaw)
#F2
F2_WordReadingRaw <- your_data_file[your_data_file[, "Timepoint"] == "F2",]
noNAsF2_WordReadingRaw <- na.omit(F2_WordReadingRaw$WordReading.Raw)
mean(noNAsF2_WordReadingRaw)
sd(noNAsF2_WordReadingRaw)
#Post hoc tests
#non-sig. interaction - test by Group
aov(WordReading.Raw ~ Group, data = your_data_file) %>% tukey_hsd()
#effect size for Groups
your_data_file%>%cohens_d(WordReading.Raw~Group)
#non-sig. interaction - test by Time Point
aov(WordReading.Raw ~ Timepoint, data = your_data_file) %>% tukey_hsd()
#effect size for Time Point
your_data_file%>%cohens_d(WordReading.Raw~Timepoint, paired=TRUE)

#plot WordReading.Z (raincloud plot)
ggplot(your_data_file, aes(x = Group, y = WordReading.Z, fill = Timepoint)) + 
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(aes(y = WordReading.Z, color = Timepoint), position = position_jitter(width = .15), size = .5, alpha = 0.8) +
  geom_boxplot(width = 0.1, fill = "white", outlier.size = 1, aes(colour = Timepoint)) + 
  stat_summary(fun = mean, geom = "point", shape = 19, size = 2, aes(colour = Timepoint)) + 
  xlab("Group") + 
  ylab("Stroop - Word Reading Z-scores") +
  scale_x_discrete(labels = c("1" = "Control", "2" = "SCD", "3" = "aMCI", "4" = "mMCI", "5" = "AD")) + 
  theme_classic() +
  coord_flip()
#run mixed design, 2 x 5 ANOVA for WordReading.Z
aov_WordReadingZ <- anova_test(data=your_data_file, dv=WordReading.Z, wid=Individual_number, between=Group, within=Timepoint, effect.size = "pes")
get_anova_table(aov_WordReadingZ)
#check descriptive statistics per each group, per each timepoint
WordReading.Z_descrip <- describeBy(your_data_file$WordReading.Z, list(your_data_file$Group, your_data_file$Timepoint))
#find mean & SD from total sample in F0 and F2 timepoints:
F0_WordReadingZ <- your_data_file[your_data_file[, "Timepoint"] == "F0",]
noNAsF0_WordReadingZ <- na.omit(F0_WordReadingZ$WordReading.Z)
mean(noNAsF0_WordReadingZ)
sd(noNAsF0_WordReadingZ)
#F2
F2_WordReadingZ <- your_data_file[your_data_file[, "Timepoint"] == "F2",]
noNAsF2_WordReadingZ <- na.omit(F2_WordReadingZ$WordReading.Z)
mean(noNAsF2_WordReadingZ)
sd(noNAsF2_WordReadingZ)
#Post hoc tests
#non-sig. interaction - test by Group
aov(WordReading.Z ~ Group, data = your_data_file) %>% tukey_hsd()
#effect size for Groups
your_data_file%>%cohens_d(WordReading.Z~Group)

#plot Inhibition.Raw (raincloud plot)
ggplot(your_data_file, aes(x = Group, y = Inhibition.Raw, fill = Timepoint)) + 
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(aes(y = Inhibition.Raw, color = Timepoint), position = position_jitter(width = .15), size = .5, alpha = 0.8) +
  geom_boxplot(width = 0.1, fill = "white", outlier.size = 1, aes(colour = Timepoint)) + 
  stat_summary(fun = mean, geom = "point", shape = 19, size = 2, aes(colour = Timepoint)) + 
  xlab("Group") + 
  ylab("Stroop - Inhibition Raw (seconds)") +
  scale_x_discrete(labels = c("1" = "Control", "2" = "SCD", "3" = "aMCI", "4" = "mMCI", "5" = "AD")) + 
  theme_classic() +
  coord_flip()
#run mixed design, 2 x 5 ANOVA for Inhibition.Raw
aov_InhibitionRaw <- anova_test(data=your_data_file, dv=Inhibition.Raw, wid=Individual_number, between=Group, within=Timepoint, effect.size = "pes")
get_anova_table(aov_InhibitionRaw)
#check descriptive statistics per each group, per each timepoint
Inhibition.Raw_descrip <- describeBy(your_data_file$Inhibition.Raw, list(your_data_file$Group, your_data_file$Timepoint))
#find mean & SD from total sample in F0 and F2 timepoints:
F0_InhibitionRaw <- your_data_file[your_data_file[, "Timepoint"] == "F0",]
noNAsF0_InhibitionRaw <- na.omit(F0_InhibitionRaw$Inhibition.Raw)
mean(noNAsF0_InhibitionRaw)
sd(noNAsF0_InhibitionRaw)
#F2
F2_InhibitionRaw <- your_data_file[your_data_file[, "Timepoint"] == "F2",]
noNAsF2_InhibitionRaw <- na.omit(F2_InhibitionRaw$Inhibition.Raw)
mean(noNAsF2_InhibitionRaw)
sd(noNAsF2_InhibitionRaw)
#Post hoc tests
#non-sig. interaction - test by Group
aov(Inhibition.Raw ~ Group, data = your_data_file) %>% tukey_hsd()
#effect size for Groups
your_data_file%>%cohens_d(Inhibition.Raw~Group)
#non-sig. interaction - test by Time Point
aov(Inhibition.Raw ~ Timepoint, data = your_data_file) %>% tukey_hsd()
#effect size for Time Point
your_data_file%>%cohens_d(Inhibition.Raw~Timepoint, paired=TRUE)

#plot Inhibition.Z (raincloud plot)
ggplot(your_data_file, aes(x = Group, y = Inhibition.Z, fill = Timepoint)) + 
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(aes(y = Inhibition.Z, color = Timepoint), position = position_jitter(width = .15), size = .5, alpha = 0.8) +
  geom_boxplot(width = 0.1, fill = "white", outlier.size = 1, aes(colour = Timepoint)) + 
  stat_summary(fun = mean, geom = "point", shape = 19, size = 2, aes(colour = Timepoint)) + 
  xlab("Group") + 
  ylab("Stroop - Inhibition Z-scores") +
  scale_x_discrete(labels = c("1" = "Control", "2" = "SCD", "3" = "aMCI", "4" = "mMCI", "5" = "AD")) + 
  theme_classic() +
  coord_flip()
#run mixed design, 2 x 5 ANOVA for Inhibition.Z
aov_InhibitionZ <- anova_test(data=your_data_file, dv=Inhibition.Z, wid=Individual_number, between=Group, within=Timepoint, effect.size = "pes")
get_anova_table(aov_InhibitionZ)
#check descriptive statistics per each group, per each timepoint
Inhibition.Z_descrip <- describeBy(your_data_file$Inhibition.Z, list(your_data_file$Group, your_data_file$Timepoint))
#find mean & SD from total sample in F0 and F2 timepoints:
F0_InhibitionZ <- your_data_file[your_data_file[, "Timepoint"] == "F0",]
noNAsF0_InhibitionZ <- na.omit(F0_InhibitionZ$Inhibition.Z)
mean(noNAsF0_InhibitionZ)
sd(noNAsF0_InhibitionZ)
#F2
F2_InhibitionZ <- your_data_file[your_data_file[, "Timepoint"] == "F2",]
noNAsF2_InhibitionZ <- na.omit(F2_InhibitionZ$Inhibition.Z)
mean(noNAsF2_InhibitionZ)
sd(noNAsF2_InhibitionZ)
#Post hoc tests
#non-sig. interaction - test by Group
aov(Inhibition.Z ~ Group, data = your_data_file) %>% tukey_hsd()
#effect size for Groups
your_data_file%>%cohens_d(Inhibition.Z~Group)
#interaction f/u tests -- Inhibition (z-scores)
#subtract differences in F2 - F0 for Inhibition 
Inhibition.Z_diff_data<-your_data_file[,c("ParticipantID","Individual_number","Group","Timepoint","Age","Sex","Inhibition.Z")]
F0_Inhibition.Z_data<-Inhibition.Z_diff_data[Inhibition.Z_diff_data$Timepoint=='F0', ]
F2_Inhibition.Z_data<-Inhibition.Z_diff_data[Inhibition.Z_diff_data$Timepoint=='F2', ]
Inhibition.ZDiff<-F2_Inhibition.Z_data$Inhibition.Z - F0_Inhibition.Z_data$Inhibition.Z
F0_Inhibition.Z_data$Inhibition.ZDiff<-Inhibition.ZDiff
#run one-way ANOVA on the F2-F0 differences in Inhibition.Z between groups
Inhibition.ZDiff_mod <- lm(Inhibition.ZDiff ~ Group, data = F0_Inhibition.Z_data)
anova(Inhibition.ZDiff_mod) #sig. difference
#effect size omnibus ANOVA
etaSquared(Inhibition.ZDiff_mod)
#post hoc f/u test
#run pairwise comparisons (post-hoc Tukey), given that the F-test was significant. 
post_hoc_Inhibition.ZDiff_mod <- glht(Inhibition.ZDiff_mod, linfct = mcp(Group = "Tukey"))
summary(post_hoc_Inhibition.ZDiff_mod)
confint(post_hoc_Inhibition.ZDiff_mod)
#effect size for Interaction
  #create dataframe
  #Inhibition.Z_diff_data_124 <- your_data_file[1:124,c("ParticipantID","Individual_number","Group","Timepoint","Age","Sex","Inhibition.Z")]
  Inhibition.Z_diff_data_124 <- your_data_file[1:119,c("ParticipantID","Individual_number","Group","Timepoint","Age","Sex","Inhibition.Z")]
  Inhibition.Z_diff_data_124$Inhibition.ZDiff <- Inhibition.ZDiff 
  #calculate effect size
  Inhibition.Z_diff_data_124%>%cohens_d(Inhibition.ZDiff~Group)

#plot Inhibition.Colour.Naming (Inhibition / Colour Naming) (raincloud plot)
ggplot(your_data_file, aes(x = Group, y = Inhibition.Colour.Naming, fill = Timepoint)) + 
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(aes(y = Inhibition.Colour.Naming, color = Timepoint), position = position_jitter(width = .15), size = .5, alpha = 0.8) +
  geom_boxplot(width = 0.1, fill = "white", outlier.size = 1, aes(colour = Timepoint)) + 
  stat_summary(fun = mean, geom = "point", shape = 19, size = 2, aes(colour = Timepoint)) + 
  xlab("Group") + 
  ylab("Stroop - Inhibition / Colour Naming") +
  scale_x_discrete(labels = c("1" = "Control", "2" = "SCD", "3" = "aMCI", "4" = "mMCI", "5" = "AD")) + 
  theme_classic() +
  coord_flip()
#run mixed design, 2 x 5 ANOVA for Inhibition.Colour.Naming
aov_InhibitionColourNaming <- anova_test(data=your_data_file, dv=Inhibition.Colour.Naming, wid=Individual_number, between=Group, within=Timepoint, effect.size = "pes")
get_anova_table(aov_InhibitionColourNaming)
#check descriptive statistics per each group, per each timepoint
Inhibition.Colour.Naming_descrip <- describeBy(your_data_file$Inhibition.Colour.Naming, list(your_data_file$Group, your_data_file$Timepoint))
#find mean & SD from total sample in F0 and F2 timepoints:
F0_InhibitionColourNaming <- your_data_file[your_data_file[, "Timepoint"] == "F0",]
noNAsF0_InhibitionColourNaming <- na.omit(F0_InhibitionColourNaming$Inhibition.Colour.Naming)
mean(noNAsF0_InhibitionColourNaming)
sd(noNAsF0_InhibitionColourNaming)
#F2
F2_InhibitionColourNaming <- your_data_file[your_data_file[, "Timepoint"] == "F2",]
noNAsF2_InhibitionColourNaming <- na.omit(F2_InhibitionColourNaming$Inhibition.Colour.Naming)
mean(noNAsF2_InhibitionColourNaming)
sd(noNAsF2_InhibitionColourNaming)

#plot Inhibition.Colour.Naming Z-scores (Inhibition / Colour Naming.zscores.calc) (raincloud plot)
ggplot(your_data_file, aes(x = Group, y = Inhibition.Colour.Naming.zscores.calc., fill = Timepoint)) + 
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(aes(y = Inhibition.Colour.Naming.zscores.calc., color = Timepoint), position = position_jitter(width = .15), size = .5, alpha = 0.8) +
  geom_boxplot(width = 0.1, fill = "white", outlier.size = 1, aes(colour = Timepoint)) + 
  stat_summary(fun = mean, geom = "point", shape = 19, size = 2, aes(colour = Timepoint)) + 
  xlab("Group") + 
  ylab("Stroop - Inhibition / Colour Naming Z-scores") +
  scale_x_discrete(labels = c("1" = "Control", "2" = "SCD", "3" = "aMCI", "4" = "mMCI", "5" = "AD")) + 
  theme_classic() +
  coord_flip()
#run mixed design, 2 x 5 ANOVA for Inhibition.Colour.Naming
aov_InhibitionColourNamingZ <- anova_test(data=your_data_file, dv=Inhibition.Colour.Naming.zscores.calc., wid=Individual_number, between=Group, within=Timepoint, effect.size = "pes")
get_anova_table(aov_InhibitionColourNamingZ)
#check descriptive statistics per each group, per each timepoint
Inhibition.Colour.NamingZ_descrip <- describeBy(your_data_file$Inhibition.Colour.Naming.zscores.calc., list(your_data_file$Group, your_data_file$Timepoint))
#find mean & SD from total sample in F0 and F2 timepoints:
F0_InhibitionColourNamingZ <- your_data_file[your_data_file[, "Timepoint"] == "F0",]
noNAsF0_InhibitionColourNamingZ <- na.omit(F0_InhibitionColourNamingZ$Inhibition.Colour.Naming.zscores.calc.)
mean(noNAsF0_InhibitionColourNamingZ)
sd(noNAsF0_InhibitionColourNamingZ)
#F2
F2_InhibitionColourNamingZ <- your_data_file[your_data_file[, "Timepoint"] == "F2",]
noNAsF2_InhibitionColourNamingZ <- na.omit(F2_InhibitionColourNamingZ$Inhibition.Colour.Naming.zscores.calc.)
mean(noNAsF2_InhibitionColourNamingZ)
sd(noNAsF2_InhibitionColourNamingZ)

#plot Inhibition.Word.Reading (Inhibition / Word Reading) (raincloud plot)
ggplot(your_data_file, aes(x = Group, y = Inhibition.Word.Reading, fill = Timepoint)) + 
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(aes(y = Inhibition.Word.Reading, color = Timepoint), position = position_jitter(width = .15), size = .5, alpha = 0.8) +
  geom_boxplot(width = 0.1, fill = "white", outlier.size = 1, aes(colour = Timepoint)) + 
  stat_summary(fun = mean, geom = "point", shape = 19, size = 2, aes(colour = Timepoint)) + 
  xlab("Group") + 
  ylab("Stroop - Inhibition / Word Reading") +
  scale_x_discrete(labels = c("1" = "Control", "2" = "SCD", "3" = "aMCI", "4" = "mMCI", "5" = "AD")) + 
  theme_classic() +
  coord_flip()
#run mixed design, 2 x 5 ANOVA for Inhibition.Word.Reading
aov_InhibitionWordReading <- anova_test(data=your_data_file, dv=Inhibition.Word.Reading, wid=Individual_number, between=Group, within=Timepoint, effect.size = "pes")
get_anova_table(aov_InhibitionWordReading)
#check descriptive statistics per each group, per each timepoint
Inhibition.Word.Reading_descrip <- describeBy(your_data_file$Inhibition.Word.Reading, list(your_data_file$Group, your_data_file$Timepoint))
#find mean & SD from total sample in F0 and F2 timepoints:
F0_InhibitionWordReading <- your_data_file[your_data_file[, "Timepoint"] == "F0",]
noNAsF0_InhibitionWordReading <- na.omit(F0_InhibitionWordReading$Inhibition.Word.Reading)
mean(noNAsF0_InhibitionWordReading)
sd(noNAsF0_InhibitionWordReading)
#F2
F2_InhibitionWordReading <- your_data_file[your_data_file[, "Timepoint"] == "F2",]
noNAsF2_InhibitionWordReading <- na.omit(F2_InhibitionWordReading$Inhibition.Word.Reading)
mean(noNAsF2_InhibitionWordReading)
sd(noNAsF2_InhibitionWordReading)
#Post hoc tests
#non-sig. interaction - test by Group
aov(Inhibition.Word.Reading ~ Group, data = your_data_file) %>% tukey_hsd()
#effect size for Groups
your_data_file%>%cohens_d(Inhibition.Word.Reading~Group)

#plot Inhibition.Word.Reading Z-scores (Inhibition / Colour Naming.zscores.calc) (raincloud plot)
ggplot(your_data_file, aes(x = Group, y = Inhibition.Word.Reading.zscores.calc., fill = Timepoint)) + 
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(aes(y = Inhibition.Word.Reading.zscores.calc., color = Timepoint), position = position_jitter(width = .15), size = .5, alpha = 0.8) +
  geom_boxplot(width = 0.1, fill = "white", outlier.size = 1, aes(colour = Timepoint)) + 
  stat_summary(fun = mean, geom = "point", shape = 19, size = 2, aes(colour = Timepoint)) + 
  xlab("Group") + 
  ylab("Stroop - Inhibition / Word Reading Z-scores") +
  scale_x_discrete(labels = c("1" = "Control", "2" = "SCD", "3" = "aMCI", "4" = "mMCI", "5" = "AD")) + 
  theme_classic() +
  coord_flip()
#run mixed design, 2 x 5 ANOVA for Inhibition.Word.Reading
aov_InhibitionWordReadingZ <- anova_test(data=your_data_file, dv=Inhibition.Word.Reading.zscores.calc., wid=Individual_number, between=Group, within=Timepoint, effect.size = "pes")
get_anova_table(aov_InhibitionWordReadingZ)
#check descriptive statistics per each group, per each timepoint
Inhibition.Word.ReadingZ_descrip <- describeBy(your_data_file$Inhibition.Word.Reading.zscores.calc., list(your_data_file$Group, your_data_file$Timepoint))
#find mean & SD from total sample in F0 and F2 timepoints:
F0_InhibitionWordReadingZ <- your_data_file[your_data_file[, "Timepoint"] == "F0",]
noNAsF0_InhibitionWordReadingZ <- na.omit(F0_InhibitionWordReadingZ$Inhibition.Word.Reading.zscores.calc.)
mean(noNAsF0_InhibitionWordReadingZ)
sd(noNAsF0_InhibitionWordReadingZ)
#F2
F2_InhibitionWordReadingZ <- your_data_file[your_data_file[, "Timepoint"] == "F2",]
noNAsF2_InhibitionWordReadingZ <- na.omit(F2_InhibitionWordReadingZ$Inhibition.Word.Reading.zscores.calc.)
mean(noNAsF2_InhibitionWordReadingZ)
sd(noNAsF2_InhibitionWordReadingZ)
#Post hoc tests
#non-sig. interaction - test by Group
aov(Inhibition.Word.Reading.zscores.calc. ~ Group, data = your_data_file) %>% tukey_hsd()
#effect size for Groups
your_data_file%>%cohens_d(Inhibition.Word.Reading.zscores.calc.~Group)

# #plot Golden_Stroop_interference_score (raincloud plot)
# ggplot(your_data_file, aes(x = Group, y = Golden_Stroop_interference_score, fill = Timepoint)) + 
#   geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
#   geom_point(aes(y = Golden_Stroop_interference_score, color = Timepoint), position = position_jitter(width = .15), size = .5, alpha = 0.8) +
#   geom_boxplot(width = 0.1, fill = "white", outlier.size = 1, aes(colour = Timepoint)) + 
#   stat_summary(fun = mean, geom = "point", shape = 19, size = 2, aes(colour = Timepoint)) + 
#   xlab("Group") + 
#   ylab("Stroop - Inteference Score (Golden, 1978)") +
#   scale_x_discrete(labels = c("1" = "Control", "2" = "SCD", "3" = "aMCI", "4" = "mMCI", "5" = "AD")) + 
#   theme_classic() +
#   coord_flip()
# #run mixed design, 2 x 5 ANOVA for Golden_Stroop_interference_score
# aov_Golden_Stroop_interference_score <- anova_test(data=your_data_file, dv=Golden_Stroop_interference_score, wid=Individual_number, between=Group, within=Timepoint, effect.size = "pes")
# get_anova_table(aov_Golden_Stroop_interference_score)
# #check descriptive statistics per each group, per each timepoint
# Golden_Stroop_interference_score_descrip <- describeBy(your_data_file$Golden_Stroop_interference_score, list(your_data_file$Group, your_data_file$Timepoint))
# #find mean & SD from total sample in F0 and F2 timepoints:
# F0_Golden_Stroop_interference_score <- your_data_file[your_data_file[, "Timepoint"] == "F0",]
# noNAsF0_Golden_Stroop_interference_score <- na.omit(F0_Golden_Stroop_interference_score$Golden_Stroop_interference_score)
# mean(noNAsF0_Golden_Stroop_interference_score)
# sd(noNAsF0_Golden_Stroop_interference_score)
# #F2
# F2_Golden_Stroop_interference_score <- your_data_file[your_data_file[, "Timepoint"] == "F2",]
# noNAsF2_Golden_Stroop_interference_score <- na.omit(F2_Golden_Stroop_interference_score$Golden_Stroop_interference_score)
# mean(noNAsF2_Golden_Stroop_interference_score)
# sd(noNAsF2_Golden_Stroop_interference_score)
# 
# #plot Golden_Stroop_interference_score.z.calculated. (raincloud plot)
# ggplot(your_data_file, aes(x = Group, y = Golden_Stroop_interference_score.z.calculated., fill = Timepoint)) + 
#   geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
#   geom_point(aes(y = Golden_Stroop_interference_score.z.calculated., color = Timepoint), position = position_jitter(width = .15), size = .5, alpha = 0.8) +
#   geom_boxplot(width = 0.1, fill = "white", outlier.size = 1, aes(colour = Timepoint)) + 
#   stat_summary(fun = mean, geom = "point", shape = 19, size = 2, aes(colour = Timepoint)) + 
#   xlab("Group") + 
#   ylab("Stroop - Inteference Score Z-scores (Golden, 1978)") +
#   scale_x_discrete(labels = c("1" = "Control", "2" = "SCD", "3" = "aMCI", "4" = "mMCI", "5" = "AD")) + 
#   theme_classic() +
#   coord_flip()
# #run mixed design, 2 x 5 ANOVA for Golden_Stroop_interference_score.z.calculated.
# aov_Golden_Stroop_interference_score.z.calculated. <- anova_test(data=your_data_file, dv=Golden_Stroop_interference_score.z.calculated., wid=Individual_number, between=Group, within=Timepoint, effect.size = "pes")
# get_anova_table(aov_Golden_Stroop_interference_score.z.calculated.)
# #check descriptive statistics per each group, per each timepoint
# Golden_Stroop_interference_score.z.calculated._descrip <- describeBy(your_data_file$Golden_Stroop_interference_score.z.calculated., list(your_data_file$Group, your_data_file$Timepoint))
# #find mean & SD from total sample in F0 and F2 timepoints:
# F0_Golden_Stroop_interference_score.z.calculated. <- your_data_file[your_data_file[, "Timepoint"] == "F0",]
# noNAsF0_Golden_Stroop_interference_score.z.calculated. <- na.omit(F0_Golden_Stroop_interference_score.z.calculated.$Golden_Stroop_interference_score.z.calculated.)
# mean(noNAsF0_Golden_Stroop_interference_score.z.calculated.)
# sd(noNAsF0_Golden_Stroop_interference_score.z.calculated.)
# #F2
# F2_Golden_Stroop_interference_score.z.calculated. <- your_data_file[your_data_file[, "Timepoint"] == "F2",]
# noNAsF2_Golden_Stroop_interference_score.z.calculated. <- na.omit(F2_Golden_Stroop_interference_score.z.calculated.$Golden_Stroop_interference_score.z.calculated.)
# mean(noNAsF2_Golden_Stroop_interference_score.z.calculated.)
# sd(noNAsF2_Golden_Stroop_interference_score.z.calculated.)

#plot Interference_score_Caffarra_2002 (raincloud plot)
ggplot(your_data_file, aes(x = Group, y = Interference_score_Caffarra_2002, fill = Timepoint)) + 
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(aes(y = Interference_score_Caffarra_2002, color = Timepoint), position = position_jitter(width = .15), size = .5, alpha = 0.8) +
  geom_boxplot(width = 0.1, fill = "white", outlier.size = 1, aes(colour = Timepoint)) + 
  stat_summary(fun = mean, geom = "point", shape = 19, size = 2, aes(colour = Timepoint)) + 
  xlab("Group") + 
  ylab("Stroop - Inteference Score") +
  scale_x_discrete(labels = c("1" = "Control", "2" = "SCD", "3" = "aMCI", "4" = "mMCI", "5" = "AD")) + 
  theme_classic() +
  coord_flip()
#run mixed design, 2 x 5 ANOVA for Interference_score_Caffarra_2002
aov_Interference_score_Caffarra_2002 <- anova_test(data=your_data_file, dv=Interference_score_Caffarra_2002, wid=Individual_number, between=Group, within=Timepoint, effect.size = "pes")
get_anova_table(aov_Interference_score_Caffarra_2002)
#check descriptive statistics per each group, per each timepoint
Interference_score_Caffarra_2002_descrip <- describeBy(your_data_file$Interference_score_Caffarra_2002, list(your_data_file$Group, your_data_file$Timepoint))
#find mean & SD from total sample in F0 and F2 timepoints:
F0_Interference_score_Caffarra_2002 <- your_data_file[your_data_file[, "Timepoint"] == "F0",]
noNAsF0_Interference_score_Caffarra_2002 <- na.omit(F0_Interference_score_Caffarra_2002$Interference_score_Caffarra_2002)
mean(noNAsF0_Interference_score_Caffarra_2002)
sd(noNAsF0_Interference_score_Caffarra_2002)
#F2
F2_Interference_score_Caffarra_2002 <- your_data_file[your_data_file[, "Timepoint"] == "F2",]
noNAsF2_Interference_score_Caffarra_2002 <- na.omit(F2_Interference_score_Caffarra_2002$Interference_score_Caffarra_2002)
mean(noNAsF2_Interference_score_Caffarra_2002)
sd(noNAsF2_Interference_score_Caffarra_2002)
#Post hoc tests
#non-sig. interaction - test by Group
aov(Interference_score_Caffarra_2002 ~ Group, data = your_data_file) %>% tukey_hsd()
#effect size for Groups
your_data_file%>%cohens_d(Interference_score_Caffarra_2002~Group)

#plot Interference_zscore_Caffarra_2002 (raincloud plot)
ggplot(your_data_file, aes(x = Group, y = Interference_zscore_Caffarra_2002, fill = Timepoint)) + 
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(aes(y = Interference_zscore_Caffarra_2002, color = Timepoint), position = position_jitter(width = .15), size = .5, alpha = 0.8) +
  geom_boxplot(width = 0.1, fill = "white", outlier.size = 1, aes(colour = Timepoint)) + 
  stat_summary(fun = mean, geom = "point", shape = 19, size = 2, aes(colour = Timepoint)) + 
  xlab("Group") + 
  ylab("Stroop - Inteference Score Z-scores (Golden, 1978)") +
  scale_x_discrete(labels = c("1" = "Control", "2" = "SCD", "3" = "aMCI", "4" = "mMCI", "5" = "AD")) + 
  theme_classic() +
  coord_flip()
#run mixed design, 2 x 5 ANOVA for Interference_zscore_Caffarra_2002
aov_Interference_zscore_Caffarra_2002 <- anova_test(data=your_data_file, dv=Interference_zscore_Caffarra_2002, wid=Individual_number, between=Group, within=Timepoint, effect.size = "pes")
get_anova_table(aov_Interference_zscore_Caffarra_2002)
#check descriptive statistics per each group, per each timepoint
Interference_zscore_Caffarra_2002_descrip <- describeBy(your_data_file$Interference_zscore_Caffarra_2002, list(your_data_file$Group, your_data_file$Timepoint))
#find mean & SD from total sample in F0 and F2 timepoints:
F0_Interference_zscore_Caffarra_2002 <- your_data_file[your_data_file[, "Timepoint"] == "F0",]
noNAsF0_Interference_zscore_Caffarra_2002 <- na.omit(F0_Interference_zscore_Caffarra_2002$Interference_zscore_Caffarra_2002)
mean(noNAsF0_Interference_zscore_Caffarra_2002)
sd(noNAsF0_Interference_zscore_Caffarra_2002)
#F2
F2_Interference_zscore_Caffarra_2002 <- your_data_file[your_data_file[, "Timepoint"] == "F2",]
noNAsF2_Interference_zscore_Caffarra_2002 <- na.omit(F2_Interference_zscore_Caffarra_2002$Interference_zscore_Caffarra_2002)
mean(noNAsF2_Interference_zscore_Caffarra_2002)
sd(noNAsF2_Interference_zscore_Caffarra_2002)
#Post hoc tests
#non-sig. interaction - test by Group
aov(Interference_zscore_Caffarra_2002 ~ Group, data = your_data_file) %>% tukey_hsd()
#effect size for Groups
your_data_file%>%cohens_d(Interference_zscore_Caffarra_2002~Group)

#4.D-KEFS Verbal Fluency + Category Fluency Task ------------------------------#
#plot LetFluency.Raw (raincloud plot)
ggplot(your_data_file, aes(x = Group, y = LetFluency.Raw, fill = Timepoint)) + 
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(aes(y = LetFluency.Raw, color = Timepoint), position = position_jitter(width = .15), size = .5, alpha = 0.8) +
  geom_boxplot(width = 0.1, fill = "white", outlier.size = 1, aes(colour = Timepoint)) + 
  stat_summary(fun = mean, geom = "point", shape = 19, size = 2, aes(colour = Timepoint)) + 
  xlab("Group") + 
  ylab("Letter Fluency (total correct responses)") +
  scale_x_discrete(labels = c("1" = "Control", "2" = "SCD", "3" = "aMCI", "4" = "mMCI", "5" = "AD")) + 
  theme_classic() +
  coord_flip()
#run mixed design, 2 x 5 ANOVA for LetFluency.Raw
aov_LetFluencyRaw <- anova_test(data=your_data_file, dv=LetFluency.Raw, wid=Individual_number, between=Group, within=Timepoint, effect.size = "pes")
get_anova_table(aov_LetFluencyRaw)
#check descriptive statistics per each group, per each timepoint
LetFluency.Raw_descrip <- describeBy(your_data_file$LetFluency.Raw, list(your_data_file$Group, your_data_file$Timepoint))
#find mean & SD from total sample in F0 and F2 timepoints:
F0_LetFluencyRaw <- your_data_file[your_data_file[, "Timepoint"] == "F0",]
noNAsF0_LetFluencyRaw <- na.omit(F0_LetFluencyRaw$LetFluency.Raw)
mean(noNAsF0_LetFluencyRaw)
sd(noNAsF0_LetFluencyRaw)
#F2
F2_LetFluencyRaw <- your_data_file[your_data_file[, "Timepoint"] == "F2",]
noNAsF2_LetFluencyRaw <- na.omit(F2_LetFluencyRaw$LetFluency.Raw)
mean(noNAsF2_LetFluencyRaw)
sd(noNAsF2_LetFluencyRaw)
#Post hoc tests
#non-sig. interaction - test by Group
aov(LetFluency.Raw ~ Group, data = your_data_file) %>% tukey_hsd()
#effect size for Groups
your_data_file%>%cohens_d(LetFluency.Raw~Group)
#plot interaction 
#remove NAs for LetFluency.Raw variable
noNAs_LetFluency.Raw <- your_data_file[,c("ParticipantID","Individual_number","Group","Timepoint","Age","Sex","LetFluency.Raw")]
noNAs_LetFluency.Raw<- noNAs_LetFluency.Raw[complete.cases(noNAs_LetFluency.Raw), ]
#view interaction plot - by Group
noNAs_LetFluency.Raw%>%
  group_by(Group,Timepoint) %>%
  summarise(s_mean=mean(LetFluency.Raw)) %>%
  ggplot(aes(y=s_mean,x=Group,colour=Timepoint,group=Timepoint))+
  geom_point()+geom_line()+
  scale_x_discrete(labels = c("1" = "C", "2" = "SCD", "3" = "aMCI", "4" = "mMCI", "5" = "AD")) + 
  ylab("Letter Fluency (total correct responses)") +
  theme_classic()
#view interaction plot - by Timepoint
noNAs_LetFluency.Raw %>%
  group_by(Group,Timepoint) %>%
  summarise(s_mean=mean(LetFluency.Raw)) %>%
  ggplot(aes(y=s_mean,x=Timepoint,colour=Group,group=Group))+
  geom_point()+geom_line()+
  scale_color_discrete(labels = c("1" = "C", "2" = "SCD", "3" = "aMCI", "4" = "mMCI", "5" = "AD")) + 
  ylab("Letter Fluency (total correct responses)") +
  theme_classic()
#interaction f/u tests -- Letter Fluency
#subtract differences in F2 - F0 for Letter Fluency
LetFluency.Raw_diff_data<-your_data_file[,c("ParticipantID","Individual_number","Group","Timepoint","Age","Sex","LetFluency.Raw")]
F0_LetFluency.Raw_data<-LetFluency.Raw_diff_data[LetFluency.Raw_diff_data$Timepoint=='F0', ]
F2_LetFluency.Raw_data<-LetFluency.Raw_diff_data[LetFluency.Raw_diff_data$Timepoint=='F2', ]
LetFluency.RawDiff<-F2_LetFluency.Raw_data$LetFluency.Raw - F0_LetFluency.Raw_data$LetFluency.Raw
F0_LetFluency.Raw_data$LetFluency.RawDiff<-LetFluency.RawDiff
#run one-way ANOVA on the F2-F0 differences in LetFluency.Raw between groups
LetFluency.RawDiff_mod <- lm(LetFluency.RawDiff ~ Group, data = F0_LetFluency.Raw_data)
anova(LetFluency.RawDiff_mod) #sig. difference
#effect size omnibus ANOVA
etaSquared(LetFluency.RawDiff_mod)
#post hoc f/u test
#run pairwise comparisons (post-hoc Tukey), given that the F-test was significant. 
post_hoc_LetFluency.RawDiff_mod <- glht(LetFluency.RawDiff_mod, linfct = mcp(Group = "Tukey"))
summary(post_hoc_LetFluency.RawDiff_mod)
confint(post_hoc_LetFluency.RawDiff_mod)
#effect size for Interaction
  #create dataframe
  #LetFluency.Raw_diff_data_124 <- your_data_file[1:124,c("ParticipantID","Individual_number","Group","Timepoint","Age","Sex","LetFluency.Raw")]
  LetFluency.Raw_diff_data_124 <- your_data_file[1:119,c("ParticipantID","Individual_number","Group","Timepoint","Age","Sex","LetFluency.Raw")]
  LetFluency.Raw_diff_data_124$LetFluency.RawDiff <- LetFluency.RawDiff 
  #calculate effect size
  LetFluency.Raw_diff_data_124%>%cohens_d(LetFluency.RawDiff~Group)

#plot LetFluency.Z (raincloud plot)
ggplot(your_data_file, aes(x = Group, y = LetFluency.Z, fill = Timepoint)) + 
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(aes(y = LetFluency.Z, color = Timepoint), position = position_jitter(width = .15), size = .5, alpha = 0.8) +
  geom_boxplot(width = 0.1, fill = "white", outlier.size = 1, aes(colour = Timepoint)) + 
  stat_summary(fun = mean, geom = "point", shape = 19, size = 2, aes(colour = Timepoint)) + 
  xlab("Group") + 
  ylab("Letter Fluency Z-scores") +
  scale_x_discrete(labels = c("1" = "Control", "2" = "SCD", "3" = "aMCI", "4" = "mMCI", "5" = "AD")) + 
  theme_classic() +
  coord_flip()
#run mixed design, 2 x 5 ANOVA for LetFluency.Z
aov_LetFluencyZ <- anova_test(data=your_data_file, dv=LetFluency.Z, wid=Individual_number, between=Group, within=Timepoint, effect.size = "pes")
get_anova_table(aov_LetFluencyZ)
#check descriptive statistics per each group, per each timepoint
LetFluency.Z_descrip <- describeBy(your_data_file$LetFluency.Z, list(your_data_file$Group, your_data_file$Timepoint))
#find mean & SD from total sample in F0 and F2 timepoints:
F0_LetFluencyZ <- your_data_file[your_data_file[, "Timepoint"] == "F0",]
noNAsF0_LetFluencyZ <- na.omit(F0_LetFluencyZ$LetFluency.Z)
mean(noNAsF0_LetFluencyZ)
sd(noNAsF0_LetFluencyZ)
#F2
F2_LetFluencyZ <- your_data_file[your_data_file[, "Timepoint"] == "F2",]
noNAsF2_LetFluencyZ <- na.omit(F2_LetFluencyZ$LetFluency.Z)
mean(noNAsF2_LetFluencyZ)
sd(noNAsF2_LetFluencyZ)
#Post hoc tests
#non-sig. interaction - test by Group
aov(LetFluency.Z ~ Group, data = your_data_file) %>% tukey_hsd()
#effect size for Groups
your_data_file%>%cohens_d(LetFluency.Z~Group)
#interaction f/u tests -- Letter Fluency (z-scores)
#subtract differences in F2 - F0 for Letter Fluency (z-scores)
LetFluency.Z_diff_data<-your_data_file[,c("ParticipantID","Individual_number","Group","Timepoint","Age","Sex","LetFluency.Z")]
F0_LetFluency.Z_data<-LetFluency.Z_diff_data[LetFluency.Z_diff_data$Timepoint=='F0', ]
F2_LetFluency.Z_data<-LetFluency.Z_diff_data[LetFluency.Z_diff_data$Timepoint=='F2', ]
LetFluency.ZDiff<-F2_LetFluency.Z_data$LetFluency.Z - F0_LetFluency.Z_data$LetFluency.Z
F0_LetFluency.Z_data$LetFluency.ZDiff<-LetFluency.ZDiff
#run one-way ANOVA on the F2-F0 differences in LetFluency.Z between groups
LetFluency.ZDiff_mod <- lm(LetFluency.ZDiff ~ Group, data = F0_LetFluency.Z_data)
anova(LetFluency.ZDiff_mod) #sig. difference
#effect size omnibus ANOVA
etaSquared(LetFluency.ZDiff_mod)
#post hoc f/u test
#run pairwise comparisons (post-hoc Tukey), given that the F-test was significant. 
post_hoc_LetFluency.ZDiff_mod <- glht(LetFluency.ZDiff_mod, linfct = mcp(Group = "Tukey"))
summary(post_hoc_LetFluency.ZDiff_mod)
confint(post_hoc_LetFluency.ZDiff_mod)
#effect size for Interaction
  #create dataframe
  LetFluency.Z_diff_data_124 <- your_data_file[1:124,c("ParticipantID","Individual_number","Group","Timepoint","Age","Sex","LetFluency.Z")]
  LetFluency.Z_diff_data_124$LetFluency.ZDiff <- LetFluency.ZDiff 
  #calculate effect size
  LetFluency.Z_diff_data_124%>%cohens_d(LetFluency.ZDiff~Group)

#plot CatFluency.Raw (raincloud plot)
ggplot(your_data_file, aes(x = Group, y = CatFluency.Raw, fill = Timepoint)) + 
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(aes(y = CatFluency.Raw, color = Timepoint), position = position_jitter(width = .15), size = .5, alpha = 0.8) +
  geom_boxplot(width = 0.1, fill = "white", outlier.size = 1, aes(colour = Timepoint)) + 
  stat_summary(fun = mean, geom = "point", shape = 19, size = 2, aes(colour = Timepoint)) + 
  xlab("Group") + 
  ylab("Category Fluency (total correct responses)") +
  scale_x_discrete(labels = c("1" = "Control", "2" = "SCD", "3" = "aMCI", "4" = "mMCI", "5" = "AD")) + 
  theme_classic() +
  coord_flip()
#run mixed design, 2 x 5 ANOVA for CatFluency.Raw
aov_CatFluencyRaw <- anova_test(data=your_data_file, dv=CatFluency.Raw, wid=Individual_number, between=Group, within=Timepoint, effect.size = "pes")
get_anova_table(aov_CatFluencyRaw)
#check descriptive statistics per each group, per each timepoint
CatFluency.Raw_descrip <- describeBy(your_data_file$CatFluency.Raw, list(your_data_file$Group, your_data_file$Timepoint))
#find mean & SD from total sample in F0 and F2 timepoints:
F0_CatFluencyRaw <- your_data_file[your_data_file[, "Timepoint"] == "F0",]
noNAsF0_CatFluencyRaw <- na.omit(F0_CatFluencyRaw$CatFluency.Raw)
mean(noNAsF0_CatFluencyRaw)
sd(noNAsF0_CatFluencyRaw)
#F2
F2_CatFluencyRaw <- your_data_file[your_data_file[, "Timepoint"] == "F2",]
noNAsF2_CatFluencyRaw <- na.omit(F2_CatFluencyRaw$CatFluency.Raw)
mean(noNAsF2_CatFluencyRaw)
sd(noNAsF2_CatFluencyRaw)
#Post hoc tests
#non-sig. interaction - test by Group
aov(CatFluency.Raw ~ Group, data = your_data_file) %>% tukey_hsd()
#effect size for Groups
your_data_file%>%cohens_d(CatFluency.Raw~Group)

#plot CatFluency.Z (raincloud plot)
ggplot(your_data_file, aes(x = Group, y = CatFluency.Z, fill = Timepoint)) + 
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(aes(y = CatFluency.Z, color = Timepoint), position = position_jitter(width = .15), size = .5, alpha = 0.8) +
  geom_boxplot(width = 0.1, fill = "white", outlier.size = 1, aes(colour = Timepoint)) + 
  stat_summary(fun = mean, geom = "point", shape = 19, size = 2, aes(colour = Timepoint)) + 
  xlab("Group") + 
  ylab("Category Fluency (total correct responses)") +
  scale_x_discrete(labels = c("1" = "Control", "2" = "SCD", "3" = "aMCI", "4" = "mMCI", "5" = "AD")) + 
  theme_classic() +
  coord_flip()
#run mixed design, 2 x 5 ANOVA for CatFluency.Z
aov_CatFluencyZ <- anova_test(data=your_data_file, dv=CatFluency.Z, wid=Individual_number, between=Group, within=Timepoint, effect.size = "pes")
get_anova_table(aov_CatFluencyZ)
#check descriptive statistics per each group, per each timepoint
CatFluency.Z_descrip <- describeBy(your_data_file$CatFluency.Z, list(your_data_file$Group, your_data_file$Timepoint))
#find mean & SD from total sample in F0 and F2 timepoints:
F0_CatFluencyZ <- your_data_file[your_data_file[, "Timepoint"] == "F0",]
noNAsF0_CatFluencyZ <- na.omit(F0_CatFluencyZ$CatFluency.Z)
mean(noNAsF0_CatFluencyZ)
sd(noNAsF0_CatFluencyZ)
#F2
F2_CatFluencyZ <- your_data_file[your_data_file[, "Timepoint"] == "F2",]
noNAsF2_CatFluencyZ <- na.omit(F2_CatFluencyZ$CatFluency.Z)
mean(noNAsF2_CatFluencyZ)
sd(noNAsF2_CatFluencyZ)
#Post hoc tests
#non-sig. interaction - test by Group
aov(CatFluency.Z ~ Group, data = your_data_file) %>% tukey_hsd()
#effect size for Groups
your_data_file%>%cohens_d(CatFluency.Z~Group)

#plot Switching.Raw (raincloud plot)
ggplot(your_data_file, aes(x = Group, y = Switching.Raw, fill = Timepoint)) + 
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(aes(y = Switching.Raw, color = Timepoint), position = position_jitter(width = .15), size = .5, alpha = 0.8) +
  geom_boxplot(width = 0.1, fill = "white", outlier.size = 1, aes(colour = Timepoint)) + 
  stat_summary(fun = mean, geom = "point", shape = 19, size = 2, aes(colour = Timepoint)) + 
  xlab("Group") + 
  ylab("Category Switching (total correct responses)") +
  scale_x_discrete(labels = c("1" = "Control", "2" = "SCD", "3" = "aMCI", "4" = "mMCI", "5" = "AD")) + 
  theme_classic() +
  coord_flip()
#run mixed design, 2 x 5 ANOVA for CatFluency.Raw
aov_SwitchingRaw <- anova_test(data=your_data_file, dv=Switching.Raw, wid=Individual_number, between=Group, within=Timepoint, effect.size = "pes")
get_anova_table(aov_SwitchingRaw)
#check descriptive statistics per each group, per each timepoint
Switching.Raw_descrip <- describeBy(your_data_file$Switching.Raw, list(your_data_file$Group, your_data_file$Timepoint))
#find mean & SD from total sample in F0 and F2 timepoints:
F0_SwitchingRaw <- your_data_file[your_data_file[, "Timepoint"] == "F0",]
noNAsF0_SwitchingRaw <- na.omit(F0_SwitchingRaw$Switching.Raw)
mean(noNAsF0_SwitchingRaw)
sd(noNAsF0_SwitchingRaw)
#F2
F2_SwitchingRaw <- your_data_file[your_data_file[, "Timepoint"] == "F2",]
noNAsF2_SwitchingRaw <- na.omit(F2_SwitchingRaw$Switching.Raw)
mean(noNAsF2_SwitchingRaw)
sd(noNAsF2_SwitchingRaw)
#Post hoc tests
#non-sig. interaction - test by Group
aov(Switching.Raw ~ Group, data = your_data_file) %>% tukey_hsd()
#effect size for Groups
your_data_file%>%cohens_d(Switching.Raw~Group)
#non-sig. interaction - test by Time Point
aov(Switching.Raw ~ Timepoint, data = your_data_file) %>% tukey_hsd()
#effect size for Time Point
your_data_file%>%cohens_d(Switching.Raw~Timepoint, paired=TRUE)

#plot Switching.z (raincloud plot)
ggplot(your_data_file, aes(x = Group, y = Switching.z, fill = Timepoint)) + 
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(aes(y = Switching.z, color = Timepoint), position = position_jitter(width = .15), size = .5, alpha = 0.8) +
  geom_boxplot(width = 0.1, fill = "white", outlier.size = 1, aes(colour = Timepoint)) + 
  stat_summary(fun = mean, geom = "point", shape = 19, size = 2, aes(colour = Timepoint)) + 
  xlab("Group") + 
  ylab("Category Switching Z-scores") +
  scale_x_discrete(labels = c("1" = "Control", "2" = "SCD", "3" = "aMCI", "4" = "mMCI", "5" = "AD")) + 
  theme_classic() +
  coord_flip()
#run mixed design, 2 x 5 ANOVA for CatFluency.z
aov_Switchingz <- anova_test(data=your_data_file, dv=Switching.z, wid=Individual_number, between=Group, within=Timepoint, effect.size = "pes")
get_anova_table(aov_Switchingz)
#check descriptive statistics per each group, per each timepoint
Switching.z_descrip <- describeBy(your_data_file$Switching.z, list(your_data_file$Group, your_data_file$Timepoint))
#find mean & SD from total sample in F0 and F2 timepoints:
F0_Switchingz <- your_data_file[your_data_file[, "Timepoint"] == "F0",]
noNAsF0_Switchingz <- na.omit(F0_Switchingz$Switching.z)
mean(noNAsF0_Switchingz)
sd(noNAsF0_Switchingz)
#F2
F2_Switchingz <- your_data_file[your_data_file[, "Timepoint"] == "F2",]
noNAsF2_Switchingz <- na.omit(F2_Switchingz$Switching.z)
mean(noNAsF2_Switchingz)
sd(noNAsF2_Switchingz)
#Post hoc tests
#non-sig. interaction - test by Group
aov(Switching.z ~ Group, data = your_data_file) %>% tukey_hsd()
#effect size for Groups
your_data_file%>%cohens_d(Switching.z~Group)

#5.Trail Making Test (TMT) A & B ----------------------------------------------#
#plot TrailsA.Raw (raincloud plot)
ggplot(your_data_file, aes(x = Group, y = TrailsA.Raw, fill = Timepoint)) + 
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(aes(y = TrailsA.Raw, color = Timepoint), position = position_jitter(width = .15), size = .5, alpha = 0.8) +
  geom_boxplot(width = 0.1, fill = "white", outlier.size = 1, aes(colour = Timepoint)) + 
  stat_summary(fun = mean, geom = "point", shape = 19, size = 2, aes(colour = Timepoint)) + 
  xlab("Group") + 
  ylab("Trail Making Test A Raw (seconds)") +
  scale_x_discrete(labels = c("1" = "Control", "2" = "SCD", "3" = "aMCI", "4" = "mMCI", "5" = "AD")) + 
  theme_classic() +
  coord_flip()
#run mixed design, 2 x 5 ANOVA for TrailsA.Raw
aov_TrailsARaw <- anova_test(data=your_data_file, dv=TrailsA.Raw, wid=Individual_number, between=Group, within=Timepoint, effect.size = "pes")
get_anova_table(aov_TrailsARaw)
#check descriptive statistics per each group, per each timepoint
TrailsA.Raw_descrip <- describeBy(your_data_file$TrailsA.Raw, list(your_data_file$Group, your_data_file$Timepoint))
#find mean & SD from total sample in F0 and F2 timepoints:
F0_TrailsARaw <- your_data_file[your_data_file[, "Timepoint"] == "F0",]
noNAsF0_TrailsARaw <- na.omit(F0_TrailsARaw$TrailsA.Raw)
mean(noNAsF0_TrailsARaw)
sd(noNAsF0_TrailsARaw)
#F2
F2_TrailsARaw <- your_data_file[your_data_file[, "Timepoint"] == "F2",]
noNAsF2_TrailsARaw <- na.omit(F2_TrailsARaw$TrailsA.Raw)
mean(noNAsF2_TrailsARaw)
sd(noNAsF2_TrailsARaw)
#Post hoc tests
#non-sig. interaction - test by Group
aov(TrailsA.Raw ~ Group, data = your_data_file) %>% tukey_hsd()
#effect size for Groups
your_data_file%>%cohens_d(TrailsA.Raw~Group)

#plot TrailsA.Z (raincloud plot)
ggplot(your_data_file, aes(x = Group, y = TrailsA.Z, fill = Timepoint)) + 
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(aes(y = TrailsA.Z, color = Timepoint), position = position_jitter(width = .15), size = .5, alpha = 0.8) +
  geom_boxplot(width = 0.1, fill = "white", outlier.size = 1, aes(colour = Timepoint)) + 
  stat_summary(fun = mean, geom = "point", shape = 19, size = 2, aes(colour = Timepoint)) + 
  xlab("Group") + 
  ylab("Trail Making Test A Z-scores") +
  scale_x_discrete(labels = c("1" = "Control", "2" = "SCD", "3" = "aMCI", "4" = "mMCI", "5" = "AD")) + 
  theme_classic() +
  coord_flip()
#run mixed design, 2 x 5 ANOVA for TrailsA.Z
aov_TrailsAZ <- anova_test(data=your_data_file, dv=TrailsA.Z, wid=Individual_number, between=Group, within=Timepoint, effect.size = "pes")
get_anova_table(aov_TrailsAZ)
#check descriptive statistics per each group, per each timepoint
TrailsA.Z_descrip <- describeBy(your_data_file$TrailsA.Z, list(your_data_file$Group, your_data_file$Timepoint))
#find mean & SD from total sample in F0 and F2 timepoints:
F0_TrailsAZ <- your_data_file[your_data_file[, "Timepoint"] == "F0",]
noNAsF0_TrailsAZ <- na.omit(F0_TrailsAZ$TrailsA.Z)
mean(noNAsF0_TrailsAZ)
sd(noNAsF0_TrailsAZ)
#F2
F2_TrailsAZ <- your_data_file[your_data_file[, "Timepoint"] == "F2",]
noNAsF2_TrailsAZ <- na.omit(F2_TrailsAZ$TrailsA.Z)
mean(noNAsF2_TrailsAZ)
sd(noNAsF2_TrailsAZ)
#Post hoc tests
#non-sig. interaction - test by Group
aov(TrailsA.Z~ Group, data = your_data_file) %>% tukey_hsd()
#effect size for Groups
your_data_file%>%cohens_d(TrailsA.Z~Group)

#plot TrailsB.Raw (raincloud plot)
ggplot(your_data_file, aes(x = Group, y = TrailsB.Raw, fill = Timepoint)) + 
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(aes(y = TrailsB.Raw, color = Timepoint), position = position_jitter(width = .15), size = .5, alpha = 0.8) +
  geom_boxplot(width = 0.1, fill = "white", outlier.size = 1, aes(colour = Timepoint)) + 
  stat_summary(fun = mean, geom = "point", shape = 19, size = 2, aes(colour = Timepoint)) + 
  xlab("Group") + 
  ylab("Trail Making Test B Raw (seconds)") +
  scale_x_discrete(labels = c("1" = "Control", "2" = "SCD", "3" = "aMCI", "4" = "mMCI", "5" = "AD")) + 
  theme_classic() +
  coord_flip()
#run mixed design, 2 x 5 ANOVA for TrailsB.Raw
aov_TrailsBRaw <- anova_test(data=your_data_file, dv=TrailsB.Raw, wid=Individual_number, between=Group, within=Timepoint, effect.size = "pes")
get_anova_table(aov_TrailsBRaw)
#check descriptive statistics per each group, per each timepoint
TrailsB.Raw_descrip <- describeBy(your_data_file$TrailsB.Raw, list(your_data_file$Group, your_data_file$Timepoint))
#find mean & SD from total sample in F0 and F2 timepoints:
F0_TrailsBRaw <- your_data_file[your_data_file[, "Timepoint"] == "F0",]
noNAsF0_TrailsBRaw <- na.omit(F0_TrailsBRaw$TrailsB.Raw)
mean(noNAsF0_TrailsBRaw)
sd(noNAsF0_TrailsBRaw)
#F2
F2_TrailsBRaw <- your_data_file[your_data_file[, "Timepoint"] == "F2",]
noNAsF2_TrailsBRaw <- na.omit(F2_TrailsBRaw$TrailsB.Raw)
mean(noNAsF2_TrailsBRaw)
sd(noNAsF2_TrailsBRaw)
#Post hoc tests
#Test by Group
aov(TrailsB.Raw ~ Group, data = your_data_file) %>% tukey_hsd()
#effect size for Groups
your_data_file%>%cohens_d(TrailsB.Raw~Group)
#Test by Time Point
aov(TrailsB.Raw ~ Timepoint, data = your_data_file) %>% tukey_hsd()
#effect size for Time Point
your_data_file%>%cohens_d(TrailsB.Raw~Timepoint, paired=TRUE)
#Sig. interaction
#Simple main effect w/ interaction - for Group
posthoc_ME_Group_TrailsB <- your_data_file %>%
  group_by(Timepoint) %>%
  anova_test(dv=TrailsB.Raw,wid=Individual_number,between=Group) %>%
  adjust_pvalue(method="fdr")
posthoc_ME_Group_TrailsB ##within each timepoint (F0 & F2), there are Group differences##
#Run Pairwise comparison between groups levels if simple main effects (above) is sig.
posthoc_pairwise_Group_TrailsB <- your_data_file %>%
  group_by(Timepoint) %>%
  tukey_hsd(TrailsB.Raw ~ Group)
posthoc_pairwise_Group_TrailsB ##examines all Group contrasts within each timepoint (F0 & F2) ##
#Simple main effect w/ ineraction - for Timepoint
posthoc_ME_Timepoint_TrailsB <- your_data_file %>%
  group_by(Group) %>%
  anova_test(dv=TrailsB.Raw,wid=Individual_number,within=Timepoint,effect.size = "pes") %>%
  get_anova_table() %>%
  adjust_pvalue(method="fdr")
posthoc_ME_Timepoint_TrailsB ## Across each Group, where were there sig. differences between timepoints (F0 vs. F2)? ##
#Pairwise comparison between groups levels if simple main effects (above) is sig.
posthoc_pairwise_Timepoint_TrailsB <- your_data_file %>%
  group_by(Group) %>%
  tukey_hsd(TrailsB.Raw ~ Timepoint, paired = TRUE)
posthoc_pairwise_Timepoint_TrailsB ##examines all Timepoint contrasts between each Group (1,2,3,4,5) ##
#All pairwise comparisons with Tukey test
summary(lm(TrailsB.Raw~Group+Timepoint+Group:Timepoint,data=your_data_file))
TukeyHSD(aov(TrailsB.Raw~Group+Timepoint+Group:Timepoint,data=your_data_file))
#effect size for interaction (mMCI Group and Time Point)
your_data_file_mMCI_TrailsB_long <- subset(your_data_file, your_data_file$Group == 4)
your_data_file_mMCI_TrailsB_long$Group <- droplevels(your_data_file_mMCI_TrailsB_long$Group)
your_data_file_mMCI_TrailsB_long%>%cohens_d(TrailsB.Raw~Timepoint, paired=TRUE)
#plot interaction 
#remove NAs for Trails B variable
noNAs_TrailsBRaw <- your_data_file[,c("ParticipantID","Individual_number","Group","Timepoint","Age","Sex","TrailsB.Raw")]
noNAs_TrailsBRaw<- noNAs_TrailsBRaw[complete.cases(noNAs_TrailsBRaw), ]
#view interaction plot - by Group
noNAs_TrailsBRaw%>%
  group_by(Group,Timepoint) %>%
  summarise(s_mean=mean(TrailsB.Raw)) %>%
  ggplot(aes(y=s_mean,x=Group,colour=Timepoint,group=Timepoint))+
  geom_point()+geom_line()+
  scale_x_discrete(labels = c("1" = "C", "2" = "SCD", "3" = "aMCI", "4" = "mMCI", "5" = "AD")) + 
  ylab("Trail Making Test B Raw (seconds)") +
  theme_classic()
#view interaction plot - by Timepoint
noNAs_TrailsBRaw %>%
  group_by(Group,Timepoint) %>%
  summarise(s_mean=mean(TrailsB.Raw)) %>%
  ggplot(aes(y=s_mean,x=Timepoint,colour=Group,group=Group))+
  geom_point()+geom_line()+
  scale_color_discrete(labels = c("1" = "C", "2" = "SCD", "3" = "aMCI", "4" = "mMCI", "5" = "AD")) + 
  ylab("Trail Making Test B Raw (seconds)") +
  theme_classic()
#interaction f/u tests -- Trails B
#subtract differences in F2 - F0 for Trails B
TrailsB.Raw_diff_data<-your_data_file[,c("ParticipantID","Individual_number","Group","Timepoint","Age","Sex","TrailsB.Raw")]
F0_TrailsB.Raw_data<-TrailsB.Raw_diff_data[TrailsB.Raw_diff_data$Timepoint=='F0', ]
F2_TrailsB.Raw_data<-TrailsB.Raw_diff_data[TrailsB.Raw_diff_data$Timepoint=='F2', ]
TrailsB.RawDiff<-F2_TrailsB.Raw_data$TrailsB.Raw - F0_TrailsB.Raw_data$TrailsB.Raw
F0_TrailsB.Raw_data$TrailsB.RawDiff<-TrailsB.RawDiff
#run one-way ANOVA on the F2-F0 differences in TrailsB.Raw between groups
TrailsB.RawDiff_mod <- lm(TrailsB.RawDiff ~ Group, data = F0_TrailsB.Raw_data)
anova(TrailsB.RawDiff_mod) #sig. difference
#effect size omnibus ANOVA
etaSquared(TrailsB.RawDiff_mod)
#post hoc f/u test
#run pairwise comparisons (post-hoc Tukey), given that the F-test was significant. 
post_hoc_TrailsB.RawDiff_mod <- glht(TrailsB.RawDiff_mod, linfct = mcp(Group = "Tukey"))
summary(post_hoc_TrailsB.RawDiff_mod)
confint(post_hoc_TrailsB.RawDiff_mod)
#effect size for Interaction
#create dataframe
#TrailsB.Raw_diff_data_124 <- your_data_file[1:124,c("ParticipantID","Individual_number","Group","Timepoint","Age","Sex","TrailsB.Raw")]
TrailsB.Raw_diff_data_124 <- your_data_file[1:119,c("ParticipantID","Individual_number","Group","Timepoint","Age","Sex","TrailsB.Raw")]
TrailsB.Raw_diff_data_124$TrailsB.RawDiff <- TrailsB.RawDiff 
#calculate effect size
TrailsB.Raw_diff_data_124%>%cohens_d(TrailsB.RawDiff~Group)
#test if there is a difference among differences (e.g., is the aMCI group difference for TMT-B between the time points 
#(F0 - F2) sig. greater compared to the other groups?)
#subtract differences in F2 - F0 for TMT-B
TrailsB_diff_data<-your_data_file[,c("ParticipantID","Individual_number","Group","Timepoint","Age","Sex","TrailsB.Raw")]
F0_TrailsB_data<-TrailsB_diff_data[TrailsB_diff_data$Timepoint=='F0', ]
F2_TrailsB_data<-TrailsB_diff_data[TrailsB_diff_data$Timepoint=='F2', ]
TrailsBDiff<-F2_TrailsB_data$TrailsB.Raw - F0_TrailsB_data$TrailsB.Raw
F0_TrailsB_data$TrailsBDiff<-TrailsBDiff
#run one-way ANOVA on the F2-F0 differences in color naming between groups
TrailsBDiff_mod <- lm(TrailsBDiff ~ Group, data = F0_TrailsB_data)
anova(TrailsBDiff_mod) #sig. difference
#effect size omnibus ANOVA
etaSquared(TrailsBDiff_mod)
#post hoc f/u test
#run pairwise comparisons (post-hoc Tukey), given that the F-test was significant. 
post_hoc_TrailsBDiff_mod <- glht(TrailsBDiff_mod, linfct = mcp(Group = "Tukey"))
summary(post_hoc_TrailsBDiff_mod)
confint(post_hoc_TrailsBDiff_mod)
#effect size for sig. post hoc tests
#for SCD vs. aMCI 
SCDvaMCI_TrailsBDiff <- subset(F0_TrailsB_data, F0_TrailsB_data$Group == 2 | F0_TrailsB_data$Group == 3)
SCDvaMCI_TrailsBDiff$Group <- droplevels(SCDvaMCI_TrailsBDiff$Group)
cohensD(TrailsBDiff ~ Group, data = SCDvaMCI_TrailsBDiff) #this looks like Hedges' g? 
#plot Trails B differences (raincloud plot)
ggplot(F0_TrailsB_data, aes(x = Group, y = TrailsBDiff, fill = Group)) + 
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(aes(y = TrailsBDiff, color = Group), position = position_jitter(width = .15), size = .5, alpha = 0.8) +
  geom_boxplot(width = 0.1, fill = "white", outlier.size = 1, aes(colour = Group)) + 
  stat_summary(fun = mean, geom = "point", shape = 19, size = 2, aes(colour = Group)) + 
  xlab("Group") + 
  ylab("Trails B Difference (F2 - F0)") +
  scale_x_discrete(labels = c("1" = "Control", "2" = "SCD", "3" = "aMCI", "4" = "mMCI", "5" = "AD")) + 
  theme_classic() +
  theme(legend.position = "none")+
  coord_flip()

#plot TrailsB.Z (raincloud plot)
ggplot(your_data_file, aes(x = Group, y = TrailsB.Z, fill = Timepoint)) + 
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(aes(y = TrailsB.Z, color = Timepoint), position = position_jitter(width = .15), size = .5, alpha = 0.8) +
  geom_boxplot(width = 0.1, fill = "white", outlier.size = 1, aes(colour = Timepoint)) + 
  stat_summary(fun = mean, geom = "point", shape = 19, size = 2, aes(colour = Timepoint)) + 
  xlab("Group") + 
  ylab("Trail Making Test B Z-scores") +
  scale_x_discrete(labels = c("1" = "Control", "2" = "SCD", "3" = "aMCI", "4" = "mMCI", "5" = "AD")) + 
  theme_classic() +
  coord_flip()
#run mixed design, 2 x 5 ANOVA for TrailsB.Z
aov_TrailsBZ <- anova_test(data=your_data_file, dv=TrailsB.Z, wid=Individual_number, between=Group, within=Timepoint, effect.size = "pes")
get_anova_table(aov_TrailsBZ)
#check descriptive statistics per each group, per each timepoint
TrailsB.Z_descrip <- describeBy(your_data_file$TrailsB.Z, list(your_data_file$Group, your_data_file$Timepoint))
#find mean & SD from total sample in F0 and F2 timepoints:
F0_TrailsBZ <- your_data_file[your_data_file[, "Timepoint"] == "F0",]
noNAsF0_TrailsBZ <- na.omit(F0_TrailsBZ$TrailsB.Z)
mean(noNAsF0_TrailsBZ)
sd(noNAsF0_TrailsBZ)
#F2
F2_TrailsBZ <- your_data_file[your_data_file[, "Timepoint"] == "F2",]
noNAsF2_TrailsBZ <- na.omit(F2_TrailsBZ$TrailsB.Z)
mean(noNAsF2_TrailsBZ)
sd(noNAsF2_TrailsBZ)
#Post hoc tests
#Test by Group
aov(TrailsB.Z ~ Group, data = your_data_file) %>% tukey_hsd()
#effect size for Groups
your_data_file%>%cohens_d(TrailsB.Z~Group)
#interaction f/u tests -- Trails B (z-scores)
#subtract differences in F2 - F0 for Trails B (z-scores)
TrailsB.Z_diff_data<-your_data_file[,c("ParticipantID","Individual_number","Group","Timepoint","Age","Sex","TrailsB.Z")]
F0_TrailsB.Z_data<-TrailsB.Z_diff_data[TrailsB.Z_diff_data$Timepoint=='F0', ]
F2_TrailsB.Z_data<-TrailsB.Z_diff_data[TrailsB.Z_diff_data$Timepoint=='F2', ]
TrailsB.ZDiff<-F2_TrailsB.Z_data$TrailsB.Z - F0_TrailsB.Z_data$TrailsB.Z
F0_TrailsB.Z_data$TrailsB.ZDiff<-TrailsB.ZDiff
#run one-way ANOVA on the F2-F0 differences in TrailsB.Z between groups
TrailsB.ZDiff_mod <- lm(TrailsB.ZDiff ~ Group, data = F0_TrailsB.Z_data)
anova(TrailsB.ZDiff_mod) #sig. difference
#effect size omnibus ANOVA
etaSquared(TrailsB.ZDiff_mod)
#post hoc f/u test
#run pairwise comparisons (post-hoc Tukey), given that the F-test was significant. 
post_hoc_TrailsB.ZDiff_mod <- glht(TrailsB.ZDiff_mod, linfct = mcp(Group = "Tukey"))
summary(post_hoc_TrailsB.ZDiff_mod)
confint(post_hoc_TrailsB.ZDiff_mod)
#effect size for Interaction
  #create dataframe
  TrailsB.Z_diff_data_124 <- your_data_file[1:124,c("ParticipantID","Individual_number","Group","Timepoint","Age","Sex","TrailsB.Z")]
  TrailsB.Z_diff_data_124$TrailsB.ZDiff <- TrailsB.ZDiff 
  #calculate effect size
  TrailsB.Z_diff_data_124%>%cohens_d(TrailsB.ZDiff~Group)

#plot TMT.B.TMT.A (TMT-B / TMT-A) (raincloud plot)
ggplot(your_data_file, aes(x = Group, y = TMT.B.TMT.A, fill = Timepoint)) + 
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(aes(y = TMT.B.TMT.A, color = Timepoint), position = position_jitter(width = .15), size = .5, alpha = 0.8) +
  geom_boxplot(width = 0.1, fill = "white", outlier.size = 1, aes(colour = Timepoint)) + 
  stat_summary(fun = mean, geom = "point", shape = 19, size = 2, aes(colour = Timepoint)) + 
  xlab("Group") + 
  ylab("TMT-B / TMT-A") +
  scale_x_discrete(labels = c("1" = "Control", "2" = "SCD", "3" = "aMCI", "4" = "mMCI", "5" = "AD")) + 
  theme_classic() +
  coord_flip()
#run mixed design, 2 x 5 ANOVA for TMT-B / TMT-A
aov_TMTBTMTA <- anova_test(data=your_data_file, dv=TMT.B.TMT.A, wid=Individual_number, between=Group, within=Timepoint, effect.size = "pes")
get_anova_table(aov_TMTBTMTA)
#check descriptive statistics per each group, per each timepoint
TMT.B.TMT.A_descrip <- describeBy(your_data_file$TMT.B.TMT.A, list(your_data_file$Group, your_data_file$Timepoint))
#find mean & SD from total sample in F0 and F2 timepoints:
F0_TMTBTMTA <- your_data_file[your_data_file[, "Timepoint"] == "F0",]
noNAsF0_TMTBTMTA <- na.omit(F0_TMTBTMTA$TMT.B.TMT.A)
mean(noNAsF0_TMTBTMTA)
sd(noNAsF0_TMTBTMTA)
#F2
F2_TMTBTMTA <- your_data_file[your_data_file[, "Timepoint"] == "F2",]
noNAsF2_TMTBTMTA <- na.omit(F2_TMTBTMTA$TMT.B.TMT.A)
mean(noNAsF2_TMTBTMTA)
sd(noNAsF2_TMTBTMTA)
#Post hoc
#non-sig. interaction - test by Time Point
aov(TMT.B.TMT.A ~ Timepoint, data = your_data_file) %>% tukey_hsd()
#effect size for Time Point
your_data_file%>%cohens_d(TMT.B.TMT.A~Timepoint, paired=TRUE)

#plot TMT.B.TMT.A Z-scores (TMT-B / TMT-A.zscores.calc.) (raincloud plot)
ggplot(your_data_file, aes(x = Group, y = TMT.B.TMT.A.zscores.calc., fill = Timepoint)) + 
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(aes(y = TMT.B.TMT.A.zscores.calc., color = Timepoint), position = position_jitter(width = .15), size = .5, alpha = 0.8) +
  geom_boxplot(width = 0.1, fill = "white", outlier.size = 1, aes(colour = Timepoint)) + 
  stat_summary(fun = mean, geom = "point", shape = 19, size = 2, aes(colour = Timepoint)) + 
  xlab("Group") + 
  ylab("TMT-B / TMT-A Z-scores") +
  scale_x_discrete(labels = c("1" = "Control", "2" = "SCD", "3" = "aMCI", "4" = "mMCI", "5" = "AD")) + 
  theme_classic() +
  coord_flip()
#run mixed design, 2 x 5 ANOVA for TMT-B / TMT-A Z-scores
aov_TMTBTMTAZ <- anova_test(data=your_data_file, dv=TMT.B.TMT.A.zscores.calc., wid=Individual_number, between=Group, within=Timepoint, effect.size = "pes")
get_anova_table(aov_TMTBTMTAZ)
#check descriptive statistics per each group, per each timepoint
TMT.B.TMT.A_descripZ <- describeBy(your_data_file$TMT.B.TMT.A.zscores.calc., list(your_data_file$Group, your_data_file$Timepoint))
#find mean & SD from total sample in F0 and F2 timepoints:
F0_TMTBTMTAZ <- your_data_file[your_data_file[, "Timepoint"] == "F0",]
noNAsF0_TMTBTMTAZ <- na.omit(F0_TMTBTMTAZ$TMT.B.TMT.A.zscores.calc.)
mean(noNAsF0_TMTBTMTAZ)
sd(noNAsF0_TMTBTMTAZ)
#F2
F2_TMTBTMTAZ <- your_data_file[your_data_file[, "Timepoint"] == "F2",]
noNAsF2_TMTBTMTAZ <- na.omit(F2_TMTBTMTAZ$TMT.B.TMT.A.zscores.calc.)
mean(noNAsF2_TMTBTMTAZ)
sd(noNAsF2_TMTBTMTAZ)


####-----------ANCOVA Testing (w/ age) -------------------------------------#### 
#run mixed design, 2 x 5 ANCOVA for HayBTime1.Raw
aov_HayBTime1Raw <- aov(HayBTime1.Raw ~ Group*Timepoint + Error(Individual_number/Timepoint) + Age,  data = your_data_file)
summary(aov_HayBTime1Raw)
# aov_HayBTime1Raw <- anova_test(data=your_data_file, dv=HayBTime1.Raw, wid=Individual_number, between=Group, within=Timepoint, covariate=Age, effect.size = "pes") #will not accept 'Age' as a covariate
# get_anova_table(aov_HayBTime1Raw)
#aov_HayBTime1Raw <- Anova(lm(HayBTime1.Raw ~ Group*Timepoint + Age  data=your_data_file), type = "III") #use this anova test to account for unbalanced designs/sample sizes
#aov_HayBTime1Raw
#ancova_model<-lm(HayBTime1.Raw ~ Group*Timepoint+Age,data = your_data_file) #works, but this doesn't specifically tie the participant between each time point
#anova(ancova_model)
#effect size w/ covariates
#eta_squared(aov_HayBTime1Raw) #doesn't work - doesn't like the class of the model
sjstats::eta_sq(aov_HayBTime1Raw)
#run mixed design, 2 x 5 ANCOVA for HayBTime2.Raw
aov_HayBTime2Raw <- aov(HayBTime2.Raw ~ Group*Timepoint + Error(Individual_number/Timepoint) + Age,  data = your_data_file)
summary(aov_HayBTime2Raw)
sjstats::eta_sq(aov_HayBTime2Raw)
#run mixed design, 2 x 5 ANCOVA for HayTime2.HayTime1_minus
aov_HayTime2.HayTime1_minus <- aov(HayTime2.HayTime1_minus ~ Group*Timepoint + Error(Individual_number/Timepoint) + Age,  data = your_data_file)
summary(aov_HayTime2.HayTime1_minus)
sjstats::eta_sq(aov_HayTime2.HayTime1_minus)
#run mixed design, 2 x 5 ANCOVA for HayBCatA.Raw
aov_HayBCatARaw <- aov(HayBCatA.Raw ~ Group*Timepoint + Error(Individual_number/Timepoint) + Age,  data = your_data_file)
summary(aov_HayBCatARaw)
sjstats::eta_sq(aov_HayBCatARaw)
#run mixed design, 2 x 5 ANCOVA for HayBCatB.Raw
aov_HayBCatBRaw <- aov(HayBCatB.Raw ~ Group*Timepoint + Error(Individual_number/Timepoint) + Age,  data = your_data_file)
summary(aov_HayBCatBRaw)
sjstats::eta_sq(aov_HayBCatBRaw)
#run mixed design, 2 x 5 ANCOVA for HayCatTotalError.Raw
aov_HayCatTotalErrorRaw <- aov(HayCatTotalError.Raw ~ Group*Timepoint + Error(Individual_number/Timepoint) + Age,  data = your_data_file)
summary(aov_HayCatTotalErrorRaw)
sjstats::eta_sq(aov_HayCatTotalErrorRaw)
#run mixed design, 2 x 5 ANCOVA for ColorNaming.Raw
aov_ColorNamingRaw <- aov(ColorNaming.Raw ~ Group*Timepoint + Error(Individual_number/Timepoint) + Age,  data = your_data_file)
summary(aov_ColorNamingRaw)
sjstats::eta_sq(aov_ColorNamingRaw)
#run mixed design, 2 x 5 ANCOVA for WordReading.Raw
aov_WordReadingRaw <- aov(WordReading.Raw ~ Group*Timepoint + Error(Individual_number/Timepoint) + Age,  data = your_data_file)
summary(aov_WordReadingRaw)
sjstats::eta_sq(aov_WordReadingRaw)
#run mixed design, 2 x 5 ANCOVA for Inhibition.Raw
aov_InhibitionRaw <- aov(Inhibition.Raw ~ Group*Timepoint + Error(Individual_number/Timepoint) + Age,  data = your_data_file)
summary(aov_InhibitionRaw)
sjstats::eta_sq(aov_InhibitionRaw)
#run mixed design, 2 x 5 ANCOVA for Inhibition.Colour.Naming
aov_InhibitionColourNaming <- aov(Inhibition.Colour.Naming~ Group*Timepoint + Error(Individual_number/Timepoint) + Age,  data = your_data_file)
summary(aov_InhibitionColourNaming) #not sig.
#run mixed design, 2 x 5 ANCOVA for Inhibition.Word.Reading
aov_InhibitionWordReading <- aov(Inhibition.Word.Reading~ Group*Timepoint + Error(Individual_number/Timepoint) + Age,  data = your_data_file)
summary(aov_InhibitionWordReading)
sjstats::eta_sq(aov_InhibitionWordReading)
# #run mixed design, 2 x 5 ANCOVA for Golden_Stroop_interference_score
# aov_Golden_Stroop_interference_score <- aov(Golden_Stroop_interference_score ~ Group*Timepoint + Error(Individual_number/Timepoint) + Age,  data = your_data_file)
# summary(aov_Golden_Stroop_interference_score)
# sjstats::eta_sq(aov_Golden_Stroop_interference_score)
#run mixed design, 2 x 5 ANCOVA for Interference_score_Caffarra_2002
aov_Interference_score_Caffarra_2002 <- aov(Interference_score_Caffarra_2002 ~ Group*Timepoint + Error(Individual_number/Timepoint) + Age,  data = your_data_file)
summary(aov_Interference_score_Caffarra_2002)
sjstats::eta_sq(aov_Interference_score_Caffarra_2002)
#run mixed design, 2 x 5 ANCOVA for TrailsA.Raw
aov_TrailsARaw <- aov(TrailsA.Raw ~ Group*Timepoint + Error(Individual_number/Timepoint) + Age,  data = your_data_file)
summary(aov_TrailsARaw)
sjstats::eta_sq(aov_TrailsARaw)
#run mixed design, 2 x 5 ANCOVA for TrailsB.Raw
aov_TrailsBRaw <- aov(TrailsB.Raw ~ Group*Timepoint + Error(Individual_number/Timepoint) + Age,  data = your_data_file)
summary(aov_TrailsBRaw)
sjstats::eta_sq(aov_TrailsBRaw)
#run mixed design, 2 x 5 ANCOVA for TMT.B.TMT.A
aov_TMTBTMTA <- aov(TMT.B.TMT.A ~ Group*Timepoint + Error(Individual_number/Timepoint) + Age,  data = your_data_file)
summary(aov_TMTBTMTA)
sjstats::eta_sq(aov_TMTBTMTA)
#run mixed design, 2 x 5 ANCOVA for LetFluency.Raw
aov_LetFluencyRaw <- aov(LetFluency.Raw ~ Group*Timepoint + Error(Individual_number/Timepoint) + Age,  data = your_data_file)
summary(aov_LetFluencyRaw)
sjstats::eta_sq(aov_LetFluencyRaw)
#run mixed design, 2 x 5 ANCOVA for CatFluency.Raw
aov_CatFluencyRaw <- aov(CatFluency.Raw ~ Group*Timepoint + Error(Individual_number/Timepoint) + Age,  data = your_data_file)
summary(aov_CatFluencyRaw)
sjstats::eta_sq(aov_CatFluencyRaw)
#run mixed design, 2 x 5 ANCOVA for Switching.Raw
aov_SwitchingRaw <- aov(Switching.Raw ~ Group*Timepoint + Error(Individual_number/Timepoint) + Age,  data = your_data_file)
summary(aov_SwitchingRaw)
sjstats::eta_sq(aov_SwitchingRaw)

#f/u post hoc tests
#HayBTime1
#remove NAs from dataset for given variable
noNAs_HayBTime1Raw <- your_data_file[,c("ParticipantID","Individual_number","Group","Timepoint","Age","Sex","HayBTime1.Raw")]
noNAs_HayBTime1Raw <- noNAs_HayBTime1Raw[complete.cases(noNAs_HayBTime1Raw), ]
post_hoc_aov_HayBTime1_covar_mod <- lme(HayBTime1.Raw ~ Group*Timepoint + Age,  random = ~1 | Individual_number/Timepoint, data=noNAs_HayBTime1Raw)
summary(glht(post_hoc_aov_HayBTime1_covar_mod, linfct=mcp(Group="Tukey"))) #no sig.

#HayBTime2
#remove NAs from dataset for given variable
noNAs_HayBTime2Raw <- your_data_file[,c("ParticipantID","Individual_number","Group","Timepoint","Age","Sex","HayBTime2.Raw")]
noNAs_HayBTime2Raw <- noNAs_HayBTime2Raw[complete.cases(noNAs_HayBTime2Raw), ]
post_hoc_aov_HayBTime2_covar_mod <- lme(HayBTime2.Raw ~ Group*Timepoint + Age,  random = ~1 | Individual_number/Timepoint, data=noNAs_HayBTime2Raw)
summary(glht(post_hoc_aov_HayBTime2_covar_mod, linfct=mcp(Group="Tukey"))) #no sig.
#interaction - HayBTime2
#Tukey test
TukeyHSD(aov(HayBTime2.Raw~Group*Timepoint+as.factor(Age), data = your_data_file)) 
#subtract differences in F2 - F0 for color naming with covariate age
HayBTime2.Raw_diff_data<-your_data_file[,c("ParticipantID","Individual_number","Group","Timepoint","Age","Sex","HayBTime2.Raw")]
F0_HayBTime2.Raw_data<-HayBTime2.Raw_diff_data[HayBTime2.Raw_diff_data$Timepoint=='F0', ]
F2_HayBTime2.Raw_data<-HayBTime2.Raw_diff_data[HayBTime2.Raw_diff_data$Timepoint=='F2', ]
HayBTime2.RawDiff<-F2_HayBTime2.Raw_data$HayBTime2.Raw - F0_HayBTime2.Raw_data$HayBTime2.Raw
F0_HayBTime2.Raw_data$HayBTime2.RawDiff<-HayBTime2.RawDiff
#run one-way ANOVA on the F2-F0 differences in HayTime2.HayTime1_minus between groups
HayBTime2.RawDiff_covar_mod <- lm(HayBTime2.RawDiff ~ Group+Age, data = F0_HayBTime2.Raw_data)
anova(HayBTime2.RawDiff_covar_mod) #no sig. difference
#effect size omnibus ANOVA
etaSquared(HayBTime2.RawDiff_covar_mod)
#post hoc f/u test
#run pairwise comparisons (post-hoc Tukey), given that the F-test was significant. 
post_hoc_HayBTime2.RawDiff_covar_mod <- glht(HayBTime2.RawDiff_covar_mod, linfct = mcp(Group = "Tukey"))
summary(post_hoc_HayBTime2.RawDiff_covar_mod)
confint(post_hoc_HayBTime2.RawDiff_covar_mod)
t_value_effect_size <- summary(glht(HayBTime2.RawDiff_covar_mod, linfct = mcp(Group = "Tukey")))
#effect size for sig. post hoc tests with covariate
  #for C vs. AD
  your_data_file_CvAD_HayBTime2.RawDiff <- subset(your_data_file, your_data_file$Group == 1 | your_data_file$Group == 5)
  your_data_file_CvAD_HayBTime2.RawDiff$Group <- droplevels(your_data_file_CvAD_HayBTime2.RawDiff$Group)
  group_number <-dplyr::count(your_data_file_CvAD_HayBTime2.RawDiff, Group) #count number of participants per group
  r_value <- cor.test(your_data_file_CvAD_HayBTime2.RawDiff$HayBTime2.Raw,your_data_file_CvAD_HayBTime2.RawDiff$Age) #find correlation value (r) between dependent variable
  a.tes(t=t_value_effect_size$test$tstat['5 - 1'],n.1=group_number['1','n'],n.2=group_number['2','n'],R=r_value$estimate,q=1) #calculate Cohen's D with the covariate of age
  #for SCD vs. AD
  your_data_file_SCDvAD_HayBTime2.RawDiff <- subset(your_data_file, your_data_file$Group == 2 | your_data_file$Group == 5)
  your_data_file_SCDvAD_HayBTime2.RawDiff$Group <- droplevels(your_data_file_SCDvAD_HayBTime2.RawDiff$Group)
  group_number <-dplyr::count(your_data_file_SCDvAD_HayBTime2.RawDiff, Group) #count number of participants per group
  r_value <- cor.test(your_data_file_SCDvAD_HayBTime2.RawDiff$HayBTime2.Raw,your_data_file_SCDvAD_HayBTime2.RawDiff$Age) #find correlation value (r) between dependent variable
  a.tes(t=t_value_effect_size$test$tstat['5 - 2'],n.1=group_number['1','n'],n.2=group_number['2','n'],R=r_value$estimate,q=1) #calculate Cohen's D with the covariate of age

#HayTime2.HayTime1_minus
#remove NAs from dataset for given variable
noNAs_HayTime2.HayTime1_minus <- your_data_file[,c("ParticipantID","Individual_number","Group","Timepoint","Age","Sex","HayTime2.HayTime1_minus")]
noNAs_HayTime2.HayTime1_minus <- noNAs_HayTime2.HayTime1_minus[complete.cases(noNAs_HayTime2.HayTime1_minus), ]
post_hoc_aov_HayTime2.HayTime1_minus_covar_mod <- lme(HayTime2.HayTime1_minus~ Group*Timepoint + Age,  random = ~1 | Individual_number/Timepoint, data=noNAs_HayTime2.HayTime1_minus)
summary(glht(post_hoc_aov_HayTime2.HayTime1_minus_covar_mod, linfct=mcp(Group="Tukey"))) #no sig.
#interaction - HayTime2.HayTime1_minus
#Tukey test
TukeyHSD(aov(HayTime2.HayTime1_minus~Group*Timepoint+as.factor(Age), data = your_data_file)) 
#subtract differences in F2 - F0 for color naming with covariate age
HayTime2.HayTime1_minus_diff_data<-your_data_file[,c("ParticipantID","Individual_number","Group","Timepoint","Age","Sex","HayTime2.HayTime1_minus")]
F0_HayTime2.HayTime1_minus_data<-HayTime2.HayTime1_minus_diff_data[HayTime2.HayTime1_minus_diff_data$Timepoint=='F0', ]
F2_HayTime2.HayTime1_minus_data<-HayTime2.HayTime1_minus_diff_data[HayTime2.HayTime1_minus_diff_data$Timepoint=='F2', ]
HayTime2.HayTime1_minusDiff<-F2_HayTime2.HayTime1_minus_data$HayTime2.HayTime1_minus - F0_HayTime2.HayTime1_minus_data$HayTime2.HayTime1_minus
F0_HayTime2.HayTime1_minus_data$HayTime2.HayTime1_minusDiff<-HayTime2.HayTime1_minusDiff
#run one-way ANOVA on the F2-F0 differences in HayTime2.HayTime1_minus between groups
HayTime2.HayTime1_minusDiff_covar_mod <- lm(HayTime2.HayTime1_minusDiff ~ Group+Age, data = F0_HayTime2.HayTime1_minus_data)
anova(HayTime2.HayTime1_minusDiff_covar_mod) #sig. difference
#effect size omnibus ANOVA
etaSquared(HayTime2.HayTime1_minusDiff_covar_mod)
#post hoc f/u test
#run pairwise comparisons (post-hoc Tukey), given that the F-test was significant. 
post_hoc_HayTime2.HayTime1_minusDiff_covar_mod <- glht(HayTime2.HayTime1_minusDiff_covar_mod, linfct = mcp(Group = "Tukey"))
summary(post_hoc_HayTime2.HayTime1_minusDiff_covar_mod)
confint(post_hoc_HayTime2.HayTime1_minusDiff_covar_mod)
t_value_effect_size <- summary(glht(HayTime2.HayTime1_minusDiff_covar_mod, linfct = mcp(Group = "Tukey")))
#effect size for sig. post hoc tests with covariate
  #for C vs. AD
  your_data_file_CvAD_HayTime2.HayTime1_minusDiff <- subset(your_data_file, your_data_file$Group == 1 | your_data_file$Group == 5)
  your_data_file_CvAD_HayTime2.HayTime1_minusDiff$Group <- droplevels(your_data_file_CvAD_HayTime2.HayTime1_minusDiff$Group)
  group_number <-dplyr::count(your_data_file_CvAD_HayTime2.HayTime1_minusDiff, Group) #count number of participants per group
  r_value <- cor.test(your_data_file_CvAD_HayTime2.HayTime1_minusDiff$HayTime2.HayTime1_minus,your_data_file_CvAD_HayTime2.HayTime1_minusDiff$Age) #find correlation value (r) between dependent variable
  a.tes(t=t_value_effect_size$test$tstat['5 - 1'],n.1=group_number['1','n'],n.2=group_number['2','n'],R=r_value$estimate,q=1) #calculate Cohen's D with the covariate of age
  #for SCD vs. AD
  your_data_file_SCDvAD_HayTime2.HayTime1_minusDiff <- subset(your_data_file, your_data_file$Group == 2 | your_data_file$Group == 5)
  your_data_file_SCDvAD_HayTime2.HayTime1_minusDiff$Group <- droplevels(your_data_file_SCDvAD_HayTime2.HayTime1_minusDiff$Group)
  group_number <-dplyr::count(your_data_file_SCDvAD_HayTime2.HayTime1_minusDiff, Group) #count number of participants per group
  r_value <- cor.test(your_data_file_SCDvAD_HayTime2.HayTime1_minusDiff$HayTime2.HayTime1_minus,your_data_file_SCDvAD_HayTime2.HayTime1_minusDiff$Age) #find correlation value (r) between dependent variable
  a.tes(t=t_value_effect_size$test$tstat['5 - 2'],n.1=group_number['1','n'],n.2=group_number['2','n'],R=r_value$estimate,q=1) #calculate Cohen's D with the covariate of age
#HayBCatA
#remove NAs from dataset for given variable
noNAs_HayBCatARaw <- your_data_file[,c("ParticipantID","Individual_number","Group","Timepoint","Age","Sex","HayBCatA.Raw")]
noNAs_HayBCatARaw <- noNAs_HayBCatARaw[complete.cases(noNAs_HayBCatARaw), ]
post_hoc_aov_HayBCatA_covar_mod <- lme(HayBCatA.Raw ~ Group*Timepoint + Age,  random = ~1 | Individual_number/Timepoint, data=noNAs_HayBCatARaw)
t_value_effect_size <- summary(glht(post_hoc_aov_HayBCatA_covar_mod, linfct=mcp(Group="Tukey")))
summary(glht(post_hoc_aov_HayBCatA_covar_mod, linfct=mcp(Group="Tukey")))
#calculate effect sizes (Cohen's D) with covariate  
  #for C vs. AD
  your_data_file_CvAD_HayBCatA.Raw <- subset(your_data_file, your_data_file$Group == 1 | your_data_file$Group == 5)
  your_data_file_CvAD_HayBCatA.Raw$Group <- droplevels(your_data_file_CvAD_HayBCatA.Raw$Group)
  group_number <-dplyr::count(your_data_file_CvAD_HayBCatA.Raw, Group) #count number of participants per group
  r_value <- cor.test(your_data_file_CvAD_HayBCatA.Raw$HayBCatA.Raw,your_data_file_CvAD_HayBCatA.Raw$Age) #find correlation value (r) between dependent variable
  a.tes(t=t_value_effect_size$test$tstat['5 - 1'],n.1=group_number['1','n'],n.2=group_number['2','n'],R=r_value$estimate,q=1) #calculate Cohen's D with the covariate of age
  #for SCD vs. AD
  your_data_file_SCDvAD_HayBCatA.Raw <- subset(your_data_file, your_data_file$Group == 2 | your_data_file$Group == 5)
  your_data_file_SCDvAD_HayBCatA.Raw$Group <- droplevels(your_data_file_SCDvAD_HayBCatA.Raw$Group)
  group_number <-dplyr::count(your_data_file_SCDvAD_HayBCatA.Raw, Group) #count number of participants per group
  r_value <- cor.test(your_data_file_SCDvAD_HayBCatA.Raw$HayBCatA.Raw,your_data_file_SCDvAD_HayBCatA.Raw$Age) #find correlation value (r) between dependent variable
  a.tes(t=t_value_effect_size$test$tstat['5 - 2'],n.1=group_number['1','n'],n.2=group_number['2','n'],R=r_value$estimate,q=1) #calculate Cohen's D with the covariate of age
  #for aMCI vs. AD
  your_data_file_aMCIvAD_HayBCatA.Raw <- subset(your_data_file, your_data_file$Group == 3 | your_data_file$Group == 5)
  your_data_file_aMCIvAD_HayBCatA.Raw$Group <- droplevels(your_data_file_aMCIvAD_HayBCatA.Raw$Group)
  group_number <-dplyr::count(your_data_file_aMCIvAD_HayBCatA.Raw, Group) #count number of participants per group
  r_value <- cor.test(your_data_file_aMCIvAD_HayBCatA.Raw$HayBCatA.Raw,your_data_file_aMCIvAD_HayBCatA.Raw$Age) #find correlation value (r) between dependent variable
  a.tes(t=t_value_effect_size$test$tstat['5 - 3'],n.1=group_number['1','n'],n.2=group_number['2','n'],R=r_value$estimate,q=1) #calculate Cohen's D with the covariate of age
  #for mMCI vs. AD
  your_data_file_mMCIvAD_HayBCatA.Raw <- subset(your_data_file, your_data_file$Group == 4 | your_data_file$Group == 5)
  your_data_file_mMCIvAD_HayBCatA.Raw$Group <- droplevels(your_data_file_mMCIvAD_HayBCatA.Raw$Group)
  group_number <-dplyr::count(your_data_file_mMCIvAD_HayBCatA.Raw, Group) #count number of participants per group
  r_value <- cor.test(your_data_file_mMCIvAD_HayBCatA.Raw$HayBCatA.Raw,your_data_file_mMCIvAD_HayBCatA.Raw$Age) #find correlation value (r) between dependent variable
  a.tes(t=t_value_effect_size$test$tstat['5 - 4'],n.1=group_number['1','n'],n.2=group_number['2','n'],R=r_value$estimate,q=1) #calculate Cohen's D with the covariate of age
#HayBCatB
#remove NAs from dataset for given variable
noNAs_HayBCatBRaw <- your_data_file[,c("ParticipantID","Individual_number","Group","Timepoint","Age","Sex","HayBCatB.Raw")]
noNAs_HayBCatBRaw <- noNAs_HayBCatBRaw[complete.cases(noNAs_HayBCatBRaw), ]
post_hoc_aov_HayBCatB_covar_mod <- lme(HayBCatB.Raw ~ Group*Timepoint + Age,  random = ~1 | Individual_number/Timepoint, data=noNAs_HayBCatBRaw)
t_value_effect_size <- summary(glht(post_hoc_aov_HayBCatB_covar_mod, linfct=mcp(Group="Tukey")))
summary(glht(post_hoc_aov_HayBCatB_covar_mod, linfct=mcp(Group="Tukey")))
#calculate effect sizes (Cohen's D) with covariate  
  #for C vs. AD
  your_data_file_CvAD_HayBCatB.Raw <- subset(your_data_file, your_data_file$Group == 1 | your_data_file$Group == 5)
  your_data_file_CvAD_HayBCatB.Raw$Group <- droplevels(your_data_file_CvAD_HayBCatB.Raw$Group)
  group_number <-dplyr::count(your_data_file_CvAD_HayBCatB.Raw, Group) #count number of participants per group
  r_value <- cor.test(your_data_file_CvAD_HayBCatB.Raw$HayBCatB.Raw,your_data_file_CvAD_HayBCatB.Raw$Age) #find correlation value (r) between dependent variable
  a.tes(t=t_value_effect_size$test$tstat['5 - 1'],n.1=group_number['1','n'],n.2=group_number['2','n'],R=r_value$estimate,q=1) #calculate Cohen's D with the covariate of age
  #for SCD vs. AD
  your_data_file_SCDvAD_HayBCatB.Raw <- subset(your_data_file, your_data_file$Group == 2 | your_data_file$Group == 5)
  your_data_file_SCDvAD_HayBCatB.Raw$Group <- droplevels(your_data_file_SCDvAD_HayBCatB.Raw$Group)
  group_number <-dplyr::count(your_data_file_SCDvAD_HayBCatB.Raw, Group) #count number of participants per group
  r_value <- cor.test(your_data_file_SCDvAD_HayBCatB.Raw$HayBCatB.Raw,your_data_file_SCDvAD_HayBCatB.Raw$Age) #find correlation value (r) between dependent variable
  a.tes(t=t_value_effect_size$test$tstat['5 - 2'],n.1=group_number['1','n'],n.2=group_number['2','n'],R=r_value$estimate,q=1) #calculate Cohen's D with the covariate of age
  #for aMCI vs. AD
  your_data_file_aMCIvAD_HayBCatB.Raw <- subset(your_data_file, your_data_file$Group == 3 | your_data_file$Group == 5)
  your_data_file_aMCIvAD_HayBCatB.Raw$Group <- droplevels(your_data_file_aMCIvAD_HayBCatB.Raw$Group)
  group_number <-dplyr::count(your_data_file_aMCIvAD_HayBCatB.Raw, Group) #count number of participants per group
  r_value <- cor.test(your_data_file_aMCIvAD_HayBCatB.Raw$HayBCatB.Raw,your_data_file_aMCIvAD_HayBCatB.Raw$Age) #find correlation value (r) between dependent variable
  a.tes(t=t_value_effect_size$test$tstat['5 - 3'],n.1=group_number['1','n'],n.2=group_number['2','n'],R=r_value$estimate,q=1) #calculate Cohen's D with the covariate of age
  #for mMCI vs. AD
  your_data_file_mMCIvAD_HayBCatB.Raw <- subset(your_data_file, your_data_file$Group == 4 | your_data_file$Group == 5)
  your_data_file_mMCIvAD_HayBCatB.Raw$Group <- droplevels(your_data_file_mMCIvAD_HayBCatB.Raw$Group)
  group_number <-dplyr::count(your_data_file_mMCIvAD_HayBCatB.Raw, Group) #count number of participants per group
  r_value <- cor.test(your_data_file_mMCIvAD_HayBCatB.Raw$HayBCatB.Raw,your_data_file_mMCIvAD_HayBCatB.Raw$Age) #find correlation value (r) between dependent variable
  a.tes(t=t_value_effect_size$test$tstat['5 - 4'],n.1=group_number['1','n'],n.2=group_number['2','n'],R=r_value$estimate,q=1) #calculate Cohen's D with the covariate of age
#HayCatTotalError.Raw
#remove NAs from dataset for given variable
noNAs_HayCatTotalError.Raw <- your_data_file[,c("ParticipantID","Individual_number","Group","Timepoint","Age","Sex","HayCatTotalError.Raw")]
noNAs_HayCatTotalError.Raw <- noNAs_HayCatTotalError.Raw[complete.cases(noNAs_HayCatTotalError.Raw), ]
post_hoc_aov_HayCatTotalError.Raw_covar_mod <- lme(HayCatTotalError.Raw~ Group*Timepoint + Age,  random = ~1 | Individual_number/Timepoint, data=noNAs_HayCatTotalError.Raw)
t_value_effect_size <- summary(glht(post_hoc_aov_HayCatTotalError.Raw_covar_mod, linfct=mcp(Group="Tukey")))
summary(glht(post_hoc_aov_HayCatTotalError.Raw_covar_mod, linfct=mcp(Group="Tukey")))
#calculate effect sizes (Cohen's D) with covariate  
  #for C vs. AD
  your_data_file_CvAD_HayCatTotalError.Raw <- subset(your_data_file, your_data_file$Group == 1 | your_data_file$Group == 5)
  your_data_file_CvAD_HayCatTotalError.Raw$Group <- droplevels(your_data_file_CvAD_HayCatTotalError.Raw$Group)
  group_number <-dplyr::count(your_data_file_CvAD_HayCatTotalError.Raw, Group) #count number of participants per group
  r_value <- cor.test(your_data_file_CvAD_HayCatTotalError.Raw$HayCatTotalError.Raw,your_data_file_CvAD_HayCatTotalError.Raw$Age) #find correlation value (r) between dependent variable
  a.tes(t=t_value_effect_size$test$tstat['5 - 1'],n.1=group_number['1','n'],n.2=group_number['2','n'],R=r_value$estimate,q=1) #calculate Cohen's D with the covariate of age
  #for SCD vs. AD
  your_data_file_SCDvAD_HayCatTotalError.Raw <- subset(your_data_file, your_data_file$Group == 2 | your_data_file$Group == 5)
  your_data_file_SCDvAD_HayCatTotalError.Raw$Group <- droplevels(your_data_file_SCDvAD_HayCatTotalError.Raw$Group)
  group_number <-dplyr::count(your_data_file_SCDvAD_HayCatTotalError.Raw, Group) #count number of participants per group
  r_value <- cor.test(your_data_file_SCDvAD_HayCatTotalError.Raw$HayCatTotalError.Raw,your_data_file_SCDvAD_HayCatTotalError.Raw$Age) #find correlation value (r) between dependent variable
  a.tes(t=t_value_effect_size$test$tstat['5 - 2'],n.1=group_number['1','n'],n.2=group_number['2','n'],R=r_value$estimate,q=1) #calculate Cohen's D with the covariate of age
  #for aMCI vs. AD
  your_data_file_aMCIvAD_HayCatTotalError.Raw <- subset(your_data_file, your_data_file$Group == 3 | your_data_file$Group == 5)
  your_data_file_aMCIvAD_HayCatTotalError.Raw$Group <- droplevels(your_data_file_aMCIvAD_HayCatTotalError.Raw$Group)
  group_number <-dplyr::count(your_data_file_aMCIvAD_HayCatTotalError.Raw, Group) #count number of participants per group
  r_value <- cor.test(your_data_file_aMCIvAD_HayCatTotalError.Raw$HayCatTotalError.Raw,your_data_file_aMCIvAD_HayCatTotalError.Raw$Age) #find correlation value (r) between dependent variable
  a.tes(t=t_value_effect_size$test$tstat['5 - 3'],n.1=group_number['1','n'],n.2=group_number['2','n'],R=r_value$estimate,q=1) #calculate Cohen's D with the covariate of age
  #for mMCI vs. AD
  your_data_file_mMCIvAD_HayCatTotalError.Raw <- subset(your_data_file, your_data_file$Group == 4 | your_data_file$Group == 5)
  your_data_file_mMCIvAD_HayCatTotalError.Raw$Group <- droplevels(your_data_file_mMCIvAD_HayCatTotalError.Raw$Group)
  group_number <-dplyr::count(your_data_file_mMCIvAD_HayCatTotalError.Raw, Group) #count number of participants per group
  r_value <- cor.test(your_data_file_mMCIvAD_HayCatTotalError.Raw$HayCatTotalError.Raw,your_data_file_mMCIvAD_HayCatTotalError.Raw$Age) #find correlation value (r) between dependent variable
  a.tes(t=t_value_effect_size$test$tstat['5 - 4'],n.1=group_number['1','n'],n.2=group_number['2','n'],R=r_value$estimate,q=1) #calculate Cohen's D with the covariate of age

#ColorNaming
#remove NAs from dataset for given variable
noNAs_ColorNamingRaw <- your_data_file[,c("ParticipantID","Individual_number","Group","Timepoint","Age","Sex","ColorNaming.Raw")]
noNAs_ColorNamingRaw <- noNAs_ColorNamingRaw[complete.cases(noNAs_ColorNamingRaw), ]
post_hoc_aov_ColorNaming_covar_mod <- lme(ColorNaming.Raw ~ Group*Timepoint + Age,  random = ~1 | Individual_number/Timepoint, data=noNAs_ColorNamingRaw)
summary(post_hoc_aov_ColorNaming_covar_mod)
summary(glht(post_hoc_aov_ColorNaming_covar_mod, linfct=mcp(Group="Tukey")))
summary(glht(post_hoc_aov_ColorNaming_covar_mod, linfct=mcp(Timepoint="Tukey")))
#interaction - ColorNaming
#Tukey test
TukeyHSD(aov(ColorNaming.Raw~Group*Timepoint+as.factor(Age), data = your_data_file)) 
# #Simple main effect w/ interaction - for Group
# posthoc_ME_Group_ColorNaming_Age <- your_data_file %>%
#   group_by(Timepoint) %>%
#   anova_test(dv=TrailsB.Raw,wid=Individual_number,between=Group,covariate=Age) %>%
#   adjust_pvalue(method="fdr")
# posthoc_ME_Group_ColorNaming_Age  ##within each timepoint (F0 & F2), there are Group differences##
# #Run Pairwise comparison between groups levels if simple main effects (above) is sig.
# posthoc_pairwise_Group_ColorNaming_Age <- your_data_file %>%
#   group_by(Timepoint) %>%
#   tukey_hsd(ColorNaming.Raw ~ Group+as.factor(Age))
# posthoc_pairwise_Group_ColorNaming_Age  ##examines all Group contrasts within each timepoint (F0 & F2) ##
# #Simple main effect w/ ineraction - for Timepoint
# posthoc_ME_Timepoint_ColorNaming_Age <- your_data_file %>%
#   group_by(Group) %>%
#   anova_test(dv=ColorNaming.Raw,wid=Individual_number,within=Timepoint,covariate=Age,effect.size = "pes") %>%
#   get_anova_table() %>%
#   adjust_pvalue(method="fdr")
# posthoc_ME_Timepoint_ColorNaming_Age ## Across each Group, where were there sig. differences between timepoints (F0 vs. F2)? ##
# #Pairwise comparison between groups levels if simple main effects (above) is sig.
# posthoc_pairwise_Timepoint_ColorNaming_Age <- your_data_file %>%
#   group_by(Group) %>%
#   tukey_hsd(ColorNaming.Raw ~ Timepoint+as.factor(Age), paired = TRUE)
# posthoc_pairwise_Timepoint_ColorNaming_Age ##examines all Timepoint contrasts between each Group (1,2,3,4,5) ##
#test if there is a difference among differences (e.g., is the aMCI group difference for color naming between the time points 
#(F0 - F2) sig. greater compared to the other groups?)

#subtract differences in F2 - F0 for color naming with covariate age
ColorNaming_diff_data<-your_data_file[,c("ParticipantID","Individual_number","Group","Timepoint","Age","Sex","ColorNaming.Raw")]
F0_ColorNaming_data<-ColorNaming_diff_data[ColorNaming_diff_data$Timepoint=='F0', ]
F2_ColorNaming_data<-ColorNaming_diff_data[ColorNaming_diff_data$Timepoint=='F2', ]
ColorNamingDiff<-F2_ColorNaming_data$ColorNaming.Raw - F0_ColorNaming_data$ColorNaming.Raw
F0_ColorNaming_data$ColorNamingDiff<-ColorNamingDiff
#run one-way ANOVA on the F2-F0 differences in color naming between groups
ColorNamingDiff_covar_mod <- lm(ColorNamingDiff ~ Group+Age, data = F0_ColorNaming_data)
anova(ColorNamingDiff_covar_mod) #sig. difference
#effect size omnibus ANOVA
etaSquared(ColorNamingDiff_covar_mod)
#post hoc f/u test
#run pairwise comparisons (post-hoc Tukey), given that the F-test was significant. 
post_hoc_ColorNamingDiff_covar_mod <- glht(ColorNamingDiff_covar_mod, linfct = mcp(Group = "Tukey"))
summary(post_hoc_ColorNamingDiff_covar_mod)
confint(post_hoc_ColorNamingDiff_covar_mod)
t_value_effect_size <- summary(glht(ColorNamingDiff_covar_mod, linfct = mcp(Group = "Tukey")))
#effect size for sig. post hoc tests with covariate
  #for SCD vs. aMCI
  your_data_file_SCDvaMCI_ColorNamingDiff <- subset(your_data_file, your_data_file$Group == 2 | your_data_file$Group == 3)
  your_data_file_SCDvaMCI_ColorNamingDiff$Group <- droplevels(your_data_file_SCDvaMCI_ColorNamingDiff$Group)
  group_number <-dplyr::count(your_data_file_SCDvaMCI_ColorNamingDiff, Group) #count number of participants per group
  r_value <- cor.test(your_data_file_SCDvaMCI_ColorNamingDiff$ColorNaming.Raw,your_data_file_SCDvaMCI_ColorNamingDiff$Age) #find correlation value (r) between dependent variable
  a.tes(t=t_value_effect_size$test$tstat['3 - 2'],n.1=group_number['1','n'],n.2=group_number['2','n'],R=r_value$estimate,q=1) #calculate Cohen's D with the covariate of age
#WordReading
#remove NAs from dataset for given variable
noNAs_WordReadingRaw <- your_data_file[,c("ParticipantID","Individual_number","Group","Timepoint","Age","Sex","WordReading.Raw")]
noNAs_WordReadingRaw <- noNAs_WordReadingRaw[complete.cases(noNAs_WordReadingRaw), ]
post_hoc_aov_WordReading_covar_mod <- lme(WordReading.Raw ~ Group*Timepoint + Age,  random = ~1 | Individual_number/Timepoint, data=noNAs_WordReadingRaw)
summary(glht(post_hoc_aov_WordReading_covar_mod, linfct=mcp(Group="Tukey"))) #no sig 
summary(glht(post_hoc_aov_WordReading_covar_mod, linfct=mcp(Timepoint="Tukey")))
#Inhibition
#remove NAs from dataset for given variable
noNAs_InhibitionRaw <- your_data_file[,c("ParticipantID","Individual_number","Group","Timepoint","Age","Sex","Inhibition.Raw")]
noNAs_InhibitionRaw <- noNAs_InhibitionRaw[complete.cases(noNAs_InhibitionRaw), ]
post_hoc_aov_Inhibition_covar_mod <- lme(Inhibition.Raw ~ Group*Timepoint + Age,  random = ~1 | Individual_number/Timepoint, data=noNAs_InhibitionRaw)
summary(glht(post_hoc_aov_Inhibition_covar_mod, linfct=mcp(Group="Tukey")))
summary(glht(post_hoc_aov_Inhibition_covar_mod, linfct=mcp(Timepoint="Tukey")))
#Inhibition.Word.Reading
#remove NAs from dataset for given variable
noNAs_InhibitionWordReading <- your_data_file[,c("ParticipantID","Individual_number","Group","Timepoint","Age","Sex","Inhibition.Word.Reading")]
noNAs_InhibitionWordReading <- noNAs_InhibitionWordReading[complete.cases(noNAs_InhibitionWordReading), ]
post_hoc_aov_InhibitionWordReading_covar_mod <- lme(Inhibition.Word.Reading ~ Group*Timepoint + Age,  random = ~1 | Individual_number/Timepoint, data=noNAs_InhibitionWordReading)
summary(glht(post_hoc_aov_InhibitionWordReading_covar_mod, linfct=mcp(Group="Tukey")))
#Interference score (Golden Stroop)
# #remove NAs from dataset for given variable
# noNAs_Golden_Stroop_interference_score <- your_data_file[,c("ParticipantID","Individual_number","Group","Timepoint","Age","Sex","Golden_Stroop_interference_score")]
# noNAs_Golden_Stroop_interference_score <- noNAs_Golden_Stroop_interference_score[complete.cases(noNAs_Golden_Stroop_interference_score), ]
# post_hoc_aov_Golden_Stroop_interference_score_covar_mod <- lme(Golden_Stroop_interference_score ~ Group*Timepoint + Age,  random = ~1 | Individual_number/Timepoint, data=noNAs_Golden_Stroop_interference_score)
# summary(glht(post_hoc_aov_Golden_Stroop_interference_score_covar_mod, linfct=mcp(Group="Tukey")))
#Interference score (Caffarra 2002)
#remove NAs from dataset for given variable
noNAs_Interference_score_Caffarra_2002 <- your_data_file[,c("ParticipantID","Individual_number","Group","Timepoint","Age","Sex","Interference_score_Caffarra_2002")]
noNAs_Interference_score_Caffarra_2002 <- noNAs_Interference_score_Caffarra_2002[complete.cases(noNAs_Interference_score_Caffarra_2002), ]
post_hoc_aov_Interference_score_Caffarra_2002_covar_mod <- lme(Interference_score_Caffarra_2002 ~ Group*Timepoint + Age,  random = ~1 | Individual_number/Timepoint, data=noNAs_Interference_score_Caffarra_2002)
summary(glht(post_hoc_aov_Interference_score_Caffarra_2002_covar_mod, linfct=mcp(Group="Tukey")))

#TrailsA
#remove NAs from dataset for given variable
noNAs_TrailsARaw <- your_data_file[,c("ParticipantID","Individual_number","Group","Timepoint","Age","Sex","TrailsA.Raw")]
noNAs_TrailsARaw <- noNAs_TrailsARaw[complete.cases(noNAs_TrailsARaw), ]
post_hoc_aov_TrailsA_covar_mod <- lme(TrailsA.Raw ~ Group*Timepoint + Age,  random = ~1 | Individual_number/Timepoint, data=noNAs_TrailsARaw)
summary(glht(post_hoc_aov_TrailsA_covar_mod, linfct=mcp(Group="Tukey")))
#TrailsB
#remove NAs from dataset for given variable
noNAs_TrailsBRaw <- your_data_file[,c("ParticipantID","Individual_number","Group","Timepoint","Age","Sex","TrailsB.Raw")]
noNAs_TrailsBRaw <- noNAs_TrailsBRaw[complete.cases(noNAs_TrailsBRaw), ]
post_hoc_aov_TrailsB_covar_mod <- lme(TrailsB.Raw ~ Group*Timepoint + Age,  random = ~1 | Individual_number/Timepoint, data=noNAs_TrailsBRaw)
summary(glht(post_hoc_aov_TrailsB_covar_mod, linfct=mcp(Group="Tukey")))
summary(glht(post_hoc_aov_TrailsB_covar_mod, linfct=mcp(Timepoint="Tukey")))
#interaction - TrailsB
#Tukey test
TukeyHSD(aov(TrailsB.Raw~Group*Timepoint+as.factor(Age), data = your_data_file)) 
#subtract differences in F2 - F0 for color naming with covariate age
TrailsB_diff_data<-your_data_file[,c("ParticipantID","Individual_number","Group","Timepoint","Age","Sex","TrailsB.Raw")]
F0_TrailsB_data<-TrailsB_diff_data[TrailsB_diff_data$Timepoint=='F0', ]
F2_TrailsB_data<-TrailsB_diff_data[TrailsB_diff_data$Timepoint=='F2', ]
TrailsBDiff<-F2_TrailsB_data$TrailsB.Raw - F0_TrailsB_data$TrailsB.Raw
F0_TrailsB_data$TrailsBDiff<-TrailsBDiff
#run one-way ANOVA on the F2-F0 differences in color naming between groups
TrailsBDiff_covar_mod <- lm(TrailsBDiff ~ Group+Age, data = F0_TrailsB_data)
anova(TrailsBDiff_covar_mod) #sig. difference
#effect size omnibus ANOVA
etaSquared(TrailsBDiff_covar_mod)
#post hoc f/u test
#run pairwise comparisons (post-hoc Tukey), given that the F-test was significant. 
post_hoc_TrailsBDiff_covar_mod <- glht(TrailsBDiff_covar_mod, linfct = mcp(Group = "Tukey"))
summary(post_hoc_TrailsBDiff_covar_mod)
confint(post_hoc_TrailsBDiff_covar_mod)
t_value_effect_size <- summary(glht(TrailsBDiff_covar_mod, linfct = mcp(Group = "Tukey")))
#effect size for sig. post hoc tests with covariate
  #for C vs. mMCI
  your_data_file_CvmMCI_TrailsBDiff <- subset(your_data_file, your_data_file$Group == 1 | your_data_file$Group == 4)
  your_data_file_CvmMCI_TrailsBDiff$Group <- droplevels(your_data_file_CvmMCI_TrailsBDiff$Group)
  group_number <-dplyr::count(your_data_file_CvmMCI_TrailsBDiff, Group) #count number of participants per group
  r_value <- cor.test(your_data_file_CvmMCI_TrailsBDiff$TrailsB.Raw,your_data_file_CvmMCI_TrailsBDiff$Age) #find correlation value (r) between dependent variable
  a.tes(t=t_value_effect_size$test$tstat['4 - 1'],n.1=group_number['1','n'],n.2=group_number['2','n'],R=r_value$estimate,q=1) #calculate Cohen's D with the covariate of age
  #for SCD vs. mMCI
  your_data_file_SCDvmMCI_TrailsBDiff <- subset(your_data_file, your_data_file$Group == 2 | your_data_file$Group == 4)
  your_data_file_SCDvmMCI_TrailsBDiff$Group <- droplevels(your_data_file_SCDvmMCI_TrailsBDiff$Group)
  group_number <-dplyr::count(your_data_file_SCDvmMCI_TrailsBDiff, Group) #count number of participants per group
  r_value <- cor.test(your_data_file_SCDvmMCI_TrailsBDiff$TrailsB.Raw,your_data_file_SCDvmMCI_TrailsBDiff$Age) #find correlation value (r) between dependent variable
  a.tes(t=t_value_effect_size$test$tstat['4 - 2'],n.1=group_number['1','n'],n.2=group_number['2','n'],R=r_value$estimate,q=1) #calculate Cohen's D with the covariate of age
#TrailsB/TrailsA
noNAs_TMTBTMTA <- your_data_file[,c("ParticipantID","Individual_number","Group","Timepoint","Age","Sex","TMT.B.TMT.A")]
noNAs_TMTBTMTA <- noNAs_TMTBTMTA[complete.cases(noNAs_TMTBTMTA), ]
post_hoc_aov_TMTBTMTA_covar_mod <- lme(TMT.B.TMT.A ~ Group*Timepoint + Age,  random = ~1 | Individual_number/Timepoint, data=noNAs_TMTBTMTA)
summary(glht(post_hoc_aov_TMTBTMTA_covar_mod, linfct=mcp(Timepoint="Tukey")))

#LetFluency
#remove NAs from dataset for given variable
noNAs_LetFluencyRaw <- your_data_file[,c("ParticipantID","Individual_number","Group","Timepoint","Age","Sex","LetFluency.Raw")]
noNAs_LetFluencyRaw <- noNAs_LetFluencyRaw[complete.cases(noNAs_LetFluencyRaw), ]
post_hoc_aov_LetFluency_covar_mod <- lme(LetFluency.Raw ~ Group*Timepoint + Age,  random = ~1 | Individual_number/Timepoint, data=noNAs_LetFluencyRaw)
summary(glht(post_hoc_aov_LetFluency_covar_mod, linfct=mcp(Group="Tukey")))
t_value_effect_size<-summary(glht(post_hoc_aov_LetFluency_covar_mod, linfct=mcp(Group="Tukey")))
#calculate effect size for the significant Group levels
  #effect size for sig. post hoc tests - need to use a.tes (from the compute.es package) to account for covariates
  #for C vs. SCD 
  your_data_file_CvSCD_LetFluency.Raw <- subset(your_data_file, your_data_file$Group == 1 | your_data_file$Group == 2)
  your_data_file_CvSCD_LetFluency.Raw$Group <- droplevels(your_data_file_CvSCD_LetFluency.Raw$Group)
  group_number <-dplyr::count(your_data_file_CvSCD_LetFluency.Raw, Group) #count number of participants per group
  r_value <- cor.test(your_data_file_CvSCD_LetFluency.Raw$LetFluency.Raw,your_data_file_CvSCD_LetFluency.Raw$Age) #find correlation value (r) between dependent variable
  a.tes(t=t_value_effect_size$test$tstat['2 - 1'],n.1=group_number['1','n'],n.2=group_number['2','n'],R=r_value$estimate,q=1) #calculate Cohen's D with the covariate of age
  #for C vs. aMCI 
  your_data_file_CvaMCI_LetFluency.Raw <- subset(your_data_file, your_data_file$Group == 1 | your_data_file$Group == 3)
  your_data_file_CvaMCI_LetFluency.Raw$Group <- droplevels(your_data_file_CvaMCI_LetFluency.Raw$Group)
  group_number <-dplyr::count(your_data_file_CvaMCI_LetFluency.Raw, Group) #count number of participants per group
  r_value <- cor.test(your_data_file_CvaMCI_LetFluency.Raw$LetFluency.Raw,your_data_file_CvaMCI_LetFluency.Raw$Age) #find correlation value (r) between dependent variable
  a.tes(t=t_value_effect_size$test$tstat['3 - 1'],n.1=group_number['1','n'],n.2=group_number['2','n'],R=r_value$estimate,q=1) #calculate Cohen's D with the covariate of age
#interaction - LetFluency.Raw
#Tukey test
TukeyHSD(aov(LetFluency.Raw~Group*Timepoint+as.factor(Age), data = your_data_file)) 
#subtract differences in F2 - F0 for color naming with covariate age
LetFluency.Raw_diff_data<-your_data_file[,c("ParticipantID","Individual_number","Group","Timepoint","Age","Sex","LetFluency.Raw")]
F0_LetFluency.Raw_data<-LetFluency.Raw_diff_data[LetFluency.Raw_diff_data$Timepoint=='F0', ]
F2_LetFluency.Raw_data<-LetFluency.Raw_diff_data[LetFluency.Raw_diff_data$Timepoint=='F2', ]
LetFluency.RawDiff<-F2_LetFluency.Raw_data$LetFluency.Raw - F0_LetFluency.Raw_data$LetFluency.Raw
F0_LetFluency.Raw_data$LetFluency.RawDiff<-LetFluency.RawDiff
#run one-way ANOVA on the F2-F0 differences in LetFluency.Raw between groups
LetFluency.RawDiff_covar_mod <- lm(LetFluency.RawDiff ~ Group+Age, data = F0_LetFluency.Raw_data)
anova(LetFluency.RawDiff_covar_mod) #sig. difference
#effect size omnibus ANOVA
etaSquared(LetFluency.RawDiff_covar_mod)
#post hoc f/u test
#run pairwise comparisons (post-hoc Tukey), given that the F-test was significant. 
post_hoc_LetFluency.RawDiff_covar_mod <- glht(LetFluency.RawDiff_covar_mod, linfct = mcp(Group = "Tukey"))
summary(post_hoc_LetFluency.RawDiff_covar_mod)
confint(post_hoc_LetFluency.RawDiff_covar_mod)
t_value_effect_size <- summary(glht(LetFluency.RawDiff_covar_mod, linfct = mcp(Group = "Tukey")))
#effect size for sig. post hoc tests with covariate
  #for C vs. AD
  your_data_file_CvAD_LetFluency.RawDiff <- subset(your_data_file, your_data_file$Group == 1 | your_data_file$Group == 5)
  your_data_file_CvAD_LetFluency.RawDiff$Group <- droplevels(your_data_file_CvAD_LetFluency.RawDiff$Group)
  group_number <-dplyr::count(your_data_file_CvAD_LetFluency.RawDiff, Group) #count number of participants per group
  r_value <- cor.test(your_data_file_CvAD_LetFluency.RawDiff$LetFluency.Raw,your_data_file_CvAD_LetFluency.RawDiff$Age) #find correlation value (r) between dependent variable
  a.tes(t=t_value_effect_size$test$tstat['5 - 1'],n.1=group_number['1','n'],n.2=group_number['2','n'],R=r_value$estimate,q=1) #calculate Cohen's D with the covariate of age
  #for SCD vs. AD
  your_data_file_SCDvAD_LetFluency.RawDiff <- subset(your_data_file, your_data_file$Group == 2 | your_data_file$Group == 5)
  your_data_file_SCDvAD_LetFluency.RawDiff$Group <- droplevels(your_data_file_SCDvAD_LetFluency.RawDiff$Group)
  group_number <-dplyr::count(your_data_file_SCDvAD_LetFluency.RawDiff, Group) #count number of participants per group
  r_value <- cor.test(your_data_file_SCDvAD_LetFluency.RawDiff$LetFluency.Raw,your_data_file_SCDvAD_LetFluency.RawDiff$Age) #find correlation value (r) between dependent variable
  a.tes(t=t_value_effect_size$test$tstat['5 - 2'],n.1=group_number['1','n'],n.2=group_number['2','n'],R=r_value$estimate,q=1) #calculate Cohen's D with the covariate of age  
  #for aMCI vs. AD
  your_data_file_aMCIvAD_LetFluency.RawDiff <- subset(your_data_file, your_data_file$Group == 3 | your_data_file$Group == 5)
  your_data_file_aMCIvAD_LetFluency.RawDiff$Group <- droplevels(your_data_file_aMCIvAD_LetFluency.RawDiff$Group)
  group_number <-dplyr::count(your_data_file_aMCIvAD_LetFluency.RawDiff, Group) #count number of participants per group
  r_value <- cor.test(your_data_file_aMCIvAD_LetFluency.RawDiff$LetFluency.Raw,your_data_file_aMCIvAD_LetFluency.RawDiff$Age) #find correlation value (r) between dependent variable
  a.tes(t=t_value_effect_size$test$tstat['5 - 2'],n.1=group_number['1','n'],n.2=group_number['2','n'],R=r_value$estimate,q=1) #calculate Cohen's D with the covariate of age  

#CatFluency
#remove NAs from dataset for given variable
noNAs_CatFluencyRaw <- your_data_file[,c("ParticipantID","Individual_number","Group","Timepoint","Age","Sex","CatFluency.Raw")]
noNAs_CatFluencyRaw <- noNAs_CatFluencyRaw[complete.cases(noNAs_CatFluencyRaw), ]
post_hoc_aov_CatFluency_covar_mod <- lme(CatFluency.Raw ~ Group*Timepoint + Age,  random = ~1 | Individual_number/Timepoint, data=noNAs_CatFluencyRaw)
summary(glht(post_hoc_aov_CatFluency_covar_mod, linfct=mcp(Group="Tukey")))
t_value_effect_size<-summary(glht(post_hoc_aov_CatFluency_covar_mod, linfct=mcp(Group="Tukey")))
#calculate effect size for the significant Group levels
  #effect size for sig. post hoc tests - need to use a.tes (from the compute.es package) to account for covariates
  #for C vs. aMCI 
  your_data_file_CvaMCI_CatFluency.Raw <- subset(your_data_file, your_data_file$Group == 1 | your_data_file$Group == 3)
  your_data_file_CvaMCI_CatFluency.Raw$Group <- droplevels(your_data_file_CvaMCI_CatFluency.Raw$Group)
  group_number <-dplyr::count(your_data_file_CvaMCI_CatFluency.Raw, Group) #count number of participants per group
  r_value <- cor.test(your_data_file_CvaMCI_CatFluency.Raw$CatFluency.Raw,your_data_file_CvaMCI_CatFluency.Raw$Age) #find correlation value (r) between dependent variable
  a.tes(t=t_value_effect_size$test$tstat['3 - 1'],n.1=group_number['1','n'],n.2=group_number['2','n'],R=r_value$estimate,q=1) #calculate Cohen's D with the covariate of age
  #for C vs. mMCI 
  your_data_file_CvmMCI_CatFluency.Raw <- subset(your_data_file, your_data_file$Group == 1 | your_data_file$Group == 4)
  your_data_file_CvmMCI_CatFluency.Raw$Group <- droplevels(your_data_file_CvmMCI_CatFluency.Raw$Group)
  group_number <-dplyr::count(your_data_file_CvmMCI_CatFluency.Raw, Group) #count number of participants per group
  r_value <- cor.test(your_data_file_CvmMCI_CatFluency.Raw$CatFluency.Raw,your_data_file_CvmMCI_CatFluency.Raw$Age) #find correlation value (r) between dependent variable
  a.tes(t=t_value_effect_size$test$tstat['4 - 1'],n.1=group_number['1','n'],n.2=group_number['2','n'],R=r_value$estimate,q=1) #calculate Cohen's D with the covariate of age
  #for C vs. AD
  your_data_file_CvAD_CatFluency.Raw <- subset(your_data_file, your_data_file$Group == 1 | your_data_file$Group == 5)
  your_data_file_CvAD_CatFluency.Raw$Group <- droplevels(your_data_file_CvAD_CatFluency.Raw$Group)
  group_number <-dplyr::count(your_data_file_CvAD_CatFluency.Raw, Group) #count number of participants per group
  r_value <- cor.test(your_data_file_CvAD_CatFluency.Raw$CatFluency.Raw,your_data_file_CvAD_CatFluency.Raw$Age) #find correlation value (r) between dependent variable
  a.tes(t=t_value_effect_size$test$tstat['5 - 1'],n.1=group_number['1','n'],n.2=group_number['2','n'],R=r_value$estimate,q=1) #calculate Cohen's D with the covariate of age
  #for SCD vs. aMCI 
  your_data_file_SCDvaMCI_CatFluency.Raw <- subset(your_data_file, your_data_file$Group == 2 | your_data_file$Group == 3)
  your_data_file_SCDvaMCI_CatFluency.Raw$Group <- droplevels(your_data_file_SCDvaMCI_CatFluency.Raw$Group)
  group_number <-dplyr::count(your_data_file_SCDvaMCI_CatFluency.Raw, Group) #count number of participants per group
  r_value <- cor.test(your_data_file_SCDvaMCI_CatFluency.Raw$CatFluency.Raw,your_data_file_SCDvaMCI_CatFluency.Raw$Age) #find correlation value (r) between dependent variable
  a.tes(t=t_value_effect_size$test$tstat['3 - 2'],n.1=group_number['1','n'],n.2=group_number['2','n'],R=r_value$estimate,q=1) #calculate Cohen's D with the covariate of age
  #for SCD vs. mMCI 
  your_data_file_SCDvmMCI_CatFluency.Raw <- subset(your_data_file, your_data_file$Group == 2 | your_data_file$Group == 4)
  your_data_file_SCDvmMCI_CatFluency.Raw$Group <- droplevels(your_data_file_SCDvmMCI_CatFluency.Raw$Group)
  group_number <-dplyr::count(your_data_file_SCDvmMCI_CatFluency.Raw, Group) #count number of participants per group
  r_value <- cor.test(your_data_file_SCDvmMCI_CatFluency.Raw$CatFluency.Raw,your_data_file_SCDvmMCI_CatFluency.Raw$Age) #find correlation value (r) between dependent variable
  a.tes(t=t_value_effect_size$test$tstat['4 - 2'],n.1=group_number['1','n'],n.2=group_number['2','n'],R=r_value$estimate,q=1) #calculate Cohen's D with the covariate of age
  #for SCD vs. AD 
  your_data_file_SCDvAD_CatFluency.Raw <- subset(your_data_file, your_data_file$Group == 2 | your_data_file$Group == 5)
  your_data_file_SCDvAD_CatFluency.Raw$Group <- droplevels(your_data_file_SCDvAD_CatFluency.Raw$Group)
  group_number <-dplyr::count(your_data_file_SCDvAD_CatFluency.Raw, Group) #count number of participants per group
  r_value <- cor.test(your_data_file_SCDvAD_CatFluency.Raw$CatFluency.Raw,your_data_file_SCDvAD_CatFluency.Raw$Age) #find correlation value (r) between dependent variable
  a.tes(t=t_value_effect_size$test$tstat['5 - 2'],n.1=group_number['1','n'],n.2=group_number['2','n'],R=r_value$estimate,q=1) #calculate Cohen's D with the covariate of age
#Switching
#remove NAs from dataset for given variable
noNAs_SwitchingRaw <- your_data_file[,c("ParticipantID","Individual_number","Group","Timepoint","Age","Sex","Switching.Raw")]
noNAs_SwitchingRaw <- noNAs_SwitchingRaw[complete.cases(noNAs_SwitchingRaw), ]
post_hoc_aov_Switching_covar_mod <- lme(Switching.Raw ~ Group*Timepoint + Age,  random = ~1 | Individual_number/Timepoint, data=noNAs_SwitchingRaw)
summary(glht(post_hoc_aov_Switching_covar_mod, linfct=mcp(Group="Tukey")))
t_value_effect_size <- summary(glht(post_hoc_aov_Switching_covar_mod, linfct=mcp(Group="Tukey"))) #save to variable for effect size testing
summary(glht(post_hoc_aov_Switching_covar_mod, linfct=mcp(Timepoint="Tukey")))
#calculate effect size for the significant Group levels
#effect size for sig. post hoc tests - need to use a.tes (from the compute.es package) to account for covariates
  #for C vs. mMCI 
  your_data_file_CvmMCI_Switching.Raw <- subset(your_data_file, your_data_file$Group == 1 | your_data_file$Group == 4)
  your_data_file_CvmMCI_Switching.Raw$Group <- droplevels(your_data_file_CvmMCI_Switching.Raw$Group)
  group_number <-dplyr::count(your_data_file_CvmMCI_Switching.Raw, Group) #count number of participants per group
  r_value <- cor.test(your_data_file_CvmMCI_Switching.Raw$Switching.Raw,your_data_file_CvmMCI_Switching.Raw$Age) #find correlation value (r) between dependent variable
  a.tes(t=t_value_effect_size$test$tstat['4 - 1'],n.1=group_number['1','n'],n.2=group_number['2','n'],R=r_value$estimate,q=1) #calculate Cohen's D with the covariate of age
  #for C vs. AD
  your_data_file_CvAD_Switching.Raw <- subset(your_data_file, your_data_file$Group == 1 | your_data_file$Group == 5)
  your_data_file_CvAD_Switching.Raw$Group <- droplevels(your_data_file_CvAD_Switching.Raw$Group)
  group_number <-dplyr::count(your_data_file_CvAD_Switching.Raw, Group) #count number of participants per group
  r_value <- cor.test(your_data_file_CvAD_Switching.Raw$Switching.Raw,your_data_file_CvAD_Switching.Raw$Age) #find correlation value (r) between dependent variable
  a.tes(t=t_value_effect_size$test$tstat['5 - 1'],n.1=group_number['1','n'],n.2=group_number['2','n'],R=r_value$estimate,q=1) #calculate Cohen's D with the covariate of age
  #for SCD vs. aMCI 
  your_data_file_SCDvaMCI_Switching.Raw <- subset(your_data_file, your_data_file$Group == 2 | your_data_file$Group == 3)
  your_data_file_SCDvaMCI_Switching.Raw$Group <- droplevels(your_data_file_SCDvaMCI_Switching.Raw$Group)
  group_number <-dplyr::count(your_data_file_SCDvaMCI_Switching.Raw, Group) #count number of participants per group
  r_value <- cor.test(your_data_file_SCDvaMCI_Switching.Raw$Switching.Raw,your_data_file_SCDvaMCI_Switching.Raw$Age) #find correlation value (r) between dependent variable
  a.tes(t=t_value_effect_size$test$tstat['3 - 2'],n.1=group_number['1','n'],n.2=group_number['2','n'],R=r_value$estimate,q=1) #calculate Cohen's D with the covariate of age
  #for SCD vs. mMCI 
  your_data_file_SCDvmMCI_Switching.Raw <- subset(your_data_file, your_data_file$Group == 2 | your_data_file$Group == 4)
  your_data_file_SCDvmMCI_Switching.Raw$Group <- droplevels(your_data_file_SCDvmMCI_Switching.Raw$Group)
  group_number <-dplyr::count(your_data_file_SCDvmMCI_Switching.Raw, Group) #count number of participants per group
  r_value <- cor.test(your_data_file_SCDvmMCI_Switching.Raw$Switching.Raw,your_data_file_SCDvmMCI_Switching.Raw$Age) #find correlation value (r) between dependent variable
  a.tes(t=t_value_effect_size$test$tstat['4 - 2'],n.1=group_number['1','n'],n.2=group_number['2','n'],R=r_value$estimate,q=1) #calculate Cohen's D with the covariate of age
  #for SCD vs. AD 
  your_data_file_SCDvAD_Switching.Raw <- subset(your_data_file, your_data_file$Group == 2 | your_data_file$Group == 5)
  your_data_file_SCDvAD_Switching.Raw$Group <- droplevels(your_data_file_SCDvAD_Switching.Raw$Group)
  group_number <-dplyr::count(your_data_file_SCDvAD_Switching.Raw, Group) #count number of participants per group
  r_value <- cor.test(your_data_file_SCDvAD_Switching.Raw$Switching.Raw,your_data_file_SCDvAD_Switching.Raw$Age) #find correlation value (r) between dependent variable
  a.tes(t=t_value_effect_size$test$tstat['5 - 2'],n.1=group_number['1','n'],n.2=group_number['2','n'],R=r_value$estimate,q=1) #calculate Cohen's D with the covariate of age
  
  
####--------------ANCOVA Testing (w/ sex and age) --------------------------#### 
#run mixed design, 2 x 5 ANCOVA for HayBTime1.Raw
aov_HayBTime1Raw <- aov(HayBTime1.Raw ~ Group*Timepoint + Error(Individual_number/Timepoint) + Age + Sex, data = your_data_file)
summary(aov_HayBTime1Raw)
#aov_HayBTime1Raw <- Anova(lm(HayBTime1.Raw ~ Group*Timepoint + Age + Sex, data=your_data_file), type = "III") #use this anova test to account for unbalanced designs/sample sizes
#aov_HayBTime1Raw
#effect size w/ covariates
#eta_squared(aov_HayBTime1Raw) #doesn't work - doesn't like the class of the model
sjstats::eta_sq(aov_HayBTime1Raw)
#run mixed design, 2 x 5 ANCOVA for HayBTime2.Raw
aov_HayBTime2Raw <- aov(HayBTime2.Raw ~ Group*Timepoint + Error(Individual_number/Timepoint) + Age + Sex, data = your_data_file)
summary(aov_HayBTime2Raw)
#effect size w/ covariates
sjstats::eta_sq(aov_HayBTime2Raw)
#run mixed design, 2 x 5 ANCOVA for HayTime2.HayTime1_minus
aov_HayTime2.HayTime1_minus <- aov(HayTime2.HayTime1_minus ~ Group*Timepoint + Error(Individual_number/Timepoint) + Age + Sex, data = your_data_file)
summary(aov_HayTime2.HayTime1_minus)
#effect size w/ covariates
sjstats::eta_sq(aov_HayTime2.HayTime1_minus)
#run mixed design, 2 x 5 ANCOVA for HayBCatA.Raw
aov_HayBCatARaw <- aov(HayBCatA.Raw ~ Group*Timepoint + Error(Individual_number/Timepoint) + Age + Sex, data = your_data_file)
summary(aov_HayBCatARaw)
#effect size w/ covariates
sjstats::eta_sq(aov_HayBCatARaw)
#run mixed design, 2 x 5 ANCOVA for HayBCatB.Raw
aov_HayBCatBRaw <- aov(HayBCatB.Raw ~ Group*Timepoint + Error(Individual_number/Timepoint) + Age + Sex, data = your_data_file)
summary(aov_HayBCatBRaw)
#effect size w/ covariates
sjstats::eta_sq(aov_HayBCatBRaw)
#run mixed design, 2 x 5 ANCOVA for HayCatTotalError.Raw
aov_HayCatTotalError.Raw <- aov(HayCatTotalError.Raw ~ Group*Timepoint + Error(Individual_number/Timepoint) + Age + Sex, data = your_data_file)
summary(aov_HayCatTotalError.Raw)
#effect size w/ covariates
sjstats::eta_sq(aov_HayCatTotalError.Raw)
#run mixed design, 2 x 5 ANCOVA for ColorNaming.Raw
aov_ColorNamingRaw <- aov(ColorNaming.Raw ~ Group*Timepoint + Error(Individual_number/Timepoint) + Age + Sex, data = your_data_file)
summary(aov_ColorNamingRaw)
#effect size w/ covariates
sjstats::eta_sq(aov_ColorNamingRaw)
#run mixed design, 2 x 5 ANCOVA for WordReading.Raw
aov_WordReadingRaw <- aov(WordReading.Raw ~ Group*Timepoint + Error(Individual_number/Timepoint) + Age + Sex, data = your_data_file)
summary(aov_WordReadingRaw)
#effect size w/ covariates
sjstats::eta_sq(aov_WordReadingRaw)
#run mixed design, 2 x 5 ANCOVA for WordReading.Raw
aov_InhibitionRaw <- aov(Inhibition.Raw ~ Group*Timepoint + Error(Individual_number/Timepoint) + Age + Sex, data = your_data_file)
summary(aov_InhibitionRaw)
#effect size w/ covariates
sjstats::eta_sq(aov_InhibitionRaw)
#run mixed design, 2 x 5 ANCOVA for Inhibition.Colour.Naming
aov_InhibitionColourNaming <- aov(Inhibition.Colour.Naming~ Group*Timepoint + Error(Individual_number/Timepoint) + Age + Sex, data = your_data_file)
summary(aov_InhibitionColourNaming)
#effect size w/ covariates
sjstats::eta_sq(aov_InhibitionColourNaming)
#run mixed design, 2 x 5 ANCOVA for Inhibition.Word.Reading
aov_InhibitionWordReading <- aov(Inhibition.Word.Reading~ Group*Timepoint + Error(Individual_number/Timepoint) + Age + Sex, data = your_data_file)
summary(aov_InhibitionWordReading)
#effect size w/ covariates
sjstats::eta_sq(aov_InhibitionWordReading)
#run mixed design, 2 x 5 ANCOVA for Interference_score_Caffarra_2002
aov_Interference_score_Caffarra_2002 <- aov(Interference_score_Caffarra_2002~ Group*Timepoint + Error(Individual_number/Timepoint) + Age + Sex, data = your_data_file)
summary(aov_Interference_score_Caffarra_2002)
#effect size w/ covariates
sjstats::eta_sq(aov_Interference_score_Caffarra_2002)
#run mixed design, 2 x 5 ANCOVA for TrailsA.Raw
aov_TrailsARaw <- aov(TrailsA.Raw ~ Group*Timepoint + Error(Individual_number/Timepoint) + Age + Sex, data = your_data_file)
summary(aov_TrailsARaw)
#effect size w/ covariates
sjstats::eta_sq(aov_TrailsARaw)
#run mixed design, 2 x 5 ANCOVA for TrailsB.Raw
aov_TrailsBRaw <- aov(TrailsB.Raw ~ Group*Timepoint + Error(Individual_number/Timepoint) + Age + Sex, data = your_data_file)
summary(aov_TrailsBRaw)
#effect size w/ covariates
sjstats::eta_sq(aov_TrailsBRaw)
#run mixed design, 2 x 5 ANCOVA for TMT.B.TMT.A
aov_TMTBTMTA <- aov(TMT.B.TMT.A ~ Group*Timepoint + Error(Individual_number/Timepoint) + Age + Sex, data = your_data_file)
summary(aov_TMTBTMTA)
#effect size w/ covariates
sjstats::eta_sq(aov_TMTBTMTA)
#run mixed design, 2 x 5 ANCOVA for LetFluency.Raw
aov_LetFluencyRaw <- aov(LetFluency.Raw ~ Group*Timepoint + Error(Individual_number/Timepoint) + Age + Sex, data = your_data_file)
summary(aov_LetFluencyRaw)
#effect size w/ covariates
sjstats::eta_sq(aov_LetFluencyRaw)
#run mixed design, 2 x 5 ANCOVA for CatFluency.Raw
aov_CatFluencyRaw <- aov(CatFluency.Raw ~ Group*Timepoint + Error(Individual_number/Timepoint) + Age + Sex, data = your_data_file)
summary(aov_CatFluencyRaw)
#effect size w/ covariates
sjstats::eta_sq(aov_CatFluencyRaw)
#run mixed design, 2 x 5 ANCOVA for Switching.Raw
aov_SwitchingRaw <- aov(Switching.Raw ~ Group*Timepoint + Error(Individual_number/Timepoint) + Age + Sex, data = your_data_file)
summary(aov_SwitchingRaw)
#effect size w/ covariates
sjstats::eta_sq(aov_SwitchingRaw)

#f/u post hoc tests
#HayBTime1
#remove NAs from dataset for given variable
noNAs_HayBTime1Raw <- your_data_file[,c("ParticipantID","Individual_number","Group","Timepoint","Age","Sex","HayBTime1.Raw")]
noNAs_HayBTime1Raw <- noNAs_HayBTime1Raw[complete.cases(noNAs_HayBTime1Raw), ]
post_hoc_aov_HayBTime1_2covar_mod <- lme(HayBTime1.Raw ~ Group*Timepoint + Age + Sex, random = ~1 | Individual_number/Timepoint, data=noNAs_HayBTime1Raw)
summary(glht(post_hoc_aov_HayBTime1_2covar_mod, linfct=mcp(Group="Tukey"))) #no sig.
#HayBTime2
#remove NAs from dataset for given variable
noNAs_HayBTime2Raw <- your_data_file[,c("ParticipantID","Individual_number","Group","Timepoint","Age","Sex","HayBTime2.Raw")]
noNAs_HayBTime2Raw <- noNAs_HayBTime2Raw[complete.cases(noNAs_HayBTime2Raw), ]
post_hoc_aov_HayBTime2_2covar_mod <- lme(HayBTime2.Raw ~ Group*Timepoint + Age + Sex, random = ~1 | Individual_number/Timepoint, data=noNAs_HayBTime2Raw)
summary(glht(post_hoc_aov_HayBTime2_2covar_mod, linfct=mcp(Group="Tukey"))) #no sig
#interaction - HayBTime2
#Tukey test
TukeyHSD(aov(HayBTime2.Raw~Group*Timepoint+as.factor(Age)+Sex, data = your_data_file))   
#subtract differences in F2 - F0 for color naming with covariate age
HayBTime2_diff_data<-your_data_file[,c("ParticipantID","Individual_number","Group","Timepoint","Age","Sex","HayBTime2.Raw")]
F0_HayBTime2_data<-HayBTime2_diff_data[HayBTime2_diff_data$Timepoint=='F0', ]
F2_HayBTime2_data<-HayBTime2_diff_data[HayBTime2_diff_data$Timepoint=='F2', ]
HayBTime2Diff<-F2_HayBTime2_data$HayBTime2.Raw - F0_HayBTime2_data$HayBTime2.Raw
F0_HayBTime2_data$HayBTime2Diff<-HayBTime2Diff
#run one-way ANOVA on the F2-F0 differences in color naming between groups
HayBTime2Diff_2covar_mod <- lm(HayBTime2Diff ~ Group+Age+Sex, data = F0_HayBTime2_data)
anova(HayBTime2Diff_2covar_mod) #sig. difference
#effect size omnibus ANOVA
etaSquared(HayBTime2Diff_2covar_mod)
#post hoc f/u test
#run pairwise comparisons (post-hoc Tukey), given that the F-test was significant. 
post_hoc_HayBTime2Diff_2covar_mod <- glht(HayBTime2Diff_2covar_mod, linfct = mcp(Group = "Tukey"))
summary(post_hoc_HayBTime2Diff_2covar_mod)
confint(post_hoc_HayBTime2Diff_2covar_mod)
t_value_effect_size <- summary(glht(HayBTime2Diff_2covar_mod, linfct = mcp(Group = "Tukey")))
#effect size for sig. post hoc tests with covariate
  #for C vs. AD
  your_data_file_CvAD_HayBTime2Diff <- subset(your_data_file, your_data_file$Group == 1 | your_data_file$Group == 5)
  your_data_file_CvAD_HayBTime2Diff$Group <- droplevels(your_data_file_CvAD_HayBTime2Diff$Group)
  group_number <-dplyr::count(your_data_file_CvAD_HayBTime2Diff, Group) #count number of participants per group
  mult.r_value_2covar_mod<-summary(lm(HayBTime2.Raw ~ Age + Sex, data = your_data_file_CvAD_HayBTime2Diff)) #create multiple regression between age, sex, and y-var, and get square root of mult-r squared as the r-value
  r_value <- sqrt(mult.r_value_2covar_mod$r.squared) #find correlation value (r) between dependent variable
  a.tes(t=t_value_effect_size$test$tstat['5 - 1'],n.1=group_number['1','n'],n.2=group_number['2','n'],R=r_value,q=2) #calculate Cohen's D with the covariate of age & sex
  #for SCD vs. AD
  your_data_file_SCDvAD_HayBTime2Diff <- subset(your_data_file, your_data_file$Group == 2 | your_data_file$Group == 5)
  your_data_file_SCDvAD_HayBTime2Diff$Group <- droplevels(your_data_file_SCDvAD_HayBTime2Diff$Group)
  group_number <-dplyr::count(your_data_file_SCDvAD_HayBTime2Diff, Group) #count number of participants per group
  mult.r_value_2covar_mod<-summary(lm(HayBTime2.Raw ~ Age + Sex, data = your_data_file_SCDvAD_HayBTime2Diff)) #create multiple regression between age, sex, and y-var, and get square root of mult-r squared as the r-value
  r_value <- sqrt(mult.r_value_2covar_mod$r.squared) #find correlation value (r) between dependent variable
  a.tes(t=t_value_effect_size$test$tstat['5 - 2'],n.1=group_number['1','n'],n.2=group_number['2','n'],R=r_value,q=2) #calculate Cohen's D with the covariate of age & sex
#HayTime2.HayTime1_minus
#remove NAs from dataset for given variable
noNAs_HayTime2.HayTime1_minus <- your_data_file[,c("ParticipantID","Individual_number","Group","Timepoint","Age","Sex","HayTime2.HayTime1_minus")]
noNAs_HayTime2.HayTime1_minus <- noNAs_HayTime2.HayTime1_minus[complete.cases(noNAs_HayTime2.HayTime1_minus), ]
post_hoc_aov_HayTime2.HayTime1_minus_2covar_mod <- lme(HayTime2.HayTime1_minus~ Group*Timepoint + Age + Sex, random = ~1 | Individual_number/Timepoint, data=noNAs_HayTime2.HayTime1_minus)
summary(glht(post_hoc_aov_HayTime2.HayTime1_minus_2covar_mod, linfct=mcp(Group="Tukey"))) #no sig
#interaction - HayBTime2
#Tukey test
TukeyHSD(aov(HayTime2.HayTime1_minus~Group*Timepoint+as.factor(Age)+Sex, data = your_data_file))   
#subtract differences in F2 - F0 for color naming with covariate age
HayTime2.HayTime1_minus_diff_data<-your_data_file[,c("ParticipantID","Individual_number","Group","Timepoint","Age","Sex","HayTime2.HayTime1_minus")]
F0_HayTime2.HayTime1_minus_data<-HayTime2.HayTime1_minus_diff_data[HayTime2.HayTime1_minus_diff_data$Timepoint=='F0', ]
F2_HayTime2.HayTime1_minus_data<-HayTime2.HayTime1_minus_diff_data[HayTime2.HayTime1_minus_diff_data$Timepoint=='F2', ]
HayTime2.HayTime1_minusDiff<-F2_HayTime2.HayTime1_minus_data$HayTime2.HayTime1_minus - F0_HayTime2.HayTime1_minus_data$HayTime2.HayTime1_minus
F0_HayTime2.HayTime1_minus_data$HayTime2.HayTime1_minusDiff<-HayTime2.HayTime1_minusDiff
#run one-way ANOVA on the F2-F0 differences in color naming between groups
HayTime2.HayTime1_minusDiff_2covar_mod <- lm(HayTime2.HayTime1_minusDiff ~ Group+Age+Sex, data = F0_HayTime2.HayTime1_minus_data)
anova(HayTime2.HayTime1_minusDiff_2covar_mod) #sig. difference
#effect size omnibus ANOVA
etaSquared(HayTime2.HayTime1_minusDiff_2covar_mod)
#post hoc f/u test
#run pairwise comparisons (post-hoc Tukey), given that the F-test was significant. 
post_hoc_HayTime2.HayTime1_minusDiff_2covar_mod <- glht(HayTime2.HayTime1_minusDiff_2covar_mod, linfct = mcp(Group = "Tukey"))
summary(post_hoc_HayTime2.HayTime1_minusDiff_2covar_mod)
confint(post_hoc_HayTime2.HayTime1_minusDiff_2covar_mod)
t_value_effect_size <- summary(glht(HayTime2.HayTime1_minusDiff_2covar_mod, linfct = mcp(Group = "Tukey")))
#effect size for sig. post hoc tests with covariate
  #for C vs. AD
  your_data_file_CvAD_HayTime2.HayTime1_minusDiff <- subset(your_data_file, your_data_file$Group == 1 | your_data_file$Group == 5)
  your_data_file_CvAD_HayTime2.HayTime1_minusDiff$Group <- droplevels(your_data_file_CvAD_HayTime2.HayTime1_minusDiff$Group)
  group_number <-dplyr::count(your_data_file_CvAD_HayTime2.HayTime1_minusDiff, Group) #count number of participants per group
  mult.r_value_2covar_mod<-summary(lm(HayTime2.HayTime1_minus ~ Age + Sex, data = your_data_file_CvAD_HayTime2.HayTime1_minusDiff)) #create multiple regression between age, sex, and y-var, and get square root of mult-r squared as the r-value
  r_value <- sqrt(mult.r_value_2covar_mod$r.squared) #find correlation value (r) between dependent variable
  a.tes(t=t_value_effect_size$test$tstat['5 - 1'],n.1=group_number['1','n'],n.2=group_number['2','n'],R=r_value,q=2) #calculate Cohen's D with the covariate of age & sex
  #for SCD vs. AD
  your_data_file_SCDvAD_HayTime2.HayTime1_minusDiff <- subset(your_data_file, your_data_file$Group == 2 | your_data_file$Group == 5)
  your_data_file_SCDvAD_HayTime2.HayTime1_minusDiff$Group <- droplevels(your_data_file_SCDvAD_HayTime2.HayTime1_minusDiff$Group)
  group_number <-dplyr::count(your_data_file_SCDvAD_HayTime2.HayTime1_minusDiff, Group) #count number of participants per group
  mult.r_value_2covar_mod<-summary(lm(HayTime2.HayTime1_minus ~ Age + Sex, data = your_data_file_SCDvAD_HayTime2.HayTime1_minusDiff)) #create multiple regression between age, sex, and y-var, and get square root of mult-r squared as the r-value
  r_value <- sqrt(mult.r_value_2covar_mod$r.squared) #find correlation value (r) between dependent variable
  a.tes(t=t_value_effect_size$test$tstat['5 - 2'],n.1=group_number['1','n'],n.2=group_number['2','n'],R=r_value,q=2) #calculate Cohen's D with the covariate of age & sex
#HayBCatA
#remove NAs from dataset for given variable
noNAs_HayBCatARaw <- your_data_file[,c("ParticipantID","Individual_number","Group","Timepoint","Age","Sex","HayBCatA.Raw")]
noNAs_HayBCatARaw <- noNAs_HayBCatARaw[complete.cases(noNAs_HayBCatARaw), ]
post_hoc_aov_HayBCatA_2covar_mod <- lme(HayBCatA.Raw ~ Group*Timepoint + Age + Sex, random = ~1 | Individual_number/Timepoint, data=noNAs_HayBCatARaw)
summary(glht(post_hoc_aov_HayBCatA_2covar_mod, linfct=mcp(Group="Tukey")))
t_value_effect_size <- summary(glht(post_hoc_aov_HayBCatA_2covar_mod, linfct=mcp(Group="Tukey")))
#effect size for sig. post hoc tests
#calculate effect sizes (Cohen's D) with covariate (using a.tes function from the compute.es package)
  #for C vs. AD
  your_data_file_CvAD_HayBCatA.Raw <- subset(your_data_file, your_data_file$Group == 1 | your_data_file$Group == 5)
  your_data_file_CvAD_HayBCatA.Raw$Group <- droplevels(your_data_file_CvAD_HayBCatA.Raw$Group)
  group_number <-dplyr::count(your_data_file_CvAD_HayBCatA.Raw, Group) #count number of participants per group
  mult.r_value_2covar_mod<-summary(lm(HayBCatA.Raw ~ Age + Sex, data = your_data_file_CvAD_HayBCatA.Raw)) #create multiple regression between age, sex, and y-var, and get square root of mult-r squared as the r-value
  r_value <- sqrt(mult.r_value_2covar_mod$r.squared) #find correlation value (r) between dependent variable
  a.tes(t=t_value_effect_size$test$tstat['5 - 1'],n.1=group_number['1','n'],n.2=group_number['2','n'],R=r_value,q=2) #calculate Cohen's D with the covariate of age & sex
  #for SCD vs. AD
  your_data_file_SCDvAD_HayBCatA.Raw <- subset(your_data_file, your_data_file$Group == 2 | your_data_file$Group == 5)
  your_data_file_SCDvAD_HayBCatA.Raw$Group <- droplevels(your_data_file_SCDvAD_HayBCatA.Raw$Group)
  group_number <-dplyr::count(your_data_file_SCDvAD_HayBCatA.Raw, Group) #count number of participants per group
  mult.r_value_2covar_mod<-summary(lm(HayBCatA.Raw ~ Age + Sex, data = your_data_file_SCDvAD_HayBCatA.Raw)) #create multiple regression between age, sex, and y-var, and get square root of mult-r squared as the r-value
  r_value <- sqrt(mult.r_value_2covar_mod$r.squared) #find correlation value (r) between dependent variable
  a.tes(t=t_value_effect_size$test$tstat['5 - 2'],n.1=group_number['1','n'],n.2=group_number['2','n'],R=r_value,q=2) #calculate Cohen's D with the covariate of age & sex
  #for aMCI vs. AD
  your_data_file_aMCIvAD_HayBCatA.Raw <- subset(your_data_file, your_data_file$Group == 3 | your_data_file$Group == 5)
  your_data_file_aMCIvAD_HayBCatA.Raw$Group <- droplevels(your_data_file_aMCIvAD_HayBCatA.Raw$Group)
  group_number <-dplyr::count(your_data_file_aMCIvAD_HayBCatA.Raw, Group) #count number of participants per group
  mult.r_value_2covar_mod<-summary(lm(HayBCatA.Raw ~ Age + Sex, data = your_data_file_aMCIvAD_HayBCatA.Raw)) #create multiple regression between age, sex, and y-var, and get square root of mult-r squared as the r-value
  r_value <- sqrt(mult.r_value_2covar_mod$r.squared) #find correlation value (r) between dependent variable
  a.tes(t=t_value_effect_size$test$tstat['5 - 3'],n.1=group_number['1','n'],n.2=group_number['2','n'],R=r_value,q=2) #calculate Cohen's D with the covariate of age & sex
  #for mMCI vs. AD
  your_data_file_mMCIvAD_HayBCatA.Raw <- subset(your_data_file, your_data_file$Group == 4 | your_data_file$Group == 5)
  your_data_file_mMCIvAD_HayBCatA.Raw$Group <- droplevels(your_data_file_mMCIvAD_HayBCatA.Raw$Group)
  group_number <-dplyr::count(your_data_file_mMCIvAD_HayBCatA.Raw, Group) #count number of participants per group
  mult.r_value_2covar_mod<-summary(lm(HayBCatA.Raw ~ Age + Sex, data = your_data_file_mMCIvAD_HayBCatA.Raw)) #create multiple regression between age, sex, and y-var, and get square root of mult-r squared as the r-value
  r_value <- sqrt(mult.r_value_2covar_mod$r.squared) #find correlation value (r) between dependent variable
  a.tes(t=t_value_effect_size$test$tstat['5 - 4'],n.1=group_number['1','n'],n.2=group_number['2','n'],R=r_value,q=2) #calculate Cohen's D with the covariate of age & sex
#HayBCatB
#remove NAs from dataset for given variable
noNAs_HayBCatBRaw <- your_data_file[,c("ParticipantID","Individual_number","Group","Timepoint","Age","Sex","HayBCatB.Raw")]
noNAs_HayBCatBRaw <- noNAs_HayBCatBRaw[complete.cases(noNAs_HayBCatBRaw), ]
post_hoc_aov_HayBCatB_2covar_mod <- lme(HayBCatB.Raw ~ Group*Timepoint + Age + Sex, random = ~1 | Individual_number/Timepoint, data=noNAs_HayBCatBRaw)
summary(glht(post_hoc_aov_HayBCatB_2covar_mod, linfct=mcp(Group="Tukey")))
t_value_effect_size <- summary(glht(post_hoc_aov_HayBCatB_2covar_mod, linfct=mcp(Group="Tukey")))
#calculate effect sizes (Cohen's D) with covariate (using a.tes function from the compute.es package)
  #for C vs. AD
  your_data_file_CvAD_HayBCatB.Raw <- subset(your_data_file, your_data_file$Group == 1 | your_data_file$Group == 5)
  your_data_file_CvAD_HayBCatB.Raw$Group <- droplevels(your_data_file_CvAD_HayBCatB.Raw$Group)
  group_number <-dplyr::count(your_data_file_CvAD_HayBCatB.Raw, Group) #count number of participants per group
  mult.r_value_2covar_mod<-summary(lm(HayBCatB.Raw ~ Age + Sex, data = your_data_file_CvAD_HayBCatB.Raw)) #create multiple regression between age, sex, and y-var, and get square root of mult-r squared as the r-value
  r_value <- sqrt(mult.r_value_2covar_mod$r.squared) #find correlation value (r) between dependent variable
  a.tes(t=t_value_effect_size$test$tstat['5 - 1'],n.1=group_number['1','n'],n.2=group_number['2','n'],R=r_value,q=2) #calculate Cohen's D with the covariate of age & sex
  #for SCD vs. AD
  your_data_file_SCDvAD_HayBCatB.Raw <- subset(your_data_file, your_data_file$Group == 2 | your_data_file$Group == 5)
  your_data_file_SCDvAD_HayBCatB.Raw$Group <- droplevels(your_data_file_SCDvAD_HayBCatB.Raw$Group)
  group_number <-dplyr::count(your_data_file_SCDvAD_HayBCatB.Raw, Group) #count number of participants per group
  mult.r_value_2covar_mod<-summary(lm(HayBCatB.Raw ~ Age + Sex, data = your_data_file_SCDvAD_HayBCatB.Raw)) #create multiple regression between age, sex, and y-var, and get square root of mult-r squared as the r-value
  r_value <- sqrt(mult.r_value_2covar_mod$r.squared) #find correlation value (r) between dependent variable
  a.tes(t=t_value_effect_size$test$tstat['5 - 2'],n.1=group_number['1','n'],n.2=group_number['2','n'],R=r_value,q=2) #calculate Cohen's D with the covariate of age & sex
  #for aMCI vs. AD
  your_data_file_aMCIvAD_HayBCatB.Raw <- subset(your_data_file, your_data_file$Group == 3 | your_data_file$Group == 5)
  your_data_file_aMCIvAD_HayBCatB.Raw$Group <- droplevels(your_data_file_aMCIvAD_HayBCatB.Raw$Group)
  group_number <-dplyr::count(your_data_file_aMCIvAD_HayBCatB.Raw, Group) #count number of participants per group
  mult.r_value_2covar_mod<-summary(lm(HayBCatB.Raw ~ Age + Sex, data = your_data_file_aMCIvAD_HayBCatB.Raw)) #create multiple regression between age, sex, and y-var, and get square root of mult-r squared as the r-value
  r_value <- sqrt(mult.r_value_2covar_mod$r.squared) #find correlation value (r) between dependent variable
  a.tes(t=t_value_effect_size$test$tstat['5 - 3'],n.1=group_number['1','n'],n.2=group_number['2','n'],R=r_value,q=2) #calculate Cohen's D with the covariate of age & sex
  #for mMCI vs. AD
  your_data_file_mMCIvAD_HayBCatB.Raw <- subset(your_data_file, your_data_file$Group == 4 | your_data_file$Group == 5)
  your_data_file_mMCIvAD_HayBCatB.Raw$Group <- droplevels(your_data_file_mMCIvAD_HayBCatB.Raw$Group)
  group_number <-dplyr::count(your_data_file_mMCIvAD_HayBCatB.Raw, Group) #count number of participants per group
  mult.r_value_2covar_mod<-summary(lm(HayBCatB.Raw ~ Age + Sex, data = your_data_file_mMCIvAD_HayBCatB.Raw)) #create multiple regression between age, sex, and y-var, and get square root of mult-r squared as the r-value
  r_value <- sqrt(mult.r_value_2covar_mod$r.squared) #find correlation value (r) between dependent variable
  a.tes(t=t_value_effect_size$test$tstat['5 - 4'],n.1=group_number['1','n'],n.2=group_number['2','n'],R=r_value,q=2) #calculate Cohen's D with the covariate of age & sex
#HayCatTotalError.Raw
#remove NAs from dataset for given variable
noNAs_HayCatTotalError.Raw <- your_data_file[,c("ParticipantID","Individual_number","Group","Timepoint","Age","Sex","HayCatTotalError.Raw")]
noNAs_HayCatTotalError.Raw <- noNAs_HayCatTotalError.Raw[complete.cases(noNAs_HayCatTotalError.Raw), ]
post_hoc_aov_HayCatTotalError.Raw_2covar_mod <- lme(HayCatTotalError.Raw ~ Group*Timepoint + Age + Sex, random = ~1 | Individual_number/Timepoint, data=noNAs_HayCatTotalError.Raw)
summary(glht(post_hoc_aov_HayCatTotalError.Raw_2covar_mod, linfct=mcp(Group="Tukey")))
t_value_effect_size <- summary(glht(post_hoc_aov_HayCatTotalError.Raw_2covar_mod, linfct=mcp(Group="Tukey")))
#calculate effect sizes (Cohen's D) with covariate (using a.tes function from the compute.es package)
  #for C vs. AD
  your_data_file_CvAD_HayCatTotalError.Raw <- subset(your_data_file, your_data_file$Group == 1 | your_data_file$Group == 5)
  your_data_file_CvAD_HayCatTotalError.Raw$Group <- droplevels(your_data_file_CvAD_HayCatTotalError.Raw$Group)
  group_number <-dplyr::count(your_data_file_CvAD_HayCatTotalError.Raw, Group) #count number of participants per group
  mult.r_value_2covar_mod<-summary(lm(HayCatTotalError.Raw ~ Age + Sex, data = your_data_file_CvAD_HayCatTotalError.Raw)) #create multiple regression between age, sex, and y-var, and get square root of mult-r squared as the r-value
  r_value <- sqrt(mult.r_value_2covar_mod$r.squared) #find correlation value (r) between dependent variable
  a.tes(t=t_value_effect_size$test$tstat['5 - 1'],n.1=group_number['1','n'],n.2=group_number['2','n'],R=r_value,q=2) #calculate Cohen's D with the covariate of age & sex
  #for SCD vs. AD
  your_data_file_SCDvAD_HayCatTotalError.Raw <- subset(your_data_file, your_data_file$Group == 2 | your_data_file$Group == 5)
  your_data_file_SCDvAD_HayCatTotalError.Raw$Group <- droplevels(your_data_file_SCDvAD_HayCatTotalError.Raw$Group)
  group_number <-dplyr::count(your_data_file_SCDvAD_HayCatTotalError.Raw, Group) #count number of participants per group
  mult.r_value_2covar_mod<-summary(lm(HayCatTotalError.Raw ~ Age + Sex, data = your_data_file_SCDvAD_HayCatTotalError.Raw)) #create multiple regression between age, sex, and y-var, and get square root of mult-r squared as the r-value
  r_value <- sqrt(mult.r_value_2covar_mod$r.squared) #find correlation value (r) between dependent variable
  a.tes(t=t_value_effect_size$test$tstat['5 - 2'],n.1=group_number['1','n'],n.2=group_number['2','n'],R=r_value,q=2) #calculate Cohen's D with the covariate of age & sex
  #for aMCI vs. AD
  your_data_file_aMCIvAD_HayCatTotalError.Raw <- subset(your_data_file, your_data_file$Group == 3 | your_data_file$Group == 5)
  your_data_file_aMCIvAD_HayCatTotalError.Raw$Group <- droplevels(your_data_file_aMCIvAD_HayCatTotalError.Raw$Group)
  group_number <-dplyr::count(your_data_file_aMCIvAD_HayCatTotalError.Raw, Group) #count number of participants per group
  mult.r_value_2covar_mod<-summary(lm(HayCatTotalError.Raw ~ Age + Sex, data = your_data_file_aMCIvAD_HayCatTotalError.Raw)) #create multiple regression between age, sex, and y-var, and get square root of mult-r squared as the r-value
  r_value <- sqrt(mult.r_value_2covar_mod$r.squared) #find correlation value (r) between dependent variable
  a.tes(t=t_value_effect_size$test$tstat['5 - 3'],n.1=group_number['1','n'],n.2=group_number['2','n'],R=r_value,q=2) #calculate Cohen's D with the covariate of age & sex
  #for mMCI vs. AD
  your_data_file_mMCIvAD_HayCatTotalError.Raw <- subset(your_data_file, your_data_file$Group == 4 | your_data_file$Group == 5)
  your_data_file_mMCIvAD_HayCatTotalError.Raw$Group <- droplevels(your_data_file_mMCIvAD_HayCatTotalError.Raw$Group)
  group_number <-dplyr::count(your_data_file_mMCIvAD_HayCatTotalError.Raw, Group) #count number of participants per group
  mult.r_value_2covar_mod<-summary(lm(HayCatTotalError.Raw ~ Age + Sex, data = your_data_file_mMCIvAD_HayCatTotalError.Raw)) #create multiple regression between age, sex, and y-var, and get square root of mult-r squared as the r-value
  r_value <- sqrt(mult.r_value_2covar_mod$r.squared) #find correlation value (r) between dependent variable
  a.tes(t=t_value_effect_size$test$tstat['5 - 4'],n.1=group_number['1','n'],n.2=group_number['2','n'],R=r_value,q=2) #calculate Cohen's D with the covariate of age & sex

#ColorNaming
#remove NAs from dataset for given variable
noNAs_ColorNamingRaw <- your_data_file[,c("ParticipantID","Individual_number","Group","Timepoint","Age","Sex","ColorNaming.Raw")]
noNAs_ColorNamingRaw <- noNAs_ColorNamingRaw[complete.cases(noNAs_ColorNamingRaw), ]
post_hoc_aov_ColorNaming_2covar_mod <- lme(ColorNaming.Raw ~ Group*Timepoint + Age + Sex, random = ~1 | Individual_number/Timepoint, data=noNAs_ColorNamingRaw)
summary(glht(post_hoc_aov_ColorNaming_2covar_mod, linfct=mcp(Group="Tukey"))) #no sig
#interaction - ColorNaming
#Tukey test
TukeyHSD(aov(ColorNaming.Raw~Group*Timepoint+as.factor(Age)+Sex, data = your_data_file))   
#subtract differences in F2 - F0 for color naming with covariate age
ColorNaming_diff_data<-your_data_file[,c("ParticipantID","Individual_number","Group","Timepoint","Age","Sex","ColorNaming.Raw")]
F0_ColorNaming_data<-ColorNaming_diff_data[ColorNaming_diff_data$Timepoint=='F0', ]
F2_ColorNaming_data<-ColorNaming_diff_data[ColorNaming_diff_data$Timepoint=='F2', ]
ColorNamingDiff<-F2_ColorNaming_data$ColorNaming.Raw - F0_ColorNaming_data$ColorNaming.Raw
F0_ColorNaming_data$ColorNamingDiff<-ColorNamingDiff
#run one-way ANOVA on the F2-F0 differences in color naming between groups
ColorNamingDiff_2covar_mod <- lm(ColorNamingDiff ~ Group+Age+Sex, data = F0_ColorNaming_data)
anova(ColorNamingDiff_2covar_mod) #sig. difference
#effect size omnibus ANOVA
etaSquared(ColorNamingDiff_2covar_mod)
#post hoc f/u test
#run pairwise comparisons (post-hoc Tukey), given that the F-test was significant. 
post_hoc_ColorNamingDiff_2covar_mod <- glht(ColorNamingDiff_2covar_mod, linfct = mcp(Group = "Tukey"))
summary(post_hoc_ColorNamingDiff_2covar_mod)
confint(post_hoc_ColorNamingDiff_2covar_mod)
t_value_effect_size <- summary(glht(ColorNamingDiff_2covar_mod, linfct = mcp(Group = "Tukey")))
#effect size for sig. post hoc tests with covariate
  #for SCD vs. aMCI
  your_data_file_SCDvaMCI_ColorNamingDiff <- subset(your_data_file, your_data_file$Group == 2 | your_data_file$Group == 3)
  your_data_file_SCDvaMCI_ColorNamingDiff$Group <- droplevels(your_data_file_SCDvaMCI_ColorNamingDiff$Group)
  group_number <-dplyr::count(your_data_file_SCDvaMCI_ColorNamingDiff, Group) #count number of participants per group
  mult.r_value_2covar_mod<-summary(lm(ColorNaming.Raw ~ Age + Sex, data = your_data_file_SCDvaMCI_ColorNamingDiff)) #create multiple regression between age, sex, and y-var, and get square root of mult-r squared as the r-value
  r_value <- sqrt(mult.r_value_2covar_mod$r.squared) #find correlation value (r) between dependent variable
  a.tes(t=t_value_effect_size$test$tstat['3 - 2'],n.1=group_number['1','n'],n.2=group_number['2','n'],R=r_value,q=2) #calculate Cohen's D with the covariate of age & sex
#WordReading
#remove NAs from dataset for given variable
noNAs_WordReadingRaw <- your_data_file[,c("ParticipantID","Individual_number","Group","Timepoint","Age","Sex","WordReading.Raw")]
noNAs_WordReadingRaw <- noNAs_WordReadingRaw[complete.cases(noNAs_WordReadingRaw), ]
post_hoc_aov_WordReading_2covar_mod <- lme(WordReading.Raw ~ Group*Timepoint + Age + Sex, random = ~1 | Individual_number/Timepoint, data=noNAs_WordReadingRaw)
summary(glht(post_hoc_aov_WordReading_2covar_mod, linfct=mcp(Group="Tukey"))) #no sig
summary(glht(post_hoc_aov_WordReading_2covar_mod, linfct=mcp(Timepoint="Tukey")))
#Inhibition
#remove NAs from dataset for given variable
noNAs_InhibitionRaw <- your_data_file[,c("ParticipantID","Individual_number","Group","Timepoint","Age","Sex","Inhibition.Raw")]
noNAs_InhibitionRaw <- noNAs_InhibitionRaw[complete.cases(noNAs_InhibitionRaw), ]
post_hoc_aov_Inhibition_2covar_mod <- lme(Inhibition.Raw ~ Group*Timepoint + Age + Sex, random = ~1 | Individual_number/Timepoint, data=noNAs_InhibitionRaw)
summary(glht(post_hoc_aov_Inhibition_2covar_mod, linfct=mcp(Group="Tukey")))
#Inhibition.Colour.Naming
#remove NAs from dataset for given variable
noNAs_Inhibition.Colour.Naming <- your_data_file[,c("ParticipantID","Individual_number","Group","Timepoint","Age","Sex","Inhibition.Colour.Naming")]
noNAs_Inhibition.Colour.Naming <- noNAs_Inhibition.Colour.Naming[complete.cases(noNAs_Inhibition.Colour.Naming), ]
post_hoc_aov_Inhibition.Colour.Naming_2covar_mod <- lme(Inhibition.Colour.Naming ~ Group*Timepoint + Age + Sex, random = ~1 | Individual_number/Timepoint, data=noNAs_Inhibition.Colour.Naming)
summary(glht(post_hoc_aov_Inhibition.Colour.Naming_2covar_mod, linfct=mcp(Group="Tukey")))
#Inhibition.Word.Reading
#remove NAs from dataset for given variable
noNAs_InhibitionWordReading <- your_data_file[,c("ParticipantID","Individual_number","Group","Timepoint","Age","Sex","Inhibition.Word.Reading")]
noNAs_InhibitionWordReading <- noNAs_InhibitionWordReading[complete.cases(noNAs_InhibitionWordReading), ]
post_hoc_aov_InhibitionWordReading_2covar_mod <- lme(Inhibition.Word.Reading ~ Group*Timepoint + Age + Sex, random = ~1 | Individual_number/Timepoint, data=noNAs_InhibitionWordReading)
summary(glht(post_hoc_aov_InhibitionWordReading_2covar_mod, linfct=mcp(Group="Tukey")))
#Interference Effect
#remove NAs from dataset for given variable
noNAs_Interference_score_Caffarra_2002 <- your_data_file[,c("ParticipantID","Individual_number","Group","Timepoint","Age","Sex","Interference_score_Caffarra_2002")]
noNAs_Interference_score_Caffarra_2002 <- noNAs_Interference_score_Caffarra_2002[complete.cases(noNAs_Interference_score_Caffarra_2002), ]
post_hoc_aov_Interference_score_Caffarra_2002_2covar_mod <- lme(Interference_score_Caffarra_2002 ~ Group*Timepoint + Age + Sex, random = ~1 | Individual_number/Timepoint, data=noNAs_Interference_score_Caffarra_2002)
summary(glht(post_hoc_aov_Interference_score_Caffarra_2002_2covar_mod, linfct=mcp(Group="Tukey")))

#TrailsA
#remove NAs from dataset for given variable
noNAs_TrailsARaw <- your_data_file[,c("ParticipantID","Individual_number","Group","Timepoint","Age","Sex","TrailsA.Raw")]
noNAs_TrailsARaw <- noNAs_TrailsARaw[complete.cases(noNAs_TrailsARaw), ]
post_hoc_aov_TrailsA_2covar_mod <- lme(TrailsA.Raw ~ Group*Timepoint + Age + Sex, random = ~1 | Individual_number/Timepoint, data=noNAs_TrailsARaw)
summary(glht(post_hoc_aov_TrailsA_2covar_mod, linfct=mcp(Group="Tukey")))
#TrailsB
#remove NAs from dataset for given variable
noNAs_TrailsBRaw <- your_data_file[,c("ParticipantID","Individual_number","Group","Timepoint","Age","Sex","TrailsB.Raw")]
noNAs_TrailsBRaw <- noNAs_TrailsBRaw[complete.cases(noNAs_TrailsBRaw), ]
post_hoc_aov_TrailsB_2covar_mod <- lme(TrailsB.Raw ~ Group*Timepoint + Age + Sex, random = ~1 | Individual_number/Timepoint, data=noNAs_TrailsBRaw)
summary(glht(post_hoc_aov_TrailsB_2covar_mod, linfct=mcp(Group="Tukey"))) #no sig
#interaction - TrailsB
#Tukey test
TukeyHSD(aov(TrailsB.Raw~Group*Timepoint+as.factor(Age)+Sex, data = your_data_file))   
#subtract differences in F2 - F0 for color naming with covariate age
TrailsB_diff_data<-your_data_file[,c("ParticipantID","Individual_number","Group","Timepoint","Age","Sex","TrailsB.Raw")]
F0_TrailsB_data<-TrailsB_diff_data[TrailsB_diff_data$Timepoint=='F0', ]
F2_TrailsB_data<-TrailsB_diff_data[TrailsB_diff_data$Timepoint=='F2', ]
TrailsBDiff<-F2_TrailsB_data$TrailsB.Raw - F0_TrailsB_data$TrailsB.Raw
F0_TrailsB_data$TrailsBDiff<-TrailsBDiff
#run one-way ANOVA on the F2-F0 differences in color naming between groups
TrailsBDiff_2covar_mod <- lm(TrailsBDiff ~ Group+Age+Sex, data = F0_TrailsB_data)
anova(TrailsBDiff_2covar_mod) #sig. difference
#effect size omnibus ANOVA
etaSquared(TrailsBDiff_2covar_mod)
#post hoc f/u test
#run pairwise comparisons (post-hoc Tukey), given that the F-test was significant. 
post_hoc_TrailsBDiff_2covar_mod <- glht(TrailsBDiff_2covar_mod, linfct = mcp(Group = "Tukey"))
summary(post_hoc_TrailsBDiff_2covar_mod)
confint(post_hoc_TrailsBDiff_2covar_mod)
t_value_effect_size <- summary(glht(TrailsBDiff_2covar_mod, linfct = mcp(Group = "Tukey")))
#effect size for sig. post hoc tests with covariate
  #for SCD vs. mMCI
  your_data_file_SCDvmMCI_TrailsBDiff <- subset(your_data_file, your_data_file$Group == 2 | your_data_file$Group == 4)
  your_data_file_SCDvmMCI_TrailsBDiff$Group <- droplevels(your_data_file_SCDvmMCI_TrailsBDiff$Group)
  group_number <-dplyr::count(your_data_file_SCDvmMCI_TrailsBDiff, Group) #count number of participants per group
  mult.r_value_2covar_mod<-summary(lm(TrailsB.Raw ~ Age + Sex, data = your_data_file_SCDvmMCI_TrailsBDiff)) #create multiple regression between age, sex, and y-var, and get square root of mult-r squared as the r-value
  r_value <- sqrt(mult.r_value_2covar_mod$r.squared) #find correlation value (r) between dependent variable
  a.tes(t=t_value_effect_size$test$tstat['4 - 2'],n.1=group_number['1','n'],n.2=group_number['2','n'],R=r_value,q=2) #calculate Cohen's D with the covariate of age & sex
  
#LetFluency
#remove NAs from dataset for given variable
noNAs_LetFluencyRaw <- your_data_file[,c("ParticipantID","Individual_number","Group","Timepoint","Age","Sex","LetFluency.Raw")]
noNAs_LetFluencyRaw <- noNAs_LetFluencyRaw[complete.cases(noNAs_LetFluencyRaw), ]
post_hoc_aov_LetFluency_2covar_mod <- lme(LetFluency.Raw ~ Group*Timepoint + Age + Sex, random = ~1 | Individual_number/Timepoint, data=noNAs_LetFluencyRaw)
summary(glht(post_hoc_aov_LetFluency_2covar_mod, linfct=mcp(Group="Tukey")))
t_value_effect_size <- summary(glht(post_hoc_aov_LetFluency_2covar_mod, linfct=mcp(Group="Tukey")))
#calculate effect sizes (Cohen's D) with covariate (using a.tes function from the compute.es package)
  #for C vs. aMCI
  your_data_file_CvaMCI_LetFluency.Raw <- subset(your_data_file, your_data_file$Group == 1 | your_data_file$Group == 3)
  your_data_file_CvaMCI_LetFluency.Raw$Group <- droplevels(your_data_file_CvaMCI_LetFluency.Raw$Group)
  group_number <-dplyr::count(your_data_file_CvaMCI_LetFluency.Raw, Group) #count number of participants per group
  mult.r_value_2covar_mod<-summary(lm(LetFluency.Raw ~ Age + Sex, data = your_data_file_CvaMCI_LetFluency.Raw)) #create multiple regression between age, sex, and y-var, and get square root of mult-r squared as the r-value
  r_value <- sqrt(mult.r_value_2covar_mod$r.squared) #find correlation value (r) between dependent variable
  a.tes(t=t_value_effect_size$test$tstat['3 - 1'],n.1=group_number['1','n'],n.2=group_number['2','n'],R=r_value,q=2) #calculate Cohen's D with the covariate of age & sex
#interaction - LetFluency
#Tukey test
TukeyHSD(aov(LetFluency.Raw~Group*Timepoint+as.factor(Age)+Sex, data = your_data_file))   
#subtract differences in F2 - F0 for color naming with covariate age
LetFluency_diff_data<-your_data_file[,c("ParticipantID","Individual_number","Group","Timepoint","Age","Sex","LetFluency.Raw")]
F0_LetFluency_data<-LetFluency_diff_data[LetFluency_diff_data$Timepoint=='F0', ]
F2_LetFluency_data<-LetFluency_diff_data[LetFluency_diff_data$Timepoint=='F2', ]
LetFluencyDiff<-F2_LetFluency_data$LetFluency.Raw - F0_LetFluency_data$LetFluency.Raw
F0_LetFluency_data$LetFluencyDiff<-LetFluencyDiff
#run one-way ANOVA on the F2-F0 differences in color naming between groups
LetFluencyDiff_2covar_mod <- lm(LetFluencyDiff ~ Group+Age+Sex, data = F0_LetFluency_data)
anova(LetFluencyDiff_2covar_mod) #sig. difference
#effect size omnibus ANOVA
etaSquared(LetFluencyDiff_2covar_mod)
#post hoc f/u test
#run pairwise comparisons (post-hoc Tukey), given that the F-test was significant. 
post_hoc_LetFluencyDiff_2covar_mod <- glht(LetFluencyDiff_2covar_mod, linfct = mcp(Group = "Tukey"))
summary(post_hoc_LetFluencyDiff_2covar_mod)
confint(post_hoc_LetFluencyDiff_2covar_mod)
t_value_effect_size <- summary(glht(LetFluencyDiff_2covar_mod, linfct = mcp(Group = "Tukey")))
#effect size for sig. post hoc tests with covariate
  #for C vs. AD
  your_data_file_CvAD_LetFluencyDiff <- subset(your_data_file, your_data_file$Group == 1 | your_data_file$Group == 5)
  your_data_file_CvAD_LetFluencyDiff$Group <- droplevels(your_data_file_CvAD_LetFluencyDiff$Group)
  group_number <-dplyr::count(your_data_file_CvAD_LetFluencyDiff, Group) #count number of participants per group
  mult.r_value_2covar_mod<-summary(lm(LetFluency.Raw ~ Age + Sex, data = your_data_file_CvAD_LetFluencyDiff)) #create multiple regression between age, sex, and y-var, and get square root of mult-r squared as the r-value
  r_value <- sqrt(mult.r_value_2covar_mod$r.squared) #find correlation value (r) between dependent variable
  a.tes(t=t_value_effect_size$test$tstat['5 - 1'],n.1=group_number['1','n'],n.2=group_number['2','n'],R=r_value,q=2) #calculate Cohen's D with the covariate of age & sex
  #for SCD vs. AD
  your_data_file_SCDvAD_LetFluencyDiff <- subset(your_data_file, your_data_file$Group == 2 | your_data_file$Group == 5)
  your_data_file_SCDvAD_LetFluencyDiff$Group <- droplevels(your_data_file_SCDvAD_LetFluencyDiff$Group)
  group_number <-dplyr::count(your_data_file_SCDvAD_LetFluencyDiff, Group) #count number of participants per group
  mult.r_value_2covar_mod<-summary(lm(LetFluency.Raw ~ Age + Sex, data = your_data_file_SCDvAD_LetFluencyDiff)) #create multiple regression between age, sex, and y-var, and get square root of mult-r squared as the r-value
  r_value <- sqrt(mult.r_value_2covar_mod$r.squared) #find correlation value (r) between dependent variable
  a.tes(t=t_value_effect_size$test$tstat['5 - 2'],n.1=group_number['1','n'],n.2=group_number['2','n'],R=r_value,q=2) #calculate Cohen's D with the covariate of age & sex
#CatFluency
#remove NAs from dataset for given variable
noNAs_CatFluencyRaw <- your_data_file[,c("ParticipantID","Individual_number","Group","Timepoint","Age","Sex","CatFluency.Raw")]
noNAs_CatFluencyRaw <- noNAs_CatFluencyRaw[complete.cases(noNAs_CatFluencyRaw), ]
post_hoc_aov_CatFluency_2covar_mod <- lme(CatFluency.Raw ~ Group*Timepoint + Age + Sex, random = ~1 | Individual_number/Timepoint, data=noNAs_CatFluencyRaw)
summary(glht(post_hoc_aov_CatFluency_2covar_mod, linfct=mcp(Group="Tukey")))
t_value_effect_size <- summary(glht(post_hoc_aov_CatFluency_2covar_mod, linfct=mcp(Group="Tukey")))
#calculate effect sizes (Cohen's D) with covariate (using a.tes function from the compute.es package)
  #for C vs. aMCI
  your_data_file_CvaMCI_CatFluency.Raw <- subset(your_data_file, your_data_file$Group == 1 | your_data_file$Group == 3)
  your_data_file_CvaMCI_CatFluency.Raw$Group <- droplevels(your_data_file_CvaMCI_CatFluency.Raw$Group)
  group_number <-dplyr::count(your_data_file_CvaMCI_CatFluency.Raw, Group) #count number of participants per group
  mult.r_value_2covar_mod<-summary(lm(CatFluency.Raw ~ Age + Sex, data = your_data_file_CvaMCI_CatFluency.Raw)) #create multiple regression between age, sex, and y-var, and get square root of mult-r squared as the r-value
  r_value <- sqrt(mult.r_value_2covar_mod$r.squared) #find correlation value (r) between dependent variable
  a.tes(t=t_value_effect_size$test$tstat['3 - 1'],n.1=group_number['1','n'],n.2=group_number['2','n'],R=r_value,q=2) #calculate Cohen's D with the covariate of age & sex
  #for C vs. mMCI
  your_data_file_CvmMCI_CatFluency.Raw <- subset(your_data_file, your_data_file$Group == 1 | your_data_file$Group == 4)
  your_data_file_CvmMCI_CatFluency.Raw$Group <- droplevels(your_data_file_CvmMCI_CatFluency.Raw$Group)
  group_number <-dplyr::count(your_data_file_CvmMCI_CatFluency.Raw, Group) #count number of participants per group
  mult.r_value_2covar_mod<-summary(lm(CatFluency.Raw ~ Age + Sex, data = your_data_file_CvmMCI_CatFluency.Raw)) #create multiple regression between age, sex, and y-var, and get square root of mult-r squared as the r-value
  r_value <- sqrt(mult.r_value_2covar_mod$r.squared) #find correlation value (r) between dependent variable
  a.tes(t=t_value_effect_size$test$tstat['4 - 1'],n.1=group_number['1','n'],n.2=group_number['2','n'],R=r_value,q=2) #calculate Cohen's D with the covariate of age & sex
  #for C vs. AD
  your_data_file_CvAD_CatFluency.Raw <- subset(your_data_file, your_data_file$Group == 1 | your_data_file$Group == 5)
  your_data_file_CvAD_CatFluency.Raw$Group <- droplevels(your_data_file_CvAD_CatFluency.Raw$Group)
  group_number <-dplyr::count(your_data_file_CvAD_CatFluency.Raw, Group) #count number of participants per group
  mult.r_value_2covar_mod<-summary(lm(CatFluency.Raw ~ Age + Sex, data = your_data_file_CvAD_CatFluency.Raw)) #create multiple regression between age, sex, and y-var, and get square root of mult-r squared as the r-value
  r_value <- sqrt(mult.r_value_2covar_mod$r.squared) #find correlation value (r) between dependent variable
  a.tes(t=t_value_effect_size$test$tstat['5 - 1'],n.1=group_number['1','n'],n.2=group_number['2','n'],R=r_value,q=2) #calculate Cohen's D with the covariate of age & sex
  #for SCD vs. mMCI
  your_data_file_SCDvmMCI_CatFluency.Raw <- subset(your_data_file, your_data_file$Group == 2 | your_data_file$Group == 4)
  your_data_file_SCDvmMCI_CatFluency.Raw$Group <- droplevels(your_data_file_SCDvmMCI_CatFluency.Raw$Group)
  group_number <-dplyr::count(your_data_file_SCDvmMCI_CatFluency.Raw, Group) #count number of participants per group
  mult.r_value_2covar_mod<-summary(lm(CatFluency.Raw ~ Age + Sex, data = your_data_file_SCDvmMCI_CatFluency.Raw)) #create multiple regression between age, sex, and y-var, and get square root of mult-r squared as the r-value
  r_value <- sqrt(mult.r_value_2covar_mod$r.squared) #find correlation value (r) between dependent variable
  a.tes(t=t_value_effect_size$test$tstat['4 - 2'],n.1=group_number['1','n'],n.2=group_number['2','n'],R=r_value,q=2) #calculate Cohen's D with the covariate of age & sex
  #for SCD vs. AD
  your_data_file_SCDvAD_CatFluency.Raw <- subset(your_data_file, your_data_file$Group == 2 | your_data_file$Group == 5)
  your_data_file_SCDvAD_CatFluency.Raw$Group <- droplevels(your_data_file_SCDvAD_CatFluency.Raw$Group)
  group_number <-dplyr::count(your_data_file_SCDvAD_CatFluency.Raw, Group) #count number of participants per group
  mult.r_value_2covar_mod<-summary(lm(CatFluency.Raw ~ Age + Sex, data = your_data_file_SCDvAD_CatFluency.Raw)) #create multiple regression between age, sex, and y-var, and get square root of mult-r squared as the r-value
  r_value <- sqrt(mult.r_value_2covar_mod$r.squared) #find correlation value (r) between dependent variable
  a.tes(t=t_value_effect_size$test$tstat['5 - 2'],n.1=group_number['1','n'],n.2=group_number['2','n'],R=r_value,q=2) #calculate Cohen's D with the covariate of age & sex
#Switching
#remove NAs from dataset for given variable
noNAs_SwitchingRaw <- your_data_file[,c("ParticipantID","Individual_number","Group","Timepoint","Age","Sex","Switching.Raw")]
noNAs_SwitchingRaw <- noNAs_SwitchingRaw[complete.cases(noNAs_SwitchingRaw), ]
post_hoc_aov_Switching_2covar_mod <- lme(Switching.Raw ~ Group*Timepoint + Age + Sex, random = ~1 | Individual_number/Timepoint, data=noNAs_SwitchingRaw)
summary(glht(post_hoc_aov_Switching_2covar_mod, linfct=mcp(Group="Tukey")))
t_value_effect_size <- summary(glht(post_hoc_aov_Switching_2covar_mod, linfct=mcp(Group="Tukey")))
#calculate effect sizes (Cohen's D) with covariate (using a.tes function from the compute.es package)
  #for C vs. mMCI
  your_data_file_CvmMCI_Switching.Raw <- subset(your_data_file, your_data_file$Group == 1 | your_data_file$Group == 4)
  your_data_file_CvmMCI_Switching.Raw$Group <- droplevels(your_data_file_CvmMCI_Switching.Raw$Group)
  group_number <-dplyr::count(your_data_file_CvmMCI_Switching.Raw, Group) #count number of participants per group
  mult.r_value_2covar_mod<-summary(lm(Switching.Raw ~ Age + Sex, data = your_data_file_CvmMCI_Switching.Raw)) #create multiple regression between age, sex, and y-var, and get square root of mult-r squared as the r-value
  r_value <- sqrt(mult.r_value_2covar_mod$r.squared) #find correlation value (r) between dependent variable
  a.tes(t=t_value_effect_size$test$tstat['4 - 1'],n.1=group_number['1','n'],n.2=group_number['2','n'],R=r_value,q=2) #calculate Cohen's D with the covariate of age & sex
  #for C vs. AD
  your_data_file_CvAD_Switching.Raw <- subset(your_data_file, your_data_file$Group == 1 | your_data_file$Group == 5)
  your_data_file_CvAD_Switching.Raw$Group <- droplevels(your_data_file_CvAD_Switching.Raw$Group)
  group_number <-dplyr::count(your_data_file_CvAD_Switching.Raw, Group) #count number of participants per group
  mult.r_value_2covar_mod<-summary(lm(Switching.Raw ~ Age + Sex, data = your_data_file_CvAD_Switching.Raw)) #create multiple regression between age, sex, and y-var, and get square root of mult-r squared as the r-value
  r_value <- sqrt(mult.r_value_2covar_mod$r.squared) #find correlation value (r) between dependent variable
  a.tes(t=t_value_effect_size$test$tstat['5 - 1'],n.1=group_number['1','n'],n.2=group_number['2','n'],R=r_value,q=2) #calculate Cohen's D with the covariate of age & sex
  #for SCD vs. aMCI
  your_data_file_SCDvaMCI_Switching.Raw <- subset(your_data_file, your_data_file$Group == 2 | your_data_file$Group == 3)
  your_data_file_SCDvaMCI_Switching.Raw$Group <- droplevels(your_data_file_SCDvaMCI_Switching.Raw$Group)
  group_number <-dplyr::count(your_data_file_SCDvaMCI_Switching.Raw, Group) #count number of participants per group
  mult.r_value_2covar_mod<-summary(lm(Switching.Raw ~ Age + Sex, data = your_data_file_SCDvaMCI_Switching.Raw)) #create multiple regression between age, sex, and y-var, and get square root of mult-r squared as the r-value
  r_value <- sqrt(mult.r_value_2covar_mod$r.squared) #find correlation value (r) between dependent variable
  a.tes(t=t_value_effect_size$test$tstat['3 - 2'],n.1=group_number['1','n'],n.2=group_number['2','n'],R=r_value,q=2) #calculate Cohen's D with the covariate of age & sex
  #for SCD vs. mMCI
  your_data_file_SCDvmMCI_Switching.Raw <- subset(your_data_file, your_data_file$Group == 2 | your_data_file$Group == 4)
  your_data_file_SCDvmMCI_Switching.Raw$Group <- droplevels(your_data_file_SCDvmMCI_Switching.Raw$Group)
  group_number <-dplyr::count(your_data_file_SCDvmMCI_Switching.Raw, Group) #count number of participants per group
  mult.r_value_2covar_mod<-summary(lm(Switching.Raw ~ Age + Sex, data = your_data_file_SCDvmMCI_Switching.Raw)) #create multiple regression between age, sex, and y-var, and get square root of mult-r squared as the r-value
  r_value <- sqrt(mult.r_value_2covar_mod$r.squared) #find correlation value (r) between dependent variable
  a.tes(t=t_value_effect_size$test$tstat['4 - 2'],n.1=group_number['1','n'],n.2=group_number['2','n'],R=r_value,q=2) #calculate Cohen's D with the covariate of age & sex
  #for SCD vs. AD
  your_data_file_SCDvAD_Switching.Raw <- subset(your_data_file, your_data_file$Group == 2 | your_data_file$Group == 5)
  your_data_file_SCDvAD_Switching.Raw$Group <- droplevels(your_data_file_SCDvAD_Switching.Raw$Group)
  group_number <-dplyr::count(your_data_file_SCDvAD_Switching.Raw, Group) #count number of participants per group
  mult.r_value_2covar_mod<-summary(lm(Switching.Raw ~ Age + Sex, data = your_data_file_SCDvAD_Switching.Raw)) #create multiple regression between age, sex, and y-var, and get square root of mult-r squared as the r-value
  r_value <- sqrt(mult.r_value_2covar_mod$r.squared) #find correlation value (r) between dependent variable
  a.tes(t=t_value_effect_size$test$tstat['5 - 2'],n.1=group_number['1','n'],n.2=group_number['2','n'],R=r_value,q=2) #calculate Cohen's D with the covariate of age & sex

  
  

  
  
  
  
  
  
