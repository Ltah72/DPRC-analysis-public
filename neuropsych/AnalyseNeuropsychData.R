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
#Example from D-KEFS Stroop Task ---------------------------------------------------------#
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

  
  

  
  
  
  
  
  
