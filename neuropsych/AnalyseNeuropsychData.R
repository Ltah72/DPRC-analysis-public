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
  
####--------------ANCOVA Testing (w/ sex and age) --------------------------#### 
#run mixed design, 2 x 5 ANCOVA for ColorNaming.Raw
aov_ColorNamingRaw <- aov(ColorNaming.Raw ~ Group*Timepoint + Error(Individual_number/Timepoint) + Age + Sex, data = your_data_file)
summary(aov_ColorNamingRaw)
#effect size w/ covariates
sjstats::eta_sq(aov_ColorNamingRaw)

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


  
  

  
  
  
  
  
  
