#calculate and visualise the percent changes in the neuropsychogical variables from baseline (F0) to a time period (e.g., F2, two years later), for longitudinal analysis 

#Author: Lenore Tahara-Eckl
#Email: Ltah262@aucklanduni.ac.nz
#Date: 20/06/21

#load libraries via pacman
pacman::p_load(dplyr, ggplot2, psych, car, multcomp, lsr, BayesFactor, tidyr, GPArotation, corrplot, lsmeans, TukeyC, lme4, lmerTest, emmeans)

#add any necessary sources: 
source("https://raw.githubusercontent.com/datavizpyr/data/master/half_flat_violinplot.R") #for raincloud graph

#set up pathway
setwd('/yourpathway/')
#file.choose function
#e.g., setwd('H:/ltah262/PhD/ExecutiveFunction/NeuroPsychAssessment/data/')


#######----------------- Longitudinal (F0 vs. F2) analysis -----------------########

#read in csv files (participant file)
your_data_file <- read.csv("your_longitudinal_data_file.csv")
#e.g, DPRC_neuropsych_data <- read.csv("longitudinal_DPRC_neuropsych_data_lined_up_valid_participants.csv")

#rename first column 
colnames(your_data_file)[1] <-'ParticipantID'

#convert variables
your_data_file$Group <- as.factor(your_data_file$Group)
your_data_file$Sex_binary <- as.factor(your_data_file$Sex_binary)
your_data_file$Timepoint <- as.factor(your_data_file$Timepoint)

#Percentage changes ((F2 - F0) / F0)
#put into long format
percent_change_data <- your_data_file %>% 
  filter(Timepoint == "F2") %>%
  select(ParticipantID, 
         Age,
         Group, 
         Sex_binary,
         Timepoint,
         PercentChange_variable1,
         PercentChange_variable2,
         etc...)
#example below:
#percent_change_data <- DPRC_neuropsych_data %>% 
 # filter(Timepoint == "F2") %>%
 # select(ParticipantID, 
  #       Age,
   #      Classification,
    #     Group, 
     #    Sex,
      #   Sex_binary,
       #  Timepoint,
        # PercentChange_TMTA,
         #PercentChange_TMTB,
         #PercentChange_ColorNaming,
         #PercentChange_WordReading,
         #PercentChange_Inhibition,
         #PercentChange_LetFluency,
         #PercentChange_CatFluency,
         #PercentChange_Switching,
         #PercentChange_HayBTime1z,
         #PercentChange_HayBTime2z,
         #PercentChange_HayBCatAz,
         #PercentChange_HayBCatBz)

#rename variables 
percent_change_data <- rename(percent_change_data, variable_name = PercentChange_variable_name)

#examples below:
#percent_change_data <- rename(percent_change_data, TMTA = PercentChange_TMTA)
#percent_change_data <- rename(percent_change_data, ColorNaming = PercentChange_ColorNaming)
#...

#put into long format
percent_change_data_long <- gather(percent_change_data, 
                                   "Test",
                                   "Percent_change", 
                                   variable1,
                                   variable2,
                                   etc...)
#example below:
#percent_change_data_long <- gather(percent_change_data, 
 #                                  "Test",
  #                                 "Percent_change", 
   #                                TMTA,
    #                               TMTB,
     #                              ColorNaming,
      #                             WordReading,
       #                            Inhibition,
        #                           LetFluency,
         #                          CatFluency,
          #                         Switching,
           #                        HayBTime1z,
            #                       HayBTime2z,
             #                      HayBCatAz,
              #                     HayBCatBz)
#plot percent changes (whole)
ggplot(percent_change_data_long, aes(x = Test, y = Percent_change)) + 
  geom_point(aes(y = Percent_change, color = Percent_change), position = position_jitter(width = .15), size = .5, alpha = 0.8) +
  #geom_boxplot(width = 0.1, fill = "white", outlier.size = .15, aes(colour = Percent_change)) + 
  xlab("Test") + 
  ylab("Percent Change") +
  theme_classic()+
  theme(legend.position = "none")

#Example: plot percent changes for one participant (no Hayling, only aMCI)
#ggplot(subset(percent_change_data_noHay_long, Group %in% ("3")))+ 
 # geom_point(aes(x = Test, y = Percent_change, color = Percent_change), position = position_jitter(width = .15), size = .5, alpha = 0.8) +
  #geom_text() +
  #geom_boxplot(width = 0.1, fill = "white", outlier.size = .15, aes(colour = Percent_change)) + 
  #xlab("Tests") + 
  #ylab("Percent Change") +
  #theme_classic() +
  #theme(legend.position = "none")

#Example: plot by participant percent changes (no Hayling)
#ggplot(percent_change_data_noHay_long, aes(x = ParticipantID, y = Percent_change)) + 
 # geom_point(aes(y = Percent_change, color = Percent_change), position = position_jitter(width = .15), size = .5, alpha = 0.8) +
  #geom_boxplot(width = 0.1, fill = "white", outlier.size = .15, aes(colour = Percent_change)) + 
  #xlab("Participant") + 
  #ylab("Percent Change") +
  #theme_classic() +
  #theme(legend.position = "none")


