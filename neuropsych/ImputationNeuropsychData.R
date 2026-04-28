#Use imputation to fill in the missing data from the DPRC neuropsych data. Will 
#be using the MICE (Multivariate Imputation via Chained Equations) multiple 
#imputation method for this (van Buuren & Groothuis-Oudshoorn, 2010). This is 
#based upon Rubin's Multiple Imputation (Rubin, 2004)

#Author: Lenore Tahara-Eckl
#Email: Ltah262@aucklanduni.ac.nz
#Date: 28/06/21

#load libraries via pacman
pacman::p_load(mice, VIM)

#set up pathway
setwd('/yourpathway/')
#e.g., setwd('H:/ltah262/PhD/ExecutiveFunction/NeuroPsychAssessment/data/June/')

#read in csv files (participant file)
your_data_file <- read.csv("your_data_file_for_imputation.csv")
#e.g., DPRC_neuropsych_data <- read.csv("DPRC_neuropsych_data_participants_imputation.csv")

#rename some columns (if needed)
names(your_data_file)[1] <- "Subject_ID"

#convert variables (if needed)
your_data_file$Group <- as.factor(your_data_file$Group)
your_data_file$Binary_sex <- as.factor(your_data_file$Binary_sex)


#-------------------run MICE imputation on the dataset-------------------------#

#check the missing values present in each variable in the dataset
md.pattern(your_data_file)

#visualise this missing data
mice_plot <- aggr(your_data_file, col=c('navyblue', 'yellow'), numbers=TRUE, sortVars=TRUE, labels=names(your_data_file), cex.axis=.7, gap=3, ylab=c("Missing data", "Pattern"))

#MICE details: 
#m = 5, for 5 multiple imputations datasets being done. 
#maxit = 50, for 50 iterations being taken to impute the missing values.The higher value you have, then the better (more accurate) predictions you will have. 
#In regards to the iterations, use 'Bodner's rule of thumb' (White et al., 2011) as a guide --> e.g. 73 imputations w/ 10 iterations
#method = method used in imputation. 'PMM' for Predictive Mean Matching, for numeric variables. LogReg is for categorical data. 
#set seed for 500
imputed_Data <- mice(your_data_file, m=5, maxit = 50, method = 'pmm', seed = 500)

#check imputted values
summary(imputed_Data)
#check the a variable you want
imputed_Data$imp$variable_name
#e.g., TOPF score
#imputed_Data$imp$TOPF.Raw

#5 datasets are available, and so you can select any using the complete() function. 
#You want to choose the dataset which represents the mean most closely. 
#e.g. select the 2nd dataset
#completeData <- mice::complete(imputed_Data, 2)

#Build your model using all 5 datasets, and then pool together your results from 
#your models 

#build your predictive models
fit0 <- with(data = imputed_Data, exp = lm(missing_data_of_variable ~ variable_1 + variable_2 + etc...)) 
#examples below:
#fit0 <- with(data = imputed_Data, exp = lm(Inhibition.Raw ~ ACE + Group + TOPF.Raw + TrailsA.Raw + TrailsB.Raw + ColorNaming.Raw + WordReading.Raw + LetFluency.Raw + CatFluency.Raw + Switching.Raw + HayBTime1.Raw + HayBTime2.Raw + HayBCatA.Raw + HayBCatB.Raw)) 
#fit1 <- with(data = imputed_Data, exp = lm(Inhibition.Raw ~ TrailsB.Raw + Switching.Raw + HayBCatA.Raw)) 

#compare your models to see which variables would contribute best to its predictive power.
stat <- pool.compare(fit1, fit0, method="Wald")

#combine results of all 5 models into 1 model (pooled model)
combine <- pool(fit)
summary(combine)


#Finally, use this model to fill in the missing values from your dataset. 







