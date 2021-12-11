#Author: Kevin Aslett
#Code Title: Supplementary_Materials.R
#Paper Title: Do Your Own Research? Searching Online About Misinformation Increases Belief
#Purpose of code: Generate all figures that are located in the supplementary materials of the paper.

#FILES OUT:
#   TABLES   #
#Study_1.txt
#Study_2.txt
#Study_3.txt
#Study_4.txt
#Study_5.txt
#Figure_2c_1.txt
#Figure_2c_2.txt
#Figure_2c_3.txt
#Figure_2d_4.txt
#Figure_2d_3.txt
#Figure_2d_2.txt
#Figure_2d_1.txt
#Figure_3.txt
#Figure_4a_1.txt
#Figure_4a_2.txt
#Figure_4b_1.txt
#Figure_4b_2.txt
#Figure_4c_1.txt
#Figure_4c_2.txt
#Figure_4d_1.txt
#Figure_4d_2.txt

#   FIGURES   #
#All_4_Studies_ROBUST.png
#Study_5_1_ROBUST.png
#Coefs_CIs_ROBUST.png
#Coefs_CIs_2_ROBUST.png
#Coefs_CIs_Predicting_Unrel_Dummy_ROBUST.png
#Quantiles_Low_DL_FULL_ROBUST.png
#Quantiles_High_DL_FULL_ROBUST.png
#Quantiles_Congruent_FULL_ROBUST.png
#Quantiles_Incong_FULL_ROBUST.png

#Load in Libraries:
library(ggplot2)
library(dplyr)
library(reshape2)
library(tidyverse)
library(Rmisc)
library(plm)
library(lmtest)
library(sandwich)
library(rsq)
library(stargazer)
library(xtable)
library(irr)
library(texreg)



#Create Article Metadata dataframe:

#Pull in this data:
Data_Bef_Aft <- read.csv('./Data/Data_Bef_Aft_Misl_False.csv')
Study_2_Ideo <- read.csv('./Data/Study_2_Respondent_Ideo.csv')

#Convert to strings:
Data_Bef_Aft$ResponseId <- as.character(Data_Bef_Aft$ResponseId)
Study_2_Ideo$ResponseId <- as.character(Study_2_Ideo$ResponseId)

#Unqiue values:
Study_2_Ideo <- unique(Study_2_Ideo)
#Merge:
Data_Bef_Aft <- merge(Data_Bef_Aft,Study_2_Ideo,by='ResponseId')

#Create dummy ideology variables:
Data_Bef_Aft <- Data_Bef_Aft %>% mutate(Dummy_Ideology = ifelse(Ideology_Score > 0,'Conservative','Moderate'))
Data_Bef_Aft <- Data_Bef_Aft %>% mutate(Dummy_Ideology = ifelse(Ideology_Score < 0,'Liberal',Dummy_Ideology))

#Create Article Lean Data using responses:
Article_Data <- Data_Bef_Aft %>% mutate(Article_Lean = ifelse(Dummy_Ideology == 'Conservative' & Dummy_Congruence == 1,'Conservative','None'))
Article_data <- Article_data %>% mutate(Article_Lean = ifelse(Dummy_Ideology == 'Liberal' & Dummy_Congruence == 1,'Liberal',Article_Lean))

#Select variables:
Article_data <- Article_data %>% select(Article_day,Article_Lean)

#Pull unique articles and remove NAs:
Article_data <- unique(Article_data)
Article_data <- na.omit(Article_data)


#Study 1:
#Pull in this data: Search Experiment 1: Study 1:
Misl_False_Search <- read.csv('./Data/Search_Exp_Misl_False.csv')
#Select variables of interest:
Model_Data_7 <- Misl_False_Search %>% select(Likert_Evaluation,Susc_FN,Treat_Search,Education_Score,Age,Gender,Income_Score,Dummy_Congruence,Familiar_Dummy,Article_day,ResponseId)
#Remove NA values:
Model_Data_7 = na.omit(Model_Data_7)
Model_Data_7$Gender <- ifelse(Model_Data_7$Gender == 'Female',1,0)

#Run OLS Model with clustered standard errors:
fit_2_1 = glm(Susc_FN ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score + Article_day, data = Model_Data_7)
lin_results_fit_2_1 = coeftest(fit_2_1, vcov. = vcovCL(fit_2_1, cluster = list(Model_Data_7$ResponseId,Model_Data_7$Article_day), type = "HC0"))
#Produce confidence intervals with clustered standard errors:
CI_2_1 <- coefci(fit_2_1, vcov. = vcovCL(fit_2_1, cluster = list(Model_Data_7$ResponseId,Model_Data_7$Article_day), type = "HC0"))

#Run OLS Model with clustered standard errors:
fit_2_2 = glm(Likert_Evaluation ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score + Article_day, data = Model_Data_7)
lin_results_fit_2_2 = coeftest(fit_2_2, vcov. = vcovCL(fit_2_2, cluster = list(Model_Data_7$ResponseId,Model_Data_7$Article_day), type = "HC0"))
#Produce confidence intervals with clustered standard errors:
CI_2_2 <- coefci(fit_2_2, vcov. = vcovCL(fit_2_2, cluster = list(Model_Data_7$ResponseId,Model_Data_7$Article_day), type = "HC0"))

#Unique Articles:
Model_Data_7$Article_day <- as.character(Model_Data_7$Article_day)
Model_Data_7$Article_day <- paste0('Article_day',Model_Data_7$Article_day)
Omit_c <- unique(Model_Data_7$Article_day)
full_omit <- paste(Omit_c, collapse = '|')

#Coefficient Names:
Coef_names= c('(Intercept)','Treatment','Age','Ideological Congruence','Education','Gender','Income')

#Write Table
texreg(list(lin_results_fit_2_1,lin_results_fit_2_2),
       include.ci = FALSE,
       digits=4,
       omit.coef = full_omit,
       caption= 'Results from OLS Regression Results Presented in Figure 1',
       label = "table",
       custom.coef.names = Coef_names,
       custom.model.names= c("Categorical Measure", "7-Point Ordinal Scale"),
       float.pos = "!htbp",
       file='./Tables/Study_1.txt',
       caption.above = TRUE)

#Study 2:


#Pull in this data:
Data_Bef_Aft <- read.csv('./Data/Data_Bef_Aft_Misl_False.csv')

#Select variables of interest:
Model_Data_7 <- Data_Bef_Aft %>% select(Likert_Evaluation,Susc_FN,Treat_Search,Education_Score,Age,Gender,Income_Score,Dummy_Congruence,Familiar_Dummy,Article_day,ResponseId)
#Remove NA values:
Model_Data_7 <- na.omit(Model_Data_7)
Model_Data_7$Gender <- ifelse(Model_Data_7$Gender == 'Female',1,0)

#Run OLS Model with clustered standard errors:
fit_2_1 = glm(Susc_FN ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score + Article_day, data = Model_Data_7)
lin_results_fit_2_1 = coeftest(fit_2_1, vcov. = vcovCL(fit_2_1, cluster = list(Model_Data_7$ResponseId,Model_Data_7$Article_day), type = "HC0"))
#Produce confidence intervals with clustered standard errors:
CI_2_1 <- coefci(fit_2_1, vcov. = vcovCL(fit_2_1, cluster = list(Model_Data_7$ResponseId,Model_Data_7$Article_day), type = "HC0"))

#Run OLS Model with clustered standard errors:
fit_2_2 = glm(Likert_Evaluation ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score + Article_day, data = Model_Data_7)
lin_results_fit_2_2 = coeftest(fit_2_2, vcov. = vcovCL(fit_2_2, cluster = list(Model_Data_7$ResponseId,Model_Data_7$Article_day), type = "HC0"))
#Produce confidence intervals with clustered standard errors:
CI_2_2 <- coefci(fit_2_2, vcov. = vcovCL(fit_2_2, cluster = list(Model_Data_7$ResponseId,Model_Data_7$Article_day), type = "HC0"))

#Unique Articles:
Model_Data_7$Article_day <- as.character(Model_Data_7$Article_day)
Model_Data_7$Article_day <- paste0('Article_day',Model_Data_7$Article_day)
Omit_c <- unique(Model_Data_7$Article_day)
full_omit <- paste(Omit_c, collapse = '|')

#Coefficient Names:
Coef_names= c('(Intercept)','Treatment','Age','Ideological Congruence','Education','Gender','Income')

#Write Table
texreg(list(lin_results_fit_2_1,lin_results_fit_2_2),
       include.ci = FALSE,
       digits=4,
       omit.coef = full_omit,
       caption= 'Results from OLS Regression Results Presented in Figure 2',
       label = "table",
       custom.coef.names = Coef_names,
       custom.model.names= c("Categorical Measure", "7-Point Ordinal Scale"),
       float.pos = "!htbp",
       file='./Tables/Study_2.txt',
       caption.above = TRUE)

#Study 3 Analysis:
#Load Data:
Latency_Data <- read.csv('./Data/Latency_FC_Data.csv')
Latency_Survey <- read.csv('./Data/Latency_Control_Survey.csv')

#Create Article_day data for purpose of merging
Latency_Survey <- Latency_Survey %>% mutate(Article_day = paste0(day,sep='_',Article))

#Select data needed:
Latency_Survey <- Latency_Survey %>% select(Evaluation,Likert_Evaluation,True_Likert_After_Info,Evaluation_After_Info,Age,Dig_Lit_Avg,Income_Score,CRT_Score,Familiar_Story,Education_Score,Duration,Article_day,Ideology_Score,Gender,ResponseId)

#Create Gender dummy variable
Latency_Survey$Gender <- as.character(Latency_Survey$Gender)
Latency_Survey$Gender <- ifelse(Latency_Survey$Gender == 'Female',1,0)


#Create Likert Score post-treatment:
Latency_Survey$True_Likert_After_Info <- as.character(Latency_Survey$True_Likert_After_Info)
Latency_Survey <- Latency_Survey %>% filter(True_Likert_After_Info != 'not asked')
Latency_Survey$True_Likert_After_Info <- substr(Latency_Survey$True_Likert_After_Info, start = 1, stop = 2)
Latency_Survey$True_Likert_After_Info <- as.numeric(Latency_Survey$True_Likert_After_Info)

#Create Likert Score pre-treatment:
Latency_Survey$Likert_Evaluation <- as.character(Latency_Survey$Likert_Evaluation)
Latency_Survey$Likert_Evaluation <- substr(Latency_Survey$Likert_Evaluation, start = 1, stop = 2)
Latency_Survey$Likert_Evaluation <- as.numeric(Latency_Survey$Likert_Evaluation)

#Create True Dummy variable pre and post-treatment:
Latency_Survey$Evaluation <- as.character(Latency_Survey$Evaluation)
Latency_Survey$Evaluation_After_Info <- as.character(Latency_Survey$Evaluation_After_Info)
Latency_Survey$Evaluation <- substr(Latency_Survey$Evaluation, start = 1, stop = 4)
Latency_Survey$Evaluation_After_Info <- substr(Latency_Survey$Evaluation_After_Info, start = 1, stop = 4)
Latency_Survey$True_Dummy <- ifelse(Latency_Survey$Evaluation == 'True',1,0)
Latency_Survey$True_Dummy_After <- ifelse(Latency_Survey$Evaluation_After_Info == 'True',1,0)

#Remove observations that posted that their age was above 85:
Latency_Survey$Age <- ifelse(Latency_Survey$Age > 85,NA,Latency_Survey$Age)



#Pull in Fact-checking data from Study 2:
Data_Bef_Aft <- read.csv('./Data/Data_Bef_Aft_Misl_False.csv')
Data_Bef_Aft$Article_day <- as.character(Data_Bef_Aft$Article_day)

Article_Days <- unique(Data_Bef_Aft$Article_day)

#Only use articles that were rated as false/misleading in Study 2
Latency_Survey <- Latency_Survey %>% filter(Article_day %in% Article_Days)

#Merge pre and post-treatment data:
After_Evaluation <- Latency_Survey %>% select(True_Dummy_After,True_Likert_After_Info,Age,Dig_Lit_Avg,Income_Score,CRT_Score,Familiar_Story,Education_Score,Duration,Article_day,Ideology_Score,Gender,ResponseId)
colnames(After_Evaluation)[1] <- 'True_Dummy'
colnames(After_Evaluation)[2] <- 'Likert_Evaluation'
Before_Evaluation <- Latency_Survey %>% select(True_Dummy,Likert_Evaluation,Age,Dig_Lit_Avg,Income_Score,CRT_Score,Familiar_Story,Education_Score,Duration,Article_day,Ideology_Score,Gender,ResponseId)

#Create Treatment variables:
After_Evaluation$Treatment <- 1
Before_Evaluation$Treatment <- 0

#merge them together
Latency_Search <- rbind(Before_Evaluation,After_Evaluation)

#Create Ideological Congruence data:
Model_Data_7 <- Latency_Search
Model_Data_7 <- Model_Data_7 %>% mutate(Dummy_Ideology = ifelse(Ideology_Score > 0,'Conservative','Moderate'))
Model_Data_7 <- Model_Data_7 %>% mutate(Dummy_Ideology = ifelse(Ideology_Score < 0,'Liberal',Dummy_Ideology))
Model_Data_7 <- merge(Model_Data_7,Article_data,all=T)
Model_Data_7$Article_Lean <- ifelse(Model_Data_7$Article_Lean == 'None','Neutral',Model_Data_7$Article_Lean)
Model_Data_7 <- Model_Data_7 %>% mutate(Dummy_Congruence = ifelse(Dummy_Ideology == Article_Lean,1,0))
Model_Data_7$Susc_FN <- Model_Data_7$True_Dummy
Model_Data_7$Treat_Search <- Model_Data_7$Treatment

#Run OLS Model with clustered standard errors:
fit_2_1 = glm(Susc_FN ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score + Article_day, data = Model_Data_7)
lin_results_fit_2_1 = coeftest(fit_2_1, vcov. = vcovCL(fit_2_1, cluster = list(Model_Data_7$ResponseId,Model_Data_7$Article_day), type = "HC0"))
#Produce confidence intervals with clustered standard errors:
CI_2_1 <- coefci(fit_2_1, vcov. = vcovCL(fit_2_1, cluster = list(Model_Data_7$ResponseId,Model_Data_7$Article_day), type = "HC0"))

#Run OLS Model with clustered standard errors:
fit_2_2 = glm(Likert_Evaluation ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score + Article_day, data = Model_Data_7)
lin_results_fit_2_2 = coeftest(fit_2_2, vcov. = vcovCL(fit_2_2, cluster = list(Model_Data_7$ResponseId,Model_Data_7$Article_day), type = "HC0"))
#Produce confidence intervals with clustered standard errors:
CI_2_2 <- coefci(fit_2_2, vcov. = vcovCL(fit_2_2, cluster = list(Model_Data_7$ResponseId,Model_Data_7$Article_day), type = "HC0"))

#Unique Articles:
Model_Data_7$Article_day <- as.character(Model_Data_7$Article_day)
Model_Data_7$Article_day <- paste0('Article_day',Model_Data_7$Article_day)
Omit_c <- unique(Model_Data_7$Article_day)
full_omit <- paste(Omit_c, collapse = '|')

#Coefficient Names:
Coef_names= c('(Intercept)','Treatment','Age','Ideological Congruence','Education','Gender','Income')

#Write Table
texreg(list(lin_results_fit_2_1,lin_results_fit_2_2),
       include.ci = FALSE,
       digits=4,
       omit.coef = full_omit,
       caption= 'Results from OLS Regression Results Presented in Figure 2',
       label = "table",
       custom.coef.names = Coef_names,
       custom.model.names= c("Categorical Measure", "7-Point Ordinal Scale"),
       float.pos = "!htbp",
       file='./Tables/Study_3.txt',
       caption.above = TRUE)

#Study 4:
#Pull in data
Data_Bef_Aft_Covid <- read.csv('./Data/Experiment_2_Study_2_Misl_False.csv')

#Select variables of interest:
Model_Data_7 <- Data_Bef_Aft_Covid %>% select(Likert_Evaluation,Susc_FN,Treat_Search,Education_Score,Age,Gender,Income_Score,Dummy_Congruence,Familiar_Dummy,Article_day,ResponseId)
#Remove NA values:
Model_Data_7 <- na.omit(Model_Data_7)

#Create control dataframe:
Study_4_Data <- Model_Data_7 %>% filter(Treat_Search == 0)

#Create Gender variable:
Model_Data_7$Gender <- as.character(Model_Data_7$Gender)
Model_Data_7$Gender <- ifelse(Model_Data_7$Gender == 'Female',1,0)

#Run OLS Model with clustered standard errors:
fit_2_1 = glm(Susc_FN ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score + Article_day, data = Model_Data_7)
lin_results_fit_2_1 = coeftest(fit_2_1, vcov. = vcovCL(fit_2_1, cluster = list(Model_Data_7$ResponseId,Model_Data_7$Article_day), type = "HC0"))
#Produce confidence intervals with clustered standard errors:
CI_2_1 <- coefci(fit_2_1, vcov. = vcovCL(fit_2_1, cluster = list(Model_Data_7$ResponseId,Model_Data_7$Article_day), type = "HC0"))

#Run OLS Model with clustered standard errors:
fit_2_2 = glm(Likert_Evaluation ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score + Article_day, data = Model_Data_7)
lin_results_fit_2_2 = coeftest(fit_2_2, vcov. = vcovCL(fit_2_2, cluster = list(Model_Data_7$ResponseId,Model_Data_7$Article_day), type = "HC0"))
#Produce confidence intervals with clustered standard errors:
CI_2_2 <- coefci(fit_2_2, vcov. = vcovCL(fit_2_2, cluster = list(Model_Data_7$ResponseId,Model_Data_7$Article_day), type = "HC0"))

#Unique Articles:
Model_Data_7$Article_day <- as.character(Model_Data_7$Article_day)
Model_Data_7$Article_day <- paste0('Article_day',Model_Data_7$Article_day)
Omit_c <- unique(Model_Data_7$Article_day)
full_omit <- paste(Omit_c, collapse = '|')

#Coefficient Names:
Coef_names= c('(Intercept)','Treatment','Age','Ideological Congruence','Education','Gender','Income')

#Write Table
texreg(list(lin_results_fit_2_1,lin_results_fit_2_2),
       include.ci = FALSE,
       digits=4,
       omit.coef = full_omit,
       caption= 'Results from OLS Regression Results Presented in Figure 2',
       label = "table",
       custom.coef.names = Coef_names,
       custom.model.names= c("Categorical Measure", "7-Point Ordinal Scale"),
       float.pos = "!htbp",
       file='./Tables/Study_4.txt',
       caption.above = TRUE)



#Study 5 Data:

#Pull in Fact-Checker Ideological Perspective
FC_Ideo_Data <- read.csv('./Data/FC_Ideo_Data.csv')
FC_Ideo_Data$X <- NULL

#Pull in ControlData :
Control_Data <- read.csv('./Data/Control_Data_Study_5.csv')

#Merge data:
Control_Data <- merge(Control_Data,FC_Ideo_Data,by='Article_day')

#Create Ideological Perspective variable:
Control_Data <- Control_Data %>% mutate(Dummy_Ideology = ifelse(Ideology_Score > 0,'Conservative','Moderate'))
Control_Data <- Control_Data %>% mutate(Dummy_Ideology = ifelse(Ideology_Score < 0,'Liberal',Dummy_Ideology))
Control_Data <- Control_Data %>% mutate(Ideo_Congruence = ifelse(Dummy_Ideology == Article_Lean,1,0))

#Read CSV (Treatment Data):
Treatment_Data <- read.csv('./Data/Treatment_Data_Study_5.csv')
Treatment_Data$Link_1 <- NULL
Treatment_Data$Link_2 <- NULL

#Merge control and treatment data:
Treatment_Data <- merge(Treatment_Data,FC_Ideo_Data,by='Article_day')

#Create Ideological Perspective variable:
Treatment_Data <- Treatment_Data %>% mutate(Dummy_Ideology = ifelse(Ideology_Score > 0,'Conservative','Moderate'))
Treatment_Data <- Treatment_Data %>% mutate(Dummy_Ideology = ifelse(Ideology_Score < 0,'Liberal',Dummy_Ideology))
Treatment_Data <- Treatment_Data %>% mutate(Ideo_Congruence = ifelse(Dummy_Ideology == Article_Lean,1,0))

#Merge data
All_Data <- rbind(Treatment_Data,Control_Data)

#Filter only false/misleading articles:
FM_Data <- All_Data %>% filter(FC_Eval == 'FM')

#Run linear regression and produCce coefficient values and clustered standard errors:
fit_1_1 = glm(True_Dummy ~ Treatment + Ideo_Congruence + Age + Gender + Education_Score + Income_Score + Article_day,data=FM_Data)
lin_results_fit_1_1 = coeftest(fit_1_1, vcov. = vcovCL(fit_1_1, cluster = list(FM_Data$Article_day), type = "HC0"))

#Run linear regression and produce coefficient values and clustered standard errors:
fit_1_2 = glm(Four_Ordinal ~Treatment + Ideo_Congruence + Age + Gender + Education_Score + Income_Score + Article_day,data=FM_Data)
lin_results_fit_1_2 = coeftest(fit_1_2, vcov. = vcovCL(fit_1_2, cluster = list(FM_Data$Article_day), type = "HC0"))

#Run linear regression and produce coefficient values and clustered standard errors:
fit_1_3 = glm(Seven_Ordinal ~ Treatment + Ideo_Congruence + Age + Gender + Education_Score + Income_Score + Article_day,data=FM_Data)
lin_results_fit_1_3 = coeftest(fit_1_3, vcov. = vcovCL(fit_1_3, cluster = list(FM_Data$Article_day), type = "HC0"))

#Name a different dataset:
Model_Data_7 <- FM_Data

#Unique Articles:
Model_Data_7$Article_day <- as.character(Model_Data_7$Article_day)
Model_Data_7$Article_day <- paste0('Article_day',Model_Data_7$Article_day)
Omit_c <- unique(Model_Data_7$Article_day)
full_omit <- paste(Omit_c, collapse = '|')

#Coefficient Names:
Coef_names= c('(Intercept)','Treatment','Age','Ideological Congruence','Education','Gender','Income')

#Write Table
texreg(list(lin_results_fit_1_1,lin_results_fit_1_2,lin_results_fit_1_3),
       include.ci = FALSE,
       digits=4,
       omit.coef = full_omit,
       caption= 'Results from OLS Regression Results Presented in Figure 2',
       label = "table",
       custom.coef.names = Coef_names,
       custom.model.names= c("Categorical Measure","4-Point Ordinal Scale","7-Point Ordinal Scale"),
       float.pos = "!htbp",
       file='./Tables/Study_5.txt',
       caption.above = TRUE)

#Merge Google Search Results and survey results for each article in Study 5:
#Day 1
Google_results_1 <- read.csv('./Data/Google_Search_Results_Treatment_Day_1_All_New_Attempt.csv')
Treatment_1 <- Treatment_Data %>% filter(Day == 'Day_1')
Treatment_1$ResponseId <-  as.character(Treatment_1$ResponseId)
Treatment_1$Article_day <-  as.character(Treatment_1$Article_day)
Google_results_1$Article_Eval <- as.character(Google_results_1$Article_Eval)
Google_results_1$Article_Eval <- paste0('Day_1_',Google_results_1$Article_Eval)
Survey_1 <- merge(Treatment_1,Google_results_1,by.x=c('ResponseId','Article_day'),by.y=c('Respondent_Id','Article_Eval'))
Survey_1 <- Survey_1 %>% select(ResponseId,Day,Article_day,Category,True_Dummy,Seven_Ordinal,Four_Ordinal,Age,Gender,FC_Eval,List_Scores,ALL_URLS,Dummy_Ideology,Ideo_Congruence,Income_Score,Education_Score,Dig_Lit_Score,Prop_Unreliable,Mean_Score,Article_Lean)

#Day 2
Google_results_2 <- read.csv('./Data/Google_Search_Results_Treatment_Day_2_All_New_Attempt.csv')
Treatment_2 <- Treatment_Data %>% filter(Day == 'Day_2')
Google_results_2$Article_Eval <- as.character(Google_results_2$Article_Eval)
Google_results_2$Article_Eval <- paste0('Day_2_',Google_results_2$Article_Eval)
Survey_2 <- merge(Treatment_2,Google_results_2,by.x=c('ResponseId','Article_day'),by.y=c('Respondent_Id','Article_Eval'))
Survey_2 <- Survey_2 %>% select(ResponseId,Day,Article_day,Category,True_Dummy,Seven_Ordinal,Four_Ordinal,Age,Gender,FC_Eval,List_Scores,ALL_URLS,Dummy_Ideology,Ideo_Congruence,Income_Score,Education_Score,Dig_Lit_Score,Prop_Unreliable,Mean_Score,Article_Lean)

#Day 3
Google_results_3 <- read.csv('./Data/Google_Search_Results_Treatment_Day_3_All_New_Attempt.csv')
Treatment_3 <- Treatment_Data %>% filter(Day == 'Day_3')
Google_results_3$Article_Eval <- as.character(Google_results_3$Article_Eval)
Google_results_3$Article_Eval <- paste0('Day_3_',Google_results_3$Article_Eval)
Survey_3 <- merge(Treatment_3,Google_results_3,by.x=c('ResponseId','Article_day'),by.y=c('Respondent_Id','Article_Eval'))
Survey_3 <- Survey_3 %>% select(ResponseId,Day,Article_day,Category,True_Dummy,Seven_Ordinal,Four_Ordinal,Age,Gender,FC_Eval,List_Scores,ALL_URLS,Dummy_Ideology,Ideo_Congruence,Income_Score,Education_Score,Dig_Lit_Score,Prop_Unreliable,Mean_Score,Article_Lean)

#Day 4
Google_results_4 <- read.csv('./Data/Google_Search_Results_Treatment_Day_4_All_New_Attempt.csv')
Treatment_4 <- Treatment_Data %>% filter(Day == 'Day_4')
Google_results_4$Article_Eval <- as.character(Google_results_4$Article_Eval)
Google_results_4$Article_Eval <- paste0('Day_4_',Google_results_4$Article_Eval)
Survey_4 <- merge(Treatment_4,Google_results_4,by.x=c('ResponseId','Article_day'),by.y=c('Respondent_Id','Article_Eval'))
Survey_4 <- Survey_4 %>% select(ResponseId,Day,Article_day,Category,True_Dummy,Seven_Ordinal,Four_Ordinal,Age,Gender,FC_Eval,List_Scores,ALL_URLS,Dummy_Ideology,Ideo_Congruence,Income_Score,Education_Score,Dig_Lit_Score,Prop_Unreliable,Mean_Score,Article_Lean)

#Day 5
Google_results_5 <- read.csv('./Data/Google_Search_Results_Treatment_Day_5_All_New_Attempt.csv')
Treatment_5 <- Treatment_Data %>% filter(Day == 'Day_5')
Google_results_5$Article_Eval <- as.character(Google_results_5$Article_Eval)
Google_results_5$Article_Eval <- paste0('Day_5_',Google_results_5$Article_Eval)
Survey_5 <- merge(Treatment_5,Google_results_5,by.x=c('ResponseId','Article_day'),by.y=c('Respondent_Id','Article_Eval'))
Survey_5 <- Survey_5 %>% select(ResponseId,Day,Article_day,Category,True_Dummy,Seven_Ordinal,Four_Ordinal,Age,Gender,FC_Eval,List_Scores,ALL_URLS,Dummy_Ideology,Ideo_Congruence,Income_Score,Education_Score,Dig_Lit_Score,Prop_Unreliable,Mean_Score,Article_Lean)

#Day 6
Google_results_6 <- read.csv('./Data/Google_Search_Results_Treatment_Day_6_All_New_Attempt.csv')
Treatment_6 <- Treatment_Data %>% filter(Day == 'Day_6')
Google_results_6$Article_Eval <- as.character(Google_results_6$Article_Eval)
Google_results_6$Article_Eval <- paste0('Day_6_',Google_results_6$Article_Eval)
Survey_6 <- merge(Treatment_6,Google_results_6,by.x=c('ResponseId','Article_day'),by.y=c('Respondent_Id','Article_Eval'))
Survey_6 <- Survey_6 %>% select(ResponseId,Day,Article_day,Category,True_Dummy,Seven_Ordinal,Four_Ordinal,Age,Gender,FC_Eval,List_Scores,ALL_URLS,Dummy_Ideology,Ideo_Congruence,Income_Score,Education_Score,Dig_Lit_Score,Prop_Unreliable,Mean_Score,Article_Lean)

#Day 7
Google_results_7 <- read.csv('./Data/Google_Search_Results_Treatment_Day_7_All_New_Attempt.csv')
Treatment_7 <- Treatment_Data %>% filter(Day == 'Day_7')
Google_results_7$Article_Eval <- as.character(Google_results_7$Article_Eval)
Google_results_7$Article_Eval <- paste0('Day_7_',Google_results_7$Article_Eval)
Survey_7 <- merge(Treatment_7,Google_results_7,by.x=c('ResponseId','Article_day'),by.y=c('Respondent_Id','Article_Eval'))
Survey_7 <- Survey_7 %>% select(ResponseId,Day,Article_day,Category,True_Dummy,Seven_Ordinal,Four_Ordinal,Age,Gender,FC_Eval,List_Scores,ALL_URLS,Dummy_Ideology,Ideo_Congruence,Income_Score,Education_Score,Dig_Lit_Score,Prop_Unreliable,Mean_Score,Article_Lean)

#Day 8
Google_results_8 <- read.csv('./Data/Google_Search_Results_Treatment_Day_8_All_New_Attempt.csv')
Treatment_8 <- Treatment_Data %>% filter(Day == 'Day_8')
Google_results_8$Article_Eval <- as.character(Google_results_8$Article_Eval)
Google_results_8$Article_Eval <- paste0('Day_8_',Google_results_8$Article_Eval)
Survey_8 <- merge(Treatment_8,Google_results_8,by.x=c('ResponseId','Article_day'),by.y=c('Respondent_Id','Article_Eval'))
Survey_8 <- Survey_8 %>% select(ResponseId,Day,Article_day,Category,True_Dummy,Seven_Ordinal,Four_Ordinal,Age,Gender,FC_Eval,List_Scores,ALL_URLS,Dummy_Ideology,Ideo_Congruence,Income_Score,Education_Score,Dig_Lit_Score,Prop_Unreliable,Mean_Score,Article_Lean)

#Day 9
Google_results_9 <- read.csv('./Data/Google_Search_Results_Treatment_Day_9_All_New_Attempt.csv')
Treatment_9 <- Treatment_Data %>% filter(Day == 'Day_9')
Google_results_9$Article_Eval <- as.character(Google_results_9$Article_Eval)
Google_results_9$Article_Eval <- paste0('Day_9_',Google_results_9$Article_Eval)
Survey_9 <- merge(Treatment_9,Google_results_9,by.x=c('ResponseId','Article_day'),by.y=c('Respondent_Id','Article_Eval'))
Survey_9 <- Survey_9 %>% select(ResponseId,Day,Article_day,Category,True_Dummy,Seven_Ordinal,Four_Ordinal,Age,Gender,FC_Eval,List_Scores,ALL_URLS,Dummy_Ideology,Ideo_Congruence,Income_Score,Education_Score,Dig_Lit_Score,Prop_Unreliable,Mean_Score,Article_Lean)

#Day 10
Google_results_10 <- read.csv('./Data/Google_Search_Results_Treatment_Day_10_All_New_Attempt.csv')
Treatment_10 <- Treatment_Data %>% filter(Day == 'Day_10')
Google_results_10$Article_Eval <- as.character(Google_results_10$Article_Eval)
Google_results_10$Article_Eval <- paste0('Day_10_',Google_results_10$Article_Eval)
Survey_10 <- merge(Treatment_10,Google_results_10,by.x=c('ResponseId','Article_day'),by.y=c('Respondent_Id','Article_Eval'))
Survey_10 <- Survey_10 %>% select(ResponseId,Day,Article_day,Category,True_Dummy,Seven_Ordinal,Four_Ordinal,Age,Gender,FC_Eval,List_Scores,ALL_URLS,Dummy_Ideology,Ideo_Congruence,Income_Score,Education_Score,Dig_Lit_Score,Prop_Unreliable,Mean_Score,Article_Lean)

#Day 11
Google_results_11 <- read.csv('./Data/Google_Search_Results_Treatment_Day_11_All_New_Attempt.csv')
Treatment_11 <- Treatment_Data %>% filter(Day == 'Day_11')
Google_results_11$Article_Eval <- as.character(Google_results_11$Article_Eval)
Google_results_11$Article_Eval <- paste0('Day_11_',Google_results_11$Article_Eval)
Survey_11 <- merge(Treatment_11,Google_results_11,by.x=c('ResponseId','Article_day'),by.y=c('Respondent_Id','Article_Eval'))
Survey_11 <- Survey_11 %>% select(ResponseId,Day,Article_day,Category,True_Dummy,Seven_Ordinal,Four_Ordinal,Age,Gender,FC_Eval,List_Scores,ALL_URLS,Dummy_Ideology,Ideo_Congruence,Income_Score,Education_Score,Dig_Lit_Score,Prop_Unreliable,Mean_Score,Article_Lean)

#Day 12
Google_results_12 <- read.csv('./Data/Google_Search_Results_Treatment_Day_12_All_New_Attempt.csv')
Treatment_12 <- Treatment_Data %>% filter(Day == 'Day_12')
Google_results_12$Article_Eval <- as.character(Google_results_12$Article_Eval)
Google_results_12$Article_Eval <- paste0('Day_12_',Google_results_12$Article_Eval)
Survey_12 <- merge(Treatment_12,Google_results_12,by.x=c('ResponseId','Article_day'),by.y=c('Respondent_Id','Article_Eval'))
Survey_12 <- Survey_12 %>% select(ResponseId,Day,Article_day,Category,True_Dummy,Seven_Ordinal,Four_Ordinal,Age,Gender,FC_Eval,List_Scores,ALL_URLS,Dummy_Ideology,Ideo_Congruence,Income_Score,Education_Score,Dig_Lit_Score,Prop_Unreliable,Mean_Score,Article_Lean)

#Merge data from each day:
Survey_Unrel <- rbind(Survey_1,
                      Survey_2,
                      Survey_3,
                      Survey_4,
                      Survey_5,
                      Survey_6,
                      Survey_7,
                      Survey_8,
                      Survey_9,
                      Survey_10,
                      Survey_11,
                      Survey_12)

#Create dataframe with this basic data:
Combined_GS_Survey_Data <- Survey_Unrel

#Create string list of news sites scores:
Survey_Unrel$List_Scores <- as.character(Survey_Unrel$List_Scores)
Survey_Unrel$List_Scores <- gsub('\\[|\\]','',Survey_Unrel$List_Scores)
Survey_Unrel$List_Scores <- gsub('\\[|\\]','',Survey_Unrel$List_Scores)
Survey_Unrel <- Survey_Unrel %>% filter(FC_Eval == 'FM')


#Create Average Score by Respondent:
Survey_Evaluations <- Survey_Unrel %>% filter(List_Scores != '')
Survey_Evaluations <- Survey_Evaluations %>% select(ResponseId,Article_day,List_Scores)
Respondent_Evaluations <- Survey_Evaluations %>% select(ResponseId,Article_day)
Respondent_Evaluations <- unique(Respondent_Evaluations)

#Create Dummy Variable for People who only saw very reliable news sites in Google Search Results (85):

Final_Mat <- matrix(ncol=5)
Only_Rel_URLs <- c()
Some_Unrel_URLs <- c()
Total_Rel_Sources <- c()
Total_Unrel_Sources <- c()
Total_Sources <- c()
for(i in 1:nrow(Survey_Unrel)){
  All_Scores <- unlist(strsplit(Survey_Unrel$List_Scores[i], split=", "))
  All_Scores <- as.numeric(All_Scores)
  Rel_Sources = 0
  Unrel_Sources = 0
  Tot_Sources = 0
  if(length(All_Scores) == 0){
    All_Scores = NA} else{
      for(x in 1:length(All_Scores)){
        Tot_Sources = Tot_Sources + 1
        if(All_Scores[x] > 85){
          Rel_Sources = Rel_Sources + 1
        } else{
          if(All_Scores[x] < 60){
            Unrel_Sources = Unrel_Sources + 1
          }
        }
      } 
      if(Rel_Sources > 0 & Rel_Sources == Tot_Sources){
        Only_Rel_Sources = 1
      } else{
        Only_Rel_Sources = 0
      }
      if(Unrel_Sources > 0){
        Some_Unrel_Sources = 1
      } else{
        Some_Unrel_Sources = 0
      }
    }
  Only_Rel_URLs = c(Only_Rel_URLs,Only_Rel_Sources)
  Some_Unrel_URLs = c(Some_Unrel_URLs,Some_Unrel_Sources)
  Total_Rel_Sources = c(Total_Rel_Sources,Rel_Sources)
  Total_Unrel_Sources = c(Total_Unrel_Sources,Unrel_Sources)
  Total_Sources = c(Total_Sources,Tot_Sources)
}

#Apply data to dataset:
Survey_Unrel$Only_Rel_URLs <- Only_Rel_URLs
Survey_Unrel$Some_Unrel_URLs <- Some_Unrel_URLs
Survey_Unrel$Total_Rel_Sources <- Total_Rel_Sources
Survey_Unrel$Total_Unrel_Sources <- Total_Unrel_Sources
Survey_Unrel$Total_Sources <- Total_Sources




#Filter only responses who only saw very reliable news sites in Google Search Results (85)
Survey_Unrel_1 <- Survey_Unrel %>% filter(Only_Rel_URLs == 1)

#Select variables:
Survey_Unrel_1 <- Survey_Unrel_1 %>% select(Article_day,True_Dummy,Four_Ordinal,Seven_Ordinal,Age,Gender,ResponseId,Education_Score,Income_Score,Ideo_Congruence)
Survey_Unrel_1$Treatment <- 1

#Only unique responses:
Survey_Unrel_1 <- unique(Survey_Unrel_1)

#Pull control data:
Control_Data <- read.csv('./Data/Control_Data_Study_5.csv')

#Merge data
Control_Data <- merge(Control_Data,FC_Ideo_Data,by='Article_day')

#Create Ideological Perspective data:
Control_Data <- Control_Data %>% mutate(Dummy_Ideology = ifelse(Ideology_Score > 0,'Conservative','Moderate'))
Control_Data <- Control_Data %>% mutate(Dummy_Ideology = ifelse(Ideology_Score < 0,'Liberal',Dummy_Ideology))
Control_Data <- Control_Data %>% mutate(Ideo_Congruence = ifelse(Dummy_Ideology == Article_Lean,1,0))

#Only keep those who had the web-tracking extension on while they evaluated articles:
Control_Data$Article_day <- as.character(Control_Data$Article_day)
Control_WT_Data <- read.csv('./Data/output_Control_Survey_2.csv')
Control_WT_Data$Article_day <- as.character(Control_WT_Data$Article_day)
Control_WT_Data$X <- NULL
Control_WT_Data <- unique(Control_WT_Data)
colnames(Control_WT_Data)[1] <- 'ResponseId'
Control_Data <- merge(Control_Data,Control_WT_Data,by=c('ResponseId','Article_day'))

#Filter false/misleading articles:
Control_Data <- Control_Data %>% filter(FC_Eval == 'FM')
Control_Data <- Control_Data %>% select(Article_day,True_Dummy,Four_Ordinal,Seven_Ordinal,Age,Gender,ResponseId,Education_Score,Income_Score,Ideo_Congruence)
Control_Data$Treatment <- 0

#Merge treatment and control articles:
New_Data <- rbind(Survey_Unrel_1,Control_Data)
#Remove NAs
New_Data <- na.omit(New_Data)



#Run Model with clustered standard errors:
fit_2_1 = glm(Seven_Ordinal ~ Treatment + Ideo_Congruence + Age + Gender + Education_Score + Income_Score + Article_day,data=New_Data)
lin_results_fit_2_1 = coeftest(fit_2_1, vcov. = vcovCL(fit_2_1, cluster = list(New_Data$ResponseId,New_Data$Article_day), type = "HC0"))

#Run Model with clustered standard errors:
fit_2_2 = glm(Four_Ordinal ~ Treatment + Ideo_Congruence + Age + Gender + Education_Score + Income_Score + Article_day,data=New_Data)
lin_results_fit_2_2 = coeftest(fit_2_2, vcov. = vcovCL(fit_2_2, cluster = list(New_Data$ResponseId,New_Data$Article_day), type = "HC0"))

#Run Model with clustered standard errors:
fit_2_3 = glm(True_Dummy ~ Treatment + Ideo_Congruence + Age + Gender + Education_Score + Income_Score  + Article_day,data=New_Data)
lin_results_fit_2_3 = coeftest(fit_2_3, vcov. = vcovCL(fit_2_3, cluster = list(New_Data$ResponseId,New_Data$Article_day), type = "HC0"))

#New dataframe:
Model_Data_7 <- New_Data

#Unique Articles:
Model_Data_7$Article_day <- as.character(Model_Data_7$Article_day)
Model_Data_7$Article_day <- paste0('Article_day',Model_Data_7$Article_day)
Omit_c <- unique(Model_Data_7$Article_day)
full_omit <- paste(Omit_c, collapse = '|')

#Coefficient Names:
Coef_names= c('(Intercept)','Treatment','Age','Ideological Congruence','Education','Gender','Income')

#Write Table
texreg(list(lin_results_fit_2_3,lin_results_fit_2_1,lin_results_fit_2_2),
       include.ci = FALSE,
       digits=4,
       omit.coef = full_omit,
       caption= 'Results from OLS Regression Results Presented in Figure 2c (Only Very Reliable News Returned)',
       label = "table",
       custom.coef.names = Coef_names,
       custom.model.names= c("Categorical Measure","4-Point Ordinal Scale","7-Point Ordinal Scale"),
       float.pos = "!htbp",
       file='./Tables/Figure_2c_1.txt',
       caption.above = TRUE)


#Some Unreliable

#Filter only responses who saw some unreliable news sites:
Survey_Unrel_1 <- Survey_Unrel %>% filter(Some_Unrel_URLs == 1)

#Select Variables:
Survey_Unrel_1 <- Survey_Unrel_1 %>% select(Article_day,True_Dummy,Four_Ordinal,Seven_Ordinal,Age,Gender,ResponseId,Education_Score,Income_Score,Ideo_Congruence)
Survey_Unrel_1$Treatment <- 1
#Unique variables:
Survey_Unrel_1 <- unique(Survey_Unrel_1)

#Merge data:
New_Data <- rbind(Survey_Unrel_1,Control_Data)
#Remove NAs
New_Data <- na.omit(New_Data)



#Run Model with clustered standard errors:
fit_2_1 = glm(Seven_Ordinal ~ Treatment + Ideo_Congruence + Age + Gender + Education_Score + Income_Score + Article_day,data=New_Data)
lin_results_fit_2_1 = coeftest(fit_2_1, vcov. = vcovCL(fit_2_1, cluster = list(New_Data$ResponseId,New_Data$Article_day), type = "HC0"))

#Run Model with clustered standard errors:
fit_2_2 = glm(Four_Ordinal ~ Treatment + Ideo_Congruence + Age + Gender + Education_Score + Income_Score + Article_day,data=New_Data)
lin_results_fit_2_2 = coeftest(fit_2_2, vcov. = vcovCL(fit_2_2, cluster = list(New_Data$ResponseId,New_Data$Article_day), type = "HC0"))

#Run Model with clustered standard errors:
fit_2_3 = glm(True_Dummy ~ Treatment + Ideo_Congruence + Age + Gender + Education_Score + Income_Score  + Article_day,data=New_Data)
lin_results_fit_2_3 = coeftest(fit_2_3, vcov. = vcovCL(fit_2_3, cluster = list(New_Data$ResponseId,New_Data$Article_day), type = "HC0"))

#New dataframe:
Model_Data_7 <- New_Data

#Unique Articles:
Model_Data_7$Article_day <- as.character(Model_Data_7$Article_day)
Model_Data_7$Article_day <- paste0('Article_day',Model_Data_7$Article_day)
Omit_c <- unique(Model_Data_7$Article_day)
full_omit <- paste(Omit_c, collapse = '|')

#Coefficient Names:
Coef_names= c('(Intercept)','Treatment','Age','Ideological Congruence','Education','Gender','Income')

#Write Table
texreg(list(lin_results_fit_2_3,lin_results_fit_2_1,lin_results_fit_2_2),
       include.ci = FALSE,
       digits=4,
       omit.coef = full_omit,
       caption= 'Results from OLS Regression Results Presented in Figure 2c (Some Unreliable News Returned)',
       label = "table",
       custom.coef.names = Coef_names,
       custom.model.names= c("Categorical Measure","4-Point Ordinal Scale","7-Point Ordinal Scale"),
       float.pos = "!htbp",
       file='./Tables/Figure_2c_2.txt',
       caption.above = TRUE)

#Filter only responses who didnt see an unreliable or reliable news sites:
Survey_Unrel_1 <- Survey_Unrel %>% filter(Total_Sources == 0)

#Select Variables:
Survey_Unrel_1 <- Survey_Unrel_1 %>% select(Article_day,True_Dummy,Four_Ordinal,Seven_Ordinal,Age,Gender,ResponseId,Education_Score,Income_Score,Ideo_Congruence)
Survey_Unrel_1$Treatment <- 1
#Unique variables:
Survey_Unrel_1 <- unique(Survey_Unrel_1)

#Merge data:
New_Data <- rbind(Survey_Unrel_1,Control_Data)
#Remove NAs
New_Data <- na.omit(New_Data)


#Run Model with clustered standard errors:
fit_2_1 = glm(Seven_Ordinal ~ Treatment + Ideo_Congruence + Age + Gender + Education_Score + Income_Score + Article_day,data=New_Data)
lin_results_fit_2_1 = coeftest(fit_2_1, vcov. = vcovCL(fit_2_1, cluster = list(New_Data$ResponseId,New_Data$Article_day), type = "HC0"))

#Run Model with clustered standard errors:
fit_2_2 = glm(Four_Ordinal ~ Treatment + Ideo_Congruence + Age + Gender + Education_Score + Income_Score + Article_day,data=New_Data)
lin_results_fit_2_2 = coeftest(fit_2_2, vcov. = vcovCL(fit_2_2, cluster = list(New_Data$ResponseId,New_Data$Article_day), type = "HC0"))

#Run Model with clustered standard errors:
fit_2_3 = glm(True_Dummy ~ Treatment + Ideo_Congruence + Age + Gender + Education_Score + Income_Score  + Article_day,data=New_Data)
lin_results_fit_2_3 = coeftest(fit_2_3, vcov. = vcovCL(fit_2_3, cluster = list(New_Data$ResponseId,New_Data$Article_day), type = "HC0"))

#New dataframe:
Model_Data_7 <- New_Data

#Unique Articles:
Model_Data_7$Article_day <- as.character(Model_Data_7$Article_day)
Model_Data_7$Article_day <- paste0('Article_day',Model_Data_7$Article_day)
Omit_c <- unique(Model_Data_7$Article_day)
full_omit <- paste(Omit_c, collapse = '|')

#Coefficient Names:
Coef_names= c('(Intercept)','Treatment','Age','Ideological Congruence','Education','Gender','Income')

#Write Table
texreg(list(lin_results_fit_2_3,lin_results_fit_2_1,lin_results_fit_2_2),
       include.ci = FALSE,
       digits=4,
       omit.coef = full_omit,
       caption= 'Results from OLS Regression Results Presented in Figure 2c (No News Returned)',
       label = "table",
       custom.coef.names = Coef_names,
       custom.model.names= c("Categorical Measure","4-Point Ordinal Scale","7-Point Ordinal Scale"),
       float.pos = "!htbp",
       file='./Tables/Figure_2c_3.txt',
       caption.above = TRUE)

#Treatment Only - Subset By Quality of Google Results:
Survey_Unrel <- Combined_GS_Survey_Data

#Create string list of news sites scores:
Survey_Unrel$List_Scores <- as.character(Survey_Unrel$List_Scores)
Survey_Unrel$List_Scores <- gsub('\\[|\\]','',Survey_Unrel$List_Scores)
Survey_Unrel$List_Scores <- gsub('\\[|\\]','',Survey_Unrel$List_Scores)
Survey_Unrel <- Survey_Unrel %>% filter(FC_Eval == 'FM')


#Pull average reliability score of news viewed by respondents:
Mean_Scores_2 = c()
Rel_Maj_Dummy = c()
for(i in 1:nrow(Survey_Unrel)){
  All_Scores <- unlist(strsplit(Survey_Unrel$List_Scores[i], split=", "))
  All_Scores <- as.numeric(All_Scores)
  Rel_Sources = 0
  if(length(All_Scores) == 0){
    All_Scores = NA
    Rel_Maj_Dummy = c(Rel_Maj_Dummy,0)} else{
      for(x in 1:length(All_Scores)){
        if(All_Scores[x] > 59.5){
          Rel_Sources = Rel_Sources + 1
        }
      } 
      if(Rel_Sources > 5){
        Rel_Maj_Dummy = c(Rel_Maj_Dummy,1)
      } else{
        Rel_Maj_Dummy = c(Rel_Maj_Dummy,0)
      }
    }
  Mean_Scores_2 = c(Mean_Scores_2,mean(All_Scores))
}


#Create mean proportion and average reliability scores of news viewed:
Survey_Unrel$Mean_Scores <- Mean_Scores_2
Survey_Unrel_Mean <- Survey_Unrel %>% dplyr::group_by(ResponseId,Article_day) %>% dplyr::summarise(Mean_All_Scores = mean(Mean_Scores,na.rm=T))
Survey_Unrel_Prop <- Survey_Unrel %>% dplyr::group_by(ResponseId,Article_day) %>% dplyr::summarise(Mean_Prop_Unrel = mean(Prop_Unreliable,na.rm=T))

#Merge:
Survey_D <- merge(Survey_Unrel_Mean,Survey_Unrel_Prop,by=c('ResponseId','Article_day'))
Survey_Unrel <- merge(Survey_Unrel,Survey_D,by=c('ResponseId','Article_day'))
Survey_Unrel <- Survey_Unrel %>% filter(Total_Sources != 0)


#Filter by quartile:
Survey_Unrel_1 <- Survey_Unrel %>% filter(Mean_All_Scores < quantile(Survey_Unrel$Mean_All_Scores,na.rm=T)[2])

#Select variables
Survey_Unrel_1 <- Survey_Unrel_1 %>% select(Article_day,True_Dummy,Four_Ordinal,Seven_Ordinal,Age,Gender,ResponseId,Education_Score,Income_Score,Ideo_Congruence)
Survey_Unrel_1$Treatment <- 1

#Create dataframe:
Survey_Unrel_1 <- unique(Survey_Unrel_1)
Survey_Unrel_1 <- as.data.frame(Survey_Unrel_1)

#Merge Treatment and Control data
New_Data <- rbind(Survey_Unrel_1,Control_Data)
#Remove NA variables:
New_Data <- na.omit(New_Data)

#Run Model with clustered standard errors:
fit_2_1 = glm(Seven_Ordinal ~ Treatment + Ideo_Congruence + Age + Gender + Education_Score + Income_Score + Article_day,data=New_Data)
lin_results_fit_2_1 = coeftest(fit_2_1, vcov. = vcovCL(fit_2_1, cluster = list(New_Data$ResponseId,New_Data$Article_day), type = "HC0"))

#Run Model with clustered standard errors:
fit_2_2 = glm(Four_Ordinal ~ Treatment + Ideo_Congruence + Age + Gender + Education_Score + Income_Score + Article_day,data=New_Data)
lin_results_fit_2_2 = coeftest(fit_2_2, vcov. = vcovCL(fit_2_2, cluster = list(New_Data$ResponseId,New_Data$Article_day), type = "HC0"))

#Run Model with clustered standard errors:
fit_2_3 = glm(True_Dummy ~ Treatment + Ideo_Congruence + Age + Gender + Education_Score + Income_Score  + Article_day,data=New_Data)
lin_results_fit_2_3 = coeftest(fit_2_3, vcov. = vcovCL(fit_2_3, cluster = list(New_Data$ResponseId,New_Data$Article_day), type = "HC0"))

#New dataframe:
Model_Data_7 <- New_Data

#Unique Articles:
Model_Data_7$Article_day <- as.character(Model_Data_7$Article_day)
Model_Data_7$Article_day <- paste0('Article_day',Model_Data_7$Article_day)
Omit_c <- unique(Model_Data_7$Article_day)
full_omit <- paste(Omit_c, collapse = '|')

#Coefficient Names:
Coef_names= c('(Intercept)','Treatment','Age','Ideological Congruence','Education','Gender','Income')

#Write Table
texreg(list(lin_results_fit_2_3,lin_results_fit_2_1,lin_results_fit_2_2),
       include.ci = FALSE,
       digits=4,
       omit.coef = full_omit,
       caption= 'Results from OLS Regression Results Presented in Figure 2d (0-25 Percentage Quartile of News Quality)',
       label = "table",
       custom.coef.names = Coef_names,
       custom.model.names= c("Categorical Measure","4-Point Ordinal Scale","7-Point Ordinal Scale"),
       float.pos = "!htbp",
       file='./Tables/Figure_2d_4.txt',
       caption.above = TRUE)

#Filter by quartile:
Survey_Unrel_1 <- Survey_Unrel %>% filter(Mean_All_Scores >= quantile(Survey_Unrel$Mean_All_Scores,na.rm=T)[2] & Mean_All_Scores < quantile(Survey_Unrel$Mean_All_Scores,na.rm=T)[3])


#Select variables
Survey_Unrel_1 <- Survey_Unrel_1 %>% select(Article_day,True_Dummy,Four_Ordinal,Seven_Ordinal,Age,Gender,ResponseId,Education_Score,Income_Score,Ideo_Congruence)
Survey_Unrel_1$Treatment <- 1

#Create dataframe:
Survey_Unrel_1 <- unique(Survey_Unrel_1)
Survey_Unrel_1 <- as.data.frame(Survey_Unrel_1)

#Merge Treatment and Control data
New_Data <- rbind(Survey_Unrel_1,Control_Data)
#Remove NA variables:
New_Data <- na.omit(New_Data)


#Run Model with clustered standard errors:
fit_2_1 = glm(Seven_Ordinal ~ Treatment + Ideo_Congruence + Age + Gender + Education_Score + Income_Score + Article_day,data=New_Data)
lin_results_fit_2_1 = coeftest(fit_2_1, vcov. = vcovCL(fit_2_1, cluster = list(New_Data$ResponseId,New_Data$Article_day), type = "HC0"))

#Run Model with clustered standard errors:
fit_2_2 = glm(Four_Ordinal ~ Treatment + Ideo_Congruence + Age + Gender + Education_Score + Income_Score + Article_day,data=New_Data)
lin_results_fit_2_2 = coeftest(fit_2_2, vcov. = vcovCL(fit_2_2, cluster = list(New_Data$ResponseId,New_Data$Article_day), type = "HC0"))

#Run Model with clustered standard errors:
fit_2_3 = glm(True_Dummy ~ Treatment + Ideo_Congruence + Age + Gender + Education_Score + Income_Score  + Article_day,data=New_Data)
lin_results_fit_2_3 = coeftest(fit_2_3, vcov. = vcovCL(fit_2_3, cluster = list(New_Data$ResponseId,New_Data$Article_day), type = "HC0"))

#New dataframe:
Model_Data_7 <- New_Data

#Unique Articles:
Model_Data_7$Article_day <- as.character(Model_Data_7$Article_day)
Model_Data_7$Article_day <- paste0('Article_day',Model_Data_7$Article_day)
Omit_c <- unique(Model_Data_7$Article_day)
full_omit <- paste(Omit_c, collapse = '|')

#Coefficient Names:
Coef_names= c('(Intercept)','Treatment','Age','Ideological Congruence','Education','Gender','Income')

#Write Table
texreg(list(lin_results_fit_2_3,lin_results_fit_2_1,lin_results_fit_2_2),
       include.ci = FALSE,
       digits=4,
       omit.coef = full_omit,
       caption= 'Results from OLS Regression Results Presented in Figure 2d (25-50 Percentage Quartile of News Quality)',
       label = "table",
       custom.coef.names = Coef_names,
       custom.model.names= c("Categorical Measure","4-Point Ordinal Scale","7-Point Ordinal Scale"),
       float.pos = "!htbp",
       file='./Tables/Figure_2d_3.txt',
       caption.above = TRUE)



#Filter by quartile:
Survey_Unrel_1 <- Survey_Unrel %>% filter(Mean_All_Scores >= quantile(Survey_Unrel$Mean_All_Scores,na.rm=T)[3] & Mean_All_Scores < quantile(Survey_Unrel$Mean_All_Scores,na.rm=T)[4])

#Select variables
Survey_Unrel_1 <- Survey_Unrel_1 %>% select(Article_day,True_Dummy,Four_Ordinal,Seven_Ordinal,Age,Gender,ResponseId,Education_Score,Income_Score,Ideo_Congruence)
Survey_Unrel_1$Treatment <- 1

#Create dataframe:
Survey_Unrel_1 <- unique(Survey_Unrel_1)
Survey_Unrel_1 <- as.data.frame(Survey_Unrel_1)

#Merge Treatment and Control data
New_Data <- rbind(Survey_Unrel_1,Control_Data)
#Remove NA variables:
New_Data <- na.omit(New_Data)

#Run Model with clustered standard errors:
fit_2_1 = glm(Seven_Ordinal ~ Treatment + Ideo_Congruence + Age + Gender + Education_Score + Income_Score + Article_day,data=New_Data)
lin_results_fit_2_1 = coeftest(fit_2_1, vcov. = vcovCL(fit_2_1, cluster = list(New_Data$ResponseId,New_Data$Article_day), type = "HC0"))

#Run Model with clustered standard errors:
fit_2_2 = glm(Four_Ordinal ~ Treatment + Ideo_Congruence + Age + Gender + Education_Score + Income_Score + Article_day,data=New_Data)
lin_results_fit_2_2 = coeftest(fit_2_2, vcov. = vcovCL(fit_2_2, cluster = list(New_Data$ResponseId,New_Data$Article_day), type = "HC0"))

#Run Model with clustered standard errors:
fit_2_3 = glm(True_Dummy ~ Treatment + Ideo_Congruence + Age + Gender + Education_Score + Income_Score  + Article_day,data=New_Data)
lin_results_fit_2_3 = coeftest(fit_2_3, vcov. = vcovCL(fit_2_3, cluster = list(New_Data$ResponseId,New_Data$Article_day), type = "HC0"))

#New dataframe:
Model_Data_7 <- New_Data

#Unique Articles:
Model_Data_7$Article_day <- as.character(Model_Data_7$Article_day)
Model_Data_7$Article_day <- paste0('Article_day',Model_Data_7$Article_day)
Omit_c <- unique(Model_Data_7$Article_day)
full_omit <- paste(Omit_c, collapse = '|')

#Coefficient Names:
Coef_names= c('(Intercept)','Treatment','Age','Ideological Congruence','Education','Gender','Income')

#Write Table
texreg(list(lin_results_fit_2_3,lin_results_fit_2_1,lin_results_fit_2_2),
       include.ci = FALSE,
       digits=4,
       omit.coef = full_omit,
       caption= 'Results from OLS Regression Results Presented in Figure 2d (50-75 Percentage Quartile of News Quality)',
       label = "table",
       custom.coef.names = Coef_names,
       custom.model.names= c("Categorical Measure","4-Point Ordinal Scale","7-Point Ordinal Scale"),
       float.pos = "!htbp",
       file='./Tables/Figure_2d_2.txt',
       caption.above = TRUE)

#Filter by quartile:
Survey_Unrel_1 <- Survey_Unrel %>% filter(Mean_All_Scores >= quantile(Survey_Unrel$Mean_All_Scores,na.rm=T)[4] & Mean_All_Scores <= 100)

#Select variables
Survey_Unrel_1 <- Survey_Unrel_1 %>% select(Article_day,True_Dummy,Four_Ordinal,Seven_Ordinal,Age,Gender,ResponseId,Education_Score,Income_Score,Ideo_Congruence)
Survey_Unrel_1$Treatment <- 1

#Create dataframe:
Survey_Unrel_1 <- unique(Survey_Unrel_1)
Survey_Unrel_1 <- as.data.frame(Survey_Unrel_1)

#Merge Treatment and Control data
New_Data <- rbind(Survey_Unrel_1,Control_Data)
#Remove NA variables:
New_Data <- na.omit(New_Data)


#Run Model with clustered standard errors:
fit_2_1 = glm(Seven_Ordinal ~ Treatment + Ideo_Congruence + Age + Gender + Education_Score + Income_Score + Article_day,data=New_Data)
lin_results_fit_2_1 = coeftest(fit_2_1, vcov. = vcovCL(fit_2_1, cluster = list(New_Data$ResponseId,New_Data$Article_day), type = "HC0"))

#Run Model with clustered standard errors:
fit_2_2 = glm(Four_Ordinal ~ Treatment + Ideo_Congruence + Age + Gender + Education_Score + Income_Score + Article_day,data=New_Data)
lin_results_fit_2_2 = coeftest(fit_2_2, vcov. = vcovCL(fit_2_2, cluster = list(New_Data$ResponseId,New_Data$Article_day), type = "HC0"))

#Run Model with clustered standard errors:
fit_2_3 = glm(True_Dummy ~ Treatment + Ideo_Congruence + Age + Gender + Education_Score + Income_Score  + Article_day,data=New_Data)
lin_results_fit_2_3 = coeftest(fit_2_3, vcov. = vcovCL(fit_2_3, cluster = list(New_Data$ResponseId,New_Data$Article_day), type = "HC0"))

#New dataframe:
Model_Data_7 <- New_Data

#Unique Articles:
Model_Data_7$Article_day <- as.character(Model_Data_7$Article_day)
Model_Data_7$Article_day <- paste0('Article_day',Model_Data_7$Article_day)
Omit_c <- unique(Model_Data_7$Article_day)
full_omit <- paste(Omit_c, collapse = '|')

#Coefficient Names:
Coef_names= c('(Intercept)','Treatment','Age','Ideological Congruence','Education','Gender','Income')

#Write Table
texreg(list(lin_results_fit_2_3,lin_results_fit_2_1,lin_results_fit_2_2),
       include.ci = FALSE,
       digits=4,
       omit.coef = full_omit,
       caption= 'Results from OLS Regression Results Presented in Figure 2d (75-100 Percentage Quartile of News Quality)',
       label = "table",
       custom.coef.names = Coef_names,
       custom.model.names= c("Categorical Measure","4-Point Ordinal Scale","7-Point Ordinal Scale"),
       float.pos = "!htbp",
       file='./Tables/Figure_2d_1.txt',
       caption.above = TRUE)



#Create dataframe with this basic data:
Combined_GS_Survey_Data <- Survey_Unrel

#Create string list of news sites scores:
Survey_Unrel$List_Scores <- as.character(Survey_Unrel$List_Scores)
Survey_Unrel$List_Scores <- gsub('\\[|\\]','',Survey_Unrel$List_Scores)
Survey_Unrel$List_Scores <- gsub('\\[|\\]','',Survey_Unrel$List_Scores)
Survey_Unrel <- Survey_Unrel %>% filter(FC_Eval == 'FM')



#Create Average Score by Respondent:
Survey_Evaluations <- Survey_Unrel %>% filter(!is.na(Mean_Score))
Survey_Evaluations <- Survey_Evaluations %>% select(ResponseId,Article_day,List_Scores)
Respondent_Evaluations <- Survey_Evaluations %>% select(ResponseId,Article_day)
Respondent_Evaluations <- unique(Respondent_Evaluations)

#Create New Matrix:
Search_Results_DF <- matrix(ncol=4)

colnames(Search_Results_DF) <- c('ResponseId',
                                 'Article_day',
                                 'Mean_Score_Final',
                                 'Prop_Unreliable_Final')

#For loop to create measure of expoisure to search engine results:
for(i in 1:nrow(Respondent_Evaluations)){
  Resp <- Respondent_Evaluations$ResponseId[i]
  Article <- Respondent_Evaluations$Article_day[i]  
  df_survey <- Survey_Evaluations %>% filter(ResponseId == Resp & Article_day == Article)
  All_Scores <- paste(df_survey$List_Scores,collapse=', ')
  All_Scores <- unlist(strsplit(All_Scores, split=", "))
  All_Scores <- as.numeric(All_Scores)
  Mean_Reliability <- mean(All_Scores)
  Total_links <- length(All_Scores)
  Total_Unrel <- 0
  for(x in 1:length(All_Scores)){
    if(All_Scores[x] < 60){
      Total_Unrel = Total_Unrel + 1
    }
  }
  Proportion_Unrel = Total_Unrel/Total_links
  
  new_df <- matrix(c(Resp,Article,Mean_Reliability,Proportion_Unrel),ncol=4)
  colnames(new_df) <- c('ResponseId',
                        'Article_day',
                        'Mean_Score_Final',
                        'Prop_Unreliable_Final')
  
  
  Search_Results_DF <- rbind(Search_Results_DF,
                             new_df)
}

#Create mean and proportion of unreliable news sites exposed:
Search_Results_DF <- as.data.frame(Search_Results_DF)
Search_Results_DF$Mean_Score_Final <- as.character(Search_Results_DF$Mean_Score_Final)
Search_Results_DF$Mean_Score_Final <- as.numeric(Search_Results_DF$Mean_Score_Final)
Search_Results_DF$Prop_Unreliable_Final <- as.character(Search_Results_DF$Prop_Unreliable_Final)
Search_Results_DF$Prop_Unreliable_Final <- as.numeric(Search_Results_DF$Prop_Unreliable_Final)

#Pull treatment data and merge google search engine results:
Survey_Unrel_1 <- Survey_Unrel %>% select(Article_day,True_Dummy,Four_Ordinal,Seven_Ordinal,Age,Gender,ResponseId,Education_Score,Income_Score,Ideo_Congruence,Dummy_Ideology,Article_Lean,Dig_Lit_Score)
Survey_Unrel_1 <- unique(Survey_Unrel_1)
Search_Survey <- merge(Survey_Unrel_1,Search_Results_DF,by=c('ResponseId','Article_day'))
Search_Survey$Unreliable_Dummy <- ifelse(Search_Survey$Prop_Unreliable_Final > 0,1,0)

#Run model with clustered standard errors:
fit_2_3 = glm(Unreliable_Dummy ~ Age + Gender + Education_Score + Income_Score + Ideo_Congruence +Dig_Lit_Score + Article_day,data=Search_Survey)
Prop_Dummy_results = coeftest(fit_2_3, vcov. = vcovCL(fit_2_3, cluster = list(Search_Survey$Article_day,Search_Survey$ResponseId), type = "HC0"))

#New Dataframe:
Model_Data_7 <- Search_Survey

#Unique Articles:
Model_Data_7$Article_day <- as.character(Model_Data_7$Article_day)
Model_Data_7$Article_day <- paste0('Article_day',Model_Data_7$Article_day)
Omit_c <- unique(Model_Data_7$Article_day)
full_omit <- paste(Omit_c, collapse = '|')

#Coefficient Names:
Coef_names= c('(Intercept)','Treatment','Age','Ideological Congruence','Education','Gender','Income')

#Write Table
texreg(list(Prop_Dummy_results),
       include.ci = FALSE,
       digits=4,
       omit.coef = full_omit,
       caption= 'Predicted Exposure to Unreliable News Sources when Searching for Information',
       label = "table",
       custom.coef.names = Coef_names,
       custom.model.names= c("Exposure to Unreliable News Site"),
       float.pos = "!htbp",
       file='./Tables/Figure_3.txt',
       caption.above = TRUE)


##############################################################################################################

#Digital Literacy

##############################################################################################################



#Treatment Only - Subset By Quality of Google Results:
Survey_Unrel <- Combined_GS_Survey_Data

#Create string with reliability scores:
Survey_Unrel$List_Scores <- as.character(Survey_Unrel$List_Scores)
Survey_Unrel$List_Scores <- gsub('\\[|\\]','',Survey_Unrel$List_Scores)
Survey_Unrel$List_Scores <- gsub('\\[|\\]','',Survey_Unrel$List_Scores)
Survey_Unrel <- Survey_Unrel %>% filter(FC_Eval == 'FM')

#Create dummy variable with Google Search Results
Survey_Unrel$ALL_URLS <- as.character(Survey_Unrel$ALL_URLS)
Survey_Unrel$GS_Results <- ifelse(nchar(Survey_Unrel$ALL_URLS) > 2,1,0)
Survey_Unrel <- Survey_Unrel %>% filter(GS_Results == 1)


#Create Variables with exposure varaibles:

#New Blank matrix
Final_Mat <- matrix(ncol=4)

#For loop that calculates variables:
Only_Rel_URLs <- c()
Some_Unrel_URLs <- c()
Total_Rel_Sources <- c()
Total_Unrel_Sources <- c()
Total_Sources <- c()
for(i in 1:nrow(Survey_Unrel)){
  All_Scores <- unlist(strsplit(Survey_Unrel$List_Scores[i], split=", "))
  All_Scores <- as.numeric(All_Scores)
  Rel_Sources = 0
  Unrel_Sources = 0
  Tot_Sources = 0
  if(length(All_Scores) == 0){
    All_Scores = NA} else{
      for(x in 1:length(All_Scores)){
        Tot_Sources = Tot_Sources + 1
        if(All_Scores[x] > 85){
          Rel_Sources = Rel_Sources + 1
        } else{
          if(All_Scores[x] < 60){
            Unrel_Sources = Unrel_Sources + 1
          }
        }
      } 
      if(Rel_Sources > 0 & Rel_Sources == Tot_Sources){
        Only_Rel_Sources = 1
      } else{
        Only_Rel_Sources = 0
      }
      if(Unrel_Sources > 0){
        Some_Unrel_Sources = 1
      } else{
        Some_Unrel_Sources = 0
      }
    }
  Only_Rel_URLs = c(Only_Rel_URLs,Only_Rel_Sources)
  Some_Unrel_URLs = c(Some_Unrel_URLs,Some_Unrel_Sources)
  Total_Rel_Sources = c(Total_Rel_Sources,Rel_Sources)
  Total_Unrel_Sources = c(Total_Unrel_Sources,Unrel_Sources)
  Total_Sources = c(Total_Sources,Tot_Sources)
}

#Add variables to dataframe:
Survey_Unrel$Only_Rel_URLs <- Only_Rel_URLs
Survey_Unrel$Some_Unrel_URLs <- Some_Unrel_URLs
Survey_Unrel$Total_Rel_Sources <- Total_Rel_Sources
Survey_Unrel$Total_Unrel_Sources <- Total_Unrel_Sources
Survey_Unrel$Total_Sources <- Total_Sources


#Filter treatment data by those that were exposed to unreliable news sites:
Survey_Unrel_1 <- Survey_Unrel %>% filter(Some_Unrel_URLs == 1)
Survey_Unrel_1 <- Survey_Unrel_1 %>% select(Article_day,True_Dummy,Four_Ordinal,Seven_Ordinal,Age,Gender,ResponseId,Education_Score,Income_Score,Ideo_Congruence,Dummy_Ideology,Article_Lean,Dig_Lit_Score)
Survey_Unrel_1$Treatment <- 1
Survey_Unrel_1 <- unique(Survey_Unrel_1)
Survey_Unrel_1 <- as.data.frame(Survey_Unrel_1)

#Pull control data:
Control_Data <- read.csv('./Data/Control_Data_Study_5.csv')

#Merge data
Control_Data <- merge(Control_Data,FC_Ideo_Data,by='Article_day')

#Create Ideological Perspective data:
Control_Data <- Control_Data %>% mutate(Dummy_Ideology = ifelse(Ideology_Score > 0,'Conservative','Moderate'))
Control_Data <- Control_Data %>% mutate(Dummy_Ideology = ifelse(Ideology_Score < 0,'Liberal',Dummy_Ideology))
Control_Data <- Control_Data %>% mutate(Ideo_Congruence = ifelse(Dummy_Ideology == Article_Lean,1,0))

#Only keep those who had the web-tracking extension on while they evaluated articles:
Control_Data$Article_day <- as.character(Control_Data$Article_day)
Control_WT_Data <- read.csv('./Data/output_Control_Survey_2.csv')
Control_WT_Data$Article_day <- as.character(Control_WT_Data$Article_day)
Control_WT_Data$X <- NULL
Control_WT_Data <- unique(Control_WT_Data)
colnames(Control_WT_Data)[1] <- 'ResponseId'
Control_Data <- merge(Control_Data,Control_WT_Data,by=c('ResponseId','Article_day'))

#Filter false/misleading articles:
Control_Data <- Control_Data %>% filter(FC_Eval == 'FM')
Control_Data <- Control_Data %>% select(Article_day,True_Dummy,Four_Ordinal,Seven_Ordinal,Age,Gender,ResponseId,Education_Score,Income_Score,Ideo_Congruence,Dummy_Ideology,Article_Lean,Dig_Lit_Score)
Control_Data$Treatment <- 0

#Merge data
New_Data <- rbind(Survey_Unrel_1,Control_Data)

#Filter those with high levels of digital literacy
New_Data <- New_Data %>% filter(Dig_Lit_Score >= median(New_Data$Dig_Lit_Score))

#Remove NAs:
New_Data <- na.omit(New_Data)

#Run Model with clustered standard errors:
fit_2_1 = glm(Seven_Ordinal ~ Treatment + Ideo_Congruence + Age + Gender + Education_Score + Income_Score + Article_day,data=New_Data)
lin_results_fit_2_1 = coeftest(fit_2_1, vcov. = vcovCL(fit_2_1, cluster = list(New_Data$ResponseId,New_Data$Article_day), type = "HC0"))

#Run Model with clustered standard errors:
fit_2_2 = glm(Four_Ordinal ~ Treatment + Ideo_Congruence + Age + Gender + Education_Score + Income_Score + Article_day,data=New_Data)
lin_results_fit_2_2 = coeftest(fit_2_2, vcov. = vcovCL(fit_2_2, cluster = list(New_Data$ResponseId,New_Data$Article_day), type = "HC0"))

#Run Model with clustered standard errors:
fit_2_3 = glm(True_Dummy ~ Treatment + Ideo_Congruence + Age + Gender + Education_Score + Income_Score  + Article_day,data=New_Data)
lin_results_fit_2_3 = coeftest(fit_2_3, vcov. = vcovCL(fit_2_3, cluster = list(New_Data$ResponseId,New_Data$Article_day), type = "HC0"))

#New dataframe:
Model_Data_7 <- New_Data

#Unique Articles:
Model_Data_7$Article_day <- as.character(Model_Data_7$Article_day)
Model_Data_7$Article_day <- paste0('Article_day',Model_Data_7$Article_day)
Omit_c <- unique(Model_Data_7$Article_day)
full_omit <- paste(Omit_c, collapse = '|')

#Coefficient Names:
Coef_names= c('(Intercept)','Treatment','Age','Ideological Congruence','Education','Gender','Income')

#Write Table
texreg(list(lin_results_fit_2_3,lin_results_fit_2_2,lin_results_fit_2_1),
       include.ci = FALSE,
       digits=4,
       omit.coef = full_omit,
       caption= 'Results from OLS Regression Results Presented in Figure 4b (Some Very Unreliable News Returned)',
       label = "table",
       custom.coef.names = Coef_names,
       custom.model.names= c("Categorical Measure","4-Point Ordinal Scale","7-Point Ordinal Scale"),
       float.pos = "!htbp",
       file='./Tables/Figure_4b_1.txt',
       caption.above = TRUE)

#Filter treatment data by those that were exposed to only reliable news sites:
Survey_Unrel_1 <- Survey_Unrel %>% filter(Only_Rel_URLs == 1)
Survey_Unrel_1 <- Survey_Unrel_1 %>% select(Article_day,True_Dummy,Four_Ordinal,Seven_Ordinal,Age,Gender,ResponseId,Education_Score,Income_Score,Ideo_Congruence,Dummy_Ideology,Article_Lean,Dig_Lit_Score)
Survey_Unrel_1$Treatment <- 1
Survey_Unrel_1 <- unique(Survey_Unrel_1)
Survey_Unrel_1 <- as.data.frame(Survey_Unrel_1)

#Merge data
New_Data <- rbind(Survey_Unrel_1,Control_Data)

#Filter those with high levels of digital literacy
New_Data <- New_Data %>% filter(Dig_Lit_Score >= median(New_Data$Dig_Lit_Score))

#Remove NAs:
New_Data <- na.omit(New_Data)

#Run Model with clustered standard errors:
fit_2_1 = glm(Seven_Ordinal ~ Treatment + Ideo_Congruence + Age + Gender + Education_Score + Income_Score + Article_day,data=New_Data)
lin_results_fit_2_1 = coeftest(fit_2_1, vcov. = vcovCL(fit_2_1, cluster = list(New_Data$ResponseId,New_Data$Article_day), type = "HC0"))

#Run Model with clustered standard errors:
fit_2_2 = glm(Four_Ordinal ~ Treatment + Ideo_Congruence + Age + Gender + Education_Score + Income_Score + Article_day,data=New_Data)
lin_results_fit_2_2 = coeftest(fit_2_2, vcov. = vcovCL(fit_2_2, cluster = list(New_Data$ResponseId,New_Data$Article_day), type = "HC0"))

#Run Model with clustered standard errors:
fit_2_3 = glm(True_Dummy ~ Treatment + Ideo_Congruence + Age + Gender + Education_Score + Income_Score  + Article_day,data=New_Data)
lin_results_fit_2_3 = coeftest(fit_2_3, vcov. = vcovCL(fit_2_3, cluster = list(New_Data$ResponseId,New_Data$Article_day), type = "HC0"))

#New dataframe:
Model_Data_7 <- New_Data

#Unique Articles:
Model_Data_7$Article_day <- as.character(Model_Data_7$Article_day)
Model_Data_7$Article_day <- paste0('Article_day',Model_Data_7$Article_day)
Omit_c <- unique(Model_Data_7$Article_day)
full_omit <- paste(Omit_c, collapse = '|')

#Coefficient Names:
Coef_names= c('(Intercept)','Treatment','Age','Ideological Congruence','Education','Gender','Income')

#Write Table
texreg(list(lin_results_fit_2_3,lin_results_fit_2_2,lin_results_fit_2_1),
       include.ci = FALSE,
       digits=4,
       omit.coef = full_omit,
       caption= 'Results from OLS Regression Results Presented in Figure 4b (Only Very Reliable News Returned)',
       label = "table",
       custom.coef.names = Coef_names,
       custom.model.names= c("Categorical Measure","4-Point Ordinal Scale","7-Point Ordinal Scale"),
       float.pos = "!htbp",
       file='./Tables/Figure_4b_2.txt',
       caption.above = TRUE)


#Filter treatment data by those that were exposed to unreliable news sites:
Survey_Unrel_1 <- Survey_Unrel %>% filter(Some_Unrel_URLs == 1)
Survey_Unrel_1 <- Survey_Unrel_1 %>% select(Article_day,True_Dummy,Four_Ordinal,Seven_Ordinal,Age,Gender,ResponseId,Education_Score,Income_Score,Ideo_Congruence,Dummy_Ideology,Article_Lean,Dig_Lit_Score)
Survey_Unrel_1$Treatment <- 1
Survey_Unrel_1 <- unique(Survey_Unrel_1)
Survey_Unrel_1 <- as.data.frame(Survey_Unrel_1)

#Merge data
New_Data <- rbind(Survey_Unrel_1,Control_Data)

#Filter those with high levels of digital literacy
New_Data <- New_Data %>% filter(Dig_Lit_Score < median(New_Data$Dig_Lit_Score))

#Remove NAs:
New_Data <- na.omit(New_Data)

#Run Model with clustered standard errors:
fit_2_1 = glm(Seven_Ordinal ~ Treatment + Ideo_Congruence + Age + Gender + Education_Score + Income_Score + Article_day,data=New_Data)
lin_results_fit_2_1 = coeftest(fit_2_1, vcov. = vcovCL(fit_2_1, cluster = list(New_Data$ResponseId,New_Data$Article_day), type = "HC0"))

#Run Model with clustered standard errors:
fit_2_2 = glm(Four_Ordinal ~ Treatment + Ideo_Congruence + Age + Gender + Education_Score + Income_Score + Article_day,data=New_Data)
lin_results_fit_2_2 = coeftest(fit_2_2, vcov. = vcovCL(fit_2_2, cluster = list(New_Data$ResponseId,New_Data$Article_day), type = "HC0"))

#Run Model with clustered standard errors:
fit_2_3 = glm(True_Dummy ~ Treatment + Ideo_Congruence + Age + Gender + Education_Score + Income_Score  + Article_day,data=New_Data)
lin_results_fit_2_3 = coeftest(fit_2_3, vcov. = vcovCL(fit_2_3, cluster = list(New_Data$ResponseId,New_Data$Article_day), type = "HC0"))

#New dataframe:
Model_Data_7 <- New_Data

#Unique Articles:
Model_Data_7$Article_day <- as.character(Model_Data_7$Article_day)
Model_Data_7$Article_day <- paste0('Article_day',Model_Data_7$Article_day)
Omit_c <- unique(Model_Data_7$Article_day)
full_omit <- paste(Omit_c, collapse = '|')

#Coefficient Names:
Coef_names= c('(Intercept)','Treatment','Age','Ideological Congruence','Education','Gender','Income')

#Write Table
texreg(list(lin_results_fit_2_3,lin_results_fit_2_2,lin_results_fit_2_1),
       include.ci = FALSE,
       digits=4,
       omit.coef = full_omit,
       caption= 'Results from OLS Regression Results Presented in Figure 4a (Some Very Unreliable News Returned)',
       label = "table",
       custom.coef.names = Coef_names,
       custom.model.names= c("Categorical Measure","4-Point Ordinal Scale","7-Point Ordinal Scale"),
       float.pos = "!htbp",
       file='./Tables/Figure_4a_1.txt',
       caption.above = TRUE)


#Filter treatment data by those that were exposed to only reliable news sites:
Survey_Unrel_1 <- Survey_Unrel %>% filter(Only_Rel_URLs == 1)
Survey_Unrel_1 <- Survey_Unrel_1 %>% select(Article_day,True_Dummy,Four_Ordinal,Seven_Ordinal,Age,Gender,ResponseId,Education_Score,Income_Score,Ideo_Congruence,Dummy_Ideology,Article_Lean,Dig_Lit_Score)
Survey_Unrel_1$Treatment <- 1
Survey_Unrel_1 <- unique(Survey_Unrel_1)
Survey_Unrel_1 <- as.data.frame(Survey_Unrel_1)

#Merge data
New_Data <- rbind(Survey_Unrel_1,Control_Data)

#Filter those with high levels of digital literacy
New_Data <- New_Data %>% filter(Dig_Lit_Score < median(New_Data$Dig_Lit_Score))

#Remove NAs:
New_Data <- na.omit(New_Data)

#Run Model with clustered standard errors:
fit_2_1 = glm(Seven_Ordinal ~ Treatment + Ideo_Congruence + Age + Gender + Education_Score + Income_Score + Article_day,data=New_Data)
lin_results_fit_2_1 = coeftest(fit_2_1, vcov. = vcovCL(fit_2_1, cluster = list(New_Data$ResponseId,New_Data$Article_day), type = "HC0"))

#Run Model with clustered standard errors:
fit_2_2 = glm(Four_Ordinal ~ Treatment + Ideo_Congruence + Age + Gender + Education_Score + Income_Score + Article_day,data=New_Data)
lin_results_fit_2_2 = coeftest(fit_2_2, vcov. = vcovCL(fit_2_2, cluster = list(New_Data$ResponseId,New_Data$Article_day), type = "HC0"))

#Run Model with clustered standard errors:
fit_2_3 = glm(True_Dummy ~ Treatment + Ideo_Congruence + Age + Gender + Education_Score + Income_Score  + Article_day,data=New_Data)
lin_results_fit_2_3 = coeftest(fit_2_3, vcov. = vcovCL(fit_2_3, cluster = list(New_Data$ResponseId,New_Data$Article_day), type = "HC0"))

#New dataframe:
Model_Data_7 <- New_Data

#Unique Articles:
Model_Data_7$Article_day <- as.character(Model_Data_7$Article_day)
Model_Data_7$Article_day <- paste0('Article_day',Model_Data_7$Article_day)
Omit_c <- unique(Model_Data_7$Article_day)
full_omit <- paste(Omit_c, collapse = '|')

#Coefficient Names:
Coef_names= c('(Intercept)','Treatment','Age','Ideological Congruence','Education','Gender','Income')

#Write Table
texreg(list(lin_results_fit_2_3,lin_results_fit_2_2,lin_results_fit_2_1),
       include.ci = FALSE,
       digits=4,
       omit.coef = full_omit,
       caption= 'Results from OLS Regression Results Presented in Figure 4a (Only Very Reliable News Returned)',
       label = "table",
       custom.coef.names = Coef_names,
       custom.model.names= c("Categorical Measure","4-Point Ordinal Scale","7-Point Ordinal Scale"),
       float.pos = "!htbp",
       file='./Tables/Figure_4a_2.txt',
       caption.above = TRUE)


####################################################################################################################





####################################################################################################################




#Filter treatment data by those that were exposed to unreliable news sites:
Survey_Unrel_1 <- Survey_Unrel %>% filter(Some_Unrel_URLs == 1)
Survey_Unrel_1 <- Survey_Unrel_1 %>% select(Article_day,True_Dummy,Four_Ordinal,Seven_Ordinal,Age,Gender,ResponseId,Education_Score,Income_Score,Ideo_Congruence,Dummy_Ideology,Article_Lean,Dig_Lit_Score)
Survey_Unrel_1$Treatment <- 1
Survey_Unrel_1 <- unique(Survey_Unrel_1)
Survey_Unrel_1 <- as.data.frame(Survey_Unrel_1)

#Merge data
New_Data <- rbind(Survey_Unrel_1,Control_Data)

#Filter by Ideological Congruence:
New_Data <- New_Data %>% filter(Ideo_Congruence == 1)

#Remove NAs:
New_Data <- na.omit(New_Data)

#Run Model with clustered standard errors:
fit_2_1 = glm(Seven_Ordinal ~ Treatment + Age + Gender + Education_Score + Income_Score + Article_day,data=New_Data)
lin_results_fit_2_1 = coeftest(fit_2_1, vcov. = vcovCL(fit_2_1, cluster = list(New_Data$ResponseId,New_Data$Article_day), type = "HC0"))

#Run Model with clustered standard errors:
fit_2_2 = glm(Four_Ordinal ~ Treatment + Age + Gender + Education_Score + Income_Score + Article_day,data=New_Data)
lin_results_fit_2_2 = coeftest(fit_2_2, vcov. = vcovCL(fit_2_2, cluster = list(New_Data$ResponseId,New_Data$Article_day), type = "HC0"))

#Run Model with clustered standard errors:
fit_2_3 = glm(True_Dummy ~ Treatment + Age + Gender + Education_Score + Income_Score  + Article_day,data=New_Data)
lin_results_fit_2_3 = coeftest(fit_2_3, vcov. = vcovCL(fit_2_3, cluster = list(New_Data$ResponseId,New_Data$Article_day), type = "HC0"))

#New dataframe:
Model_Data_7 <- New_Data

#Unique Articles:
Model_Data_7$Article_day <- as.character(Model_Data_7$Article_day)
Model_Data_7$Article_day <- paste0('Article_day',Model_Data_7$Article_day)
Omit_c <- unique(Model_Data_7$Article_day)
full_omit <- paste(Omit_c, collapse = '|')

#Coefficient Names:
Coef_names= c('(Intercept)','Treatment','Age','Gender','Education','Income')

#Write Table
texreg(list(lin_results_fit_2_3,lin_results_fit_2_2,lin_results_fit_2_1),
       include.ci = FALSE,
       digits=4,
       omit.coef = full_omit,
       caption= 'Results from OLS Regression Results Presented in Figure 4c (Some Very Unreliable News Returned)',
       label = "table",
       custom.coef.names = Coef_names,
       custom.model.names= c("Categorical Measure","4-Point Ordinal Scale","7-Point Ordinal Scale"),
       float.pos = "!htbp",
       file='./Tables/Figure_4c_1.txt',
       caption.above = TRUE)


#Filter treatment data by those that were exposed to onyl very reliable news sites:
Survey_Unrel_1 <- Survey_Unrel %>% filter(Only_Rel_URLs == 1)
Survey_Unrel_1 <- Survey_Unrel_1 %>% select(Article_day,True_Dummy,Four_Ordinal,Seven_Ordinal,Age,Gender,ResponseId,Education_Score,Income_Score,Ideo_Congruence,Dummy_Ideology,Article_Lean,Dig_Lit_Score)
Survey_Unrel_1$Treatment <- 1
Survey_Unrel_1 <- unique(Survey_Unrel_1)
Survey_Unrel_1 <- as.data.frame(Survey_Unrel_1)

#Merge data
New_Data <- rbind(Survey_Unrel_1,Control_Data)

#Filter by Ideological Congruence:
New_Data <- New_Data %>% filter(Ideo_Congruence == 1)

#Remove NAs:
New_Data <- na.omit(New_Data)

#Run Model with clustered standard errors:
fit_2_1 = glm(Seven_Ordinal ~ Treatment + Age + Gender + Education_Score + Income_Score + Article_day,data=New_Data)
lin_results_fit_2_1 = coeftest(fit_2_1, vcov. = vcovCL(fit_2_1, cluster = list(New_Data$ResponseId,New_Data$Article_day), type = "HC0"))

#Run Model with clustered standard errors:
fit_2_2 = glm(Four_Ordinal ~ Treatment + Age + Gender + Education_Score + Income_Score + Article_day,data=New_Data)
lin_results_fit_2_2 = coeftest(fit_2_2, vcov. = vcovCL(fit_2_2, cluster = list(New_Data$ResponseId,New_Data$Article_day), type = "HC0"))

#Run Model with clustered standard errors:
fit_2_3 = glm(True_Dummy ~ Treatment + Age + Gender + Education_Score + Income_Score  + Article_day,data=New_Data)
lin_results_fit_2_3 = coeftest(fit_2_3, vcov. = vcovCL(fit_2_3, cluster = list(New_Data$ResponseId,New_Data$Article_day), type = "HC0"))

#New dataframe:
Model_Data_7 <- New_Data

#Unique Articles:
Model_Data_7$Article_day <- as.character(Model_Data_7$Article_day)
Model_Data_7$Article_day <- paste0('Article_day',Model_Data_7$Article_day)
Omit_c <- unique(Model_Data_7$Article_day)
full_omit <- paste(Omit_c, collapse = '|')

#Coefficient Names:
Coef_names= c('(Intercept)','Treatment','Age','Gender','Education','Income')

#Write Table
texreg(list(lin_results_fit_2_3,lin_results_fit_2_2,lin_results_fit_2_1),
       include.ci = FALSE,
       digits=4,
       omit.coef = full_omit,
       caption= 'Results from OLS Regression Results Presented in Figure 4c (Only Reliable News Returned)',
       label = "table",
       custom.coef.names = Coef_names,
       custom.model.names= c("Categorical Measure","4-Point Ordinal Scale","7-Point Ordinal Scale"),
       float.pos = "!htbp",
       file='./Tables/Figure_4c_2.txt',
       caption.above = TRUE)



#Filter treatment data by those that were exposed to unreliable news sites:
Survey_Unrel_1 <- Survey_Unrel %>% filter(Some_Unrel_URLs == 1)
Survey_Unrel_1 <- Survey_Unrel_1 %>% select(Article_day,True_Dummy,Four_Ordinal,Seven_Ordinal,Age,Gender,ResponseId,Education_Score,Income_Score,Ideo_Congruence,Dummy_Ideology,Article_Lean)
Survey_Unrel_1$Treatment <- 1
Survey_Unrel_1 <- unique(Survey_Unrel_1)
Survey_Unrel_1 <- as.data.frame(Survey_Unrel_1)


#Pull in control data:
Control_Data <- read.csv('./Data/Control_Data_Study_5.csv')
#Merge:
Control_Data <- merge(Control_Data,FC_Ideo_Data,by='Article_day')
#Create ideological congruence:
Control_Data <- Control_Data %>% mutate(Dummy_Ideology = ifelse(Ideology_Score > 0,'Conservative','Moderate'))
Control_Data <- Control_Data %>% mutate(Dummy_Ideology = ifelse(Ideology_Score < 0,'Liberal',Dummy_Ideology))
Control_Data <- Control_Data %>% mutate(Ideo_Congruence = ifelse(Dummy_Ideology == Article_Lean,1,0))
Control_Data$Article_day <- as.character(Control_Data$Article_day)

#Pull in web extension data:
Control_WT_Data <- read.csv('./Data/output_Control_Survey_2.csv')
Control_WT_Data$Article_day <- as.character(Control_WT_Data$Article_day)
Control_WT_Data$X <- NULL
Control_WT_Data <- unique(Control_WT_Data)
colnames(Control_WT_Data)[1] <- 'ResponseId'
#Merge:
Control_Data <- merge(Control_Data,Control_WT_Data,by=c('ResponseId','Article_day'))
#Filter by false articles:
Control_Data <- Control_Data %>% filter(FC_Eval == 'FM')
#Select data:
Control_Data <- Control_Data %>% select(Article_day,True_Dummy,Four_Ordinal,Seven_Ordinal,Age,Gender,ResponseId,Education_Score,Income_Score,Ideo_Congruence,Dummy_Ideology,Article_Lean)
Control_Data$Treatment <- 0

#Merge:
New_Data <- rbind(Survey_Unrel_1,Control_Data)

#Filter individuals with ideological incongruence:
New_Data_1 <- New_Data %>% filter(Article_Lean == 'Liberal' | Article_Lean ==  'Conservative')
New_Data_2 <- New_Data %>% filter(Article_Lean == 'Conservative' | Article_Lean ==  'Liberal')

#Merge control and treatment datra:
New_Data <- rbind(New_Data_1,New_Data_2)

#Remove NAs:
New_Data <- na.omit(New_Data)

#Run Model with clustered standard errors:
fit_2_1 = glm(Seven_Ordinal ~ Treatment + Age + Gender + Education_Score + Income_Score + Article_day,data=New_Data)
lin_results_fit_2_1 = coeftest(fit_2_1, vcov. = vcovCL(fit_2_1, cluster = list(New_Data$ResponseId,New_Data$Article_day), type = "HC0"))

#Run Model with clustered standard errors:
fit_2_2 = glm(Four_Ordinal ~ Treatment  + Age + Gender + Education_Score + Income_Score + Article_day,data=New_Data)
lin_results_fit_2_2 = coeftest(fit_2_2, vcov. = vcovCL(fit_2_2, cluster = list(New_Data$ResponseId,New_Data$Article_day), type = "HC0"))

#Run Model with clustered standard errors:
fit_2_3 = glm(True_Dummy ~ Treatment + Age + Gender + Education_Score + Income_Score  + Article_day,data=New_Data)
lin_results_fit_2_3 = coeftest(fit_2_3, vcov. = vcovCL(fit_2_3, cluster = list(New_Data$ResponseId,New_Data$Article_day), type = "HC0"))

#New dataframe:
Model_Data_7 <- New_Data

#Unique Articles:
Model_Data_7$Article_day <- as.character(Model_Data_7$Article_day)
Model_Data_7$Article_day <- paste0('Article_day',Model_Data_7$Article_day)
Omit_c <- unique(Model_Data_7$Article_day)
full_omit <- paste(Omit_c, collapse = '|')

#Coefficient Names:
Coef_names= c('(Intercept)','Treatment','Age','Gender','Education','Income')


#Write Table
texreg(list(lin_results_fit_2_3,lin_results_fit_2_2,lin_results_fit_2_1),
       include.ci = FALSE,
       digits=4,
       omit.coef = full_omit,
       caption= 'Results from OLS Regression Results Presented in Figure 4d (Some Very Unreliable News Returned)',
       label = "table",
       custom.coef.names = Coef_names,
       custom.model.names= c("Categorical Measure","4-Point Ordinal Scale","7-Point Ordinal Scale"),
       float.pos = "!htbp",
       file='./Tables/Figure_4d_1.txt',
       caption.above = TRUE)



#Filter treatment data by those that were exposed to only very reliable news sites:
Survey_Unrel_1 <- Survey_Unrel %>% filter(Only_Rel_URLs == 1)
Survey_Unrel_1 <- Survey_Unrel_1 %>% select(Article_day,True_Dummy,Four_Ordinal,Seven_Ordinal,Age,Gender,ResponseId,Education_Score,Income_Score,Ideo_Congruence,Dummy_Ideology,Article_Lean)
Survey_Unrel_1$Treatment <- 1
Survey_Unrel_1 <- unique(Survey_Unrel_1)
Survey_Unrel_1 <- as.data.frame(Survey_Unrel_1)

#Merge:
New_Data <- rbind(Survey_Unrel_1,Control_Data)

#Filter individuals with ideological incongruence:
New_Data_1 <- New_Data %>% filter(Article_Lean == 'Liberal' | Article_Lean ==  'Conservative')
New_Data_2 <- New_Data %>% filter(Article_Lean == 'Conservative' | Article_Lean ==  'Liberal')

#Merge control and treatment datra:
New_Data <- rbind(New_Data_1,New_Data_2)

#Remove NAs:
New_Data <- na.omit(New_Data)

#Run Model with clustered standard errors:
fit_2_1 = glm(Seven_Ordinal ~ Treatment + Age + Gender + Education_Score + Income_Score + Article_day,data=New_Data)
lin_results_fit_2_1 = coeftest(fit_2_1, vcov. = vcovCL(fit_2_1, cluster = list(New_Data$ResponseId,New_Data$Article_day), type = "HC0"))

#Run Model with clustered standard errors:
fit_2_2 = glm(Four_Ordinal ~ Treatment +  Age + Gender + Education_Score + Income_Score + Article_day,data=New_Data)
lin_results_fit_2_2 = coeftest(fit_2_2, vcov. = vcovCL(fit_2_2, cluster = list(New_Data$ResponseId,New_Data$Article_day), type = "HC0"))

#Run Model with clustered standard errors:
fit_2_3 = glm(True_Dummy ~ Treatment + Age + Gender + Education_Score + Income_Score  + Article_day,data=New_Data)
lin_results_fit_2_3 = coeftest(fit_2_3, vcov. = vcovCL(fit_2_3, cluster = list(New_Data$ResponseId,New_Data$Article_day), type = "HC0"))

#New dataframe:
Model_Data_7 <- New_Data

#Unique Articles:
Model_Data_7$Article_day <- as.character(Model_Data_7$Article_day)
Model_Data_7$Article_day <- paste0('Article_day',Model_Data_7$Article_day)
Omit_c <- unique(Model_Data_7$Article_day)
full_omit <- paste(Omit_c, collapse = '|')

#Coefficient Names:
Coef_names= c('(Intercept)','Treatment','Age','Gender','Education','Income')

#Write Table
texreg(list(lin_results_fit_2_3,lin_results_fit_2_2,lin_results_fit_2_1),
       include.ci = FALSE,
       digits=4,
       omit.coef = full_omit,
       caption= 'Results from OLS Regression Results Presented in Figure 4d (Only Reliable News Returned)',
       label = "table",
       custom.coef.names = Coef_names,
       custom.model.names= c("Categorical Measure","4-Point Ordinal Scale","7-Point Ordinal Scale"),
       float.pos = "!htbp",
       file='./Tables/Figure_4d_2.txt',
       caption.above = TRUE)


####################################################################################################################


#Supplementary Logistic Regression Results


####################################################################################################################

#Study 2 Analysis:
#Pull in this data:
Data_Bef_Aft <- read.csv('./Data/Data_Bef_Aft_Misl_False.csv')

Study_2_Ideo <- read.csv('./Data/Study_2_Respondent_Ideo.csv')

Data_Bef_Aft$ResponseId <- as.character(Data_Bef_Aft$ResponseId)
Study_2_Ideo$ResponseId <- as.character(Study_2_Ideo$ResponseId)

Study_2_Ideo <- unique(Study_2_Ideo)

Data_Bef_Aft <- merge(Data_Bef_Aft,Study_2_Ideo,by='ResponseId')


Data_Bef_Aft <- Data_Bef_Aft %>% mutate(Dummy_Ideology = ifelse(Ideology_Score > 0,'Conservative','Moderate'))
Data_Bef_Aft <- Data_Bef_Aft %>% mutate(Dummy_Ideology = ifelse(Ideology_Score < 0,'Liberal',Dummy_Ideology))

Article_data <- Data_Bef_Aft %>% mutate(Article_Lean = ifelse(Dummy_Ideology == 'Conservative' & Dummy_Congruence == 1,'Conservative','None'))
Article_data <- Article_data %>% mutate(Article_Lean = ifelse(Dummy_Ideology == 'Liberal' & Dummy_Congruence == 1,'Liberal',Article_Lean))

Article_data <- Article_data %>% select(Article_day,Article_Lean)

Article_data <- unique(Article_data)
Article_data <- na.omit(Article_data)

full_omit_2 <- c()

#Study 1:

#Study 1:
#Pull in this data: Search Experiment 1: Study 1:
Misl_False_Search <- read.csv('./Data/Search_Exp_Misl_False.csv')
#Select variables of interest:
Model_Data_7 <- Misl_False_Search %>% select(Likert_Evaluation,Susc_FN,Treat_Search,Education_Score,Age,Gender,Income_Score,Dummy_Congruence,Familiar_Dummy,Article_day,ResponseId)
#Remove NA values:
Model_Data_7 = na.omit(Model_Data_7)
Model_Data_7$Gender <- ifelse(Model_Data_7$Gender == 'Female',1,0)

#Run OLS Model with clustered standard errors:
fit_2_1 = glm(Susc_FN ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score + Article_day,family=binomial, data = Model_Data_7)
lin_results_fit_2_1 = coeftest(fit_2_1, vcov. = vcovCL(fit_2_1, cluster = list(Model_Data_7$ResponseId,Model_Data_7$Article_day), type = "HC0"))

#Unique Articles:
Model_Data_7$Article_day <- as.character(Model_Data_7$Article_day)
Model_Data_7$Article_day <- paste0('Article_day',Model_Data_7$Article_day)
Omit_c <- unique(Model_Data_7$Article_day)
full_omit <- paste(Omit_c, collapse = '|')

full_omit_2 <- c(full_omit_2,full_omit)



#Study 2:
#Pull in this data:
Data_Bef_Aft <- read.csv('./Data/Data_Bef_Aft_Misl_False.csv')

#Select variables of interest:
Model_Data_7 <- Data_Bef_Aft %>% select(Likert_Evaluation,Susc_FN,Treat_Search,Education_Score,Age,Gender,Income_Score,Dummy_Congruence,Familiar_Dummy,Article_day,ResponseId)
#Remove NA values:
Model_Data_7 <- na.omit(Model_Data_7)
Model_Data_7$Gender <- ifelse(Model_Data_7$Gender == 'Female',1,0)

#Run linear regression and produce coefficient values:
fit_2_1 = glm(Susc_FN ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score + Article_day,family=binomial, data = Model_Data_7)
lin_results_fit_2_2 = coeftest(fit_2_1, vcov. = vcovCL(fit_2_1, cluster = list(Model_Data_7$Article_day), type = "HC0"))

#Unique Articles:
Model_Data_7$Article_day <- as.character(Model_Data_7$Article_day)
Model_Data_7$Article_day <- paste0('Article_day',Model_Data_7$Article_day)
Omit_c <- unique(Model_Data_7$Article_day)
full_omit <- paste(Omit_c, collapse = '|')
full_omit_2 <- c(full_omit_2,full_omit)


#Study 3 Analysis:
#Load Data:
Latency_Data <- read.csv('./Data/Latency_FC_Data.csv')
Latency_Survey <- read.csv('./Data/Latency_Control_Survey.csv')

#Create Article_day data for purpose of merging
Latency_Survey <- Latency_Survey %>% mutate(Article_day = paste0(day,sep='_',Article))

#Select data needed:
Latency_Survey <- Latency_Survey %>% select(Evaluation,Likert_Evaluation,True_Likert_After_Info,Evaluation_After_Info,Age,Dig_Lit_Avg,Income_Score,CRT_Score,Familiar_Story,Education_Score,Duration,Article_day,Ideology_Score,Gender,ResponseId)

#Create Gender dummy variable
Latency_Survey$Gender <- as.character(Latency_Survey$Gender)
Latency_Survey$Gender <- ifelse(Latency_Survey$Gender == 'Female',1,0)


#Create Likert Score post-treatment:
Latency_Survey$True_Likert_After_Info <- as.character(Latency_Survey$True_Likert_After_Info)
Latency_Survey <- Latency_Survey %>% filter(True_Likert_After_Info != 'not asked')
Latency_Survey$True_Likert_After_Info <- substr(Latency_Survey$True_Likert_After_Info, start = 1, stop = 2)
Latency_Survey$True_Likert_After_Info <- as.numeric(Latency_Survey$True_Likert_After_Info)

#Create Likert Score pre-treatment:
Latency_Survey$Likert_Evaluation <- as.character(Latency_Survey$Likert_Evaluation)
Latency_Survey$Likert_Evaluation <- substr(Latency_Survey$Likert_Evaluation, start = 1, stop = 2)
Latency_Survey$Likert_Evaluation <- as.numeric(Latency_Survey$Likert_Evaluation)

#Create True Dummy variable pre and post-treatment:
Latency_Survey$Evaluation <- as.character(Latency_Survey$Evaluation)
Latency_Survey$Evaluation_After_Info <- as.character(Latency_Survey$Evaluation_After_Info)
Latency_Survey$Evaluation <- substr(Latency_Survey$Evaluation, start = 1, stop = 4)
Latency_Survey$Evaluation_After_Info <- substr(Latency_Survey$Evaluation_After_Info, start = 1, stop = 4)
Latency_Survey$True_Dummy <- ifelse(Latency_Survey$Evaluation == 'True',1,0)
Latency_Survey$True_Dummy_After <- ifelse(Latency_Survey$Evaluation_After_Info == 'True',1,0)

#Remove observations that posted that their age was above 85:
Latency_Survey$Age <- ifelse(Latency_Survey$Age > 85,NA,Latency_Survey$Age)



#Pull in Fact-checking data from Study 2:
Data_Bef_Aft <- read.csv('./Data/Data_Bef_Aft_Misl_False.csv')
Data_Bef_Aft$Article_day <- as.character(Data_Bef_Aft$Article_day)

Article_Days <- unique(Data_Bef_Aft$Article_day)

#Only use articles that were rated as false/misleading in Study 2
Latency_Survey <- Latency_Survey %>% filter(Article_day %in% Article_Days)

#Merge pre and post-treatment data:
After_Evaluation <- Latency_Survey %>% select(True_Dummy_After,True_Likert_After_Info,Age,Dig_Lit_Avg,Income_Score,CRT_Score,Familiar_Story,Education_Score,Duration,Article_day,Ideology_Score,Gender,ResponseId)
colnames(After_Evaluation)[1] <- 'True_Dummy'
colnames(After_Evaluation)[2] <- 'Likert_Evaluation'
Before_Evaluation <- Latency_Survey %>% select(True_Dummy,Likert_Evaluation,Age,Dig_Lit_Avg,Income_Score,CRT_Score,Familiar_Story,Education_Score,Duration,Article_day,Ideology_Score,Gender,ResponseId)

#Create Treatment variables:
After_Evaluation$Treatment <- 1
Before_Evaluation$Treatment <- 0

#merge them together
Latency_Search <- rbind(Before_Evaluation,After_Evaluation)

#Create Ideological Congruence data:
Model_Data_7 <- Latency_Search
Model_Data_7 <- Model_Data_7 %>% mutate(Dummy_Ideology = ifelse(Ideology_Score > 0,'Conservative','Moderate'))
Model_Data_7 <- Model_Data_7 %>% mutate(Dummy_Ideology = ifelse(Ideology_Score < 0,'Liberal',Dummy_Ideology))
Model_Data_7 <- merge(Model_Data_7,Article_data,all=T)
Model_Data_7$Article_Lean <- ifelse(Model_Data_7$Article_Lean == 'None','Neutral',Model_Data_7$Article_Lean)
Model_Data_7 <- Model_Data_7 %>% mutate(Dummy_Congruence = ifelse(Dummy_Ideology == Article_Lean,1,0))
Model_Data_7$Susc_FN <- Model_Data_7$True_Dummy
Model_Data_7$Treat_Search <- Model_Data_7$Treatment

#Run linear regression and produce coefficient values:
fit_2_1 <- glm(Susc_FN ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score + Article_day,family=binomial, data = Model_Data_7)
lin_results_fit_2_3 <- coeftest(fit_2_1, vcov. = vcovCL(fit_2_1, cluster = list(Model_Data_7$Article_day), type = "HC0"))

#Unique Articles:
Model_Data_7$Article_day <- as.character(Model_Data_7$Article_day)
Model_Data_7$Article_day <- paste0('Article_day',Model_Data_7$Article_day)
Omit_c <- unique(Model_Data_7$Article_day)
full_omit <- paste(Omit_c, collapse = '|')

full_omit_2 <- c(full_omit_2,full_omit)




#Study 4:
#Pull in data:
Data_Bef_Aft_Covid <- read.csv('./Data/Experiment_2_Study_2_Misl_False.csv')
Study_4_Data <- Data_Bef_Aft_Covid %>% filter(Treat_Search == 0)

#Select variables of interest:
Model_Data_7 <- Data_Bef_Aft_Covid %>% select(Likert_Evaluation,Susc_FN,Treat_Search,Education_Score,Age,Gender,Income_Score,Dummy_Congruence,Familiar_Dummy,Article_day,ResponseId)
#Remove NA values:
Model_Data_7 <- na.omit(Model_Data_7)

#Create Gender data
Model_Data_7$Gender <- as.character(Model_Data_7$Gender)
Model_Data_7$Gender <- ifelse(Model_Data_7$Gender == 'Female',1,0)

#Run OLS Model with clustered standard errors:
fit_2_1 <- glm(Susc_FN ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score + Article_day,family=binomial, data = Model_Data_7)
lin_results_fit_2_4 <- coeftest(fit_2_1, vcov. = vcovCL(fit_2_1, cluster = list(Model_Data_7$Article_day), type = "HC0"))

#Unique Articles:
Model_Data_7$Article_day <- as.character(Model_Data_7$Article_day)
Model_Data_7$Article_day <- paste0('Article_day',Model_Data_7$Article_day)
Omit_c <- unique(Model_Data_7$Article_day)
full_omit <- paste(Omit_c, collapse = '|')
full_omit_2 <- c(full_omit_2,full_omit)
full_omit_2 <- paste(full_omit_2, collapse = '|')

#Coefficient Names:
Coef_names= c('(Intercept)','Treatment','Age','Ideological Congruence','Education','Gender','Income')

#Write Table
texreg(list(lin_results_fit_2_1,lin_results_fit_2_2,lin_results_fit_2_3,lin_results_fit_2_4),
       include.ci = FALSE,
       digits=4,
       omit.coef = full_omit_2,
       caption= 'Logistic Regression Results for Binary Dependent Variables in Figure 1',
       label = "table",
       custom.coef.names = Coef_names,
       custom.model.names= c("Study 1", "Study 2","Study 3","Study 4"),
       float.pos = "!htbp",
       file='./Tables/Logistic_Figure_1.txt',
       caption.above = TRUE)







################################################################################################################
####Figures - Robust - Mode

##############################################################################################################

#Create list of robust mode articles:

#Pull Factchecking data from Studies 1-3:
FCer_Data <- read.csv('.//Data//Fact_Checker_Data_Master.csv')
FCer_Data <- FCer_Data %>% select(Article_day,Pariticipant_1_Eval,Pariticipant_2_Eval,Pariticipant_3_Eval,Pariticipant_4_Eval,Pariticipant_5_Eval,Pariticipant_6_Eval)

#Normalize responses:
FCer_Data$Pariticipant_1_Eval <- tolower(substr(FCer_Data$Pariticipant_1_Eval,1,1))
FCer_Data$Pariticipant_2_Eval <- tolower(substr(FCer_Data$Pariticipant_2_Eval,1,1))
FCer_Data$Pariticipant_3_Eval <- tolower(substr(FCer_Data$Pariticipant_3_Eval,1,1))
FCer_Data$Pariticipant_4_Eval <- tolower(substr(FCer_Data$Pariticipant_4_Eval,1,1))
FCer_Data$Pariticipant_5_Eval <- tolower(substr(FCer_Data$Pariticipant_5_Eval,1,1))
FCer_Data$Pariticipant_6_Eval <- tolower(substr(FCer_Data$Pariticipant_6_Eval,1,1))

#Study 4: Covid Articles
#Pull data:
FCer_Data_covid <- read.csv('./Data/fcers_byarticle_all_sets_covid.csv')
FCer_Data_covid$Article_day <- paste0(FCer_Data_covid$date,'_',FCer_Data_covid$article_type)
FCer_Data_covid <- FCer_Data_covid %>% select(Article_day,FC1,FC2,FC3,FC4,FC5)

#Normailize columns names:
colnames(FCer_Data_covid) <- c('Article_day','Pariticipant_1_Eval','Pariticipant_2_Eval','Pariticipant_3_Eval','Pariticipant_4_Eval','Pariticipant_5_Eval')

#Create column
FCer_Data_covid$Pariticipant_6_Eval <- NA

#Merge:
FCer_Data <- rbind(FCer_Data,FCer_Data_covid)

#Study 5:
#Pull data:
FCer_Data_S_5 <- read.csv('./Data/FCers_Study_5.csv')
FCer_Data_S_5 <- FCer_Data_S_5 %>% select(Day_Article,Pariticipant_1_Eval,Pariticipant_2_Eval,Pariticipant_3_Eval,Pariticipant_4_Eval,Pariticipant_5_Eval)
colnames(FCer_Data_S_5)[1] <- 'Article_day'

#Create column
FCer_Data_S_5$Pariticipant_6_Eval <- NA
#Merge:
FCer_Data <- rbind(FCer_Data,FCer_Data_S_5)

#Normalize responses:
FCer_Data$Pariticipant_1_Eval <- ifelse(FCer_Data$Pariticipant_1_Eval == 'm','f',FCer_Data$Pariticipant_1_Eval)
FCer_Data$Pariticipant_2_Eval <- ifelse(FCer_Data$Pariticipant_2_Eval == 'm','f',FCer_Data$Pariticipant_2_Eval)
FCer_Data$Pariticipant_3_Eval <- ifelse(FCer_Data$Pariticipant_3_Eval == 'm','f',FCer_Data$Pariticipant_3_Eval)
FCer_Data$Pariticipant_4_Eval <- ifelse(FCer_Data$Pariticipant_4_Eval == 'm','f',FCer_Data$Pariticipant_4_Eval)
FCer_Data$Pariticipant_5_Eval <- ifelse(FCer_Data$Pariticipant_5_Eval == 'm','f',FCer_Data$Pariticipant_5_Eval)
FCer_Data$Pariticipant_6_Eval <- ifelse(FCer_Data$Pariticipant_6_Eval == 'm','f',FCer_Data$Pariticipant_6_Eval)

#Create number of fact-checks per article:
FCer_Data$FC_Count <- 0
FCer_Data$FC_Count <- ifelse(is.na(FCer_Data$Pariticipant_1_Eval),FCer_Data$FC_Count,FCer_Data$FC_Count+1)
FCer_Data$FC_Count <- ifelse(is.na(FCer_Data$Pariticipant_2_Eval),FCer_Data$FC_Count,FCer_Data$FC_Count+1)
FCer_Data$FC_Count <- ifelse(is.na(FCer_Data$Pariticipant_3_Eval),FCer_Data$FC_Count,FCer_Data$FC_Count+1)
FCer_Data$FC_Count <- ifelse(is.na(FCer_Data$Pariticipant_4_Eval),FCer_Data$FC_Count,FCer_Data$FC_Count+1)
FCer_Data$FC_Count <- ifelse(is.na(FCer_Data$Pariticipant_5_Eval),FCer_Data$FC_Count,FCer_Data$FC_Count+1)
FCer_Data$FC_Count <- ifelse(is.na(FCer_Data$Pariticipant_6_Eval),FCer_Data$FC_Count,FCer_Data$FC_Count+1)

#Replace NAs with none
FCer_Data$Pariticipant_1_Eval <- ifelse(is.na(FCer_Data$Pariticipant_1_Eval),'none',FCer_Data$Pariticipant_1_Eval)
FCer_Data$Pariticipant_2_Eval <- ifelse(is.na(FCer_Data$Pariticipant_2_Eval),'none',FCer_Data$Pariticipant_2_Eval)
FCer_Data$Pariticipant_3_Eval <- ifelse(is.na(FCer_Data$Pariticipant_3_Eval),'none',FCer_Data$Pariticipant_3_Eval)
FCer_Data$Pariticipant_4_Eval <- ifelse(is.na(FCer_Data$Pariticipant_4_Eval),'none',FCer_Data$Pariticipant_4_Eval)
FCer_Data$Pariticipant_5_Eval <- ifelse(is.na(FCer_Data$Pariticipant_5_Eval),'none',FCer_Data$Pariticipant_5_Eval)
FCer_Data$Pariticipant_6_Eval <- ifelse(is.na(FCer_Data$Pariticipant_6_Eval),'none',FCer_Data$Pariticipant_6_Eval)


#Create count of false/misleading ratings:
FCer_Data$F_Count <- 0
FCer_Data$F_Count <- ifelse(FCer_Data$Pariticipant_1_Eval == 'f',FCer_Data$F_Count+1,FCer_Data$F_Count)
FCer_Data$F_Count <- ifelse(FCer_Data$Pariticipant_2_Eval == 'f',FCer_Data$F_Count+1,FCer_Data$F_Count)
FCer_Data$F_Count <- ifelse(FCer_Data$Pariticipant_3_Eval == 'f',FCer_Data$F_Count+1,FCer_Data$F_Count)
FCer_Data$F_Count <- ifelse(FCer_Data$Pariticipant_4_Eval == 'f',FCer_Data$F_Count+1,FCer_Data$F_Count)
FCer_Data$F_Count <- ifelse(FCer_Data$Pariticipant_5_Eval == 'f',FCer_Data$F_Count+1,FCer_Data$F_Count)
FCer_Data$F_Count <- ifelse(FCer_Data$Pariticipant_6_Eval == 'f',FCer_Data$F_Count+1,FCer_Data$F_Count)

#Create count of true ratings:
FCer_Data$T_Count <- 0
FCer_Data$T_Count <- ifelse(FCer_Data$Pariticipant_1_Eval == 't',FCer_Data$T_Count+1,FCer_Data$T_Count)
FCer_Data$T_Count <- ifelse(FCer_Data$Pariticipant_2_Eval == 't',FCer_Data$T_Count+1,FCer_Data$T_Count)
FCer_Data$T_Count <- ifelse(FCer_Data$Pariticipant_3_Eval == 't',FCer_Data$T_Count+1,FCer_Data$T_Count)
FCer_Data$T_Count <- ifelse(FCer_Data$Pariticipant_4_Eval == 't',FCer_Data$T_Count+1,FCer_Data$T_Count)
FCer_Data$T_Count <- ifelse(FCer_Data$Pariticipant_5_Eval == 't',FCer_Data$T_Count+1,FCer_Data$T_Count)
FCer_Data$T_Count <- ifelse(FCer_Data$Pariticipant_6_Eval == 't',FCer_Data$T_Count+1,FCer_Data$T_Count)

#Create proportion of responses
FCer_Data <- FCer_Data %>% mutate(Prop_F = F_Count/FC_Count)
FCer_Data <- FCer_Data %>% mutate(Prop_T = T_Count/FC_Count)

#Create robust proportions:
FCer_Data <- FCer_Data %>% mutate(Robust_Prop_F = (F_Count-1)/FC_Count)
FCer_Data <- FCer_Data %>% mutate(Robust_Prop_T = (T_Count-1)/FC_Count)

#Create list of articles with false/misleading robust modal classifcation:
FCer_Data_FM <- FCer_Data %>% filter(Robust_Prop_F > 0.5)
Robust_Articles <- unique(FCer_Data_FM$Article_day)
Robust_Articles <- as.character(Robust_Articles)

#Create list of articles with true robust modal classifcation:
FCer_Data_T <- FCer_Data %>% filter(Robust_Prop_T > 0.5)
Robust_Articles_T <- unique(FCer_Data_T$Article_day)
Robust_Articles_T <- as.character(Robust_Articles_T)


#Run Model Testing Effect of Searching Online on Belief in Misinformation for Study 1:

#Pull in this data: Search Experiment 1: Study 1:
Misl_False_Search <- read.csv('./Data/Search_Exp_Misl_False.csv')

#Select variables of interest:
Model_Data_7 <- Misl_False_Search %>% select(Likert_Evaluation,Susc_FN,Treat_Search,Education_Score,Age,Gender,Income_Score,Dummy_Congruence,Familiar_Dummy,Article_day,ResponseId)
#Remove NA values:
Model_Data_7 = na.omit(Model_Data_7)

Model_Data_7 <- Model_Data_7 %>% filter(Article_day %in% Robust_Articles)

#Create dataset with just control data:
Study_1_Data <- Model_Data_7 %>% filter(Treat_Search == 0)

#Run linear regression and produce coefficient values:
fit_1_1 = glm(Susc_FN ~ Treat_Search + Education_Score + Age + Gender + Income_Score + Article_day, data = Model_Data_7)

#Run linear regression and produce coefficient values:
fit_1_1 = glm(Susc_FN ~ Treat_Search + Education_Score + Age + Gender + Income_Score + Article_day, data = Model_Data_7)

#Produce confidence intervals with clustered standard errors:
CI_1_1 = coefci(fit_1_1, vcov. = vcovCL(fit_1_1, cluster = list(Model_Data_7$ResponseId), type = "HC0"))

#Run linear regression and produce coefficient values:
fit_1_2 = glm(Likert_Evaluation ~ Treat_Search + Education_Score + Age + Gender + Income_Score + Article_day, data = Model_Data_7)

#Produce confidence intervals with clustered standard errors:
CI_1_2 = coefci(fit_1_2, vcov. = vcovCL(fit_1_2, cluster = list(Model_Data_7$ResponseId), type = "HC0"))



#Run Model Testing Effect of Searching Online on Belief in Misinformation for Study 2:


#Pull in this data:
Data_Bef_Aft <- read.csv('./Data/Data_Bef_Aft_Misl_False.csv')

#Select variables of interest:
Model_Data_7 <- Data_Bef_Aft %>% select(Likert_Evaluation,Susc_FN,Treat_Search,Education_Score,Age,Gender,Income_Score,Dummy_Congruence,Familiar_Dummy,Article_day,ResponseId)
#Remove NA values:
Model_Data_7 <- na.omit(Model_Data_7)

Model_Data_7 <- Model_Data_7 %>% filter(Article_day %in% Robust_Articles)


#Create dataset with just control data:
Study_2_Data <- Model_Data_7 %>% filter(Treat_Search == 0)

#Run linear regression and produce coefficient values:
fit_2_1 = glm(Susc_FN ~ Treat_Search + Age + Gender + Education_Score + Article_day, data = Model_Data_7)

#Produce confidence intervals with clustered standard errors:
CI_2_1 <- coefci(fit_2_1, vcov. = vcovCL(fit_2_1, cluster = list(Model_Data_7$ResponseId,Model_Data_7$Article_day), type = "HC0"))

#Run linear regression and produce coefficient values:
fit_2_2 = glm(Likert_Evaluation ~ Treat_Search + Age + Gender + Article_day, data = Model_Data_7)

#Produce confidence intervals with clustered standard errors:
CI_2_2 <- coefci(fit_2_2, vcov. = vcovCL(fit_2_2, cluster = list(Model_Data_7$ResponseId,Model_Data_7$Article_day), type = "HC0"))




#Run Model Testing Effect of Searching Online on Belief in Misinformation for Study 3:


#Load Data:
Latency_Data <- read.csv('./Data/Latency_FC_Data.csv')
Latency_Survey <- read.csv('./Data/Latency_Control_Survey.csv')

#Create Article_day data for purpose of merging
Latency_Survey <- Latency_Survey %>% mutate(Article_day = paste0(day,sep='_',Article))

#Select data needed:
Latency_Survey <- Latency_Survey %>% select(Evaluation,Likert_Evaluation,True_Likert_After_Info,Evaluation_After_Info,Age,Dig_Lit_Avg,Income_Score,CRT_Score,Familiar_Story,Education_Score,Duration,Article_day,Ideology_Score,Gender,ResponseId)

#Create Gender dummy variable
Latency_Survey$Gender <- as.character(Latency_Survey$Gender)
Latency_Survey$Gender <- ifelse(Latency_Survey$Gender == 'Female',1,0)


#Create Likert Score post-treatment:
Latency_Survey$True_Likert_After_Info <- as.character(Latency_Survey$True_Likert_After_Info)
Latency_Survey <- Latency_Survey %>% filter(True_Likert_After_Info != 'not asked')
Latency_Survey$True_Likert_After_Info <- substr(Latency_Survey$True_Likert_After_Info, start = 1, stop = 2)
Latency_Survey$True_Likert_After_Info <- as.numeric(Latency_Survey$True_Likert_After_Info)

#Create Likert Score pre-treatment:
Latency_Survey$Likert_Evaluation <- as.character(Latency_Survey$Likert_Evaluation)
Latency_Survey$Likert_Evaluation <- substr(Latency_Survey$Likert_Evaluation, start = 1, stop = 2)
Latency_Survey$Likert_Evaluation <- as.numeric(Latency_Survey$Likert_Evaluation)

#Create True Dummy variable pre and post-treatment:
Latency_Survey$Evaluation <- as.character(Latency_Survey$Evaluation)
Latency_Survey$Evaluation_After_Info <- as.character(Latency_Survey$Evaluation_After_Info)
Latency_Survey$Evaluation <- substr(Latency_Survey$Evaluation, start = 1, stop = 4)
Latency_Survey$Evaluation_After_Info <- substr(Latency_Survey$Evaluation_After_Info, start = 1, stop = 4)
Latency_Survey$True_Dummy <- ifelse(Latency_Survey$Evaluation == 'True',1,0)
Latency_Survey$True_Dummy_After <- ifelse(Latency_Survey$Evaluation_After_Info == 'True',1,0)

#Remove observations that posted that their age was above 85:
Latency_Survey$Age <- ifelse(Latency_Survey$Age > 85,NA,Latency_Survey$Age)



#Pull in Fact-checking data from Study 2:
Data_Bef_Aft <- read.csv('./Data/Data_Bef_Aft_Misl_False.csv')
Data_Bef_Aft$Article_day <- as.character(Data_Bef_Aft$Article_day)

#Only use articles that were rated as false/misleading in Study 2
Latency_Survey <- Latency_Survey %>% filter(Article_day %in% Article_Days)

#Merge pre and post-treatment data:
After_Evaluation <- Latency_Survey %>% select(True_Dummy_After,True_Likert_After_Info,Age,Dig_Lit_Avg,Income_Score,CRT_Score,Familiar_Story,Education_Score,Duration,Article_day,Ideology_Score,Gender,ResponseId)
colnames(After_Evaluation)[1] <- 'True_Dummy'
colnames(After_Evaluation)[2] <- 'Likert_Evaluation'
Before_Evaluation <- Latency_Survey %>% select(True_Dummy,Likert_Evaluation,Age,Dig_Lit_Avg,Income_Score,CRT_Score,Familiar_Story,Education_Score,Duration,Article_day,Ideology_Score,Gender,ResponseId)

#Create Treatment variables:
After_Evaluation$Treatment <- 1
Before_Evaluation$Treatment <- 0

#merge them together
Latency_Search <- rbind(Before_Evaluation,After_Evaluation)

Latency_Search <- Latency_Search %>% filter(Article_day %in% Robust_Articles)

#Create Control dataframe
Study_3_Data <- Latency_Search %>% filter(Treatment == 0)

#Run linear regression and produce coefficient values:
fit_3_1 = glm(True_Dummy ~ Treatment + Age + Gender + Article_day, data = Latency_Search)
#Produce confidence intervals using clustered standard errors:
CI_3_1 <- coefci(fit_3_1, vcov. = vcovCL(fit_3_1, cluster = list(Latency_Search$ResponseId), type = "HC0"))

#Run linear regression and produce coefficient values:
fit_3_2 = glm(Likert_Evaluation ~ Treatment + Age + Gender + Article_day, data = Latency_Search)
#Produce confidence intervals using clustered standard errors:
CI_3_2 <- coefci(fit_3_2, vcov. = vcovCL(fit_3_2, cluster = list(Latency_Search$ResponseId), type = "HC0"))



#Run Model Testing Effect of Searching Online on Belief in Misinformation for Study 4:


#Pull in data
Data_Bef_Aft_Covid <- read.csv('./Data/Experiment_2_Study_2_Misl_False.csv')

Model_Data_7 <- Model_Data_7 %>% filter(Article_day %in% Robust_Articles)

#Create dataframe with just control data
Study_4_Data <- Data_Bef_Aft_Covid %>% filter(Treat_Search == 0)

#Select variables of interest:
Model_Data_7 <- Data_Bef_Aft_Covid %>% select(Likert_Evaluation,Susc_FN,Treat_Search,Education_Score,Age,Gender,Income_Score,Dummy_Congruence,Familiar_Dummy,Article_day,ResponseId)
#Remove NA values:
Model_Data_7 <- na.omit(Model_Data_7)

#Run linear regression and produce coefficient values:
fit_4_1 = glm(Susc_FN ~ Treat_Search + Age + Gender + Article_day, data = Model_Data_7)
#Produce confidence intervals with clustered standard errors:
CI_4_1 <- coefci(fit_4_1, vcov. = vcovCL(fit_4_1, cluster = list(Model_Data_7$ResponseId), type = "HC0"))

#Run linear regression and produce coefficient values:
fit_4_2 = glm(Likert_Evaluation ~ Treat_Search + Age + Gender + Article_day, data = Model_Data_7)
#Produce confidence intervals with clustered standard errors:
CI_4_2 <- coefci(fit_4_2, vcov. = vcovCL(fit_4_2, cluster = list(Model_Data_7$ResponseId), type = "HC0"))

#Create vector with Study names:
Coef_names <- c('Study 1',
                'Study 1',
                'Study 2',
                'Study 2',
                'Study 3',
                'Study 3',
                'Study 4',
                'Study 4')

#Create vector with measures:
Measure <-    c('Rate as True',
                'Ordinal Scale (7)',
                'Rate as True',
                'Ordinal Scale (7)',
                'Rate as True',
                'Ordinal Scale (7)',
                'Rate as True',
                'Ordinal Scale (7)')

#Create vector with coefficients:
Coefficients <- c(fit_1_1$coefficients[2]/sd(Study_1_Data$Susc_FN),
                  fit_1_2$coefficients[2]/sd(Study_1_Data$Likert_Evaluation),
                  fit_2_1$coefficients[2]/sd(Study_2_Data$Susc_FN),
                  fit_2_2$coefficients[2]/sd(Study_2_Data$Likert_Evaluation),
                  fit_3_1$coefficients[2]/sd(Study_3_Data$True_Dummy),
                  fit_3_2$coefficients[2]/sd(Study_3_Data$Likert_Evaluation),
                  fit_4_1$coefficients[2]/sd(Study_4_Data$Susc_FN),
                  fit_4_2$coefficients[2]/sd(Study_4_Data$Likert_Evaluation))

#Create vector with upper confidence intervals:
CI_Upper <- c(CI_1_1[2,2]/sd(Study_1_Data$Susc_FN),
              CI_1_2[2,2]/sd(Study_1_Data$Likert_Evaluation),
              CI_2_1[2,2]/sd(Study_2_Data$Susc_FN),
              CI_2_2[2,2]/sd(Study_2_Data$Likert_Evaluation),
              CI_3_1[2,2]/sd(Study_3_Data$True_Dummy),
              CI_3_2[2,2]/sd(Study_3_Data$Likert_Evaluation),
              CI_4_1[2,2]/sd(Study_4_Data$Susc_FN),
              CI_4_2[2,2]/sd(Study_4_Data$Likert_Evaluation))

#Create vector with lower confidence intervals:
CI_Lower <- c(CI_1_1[2,1]/sd(Study_1_Data$Susc_FN),
              CI_1_2[2,1]/sd(Study_1_Data$Likert_Evaluation),
              CI_2_1[2,1]/sd(Study_2_Data$Susc_FN),
              CI_2_2[2,1]/sd(Study_2_Data$Likert_Evaluation),
              CI_3_1[2,1]/sd(Study_3_Data$True_Dummy),
              CI_3_2[2,1]/sd(Study_3_Data$Likert_Evaluation),
              CI_4_1[2,1]/sd(Study_4_Data$Susc_FN),
              CI_4_2[2,1]/sd(Study_4_Data$Likert_Evaluation))          

#Put together matrix with data for plot:
d_matrix <- cbind(Coef_names,Coefficients,CI_Upper,CI_Lower,Measure)
rownames(d_matrix) <- c()
d_matrix <- data.frame(d_matrix)
d_matrix$Coefficients <- as.character(d_matrix$Coefficients)
d_matrix$CI_Lower <- as.character(d_matrix$CI_Lower)
d_matrix$CI_Upper <- as.character(d_matrix$CI_Upper)
d_matrix$Coefficients <- as.numeric(d_matrix$Coefficients)
d_matrix$CI_Lower <- as.numeric(d_matrix$CI_Lower)
d_matrix$CI_Upper <- as.numeric(d_matrix$CI_Upper)
d_matrix <- d_matrix %>% arrange(desc(row_number()))

#Set points on Y-Axis:
d_matrix$x<-c(0.8,1.2,1.8,2.2,2.8,3.2,3.8,4.2)

#Produce plot:
ggplot(data = d_matrix, aes(x = x, y = Coefficients,color=Measure)) +
  geom_hline(aes(yintercept = 0), color = "gray",
             linetype = 2, size = 1.2) +
  geom_point(size=4) +
  geom_linerange(aes(min = CI_Lower,
                     max = CI_Upper),
                 size=1.5) +
  scale_color_manual(values=c('red','blue','purple'), name = "Measure") +
  ylab("\nEffect of Searching for Information on \nPerceived Veracity of Misinformation \n(1 unit is 1 standard deviation \nof that measure in the control group) ") +
  theme_classic() +
  theme(axis.title.x = element_text(size=18),
        axis.text.x  = element_text(size=16),
        axis.title.y = element_text(size=16),
        axis.text.y  = element_text(size=22),
        plot.title = element_text(size = 16),
        legend.title = element_text(size=16),
        legend.text = element_text(size=14)) +
  ylim(-0.10,0.4) +
  scale_x_continuous(" \n",breaks=c(1,2,3,4),labels=c('Study 4',
                                                      'Study 3',
                                                      'Study 2',
                                                      'Study 1'),limits=c(0.5,4.5)) +
  coord_flip()

#Save figure:
ggsave('./Figures/All_4_Studies_ROBUST.png',height=6,width=8)

################################################################################################################

############################ Figure 2a: Study_5_Bar_Graph_Google_Search_ROBUST.png ####################################

################################################################################################################

#Pull in treatment data for Study 5:
Treatment_Data <- read.csv('./Data/Treatment_Data_Study_5.csv')
Treatment_Data$Link_1 <- NULL
Treatment_Data$Link_2 <- NULL

Treatment_Data <- merge(Treatment_Data,FC_Ideo_Data,by='Article_day')

Treatment_Data <- Treatment_Data %>% mutate(Dummy_Ideology = ifelse(Ideology_Score > 0,'Conservative','Moderate'))
Treatment_Data <- Treatment_Data %>% mutate(Dummy_Ideology = ifelse(Ideology_Score < 0,'Liberal',Dummy_Ideology))
Treatment_Data <- Treatment_Data %>% mutate(Ideo_Congruence = ifelse(Dummy_Ideology == Article_Lean,1,0))

#Merge Google Search Results and survey results for each article in Study 5:
#Day 1
Google_results_1 <- read.csv('./Data/Google_Search_Results_Treatment_Day_1_All_New_Attempt.csv')
Treatment_1 <- Treatment_Data %>% filter(Day == 'Day_1')
Treatment_1$ResponseId <-  as.character(Treatment_1$ResponseId)
Treatment_1$Article_day <-  as.character(Treatment_1$Article_day)
Google_results_1$Article_Eval <- as.character(Google_results_1$Article_Eval)
Google_results_1$Article_Eval <- paste0('Day_1_',Google_results_1$Article_Eval)
Survey_1 <- merge(Treatment_1,Google_results_1,by.x=c('ResponseId','Article_day'),by.y=c('Respondent_Id','Article_Eval'))
Survey_1 <- Survey_1 %>% select(ResponseId,Day,Article_day,Category,True_Dummy,Seven_Ordinal,Four_Ordinal,Age,Gender,FC_Eval,List_Scores,ALL_URLS,Dummy_Ideology,Ideo_Congruence,Income_Score,Education_Score,Dig_Lit_Score,Prop_Unreliable,Mean_Score,Article_Lean)

#Day 2
Google_results_2 <- read.csv('./Data/Google_Search_Results_Treatment_Day_2_All_New_Attempt.csv')
Treatment_2 <- Treatment_Data %>% filter(Day == 'Day_2')
Google_results_2$Article_Eval <- as.character(Google_results_2$Article_Eval)
Google_results_2$Article_Eval <- paste0('Day_2_',Google_results_2$Article_Eval)
Survey_2 <- merge(Treatment_2,Google_results_2,by.x=c('ResponseId','Article_day'),by.y=c('Respondent_Id','Article_Eval'))
Survey_2 <- Survey_2 %>% select(ResponseId,Day,Article_day,Category,True_Dummy,Seven_Ordinal,Four_Ordinal,Age,Gender,FC_Eval,List_Scores,ALL_URLS,Dummy_Ideology,Ideo_Congruence,Income_Score,Education_Score,Dig_Lit_Score,Prop_Unreliable,Mean_Score,Article_Lean)

#Day 3
Google_results_3 <- read.csv('./Data/Google_Search_Results_Treatment_Day_3_All_New_Attempt.csv')
Treatment_3 <- Treatment_Data %>% filter(Day == 'Day_3')
Google_results_3$Article_Eval <- as.character(Google_results_3$Article_Eval)
Google_results_3$Article_Eval <- paste0('Day_3_',Google_results_3$Article_Eval)
Survey_3 <- merge(Treatment_3,Google_results_3,by.x=c('ResponseId','Article_day'),by.y=c('Respondent_Id','Article_Eval'))
Survey_3 <- Survey_3 %>% select(ResponseId,Day,Article_day,Category,True_Dummy,Seven_Ordinal,Four_Ordinal,Age,Gender,FC_Eval,List_Scores,ALL_URLS,Dummy_Ideology,Ideo_Congruence,Income_Score,Education_Score,Dig_Lit_Score,Prop_Unreliable,Mean_Score,Article_Lean)

#Day 4
Google_results_4 <- read.csv('./Data/Google_Search_Results_Treatment_Day_4_All_New_Attempt.csv')
Treatment_4 <- Treatment_Data %>% filter(Day == 'Day_4')
Google_results_4$Article_Eval <- as.character(Google_results_4$Article_Eval)
Google_results_4$Article_Eval <- paste0('Day_4_',Google_results_4$Article_Eval)
Survey_4 <- merge(Treatment_4,Google_results_4,by.x=c('ResponseId','Article_day'),by.y=c('Respondent_Id','Article_Eval'))
Survey_4 <- Survey_4 %>% select(ResponseId,Day,Article_day,Category,True_Dummy,Seven_Ordinal,Four_Ordinal,Age,Gender,FC_Eval,List_Scores,ALL_URLS,Dummy_Ideology,Ideo_Congruence,Income_Score,Education_Score,Dig_Lit_Score,Prop_Unreliable,Mean_Score,Article_Lean)

#Day 5
Google_results_5 <- read.csv('./Data/Google_Search_Results_Treatment_Day_5_All_New_Attempt.csv')
Treatment_5 <- Treatment_Data %>% filter(Day == 'Day_5')
Google_results_5$Article_Eval <- as.character(Google_results_5$Article_Eval)
Google_results_5$Article_Eval <- paste0('Day_5_',Google_results_5$Article_Eval)
Survey_5 <- merge(Treatment_5,Google_results_5,by.x=c('ResponseId','Article_day'),by.y=c('Respondent_Id','Article_Eval'))
Survey_5 <- Survey_5 %>% select(ResponseId,Day,Article_day,Category,True_Dummy,Seven_Ordinal,Four_Ordinal,Age,Gender,FC_Eval,List_Scores,ALL_URLS,Dummy_Ideology,Ideo_Congruence,Income_Score,Education_Score,Dig_Lit_Score,Prop_Unreliable,Mean_Score,Article_Lean)

#Day 6
Google_results_6 <- read.csv('./Data/Google_Search_Results_Treatment_Day_6_All_New_Attempt.csv')
Treatment_6 <- Treatment_Data %>% filter(Day == 'Day_6')
Google_results_6$Article_Eval <- as.character(Google_results_6$Article_Eval)
Google_results_6$Article_Eval <- paste0('Day_6_',Google_results_6$Article_Eval)
Survey_6 <- merge(Treatment_6,Google_results_6,by.x=c('ResponseId','Article_day'),by.y=c('Respondent_Id','Article_Eval'))
Survey_6 <- Survey_6 %>% select(ResponseId,Day,Article_day,Category,True_Dummy,Seven_Ordinal,Four_Ordinal,Age,Gender,FC_Eval,List_Scores,ALL_URLS,Dummy_Ideology,Ideo_Congruence,Income_Score,Education_Score,Dig_Lit_Score,Prop_Unreliable,Mean_Score,Article_Lean)

#Day 7
Google_results_7 <- read.csv('./Data/Google_Search_Results_Treatment_Day_7_All_New_Attempt.csv')
Treatment_7 <- Treatment_Data %>% filter(Day == 'Day_7')
Google_results_7$Article_Eval <- as.character(Google_results_7$Article_Eval)
Google_results_7$Article_Eval <- paste0('Day_7_',Google_results_7$Article_Eval)
Survey_7 <- merge(Treatment_7,Google_results_7,by.x=c('ResponseId','Article_day'),by.y=c('Respondent_Id','Article_Eval'))
Survey_7 <- Survey_7 %>% select(ResponseId,Day,Article_day,Category,True_Dummy,Seven_Ordinal,Four_Ordinal,Age,Gender,FC_Eval,List_Scores,ALL_URLS,Dummy_Ideology,Ideo_Congruence,Income_Score,Education_Score,Dig_Lit_Score,Prop_Unreliable,Mean_Score,Article_Lean)

#Day 8
Google_results_8 <- read.csv('./Data/Google_Search_Results_Treatment_Day_8_All_New_Attempt.csv')
Treatment_8 <- Treatment_Data %>% filter(Day == 'Day_8')
Google_results_8$Article_Eval <- as.character(Google_results_8$Article_Eval)
Google_results_8$Article_Eval <- paste0('Day_8_',Google_results_8$Article_Eval)
Survey_8 <- merge(Treatment_8,Google_results_8,by.x=c('ResponseId','Article_day'),by.y=c('Respondent_Id','Article_Eval'))
Survey_8 <- Survey_8 %>% select(ResponseId,Day,Article_day,Category,True_Dummy,Seven_Ordinal,Four_Ordinal,Age,Gender,FC_Eval,List_Scores,ALL_URLS,Dummy_Ideology,Ideo_Congruence,Income_Score,Education_Score,Dig_Lit_Score,Prop_Unreliable,Mean_Score,Article_Lean)

#Day 9
Google_results_9 <- read.csv('./Data/Google_Search_Results_Treatment_Day_9_All_New_Attempt.csv')
Treatment_9 <- Treatment_Data %>% filter(Day == 'Day_9')
Google_results_9$Article_Eval <- as.character(Google_results_9$Article_Eval)
Google_results_9$Article_Eval <- paste0('Day_9_',Google_results_9$Article_Eval)
Survey_9 <- merge(Treatment_9,Google_results_9,by.x=c('ResponseId','Article_day'),by.y=c('Respondent_Id','Article_Eval'))
Survey_9 <- Survey_9 %>% select(ResponseId,Day,Article_day,Category,True_Dummy,Seven_Ordinal,Four_Ordinal,Age,Gender,FC_Eval,List_Scores,ALL_URLS,Dummy_Ideology,Ideo_Congruence,Income_Score,Education_Score,Dig_Lit_Score,Prop_Unreliable,Mean_Score,Article_Lean)

#Day 10
Google_results_10 <- read.csv('./Data/Google_Search_Results_Treatment_Day_10_All_New_Attempt.csv')
Treatment_10 <- Treatment_Data %>% filter(Day == 'Day_10')
Google_results_10$Article_Eval <- as.character(Google_results_10$Article_Eval)
Google_results_10$Article_Eval <- paste0('Day_10_',Google_results_10$Article_Eval)
Survey_10 <- merge(Treatment_10,Google_results_10,by.x=c('ResponseId','Article_day'),by.y=c('Respondent_Id','Article_Eval'))
Survey_10 <- Survey_10 %>% select(ResponseId,Day,Article_day,Category,True_Dummy,Seven_Ordinal,Four_Ordinal,Age,Gender,FC_Eval,List_Scores,ALL_URLS,Dummy_Ideology,Ideo_Congruence,Income_Score,Education_Score,Dig_Lit_Score,Prop_Unreliable,Mean_Score,Article_Lean)

#Day 11
Google_results_11 <- read.csv('./Data/Google_Search_Results_Treatment_Day_11_All_New_Attempt.csv')
Treatment_11 <- Treatment_Data %>% filter(Day == 'Day_11')
Google_results_11$Article_Eval <- as.character(Google_results_11$Article_Eval)
Google_results_11$Article_Eval <- paste0('Day_11_',Google_results_11$Article_Eval)
Survey_11 <- merge(Treatment_11,Google_results_11,by.x=c('ResponseId','Article_day'),by.y=c('Respondent_Id','Article_Eval'))
Survey_11 <- Survey_11 %>% select(ResponseId,Day,Article_day,Category,True_Dummy,Seven_Ordinal,Four_Ordinal,Age,Gender,FC_Eval,List_Scores,ALL_URLS,Dummy_Ideology,Ideo_Congruence,Income_Score,Education_Score,Dig_Lit_Score,Prop_Unreliable,Mean_Score,Article_Lean)

#Day 12
Google_results_12 <- read.csv('./Data/Google_Search_Results_Treatment_Day_12_All_New_Attempt.csv')
Treatment_12 <- Treatment_Data %>% filter(Day == 'Day_12')
Google_results_12$Article_Eval <- as.character(Google_results_12$Article_Eval)
Google_results_12$Article_Eval <- paste0('Day_12_',Google_results_12$Article_Eval)
Survey_12 <- merge(Treatment_12,Google_results_12,by.x=c('ResponseId','Article_day'),by.y=c('Respondent_Id','Article_Eval'))
Survey_12 <- Survey_12 %>% select(ResponseId,Day,Article_day,Category,True_Dummy,Seven_Ordinal,Four_Ordinal,Age,Gender,FC_Eval,List_Scores,ALL_URLS,Dummy_Ideology,Ideo_Congruence,Income_Score,Education_Score,Dig_Lit_Score,Prop_Unreliable,Mean_Score,Article_Lean)

#Merge data from each day:
Survey_Unrel <- rbind(Survey_1,
                      Survey_2,
                      Survey_3,
                      Survey_4,
                      Survey_5,
                      Survey_6,
                      Survey_7,
                      Survey_8,
                      Survey_9,
                      Survey_10,
                      Survey_11,
                      Survey_12)

#Create dataframe with this basic data:
Combined_GS_Survey_Data <- Survey_Unrel

#Create string list of news sites scores:
Survey_Unrel$List_Scores <- as.character(Survey_Unrel$List_Scores)
Survey_Unrel$List_Scores <- gsub('\\[|\\]','',Survey_Unrel$List_Scores)
Survey_Unrel$List_Scores <- gsub('\\[|\\]','',Survey_Unrel$List_Scores)

#FILTER ONLY FALSE/MISLEADING ARTICLES
Survey_Unrel <- Survey_Unrel %>% filter(FC_Eval == 'FM')

#Create Average Score by Respondent:
Survey_Evaluations <- Survey_Unrel %>% filter(!is.na(Mean_Score))
Survey_Evaluations <- Survey_Evaluations %>% select(ResponseId,Article_day,List_Scores)
Respondent_Evaluations <- Survey_Evaluations %>% select(ResponseId,Article_day)
Respondent_Evaluations <- unique(Respondent_Evaluations)

#Create empty matrix
Search_Results_DF <- matrix(ncol=4)
colnames(Search_Results_DF) <- c('ResponseId',
                                 'Article_day',
                                 'Mean_Score_Final',
                                 'Prop_Unreliable_Final')

#For loop that create average score of news sites for each respondent:
for(i in 1:nrow(Respondent_Evaluations)){
  Resp <- Respondent_Evaluations$ResponseId[i]
  Article <- Respondent_Evaluations$Article_day[i]  
  df_survey <- Survey_Evaluations %>% filter(ResponseId == Resp & Article_day == Article)
  All_Scores <- paste(df_survey$List_Scores,collapse=', ')
  All_Scores <- unlist(strsplit(All_Scores, split=", "))
  All_Scores <- as.numeric(All_Scores)
  Mean_Reliability <- mean(All_Scores)
  Total_links <- length(All_Scores)
  Total_Unrel <- 0
  for(x in 1:length(All_Scores)){
    if(All_Scores[x] < 60){
      Total_Unrel = Total_Unrel + 1
    }
  }
  Proportion_Unrel = Total_Unrel/Total_links
  
  #Create matrix with average score and proportion of unreliable news sites:
  new_df <- matrix(c(Resp,Article,Mean_Reliability,Proportion_Unrel),ncol=4)
  colnames(new_df) <- c('ResponseId',
                        'Article_day',
                        'Mean_Score_Final',
                        'Prop_Unreliable_Final')
  
  Search_Results_DF <- rbind(Search_Results_DF,
                             new_df)
}

#Create dataframe:
Search_Results_DF <- as.data.frame(Search_Results_DF)

#Make mean score and proportion numeric variables:
Search_Results_DF$Mean_Score_Final <- as.character(Search_Results_DF$Mean_Score_Final)
Search_Results_DF$Mean_Score_Final <- as.numeric(Search_Results_DF$Mean_Score_Final)
Search_Results_DF$Prop_Unreliable_Final <- as.character(Search_Results_DF$Prop_Unreliable_Final)
Search_Results_DF$Prop_Unreliable_Final <- as.numeric(Search_Results_DF$Prop_Unreliable_Final)

#Create False/Misleading Article Dataframe:
New_FM <- Search_Results_DF



#Merged Google Search Results and survey results for each article in Study 5:
Survey_Unrel <- Combined_GS_Survey_Data

#Create string list of news sites scores:
Survey_Unrel$List_Scores <- as.character(Survey_Unrel$List_Scores)
Survey_Unrel$List_Scores <- gsub('\\[|\\]','',Survey_Unrel$List_Scores)
Survey_Unrel$List_Scores <- gsub('\\[|\\]','',Survey_Unrel$List_Scores)

#FILTER ONLY TRUE ARTICLES
Survey_Unrel <- Survey_Unrel %>% filter(FC_Eval == 'True')

#Create Average Score by Respondent:
Survey_Evaluations <- Survey_Unrel %>% filter(!is.na(Mean_Score))
Survey_Evaluations <- Survey_Evaluations %>% select(ResponseId,Article_day,List_Scores)
Respondent_Evaluations <- Survey_Evaluations %>% select(ResponseId,Article_day)
Respondent_Evaluations <- unique(Respondent_Evaluations)

#Create Empty Matrix
Search_Results_DF <- matrix(ncol=4)
colnames(Search_Results_DF) <- c('ResponseId',
                                 'Article_day',
                                 'Mean_Score_Final',
                                 'Prop_Unreliable_Final')


#For loop creating average mean scores and proportion unreliable:
for(i in 1:nrow(Respondent_Evaluations)){
  Resp <- Respondent_Evaluations$ResponseId[i]
  Article <- Respondent_Evaluations$Article_day[i]  
  df_survey <- Survey_Evaluations %>% filter(ResponseId == Resp & Article_day == Article)
  All_Scores <- paste(df_survey$List_Scores,collapse=', ')
  All_Scores <- unlist(strsplit(All_Scores, split=", "))
  All_Scores <- as.numeric(All_Scores)
  Mean_Reliability <- mean(All_Scores)
  Total_links <- length(All_Scores)
  Total_Unrel <- 0
  for(x in 1:length(All_Scores)){
    if(All_Scores[x] < 60){
      Total_Unrel = Total_Unrel + 1
    }
  }
  Proportion_Unrel = Total_Unrel/Total_links
  
  new_df <- matrix(c(Resp,Article,Mean_Reliability,Proportion_Unrel),ncol=4)
  colnames(new_df) <- c('ResponseId',
                        'Article_day',
                        'Mean_Score_Final',
                        'Prop_Unreliable_Final')
  
  Search_Results_DF <- rbind(Search_Results_DF,
                             new_df)
}


#Create dataframe:
Search_Results_DF <- as.data.frame(Search_Results_DF)

#Make mean scores and propotion of links that are unreliable numeric variables:
Search_Results_DF$Mean_Score_Final <- as.character(Search_Results_DF$Mean_Score_Final)
Search_Results_DF$Mean_Score_Final <- as.numeric(Search_Results_DF$Mean_Score_Final)
Search_Results_DF$Prop_Unreliable_Final <- as.character(Search_Results_DF$Prop_Unreliable_Final)
Search_Results_DF$Prop_Unreliable_Final <- as.numeric(Search_Results_DF$Prop_Unreliable_Final)

#Create dataframe:
New_T <- Search_Results_DF



#Filter Robust Mode Articles:
New_FM <- New_FM %>% filter(Article_day %in% Robust_Articles)
New_T <- New_T %>% filter(Article_day %in% Robust_Articles_T)

#Create Count of individuals that didnt see any unreliable news sites:
True_Count_Zero <- New_T %>% filter(Prop_Unreliable_Final == 0)
FM_Count_Zero <- New_FM %>% filter(Prop_Unreliable_Final == 0)

#Create Count of individuals that saw 0-50% unreliable news sites:
True_Count_10_25 <- New_T %>% filter(Prop_Unreliable_Final > 0 & Prop_Unreliable_Final <= 0.5)
FM_Count_10_25 <- New_FM %>% filter(Prop_Unreliable_Final > 0 & Prop_Unreliable_Final <= 0.5)

#Create Count of individuals that more than 50% unreliable news sites:
True_Count_25_100 <- New_T %>% filter(Prop_Unreliable_Final > 0.5)
FM_Count_25_100 <- New_FM %>% filter(Prop_Unreliable_Final > 0.5)


#Create matrix to plot:
Matrix_Dist <- matrix(c(nrow(True_Count_Zero)/nrow(New_T),'0%','True',
                        nrow(FM_Count_Zero)/nrow(New_FM),'0%','FM',
                        nrow(True_Count_10_25)/nrow(New_T),'0-50%','True',
                        nrow(FM_Count_10_25)/nrow(New_FM),'0-50%','FM',
                        nrow(True_Count_25_100)/nrow(New_T),'50-100%','True',
                        nrow(FM_Count_25_100)/nrow(New_FM),'50-100%','FM'),ncol=3,byrow=T)


Matrix_Dist <- as.data.frame(Matrix_Dist)
colnames(Matrix_Dist) <- c('Proportion','Percentage','Article_Rating')
Matrix_Dist$Proportion <- as.character(Matrix_Dist$Proportion)
Matrix_Dist$Proportion <- as.numeric(Matrix_Dist$Proportion)
Matrix_Dist$Percentage <- factor(Matrix_Dist$Percentage,levels=c('0%',
                                                                 '0-50%',
                                                                 '50-100%'))
Matrix_Dist$Article_Rating <- factor(Matrix_Dist$Article_Rating,levels=c('True',
                                                                         'FM'))
Matrix_Dist$Proportion <- round(Matrix_Dist$Proportion,2)


#Produce plot:
ggplot(Matrix_Dist, aes(fill=Article_Rating, y=Proportion, x=Percentage)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_manual(values=c('royalblue2','red3'), name = "Article\nRating") +
  geom_density(adjust=3, alpha=.4) +
  ylab('Proportion of Individuals \n') +
  xlab('\n Percentage of News Links Returned by Search Engine\n That are From Unreliable Sources') +
  theme_bw() + 
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(size=22),
        axis.text.x  = element_text(size=20),
        axis.title.y = element_text(size=22),
        axis.text.y  = element_text(size=20),
        title =element_text(size=20, face='bold'),
        legend.text = element_text(size=20)) + guides(fill=guide_legend(
          keywidth=0.3,
          keyheight=0.3,
          default.unit="inch")) +
  geom_text(aes(label=Proportion), position=position_dodge(width=0.9), vjust=-0.25,size=6) +
  ylim(0,1)

#Save figure:
ggsave('./Figures/Study_5_Bar_Graph_Google_Search_ROBUST.png',height=8,width=12)

################################################################################################################

########################################## Figure 2b: Study_5_1_ROBUST.png ############################################

################################################################################################################


#Pull in Fact-Checker Ideological Perspective
FC_Ideo_Data <- read.csv('./Data/FC_Ideo_Data.csv')
FC_Ideo_Data$X <- NULL

#Pull in ControlData :
Control_Data <- read.csv('./Data/Control_Data_Study_5.csv')

#Merge data:
Control_Data <- merge(Control_Data,FC_Ideo_Data,by='Article_day')

#Create Ideological Perspective variable:
Control_Data <- Control_Data %>% mutate(Dummy_Ideology = ifelse(Ideology_Score > 0,'Conservative','Moderate'))
Control_Data <- Control_Data %>% mutate(Dummy_Ideology = ifelse(Ideology_Score < 0,'Liberal',Dummy_Ideology))
Control_Data <- Control_Data %>% mutate(Ideo_Congruence = ifelse(Dummy_Ideology == Article_Lean,1,0))

#Read CSV (Treatment Data):
Treatment_Data <- read.csv('./Data/Treatment_Data_Study_5.csv')
Treatment_Data$Link_1 <- NULL
Treatment_Data$Link_2 <- NULL

#Merge control and treatment data:
Treatment_Data <- merge(Treatment_Data,FC_Ideo_Data,by='Article_day')

#Create Ideological Perspective variable:
Treatment_Data <- Treatment_Data %>% mutate(Dummy_Ideology = ifelse(Ideology_Score > 0,'Conservative','Moderate'))
Treatment_Data <- Treatment_Data %>% mutate(Dummy_Ideology = ifelse(Ideology_Score < 0,'Liberal',Dummy_Ideology))
Treatment_Data <- Treatment_Data %>% mutate(Ideo_Congruence = ifelse(Dummy_Ideology == Article_Lean,1,0))

#Merge data
All_Data <- rbind(Treatment_Data,Control_Data)

#Filter only false/misleading articles:
FM_Data <- All_Data %>% filter(FC_Eval == 'FM')

#Filter Robust Mode Articles:
FM_Data <- FM_Data %>% filter(Article_day %in% Robust_Articles)

#Create control and treatment dataframes
C_Data <- FM_Data %>% filter(Treatment == 0)
T_Data <- FM_Data %>% filter(Treatment == 1)


#Run linear regression and produCce coefficient values:
fit_1_1 = glm(True_Dummy ~ Treatment + Age + Gender + Education_Score + Income_Score + Ideo_Congruence + Article_day,data=FM_Data)
#Produce confidence intervals with clusterd standard errors:
CI_1_1 = coefci(fit_1_1, vcov. = vcovCL(fit_1_1, cluster = list(FM_Data$ResponseId), type = "HC0"))

#Run linear regression and produce coefficient values:
fit_1_2 = glm(Four_Ordinal ~Treatment + Age + Gender + Education_Score + Income_Score + Ideo_Congruence + Article_day,data=FM_Data)
#Produce confidence intervals with clusterd standard errors:
CI_1_2 = coefci(fit_1_2, vcov. = vcovCL(fit_1_2, cluster = list(FM_Data$ResponseId), type = "HC0"))

#Run linear regression and produce coefficient values:
fit_1_3 = glm(Seven_Ordinal ~ Treatment + Age + Gender + Education_Score + Income_Score + Ideo_Congruence + Article_day,data=FM_Data)
#Produce confidence intervals with clusterd standard errors:
CI_1_3 = coefci(fit_1_3, vcov. = vcovCL(fit_1_3, cluster = list(FM_Data$ResponseId), type = "HC0"))


#Figure

#Create vector with measures:
Coef_names <- c('Rate as True',
                'Ordinal Scale (4)',
                'Ordinal Scale (7)')

#Create vector with coefficients:
Coefficients <- c(fit_1_1$coefficients[2]/sd(FM_Data$True_Dummy),
                  fit_1_2$coefficients[2]/sd(FM_Data$Four_Ordinal),
                  fit_1_3$coefficients[2]/sd(FM_Data$Seven_Ordinal))

#Create upper confidence intervals:
CI_Upper <- c(CI_1_1[2,2]/sd(FM_Data$True_Dummy),
              CI_1_2[2,2]/sd(FM_Data$Four_Ordinal),
              CI_1_3[2,2]/sd(FM_Data$Seven_Ordinal))            

#Create lower confidence intervals:
CI_Lower <- c(CI_1_1[2,1]/sd(FM_Data$True_Dummy),
              CI_1_2[2,1]/sd(FM_Data$Four_Ordinal),
              CI_1_3[2,1]/sd(FM_Data$Seven_Ordinal))           

#Create data frame:
d_matrix <- cbind(Coef_names,Coefficients,CI_Upper,CI_Lower)
rownames(d_matrix) <- c()
d_matrix <- data.frame(d_matrix)

d_matrix$Coefficients <- as.character(d_matrix$Coefficients)
d_matrix$CI_Lower <- as.character(d_matrix$CI_Lower)
d_matrix$CI_Upper <- as.character(d_matrix$CI_Upper)

d_matrix$Coefficients <- as.numeric(d_matrix$Coefficients)
d_matrix$CI_Lower <- as.numeric(d_matrix$CI_Lower)
d_matrix$CI_Upper <- as.numeric(d_matrix$CI_Upper)

d_matrix <- d_matrix %>% arrange(desc(row_number()))

d_matrix$x<-c(0.3,0.2,0.1)


#Produce plot:
ggplot(data = d_matrix, aes(x = x, y = Coefficients)) +
  geom_hline(aes(yintercept = 0), color = "gray",
             linetype = 2, size = 1.2) +
  geom_point(size=4) +
  geom_linerange(aes(min = CI_Lower,
                     max = CI_Upper),
                 size=1.5) +
  scale_color_manual(values=c('red','blue','purple'), name = "Period") +
  ylab("\nEffect of Searching for Information on \nPerceived Veracity of Misinformation \n(1 unit is 1 standard deviation of that measure) ") +
  theme_classic() +
  theme(axis.title.x = element_text(size=18),
        axis.text.x  = element_text(size=16),
        axis.title.y = element_text(size=16),
        axis.text.y  = element_text(size=22),
        plot.title = element_text(size = 16),
        legend.title = element_text(size=16),
        legend.text = element_text(size=14)) +
  ylim(-0.13,0.7) +
  scale_x_continuous(" \n",breaks=c(0.3,0.2,0.1),labels=Coef_names,limits=c(0.0,0.4)) +
  coord_flip()

#Save plot:
ggsave('./Figures/Study_5_1_ROBUST.png',height=8,width=8)

################################################################################################################

########################################## Figure 2c: Coefs_CIs_ROBUST.png ############################################

################################################################################################################

#Filter DT Data by robust mode:
Combined_GS_Survey_Data <- Combined_GS_Survey_Data %>% filter(Article_day %in% Robust_Articles)

#Treatment Only - Subset By Quality of Google Results:
Survey_Unrel <- Combined_GS_Survey_Data

#Create string list of news sites scores:
Survey_Unrel$List_Scores <- as.character(Survey_Unrel$List_Scores)
Survey_Unrel$List_Scores <- gsub('\\[|\\]','',Survey_Unrel$List_Scores)
Survey_Unrel$List_Scores <- gsub('\\[|\\]','',Survey_Unrel$List_Scores)
Survey_Unrel <- Survey_Unrel %>% filter(FC_Eval == 'FM')



#Create Average Score by Respondent:
Survey_Evaluations <- Survey_Unrel %>% filter(!is.na(Mean_Score))
Survey_Evaluations <- Survey_Evaluations %>% select(ResponseId,Article_day,List_Scores)
Respondent_Evaluations <- Survey_Evaluations %>% select(ResponseId,Article_day)
Respondent_Evaluations <- unique(Respondent_Evaluations)

#Filter only responses that we have search result data for:
Survey_Unrel$ALL_URLS <- as.character(Survey_Unrel$ALL_URLS)
Survey_Unrel$GS_Results <- ifelse(!is.na(Survey_Unrel$Mean_Scores) | nchar(Survey_Unrel$ALL_URLS) > 2,1,0)
Survey_Unrel <- Survey_Unrel %>% filter(GS_Results == 1)


#Create Dummy Variable for People who only saw very reliable news sites in Google Search Results (85):

Final_Mat <- matrix(ncol=5)
Only_Rel_URLs <- c()
Some_Unrel_URLs <- c()
Total_Rel_Sources <- c()
Total_Unrel_Sources <- c()
Total_Sources <- c()
for(i in 1:nrow(Survey_Unrel)){
  All_Scores <- unlist(strsplit(Survey_Unrel$List_Scores[i], split=", "))
  All_Scores <- as.numeric(All_Scores)
  Rel_Sources = 0
  Unrel_Sources = 0
  Tot_Sources = 0
  if(length(All_Scores) == 0){
    All_Scores = NA} else{
      for(x in 1:length(All_Scores)){
        Tot_Sources = Tot_Sources + 1
        if(All_Scores[x] > 85){
          Rel_Sources = Rel_Sources + 1
        } else{
          if(All_Scores[x] < 60){
            Unrel_Sources = Unrel_Sources + 1
          }
        }
      } 
      if(Rel_Sources > 0 & Rel_Sources == Tot_Sources){
        Only_Rel_Sources = 1
      } else{
        Only_Rel_Sources = 0
      }
      if(Unrel_Sources > 0){
        Some_Unrel_Sources = 1
      } else{
        Some_Unrel_Sources = 0
      }
    }
  Only_Rel_URLs = c(Only_Rel_URLs,Only_Rel_Sources)
  Some_Unrel_URLs = c(Some_Unrel_URLs,Some_Unrel_Sources)
  Total_Rel_Sources = c(Total_Rel_Sources,Rel_Sources)
  Total_Unrel_Sources = c(Total_Unrel_Sources,Unrel_Sources)
  Total_Sources = c(Total_Sources,Tot_Sources)
}

#Apply data to dataset:
Survey_Unrel$Only_Rel_URLs <- Only_Rel_URLs
Survey_Unrel$Some_Unrel_URLs <- Some_Unrel_URLs
Survey_Unrel$Total_Rel_Sources <- Total_Rel_Sources
Survey_Unrel$Total_Unrel_Sources <- Total_Unrel_Sources
Survey_Unrel$Total_Sources <- Total_Sources


#Filter only responses who only saw very reliable news sites in Google Search Results (85)
Survey_Unrel_1 <- Survey_Unrel %>% filter(Only_Rel_URLs == 1)

#Select variables:
Survey_Unrel_1 <- Survey_Unrel_1 %>% select(Article_day,True_Dummy,Four_Ordinal,Seven_Ordinal,Age,Gender,ResponseId,Education_Score,Income_Score,Ideo_Congruence)
Survey_Unrel_1$Treatment <- 1

#Only unique responses:
Survey_Unrel_1 <- unique(Survey_Unrel_1)

#Pull control data:
Control_Data <- read.csv('./Data/Control_Data_Study_5.csv')

#Merge data
Control_Data <- merge(Control_Data,FC_Ideo_Data,by='Article_day')

#Create Ideological Perspective data:
Control_Data <- Control_Data %>% mutate(Dummy_Ideology = ifelse(Ideology_Score > 0,'Conservative','Moderate'))
Control_Data <- Control_Data %>% mutate(Dummy_Ideology = ifelse(Ideology_Score < 0,'Liberal',Dummy_Ideology))
Control_Data <- Control_Data %>% mutate(Ideo_Congruence = ifelse(Dummy_Ideology == Article_Lean,1,0))

#Only keep those who had the web-tracking extension on while they evaluated articles:
Control_Data$Article_day <- as.character(Control_Data$Article_day)
Control_WT_Data <- read.csv('./Data/output_Control_Survey_2.csv')
Control_WT_Data$Article_day <- as.character(Control_WT_Data$Article_day)
Control_WT_Data$X <- NULL
Control_WT_Data <- unique(Control_WT_Data)
colnames(Control_WT_Data)[1] <- 'ResponseId'
Control_Data <- merge(Control_Data,Control_WT_Data,by=c('ResponseId','Article_day'))

#Filter false/misleading articles:
Control_Data <- Control_Data %>% filter(FC_Eval == 'FM')
Control_Data <- Control_Data %>% select(Article_day,True_Dummy,Four_Ordinal,Seven_Ordinal,Age,Gender,ResponseId,Education_Score,Income_Score,Ideo_Congruence)
Control_Data$Treatment <- 0

#Filter Control Data by robust mode:
Control_Data <- Control_Data %>% filter(Article_day %in% Robust_Articles)

#Merge treatment and control articles:
New_Data <- rbind(Survey_Unrel_1,Control_Data)
#Remove NAs
New_Data <- na.omit(New_Data)


#Run model:
fit_2_1 = glm(Seven_Ordinal ~ Treatment + Age + Gender + Education_Score + Income_Score + Ideo_Congruence + Article_day,data=New_Data)
#Produce confidence intervals with clustered standard errors:
CI_2_1 = coefci(fit_2_1, vcov. = vcovCL(fit_2_1, cluster = list(New_Data$ResponseId,New_Data$Article_day), type = "HC0"))

#Run model:
fit_2_2 = glm(Four_Ordinal ~ Treatment + Age + Gender + Education_Score + Income_Score + Ideo_Congruence + Article_day,data=New_Data)
#Produce confidence intervals with clustered standard errors:
CI_2_2 = coefci(fit_2_2, vcov. = vcovCL(fit_2_2, cluster = list(New_Data$ResponseId,New_Data$Article_day), type = "HC0"))

#Run model:
fit_2_3 = glm(True_Dummy ~ Treatment + Age + Gender + Education_Score + Income_Score + Ideo_Congruence + Article_day,data=New_Data)
#Produce confidence intervals with clustered standard errors:
CI_2_3 = coefci(fit_2_3, vcov. = vcovCL(fit_2_3, cluster = list(New_Data$ResponseId,New_Data$Article_day), type = "HC0"))

#Create empty matrix:
Final_Mat <- matrix(ncol=5)

#Merge data:
New_matr <- matrix(c(round(fit_2_1$coefficients[2]/sd(New_Data$Seven_Ordinal),4),round(CI_2_1[2,1]/sd(New_Data$Seven_Ordinal),4),round(CI_2_1[2,2]/sd(New_Data$Seven_Ordinal),4),'Ordinal (7)','Only Very Reliable News',
                     round(fit_2_2$coefficients[2]/sd(New_Data$Four_Ordinal),4),round(CI_2_2[2,1]/sd(New_Data$Four_Ordinal),4),round(CI_2_2[2,2]/sd(New_Data$Four_Ordinal),4),'Ordinal (4)','Only Very Reliable News',
                     round(fit_2_3$coefficients[2]/sd(New_Data$True_Dummy),4),round(CI_2_3[2,1]/sd(New_Data$True_Dummy),4),round(CI_2_3[2,2]/sd(New_Data$True_Dummy),4),'True (Dummy)','Only Very Reliable News'),ncol=5,byrow=T)
Final_Mat <- rbind(Final_Mat,New_matr)





#Filter only responses who saw some unreliable news sites:
Survey_Unrel_1 <- Survey_Unrel %>% filter(Some_Unrel_URLs == 1)

#Select Variables:
Survey_Unrel_1 <- Survey_Unrel_1 %>% select(Article_day,True_Dummy,Four_Ordinal,Seven_Ordinal,Age,Gender,ResponseId,Education_Score,Income_Score,Ideo_Congruence)
Survey_Unrel_1$Treatment <- 1
#Unique variables:
Survey_Unrel_1 <- unique(Survey_Unrel_1)

#Merge data:
New_Data <- rbind(Survey_Unrel_1,Control_Data)
#Remove NAs
New_Data <- na.omit(New_Data)



fit_2_1 = glm(Seven_Ordinal ~ Treatment + Age + Gender + Education_Score + Income_Score + Ideo_Congruence + Article_day,data=New_Data)
summary(fit_2_1)
#Produce clustere standard errors:
CI_2_1 = coefci(fit_2_1, vcov. = vcovCL(fit_2_1, cluster = list(New_Data$ResponseId,New_Data$Article_day), type = "HC0"))

fit_2_2 = glm(Four_Ordinal ~ Treatment + Age + Gender + Education_Score + Income_Score + Ideo_Congruence + Article_day,data=New_Data)
summary(fit_2_2)
#Produce clustere standard errors:
CI_2_2 = coefci(fit_2_2, vcov. = vcovCL(fit_2_2, cluster = list(New_Data$ResponseId,New_Data$Article_day), type = "HC0"))

fit_2_3 = glm(True_Dummy ~ Treatment + Age + Gender + Education_Score + Income_Score + Ideo_Congruence + Article_day,data=New_Data)
summary(fit_2_3)
#Produce clustere standard errors:
CI_2_3 = coefci(fit_2_3, vcov. = vcovCL(fit_2_3, cluster = list(New_Data$ResponseId,New_Data$Article_day), type = "HC0"))

New_matr <- matrix(c(round(fit_2_1$coefficients[2]/sd(New_Data$Seven_Ordinal),4),round(CI_2_1[2,1]/sd(New_Data$Seven_Ordinal),4),round(CI_2_1[2,2]/sd(New_Data$Seven_Ordinal),4),'Ordinal (7)','Some Very Unreliable News',
                     round(fit_2_2$coefficients[2]/sd(New_Data$Four_Ordinal),4),round(CI_2_2[2,1]/sd(New_Data$Four_Ordinal),4),round(CI_2_2[2,2]/sd(New_Data$Four_Ordinal),4),'Ordinal (4)','Some Very Unreliable News',
                     round(fit_2_3$coefficients[2]/sd(New_Data$True_Dummy),4),round(CI_2_3[2,1]/sd(New_Data$True_Dummy),4),round(CI_2_3[2,2]/sd(New_Data$True_Dummy),4),'True (Dummy)','Some Very Unreliable News'),ncol=5,byrow=T)

Final_Mat <- rbind(Final_Mat,New_matr)



#Filter only responses who didnt see an unreliable or reliable news sites:
Survey_Unrel_1 <- Survey_Unrel %>% filter(Total_Sources == 0)

#Select Variables:
Survey_Unrel_1 <- Survey_Unrel_1 %>% select(Article_day,True_Dummy,Four_Ordinal,Seven_Ordinal,Age,Gender,ResponseId,Education_Score,Income_Score,Ideo_Congruence)
Survey_Unrel_1$Treatment <- 1
#Unique variables:
Survey_Unrel_1 <- unique(Survey_Unrel_1)

#Merge data:
New_Data <- rbind(Survey_Unrel_1,Control_Data)
#Remove NAs
New_Data <- na.omit(New_Data)

fit_2_1 = glm(Seven_Ordinal ~ Treatment + Age + Gender + Education_Score + Income_Score + Ideo_Congruence + Article_day,data=New_Data)
summary(fit_2_1)
#Produce clustere standard errors:
CI_2_1 = coefci(fit_2_1, vcov. = vcovCL(fit_2_1, cluster = list(New_Data$ResponseId,New_Data$Article_day), type = "HC0"))

fit_2_2 = glm(Four_Ordinal ~ Treatment + Age + Gender + Education_Score + Income_Score + Ideo_Congruence + Article_day,data=New_Data)
summary(fit_2_2)
#Produce clustere standard errors:
CI_2_2 = coefci(fit_2_2, vcov. = vcovCL(fit_2_2, cluster = list(New_Data$ResponseId,New_Data$Article_day), type = "HC0"))

fit_2_3 = glm(True_Dummy ~ Treatment + Age + Gender + Education_Score + Income_Score + Ideo_Congruence + Article_day,data=New_Data)
summary(fit_2_3)
#Produce clustere standard errors:
CI_2_3 = coefci(fit_2_3, vcov. = vcovCL(fit_2_3, cluster = list(New_Data$ResponseId,New_Data$Article_day), type = "HC0"))

#Merge data with matrix:
New_matr <- matrix(c(round(fit_2_1$coefficients[2]/sd(New_Data$Seven_Ordinal),4),round(CI_2_1[2,1]/sd(New_Data$Seven_Ordinal),4),round(CI_2_1[2,2]/sd(New_Data$Seven_Ordinal),4),'Ordinal (7)','No News Sites',
                     round(fit_2_2$coefficients[2]/sd(New_Data$Four_Ordinal),4),round(CI_2_2[2,1]/sd(New_Data$Four_Ordinal),4),round(CI_2_2[2,2]/sd(New_Data$Four_Ordinal),4),'Ordinal (4)','No News Sites',
                     round(fit_2_3$coefficients[2]/sd(New_Data$True_Dummy),4),round(CI_2_3[2,1]/sd(New_Data$True_Dummy),4),round(CI_2_3[2,2]/sd(New_Data$True_Dummy),4),'True (Dummy)','No News Sites'),ncol=5,byrow=T)
Final_Mat <- rbind(Final_Mat,New_matr)


#Transform matrix into dataframe:
Final_Mat <- as.data.frame(Final_Mat)

#NAme matrix:
colnames(Final_Mat) <- c('Coef','Low_Conf','Upp_Conf','Measure','Type_News')

#Remove NAs
Final_Mat <- na.omit(Final_Mat)

#Clean up matrix:
Final_Mat$x<-c(0.8,0.9,1.0,1.4,1.5,1.6,2.0,2.1,2.2)
Final_Mat$Coef <- as.character(Final_Mat$Coef)
Final_Mat$Coef <- as.numeric(Final_Mat$Coef)
Final_Mat$Upp_Conf <- as.character(Final_Mat$Upp_Conf)
Final_Mat$Upp_Conf <- as.numeric(Final_Mat$Upp_Conf)
Final_Mat$Low_Conf <- as.character(Final_Mat$Low_Conf)
Final_Mat$Low_Conf <- as.numeric(Final_Mat$Low_Conf)


#Produce plot:
ggplot(data = Final_Mat, aes(x = x, y = Coef)) +
  geom_hline(aes(yintercept = 0), color = "gray",
             linetype = 2, size = 1.2) +
  geom_point(aes(color = Measure, shape=Measure),size=4) +
  geom_linerange(aes(min = Low_Conf, 
                     max = Upp_Conf, 
                     color = Measure),
                 size=1.5) +
  scale_color_manual(values=c('red','blue','purple'), name = "Measure") +
  ylab("\n Effect of Searching for Information Quality on Belief in Misinformation\n Dependent on Quality of Information Returned from Google Search Results \n(1 unit is 1 standard deviation of that measure) ") +
  theme_classic() +
  theme(axis.title.x = element_text(size=16),
        axis.text.x  = element_text(size=16),
        axis.title.y = element_text(size=16),
        axis.text.y  = element_text(size=16),
        plot.title = element_text(size = 16),
        legend.title = element_text(size=16),
        legend.text = element_text(size=14)) +
  ylim(-0.5,0.5) +
  scale_x_continuous(" \n",breaks=c(2.1,1.5,0.9),labels=c('No News Sites',
                                                          'Some Very Unreliable News',
                                                          'Only Very Reliable News'),limits=c(0.5,2.5)) +
  coord_flip()

#Save Figure:
ggsave('./Figures/Coefs_CIs_ROBUST.png',height=12,width=10)



################################################################################################################

########################################## Figure 2d: Coefs_CIs_2_ROBUST.png ##########################################

################################################################################################################

#Treatment Only - Subset By Quality of Google Results:
Survey_Unrel <- Combined_GS_Survey_Data

#Create string list of news sites scores:
Survey_Unrel$List_Scores <- as.character(Survey_Unrel$List_Scores)
Survey_Unrel$List_Scores <- gsub('\\[|\\]','',Survey_Unrel$List_Scores)
Survey_Unrel$List_Scores <- gsub('\\[|\\]','',Survey_Unrel$List_Scores)
Survey_Unrel <- Survey_Unrel %>% filter(FC_Eval == 'FM')


#Pull average reliability score of news viewed by respondents:
Mean_Scores_2 = c()
Rel_Maj_Dummy = c()
for(i in 1:nrow(Survey_Unrel)){
  All_Scores <- unlist(strsplit(Survey_Unrel$List_Scores[i], split=", "))
  All_Scores <- as.numeric(All_Scores)
  Rel_Sources = 0
  if(length(All_Scores) == 0){
    All_Scores = NA
    Rel_Maj_Dummy = c(Rel_Maj_Dummy,0)} else{
      for(x in 1:length(All_Scores)){
        if(All_Scores[x] > 59.5){
          Rel_Sources = Rel_Sources + 1
        }
      } 
      if(Rel_Sources > 5){
        Rel_Maj_Dummy = c(Rel_Maj_Dummy,1)
      } else{
        Rel_Maj_Dummy = c(Rel_Maj_Dummy,0)
      }
    }
  Mean_Scores_2 = c(Mean_Scores_2,mean(All_Scores))
}


#Create mean proportion and average reliability scores of news viewed:
Survey_Unrel$Mean_Scores <- Mean_Scores_2
Survey_Unrel_Mean <- Survey_Unrel %>% dplyr::group_by(ResponseId,Article_day) %>% dplyr::summarise(Mean_All_Scores = mean(Mean_Scores,na.rm=T))
Survey_Unrel_Prop <- Survey_Unrel %>% dplyr::group_by(ResponseId,Article_day) %>% dplyr::summarise(Mean_Prop_Unrel = mean(Prop_Unreliable,na.rm=T))

#Merge:
Survey_D <- merge(Survey_Unrel_Mean,Survey_Unrel_Prop,by=c('ResponseId','Article_day'))
Survey_Unrel <- merge(Survey_Unrel,Survey_D,by=c('ResponseId','Article_day'))
Survey_Unrel <- Survey_Unrel %>% filter(Total_Sources != 0)


#Filter by quartile:
Survey_Unrel_1 <- Survey_Unrel %>% filter(Mean_All_Scores < quantile(Survey_Unrel$Mean_All_Scores,na.rm=T)[2])

#Select variables
Survey_Unrel_1 <- Survey_Unrel_1 %>% select(Article_day,True_Dummy,Four_Ordinal,Seven_Ordinal,Age,Gender,ResponseId,Education_Score,Income_Score,Ideo_Congruence)
Survey_Unrel_1$Treatment <- 1

#Create dataframe:
Survey_Unrel_1 <- unique(Survey_Unrel_1)
Survey_Unrel_1 <- as.data.frame(Survey_Unrel_1)

#Merge Treatment and Control data
New_Data <- rbind(Survey_Unrel_1,Control_Data)
#Remove NA variables:
New_Data <- na.omit(New_Data)

#Run Model:
fit_2_1 = glm(Seven_Ordinal ~ Treatment + Age + Gender + Education_Score + Income_Score + Ideo_Congruence + Article_day,data=New_Data)
#Produce confidence interval with clustered standard errors:
CI_2_1 = coefci(fit_2_1, vcov. = vcovCL(fit_2_1, cluster = list(New_Data$ResponseId,New_Data$Article_day), type = "HC0"))

#Run Model:
fit_2_2 = glm(Four_Ordinal ~ Treatment + Age + Gender + Education_Score + Income_Score + Ideo_Congruence + Article_day,data=New_Data)
#Produce confidence interval with clustered standard errors:
CI_2_2 = coefci(fit_2_2, vcov. = vcovCL(fit_2_2, cluster = list(New_Data$ResponseId,New_Data$Article_day), type = "HC0"))

#Run Model:
fit_2_3 = glm(True_Dummy ~ Treatment + Age + Gender + Education_Score + Income_Score + Ideo_Congruence + Article_day,data=New_Data)
#Produce confidence interval with clustered standard errors:
CI_2_3 = coefci(fit_2_3, vcov. = vcovCL(fit_2_3, cluster = list(New_Data$ResponseId,New_Data$Article_day), type = "HC0"))

#Merge Coeficients and confidence intervals:

Final_Mat <- matrix(ncol=5)
New_matr <- matrix(c(round(fit_2_1$coefficients[2]/sd(New_Data$Seven_Ordinal),4),round(CI_2_1[2,1]/sd(New_Data$Seven_Ordinal),4),round(CI_2_1[2,2]/sd(New_Data$Seven_Ordinal),4),'Ordinal (7)','0-25%',
                     round(fit_2_2$coefficients[2]/sd(New_Data$Four_Ordinal),4),round(CI_2_2[2,1]/sd(New_Data$Four_Ordinal),4),round(CI_2_2[2,2]/sd(New_Data$Four_Ordinal),4),'Ordinal (4)','0-25%',
                     round(fit_2_3$coefficients[2]/sd(New_Data$True_Dummy),4),round(CI_2_3[2,1]/sd(New_Data$True_Dummy),4),round(CI_2_3[2,2]/sd(New_Data$True_Dummy),4),'True (Dummy)','0-25%'),ncol=5,byrow=T)

Final_Mat <- rbind(Final_Mat,New_matr)


#Filter by quartile:
Survey_Unrel_1 <- Survey_Unrel %>% filter(Mean_All_Scores >= quantile(Survey_Unrel$Mean_All_Scores,na.rm=T)[2] & Mean_All_Scores < quantile(Survey_Unrel$Mean_All_Scores,na.rm=T)[3])


#Select variables
Survey_Unrel_1 <- Survey_Unrel_1 %>% select(Article_day,True_Dummy,Four_Ordinal,Seven_Ordinal,Age,Gender,ResponseId,Education_Score,Income_Score,Ideo_Congruence)
Survey_Unrel_1$Treatment <- 1

#Create dataframe:
Survey_Unrel_1 <- unique(Survey_Unrel_1)
Survey_Unrel_1 <- as.data.frame(Survey_Unrel_1)

#Merge Treatment and Control data
New_Data <- rbind(Survey_Unrel_1,Control_Data)
#Remove NA variables:
New_Data <- na.omit(New_Data)

#Run Model:
fit_2_1 = glm(Seven_Ordinal ~ Treatment + Age + Gender + Education_Score + Income_Score + Ideo_Congruence + Article_day,data=New_Data)
#Produce confidence interval with clustered standard errors:
CI_2_1 = coefci(fit_2_1, vcov. = vcovCL(fit_2_1, cluster = list(New_Data$ResponseId,New_Data$Article_day), type = "HC0"))

#Run Model:
fit_2_2 = glm(Four_Ordinal ~ Treatment + Age + Gender + Education_Score + Income_Score + Ideo_Congruence + Article_day,data=New_Data)
#Produce confidence interval with clustered standard errors:
CI_2_2 = coefci(fit_2_2, vcov. = vcovCL(fit_2_2, cluster = list(New_Data$ResponseId,New_Data$Article_day), type = "HC0"))

#Run Model:
fit_2_3 = glm(True_Dummy ~ Treatment + Age + Gender + Education_Score + Income_Score + Ideo_Congruence + Article_day,data=New_Data)
#Produce confidence interval with clustered standard errors:
CI_2_3 = coefci(fit_2_3, vcov. = vcovCL(fit_2_3, cluster = list(New_Data$ResponseId,New_Data$Article_day), type = "HC0"))

#Merge Coeficients and confidence intervals:
New_matr <- matrix(c(round(fit_2_1$coefficients[2]/sd(New_Data$Seven_Ordinal),4),round(CI_2_1[2,1]/sd(New_Data$Seven_Ordinal),4),round(CI_2_1[2,2]/sd(New_Data$Seven_Ordinal),4),'Ordinal (7)','25-50%',
                     round(fit_2_2$coefficients[2]/sd(New_Data$Four_Ordinal),4),round(CI_2_2[2,1]/sd(New_Data$Four_Ordinal),4),round(CI_2_2[2,2]/sd(New_Data$Four_Ordinal),4),'Ordinal (4)','25-50%',
                     round(fit_2_3$coefficients[2]/sd(New_Data$True_Dummy),4),round(CI_2_3[2,1]/sd(New_Data$True_Dummy),4),round(CI_2_3[2,2]/sd(New_Data$True_Dummy),4),'True (Dummy)','25-50%'),ncol=5,byrow=T)

Final_Mat <- rbind(Final_Mat,New_matr)



#Filter by quartile:
Survey_Unrel_1 <- Survey_Unrel %>% filter(Mean_All_Scores >= quantile(Survey_Unrel$Mean_All_Scores,na.rm=T)[3] & Mean_All_Scores < quantile(Survey_Unrel$Mean_All_Scores,na.rm=T)[4])

#Select variables
Survey_Unrel_1 <- Survey_Unrel_1 %>% select(Article_day,True_Dummy,Four_Ordinal,Seven_Ordinal,Age,Gender,ResponseId,Education_Score,Income_Score,Ideo_Congruence)
Survey_Unrel_1$Treatment <- 1

#Create dataframe:
Survey_Unrel_1 <- unique(Survey_Unrel_1)
Survey_Unrel_1 <- as.data.frame(Survey_Unrel_1)

#Merge Treatment and Control data
New_Data <- rbind(Survey_Unrel_1,Control_Data)
#Remove NA variables:
New_Data <- na.omit(New_Data)

#Run Model:
fit_2_1 = glm(Seven_Ordinal ~ Treatment + Age + Gender + Education_Score + Income_Score + Ideo_Congruence + Article_day,data=New_Data)
#Produce confidence interval with clustered standard errors:
CI_2_1 = coefci(fit_2_1, vcov. = vcovCL(fit_2_1, cluster = list(New_Data$ResponseId,New_Data$Article_day), type = "HC0"))

#Run Model:
fit_2_2 = glm(Four_Ordinal ~ Treatment + Age + Gender + Education_Score + Income_Score + Ideo_Congruence + Article_day,data=New_Data)
#Produce confidence interval with clustered standard errors:
CI_2_2 = coefci(fit_2_2, vcov. = vcovCL(fit_2_2, cluster = list(New_Data$ResponseId,New_Data$Article_day), type = "HC0"))

#Run Model:
fit_2_3 = glm(True_Dummy ~ Treatment + Age + Gender + Education_Score + Income_Score + Ideo_Congruence + Article_day,data=New_Data)
#Produce confidence interval with clustered standard errors:
CI_2_3 = coefci(fit_2_3, vcov. = vcovCL(fit_2_3, cluster = list(New_Data$ResponseId,New_Data$Article_day), type = "HC0"))


New_matr <- matrix(c(round(fit_2_1$coefficients[2]/sd(New_Data$Seven_Ordinal),4),round(CI_2_1[2,1]/sd(New_Data$Seven_Ordinal),4),round(CI_2_1[2,2]/sd(New_Data$Seven_Ordinal),4),'Ordinal (7)','50-75%',
                     round(fit_2_2$coefficients[2]/sd(New_Data$Four_Ordinal),4),round(CI_2_2[2,1]/sd(New_Data$Four_Ordinal),4),round(CI_2_2[2,2]/sd(New_Data$Four_Ordinal),4),'Ordinal (4)','50-75%',
                     round(fit_2_3$coefficients[2]/sd(New_Data$True_Dummy),4),round(CI_2_3[2,1]/sd(New_Data$True_Dummy),4),round(CI_2_3[2,2]/sd(New_Data$True_Dummy),4),'True (Dummy)','50-75%'),ncol=5,byrow=T)

Final_Mat <- rbind(Final_Mat,New_matr)


#Filter by quartile:
Survey_Unrel_1 <- Survey_Unrel %>% filter(Mean_All_Scores >= quantile(Survey_Unrel$Mean_All_Scores,na.rm=T)[4] & Mean_All_Scores <= 100)

#Select variables
Survey_Unrel_1 <- Survey_Unrel_1 %>% select(Article_day,True_Dummy,Four_Ordinal,Seven_Ordinal,Age,Gender,ResponseId,Education_Score,Income_Score,Ideo_Congruence)
Survey_Unrel_1$Treatment <- 1

#Create dataframe:
Survey_Unrel_1 <- unique(Survey_Unrel_1)
Survey_Unrel_1 <- as.data.frame(Survey_Unrel_1)

#Merge Treatment and Control data
New_Data <- rbind(Survey_Unrel_1,Control_Data)
#Remove NA variables:
New_Data <- na.omit(New_Data)

#Run Model:
fit_2_1 = glm(Seven_Ordinal ~ Treatment + Age + Gender + Education_Score + Income_Score + Ideo_Congruence + Article_day,data=New_Data)
#Produce confidence interval with clustered standard errors:
CI_2_1 = coefci(fit_2_1, vcov. = vcovCL(fit_2_1, cluster = list(New_Data$ResponseId,New_Data$Article_day), type = "HC0"))

#Run Model:
fit_2_2 = glm(Four_Ordinal ~ Treatment + Age + Gender + Education_Score + Income_Score + Ideo_Congruence + Article_day,data=New_Data)
#Produce confidence interval with clustered standard errors:
CI_2_2 = coefci(fit_2_2, vcov. = vcovCL(fit_2_2, cluster = list(New_Data$ResponseId,New_Data$Article_day), type = "HC0"))

#Run Model:
fit_2_3 = glm(True_Dummy ~ Treatment + Age + Gender + Education_Score + Income_Score + Ideo_Congruence + Article_day,data=New_Data)
#Produce confidence interval with clustered standard errors:
CI_2_3 = coefci(fit_2_3, vcov. = vcovCL(fit_2_3, cluster = list(New_Data$ResponseId,New_Data$Article_day), type = "HC0"))


New_matr <- matrix(c(round(fit_2_1$coefficients[2]/sd(New_Data$Seven_Ordinal),4),round(CI_2_1[2,1]/sd(New_Data$Seven_Ordinal),4),round(CI_2_1[2,2]/sd(New_Data$Seven_Ordinal),4),'Ordinal (7)','75-100%',
                     round(fit_2_2$coefficients[2]/sd(New_Data$Four_Ordinal),4),round(CI_2_2[2,1]/sd(New_Data$Four_Ordinal),4),round(CI_2_2[2,2]/sd(New_Data$Four_Ordinal),4),'Ordinal (4)','75-100%',
                     round(fit_2_3$coefficients[2]/sd(New_Data$True_Dummy),4),round(CI_2_3[2,1]/sd(New_Data$True_Dummy),4),round(CI_2_3[2,2]/sd(New_Data$True_Dummy),4),'True (Dummy)','75-100%'),ncol=5,byrow=T)

Final_Mat <- rbind(Final_Mat,New_matr)


#Create dataframe to produce plot
Final_Mat <- as.data.frame(Final_Mat)
Final_Mat <- na.omit(Final_Mat)
colnames(Final_Mat) <- c('Coef','Low_Conf','Upp_Conf','Measure','Type_News')
Final_Mat <- na.omit(Final_Mat)
Final_Mat$x<-c(0.8,0.9,1.0,1.4,1.5,1.6,2.0,2.1,2.2,2.6,2.7,2.8)
Final_Mat$Coef <- as.character(Final_Mat$Coef)
Final_Mat$Coef <- as.numeric(Final_Mat$Coef)
Final_Mat$Upp_Conf <- as.character(Final_Mat$Upp_Conf)
Final_Mat$Upp_Conf <- as.numeric(Final_Mat$Upp_Conf)
Final_Mat$Low_Conf <- as.character(Final_Mat$Low_Conf)
Final_Mat$Low_Conf <- as.numeric(Final_Mat$Low_Conf)


#Produce Plot:
ggplot(data = Final_Mat, aes(x = x, y = Coef)) +
  geom_hline(aes(yintercept = 0), color = "gray",
             linetype = 2, size = 1.2) +
  geom_point(aes(color = Measure, shape=Measure),size=4) +
  geom_linerange(aes(min = Low_Conf, 
                     max = Upp_Conf, 
                     color = Measure),
                 size=1.5) +
  scale_color_manual(values=c('red','blue','purple'), name = "Measure") +
  ylab("\n Effect of Searching for Information Quality on Belief in Misinformation\n Dependent on Quality of Information Returned from Google Search Results \n(1 unit is 1 standard deviation of that measure) ") +
  theme_classic() +
  theme(axis.title.x = element_text(size=16),
        axis.text.x  = element_text(size=16),
        axis.title.y = element_text(size=16),
        axis.text.y  = element_text(size=16),
        plot.title = element_text(size = 16),
        legend.title = element_text(size=16),
        legend.text = element_text(size=14)) +
  ylim(-0.7,0.7) +
  scale_x_continuous(" \n",breaks=c(2.7,2.1,1.5,0.9),labels=c('75-100%',
                                                              '50-75%',
                                                              '25-50%',
                                                              '0-25%'),limits=c(0.5,3.0)) +
  coord_flip()

#Save Figure:
ggsave('./Figures/Coefs_CIs_2_ROBUST.png',height=12,width=10)



################################################################################################################

################################# Figure 3: Coefs_CIs_Predicting_Unrel_Dummy_ROBUST.png ###############################

################################################################################################################


#Treatment Only - Subset By Quality of Google Results:
Survey_Unrel <- Combined_GS_Survey_Data

#Create string list of news sites scores:
Survey_Unrel$List_Scores <- as.character(Survey_Unrel$List_Scores)
Survey_Unrel$List_Scores <- gsub('\\[|\\]','',Survey_Unrel$List_Scores)
Survey_Unrel$List_Scores <- gsub('\\[|\\]','',Survey_Unrel$List_Scores)
Survey_Unrel <- Survey_Unrel %>% filter(FC_Eval == 'FM')



#Create Average Score by Respondent:
Survey_Evaluations <- Survey_Unrel %>% filter(!is.na(Mean_Score))
Survey_Evaluations <- Survey_Evaluations %>% select(ResponseId,Article_day,List_Scores)
Respondent_Evaluations <- Survey_Evaluations %>% select(ResponseId,Article_day)
Respondent_Evaluations <- unique(Respondent_Evaluations)

#Blank matrix:
Search_Results_DF <- matrix(ncol=4)
colnames(Search_Results_DF) <- c('ResponseId',
                                 'Article_day',
                                 'Mean_Score_Final',
                                 'Prop_Unreliable_Final')

#For loop that creates average reliability by Google Search engine results:
for(i in 1:nrow(Respondent_Evaluations)){
  Resp <- Respondent_Evaluations$ResponseId[i]
  Article <- Respondent_Evaluations$Article_day[i]  
  df_survey <- Survey_Evaluations %>% filter(ResponseId == Resp & Article_day == Article)
  All_Scores <- paste(df_survey$List_Scores,collapse=', ')
  All_Scores <- unlist(strsplit(All_Scores, split=", "))
  All_Scores <- as.numeric(All_Scores)
  Mean_Reliability <- mean(All_Scores)
  Total_links <- length(All_Scores)
  Total_Unrel <- 0
  for(x in 1:length(All_Scores)){
    if(All_Scores[x] < 60){
      Total_Unrel = Total_Unrel + 1
    }
  }
  Proportion_Unrel = Total_Unrel/Total_links
  
  new_df <- matrix(c(Resp,Article,Mean_Reliability,Proportion_Unrel),ncol=4)
  colnames(new_df) <- c('ResponseId',
                        'Article_day',
                        'Mean_Score_Final',
                        'Prop_Unreliable_Final')
  
  
  Search_Results_DF <- rbind(Search_Results_DF,
                             new_df)
}



#Add mean reliability scores and proportion of unreliable news:
Search_Results_DF <- as.data.frame(Search_Results_DF)
Search_Results_DF$Mean_Score_Final <- as.character(Search_Results_DF$Mean_Score_Final)
Search_Results_DF$Mean_Score_Final <- as.numeric(Search_Results_DF$Mean_Score_Final)
Search_Results_DF$Prop_Unreliable_Final <- as.character(Search_Results_DF$Prop_Unreliable_Final)
Search_Results_DF$Prop_Unreliable_Final <- as.numeric(Search_Results_DF$Prop_Unreliable_Final)

#Select from treatment data:
Survey_Unrel_1 <- Survey_Unrel %>% select(Article_day,True_Dummy,Four_Ordinal,Seven_Ordinal,Age,Gender,ResponseId,Education_Score,Income_Score,Ideo_Congruence,Dummy_Ideology,Article_Lean,Dig_Lit_Score)
Survey_Unrel_1 <- unique(Survey_Unrel_1)

#Merge survey and web-tracking data:
Search_Survey <- merge(Survey_Unrel_1,Search_Results_DF,by=c('ResponseId','Article_day'))

#Create unreliable news exposure dummy variable:
Search_Survey$Unreliable_Dummy <- ifelse(Search_Survey$Prop_Unreliable_Final > 0,1,0)

fit_2_3 = glm(Unreliable_Dummy ~ Age + Gender + Education_Score + Income_Score + Ideo_Congruence +Dig_Lit_Score + Article_day,data=Search_Survey)
#Produce cionfidence intervals with clustered standard errors:
CI_2_3 = coefci(fit_2_3, vcov. = vcovCL(fit_2_3, cluster = list(Search_Survey$ResponseId,Search_Survey$Article_day), type = "HC0"))





#Create dataframe with coefficients and confidence intervals:
Coefficients <- c(round(fit_2_3$coefficients[2]*sd(Search_Survey$Age),4),
                  round(fit_2_3$coefficients[3]*sd(Search_Survey$Gender),4),
                  round(fit_2_3$coefficients[4]*sd(Search_Survey$Education_Score),4),
                  round(fit_2_3$coefficients[5]*sd(Search_Survey$Income_Score),4),
                  round(fit_2_3$coefficients[6]*sd(Search_Survey$Ideo_Congruence),4),
                  round(fit_2_3$coefficients[7]*sd(Search_Survey$Dig_Lit_Score),4))

CI_Upper <- c(round(CI_2_3[2,1]*sd(Search_Survey$Age),4),
              round(CI_2_3[3,1]*sd(Search_Survey$Gender),4),
              round(CI_2_3[4,1]*sd(Search_Survey$Education_Score),4),
              round(CI_2_3[5,1]*sd(Search_Survey$Income_Score),4),
              round(CI_2_3[6,1]*sd(Search_Survey$Ideo_Congruence),4),
              round(CI_2_3[7,1]*sd(Search_Survey$Dig_Lit_Score),4))            

CI_Lower <- c(round(CI_2_3[2,2]*sd(Search_Survey$Age),4),
              round(CI_2_3[3,2]*sd(Search_Survey$Gender),4),
              round(CI_2_3[4,2]*sd(Search_Survey$Education_Score),4),
              round(CI_2_3[5,2]*sd(Search_Survey$Income_Score),4),
              round(CI_2_3[6,2]*sd(Search_Survey$Ideo_Congruence),4),
              round(CI_2_3[7,2]*sd(Search_Survey$Dig_Lit_Score),4))           

Coef_names <- c('Age',
                'Gender',
                'Education',
                'Income',
                'Ideological\nCongruence',
                'Digital Literacy')

d_matrix <- cbind(Coef_names,Coefficients,CI_Upper,CI_Lower)
rownames(d_matrix) <- c()
d_matrix <- data.frame(d_matrix)
d_matrix$Coefficients <- as.character(d_matrix$Coefficients)
d_matrix$CI_Lower <- as.character(d_matrix$CI_Lower)
d_matrix$CI_Upper <- as.character(d_matrix$CI_Upper)
d_matrix$Coefficients <- as.numeric(d_matrix$Coefficients)
d_matrix$CI_Lower <- as.numeric(d_matrix$CI_Lower)
d_matrix$CI_Upper <- as.numeric(d_matrix$CI_Upper)
d_matrix <- d_matrix %>% arrange(desc(row_number()))
d_matrix$x<-c(0.1,0.2,0.3,0.4,0.5,0.6)

#Produce plot:
ggplot(data = d_matrix, aes(x = x, y = Coefficients)) +
  geom_hline(aes(yintercept = 0), color = "gray",
             linetype = 2, size = 1.2) +
  geom_point(size=4) +
  geom_linerange(aes(min = CI_Lower,
                     max = CI_Upper),
                 size=1.5) +
  scale_color_manual(values=c('red','blue','purple'), name = "Period") +
  ylab("\n The Effect of a One Standard Deviation Increase of Ind. Variable\n on Probability of Exposure to Unreliable News Site") +
  theme_classic() +
  theme(axis.title.x = element_text(size=22),
        axis.text.x  = element_text(size=20),
        axis.title.y = element_text(size=20),
        axis.text.y  = element_text(size=22),
        plot.title = element_text(size = 20),
        legend.title = element_text(size=20),
        legend.text = element_text(size=20)) +
  ylim(-0.1,0.15) +
  scale_x_continuous(" \n",breaks=c(0.1,0.2,0.3,0.4,0.5,0.6),labels=Coef_names,limits=c(0.0,0.7)) +
  coord_flip()

#Save figure:
ggsave('./Figures/Coefs_CIs_Predicting_Unrel_Dummy_ROBUST.png',height=8,width=12)


################################################################################################################

################################ Figure 4b: Quantiles_High_DL_FULL_ROBUST.png ##########################################

################################################################################################################

#Treatment Only - Subset By Quality of Google Results:
Survey_Unrel <- Combined_GS_Survey_Data

#Create string with reliability scores:
Survey_Unrel$List_Scores <- as.character(Survey_Unrel$List_Scores)
Survey_Unrel$List_Scores <- gsub('\\[|\\]','',Survey_Unrel$List_Scores)
Survey_Unrel$List_Scores <- gsub('\\[|\\]','',Survey_Unrel$List_Scores)
Survey_Unrel <- Survey_Unrel %>% filter(FC_Eval == 'FM')

#Create dummy variable with Google Search Results
Survey_Unrel$ALL_URLS <- as.character(Survey_Unrel$ALL_URLS)
Survey_Unrel$GS_Results <- ifelse(nchar(Survey_Unrel$ALL_URLS) > 2,1,0)
Survey_Unrel <- Survey_Unrel %>% filter(GS_Results == 1)


#Create Variables with exposure varaibles:

#New Blank matrix
Final_Mat <- matrix(ncol=4)

#For loop that calculates variables:
Only_Rel_URLs <- c()
Some_Unrel_URLs <- c()
Total_Rel_Sources <- c()
Total_Unrel_Sources <- c()
Total_Sources <- c()
for(i in 1:nrow(Survey_Unrel)){
  All_Scores <- unlist(strsplit(Survey_Unrel$List_Scores[i], split=", "))
  All_Scores <- as.numeric(All_Scores)
  Rel_Sources = 0
  Unrel_Sources = 0
  Tot_Sources = 0
  if(length(All_Scores) == 0){
    All_Scores = NA} else{
      for(x in 1:length(All_Scores)){
        Tot_Sources = Tot_Sources + 1
        if(All_Scores[x] > 85){
          Rel_Sources = Rel_Sources + 1
        } else{
          if(All_Scores[x] < 60){
            Unrel_Sources = Unrel_Sources + 1
          }
        }
      } 
      if(Rel_Sources > 0 & Rel_Sources == Tot_Sources){
        Only_Rel_Sources = 1
      } else{
        Only_Rel_Sources = 0
      }
      if(Unrel_Sources > 0){
        Some_Unrel_Sources = 1
      } else{
        Some_Unrel_Sources = 0
      }
    }
  Only_Rel_URLs = c(Only_Rel_URLs,Only_Rel_Sources)
  Some_Unrel_URLs = c(Some_Unrel_URLs,Some_Unrel_Sources)
  Total_Rel_Sources = c(Total_Rel_Sources,Rel_Sources)
  Total_Unrel_Sources = c(Total_Unrel_Sources,Unrel_Sources)
  Total_Sources = c(Total_Sources,Tot_Sources)
}

#Add variables to dataframe:
Survey_Unrel$Only_Rel_URLs <- Only_Rel_URLs
Survey_Unrel$Some_Unrel_URLs <- Some_Unrel_URLs
Survey_Unrel$Total_Rel_Sources <- Total_Rel_Sources
Survey_Unrel$Total_Unrel_Sources <- Total_Unrel_Sources
Survey_Unrel$Total_Sources <- Total_Sources


#Filter by those exposed to unreliable news sites:
Survey_Unrel_1 <- Survey_Unrel %>% filter(Some_Unrel_URLs == 1)

#Select variables:
Survey_Unrel_1 <- Survey_Unrel_1 %>% select(Article_day,True_Dummy,Four_Ordinal,Seven_Ordinal,Age,Gender,ResponseId,Education_Score,Income_Score,Ideo_Congruence,Dummy_Ideology,Article_Lean,Dig_Lit_Score)
Survey_Unrel_1$Treatment <- 1

#Create dataframe with unique response:
Survey_Unrel_1 <- unique(Survey_Unrel_1)
Survey_Unrel_1 <- as.data.frame(Survey_Unrel_1)




#Pull control data:
Control_Data <- read.csv('./Data/Control_Data_Study_5.csv')

#Merge data
Control_Data <- merge(Control_Data,FC_Ideo_Data,by='Article_day')

#Create Ideological Perspective data:
Control_Data <- Control_Data %>% mutate(Dummy_Ideology = ifelse(Ideology_Score > 0,'Conservative','Moderate'))
Control_Data <- Control_Data %>% mutate(Dummy_Ideology = ifelse(Ideology_Score < 0,'Liberal',Dummy_Ideology))
Control_Data <- Control_Data %>% mutate(Ideo_Congruence = ifelse(Dummy_Ideology == Article_Lean,1,0))

#Only keep those who had the web-tracking extension on while they evaluated articles:
Control_Data$Article_day <- as.character(Control_Data$Article_day)
Control_WT_Data <- read.csv('./Data/output_Control_Survey_2.csv')
Control_WT_Data$Article_day <- as.character(Control_WT_Data$Article_day)
Control_WT_Data$X <- NULL
Control_WT_Data <- unique(Control_WT_Data)
colnames(Control_WT_Data)[1] <- 'ResponseId'
Control_Data <- merge(Control_Data,Control_WT_Data,by=c('ResponseId','Article_day'))

#Filter false/misleading articles:
Control_Data <- Control_Data %>% filter(FC_Eval == 'FM')
Control_Data <- Control_Data %>% select(Article_day,True_Dummy,Four_Ordinal,Seven_Ordinal,Age,Gender,ResponseId,Education_Score,Income_Score,Ideo_Congruence,Dummy_Ideology,Article_Lean,Dig_Lit_Score)
Control_Data$Treatment <- 0

#Combine data:
New_Data <- rbind(Survey_Unrel_1,Control_Data)

#filter digital literacy:
New_Data <- New_Data %>% filter(Dig_Lit_Score >= median(New_Data$Dig_Lit_Score))

#Remove NAs
New_Data <- na.omit(New_Data)


#Run Model
fit_2_1 = glm(Seven_Ordinal ~ Treatment + Age + Gender + Education_Score + Income_Score + Ideo_Congruence + Article_day,data=New_Data)
#Produce confidence intervals with clustered standard errors:
CI_2_1 = coefci(fit_2_1, vcov. = vcovCL(fit_2_1, cluster = list(New_Data$ResponseId,New_Data$Article_day), type = "HC0"))

#Run Model
fit_2_2 = glm(Four_Ordinal ~ Treatment + Age + Gender + Education_Score + Income_Score + Ideo_Congruence + Article_day,data=New_Data)
#Produce confidence intervals with clustered standard errors:
CI_2_2 = coefci(fit_2_2, vcov. = vcovCL(fit_2_2, cluster = list(New_Data$ResponseId,New_Data$Article_day), type = "HC0"))

#Run Model
fit_2_3 = glm(True_Dummy ~ Treatment + Age + Gender + Education_Score + Income_Score + Ideo_Congruence + Article_day,data=New_Data)
#Produce confidence intervals with clustered standard errors:
CI_2_3 = coefci(fit_2_3, vcov. = vcovCL(fit_2_3, cluster = list(New_Data$ResponseId,New_Data$Article_day), type = "HC0"))


Final_Mat <- matrix(ncol=5)


New_matr <- matrix(c(round(fit_2_1$coefficients[2]/sd(New_Data$Seven_Ordinal),4),round(CI_2_1[2,1]/sd(New_Data$Seven_Ordinal),4),round(CI_2_1[2,2]/sd(New_Data$Seven_Ordinal),4),'Ordinal (7)','Some Very\nUnreliable News',
                     round(fit_2_2$coefficients[2]/sd(New_Data$Four_Ordinal),4),round(CI_2_2[2,1]/sd(New_Data$Four_Ordinal),4),round(CI_2_2[2,2]/sd(New_Data$Four_Ordinal),4),'Ordinal (4)','Some Very\nUnreliable News',
                     round(fit_2_3$coefficients[2]/sd(New_Data$True_Dummy),4),round(CI_2_3[2,1]/sd(New_Data$True_Dummy),4),round(CI_2_3[2,2]/sd(New_Data$True_Dummy),4),'True (Dummy)','Some Very\nUnreliable News'),ncol=5,byrow=T)

Final_Mat <- rbind(Final_Mat,New_matr)


#Filter:
Survey_Unrel_1 <- Survey_Unrel %>% filter(Only_Rel_URLs == 1)

#Select variables:
Survey_Unrel_1 <- Survey_Unrel_1 %>% select(Article_day,True_Dummy,Four_Ordinal,Seven_Ordinal,Age,Gender,ResponseId,Education_Score,Income_Score,Ideo_Congruence,Dummy_Ideology,Article_Lean,Dig_Lit_Score)
Survey_Unrel_1$Treatment <- 1

#Create dataframe with unique response:
Survey_Unrel_1 <- unique(Survey_Unrel_1)
Survey_Unrel_1 <- as.data.frame(Survey_Unrel_1)

#Combine data:
New_Data <- rbind(Survey_Unrel_1,Control_Data)

#filter digital literacy:
New_Data <- New_Data %>% filter(Dig_Lit_Score >= median(New_Data$Dig_Lit_Score))

#Remove NAs
New_Data <- na.omit(New_Data)


#Run Model
fit_2_1 = glm(Seven_Ordinal ~ Treatment + Age + Gender + Education_Score + Income_Score + Ideo_Congruence + Article_day,data=New_Data)
#Produce confidence intervals with clustered standard errors:
CI_2_1 = coefci(fit_2_1, vcov. = vcovCL(fit_2_1, cluster = list(New_Data$ResponseId,New_Data$Article_day), type = "HC0"))

#Run Model
fit_2_2 = glm(Four_Ordinal ~ Treatment + Age + Gender + Education_Score + Income_Score + Ideo_Congruence + Article_day,data=New_Data)
#Produce confidence intervals with clustered standard errors:
CI_2_2 = coefci(fit_2_2, vcov. = vcovCL(fit_2_2, cluster = list(New_Data$ResponseId,New_Data$Article_day), type = "HC0"))

#Run Model
fit_2_3 = glm(True_Dummy ~ Treatment + Age + Gender + Education_Score + Income_Score + Ideo_Congruence + Article_day,data=New_Data)
#Produce confidence intervals with clustered standard errors:
CI_2_3 = coefci(fit_2_3, vcov. = vcovCL(fit_2_3, cluster = list(New_Data$ResponseId,New_Data$Article_day), type = "HC0"))

#Add to matrix:
New_matr <- matrix(c(round(fit_2_1$coefficients[2]/sd(New_Data$Seven_Ordinal),4),round(CI_2_1[2,1]/sd(New_Data$Seven_Ordinal),4),round(CI_2_1[2,2]/sd(New_Data$Seven_Ordinal),4),'Ordinal (7)','Only Very\nReliable News',
                     round(fit_2_2$coefficients[2]/sd(New_Data$Four_Ordinal),4),round(CI_2_2[2,1]/sd(New_Data$Four_Ordinal),4),round(CI_2_2[2,2]/sd(New_Data$Four_Ordinal),4),'Ordinal (4)','Only Very\nReliable News',
                     round(fit_2_3$coefficients[2]/sd(New_Data$True_Dummy),4),round(CI_2_3[2,1]/sd(New_Data$True_Dummy),4),round(CI_2_3[2,2]/sd(New_Data$True_Dummy),4),'True (Dummy)','Only Very\nReliable News'),ncol=5,byrow=T)
Final_Mat <- rbind(Final_Mat,New_matr)


#Use dataframe to create plot:
Final_Mat <- as.data.frame(Final_Mat)
Final_Mat <- na.omit(Final_Mat)
colnames(Final_Mat) <- c('Coef','Low_Conf','Upp_Conf','Measure','Type_News')
Final_Mat <- na.omit(Final_Mat)

Final_Mat$x<- c(0.8,0.9,1.0,1.4,1.5,1.6)
Final_Mat$Coef <- as.character(Final_Mat$Coef)
Final_Mat$Coef <- as.numeric(Final_Mat$Coef)
Final_Mat$Upp_Conf <- as.character(Final_Mat$Upp_Conf)
Final_Mat$Upp_Conf <- as.numeric(Final_Mat$Upp_Conf)
Final_Mat$Low_Conf <- as.character(Final_Mat$Low_Conf)
Final_Mat$Low_Conf <- as.numeric(Final_Mat$Low_Conf)
Final_Mat$Type_News <- factor(Final_Mat$Type_News,levels=c('Some Very\nUnreliable News',
                                                           'Only Very\nReliable News'))

#Produce Plot:
ggplot(data = Final_Mat, aes(x = x, y = Coef)) +
  geom_hline(aes(yintercept = 0), color = "gray",
             linetype = 2, size = 1.2) +
  geom_point(aes(color = Measure, shape=Measure),size=4) +
  geom_linerange(aes(min = Low_Conf, 
                     max = Upp_Conf, 
                     color = Measure),
                 size=1.5) +
  scale_color_manual(values=c('red','blue','purple'), name = "Measure") +
  labs(color  = "Measure", shape  = "Measure") +
  ylab("\nEffect of Encouragement to Search for Information on \n Likelihood of Rating Misinformation True") +
  theme_classic() +
  theme(axis.title.x = element_text(size=22),
        axis.text.x  = element_text(size=20),
        axis.title.y = element_text(size=20),
        axis.text.y  = element_text(size=22),
        plot.title = element_text(size = 20),
        legend.title = element_text(size=20),
        legend.text = element_text(size=20)) +
  ylim(-0.5,0.5) +
  scale_x_continuous("News Quality Returned by Search Engines \n",breaks=c(0.9,1.5),labels=c('Some Very\nUnreliable News',
                                                                                             'Only Very\nReliable News'),limits=c(0.5,2.0)) +
  coord_flip()

#Save Figure:
ggsave('./Figures/Quantiles_High_DL_FULL_ROBUST.png',height=8,width=8)


################################################################################################################

############################## Figure 4a: Quantiles_Low_DL_FULL_ROBUST.png ############################################

################################################################################################################

#Filter:
Survey_Unrel_1 <- Survey_Unrel %>% filter(Some_Unrel_URLs == 1)

#Select variables:
Survey_Unrel_1 <- Survey_Unrel_1 %>% select(Article_day,True_Dummy,Four_Ordinal,Seven_Ordinal,Age,Gender,ResponseId,Education_Score,Income_Score,Ideo_Congruence,Dummy_Ideology,Article_Lean,Dig_Lit_Score)
Survey_Unrel_1$Treatment <- 1

#Create dataframe with unique response:
Survey_Unrel_1 <- unique(Survey_Unrel_1)
Survey_Unrel_1 <- as.data.frame(Survey_Unrel_1)

#Combine data:
New_Data <- rbind(Survey_Unrel_1,Control_Data)

#filter digital literacy:
New_Data <- New_Data %>% filter(Dig_Lit_Score < median(New_Data$Dig_Lit_Score))

#Remove NAs
New_Data <- na.omit(New_Data)

#Run Model
fit_2_1 = glm(Seven_Ordinal ~ Treatment + Age + Gender + Education_Score + Income_Score + Ideo_Congruence + Article_day,data=New_Data)
#Produce confidence intervals with clustered standard errors:
CI_2_1 = coefci(fit_2_1, vcov. = vcovCL(fit_2_1, cluster = list(New_Data$ResponseId,New_Data$Article_day), type = "HC0"))

#Run Model
fit_2_2 = glm(Four_Ordinal ~ Treatment + Age + Gender + Education_Score + Income_Score + Ideo_Congruence + Article_day,data=New_Data)
#Produce confidence intervals with clustered standard errors:
CI_2_2 = coefci(fit_2_2, vcov. = vcovCL(fit_2_2, cluster = list(New_Data$ResponseId,New_Data$Article_day), type = "HC0"))

#Run Model
fit_2_3 = glm(True_Dummy ~ Treatment + Age + Gender + Education_Score + Income_Score + Ideo_Congruence + Article_day,data=New_Data)
#Produce confidence intervals with clustered standard errors:
CI_2_3 = coefci(fit_2_3, vcov. = vcovCL(fit_2_3, cluster = list(New_Data$ResponseId,New_Data$Article_day), type = "HC0"))

#Create matrix with coefiicients and confidence intervals:
Final_Mat <- matrix(ncol=5)
New_matr <- matrix(c(round(fit_2_1$coefficients[2]/sd(New_Data$Seven_Ordinal),4),round(CI_2_1[2,1]/sd(New_Data$Seven_Ordinal),4),round(CI_2_1[2,2]/sd(New_Data$Seven_Ordinal),4),'Ordinal (7)','Some Very\nUnreliable News',
                     round(fit_2_2$coefficients[2]/sd(New_Data$Four_Ordinal),4),round(CI_2_2[2,1]/sd(New_Data$Four_Ordinal),4),round(CI_2_2[2,2]/sd(New_Data$Four_Ordinal),4),'Ordinal (4)','Some Very\nUnreliable News',
                     round(fit_2_3$coefficients[2]/sd(New_Data$True_Dummy),4),round(CI_2_3[2,1]/sd(New_Data$True_Dummy),4),round(CI_2_3[2,2]/sd(New_Data$True_Dummy),4),'True (Dummy)','Some Very\nUnreliable News'),ncol=5,byrow=T)
Final_Mat <- rbind(Final_Mat,New_matr)


#Filter:
Survey_Unrel_1 <- Survey_Unrel %>% filter(Only_Rel_URLs == 1)

#Select variables:
Survey_Unrel_1 <- Survey_Unrel_1 %>% select(Article_day,True_Dummy,Four_Ordinal,Seven_Ordinal,Age,Gender,ResponseId,Education_Score,Income_Score,Ideo_Congruence,Dummy_Ideology,Article_Lean,Dig_Lit_Score)
Survey_Unrel_1$Treatment <- 1

#Create dataframe with unique response:
Survey_Unrel_1 <- unique(Survey_Unrel_1)
Survey_Unrel_1 <- as.data.frame(Survey_Unrel_1)

#Combine data:
New_Data <- rbind(Survey_Unrel_1,Control_Data)

#filter digital literacy:
New_Data <- New_Data %>% filter(Dig_Lit_Score < median(New_Data$Dig_Lit_Score))

#Remove NAs
New_Data <- na.omit(New_Data)


#Run Model
fit_2_1 = glm(Seven_Ordinal ~ Treatment + Age + Gender + Education_Score + Income_Score + Ideo_Congruence + Article_day,data=New_Data)
#Produce confidence intervals with clustered standard errors:
CI_2_1 = coefci(fit_2_1, vcov. = vcovCL(fit_2_1, cluster = list(New_Data$ResponseId,New_Data$Article_day), type = "HC0"))

#Run Model
fit_2_2 = glm(Four_Ordinal ~ Treatment + Age + Gender + Education_Score + Income_Score + Ideo_Congruence + Article_day,data=New_Data)
#Produce confidence intervals with clustered standard errors:
CI_2_2 = coefci(fit_2_2, vcov. = vcovCL(fit_2_2, cluster = list(New_Data$ResponseId,New_Data$Article_day), type = "HC0"))

#Run Model
fit_2_3 = glm(True_Dummy ~ Treatment + Age + Gender + Education_Score + Income_Score + Ideo_Congruence + Article_day,data=New_Data)
#Produce confidence intervals with clustered standard errors:
CI_2_3 = coefci(fit_2_3, vcov. = vcovCL(fit_2_3, cluster = list(New_Data$ResponseId,New_Data$Article_day), type = "HC0"))

#Merge data:
New_matr <- matrix(c(round(fit_2_1$coefficients[2]/sd(New_Data$Seven_Ordinal),4),round(CI_2_1[2,1]/sd(New_Data$Seven_Ordinal),4),round(CI_2_1[2,2]/sd(New_Data$Seven_Ordinal),4),'Ordinal (7)','Only Very\nReliable News',
                     round(fit_2_2$coefficients[2]/sd(New_Data$Four_Ordinal),4),round(CI_2_2[2,1]/sd(New_Data$Four_Ordinal),4),round(CI_2_2[2,2]/sd(New_Data$Four_Ordinal),4),'Ordinal (4)','Only Very\nReliable News',
                     round(fit_2_3$coefficients[2]/sd(New_Data$True_Dummy),4),round(CI_2_3[2,1]/sd(New_Data$True_Dummy),4),round(CI_2_3[2,2]/sd(New_Data$True_Dummy),4),'True (Dummy)','Only Very\nReliable News'),ncol=5,byrow=T)


#Create dataframe to create plot:
Final_Mat <- rbind(Final_Mat,New_matr)
Final_Mat <- as.data.frame(Final_Mat)
Final_Mat <- na.omit(Final_Mat)
colnames(Final_Mat) <- c('Coef','Low_Conf','Upp_Conf','Measure','Type_News')
Final_Mat <- na.omit(Final_Mat)
Final_Mat$x<-c(0.8,0.9,1.0,1.4,1.5,1.6)
Final_Mat$Coef <- as.character(Final_Mat$Coef)
Final_Mat$Coef <- as.numeric(Final_Mat$Coef)
Final_Mat$Upp_Conf <- as.character(Final_Mat$Upp_Conf)
Final_Mat$Upp_Conf <- as.numeric(Final_Mat$Upp_Conf)
Final_Mat$Low_Conf <- as.character(Final_Mat$Low_Conf)
Final_Mat$Low_Conf <- as.numeric(Final_Mat$Low_Conf)


#Produce Plot:
ggplot(data = Final_Mat, aes(x = x, y = Coef)) +
  geom_hline(aes(yintercept = 0), color = "gray",
             linetype = 2, size = 1.2) +
  geom_point(aes(color = Measure, shape=Measure),size=4) +
  geom_linerange(aes(min = Low_Conf, 
                     max = Upp_Conf, 
                     color = Measure),
                 size=1.5) +
  scale_color_manual(values=c('red','blue','purple'), name = "Measure") +
  labs(color  = "Measure", shape  = "Measure") +
  ylab("\nEffect of Encouragement to Search for Information on \n Likelihood of Rating Misinformation True") +
  theme_classic() +
  theme(axis.title.x = element_text(size=22),
        axis.text.x  = element_text(size=20),
        axis.title.y = element_text(size=20),
        axis.text.y  = element_text(size=22),
        legend.title = element_text(size=20),
        legend.text = element_text(size=18)) +
  ylim(-0.6,0.5) +
  scale_x_continuous("News Quality Returned by Search Engines \n",breaks=c(0.9,1.5),labels=c('Some Very\nUnreliable News',
                                                                                             'Only Very\nReliable News'),limits=c(0.5,2.0)) +
  coord_flip()

#Save Figure:
ggsave('./Figures/Quantiles_Low_DL_FULL_ROBUST.png',height=8,width=8)


################################################################################################################

############################### Figure 4c: Quantiles_Congruent_FULL_ROBUST.png ########################################

################################################################################################################

#Filter:
Survey_Unrel_1 <- Survey_Unrel %>% filter(Some_Unrel_URLs == 1)

#Select variables:
Survey_Unrel_1 <- Survey_Unrel_1 %>% select(Article_day,True_Dummy,Four_Ordinal,Seven_Ordinal,Age,Gender,ResponseId,Education_Score,Income_Score,Ideo_Congruence,Dummy_Ideology,Article_Lean,Dig_Lit_Score)
Survey_Unrel_1$Treatment <- 1

#Create dataframe with unique response:
Survey_Unrel_1 <- unique(Survey_Unrel_1)
Survey_Unrel_1 <- as.data.frame(Survey_Unrel_1)

#Combine data:
New_Data <- rbind(Survey_Unrel_1,Control_Data)

#filter ideologically congruent:
New_Data <- New_Data %>% filter(Ideo_Congruence == 1)

#Remove NAs
New_Data <- na.omit(New_Data)


#Run Model
fit_2_1 = glm(Seven_Ordinal ~ Treatment + Age + Gender + Education_Score + Income_Score + Ideo_Congruence + Article_day,data=New_Data)
#Produce confidence intervals with clustered standard errors:
CI_2_1 = coefci(fit_2_1, vcov. = vcovCL(fit_2_1, cluster = list(New_Data$ResponseId,New_Data$Article_day), type = "HC0"))

#Run Model
fit_2_2 = glm(Four_Ordinal ~ Treatment + Age + Gender + Education_Score + Income_Score + Ideo_Congruence + Article_day,data=New_Data)
#Produce confidence intervals with clustered standard errors:
CI_2_2 = coefci(fit_2_2, vcov. = vcovCL(fit_2_2, cluster = list(New_Data$ResponseId,New_Data$Article_day), type = "HC0"))

#Run Model
fit_2_3 = glm(True_Dummy ~ Treatment + Age + Gender + Education_Score + Income_Score + Ideo_Congruence + Article_day,data=New_Data)
#Produce confidence intervals with clustered standard errors:
CI_2_3 = coefci(fit_2_3, vcov. = vcovCL(fit_2_3, cluster = list(New_Data$ResponseId,New_Data$Article_day), type = "HC0"))


#Create matrix with coefficients and confidence intervals:
Final_Mat <- matrix(ncol=5)
New_matr <- matrix(c(round(fit_2_1$coefficients[2]/sd(New_Data$Seven_Ordinal),4),round(CI_2_1[2,1]/sd(New_Data$Seven_Ordinal),4),round(CI_2_1[2,2]/sd(New_Data$Seven_Ordinal),4),'Ordinal (7)','Some Very\nUnreliable News',
                     round(fit_2_2$coefficients[2]/sd(New_Data$Four_Ordinal),4),round(CI_2_2[2,1]/sd(New_Data$Four_Ordinal),4),round(CI_2_2[2,2]/sd(New_Data$Four_Ordinal),4),'Ordinal (4)','Some Very\nUnreliable News',
                     round(fit_2_3$coefficients[2]/sd(New_Data$True_Dummy),4),round(CI_2_3[2,1]/sd(New_Data$True_Dummy),4),round(CI_2_3[2,2]/sd(New_Data$True_Dummy),4),'True (Dummy)','Some Very\nUnreliable News'),ncol=5,byrow=T)
Final_Mat <- rbind(Final_Mat,New_matr)


#Filter:
Survey_Unrel_1 <- Survey_Unrel %>% filter(Only_Rel_URLs == 1)

#Select variables:
Survey_Unrel_1 <- Survey_Unrel_1 %>% select(Article_day,True_Dummy,Four_Ordinal,Seven_Ordinal,Age,Gender,ResponseId,Education_Score,Income_Score,Ideo_Congruence,Dummy_Ideology,Article_Lean,Dig_Lit_Score)
Survey_Unrel_1$Treatment <- 1

#Create dataframe with unique response:
Survey_Unrel_1 <- unique(Survey_Unrel_1)
Survey_Unrel_1 <- as.data.frame(Survey_Unrel_1)

#Combine data:
New_Data <- rbind(Survey_Unrel_1,Control_Data)

#filter ideologically congruent:
New_Data <- New_Data %>% filter(Ideo_Congruence == 1)

#Remove NAs
New_Data <- na.omit(New_Data)


#Run Model
fit_2_1 = glm(Seven_Ordinal ~ Treatment + Age + Gender + Education_Score + Income_Score + Ideo_Congruence + Article_day,data=New_Data)
#Produce confidence intervals with clustered standard errors:
CI_2_1 = coefci(fit_2_1, vcov. = vcovCL(fit_2_1, cluster = list(New_Data$ResponseId,New_Data$Article_day), type = "HC0"))

#Run Model
fit_2_2 = glm(Four_Ordinal ~ Treatment + Age + Gender + Education_Score + Income_Score + Ideo_Congruence + Article_day,data=New_Data)
#Produce confidence intervals with clustered standard errors:
CI_2_2 = coefci(fit_2_2, vcov. = vcovCL(fit_2_2, cluster = list(New_Data$ResponseId,New_Data$Article_day), type = "HC0"))

#Run Model
fit_2_3 = glm(True_Dummy ~ Treatment + Age + Gender + Education_Score + Income_Score + Ideo_Congruence + Article_day,data=New_Data)
#Produce confidence intervals with clustered standard errors:
CI_2_3 = coefci(fit_2_3, vcov. = vcovCL(fit_2_3, cluster = list(New_Data$ResponseId,New_Data$Article_day), type = "HC0"))

#Merge dataL
New_matr <- matrix(c(round(fit_2_1$coefficients[2]/sd(New_Data$Seven_Ordinal),4),round(CI_2_1[2,1]/sd(New_Data$Seven_Ordinal),4),round(CI_2_1[2,2]/sd(New_Data$Seven_Ordinal),4),'Ordinal (7)','Only Very\nReliable News',
                     round(fit_2_2$coefficients[2]/sd(New_Data$Four_Ordinal),4),round(CI_2_2[2,1]/sd(New_Data$Four_Ordinal),4),round(CI_2_2[2,2]/sd(New_Data$Four_Ordinal),4),'Ordinal (4)','Only Very\nReliable News',
                     round(fit_2_3$coefficients[2]/sd(New_Data$True_Dummy),4),round(CI_2_3[2,1]/sd(New_Data$True_Dummy),4),round(CI_2_3[2,2]/sd(New_Data$True_Dummy),4),'True (Dummy)','Only Very\nReliable News'),ncol=5,byrow=T)

#Create dataset to create plot:
Final_Mat <- rbind(Final_Mat,New_matr)
Final_Mat <- as.data.frame(Final_Mat)
Final_Mat <- na.omit(Final_Mat)
colnames(Final_Mat) <- c('Coef','Low_Conf','Upp_Conf','Measure','Type_News')
Final_Mat <- na.omit(Final_Mat)
Final_Mat$x<-c(0.8,0.9,1.0,1.4,1.5,1.6)
Final_Mat$Coef <- as.character(Final_Mat$Coef)
Final_Mat$Coef <- as.numeric(Final_Mat$Coef)
Final_Mat$Upp_Conf <- as.character(Final_Mat$Upp_Conf)
Final_Mat$Upp_Conf <- as.numeric(Final_Mat$Upp_Conf)
Final_Mat$Low_Conf <- as.character(Final_Mat$Low_Conf)
Final_Mat$Low_Conf <- as.numeric(Final_Mat$Low_Conf)
Final_Mat$Type_News <- factor(Final_Mat$Type_News,levels=c('Some Very\nUnreliable News',
                                                           'Only Very\nReliable News'))

#Create plot:
ggplot(data = Final_Mat, aes(x = x, y = Coef)) +
  geom_hline(aes(yintercept = 0), color = "gray",
             linetype = 2, size = 1.2) +
  geom_point(aes(color = Measure, shape=Measure),size=4) +
  geom_linerange(aes(min = Low_Conf, 
                     max = Upp_Conf, 
                     color = Measure),
                 size=1.5) +
  scale_color_manual(values=c('red','blue','purple'), name = "Measure") +
  labs(color  = "Measure", shape  = "Measure") +
  ylab("\nEffect of Encouragement to Search for Information on \n Likelihood of Rating Misinformation True") +
  theme_classic() +
  theme(axis.title.x = element_text(size=22),
        axis.text.x  = element_text(size=20),
        axis.title.y = element_text(size=20),
        axis.text.y  = element_text(size=22),
        plot.title = element_text(size = 20),
        legend.title = element_text(size=20),
        legend.text = element_text(size=20)) +
  ylim(-0.8,0.5) +
  scale_x_continuous("News Quality Returned by Search Engines \n",breaks=c(0.9,1.5),labels=c('Some Very\nUnreliable News',
                                                                                             'Only Very\nReliable News'),limits=c(0.5,2.0)) +
  coord_flip()

#Save figure:
ggsave('./Figures/Quantiles_Congruent_FULL_ROBUST.png',height=8,width=8)

################################################################################################################

################################ Figure 4d: Quantiles_Incong_FULL_ROBUST.png ##########################################

################################################################################################################


#Filter:
Survey_Unrel_1 <- Survey_Unrel %>% filter(Some_Unrel_URLs == 1)

#Select variables:
Survey_Unrel_1 <- Survey_Unrel_1 %>% select(Article_day,True_Dummy,Four_Ordinal,Seven_Ordinal,Age,Gender,ResponseId,Education_Score,Income_Score,Ideo_Congruence,Dummy_Ideology,Article_Lean,Dig_Lit_Score)
Survey_Unrel_1$Treatment <- 1

#Create dataframe with unique response:
Survey_Unrel_1 <- unique(Survey_Unrel_1)
Survey_Unrel_1 <- as.data.frame(Survey_Unrel_1)

#Combine data:
New_Data <- rbind(Survey_Unrel_1,Control_Data)

#Filter individuals with ideological incongruence:
New_Data_1 <- New_Data %>% filter(Article_Lean == 'Liberal' | Article_Lean ==  'Conservative')
New_Data_2 <- New_Data %>% filter(Article_Lean == 'Conservative' | Article_Lean ==  'Liberal')

#Merge control and treatment datra:
New_Data <- rbind(New_Data_1,New_Data_2)

#Remove NAs
New_Data <- na.omit(New_Data)


#Run Model
fit_2_1 = glm(Seven_Ordinal ~ Treatment + Age + Gender + Education_Score + Income_Score + Ideo_Congruence + Article_day,data=New_Data)
#Produce confidence intervals with clustered standard errors:
CI_2_1 = coefci(fit_2_1, vcov. = vcovCL(fit_2_1, cluster = list(New_Data$ResponseId,New_Data$Article_day), type = "HC0"))

#Run Model
fit_2_2 = glm(Four_Ordinal ~ Treatment + Age + Gender + Education_Score + Income_Score + Ideo_Congruence + Article_day,data=New_Data)
#Produce confidence intervals with clustered standard errors:
CI_2_2 = coefci(fit_2_2, vcov. = vcovCL(fit_2_2, cluster = list(New_Data$ResponseId,New_Data$Article_day), type = "HC0"))

#Run Model
fit_2_3 = glm(True_Dummy ~ Treatment + Age + Gender + Education_Score + Income_Score + Ideo_Congruence + Article_day,data=New_Data)
#Produce confidence intervals with clustered standard errors:
CI_2_3 = coefci(fit_2_3, vcov. = vcovCL(fit_2_3, cluster = list(New_Data$ResponseId,New_Data$Article_day), type = "HC0"))


#Create matrix with coefficients and confidence intervals:
Final_Mat <- matrix(ncol=5)
New_matr <- matrix(c(round(fit_2_1$coefficients[2]/sd(New_Data$Seven_Ordinal),4),round(CI_2_1[2,1]/sd(New_Data$Seven_Ordinal),4),round(CI_2_1[2,2]/sd(New_Data$Seven_Ordinal),4),'Ordinal (7)','Some Very\nUnreliable News',
                     round(fit_2_2$coefficients[2]/sd(New_Data$Four_Ordinal),4),round(CI_2_2[2,1]/sd(New_Data$Four_Ordinal),4),round(CI_2_2[2,2]/sd(New_Data$Four_Ordinal),4),'Ordinal (4)','Some Very\nUnreliable News',
                     round(fit_2_3$coefficients[2]/sd(New_Data$True_Dummy),4),round(CI_2_3[2,1]/sd(New_Data$True_Dummy),4),round(CI_2_3[2,2]/sd(New_Data$True_Dummy),4),'True (Dummy)','Some Very\nUnreliable News'),ncol=5,byrow=T)
Final_Mat <- rbind(Final_Mat,New_matr)


#Filter:
Survey_Unrel_1 <- Survey_Unrel %>% filter(Only_Rel_URLs == 1)

#Select variables:
Survey_Unrel_1 <- Survey_Unrel_1 %>% select(Article_day,True_Dummy,Four_Ordinal,Seven_Ordinal,Age,Gender,ResponseId,Education_Score,Income_Score,Ideo_Congruence,Dummy_Ideology,Article_Lean,Dig_Lit_Score)
Survey_Unrel_1$Treatment <- 1

#Create dataframe with unique response:
Survey_Unrel_1 <- unique(Survey_Unrel_1)
Survey_Unrel_1 <- as.data.frame(Survey_Unrel_1)

#Combine data:
New_Data <- rbind(Survey_Unrel_1,Control_Data)

#Filter individuals with ideological incongruence:
New_Data_1 <- New_Data %>% filter(Article_Lean == 'Liberal' | Article_Lean ==  'Conservative')
New_Data_2 <- New_Data %>% filter(Article_Lean == 'Conservative' | Article_Lean ==  'Liberal')

#Merge control and treatment datra:
New_Data <- rbind(New_Data_1,New_Data_2)
#Remove NAs
New_Data <- na.omit(New_Data)


#Run Model
fit_2_1 = glm(Seven_Ordinal ~ Treatment + Age + Gender + Education_Score + Income_Score + Ideo_Congruence + Article_day,data=New_Data)
#Produce confidence intervals with clustered standard errors:
CI_2_1 = coefci(fit_2_1, vcov. = vcovCL(fit_2_1, cluster = list(New_Data$ResponseId,New_Data$Article_day), type = "HC0"))

#Run Model
fit_2_2 = glm(Four_Ordinal ~ Treatment + Age + Gender + Education_Score + Income_Score + Ideo_Congruence + Article_day,data=New_Data)
#Produce confidence intervals with clustered standard errors:
CI_2_2 = coefci(fit_2_2, vcov. = vcovCL(fit_2_2, cluster = list(New_Data$ResponseId,New_Data$Article_day), type = "HC0"))

#Run Model
fit_2_3 = glm(True_Dummy ~ Treatment + Age + Gender + Education_Score + Income_Score + Ideo_Congruence + Article_day,data=New_Data)
#Produce confidence intervals with clustered standard errors:
CI_2_3 = coefci(fit_2_3, vcov. = vcovCL(fit_2_3, cluster = list(New_Data$ResponseId,New_Data$Article_day), type = "HC0"))

#Merge data:
New_matr <- matrix(c(round(fit_2_1$coefficients[2]/sd(New_Data$Seven_Ordinal),4),round(CI_2_1[2,1]/sd(New_Data$Seven_Ordinal),4),round(CI_2_1[2,2]/sd(New_Data$Seven_Ordinal),4),'Ordinal (7)','Only Very\nReliable News',
                     round(fit_2_2$coefficients[2]/sd(New_Data$Four_Ordinal),4),round(CI_2_2[2,1]/sd(New_Data$Four_Ordinal),4),round(CI_2_2[2,2]/sd(New_Data$Four_Ordinal),4),'Ordinal (4)','Only Very\nReliable News',
                     round(fit_2_3$coefficients[2]/sd(New_Data$True_Dummy),4),round(CI_2_3[2,1]/sd(New_Data$True_Dummy),4),round(CI_2_3[2,2]/sd(New_Data$True_Dummy),4),'True (Dummy)','Only Very\nReliable News'),ncol=5,byrow=T)

Final_Mat <- rbind(Final_Mat,New_matr)


#Create data for plot:
Final_Mat <- as.data.frame(Final_Mat)
Final_Mat <- na.omit(Final_Mat)
colnames(Final_Mat) <- c('Coef','Low_Conf','Upp_Conf','Measure','Type_News')
Final_Mat <- na.omit(Final_Mat)
Final_Mat$x<-c(0.8,0.9,1.0,1.4,1.5,1.6)
Final_Mat$Coef <- as.character(Final_Mat$Coef)
Final_Mat$Coef <- as.numeric(Final_Mat$Coef)
Final_Mat$Upp_Conf <- as.character(Final_Mat$Upp_Conf)
Final_Mat$Upp_Conf <- as.numeric(Final_Mat$Upp_Conf)
Final_Mat$Low_Conf <- as.character(Final_Mat$Low_Conf)
Final_Mat$Low_Conf <- as.numeric(Final_Mat$Low_Conf)
Final_Mat$Type_News <- factor(Final_Mat$Type_News,levels=c('Some Very\nUnreliable News',
                                                           'Only Very\nReliable News'))

#Produce Plot:
ggplot(data = Final_Mat, aes(x = x, y = Coef)) +
  geom_hline(aes(yintercept = 0), color = "gray",
             linetype = 2, size = 1.2) +
  geom_point(aes(color = Measure, shape=Measure),size=4) +
  geom_linerange(aes(min = Low_Conf, 
                     max = Upp_Conf, 
                     color = Measure),
                 size=1.5) +
  scale_color_manual(values=c('red','blue','purple'), name = "Measure") +
  labs(color  = "Measure", shape  = "Measure") +
  ylab("\nEffect of Encouragement to Search for Information on \n Likelihood of Rating Misinformation True") +
  theme_classic() +
  theme(axis.title.x = element_text(size=22),
        axis.text.x  = element_text(size=20),
        axis.title.y = element_text(size=20),
        axis.text.y  = element_text(size=22),
        plot.title = element_text(size = 20),
        legend.title = element_text(size=20),
        legend.text = element_text(size=20)) +
  ylim(-0.5,0.5) +
  scale_x_continuous("News Quality Returned by Search Engines \n",breaks=c(0.9,1.5),labels=c('Some Very\nUnreliable News',
                                                                                             'Only Very\nReliable News'),limits=c(0.5,2.0)) +
  coord_flip()

#Save figure:
ggsave('./Figures/Quantiles_Incong_FULL_ROBUST.png',height=8,width=8)











#Filter:
Survey_Unrel_1 <- Survey_Unrel %>% filter(Some_Unrel_URLs == 1)

#Select variables:
Survey_Unrel_1 <- Survey_Unrel_1 %>% select(Article_day,True_Dummy,Four_Ordinal,Seven_Ordinal,Age,Gender,ResponseId,Education_Score,Income_Score,Ideo_Congruence,Dummy_Ideology,Article_Lean,Dig_Lit_Score)
Survey_Unrel_1$Treatment <- 1

#Create dataframe with unique response:
Survey_Unrel_1 <- unique(Survey_Unrel_1)
Survey_Unrel_1 <- as.data.frame(Survey_Unrel_1)

#Combine data:
New_Data <- rbind(Survey_Unrel_1,Control_Data)

#Filter individuals with ideological incongruence:
New_Data_1 <- New_Data %>% filter(Article_Lean == 'Liberal' | Article_Lean ==  'Conservative')
New_Data_2 <- New_Data %>% filter(Article_Lean == 'Conservative' | Article_Lean ==  'Liberal')

#Merge control and treatment datra:
New_Data <- rbind(New_Data_1,New_Data_2)

#Remove NAs
New_Data <- na.omit(New_Data)


#Run Model
fit_2_1 = glm(Seven_Ordinal ~ Treatment + Age + Gender + Education_Score + Income_Score + Ideo_Congruence + Article_day,data=New_Data)
#Produce confidence intervals with clustered standard errors:
CI_2_1 = coefci(fit_2_1, vcov. = vcovCL(fit_2_1, cluster = list(New_Data$ResponseId,New_Data$Article_day), type = "HC0"))

#Run Model
fit_2_2 = glm(Four_Ordinal ~ Treatment + Age + Gender + Education_Score + Income_Score + Ideo_Congruence + Article_day,data=New_Data)
#Produce confidence intervals with clustered standard errors:
CI_2_2 = coefci(fit_2_2, vcov. = vcovCL(fit_2_2, cluster = list(New_Data$ResponseId,New_Data$Article_day), type = "HC0"))

#Run Model
fit_2_3 = glm(True_Dummy ~ Treatment + Age + Gender + Education_Score + Income_Score + Ideo_Congruence + Article_day,data=New_Data)
#Produce confidence intervals with clustered standard errors:
CI_2_3 = coefci(fit_2_3, vcov. = vcovCL(fit_2_3, cluster = list(New_Data$ResponseId,New_Data$Article_day), type = "HC0"))


#Create matrix with coefficients and confidence intervals:
Final_Mat <- matrix(ncol=5)
New_matr <- matrix(c(round(fit_2_1$coefficients[2]/sd(New_Data$Seven_Ordinal),4),round(CI_2_1[2,1]/sd(New_Data$Seven_Ordinal),4),round(CI_2_1[2,2]/sd(New_Data$Seven_Ordinal),4),'Ordinal (7)','Some Very\nUnreliable News',
                     round(fit_2_2$coefficients[2]/sd(New_Data$Four_Ordinal),4),round(CI_2_2[2,1]/sd(New_Data$Four_Ordinal),4),round(CI_2_2[2,2]/sd(New_Data$Four_Ordinal),4),'Ordinal (4)','Some Very\nUnreliable News',
                     round(fit_2_3$coefficients[2]/sd(New_Data$True_Dummy),4),round(CI_2_3[2,1]/sd(New_Data$True_Dummy),4),round(CI_2_3[2,2]/sd(New_Data$True_Dummy),4),'True (Dummy)','Some Very\nUnreliable News'),ncol=5,byrow=T)
Final_Mat <- rbind(Final_Mat,New_matr)


#Filter:
Survey_Unrel_1 <- Survey_Unrel %>% filter(Only_Rel_URLs == 1)

#Select variables:
Survey_Unrel_1 <- Survey_Unrel_1 %>% select(Article_day,True_Dummy,Four_Ordinal,Seven_Ordinal,Age,Gender,ResponseId,Education_Score,Income_Score,Ideo_Congruence,Dummy_Ideology,Article_Lean,Dig_Lit_Score)
Survey_Unrel_1$Treatment <- 1

#Create dataframe with unique response:
Survey_Unrel_1 <- unique(Survey_Unrel_1)
Survey_Unrel_1 <- as.data.frame(Survey_Unrel_1)

#Combine data:
New_Data <- rbind(Survey_Unrel_1,Control_Data)

#filter digital literacy:
#Merge:
New_Data <- rbind(Survey_Unrel_1,Control_Data)

#Filter individuals with ideological incongruence:
New_Data_1 <- New_Data %>% filter(Article_Lean == 'Liberal' | Article_Lean ==  'Conservative')
New_Data_2 <- New_Data %>% filter(Article_Lean == 'Conservative' | Article_Lean ==  'Liberal')

#Merge control and treatment datra:
New_Data <- rbind(New_Data_1,New_Data_2)

#Remove NAs
New_Data <- na.omit(New_Data)


#Run Model
fit_2_1 = glm(Seven_Ordinal ~ Treatment + Age + Gender + Education_Score + Income_Score + Ideo_Congruence + Article_day,data=New_Data)
#Produce confidence intervals with clustered standard errors:
CI_2_1 = coefci(fit_2_1, vcov. = vcovCL(fit_2_1, cluster = list(New_Data$ResponseId,New_Data$Article_day), type = "HC0"))

#Run Model
fit_2_2 = glm(Four_Ordinal ~ Treatment + Age + Gender + Education_Score + Income_Score + Ideo_Congruence + Article_day,data=New_Data)
#Produce confidence intervals with clustered standard errors:
CI_2_2 = coefci(fit_2_2, vcov. = vcovCL(fit_2_2, cluster = list(New_Data$ResponseId,New_Data$Article_day), type = "HC0"))

#Run Model
fit_2_3 = glm(True_Dummy ~ Treatment + Age + Gender + Education_Score + Income_Score + Ideo_Congruence + Article_day,data=New_Data)
#Produce confidence intervals with clustered standard errors:
CI_2_3 = coefci(fit_2_3, vcov. = vcovCL(fit_2_3, cluster = list(New_Data$ResponseId,New_Data$Article_day), type = "HC0"))

#Merge data:
New_matr <- matrix(c(round(fit_2_1$coefficients[2]/sd(New_Data$Seven_Ordinal),4),round(CI_2_1[2,1]/sd(New_Data$Seven_Ordinal),4),round(CI_2_1[2,2]/sd(New_Data$Seven_Ordinal),4),'Ordinal (7)','Only Very\nReliable News',
                     round(fit_2_2$coefficients[2]/sd(New_Data$Four_Ordinal),4),round(CI_2_2[2,1]/sd(New_Data$Four_Ordinal),4),round(CI_2_2[2,2]/sd(New_Data$Four_Ordinal),4),'Ordinal (4)','Only Very\nReliable News',
                     round(fit_2_3$coefficients[2]/sd(New_Data$True_Dummy),4),round(CI_2_3[2,1]/sd(New_Data$True_Dummy),4),round(CI_2_3[2,2]/sd(New_Data$True_Dummy),4),'True (Dummy)','Only Very\nReliable News'),ncol=5,byrow=T)

Final_Mat <- rbind(Final_Mat,New_matr)


#Create data for plot:
Final_Mat <- as.data.frame(Final_Mat)
Final_Mat <- na.omit(Final_Mat)
colnames(Final_Mat) <- c('Coef','Low_Conf','Upp_Conf','Measure','Type_News')
Final_Mat <- na.omit(Final_Mat)
Final_Mat$x<-c(0.8,0.9,1.0,1.4,1.5,1.6)
Final_Mat$Coef <- as.character(Final_Mat$Coef)
Final_Mat$Coef <- as.numeric(Final_Mat$Coef)
Final_Mat$Upp_Conf <- as.character(Final_Mat$Upp_Conf)
Final_Mat$Upp_Conf <- as.numeric(Final_Mat$Upp_Conf)
Final_Mat$Low_Conf <- as.character(Final_Mat$Low_Conf)
Final_Mat$Low_Conf <- as.numeric(Final_Mat$Low_Conf)
Final_Mat$Type_News <- factor(Final_Mat$Type_News,levels=c('Some Very\nUnreliable News',
                                                           'Only Very\nReliable News'))

#Produce Plot:
ggplot(data = Final_Mat, aes(x = x, y = Coef)) +
  geom_hline(aes(yintercept = 0), color = "gray",
             linetype = 2, size = 1.2) +
  geom_point(aes(color = Measure, shape=Measure),size=4) +
  geom_linerange(aes(min = Low_Conf, 
                     max = Upp_Conf, 
                     color = Measure),
                 size=1.5) +
  scale_color_manual(values=c('red','blue','purple'), name = "Measure") +
  labs(color  = "Measure", shape  = "Measure") +
  ylab("\nEffect of Encouragement to Search for Information on \n Likelihood of Rating Misinformation True") +
  theme_classic() +
  theme(axis.title.x = element_text(size=22),
        axis.text.x  = element_text(size=20),
        axis.title.y = element_text(size=20),
        axis.text.y  = element_text(size=22),
        plot.title = element_text(size = 20),
        legend.title = element_text(size=20),
        legend.text = element_text(size=20)) +
  ylim(-0.7,0.7) +
  scale_x_continuous("News Quality Returned by Search Engines \n",breaks=c(0.9,1.5),labels=c('Some Very\nUnreliable News',
                                                                                             'Only Very\nReliable News'),limits=c(0.5,2.0)) +
  coord_flip()

#Save figure:
ggsave('./Figures/Quantiles_Incong_FULL_ROBUST.png',height=8,width=8)












