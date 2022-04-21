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

# FIGURES Out:
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
library(multiwayvcov)
library(miceadds)
library(mice)
library(miceadds)
library(plm)
library(fixest)
library(broom)

#Set dictionary for fixest objects:
setFixest_dict(c(Likert_Evaluation = "7-Point Ordinal Scale",
                 Susc_FN = "Categorical (Rated as True)",
                 True_Dummy = "Categorical (Rated as True)",
                 Treat_Search = "Treatment (Search)",
                 Treatment = "Treatment (Search)",
                 Four_Ordinal = "4-Point Ordinal Scale",
                 Seven_Ordinal = "7-Point Ordinal Scale",
                 Dummy_Congruence = "Ideological Congruence",
                 Ideo_Congruence = "Ideological Congruence",
                 Education_Score = "Education",
                 Gender = "Gender (Female dummy)",
                 Income_Score = "Income",
                 Article_day = "Article",
                 Dig_Lit_Score = "Digital Literacy",
                 ResponseId = "Respondent"))

#Create Article Metadata dataframe:
#Pull in this data:
Data_Bef_Aft <- read.csv('./Data/Data_Bef_Aft_Misl_False.csv')
Study_2_Ideo <- read.csv('./Data/Study_2_Respondent_Ideo.csv')

#Convert to strings:
Data_Bef_Aft$ResponseId <- as.character(Data_Bef_Aft$ResponseId)
Study_2_Ideo$ResponseId <- as.character(Study_2_Ideo$ResponseId)

#Only keep unique values:
Study_2_Ideo <- unique(Study_2_Ideo)
#Merge:
Data_Bef_Aft <- merge(Data_Bef_Aft,Study_2_Ideo,by='ResponseId')

#Create dummy ideology variables:
Data_Bef_Aft <- Data_Bef_Aft %>% mutate(Dummy_Ideology = ifelse(Ideology_Score > 0,'Conservative','Moderate'))
Data_Bef_Aft <- Data_Bef_Aft %>% mutate(Dummy_Ideology = ifelse(Ideology_Score < 0,'Liberal',Dummy_Ideology))

#Create Article Lean Data using responses:
Article_data <- Data_Bef_Aft %>% mutate(Article_Lean = ifelse(Dummy_Ideology == 'Conservative' & Dummy_Congruence == 1,'Conservative','None'))
Article_data <- Article_data %>% mutate(Article_Lean = ifelse(Dummy_Ideology == 'Liberal' & Dummy_Congruence == 1,'Liberal',Article_Lean))

#Select only article variables:
Article_data <- Article_data %>% select(Article_day,Article_Lean)

#Pull unique articles and remove NAs:
Article_data <- unique(Article_data)
Article_data <- na.omit(Article_data)

#Study 1:
#Pull in this data: Search Experiment 1: Study 1:
Study_1_df <- read.csv('./Data/Search_Exp_Misl_False.csv')
#Select variables of interest:
Study_1_df <- Study_1_df %>% select(Likert_Evaluation,Susc_FN,Treat_Search,Education_Score,Age,Gender,Income_Score,Dummy_Congruence,Familiar_Dummy,Article_day,ResponseId)
#Remove NA values:
Study_1_df <- na.omit(Study_1_df)
Study_1_df$Gender <- ifelse(Study_1_df$Gender == 'Female',1,0)

#Run OLS Model with clustered standard errors:
lin_results_fit_1_1 = feols(Susc_FN ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Study_1_df)
#Produce confidence intervals with clustered standard errors:
CI_1_1 <- confint(lin_results_fit_1_1)

#Run OLS Model with clustered standard errors:
lin_results_fit_1_2 = feols(Likert_Evaluation ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Study_1_df)
#Produce confidence intervals with clustered standard errors:
CI_1_2 <- confint(lin_results_fit_1_2)

fit_1_1 <- glm(Susc_FN ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score + Article_day, data = Study_1_df)
fit_1_2 <- glm(Likert_Evaluation ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score + Article_day, data = Study_1_df)


#F-statistic
glm.0 <- glm(Susc_FN ~ 1,data=Study_1_df)
Results <- anova(fit_1_1, glm.0, test="F")
F_Tab_1_1 <- Results$F[2]

#F-statistic
glm.0 <- glm(Likert_Evaluation ~ 1,data=Study_1_df)
Results <- anova(fit_1_2, glm.0, test="F")
F_Tab_1_2 <- Results$F[2]



library(fixest)

#Write Table:
etable(lin_results_fit_1_1,lin_results_fit_1_2, tex = TRUE,
       signifCode= c(`***` = 0.001, `**` = 0.01, `*` = 0.05),
       file='./Tables/Study_1.txt',
       replace=TRUE,
       title='Results from OLS Regression Results Presented in Figure 1a and 1b (Study 1)')

#Study 2:
#Pull in this data:
Data_Bef_Aft <- read.csv('./Data/Data_Bef_Aft_Misl_False.csv')

#Select variables of interest:
Study_2_df <- Data_Bef_Aft %>% select(Likert_Evaluation,Susc_FN,Treat_Search,Education_Score,Age,Gender,Income_Score,Dummy_Congruence,Familiar_Dummy,Article_day,ResponseId)
#Remove NA values:
Study_2_df <- na.omit(Study_2_df)
Study_2_df$Gender <- ifelse(Study_2_df$Gender == 'Female',1,0)

#Run OLS Model with clustered standard errors:
lin_results_fit_2_1 = feols(Susc_FN ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Study_2_df)
#Produce confidence intervals with clustered standard errors:
CI_2_1 <- confint(lin_results_fit_2_1)

#Run OLS Model with clustered standard errors:
lin_results_fit_2_2 = feols(Likert_Evaluation ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Study_2_df)
#Produce confidence intervals with clustered standard errors:
CI_2_2 <- confint(lin_results_fit_2_2)


fit_1_1 <- glm(Susc_FN ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score + Article_day, data = Study_2_df)
fit_1_2 <- glm(Likert_Evaluation ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score + Article_day, data = Study_2_df)

#F-statistic
glm.0 <- glm(Susc_FN ~ 1,data=Study_2_df)
Results <- anova(fit_1_1, glm.0, test="F")
F_Tab_1_1 <- Results$F[2]

#F-statistic
glm.0 <- glm(Likert_Evaluation ~ 1,data=Study_2_df)
Results <- anova(fit_1_2, glm.0, test="F")
F_Tab_1_2 <- Results$F[2]


#Write Table:
etable(lin_results_fit_2_1,lin_results_fit_2_2, tex = TRUE,
       signifCode= c(`***` = 0.001, `**` = 0.01, `*` = 0.05),
       file='./Tables/Study_2.txt',
       replace=TRUE,
       title='Results from OLS Regression Results Presented in Figure 1a and 1b (Study 2)')


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
Study_3_df <- rbind(Before_Evaluation,After_Evaluation)

#Create Ideological Congruence data:
Study_3_df <- Study_3_df %>% mutate(Dummy_Ideology = ifelse(Ideology_Score > 0,'Conservative','Moderate'))
Study_3_df <- Study_3_df %>% mutate(Dummy_Ideology = ifelse(Ideology_Score < 0,'Liberal',Dummy_Ideology))
Study_3_df <- merge(Study_3_df,Article_data,all=T)
Study_3_df$Article_Lean <- ifelse(Study_3_df$Article_Lean == 'None','Neutral',Study_3_df$Article_Lean)
Study_3_df <- Study_3_df %>% mutate(Dummy_Congruence = ifelse(Dummy_Ideology == Article_Lean,1,0))
Study_3_df$Susc_FN <- Study_3_df$True_Dummy
Study_3_df$Treat_Search <- Study_3_df$Treatment

Study_3_df <- na.omit(Study_3_df)

#Run linear regression and produce coefficient values:
lin_results_fit_3_1 = feols(True_Dummy ~ Treatment + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Study_3_df)
#Produce confidence intervals using clustered standard errors:
CI_3_1 <- confint(lin_results_fit_3_1)

#Run linear regression and produce coefficient values:
lin_results_fit_3_2 = feols(Likert_Evaluation ~ Treatment + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Study_3_df)
#Produce confidence intervals using clustered standard errors:
CI_3_2 <- confint(lin_results_fit_3_2)

fit_1_1 <- glm(Susc_FN ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score + Article_day, data = Study_3_df)
fit_1_2 <- glm(Likert_Evaluation ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score + Article_day, data = Study_3_df)

#F-statistic
glm.0 <- glm(Susc_FN ~ 1,data=Study_3_df)
Results <- anova(fit_1_1, glm.0, test="F")
F_Tab_1_1 <- Results$F[2]

#F-statistic
glm.0 <- glm(Likert_Evaluation ~ 1,data=Study_3_df)
Results <- anova(fit_1_2, glm.0, test="F")
F_Tab_1_2 <- Results$F[2]

etable(lin_results_fit_3_1,lin_results_fit_3_2, tex = TRUE,
       signifCode= c(`***` = 0.001, `**` = 0.01, `*` = 0.05),
       file='./Tables/Study_3.txt',
       replace=TRUE,
       title='Results from OLS Regression Results Presented in Figure 1a and 1b (Study 3)')

#Study 4:
#Pull in data
Study_4_df <- read.csv('./Data/Experiment_2_Study_2_Misl_False.csv')

#Select variables of interest:
Study_4_df <- Study_4_df %>% select(Likert_Evaluation,Susc_FN,Treat_Search,Education_Score,Age,Gender,Income_Score,Dummy_Congruence,Familiar_Dummy,Article_day,ResponseId)
#Remove NA values:
Study_4_df <- na.omit(Study_4_df)

#Create control dataframe:
Study_4_Data <- Study_4_df %>% filter(Treat_Search == 0)

#Create Gender variable:
Study_4_df$Gender <- as.character(Study_4_df$Gender)
Study_4_df$Gender <- ifelse(Study_4_df$Gender == 'Female',1,0)

#Run OLS Model with clustered standard errors:
lin_results_fit_4_1 = feols(Susc_FN ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Study_4_df)
#Produce confidence intervals with clustered standard errors:
CI_4_1 <- confint(lin_results_fit_4_1)

#Run OLS Model with clustered standard errors:
lin_results_fit_4_2 = feols(Likert_Evaluation ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Study_4_df)
#Produce confidence intervals with clustered standard errors:
CI_4_2 <- confint(lin_results_fit_4_2)

fit_1_1 <- glm(Susc_FN ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score + Article_day, data = Study_4_df)
fit_1_2 <- glm(Likert_Evaluation ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score + Article_day, data = Study_4_df)

#F-statistic
glm.0 <- glm(Susc_FN ~ 1,data=Study_4_df)
Results <- anova(fit_1_1, glm.0, test="F")
F_Tab_1_1 <- Results$F[2]

#F-statistic
glm.0 <- glm(Likert_Evaluation ~ 1,data=Study_4_df)
Results <- anova(fit_1_2, glm.0, test="F")
F_Tab_1_2 <- Results$F[2]

#Write Table:
etable(lin_results_fit_4_1,lin_results_fit_4_2, tex = TRUE,
       signifCode= c(`***` = 0.001, `**` = 0.01, `*` = 0.05),
       file='./Tables/Study_4.txt',
       replace=TRUE,
       title='Results from OLS Regression Results Presented in Figure 1a and 1b (Study 4)')

#Study 5 Data:

#Create Treatment Data:

#Ideological Perspective of Articles:
FC_Ideo_Data <- read.csv('./Data/FC_Ideo_Data.csv')
FC_Ideo_Data$X <- NULL

#Pull in treatment data for Study 5:
Treatment_Data <- read.csv('./Data/Treatment_Data_Study_5.csv')
Treatment_Data$Link_1 <- NULL
Treatment_Data$Link_2 <- NULL

#Merge datasets:
Treatment_Data <- merge(Treatment_Data,FC_Ideo_Data,by='Article_day')

#Filter out false/misleading articles
T_Data <- Treatment_Data %>% filter(FC_Eval == 'FM')

#Create dummy political ideology variable:
T_Data <- T_Data %>% mutate(Dummy_Ideology = ifelse(Ideology_Score > 0,'Conservative','Moderate'))
T_Data <- T_Data %>% mutate(Dummy_Ideology = ifelse(Ideology_Score < 0,'Liberal',Dummy_Ideology))
T_Data <- T_Data %>% mutate(Ideo_Congruence = ifelse(Dummy_Ideology == Article_Lean,1,0))

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
Study_5_df <- rbind(Treatment_Data,Control_Data)

#Filter only false/misleading articles:
FM_Data_Study_5 <- Study_5_df %>% filter(FC_Eval == 'FM')
T_Data_Study_5 <- Study_5_df %>% filter(Treatment == 1)

#Run OLS Model with clustered standard errors:
lin_results_fit_5_1 = feols(True_Dummy ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = FM_Data_Study_5)
#Produce confidence intervals with clustered standard errors:
CI_5_1 <- confint(lin_results_fit_5_1)

#Run OLS Model with clustered standard errors:
lin_results_fit_5_2 = feols(Four_Ordinal~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = FM_Data_Study_5)
#Produce confidence intervals with clustered standard errors:
CI_5_2 <- confint(lin_results_fit_5_2)

#Run OLS Model with clustered standard errors:
lin_results_fit_5_3 = feols(Seven_Ordinal ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = FM_Data_Study_5)
#Produce confidence intervals with clustered standard errors:
CI_5_3 <- confint(lin_results_fit_5_3)


fit_1_1 <- glm(True_Dummy ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score + Article_day, data = FM_Data_Study_5)
fit_1_2 <- glm(Four_Ordinal ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score + Article_day, data = FM_Data_Study_5)
fit_1_3 <- glm(Seven_Ordinal ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score + Article_day, data = FM_Data_Study_5)

#F-statistic
glm.0 <- glm(True_Dummy ~ 1,data=FM_Data_Study_5)
Results <- anova(fit_1_1, glm.0, test="F")
F_Tab_1_1 <- Results$F[2]

#F-statistic
glm.0 <- glm(Four_Ordinal ~ 1,data=FM_Data_Study_5)
Results <- anova(fit_1_2, glm.0, test="F")
F_Tab_1_2 <- Results$F[2]

#F-statistic
glm.0 <- glm(Seven_Ordinal ~ 1,data=FM_Data_Study_5)
Results <- anova(fit_1_3, glm.0, test="F")
F_Tab_1_3 <- Results$F[2]



#Write Table:
etable(lin_results_fit_5_1,lin_results_fit_5_2,lin_results_fit_5_3, tex = TRUE,
       signifCode= c(`***` = 0.001, `**` = 0.01, `*` = 0.05),
       file='./Tables/Study_5.txt',
       replace=TRUE,
       title='Results from OLS Regression Results Presented in Figure 2b')


#Merge Google Search Results and survey results for each article in Study 5:
#Day 1
Google_results_1 <- read.csv('./Data/Google_Search_Results_Treatment_Day_1_All_New_NG_Ratings.csv')
Treatment_1 <- Treatment_Data %>% filter(Day == 'Day_1')
Google_results_1$Article_Eval <- as.character(Google_results_1$Article_Eval)
Google_results_1$Article_Eval <- paste0('Day_1_',Google_results_1$Article_Eval)
Survey_1 <- merge(Treatment_1,Google_results_1,by.x=c('ResponseId','Article_day'),by.y=c('Respondent_Id','Article_Eval'))
Survey_1 <- Survey_1 %>% select(ResponseId,Day,Article_day,Category,True_Dummy,Seven_Ordinal,Four_Ordinal,Age,Gender,FC_Eval,List_Scores,ALL_URLS,Dummy_Ideology,Ideo_Congruence,Income_Score,Education_Score,Dig_Lit_Score,Prop_Unreliable,Mean_Score,Article_Lean)

#Day 2
Google_results_2 <- read.csv('./Data/Google_Search_Results_Treatment_Day_2_All_New_NG_Ratings.csv')
Treatment_2 <- Treatment_Data %>% filter(Day == 'Day_2')
Google_results_2$Article_Eval <- as.character(Google_results_2$Article_Eval)
Google_results_2$Article_Eval <- paste0('Day_2_',Google_results_2$Article_Eval)
Survey_2 <- merge(Treatment_2,Google_results_2,by.x=c('ResponseId','Article_day'),by.y=c('Respondent_Id','Article_Eval'))
Survey_2 <- Survey_2 %>% select(ResponseId,Day,Article_day,Category,True_Dummy,Seven_Ordinal,Four_Ordinal,Age,Gender,FC_Eval,List_Scores,ALL_URLS,Dummy_Ideology,Ideo_Congruence,Income_Score,Education_Score,Dig_Lit_Score,Prop_Unreliable,Mean_Score,Article_Lean)

#Day 3
Google_results_3 <- read.csv('./Data/Google_Search_Results_Treatment_Day_3_All_New_NG_Ratings.csv')
Treatment_3 <- Treatment_Data %>% filter(Day == 'Day_3')
Google_results_3$Article_Eval <- as.character(Google_results_3$Article_Eval)
Google_results_3$Article_Eval <- paste0('Day_3_',Google_results_3$Article_Eval)
Survey_3 <- merge(Treatment_3,Google_results_3,by.x=c('ResponseId','Article_day'),by.y=c('Respondent_Id','Article_Eval'))
Survey_3 <- Survey_3 %>% select(ResponseId,Day,Article_day,Category,True_Dummy,Seven_Ordinal,Four_Ordinal,Age,Gender,FC_Eval,List_Scores,ALL_URLS,Dummy_Ideology,Ideo_Congruence,Income_Score,Education_Score,Dig_Lit_Score,Prop_Unreliable,Mean_Score,Article_Lean)

#Day 4
Google_results_4 <- read.csv('./Data/Google_Search_Results_Treatment_Day_4_All_New_NG_Ratings.csv')
Treatment_4 <- Treatment_Data %>% filter(Day == 'Day_4')
Google_results_4$Article_Eval <- as.character(Google_results_4$Article_Eval)
Google_results_4$Article_Eval <- paste0('Day_4_',Google_results_4$Article_Eval)
Survey_4 <- merge(Treatment_4,Google_results_4,by.x=c('ResponseId','Article_day'),by.y=c('Respondent_Id','Article_Eval'))
Survey_4 <- Survey_4 %>% select(ResponseId,Day,Article_day,Category,True_Dummy,Seven_Ordinal,Four_Ordinal,Age,Gender,FC_Eval,List_Scores,ALL_URLS,Dummy_Ideology,Ideo_Congruence,Income_Score,Education_Score,Dig_Lit_Score,Prop_Unreliable,Mean_Score,Article_Lean)

#Day 5
Google_results_5 <- read.csv('./Data/Google_Search_Results_Treatment_Day_5_All_New_NG_Ratings.csv')
Treatment_5 <- Treatment_Data %>% filter(Day == 'Day_5')
Google_results_5$Article_Eval <- as.character(Google_results_5$Article_Eval)
Google_results_5$Article_Eval <- paste0('Day_5_',Google_results_5$Article_Eval)
Survey_5 <- merge(Treatment_5,Google_results_5,by.x=c('ResponseId','Article_day'),by.y=c('Respondent_Id','Article_Eval'))
Survey_5 <- Survey_5 %>% select(ResponseId,Day,Article_day,Category,True_Dummy,Seven_Ordinal,Four_Ordinal,Age,Gender,FC_Eval,List_Scores,ALL_URLS,Dummy_Ideology,Ideo_Congruence,Income_Score,Education_Score,Dig_Lit_Score,Prop_Unreliable,Mean_Score,Article_Lean)

#Day 6
Google_results_6 <- read.csv('./Data/Google_Search_Results_Treatment_Day_6_All_New_NG_Ratings.csv')
Treatment_6 <- Treatment_Data %>% filter(Day == 'Day_6')
Google_results_6$Article_Eval <- as.character(Google_results_6$Article_Eval)
Google_results_6$Article_Eval <- paste0('Day_6_',Google_results_6$Article_Eval)
Survey_6 <- merge(Treatment_6,Google_results_6,by.x=c('ResponseId','Article_day'),by.y=c('Respondent_Id','Article_Eval'))
Survey_6 <- Survey_6 %>% select(ResponseId,Day,Article_day,Category,True_Dummy,Seven_Ordinal,Four_Ordinal,Age,Gender,FC_Eval,List_Scores,ALL_URLS,Dummy_Ideology,Ideo_Congruence,Income_Score,Education_Score,Dig_Lit_Score,Prop_Unreliable,Mean_Score,Article_Lean)

#Day 7
Google_results_7 <- read.csv('./Data/Google_Search_Results_Treatment_Day_7_All_New_NG_Ratings.csv')
Treatment_7 <- Treatment_Data %>% filter(Day == 'Day_7')
Google_results_7$Article_Eval <- as.character(Google_results_7$Article_Eval)
Google_results_7$Article_Eval <- paste0('Day_7_',Google_results_7$Article_Eval)
Survey_7 <- merge(Treatment_7,Google_results_7,by.x=c('ResponseId','Article_day'),by.y=c('Respondent_Id','Article_Eval'))
Survey_7 <- Survey_7 %>% select(ResponseId,Day,Article_day,Category,True_Dummy,Seven_Ordinal,Four_Ordinal,Age,Gender,FC_Eval,List_Scores,ALL_URLS,Dummy_Ideology,Ideo_Congruence,Income_Score,Education_Score,Dig_Lit_Score,Prop_Unreliable,Mean_Score,Article_Lean)

#Day 8
Google_results_8 <- read.csv('./Data/Google_Search_Results_Treatment_Day_8_All_New_NG_Ratings.csv')
Treatment_8 <- Treatment_Data %>% filter(Day == 'Day_8')
Google_results_8$Article_Eval <- as.character(Google_results_8$Article_Eval)
Google_results_8$Article_Eval <- paste0('Day_8_',Google_results_8$Article_Eval)
Survey_8 <- merge(Treatment_8,Google_results_8,by.x=c('ResponseId','Article_day'),by.y=c('Respondent_Id','Article_Eval'))
Survey_8 <- Survey_8 %>% select(ResponseId,Day,Article_day,Category,True_Dummy,Seven_Ordinal,Four_Ordinal,Age,Gender,FC_Eval,List_Scores,ALL_URLS,Dummy_Ideology,Ideo_Congruence,Income_Score,Education_Score,Dig_Lit_Score,Prop_Unreliable,Mean_Score,Article_Lean)

#Day 9
Google_results_9 <- read.csv('./Data/Google_Search_Results_Treatment_Day_9_All_New_NG_Ratings.csv')
Treatment_9 <- Treatment_Data %>% filter(Day == 'Day_9')
Google_results_9$Article_Eval <- as.character(Google_results_9$Article_Eval)
Google_results_9$Article_Eval <- paste0('Day_9_',Google_results_9$Article_Eval)
Survey_9 <- merge(Treatment_9,Google_results_9,by.x=c('ResponseId','Article_day'),by.y=c('Respondent_Id','Article_Eval'))
Survey_9 <- Survey_9 %>% select(ResponseId,Day,Article_day,Category,True_Dummy,Seven_Ordinal,Four_Ordinal,Age,Gender,FC_Eval,List_Scores,ALL_URLS,Dummy_Ideology,Ideo_Congruence,Income_Score,Education_Score,Dig_Lit_Score,Prop_Unreliable,Mean_Score,Article_Lean)

#Day 10
Google_results_10 <- read.csv('./Data/Google_Search_Results_Treatment_Day_10_All_New_NG_Ratings.csv')
Treatment_10 <- Treatment_Data %>% filter(Day == 'Day_10')
Google_results_10$Article_Eval <- as.character(Google_results_10$Article_Eval)
Google_results_10$Article_Eval <- paste0('Day_10_',Google_results_10$Article_Eval)
Survey_10 <- merge(Treatment_10,Google_results_10,by.x=c('ResponseId','Article_day'),by.y=c('Respondent_Id','Article_Eval'))
Survey_10 <- Survey_10 %>% select(ResponseId,Day,Article_day,Category,True_Dummy,Seven_Ordinal,Four_Ordinal,Age,Gender,FC_Eval,List_Scores,ALL_URLS,Dummy_Ideology,Ideo_Congruence,Income_Score,Education_Score,Dig_Lit_Score,Prop_Unreliable,Mean_Score,Article_Lean)

#Day 11
Google_results_11 <- read.csv('./Data/Google_Search_Results_Treatment_Day_11_All_New_NG_Ratings.csv')
Treatment_11 <- Treatment_Data %>% filter(Day == 'Day_11')
Google_results_11$Article_Eval <- as.character(Google_results_11$Article_Eval)
Google_results_11$Article_Eval <- paste0('Day_11_',Google_results_11$Article_Eval)
Survey_11 <- merge(Treatment_11,Google_results_11,by.x=c('ResponseId','Article_day'),by.y=c('Respondent_Id','Article_Eval'))
Survey_11 <- Survey_11 %>% select(ResponseId,Day,Article_day,Category,True_Dummy,Seven_Ordinal,Four_Ordinal,Age,Gender,FC_Eval,List_Scores,ALL_URLS,Dummy_Ideology,Ideo_Congruence,Income_Score,Education_Score,Dig_Lit_Score,Prop_Unreliable,Mean_Score,Article_Lean)

#Day 12
Google_results_12 <- read.csv('./Data/Google_Search_Results_Treatment_Day_12_All_New_NG_Ratings.csv')
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
Survey_Unrel$GS_Results <- ifelse(nchar(Survey_Unrel$ALL_URLS) > 2,1,0)
Survey_Unrel <- Survey_Unrel %>% filter(GS_Results == 1)

#Create blank vectors that data will be added to in for loop:
Only_Rel_URLs <- c()
Some_Unrel_URLs <- c()
Total_Rel_Sources <- c()
Total_Unrel_Sources <- c()
Total_Sources <- c()
Respondents_list <- c()
Articles_list <- c()
Mean_Scores_list = c()
Previous_Respondent = ''
Previous_Article = ''
All_Scores <- c()

Only_Rel_Sources = NA
Some_Unrel_Sources = NA
Rel_Sources = NA
Unrel_Sources = NA
Tot_Sources = NA

#Run for loop to create dataset with search engine results data for those in treatment group of Study 5:
for(i in 1:nrow(Survey_Unrel)){
  if(Survey_Unrel$ResponseId[i] == Previous_Respondent & Survey_Unrel$Article_day[i] == Previous_Article){
    New_Scores <- unlist(strsplit(Survey_Unrel$List_Scores[i], split=", "))
    New_Scores <- as.numeric(New_Scores)
    All_Scores <- c(All_Scores,New_Scores)
    Rel_Sources = 0
    Unrel_Sources = 0
    Tot_Sources = 0
    All_Scores <- All_Scores[!is.na(All_Scores)]
    Mean_Score = mean(All_Scores)
    if(length(All_Scores) == 0){
      All_Scores = NA} else{
        for(x in 1:length(All_Scores)){
          Tot_Sources = Tot_Sources + 1
          if(All_Scores[x] > 90){
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
  } else{
    Only_Rel_URLs = c(Only_Rel_URLs,Only_Rel_Sources)
    Some_Unrel_URLs = c(Some_Unrel_URLs,Some_Unrel_Sources)
    Total_Rel_Sources = c(Total_Rel_Sources,Rel_Sources)
    Total_Unrel_Sources = c(Total_Unrel_Sources,Unrel_Sources)
    Total_Sources = c(Total_Sources,Tot_Sources)
    Respondents_list <- c(Respondents_list,Previous_Respondent)
    Articles_list <- c(Articles_list,Previous_Article)
    Mean_Scores_list = c(Mean_Scores_list,mean(All_Scores))
    Previous_Respondent = as.character(Survey_Unrel$ResponseId[i])
    Previous_Article = as.character(Survey_Unrel$Article_day[i])
    All_Scores <- unlist(strsplit(Survey_Unrel$List_Scores[i], split=", "))
    All_Scores <- as.numeric(All_Scores)
    Rel_Sources = 0
    Unrel_Sources = 0
    Tot_Sources = 0
    All_Scores <- All_Scores[!is.na(All_Scores)]
    if(length(All_Scores) == 0){
      All_Scores = NA} else{
        for(x in 1:length(All_Scores)){
          Tot_Sources = Tot_Sources + 1
          if(All_Scores[x] > 90){
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
  }
}

Only_Rel_URLs = c(Only_Rel_URLs,Only_Rel_Sources)
Some_Unrel_URLs = c(Some_Unrel_URLs,Some_Unrel_Sources)
Total_Rel_Sources = c(Total_Rel_Sources,Rel_Sources)
Total_Unrel_Sources = c(Total_Unrel_Sources,Unrel_Sources)
Total_Sources = c(Total_Sources,Tot_Sources)
Respondents_list <- c(Respondents_list,Previous_Respondent)
Articles_list <- c(Articles_list,Previous_Article)
Mean_Scores_list = c(Mean_Scores_list,mean(All_Scores))
All_Scores <- unlist(strsplit(Survey_Unrel$List_Scores[i], split=", "))
All_Scores <- as.numeric(All_Scores)
Rel_Sources = 0
Unrel_Sources = 0
Tot_Sources = 0
if(length(All_Scores) == 0){
  All_Scores = NA} else{
    for(x in 1:length(All_Scores)){
      Tot_Sources = Tot_Sources + 1
      if(All_Scores[x] > 90){
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

#Create matrix with vectors of data:
Data_Scores <- matrix(c(Respondents_list,
                        Articles_list,
                        Total_Rel_Sources,
                        Total_Unrel_Sources,
                        Total_Sources,
                        Only_Rel_URLs,
                        Some_Unrel_URLs,
                        Mean_Scores_list),ncol = 8,byrow = FALSE)

#Convert materix to dataframe:
Data_Scores <- as.data.frame(Data_Scores)

#Name columns:
colnames(Data_Scores) <- c('ResponseId',
                           'Article_day',
                           'Tot_Reliable',
                           'Tot_Unreliable',
                           'Total_Sources',
                           'Only_Rel_Sources',
                           'Some_Unrel_Sources',
                           'Mean_Scores')

#Convert variables from factors to strings:
Data_Scores$Mean_Scores <- as.character(Data_Scores$Mean_Scores)
Data_Scores$Mean_Scores <- as.numeric(Data_Scores$Mean_Scores)
Data_Scores$Only_Rel_Sources <- as.character(Data_Scores$Only_Rel_Sources)
Data_Scores$Only_Rel_Sources <- as.numeric(Data_Scores$Only_Rel_Sources)
Data_Scores$Some_Unrel_Sources <- as.character(Data_Scores$Some_Unrel_Sources)
Data_Scores$Some_Unrel_Sources <- as.numeric(Data_Scores$Some_Unrel_Sources)
Data_Scores$Total_Sources <- as.character(Data_Scores$Total_Sources)
Data_Scores$Total_Sources <- as.numeric(Data_Scores$Total_Sources)
Data_Scores$Tot_Unreliable <- as.character(Data_Scores$Tot_Unreliable)
Data_Scores$Tot_Unreliable <- as.numeric(Data_Scores$Tot_Unreliable)
Data_Scores$Tot_Reliable <- as.character(Data_Scores$Tot_Reliable)
Data_Scores$Tot_Reliable <- as.numeric(Data_Scores$Tot_Reliable)
Data_Scores$Tot_Unreliable <- as.character(Data_Scores$Tot_Unreliable)
Data_Scores$Tot_Unreliable <- as.numeric(Data_Scores$Tot_Unreliable)

#Convert from factors to strings:
Data_Scores$Article <- as.character(Data_Scores$Article)
Data_Scores$ResponseId <- as.character(Data_Scores$ResponseId)


#Filter only responses who only saw very reliable news sites in Google Search Results (90)
only_rel_data <- Data_Scores %>% filter(Only_Rel_URLs == 1)
Treatment_rel_data <- merge(T_Data,only_rel_data,by=c('ResponseId','Article_day'))

#Select variables:
Treatment_rel_data <- Treatment_rel_data %>% select(Article_day,True_Dummy,Four_Ordinal,Seven_Ordinal,Age,Gender,ResponseId,Education_Score,Income_Score,Ideo_Congruence)
Treatment_rel_data$Treatment <- 1

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
Study_5_subset_1 <- rbind(Treatment_rel_data,Control_Data)
#Remove NAs
Study_5_subset_1 <- na.omit(Study_5_subset_1)

#Run OLS Model with clustered standard errors:
fit_fig_2c_1 = feols(Seven_Ordinal ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=Study_5_subset_1)

#Run OLS Model with clustered standard errors:
fit_fig_2c_2 = feols(Four_Ordinal ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=Study_5_subset_1)

#Run OLS Model with clustered standard errors:
fit_fig_2c_3 = feols(True_Dummy ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=Study_5_subset_1)


#Write Table:
etable(fit_fig_2c_3,fit_fig_2c_2,fit_fig_2c_1, tex = TRUE,
       signifCode= c(`***` = 0.001, `**` = 0.01, `*` = 0.05),
       file='./Tables/Figure_2c_1.txt',
       replace=TRUE,
       title='Results from OLS Regression Results Presented in Figure 2c (Only Very Reliable News Returned)')



#Some Unreliable
#Filter only responses who only saw very reliable news sites in Google Search Results (85)
only_unrel_data <- Data_Scores %>% filter(Some_Unrel_URLs == 1)
Treatment_unrel_data <- merge(T_Data,only_unrel_data,by=c('ResponseId','Article_day'))

#Select variables:
Treatment_unrel_data <- Treatment_unrel_data %>% select(Article_day,True_Dummy,Four_Ordinal,Seven_Ordinal,Age,Gender,ResponseId,Education_Score,Income_Score,Ideo_Congruence)
Treatment_unrel_data$Treatment <- 1

#Merge data:
Study_5_subset_2 <- rbind(Treatment_unrel_data,Control_Data)
#Remove NAs
Study_5_subset_2 <- na.omit(Study_5_subset_2)



#Run OLS Model with clustered standard errors:
fit_fig_2c_4 = feols(Seven_Ordinal ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=Study_5_subset_2)

#Run OLS Model with clustered standard errors:
fit_fig_2c_5 = feols(Four_Ordinal ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=Study_5_subset_2)

#Run OLS Model with clustered standard errors:
fit_fig_2c_6 = feols(True_Dummy ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=Study_5_subset_2)


#Write Table:
etable(fit_fig_2c_6,fit_fig_2c_5,fit_fig_2c_4, tex = TRUE,
       signifCode= c(`***` = 0.001, `**` = 0.01, `*` = 0.05),
       file='./Tables/Figure_2c_2.txt',
       replace=TRUE,
       title='Results from OLS Regression Results Presented in Figure 2c (Some Unreliable News Returned)')




#Filter responses by quartile of mean news quality:
Data_subset_lowest <- Data_Scores %>% filter(Mean_Scores < quantile(Data_Scores$Mean_Scores,na.rm=T)[2])
Treatment_subset_lowest <- merge(T_Data,Data_subset_lowest,by=c('ResponseId','Article_day'))

#Select variables:
Treatment_subset_lowest <- Treatment_subset_lowest %>% select(Article_day,True_Dummy,Four_Ordinal,Seven_Ordinal,Age,Gender,ResponseId,Education_Score,Income_Score,Ideo_Congruence)
Treatment_subset_lowest$Treatment <- 1

#Merge data:
Data_subset_lowest <- rbind(Treatment_subset_lowest,Control_Data)
#Remove NAs
Data_subset_lowest <- na.omit(Data_subset_lowest)


#Run OLS Model with clustered standard errors:
lin_results_fit_lowest_1 = feols(Seven_Ordinal ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=Data_subset_lowest)

#Run OLS Model with clustered standard errors:
lin_results_fit_lowest_2 = feols(Four_Ordinal ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=Data_subset_lowest)

#Run OLS Model with clustered standard errors:
lin_results_fit_lowest_3 = feols(True_Dummy ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=Data_subset_lowest)


#Write Table:
etable(lin_results_fit_lowest_3,lin_results_fit_lowest_2,lin_results_fit_lowest_1, tex = TRUE,
       signifCode= c(`***` = 0.001, `**` = 0.01, `*` = 0.05),
       file='./Tables/Figure_2d_4.txt',
       replace=TRUE,
       title='Results from OLS Regression Results Presented in Figure 2d (0-25 Percentage Quartile of News Quality)')

#Filter by quartile:
Treatment_subset_2nd_lowest <- Data_Scores %>% filter(Mean_Scores >= quantile(Data_Scores$Mean_Scores,na.rm=T)[2] & Mean_Scores < quantile(Data_Scores$Mean_Scores,na.rm=T)[3])
Treatment_subset_2nd_lowest <- merge(T_Data,Treatment_subset_2nd_lowest,by=c('ResponseId','Article_day'))

#Select variables:
Treatment_subset_2nd_lowest <- Treatment_subset_2nd_lowest %>% select(Article_day,True_Dummy,Four_Ordinal,Seven_Ordinal,Age,Gender,ResponseId,Education_Score,Income_Score,Ideo_Congruence)
Treatment_subset_2nd_lowest$Treatment <- 1

#Merge data:
Data_subset_2nd_lowest <- rbind(Treatment_subset_2nd_lowest,Control_Data)
#Remove NAs
Data_subset_2nd_lowest <- na.omit(Data_subset_2nd_lowest)


#Run OLS Model with clustered standard errors:
lin_results_fit_subset_2nd_lowest_1 = feols(Seven_Ordinal ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=Data_subset_2nd_lowest)

#Run OLS Model with clustered standard errors:
lin_results_fit_subset_2nd_lowest_2 = feols(Four_Ordinal ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=Data_subset_2nd_lowest)

#Run OLS Model with clustered standard errors:
lin_results_fit_subset_2nd_lowest_3 = feols(True_Dummy ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=Data_subset_2nd_lowest)


#Write Table:
etable(lin_results_fit_subset_2nd_lowest_3,lin_results_fit_subset_2nd_lowest_2,lin_results_fit_subset_2nd_lowest_1,
       tex = TRUE,
       signifCode= c(`***` = 0.001, `**` = 0.01, `*` = 0.05),
       file='./Tables/Figure_2d_3.txt',
       replace=TRUE,
       title='Results from OLS Regression Results Presented in Figure 2d (25-50 Percentage Quartile of News Quality)')

#Filter by quartile:
Treatment_subset_3rd_lowest <- Data_Scores %>% filter(Mean_Scores >= quantile(Data_Scores$Mean_Scores,na.rm=T)[3] & Mean_Scores < quantile(Data_Scores$Mean_Scores,na.rm=T)[4])
Treatment_subset_3rd_lowest <- merge(T_Data,Treatment_subset_3rd_lowest,by=c('ResponseId','Article_day'))

#Select variables:
Treatment_subset_3rd_lowest <- Treatment_subset_3rd_lowest %>% select(Article_day,True_Dummy,Four_Ordinal,Seven_Ordinal,Age,Gender,ResponseId,Education_Score,Income_Score,Ideo_Congruence)
Treatment_subset_3rd_lowest$Treatment <- 1

#Merge data:
Data_subset_3rd_lowest <- rbind(Treatment_subset_3rd_lowest,Control_Data)
#Remove NAs
Data_subset_3rd_lowest <- na.omit(Data_subset_3rd_lowest)


#Run OLS Model with clustered standard errors:
lin_results_fit_subset_3rd_lowest_1 = feols(Seven_Ordinal ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=Data_subset_3rd_lowest)

#Run OLS Model with clustered standard errors:
lin_results_fit_subset_3rd_lowest_2 = feols(Four_Ordinal ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=Data_subset_3rd_lowest)

#Run OLS Model with clustered standard errors:
lin_results_fit_subset_3rd_lowest_3 = feols(True_Dummy ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=Data_subset_3rd_lowest)


#Write Table:
etable(lin_results_fit_subset_3rd_lowest_3,lin_results_fit_subset_3rd_lowest_2,lin_results_fit_subset_3rd_lowest_1,
       tex = TRUE,
       signifCode= c(`***` = 0.001, `**` = 0.01, `*` = 0.05),
       file='./Tables/Figure_2d_2.txt',
       replace=TRUE,
       title='Results from OLS Regression Results Presented in Figure 2d (50-75 Percentage Quartile of News Quality)')


#Filter by quartile:
Treatment_subset_highest <- Data_Scores %>% filter(Mean_Scores > quantile(Data_Scores$Mean_Scores,na.rm=T)[4])
Treatment_subset_highest <- merge(T_Data,Treatment_subset_highest,by=c('ResponseId','Article_day'))

#Select variables:
Treatment_subset_highest <- Treatment_subset_highest %>% select(Article_day,True_Dummy,Four_Ordinal,Seven_Ordinal,Age,Gender,ResponseId,Education_Score,Income_Score,Ideo_Congruence)
Treatment_subset_highest$Treatment <- 1

#Merge data:
Data_subset_highest <- rbind(Treatment_subset_highest,Control_Data)
#Remove NAs
Data_subset_highest <- na.omit(Data_subset_highest)



#Run OLS Model with clustered standard errors:
lin_results_fit_subset_highest_1 = feols(Seven_Ordinal ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=Data_subset_highest)

#Run OLS Model with clustered standard errors:
lin_results_fit_subset_highest_2 = feols(Four_Ordinal ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=Data_subset_highest)

#Run OLS Model with clustered standard errors:
lin_results_fit_subset_highest_3 = feols(True_Dummy ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=Data_subset_highest)


#Write Table:
etable(lin_results_fit_subset_highest_3,lin_results_fit_subset_highest_2,lin_results_fit_subset_highest_1,
       tex = TRUE,
       signifCode= c(`***` = 0.001, `**` = 0.01, `*` = 0.05),
       file='./Tables/Figure_2d_1.txt',
       replace=TRUE,
       title='Results from OLS Regression Results Presented in Figure 2d (75-100 Percentage Quartile of News Quality)')


################################################################################################################

################################# Figure 3a: Coefs_CIs_Predicting_Unrel_Dummy.png ###############################

################################################################################################################

#Filter by quartile:
All_Treatment_df <- merge(T_Data,Data_Scores,by=c('ResponseId','Article_day'))
All_Treatment_df <- All_Treatment_df %>% select(Some_Unrel_Sources,Age,Gender,Education_Score,Income_Score,Ideo_Congruence,Dig_Lit_Score,Article_day,ResponseId)

#Run OLS Model with clustered standard errors:
Prop_Dummy_results = feols(Some_Unrel_Sources ~ Age + Gender + Education_Score + Income_Score + Ideo_Congruence +Dig_Lit_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=All_Treatment_df)

fit_1_1 <- glm(Some_Unrel_Sources ~ Age + Gender + Education_Score + Income_Score + Ideo_Congruence + Dig_Lit_Score + Article_day, data = All_Treatment_df)

#F-statistic
glm.0 <- glm(Some_Unrel_Sources ~ 1,data=All_Treatment_df)
Results <- anova(fit_1_1, glm.0, test="F")
F_Tab_1_1 <- Results$F[2]


#Write Table:
etable(Prop_Dummy_results,
       tex = TRUE,
       signifCode= c(`***` = 0.001, `**` = 0.01, `*` = 0.05),
       file='./Tables/Figure_3_a.txt',
       replace=TRUE,
       title='Predicted Exposure to Unreliable News Sources when Searching for Information')

################################################################################################################

################################ Figure 3b: Coefs_CIs_Predicting_Headline_Link.png  ##########################################

################################################################################################################

#Pull-in search data data:
Headline_coding <- read.csv('./Data/Headline_Coding.csv')
#Select variables needed:
Headline_coding <- Headline_coding %>% select(ResponseId,Article_day,Headline_Link,Age,Gender,Education_Score,Income_Score,Ideo_Congruence,Dig_Lit_Score)

#Run OLS Model with clustered standard errors:
Prop_Dummy_headline_results = feols(Headline_Link ~ Age + Gender + Education_Score + Income_Score + Ideo_Congruence +Dig_Lit_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=Headline_coding)

fit_1_1 <- glm(Headline_Link ~ Age + Gender + Education_Score + Income_Score + Ideo_Congruence + Dig_Lit_Score + Article_day, data = Headline_coding)

#F-statistic
glm.0 <- glm(Headline_Link ~ 1,data=Headline_coding)
Results <- anova(fit_1_1, glm.0, test="F")
F_Tab_1_1 <- Results$F[2]


#Write Table:
etable(Prop_Dummy_headline_results,
       tex = TRUE,
       signifCode= c(`***` = 0.001, `**` = 0.01, `*` = 0.05),
       file='./Tables/Figure_3_b.txt',
       replace=TRUE,
       title='Predicted Use of Headline or URL as a Search Query when Searching Online about Misinformation')


##############################################################################################################

#Digital Literacy

##############################################################################################################


#Create dataset with all respondents - Merge treatment data and search engine data:
Treat_Data_w_scores <- merge(Data_Scores,T_Data,by=c('Article_day','ResponseId'))
Treat_Data_w_scores <- Treat_Data_w_scores %>% filter(FC_Eval == 'FM')

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
Control_Data <- Control_Data %>% select(Article_day,True_Dummy,Four_Ordinal,Seven_Ordinal,Age,Gender,ResponseId,Education_Score,Income_Score,Dig_Lit_Score,Ideo_Congruence,Dummy_Ideology,Article_Lean)

#Select variables:
Treat_Data_w_scores <- Treat_Data_w_scores %>% select(Article_day,True_Dummy,Four_Ordinal,Seven_Ordinal,Age,Gender,ResponseId,Education_Score,Income_Score,Dig_Lit_Score,Ideo_Congruence,Dummy_Ideology,Article_Lean)

#Combine control and treatment data:
Study_5_Control_Treatment <- rbind(Control_Data,Treat_Data_w_scores)
Study_5_Control_Treatment <- unique(Study_5_Control_Treatment)

#Filter by news quality :
upper_half_df <- Data_Scores %>% filter(Mean_Scores >= median(Data_Scores$Mean_Scores,na.rm=T))
upper_half_df <- merge(T_Data,upper_half_df,by=c('ResponseId','Article_day'))

#Select Variables:
upper_half_df <- upper_half_df %>% select(Article_day,True_Dummy,Four_Ordinal,Seven_Ordinal,Age,Gender,ResponseId,Education_Score,Income_Score,Ideo_Congruence,Dig_Lit_Score,Dummy_Ideology,Article_Lean)

#Convert to character variables:
upper_half_df$ResponseId <- as.character(upper_half_df$ResponseId)
upper_half_df$Article_day <- as.character(upper_half_df$Article_day)

#Create treatment variable:
upper_half_df$Treatment <- 1

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
Control_Data <- Control_Data %>% select(Article_day,True_Dummy,Four_Ordinal,Seven_Ordinal,Age,Gender,ResponseId,Education_Score,Income_Score,Ideo_Congruence,Dig_Lit_Score,Dummy_Ideology,Article_Lean)
Control_Data$Treatment <- 0

#Combine data:
upper_half_df <- rbind(upper_half_df,Control_Data)

#filter digital literacy:
upper_half_df_high_diglit <- upper_half_df %>% filter(Dig_Lit_Score >= median(Study_5_Control_Treatment$Dig_Lit_Score))

#Remove NAs
upper_half_df_high_diglit <- na.omit(upper_half_df_high_diglit)



#Run OLS Model with clustered standard errors:
lin_results_fit_4b_1 = feols(Seven_Ordinal ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=upper_half_df_high_diglit)

#Run OLS Model with clustered standard errors:
lin_results_fit_4b_2 = feols(Four_Ordinal ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=upper_half_df_high_diglit)

#Run OLS Model with clustered standard errors:
lin_results_fit_4b_3 = feols(True_Dummy ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=upper_half_df_high_diglit)


#Write Table:
etable(lin_results_fit_4b_3,lin_results_fit_4b_2,lin_results_fit_4b_1,
       tex = TRUE,
       signifCode= c(`***` = 0.001, `**` = 0.01, `*` = 0.05),
       file='./Tables/Figure_4b_1.txt',
       replace=TRUE,
       title='Results from OLS Regression Results Presented in Figure 4b (Lower half of news quality returned - upper half of digital literacy)')


#Filter by quartile:
lower_half_df <- Data_Scores %>% filter(Mean_Scores < median(Data_Scores$Mean_Scores,na.rm=T))
lower_half_df <- merge(T_Data,lower_half_df,by=c('ResponseId','Article_day'))

#Select Variables:
lower_half_df <- lower_half_df %>% select(Article_day,True_Dummy,Four_Ordinal,Seven_Ordinal,Age,Gender,ResponseId,Education_Score,Income_Score,Ideo_Congruence,Dig_Lit_Score,Dummy_Ideology,Article_Lean)
lower_half_df$ResponseId <- as.character(lower_half_df$ResponseId)
lower_half_df$Article_day <- as.character(lower_half_df$Article_day)
lower_half_df$Treatment <- 1

#Combine data:
lower_half_df <- rbind(lower_half_df,Control_Data)

#filter digital literacy:
lower_half_df_high_diglit <- lower_half_df %>% filter(Dig_Lit_Score >= median(Study_5_Control_Treatment$Dig_Lit_Score))

#Remove NAs
lower_half_df_high_diglit <- na.omit(lower_half_df_high_diglit)


#Run OLS Model with clustered standard errors:
lin_results_fit_4b_4 = feols(Seven_Ordinal ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=lower_half_df_high_diglit)

#Run OLS Model with clustered standard errors:
lin_results_fit_4b_5 = feols(Four_Ordinal ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=lower_half_df_high_diglit)

#Run OLS Model with clustered standard errors:
lin_results_fit_4b_6 = feols(True_Dummy ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=lower_half_df_high_diglit)


#Write Table:
etable(lin_results_fit_4b_6,lin_results_fit_4b_5,lin_results_fit_4b_4,
       tex = TRUE,
       signifCode= c(`***` = 0.001, `**` = 0.01, `*` = 0.05),
       file='./Tables/Figure_4b_2.txt',
       replace=TRUE,
       title='Results from OLS Regression Results Presented in Figure 4b (Lower half of news quality returned - upper half of digital literacy)')



#filter digital literacy:
upper_half_df_low_diglit <- upper_half_df %>% filter(Dig_Lit_Score < median(Study_5_Control_Treatment$Dig_Lit_Score))

#Remove NAs
upper_half_df_low_diglit <- na.omit(upper_half_df_low_diglit)

#Run OLS Model with clustered standard errors:
lin_results_fit_4a_1 = feols(Seven_Ordinal ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=upper_half_df_low_diglit)

#Run OLS Model with clustered standard errors:
lin_results_fit_4a_2 = feols(Four_Ordinal ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=upper_half_df_low_diglit)

#Run OLS Model with clustered standard errors:
lin_results_fit_4a_3 = feols(True_Dummy ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=upper_half_df_low_diglit)


#Write Table:
etable(lin_results_fit_4a_3,lin_results_fit_4a_2,lin_results_fit_4a_1,
       tex = TRUE,
       signifCode= c(`***` = 0.001, `**` = 0.01, `*` = 0.05),
       file='./Tables/Figure_4a_1.txt',
       replace=TRUE,
       title='Results from OLS Regression Results Presented in Figure 4a (Upper half of news quality returned - lower half of digital literacy)')



#filter digital literacy:
lower_half_df_low_diglit <- lower_half_df %>% filter(Dig_Lit_Score < median(Study_5_Control_Treatment$Dig_Lit_Score))

#Remove NAs
lower_half_df_low_diglit <- na.omit(lower_half_df_low_diglit)


#Run OLS Model with clustered standard errors:
lin_results_fit_4a_4 = feols(Seven_Ordinal ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=lower_half_df_low_diglit)

#Run OLS Model with clustered standard errors:
lin_results_fit_4a_5 = feols(Four_Ordinal ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=lower_half_df_low_diglit)

#Run OLS Model with clustered standard errors:
lin_results_fit_4a_6 = feols(True_Dummy ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=lower_half_df_low_diglit)


#Write Table:
etable(lin_results_fit_4a_6,lin_results_fit_4a_5,lin_results_fit_4a_4,
       tex = TRUE,
       signifCode= c(`***` = 0.001, `**` = 0.01, `*` = 0.05),
       file='./Tables/Figure_4a_2.txt',
       replace=TRUE,
       title='Results from OLS Regression Results Presented in Figure 4a (Lower half of news quality returned - lower half of digital literacy)')


####################################################################################################################


############################## Regression Results for Figures 4c and Figures 4d ####################################


####################################################################################################################

#filter only those ideologically congruent to the item of misinformation:
upper_half_df_congruent <- upper_half_df %>% filter(Ideo_Congruence == 1)

#Remove NAs
upper_half_df_congruent <- na.omit(upper_half_df_congruent)


#Run OLS Model with clustered standard errors:
lin_results_fit_4c_1 = feols(Seven_Ordinal ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=upper_half_df_congruent)

#Run OLS Model with clustered standard errors:
lin_results_fit_4c_2 = feols(Four_Ordinal ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=upper_half_df_congruent)

#Run OLS Model with clustered standard errors:
lin_results_fit_4c_3 = feols(True_Dummy ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=upper_half_df_congruent)


#Write Table:
etable(lin_results_fit_4c_3,lin_results_fit_4c_2,lin_results_fit_4c_1,
       tex = TRUE,
       signifCode= c(`***` = 0.001, `**` = 0.01, `*` = 0.05),
       file='./Tables/Figure_4c_1.txt',
       replace=TRUE,
       title='Results from OLS Regression Results Presented in Figure 4c (Upper Half of News Quality Returned - Ideological Congruence)')

#filter only those ideologically congruent to the item of misinformation:
lower_half_df_congruent <- lower_half_df %>% filter(Ideo_Congruence == 1)

#Remove NAs
lower_half_df_congruent <- na.omit(lower_half_df_congruent)

#Run OLS Model with clustered standard errors:
lin_results_fit_4c_4 = feols(Seven_Ordinal ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=lower_half_df_congruent)

#Run OLS Model with clustered standard errors:
lin_results_fit_4c_5 = feols(Four_Ordinal ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=lower_half_df_congruent)

#Run OLS Model with clustered standard errors:
lin_results_fit_4c_6 = feols(True_Dummy ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=lower_half_df_congruent)

#Write Table:
etable(lin_results_fit_4c_6,lin_results_fit_4c_5,lin_results_fit_4c_4,
       tex = TRUE,
       signifCode= c(`***` = 0.001, `**` = 0.01, `*` = 0.05),
       file='./Tables/Figure_4c_2.txt',
       replace=TRUE,
       title='Results from OLS Regression Results Presented in Figure 4c (Lower Half of News Quality Returned - Ideological Congruence)')



#filter only those ideologically incongruent to the item of misinformation:
upper_half_df_incongruent_1 <- upper_half_df %>% filter(Article_Lean == 'Liberal' | Article_Lean ==  'Conservative')
upper_half_df_incongruent_2 <- upper_half_df %>% filter(Article_Lean == 'Conservative' | Article_Lean ==  'Liberal')

upper_half_df_incongruent <- rbind(upper_half_df_incongruent_1,upper_half_df_incongruent_2)

#Remove NAs
upper_half_df_incongruent <- na.omit(upper_half_df_incongruent)

#Run OLS Model with clustered standard errors:
lin_results_fit_4d_1 = feols(Seven_Ordinal ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=upper_half_df_incongruent)

#Run OLS Model with clustered standard errors:
lin_results_fit_4d_2 = feols(Four_Ordinal ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=upper_half_df_incongruent)

#Run OLS Model with clustered standard errors:
lin_results_fit_4d_3 = feols(True_Dummy ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=upper_half_df_incongruent)

#Write Table:
etable(lin_results_fit_4d_3,lin_results_fit_4d_2,lin_results_fit_4d_1,
       tex = TRUE,
       signifCode= c(`***` = 0.001, `**` = 0.01, `*` = 0.05),
       file='./Tables/Figure_4d_1.txt',
       replace=TRUE,
       title='Results from OLS Regression Results Presented in Figure 4d (Upper Half of News Quality Returned - Ideologically Incongruent)')

#filter only those ideologically incongruent to the item of misinformation:
lower_half_df_incongruent_1 <- lower_half_df %>% filter(Article_Lean == 'Liberal' | Article_Lean ==  'Conservative')
lower_half_df_incongruent_2 <- lower_half_df %>% filter(Article_Lean == 'Conservative' | Article_Lean ==  'Liberal')

lower_half_df_incongruent <- rbind(lower_half_df_incongruent_1,lower_half_df_incongruent_2)

#Run OLS Model with clustered standard errors:
lin_results_fit_4d_4 = feols(Seven_Ordinal ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=lower_half_df_incongruent)

#Run OLS Model with clustered standard errors:
lin_results_fit_4d_5 = feols(Four_Ordinal ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=lower_half_df_incongruent)

#Run OLS Model with clustered standard errors:
lin_results_fit_4d_6 = feols(True_Dummy ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=lower_half_df_incongruent)

#Write Table:
etable(lin_results_fit_4d_6,lin_results_fit_4d_5,lin_results_fit_4d_4,
       tex = TRUE,
       signifCode= c(`***` = 0.001, `**` = 0.01, `*` = 0.05),
       file='./Tables/Figure_4d_2.txt',
       replace=TRUE,
       title='Results from OLS Regression Results Presented in Figure 4d (Lower Half of News Quality Returned - Ideologically Incongruent)')

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


########################################## Figure 1a and Figure 1b ####################################################

#Study 1:
#Filter only robust articles:
Study_1_df$Article_day <- gsub('Article_day','',Study_1_df$Article_day)
Study_1_df_robust <- Study_1_df %>% filter(Article_day %in% Robust_Articles)

#Run OLS Model with clustered standard errors:
fit_1_1_robust = feols(Susc_FN ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Study_1_df_robust)
#Produce confidence intervals with clustered standard errors:
CI_1_1_robust <- confint(fit_1_1_robust)

#Run OLS Model with clustered standard errors:
fit_1_2_robust = feols(Likert_Evaluation ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Study_1_df_robust)
#Produce confidence intervals with clustered standard errors:
CI_1_2_robust <- confint(fit_1_2_robust)

#Study 2:
#Filter only robust articles:
Study_2_df$Article_day <- gsub('Article_day','',Study_2_df$Article_day)
Study_2_df_robust <- Study_2_df %>% filter(Article_day %in% Robust_Articles)

#Run OLS Model with clustered standard errors:
fit_2_1_robust = feols(Susc_FN ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Study_2_df_robust)
#Produce confidence intervals with clustered standard errors:
CI_2_1_robust <- confint(fit_2_1_robust)

#Run OLS Model with clustered standard errors:
fit_2_2_robust = feols(Likert_Evaluation ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Study_2_df_robust)
#Produce confidence intervals with clustered standard errors:
CI_2_2_robust <- confint(fit_2_2_robust)

#Study 3:
#Filter only robust articles:
Study_3_df$Article_day <- gsub('Article_day','',Study_3_df$Article_day)
Study_3_df_robust <- Study_3_df %>% filter(Article_day %in% Robust_Articles)

#Run OLS Model with clustered standard errors:
fit_3_1_robust = feols(Susc_FN ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Study_3_df_robust)
#Produce confidence intervals with clustered standard errors:
CI_3_1_robust <- confint(fit_3_1_robust)

#Run OLS Model with clustered standard errors:
fit_3_2_robust = feols(Likert_Evaluation ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Study_3_df_robust)
#Produce confidence intervals with clustered standard errors:
CI_3_2_robust <- confint(fit_3_2_robust)

#Run Model Testing Effect of Searching Online on Belief in Misinformation for Study 4:

#Study 4:
#Filter only robust articles:
Study_4_df$Article_day <- gsub('Article_day','',Study_4_df$Article_day)
Study_4_df_robust <- Study_4_df %>% filter(Article_day %in% Robust_Articles)

#Run OLS Model with clustered standard errors:
fit_4_1_robust = feols(Susc_FN ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Study_4_df_robust)
#Produce confidence intervals with clustered standard errors:
CI_4_1_robust <- confint(fit_4_1_robust)

#Run OLS Model with clustered standard errors:
fit_4_2_robust = feols(Likert_Evaluation ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Study_4_df_robust)
#Produce confidence intervals with clustered standard errors:
CI_4_2_robust <- confint(fit_4_2_robust)

#################################################### Categorical (Rate as True) ####################################################

#Create vector with Study names:
Coef_names <- rev(c('Study 1',
                    'Study 2',
                    'Study 3',
                    'Study 4'))

#Create vector with coefficients:
Coefficients <- rev(c(fit_1_1_robust$coefficients[1],
                      fit_2_1_robust$coefficients[1],
                      fit_3_1_robust$coefficients[1],
                      fit_4_1_robust$coefficients[1]))

#Create vector with upper confidence intervals:
CI_Upper <- rev(c(CI_1_1_robust[1,2],
                  CI_2_1_robust[1,2],
                  CI_3_1_robust[1,2],
                  CI_4_1_robust[1,2]))

#Create vector with lower confidence intervals:
CI_Lower <- rev(c(CI_1_1_robust[1,1],
                  CI_2_1_robust[1,1],
                  CI_3_1_robust[1,1],
                  CI_4_1_robust[1,1]))         

#Put together matrix with data for plot:
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

#Set points on Y-Axis:
d_matrix$x<-c(4,3,2,1)

#Produce plot:
ggplot(data = d_matrix, aes(x = x, y = Coefficients)) +
  geom_hline(aes(yintercept = 0), color = "gray",
             linetype = 2, size = 1.2) +
  geom_point(size=4) +
  geom_linerange(aes(min = CI_Lower,
                     max = CI_Upper),
                 size=1.5) +
  scale_color_manual(values=c('red','blue','purple'), name = "Measure") +
  ylab("\nEffect of Searching Online on Probability \nof Rating Misinformation as True") +
  theme_classic() +
  theme(axis.title.x = element_text(size=18),
        axis.text.x  = element_text(size=16),
        axis.title.y = element_text(size=16),
        axis.text.y  = element_text(size=22),
        plot.title = element_text(size = 16),
        legend.title = element_text(size=16),
        legend.text = element_text(size=14)) +
  ylim(-0.05,0.2) +
  scale_x_continuous(" \n",breaks=c(1,2,3,4),labels=c('Study 4',
                                                      'Study 3',
                                                      'Study 2',
                                                      'Study 1'),limits=c(0.5,4.5)) +
  coord_flip()

#Save figure:
ggsave('./Figures/All_4_Studies_Categorical_Robust.png',height=6,width=8)




#################################################### Ordinal Scale (Seven) ####################################################

#Create vector with Study names:
Coef_names <- rev(c('Study 1',
                    'Study 2',
                    'Study 3',
                    'Study 4'))

#Create vector with coefficients:
Coefficients <- rev(c(fit_1_2_robust$coefficients[1],
                      fit_2_2_robust$coefficients[1],
                      fit_3_2_robust$coefficients[1],
                      fit_4_2_robust$coefficients[1]))

#Create vector with upper confidence intervals:
CI_Upper <- rev(c(CI_1_2_robust[1,2],
                  CI_2_2_robust[1,2],
                  CI_3_2_robust[1,2],
                  CI_4_2_robust[1,2]))

#Create vector with lower confidence intervals:
CI_Lower <- rev(c(CI_1_2_robust[1,1],
                  CI_2_2_robust[1,1],
                  CI_3_2_robust[1,1],
                  CI_4_2_robust[1,1]))         

#Put together matrix with data for plot:
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

#Set points on Y-Axis:
d_matrix$x<-c(4,3,2,1)

#Produce plot:
ggplot(data = d_matrix, aes(x = x, y = Coefficients)) +
  geom_hline(aes(yintercept = 0), color = "gray",
             linetype = 2, size = 1.2) +
  geom_point(size=4) +
  geom_linerange(aes(min = CI_Lower,
                     max = CI_Upper),
                 size=1.5) +
  scale_color_manual(values=c('red','blue','purple'), name = "Measure") +
  ylab("\nEffect of Searching Online on the \nPerceived Veracity of Misinformation (7-point scale) ") +
  theme_classic() +
  theme(axis.title.x = element_text(size=18),
        axis.text.x  = element_text(size=16),
        axis.title.y = element_text(size=16),
        axis.text.y  = element_text(size=22),
        plot.title = element_text(size = 16),
        legend.title = element_text(size=16),
        legend.text = element_text(size=14)) +
  ylim(-0.10,0.8) +
  scale_x_continuous(" \n",breaks=c(1,2,3,4),labels=c('Study 4',
                                                      'Study 3',
                                                      'Study 2',
                                                      'Study 1'),limits=c(0.5,4.5)) +
  coord_flip()

#Save figure:
ggsave('./Figures/All_4_Studies_Ordinal_Robust.png',height=6,width=8)



################################################################################################################

############################ Figure 2a: Study_5_Bar_Graph_Google_Search_ROBUST.png ####################################

################################################################################################################


#Pull in Fact-Checker Ideological Perspective
FC_Ideo_Data <- read.csv('./Data/FC_Ideo_Data.csv')
FC_Ideo_Data$X <- NULL

#Pull in treatment data for Study 5L
Treatment_Data <- read.csv('./Data/Treatment_Data_Study_5.csv')
Treatment_Data$Link_1 <- NULL
Treatment_Data$Link_2 <- NULL

Treatment_Data <- merge(Treatment_Data,FC_Ideo_Data,by='Article_day')

Treatment_Data <- Treatment_Data %>% mutate(Dummy_Ideology = ifelse(Ideology_Score > 0,'Conservative','Moderate'))
Treatment_Data <- Treatment_Data %>% mutate(Dummy_Ideology = ifelse(Ideology_Score < 0,'Liberal',Dummy_Ideology))
Treatment_Data <- Treatment_Data %>% mutate(Ideo_Congruence = ifelse(Dummy_Ideology == Article_Lean,1,0))

#Merge Google Search Results and survey results for each article in Study 5:
#Day 1
Google_results_1 <- read.csv('./Data/Google_Search_Results_Treatment_Day_1_All_New_NG_Ratings.csv')
Treatment_1 <- Treatment_Data %>% filter(Day == 'Day_1')
Google_results_1$Article_Eval <- as.character(Google_results_1$Article_Eval)
Google_results_1$Article_Eval <- paste0('Day_1_',Google_results_1$Article_Eval)
Survey_1 <- merge(Treatment_1,Google_results_1,by.x=c('ResponseId','Article_day'),by.y=c('Respondent_Id','Article_Eval'))
Survey_1 <- Survey_1 %>% select(ResponseId,Day,Article_day,Category,True_Dummy,Seven_Ordinal,Four_Ordinal,Age,Gender,FC_Eval,List_Scores,ALL_URLS,Dummy_Ideology,Ideo_Congruence,Income_Score,Education_Score,Dig_Lit_Score,Prop_Unreliable,Mean_Score,Article_Lean)

#Day 2
Google_results_2 <- read.csv('./Data/Google_Search_Results_Treatment_Day_2_All_New_NG_Ratings.csv')
Treatment_2 <- Treatment_Data %>% filter(Day == 'Day_2')
Google_results_2$Article_Eval <- as.character(Google_results_2$Article_Eval)
Google_results_2$Article_Eval <- paste0('Day_2_',Google_results_2$Article_Eval)
Survey_2 <- merge(Treatment_2,Google_results_2,by.x=c('ResponseId','Article_day'),by.y=c('Respondent_Id','Article_Eval'))
Survey_2 <- Survey_2 %>% select(ResponseId,Day,Article_day,Category,True_Dummy,Seven_Ordinal,Four_Ordinal,Age,Gender,FC_Eval,List_Scores,ALL_URLS,Dummy_Ideology,Ideo_Congruence,Income_Score,Education_Score,Dig_Lit_Score,Prop_Unreliable,Mean_Score,Article_Lean)

#Day 3
Google_results_3 <- read.csv('./Data/Google_Search_Results_Treatment_Day_3_All_New_NG_Ratings.csv')
Treatment_3 <- Treatment_Data %>% filter(Day == 'Day_3')
Google_results_3$Article_Eval <- as.character(Google_results_3$Article_Eval)
Google_results_3$Article_Eval <- paste0('Day_3_',Google_results_3$Article_Eval)
Survey_3 <- merge(Treatment_3,Google_results_3,by.x=c('ResponseId','Article_day'),by.y=c('Respondent_Id','Article_Eval'))
Survey_3 <- Survey_3 %>% select(ResponseId,Day,Article_day,Category,True_Dummy,Seven_Ordinal,Four_Ordinal,Age,Gender,FC_Eval,List_Scores,ALL_URLS,Dummy_Ideology,Ideo_Congruence,Income_Score,Education_Score,Dig_Lit_Score,Prop_Unreliable,Mean_Score,Article_Lean)

#Day 4
Google_results_4 <- read.csv('./Data/Google_Search_Results_Treatment_Day_4_All_New_NG_Ratings.csv')
Treatment_4 <- Treatment_Data %>% filter(Day == 'Day_4')
Google_results_4$Article_Eval <- as.character(Google_results_4$Article_Eval)
Google_results_4$Article_Eval <- paste0('Day_4_',Google_results_4$Article_Eval)
Survey_4 <- merge(Treatment_4,Google_results_4,by.x=c('ResponseId','Article_day'),by.y=c('Respondent_Id','Article_Eval'))
Survey_4 <- Survey_4 %>% select(ResponseId,Day,Article_day,Category,True_Dummy,Seven_Ordinal,Four_Ordinal,Age,Gender,FC_Eval,List_Scores,ALL_URLS,Dummy_Ideology,Ideo_Congruence,Income_Score,Education_Score,Dig_Lit_Score,Prop_Unreliable,Mean_Score,Article_Lean)

#Day 5
Google_results_5 <- read.csv('./Data/Google_Search_Results_Treatment_Day_5_All_New_NG_Ratings.csv')
Treatment_5 <- Treatment_Data %>% filter(Day == 'Day_5')
Google_results_5$Article_Eval <- as.character(Google_results_5$Article_Eval)
Google_results_5$Article_Eval <- paste0('Day_5_',Google_results_5$Article_Eval)
Survey_5 <- merge(Treatment_5,Google_results_5,by.x=c('ResponseId','Article_day'),by.y=c('Respondent_Id','Article_Eval'))
Survey_5 <- Survey_5 %>% select(ResponseId,Day,Article_day,Category,True_Dummy,Seven_Ordinal,Four_Ordinal,Age,Gender,FC_Eval,List_Scores,ALL_URLS,Dummy_Ideology,Ideo_Congruence,Income_Score,Education_Score,Dig_Lit_Score,Prop_Unreliable,Mean_Score,Article_Lean)

#Day 6
Google_results_6 <- read.csv('./Data/Google_Search_Results_Treatment_Day_6_All_New_NG_Ratings.csv')
Treatment_6 <- Treatment_Data %>% filter(Day == 'Day_6')
Google_results_6$Article_Eval <- as.character(Google_results_6$Article_Eval)
Google_results_6$Article_Eval <- paste0('Day_6_',Google_results_6$Article_Eval)
Survey_6 <- merge(Treatment_6,Google_results_6,by.x=c('ResponseId','Article_day'),by.y=c('Respondent_Id','Article_Eval'))
Survey_6 <- Survey_6 %>% select(ResponseId,Day,Article_day,Category,True_Dummy,Seven_Ordinal,Four_Ordinal,Age,Gender,FC_Eval,List_Scores,ALL_URLS,Dummy_Ideology,Ideo_Congruence,Income_Score,Education_Score,Dig_Lit_Score,Prop_Unreliable,Mean_Score,Article_Lean)

#Day 7
Google_results_7 <- read.csv('./Data/Google_Search_Results_Treatment_Day_7_All_New_NG_Ratings.csv')
Treatment_7 <- Treatment_Data %>% filter(Day == 'Day_7')
Google_results_7$Article_Eval <- as.character(Google_results_7$Article_Eval)
Google_results_7$Article_Eval <- paste0('Day_7_',Google_results_7$Article_Eval)
Survey_7 <- merge(Treatment_7,Google_results_7,by.x=c('ResponseId','Article_day'),by.y=c('Respondent_Id','Article_Eval'))
Survey_7 <- Survey_7 %>% select(ResponseId,Day,Article_day,Category,True_Dummy,Seven_Ordinal,Four_Ordinal,Age,Gender,FC_Eval,List_Scores,ALL_URLS,Dummy_Ideology,Ideo_Congruence,Income_Score,Education_Score,Dig_Lit_Score,Prop_Unreliable,Mean_Score,Article_Lean)

#Day 8
Google_results_8 <- read.csv('./Data/Google_Search_Results_Treatment_Day_8_All_New_NG_Ratings.csv')
Treatment_8 <- Treatment_Data %>% filter(Day == 'Day_8')
Google_results_8$Article_Eval <- as.character(Google_results_8$Article_Eval)
Google_results_8$Article_Eval <- paste0('Day_8_',Google_results_8$Article_Eval)
Survey_8 <- merge(Treatment_8,Google_results_8,by.x=c('ResponseId','Article_day'),by.y=c('Respondent_Id','Article_Eval'))
Survey_8 <- Survey_8 %>% select(ResponseId,Day,Article_day,Category,True_Dummy,Seven_Ordinal,Four_Ordinal,Age,Gender,FC_Eval,List_Scores,ALL_URLS,Dummy_Ideology,Ideo_Congruence,Income_Score,Education_Score,Dig_Lit_Score,Prop_Unreliable,Mean_Score,Article_Lean)

#Day 9
Google_results_9 <- read.csv('./Data/Google_Search_Results_Treatment_Day_9_All_New_NG_Ratings.csv')
Treatment_9 <- Treatment_Data %>% filter(Day == 'Day_9')
Google_results_9$Article_Eval <- as.character(Google_results_9$Article_Eval)
Google_results_9$Article_Eval <- paste0('Day_9_',Google_results_9$Article_Eval)
Survey_9 <- merge(Treatment_9,Google_results_9,by.x=c('ResponseId','Article_day'),by.y=c('Respondent_Id','Article_Eval'))
Survey_9 <- Survey_9 %>% select(ResponseId,Day,Article_day,Category,True_Dummy,Seven_Ordinal,Four_Ordinal,Age,Gender,FC_Eval,List_Scores,ALL_URLS,Dummy_Ideology,Ideo_Congruence,Income_Score,Education_Score,Dig_Lit_Score,Prop_Unreliable,Mean_Score,Article_Lean)

#Day 10
Google_results_10 <- read.csv('./Data/Google_Search_Results_Treatment_Day_10_All_New_NG_Ratings.csv')
Treatment_10 <- Treatment_Data %>% filter(Day == 'Day_10')
Google_results_10$Article_Eval <- as.character(Google_results_10$Article_Eval)
Google_results_10$Article_Eval <- paste0('Day_10_',Google_results_10$Article_Eval)
Survey_10 <- merge(Treatment_10,Google_results_10,by.x=c('ResponseId','Article_day'),by.y=c('Respondent_Id','Article_Eval'))
Survey_10 <- Survey_10 %>% select(ResponseId,Day,Article_day,Category,True_Dummy,Seven_Ordinal,Four_Ordinal,Age,Gender,FC_Eval,List_Scores,ALL_URLS,Dummy_Ideology,Ideo_Congruence,Income_Score,Education_Score,Dig_Lit_Score,Prop_Unreliable,Mean_Score,Article_Lean)

#Day 11
Google_results_11 <- read.csv('./Data/Google_Search_Results_Treatment_Day_11_All_New_NG_Ratings.csv')
Treatment_11 <- Treatment_Data %>% filter(Day == 'Day_11')
Google_results_11$Article_Eval <- as.character(Google_results_11$Article_Eval)
Google_results_11$Article_Eval <- paste0('Day_11_',Google_results_11$Article_Eval)
Survey_11 <- merge(Treatment_11,Google_results_11,by.x=c('ResponseId','Article_day'),by.y=c('Respondent_Id','Article_Eval'))
Survey_11 <- Survey_11 %>% select(ResponseId,Day,Article_day,Category,True_Dummy,Seven_Ordinal,Four_Ordinal,Age,Gender,FC_Eval,List_Scores,ALL_URLS,Dummy_Ideology,Ideo_Congruence,Income_Score,Education_Score,Dig_Lit_Score,Prop_Unreliable,Mean_Score,Article_Lean)

#Day 12
Google_results_12 <- read.csv('./Data/Google_Search_Results_Treatment_Day_12_All_New_NG_Ratings.csv')
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

#Create dataframe with this basic search data:
Combined_GS_Survey_Data <- Survey_Unrel


########################## Create news quality data for false/misleading articles in Study 5

#Create string list of news sites scores:
Survey_Unrel$List_Scores <- as.character(Survey_Unrel$List_Scores)
Survey_Unrel$List_Scores <- gsub('\\[|\\]','',Survey_Unrel$List_Scores)
Survey_Unrel$List_Scores <- gsub('\\[|\\]','',Survey_Unrel$List_Scores)

#Filter only false/misleading articles:
Survey_Unrel <- Survey_Unrel %>% filter(FC_Eval == 'FM')

#Create Average Score by Respondent:
Survey_Evaluations <- Survey_Unrel %>% filter(!is.na(Mean_Score))
Survey_Evaluations <- Survey_Evaluations %>% select(ResponseId,Article_day,List_Scores)
Respondent_Evaluations <- Survey_Evaluations %>% select(ResponseId,Article_day)
Respondent_Evaluations <- unique(Respondent_Evaluations)

#Create empty matrix
Search_Results_FM_DF <- matrix(ncol=4)
colnames(Search_Results_FM_DF) <- c('ResponseId',
                                    'Article_day',
                                    'Mean_Score_Final',
                                    'Prop_Unreliable_Final')

#Run For loop that create average score of news sites for each respondent:
for(i in 1:nrow(Respondent_Evaluations)){
  Resp <- as.character(Respondent_Evaluations$ResponseId[i])
  Article <- as.character(Respondent_Evaluations$Article_day[i])
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
  
  Search_Results_FM_DF <- rbind(Search_Results_FM_DF,
                                new_df)
}

#Create dataframe:
Search_Results_FM_DF <- as.data.frame(Search_Results_FM_DF)

#Make mean score and proportion numeric variables:
Search_Results_FM_DF$Mean_Score_Final <- as.character(Search_Results_FM_DF$Mean_Score_Final)
Search_Results_FM_DF$Mean_Score_Final <- as.numeric(Search_Results_FM_DF$Mean_Score_Final)
Search_Results_FM_DF$Prop_Unreliable_Final <- as.character(Search_Results_FM_DF$Prop_Unreliable_Final)
Search_Results_FM_DF$Prop_Unreliable_Final <- as.numeric(Search_Results_FM_DF$Prop_Unreliable_Final)

#Create Dataframe with data for False/Misleading Articles:
New_FM <- Search_Results_FM_DF




########################## Create news quality data for true articles in Study 5:
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
Search_Results_T_DF <- matrix(ncol=4)
colnames(Search_Results_T_DF) <- c('ResponseId',
                                   'Article_day',
                                   'Mean_Score_Final',
                                   'Prop_Unreliable_Final')


#For loop creating average mean scores and proportion unreliable:
for(i in 1:nrow(Respondent_Evaluations)){
  Resp <- as.character(Respondent_Evaluations$ResponseId[i])
  Article <- as.character(Respondent_Evaluations$Article_day[i])
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
  
  Search_Results_T_DF <- rbind(Search_Results_T_DF,
                               new_df)
}


#Create dataframe:
Search_Results_T_DF <- as.data.frame(Search_Results_T_DF)

#Make mean scores and propotion of links that are unreliable numeric variables:
Search_Results_T_DF$Mean_Score_Final <- as.character(Search_Results_T_DF$Mean_Score_Final)
Search_Results_T_DF$Mean_Score_Final <- as.numeric(Search_Results_T_DF$Mean_Score_Final)
Search_Results_T_DF$Prop_Unreliable_Final <- as.character(Search_Results_T_DF$Prop_Unreliable_Final)
Search_Results_T_DF$Prop_Unreliable_Final <- as.numeric(Search_Results_T_DF$Prop_Unreliable_Final)

#Create dataframe with true articles:
New_T <- Search_Results_T_DF

New_T <- New_T %>% filter(Article_day %in% Robust_Articles_T)
New_FM <- New_FM %>% filter(Article_day %in% Robust_Articles)

####################################### Create Figure 2a:

#Create Count of individuals that didnt see any unreliable news sites:
True_Count_Zero <- New_T %>% filter(Prop_Unreliable_Final == 0)
FM_Count_Zero <- New_FM %>% filter(Prop_Unreliable_Final == 0)

#Create Count of individuals that saw at lesat some unreliable news sites:
True_Count_Above_Zero <- New_T %>% filter(Prop_Unreliable_Final > 0)
FM_Count_Above_Zero <- New_FM %>% filter(Prop_Unreliable_Final > 0)



#Create matrix to plot:
Matrix_Dist <- matrix(c(nrow(True_Count_Zero)/nrow(New_T),'Zero','True',
                        nrow(FM_Count_Zero)/nrow(New_FM),'Zero','FM',
                        nrow(True_Count_Above_Zero)/nrow(New_T),'One or more','True',
                        nrow(FM_Count_Above_Zero)/nrow(New_FM),'One or more','FM'),ncol=3,byrow=T)


Matrix_Dist <- as.data.frame(Matrix_Dist)
colnames(Matrix_Dist) <- c('Proportion','Percentage','Article_Rating')
Matrix_Dist$Proportion <- as.character(Matrix_Dist$Proportion)
Matrix_Dist$Proportion <- as.numeric(Matrix_Dist$Proportion)
Matrix_Dist$Percentage <- factor(Matrix_Dist$Percentage,levels=c('Zero',
                                                                 'One or more'))

Matrix_Dist$Article_Rating <- factor(Matrix_Dist$Article_Rating,levels=c('True',
                                                                         'FM'))
Matrix_Dist$Proportion <- round(Matrix_Dist$Proportion,2)


#Produce plot:
ggplot(Matrix_Dist, aes(fill=Percentage, y=Proportion, x=Article_Rating)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_manual(values=c('springgreen3','red3'), name = "Number of News Links \nReturned by Search\nEngines From\nUnreliable Sources") +
  geom_density(adjust=3, alpha=.4) +
  ylab('Proportion of Individuals Whose Search Engine Results         \n Return Unreliable News by Article Type         \n') +
  xlab('\nFact-Checker Rating of Article Individual\n Queries Search Engines About') +
  theme_bw() + 
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(size=24),
        axis.text.x  = element_text(size=22),
        axis.title.y = element_text(size=24),
        axis.text.y  = element_text(size=22),
        title =element_text(size=18, face='bold'),
        legend.text = element_text(size=18)) + guides(fill=guide_legend(
          keywidth=0.3,
          keyheight=0.3,
          default.unit="inch")) +
  geom_text(aes(label=Proportion), position=position_dodge(width=0.9), vjust=-0.25,size=6) +
  ylim(0,1)

#Save figure:
ggsave('./Figures/Study_5_Bar_Graph_Google_Search_ROBUST.png',height=12,width=12)

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

Control_Data <- Control_Data %>% select(ResponseId,Article_day,True_Dummy,Four_Ordinal,Seven_Ordinal,Age,Gender,Education_Score,Income_Score,Ideo_Congruence,Treatment,Dig_Lit_Score,FC_Eval)

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

Treatment_Data <- Treatment_Data %>% select(ResponseId,Article_day,True_Dummy,Four_Ordinal,Seven_Ordinal,Age,Gender,Education_Score,Income_Score,Ideo_Congruence,Treatment,Dig_Lit_Score,FC_Eval)

#Merge data
Study_5_df <- rbind(Treatment_Data,Control_Data)

#Filter only false/misleading articles:
Study_5_df <- Study_5_df %>% filter(FC_Eval == 'FM')
Study_5_df_robust <- Study_5_df %>% filter(Article_day %in% Robust_Articles)

#Create control and treatment dataframes
Study_5_Control_df <- Study_5_df %>% filter(Treatment == 0)
Study_5_Treatment_df <- Study_5_df %>% filter(Treatment == 1)

#Run linear regression and produCce coefficient values:
fit_5_1_robust = glm(True_Dummy ~ Treatment + Age + Gender + Education_Score + Income_Score + Ideo_Congruence + Article_day,data=Study_5_df_robust)
#Produce confidence intervals with clusterd standard errors:
CI_5_1_robust = coefci(fit_5_1_robust, vcov. = vcovCL(fit_5_1_robust, cluster = list(Study_5_df_robust$ResponseId,Study_5_df_robust$Article_day), type = "HC0"))

#Run linear regression and produce coefficient values:
fit_5_2_robust = glm(Four_Ordinal ~Treatment + Age + Gender + Education_Score + Income_Score + Ideo_Congruence + Article_day,data=Study_5_df_robust)
#Produce confidence intervals with clusterd standard errors:
CI_5_2_robust = coefci(fit_5_2_robust, vcov. = vcovCL(fit_5_2_robust, cluster = list(Study_5_df_robust$ResponseId,Study_5_df_robust$Article_day), type = "HC0"))

#Run linear regression and produce coefficient values:
fit_5_3_robust = glm(Seven_Ordinal ~ Treatment + Age + Gender + Education_Score + Income_Score + Ideo_Congruence + Article_day,data=Study_5_df_robust)
#Produce confidence intervals with clusterd standard errors:
CI_5_3_robust = coefci(fit_5_3_robust, vcov. = vcovCL(fit_5_3_robust, cluster = list(Study_5_df_robust$ResponseId,Study_5_df_robust$Article_day), type = "HC0"))

#Create Figure 2b

#Create vector with measures:
Coef_names <- c('Rate as True',
                'Ordinal Scale (4)',
                'Ordinal Scale (7)')

#Create vector with coefficients:
Coefficients <- c(fit_5_1_robust$coefficients[2]/sd(Study_5_Control_df$True_Dummy),
                  fit_5_2_robust$coefficients[2]/sd(Study_5_Control_df$Four_Ordinal),
                  fit_5_3_robust$coefficients[2]/sd(Study_5_Control_df$Seven_Ordinal))

#Create upper confidence intervals:
CI_Upper <- c(CI_5_1_robust[2,2]/sd(Study_5_Control_df$True_Dummy),
              CI_5_2_robust[2,2]/sd(Study_5_Control_df$Four_Ordinal),
              CI_5_3_robust[2,2]/sd(Study_5_Control_df$Seven_Ordinal))            

#Create lower confidence intervals:
CI_Lower <- c(CI_5_1_robust[2,1]/sd(Study_5_Control_df$True_Dummy),
              CI_5_2_robust[2,1]/sd(Study_5_Control_df$Four_Ordinal),
              CI_5_3_robust[2,1]/sd(Study_5_Control_df$Seven_Ordinal))           

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
  scale_x_continuous("Perceived Veracity Scale \n",breaks=c(0.3,0.2,0.1),labels=Coef_names,limits=c(0.0,0.4)) +
  coord_flip()

#Save plot:
ggsave('./Figures/Study_5_1_ROBUST.png',height=8,width=8)

################################################################################################################

########################################## Figure 2c: Coefs_CIs_ROBUST.png ############################################

################################################################################################################

#filter responses:
Study_5_subset_1$Article_day <- gsub('Article_day','',Study_5_subset_1$Article_day)
Robust_Study_5_subset_1 <- Study_5_subset_1 %>% filter(Article_day %in% Robust_Articles)

#Run OLS Model with clustered standard errors:
fit_1_1 = feols(Seven_Ordinal ~ Treatment + Age + Gender + Education_Score + Income_Score + Ideo_Congruence|Article_day, cluster = ~ResponseId+Article_day, data = Robust_Study_5_subset_1)
#Produce confidence intervals with clustered standard errors:
CI_1_1 <- confint(fit_1_1)

#Run model:
fit_1_2 = feols(Four_Ordinal ~ Treatment + Age + Gender + Education_Score + Income_Score + Ideo_Congruence|Article_day, cluster = ~ResponseId+Article_day, data = Robust_Study_5_subset_1)
#Produce confidence intervals with clustered standard errors:
CI_1_2 = confint(fit_1_2)

#Run model:
fit_1_3 = feols(True_Dummy ~ Treatment + Age + Gender + Education_Score + Income_Score + Ideo_Congruence|Article_day, cluster = ~ResponseId+Article_day, data = Robust_Study_5_subset_1)
#Produce confidence intervals with clustered standard errors:
CI_1_3 = confint(fit_1_3)

#Create empty matrix:
Fig_2c_Mat <- matrix(ncol=5)

#Merge data:
New_matr <- matrix(c(round(fit_1_1$coefficients[1]/sd(Robust_Study_5_subset_1$Seven_Ordinal),4),round(CI_1_1[1,1]/sd(Robust_Study_5_subset_1$Seven_Ordinal),4),round(CI_1_1[1,2]/sd(Robust_Study_5_subset_1$Seven_Ordinal),4),'Ordinal (7)','Only Very Reliable News',
                     round(fit_1_2$coefficients[1]/sd(Robust_Study_5_subset_1$Four_Ordinal),4),round(CI_1_2[1,1]/sd(Robust_Study_5_subset_1$Four_Ordinal),4),round(CI_1_2[1,2]/sd(Robust_Study_5_subset_1$Four_Ordinal),4),'Ordinal (4)','Only Very Reliable News',
                     round(fit_1_3$coefficients[1]/sd(Robust_Study_5_subset_1$True_Dummy),4),round(CI_1_3[1,1]/sd(Robust_Study_5_subset_1$True_Dummy),4),round(CI_1_3[1,2]/sd(Robust_Study_5_subset_1$True_Dummy),4),'True (Dummy)','Only Very Reliable News'),ncol=5,byrow=T)
Fig_2c_Mat <- rbind(Fig_2c_Mat,New_matr)


#filter responses:
Study_5_subset_2$Article_day <- gsub('Article_day','',Study_5_subset_2$Article_day)
Robust_Study_5_subset_2 <- Study_5_subset_2 %>% filter(Article_day %in% Robust_Articles)


#Run OLS Model with clustered standard errors:
fit_2_1 = feols(Seven_Ordinal ~ Treatment + Age + Gender + Education_Score + Income_Score + Ideo_Congruence|Article_day, cluster = ~ResponseId+Article_day, data = Robust_Study_5_subset_2)
#Produce confidence intervals with clustered standard errors:
CI_2_1 <- confint(fit_2_1)

#Run model:
fit_2_2 = feols(Four_Ordinal ~ Treatment + Age + Gender + Education_Score + Income_Score + Ideo_Congruence|Article_day, cluster = ~ResponseId+Article_day, data = Robust_Study_5_subset_2)
#Produce confidence intervals with clustered standard errors:
CI_2_2 = confint(fit_2_2)

#Run model:
fit_2_3 = feols(True_Dummy ~ Treatment + Age + Gender + Education_Score + Income_Score + Ideo_Congruence|Article_day, cluster = ~ResponseId+Article_day, data = Robust_Study_5_subset_2)
#Produce confidence intervals with clustered standard errors:
CI_2_3 = confint(fit_2_3)


#Merge data:
New_matr <- matrix(c(round(fit_2_1$coefficients[1]/sd(Robust_Study_5_subset_2$Seven_Ordinal),4),round(CI_2_1[1,1]/sd(Robust_Study_5_subset_2$Seven_Ordinal),4),round(CI_2_1[1,2]/sd(Robust_Study_5_subset_2$Seven_Ordinal),4),'Ordinal (7)','Some Very Unreliable News',
                     round(fit_2_2$coefficients[1]/sd(Robust_Study_5_subset_2$Four_Ordinal),4),round(CI_2_2[1,1]/sd(Robust_Study_5_subset_2$Four_Ordinal),4),round(CI_2_2[1,2]/sd(Robust_Study_5_subset_2$Four_Ordinal),4),'Ordinal (4)','Some Very Unreliable News',
                     round(fit_2_3$coefficients[1]/sd(Robust_Study_5_subset_2$True_Dummy),4),round(CI_2_3[1,1]/sd(Robust_Study_5_subset_2$True_Dummy),4),round(CI_2_3[1,2]/sd(Robust_Study_5_subset_2$True_Dummy),4),'True (Dummy)','Some Very Unreliable News'),ncol=5,byrow=T)

Fig_2c_Mat <- rbind(Fig_2c_Mat,New_matr)


#Transform matrix into dataframe:
Fig_2c_Mat <- as.data.frame(Fig_2c_Mat)

#Name matrix:
colnames(Fig_2c_Mat) <- c('Coef','Low_Conf','Upp_Conf','Measure','Type_News')

#Remove NAs
Fig_2c_Mat <- na.omit(Fig_2c_Mat)

#Clean up matrix:
Fig_2c_Mat$x<-c(0.8,0.9,1.0,1.4,1.5,1.6)
Fig_2c_Mat$Coef <- as.character(Fig_2c_Mat$Coef)
Fig_2c_Mat$Coef <- as.numeric(Fig_2c_Mat$Coef)
Fig_2c_Mat$Upp_Conf <- as.character(Fig_2c_Mat$Upp_Conf)
Fig_2c_Mat$Upp_Conf <- as.numeric(Fig_2c_Mat$Upp_Conf)
Fig_2c_Mat$Low_Conf <- as.character(Fig_2c_Mat$Low_Conf)
Fig_2c_Mat$Low_Conf <- as.numeric(Fig_2c_Mat$Low_Conf)

#Produce plot:
ggplot(data = Fig_2c_Mat, aes(x = x, y = Coef)) +
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
  scale_x_continuous("Type of News Returned by Google Search Engine \n",breaks=c(1.5,0.9),labels=c('Some Unreliable News',
                                                                                                   'Only Reliable News'),limits=c(0.5,2.0)) +
  coord_flip()



#Save Figure:
ggsave('./Figures/Coefs_CIs_ROBUST.png',height=12,width=10)



################################################################################################################

########################################## Figure 2d: Coefs_CIs_2_ROBUST.png ##########################################

################################################################################################################


#Filter responses
Data_subset_lowest$Article_day <- gsub('Article_day','',Data_subset_lowest$Article_day)
Robust_Data_subset_lowest <- Data_subset_lowest %>% filter(Article_day %in% Robust_Articles)

#Run OLS Model with clustered standard errors:
fit_1_1 = feols(Seven_Ordinal ~ Treatment + Age + Gender + Education_Score + Income_Score + Ideo_Congruence|Article_day, cluster = ~ResponseId+Article_day, data = Robust_Data_subset_lowest)
#Produce confidence intervals with clustered standard errors:
CI_1_1 <- confint(fit_1_1)

#Run model:
fit_1_2 = feols(Four_Ordinal ~ Treatment + Age + Gender + Education_Score + Income_Score + Ideo_Congruence|Article_day, cluster = ~ResponseId+Article_day, data = Robust_Data_subset_lowest)
#Produce confidence intervals with clustered standard errors:
CI_1_2 = confint(fit_1_2)

#Run model:
fit_1_3 = feols(True_Dummy ~ Treatment + Age + Gender + Education_Score + Income_Score + Ideo_Congruence|Article_day, cluster = ~ResponseId+Article_day, data = Robust_Data_subset_lowest)
#Produce confidence intervals with clustered standard errors:
CI_1_3 = confint(fit_1_3)


#Merge Coeficients and confidence intervals:

Fig_2d_Mat <- matrix(ncol=5)
New_matr <- matrix(c(round(fit_1_1$coefficients[1]/sd(Robust_Data_subset_lowest$Seven_Ordinal),4),round(CI_2_1[1,1]/sd(Robust_Data_subset_lowest$Seven_Ordinal),4),round(CI_2_1[1,2]/sd(Robust_Data_subset_lowest$Seven_Ordinal),4),'Ordinal (7)','0-25%',
                     round(fit_1_2$coefficients[1]/sd(Robust_Data_subset_lowest$Four_Ordinal),4),round(CI_2_2[1,1]/sd(Robust_Data_subset_lowest$Four_Ordinal),4),round(CI_2_2[1,2]/sd(Robust_Data_subset_lowest$Four_Ordinal),4),'Ordinal (4)','0-25%',
                     round(fit_1_3$coefficients[1]/sd(Robust_Data_subset_lowest$True_Dummy),4),round(CI_2_3[1,1]/sd(Robust_Data_subset_lowest$True_Dummy),4),round(CI_2_3[1,2]/sd(Robust_Data_subset_lowest$True_Dummy),4),'True (Dummy)','0-25%'),ncol=5,byrow=T)
#Bind new data:
Fig_2d_Mat <- rbind(Fig_2d_Mat,New_matr)


#filter responses:
Data_subset_2nd_lowest$Article_day <- gsub('Article_day','',Data_subset_2nd_lowest$Article_day)
Robust_Data_subset_2nd_lowest <- Data_subset_2nd_lowest %>% filter(Article_day %in% Robust_Articles)

#Run OLS Model with clustered standard errors:
fit_2_1 = feols(Seven_Ordinal ~ Treatment + Age + Gender + Education_Score + Income_Score + Ideo_Congruence|Article_day, cluster = ~ResponseId+Article_day, data = Robust_Data_subset_2nd_lowest)
#Produce confidence intervals with clustered standard errors:
CI_2_1 <- confint(fit_2_1)

#Run model:
fit_2_2 = feols(Four_Ordinal ~ Treatment + Age + Gender + Education_Score + Income_Score + Ideo_Congruence|Article_day, cluster = ~ResponseId+Article_day, data = Robust_Data_subset_2nd_lowest)
#Produce confidence intervals with clustered standard errors:
CI_2_2 = confint(fit_2_2)

#Run model:
fit_2_3 = feols(True_Dummy ~ Treatment + Age + Gender + Education_Score + Income_Score + Ideo_Congruence|Article_day, cluster = ~ResponseId+Article_day, data = Robust_Data_subset_2nd_lowest)
#Produce confidence intervals with clustered standard errors:
CI_2_3 = confint(fit_2_3)

#Merge Coeficients and confidence intervals:
New_matr <- matrix(c(round(fit_2_1$coefficients[1]/sd(Robust_Data_subset_2nd_lowest$Seven_Ordinal),4),round(CI_2_1[1,1]/sd(Robust_Data_subset_2nd_lowest$Seven_Ordinal),4),round(CI_2_1[1,2]/sd(Robust_Data_subset_2nd_lowest$Seven_Ordinal),4),'Ordinal (7)','25-50%',
                     round(fit_2_2$coefficients[1]/sd(Robust_Data_subset_2nd_lowest$Four_Ordinal),4),round(CI_2_2[1,1]/sd(Robust_Data_subset_2nd_lowest$Four_Ordinal),4),round(CI_2_2[1,2]/sd(Robust_Data_subset_2nd_lowest$Four_Ordinal),4),'Ordinal (4)','25-50%',
                     round(fit_2_3$coefficients[1]/sd(Robust_Data_subset_2nd_lowest$True_Dummy),4),round(CI_2_3[1,1]/sd(Robust_Data_subset_2nd_lowest$True_Dummy),4),round(CI_2_3[1,2]/sd(Robust_Data_subset_2nd_lowest$True_Dummy),4),'True (Dummy)','25-50%'),ncol=5,byrow=T)

#Bind new data:
Fig_2d_Mat <- rbind(Fig_2d_Mat,New_matr)


#filter responses:
Data_subset_3rd_lowest$Article_day <- gsub('Article_day','',Data_subset_3rd_lowest$Article_day)
Robust_Data_subset_3rd_lowest <- Data_subset_3rd_lowest %>% filter(Article_day %in% Robust_Articles)


#Run OLS Model with clustered standard errors:
fit_3_1 = feols(Seven_Ordinal ~ Treatment + Age + Gender + Education_Score + Income_Score + Ideo_Congruence|Article_day, cluster = ~ResponseId+Article_day, data = Robust_Data_subset_3rd_lowest)
#Produce confidence intervals with clustered standard errors:
CI_3_1 <- confint(fit_3_1)

#Run model:
fit_3_2 = feols(Four_Ordinal ~ Treatment + Age + Gender + Education_Score + Income_Score + Ideo_Congruence|Article_day, cluster = ~ResponseId+Article_day, data = Robust_Data_subset_3rd_lowest)
#Produce confidence intervals with clustered standard errors:
CI_3_2 = confint(fit_3_2)

#Run model:
fit_3_3 = feols(True_Dummy ~ Treatment + Age + Gender + Education_Score + Income_Score + Ideo_Congruence|Article_day, cluster = ~ResponseId+Article_day, data = Robust_Data_subset_3rd_lowest)
#Produce confidence intervals with clustered standard errors:
CI_3_3 = confint(fit_3_3)


New_matr <- matrix(c(round(fit_3_1$coefficients[1]/sd(Robust_Data_subset_3rd_lowest$Seven_Ordinal),4),round(CI_3_1[1,1]/sd(Robust_Data_subset_3rd_lowest$Seven_Ordinal),4),round(CI_3_1[1,2]/sd(Robust_Data_subset_3rd_lowest$Seven_Ordinal),4),'Ordinal (7)','50-75%',
                     round(fit_3_2$coefficients[1]/sd(Robust_Data_subset_3rd_lowest$Four_Ordinal),4),round(CI_3_2[1,1]/sd(Robust_Data_subset_3rd_lowest$Four_Ordinal),4),round(CI_3_2[1,2]/sd(Robust_Data_subset_3rd_lowest$Four_Ordinal),4),'Ordinal (4)','50-75%',
                     round(fit_3_3$coefficients[1]/sd(Robust_Data_subset_3rd_lowest$True_Dummy),4),round(CI_3_3[1,1]/sd(Robust_Data_subset_3rd_lowest$True_Dummy),4),round(CI_3_3[1,2]/sd(Robust_Data_subset_3rd_lowest$True_Dummy),4),'True (Dummy)','50-75%'),ncol=5,byrow=T)
#Bind new data:
Fig_2d_Mat <- rbind(Fig_2d_Mat,New_matr)


#filter responses:
Data_subset_highest$Article_day <- gsub('Article_day','',Data_subset_highest$Article_day)
Robust_Data_subset_highest <- Data_subset_highest %>% filter(Article_day %in% Robust_Articles)


fit_4_1 = feols(Seven_Ordinal ~ Treatment + Age + Gender + Education_Score + Income_Score + Ideo_Congruence|Article_day, cluster = ~ResponseId+Article_day, data = Robust_Data_subset_highest)
#Produce confidence intervals with clustered standard errors:
CI_4_1 <- confint(fit_4_1)

#Run model:
fit_4_2 = feols(Four_Ordinal ~ Treatment + Age + Gender + Education_Score + Income_Score + Ideo_Congruence|Article_day, cluster = ~ResponseId+Article_day, data = Robust_Data_subset_highest)
#Produce confidence intervals with clustered standard errors:
CI_4_2 = confint(fit_4_2)

#Run model:
fit_4_3 = feols(True_Dummy ~ Treatment + Age + Gender + Education_Score + Income_Score + Ideo_Congruence|Article_day, cluster = ~ResponseId+Article_day, data = Robust_Data_subset_highest)
#Produce confidence intervals with clustered standard errors:
CI_4_3 = confint(fit_4_3)

New_matr <- matrix(c(round(fit_4_1$coefficients[1]/sd(Robust_Data_subset_highest$Seven_Ordinal),4),round(CI_4_1[1,1]/sd(Robust_Data_subset_highest$Seven_Ordinal),4),round(CI_4_1[1,2]/sd(Robust_Data_subset_highest$Seven_Ordinal),4),'Ordinal (7)','75-100%',
                     round(fit_4_2$coefficients[1]/sd(Robust_Data_subset_highest$Four_Ordinal),4),round(CI_4_2[1,1]/sd(Robust_Data_subset_highest$Four_Ordinal),4),round(CI_4_2[1,2]/sd(Robust_Data_subset_highest$Four_Ordinal),4),'Ordinal (4)','75-100%',
                     round(fit_4_3$coefficients[1]/sd(Robust_Data_subset_highest$True_Dummy),4),round(CI_4_3[1,1]/sd(Robust_Data_subset_highest$True_Dummy),4),round(CI_4_3[1,2]/sd(Robust_Data_subset_highest$True_Dummy),4),'True (Dummy)','75-100%'),ncol=5,byrow=T)
#Bind new data:
Fig_2d_Mat <- rbind(Fig_2d_Mat,New_matr)


#Create dataframe to produce plot
Fig_2d_Mat <- as.data.frame(Fig_2d_Mat)
Fig_2d_Mat <- na.omit(Fig_2d_Mat)
colnames(Fig_2d_Mat) <- c('Coef','Low_Conf','Upp_Conf','Measure','Type_News')
Fig_2d_Mat <- na.omit(Fig_2d_Mat)
Fig_2d_Mat$x<-c(0.8,0.9,1.0,1.4,1.5,1.6,2.0,2.1,2.2,2.6,2.7,2.8)
Fig_2d_Mat$Coef <- as.character(Fig_2d_Mat$Coef)
Fig_2d_Mat$Coef <- as.numeric(Fig_2d_Mat$Coef)
Fig_2d_Mat$Upp_Conf <- as.character(Fig_2d_Mat$Upp_Conf)
Fig_2d_Mat$Upp_Conf <- as.numeric(Fig_2d_Mat$Upp_Conf)
Fig_2d_Mat$Low_Conf <- as.character(Fig_2d_Mat$Low_Conf)
Fig_2d_Mat$Low_Conf <- as.numeric(Fig_2d_Mat$Low_Conf)


#Produce Plot:
ggplot(data = Fig_2d_Mat, aes(x = x, y = Coef)) +
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
  ylim(-0.6,0.7) +
  scale_x_continuous("Quartile of News Quality Returned by Google Search Engine \n",breaks=c(2.7,2.1,1.5,0.9),labels=c('75-100%',
                                                                                                                       '50-75%',
                                                                                                                       '25-50%',
                                                                                                                       '0-25%'),limits=c(0.5,3.0)) +
  coord_flip()


#Save Figure:
ggsave('./Figures/Coefs_CIs_2_ROBUST.png',height=12,width=10)



################################################################################################################

################################# Figure 3: Coefs_CIs_Predicting_Unrel_Dummy_ROBUST.png ###############################

################################################################################################################

Treatment_rel_data <- merge(T_Data,Data_Scores,by=c('ResponseId','Article_day'))
Treatment_rel_data$Unreliable_Dummy <- ifelse(Treatment_rel_data$Tot_Unreliable > 1,1,0)

#Filter Robust Mode Articles:
Treatment_rel_data <- Treatment_rel_data %>% filter(Article_day %in% Robust_Articles)



#Run model:
fit_unrel_dumm = feols(Unreliable_Dummy ~ Age + Gender + Education_Score + Income_Score + Ideo_Congruence + Dig_Lit_Score|Article_day, cluster = ~ResponseId+Article_day, data = Treatment_rel_data)
#Produce confidence intervals with clustered standard errors:
CI_unrel_dumm = confint(fit_unrel_dumm)

Search_Survey <- Treatment_rel_data

#Create dataframe with coefficients and confidence intervals:
Coefficients <- c(round(fit_unrel_dumm$coefficients[1]*sd(Search_Survey$Age),4),
                  round(fit_unrel_dumm$coefficients[2]*sd(Search_Survey$Gender),4),
                  round(fit_unrel_dumm$coefficients[3]*sd(Search_Survey$Education_Score),4),
                  round(fit_unrel_dumm$coefficients[4]*sd(Search_Survey$Income_Score),4),
                  round(fit_unrel_dumm$coefficients[5]*sd(Search_Survey$Ideo_Congruence),4),
                  round(fit_unrel_dumm$coefficients[6]*sd(Search_Survey$Dig_Lit_Score),4))

CI_Upper <- c(round(CI_unrel_dumm[1,1]*sd(Search_Survey$Age),4),
              round(CI_unrel_dumm[2,1]*sd(Search_Survey$Gender),4),
              round(CI_unrel_dumm[3,1]*sd(Search_Survey$Education_Score),4),
              round(CI_unrel_dumm[4,1]*sd(Search_Survey$Income_Score),4),
              round(CI_unrel_dumm[5,1]*sd(Search_Survey$Ideo_Congruence),4),
              round(CI_unrel_dumm[6,1]*sd(Search_Survey$Dig_Lit_Score),4))            

CI_Lower <- c(round(CI_unrel_dumm[1,2]*sd(Search_Survey$Age),4),
              round(CI_unrel_dumm[2,2]*sd(Search_Survey$Gender),4),
              round(CI_unrel_dumm[3,2]*sd(Search_Survey$Education_Score),4),
              round(CI_unrel_dumm[4,2]*sd(Search_Survey$Income_Score),4),
              round(CI_unrel_dumm[5,2]*sd(Search_Survey$Ideo_Congruence),4),
              round(CI_unrel_dumm[6,2]*sd(Search_Survey$Dig_Lit_Score),4))           

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

################################ Figure 3b: Coefs_CIs_Predicting_Headline_Link.png  ##########################################

################################################################################################################

#Pull-in search data data:
Headline_coding <- read.csv('./Data/Headline_Coding.csv')

#Select variables needed:
Headline_coding <- Headline_coding %>% select(ResponseId,Article_day,Headline_Link,Age,Gender,Education_Score,Income_Score,Ideo_Congruence,Dig_Lit_Score)

#Filter Robust Mode Articles:
Headline_coding <- Headline_coding %>% filter(Article_day %in% Robust_Articles)


#Run model:
fit_head_url = feols(Headline_Link ~ Age + Gender + Education_Score + Income_Score + Ideo_Congruence + Dig_Lit_Score|Article_day, cluster = ~ResponseId+Article_day, data = Headline_coding)
#Produce confidence intervals with clustered standard errors:
CI_head_url = confint(fit_head_url)

#List of coefficients
Coefficients <- c(round(fit_head_url$coefficients[1]*sd(Headline_coding$Age),4),
                  round(fit_head_url$coefficients[2]*sd(Headline_coding$Gender),4),
                  round(fit_head_url$coefficients[3]*sd(Headline_coding$Education_Score),4),
                  round(fit_head_url$coefficients[4]*sd(Headline_coding$Income_Score),4),
                  round(fit_head_url$coefficients[5]*sd(Headline_coding$Ideo_Congruence),4),
                  round(fit_head_url$coefficients[6]*sd(Headline_coding$Dig_Lit_Score),4))

#Vector of upper confidence intervals:
CI_Upper <- c(round(CI_head_url[1,1]*sd(Headline_coding$Age),4),
              round(CI_head_url[2,1]*sd(Headline_coding$Gender),4),
              round(CI_head_url[3,1]*sd(Headline_coding$Education_Score),4),
              round(CI_head_url[4,1]*sd(Headline_coding$Income_Score),4),
              round(CI_head_url[5,1]*sd(Headline_coding$Ideo_Congruence),4),
              round(CI_head_url[6,1]*sd(Headline_coding$Dig_Lit_Score),4))            

#Vector of lower confidence intervals:
CI_Lower <- c(round(CI_head_url[1,2]*sd(Headline_coding$Age),4),
              round(CI_head_url[2,2]*sd(Headline_coding$Gender),4),
              round(CI_head_url[3,2]*sd(Headline_coding$Education_Score),4),
              round(CI_head_url[4,2]*sd(Headline_coding$Income_Score),4),
              round(CI_head_url[5,2]*sd(Headline_coding$Ideo_Congruence),4),
              round(CI_head_url[6,2]*sd(Headline_coding$Dig_Lit_Score),4))           

#Vector of coefficient names:
Coef_names <- c('Age',
                'Gender',
                'Education',
                'Income',
                'Ideological\nCongruence',
                'Digital Literacy')

#Put together matrix to create plot:
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
  ylab("\n The Effect of a One Standard Deviation Increase of Ind. Variable\n on Probability of using Headline or Link as Search Term") +
  theme_classic() +
  theme(axis.title.x = element_text(size=22),
        axis.text.x  = element_text(size=20),
        axis.title.y = element_text(size=20),
        axis.text.y  = element_text(size=22),
        plot.title = element_text(size = 20),
        legend.title = element_text(size=20),
        legend.text = element_text(size=20)) +
  ylim(-0.1,0.1) +
  scale_x_continuous("Demographic Covariates \n",breaks=c(0.1,0.2,0.3,0.4,0.5,0.6),labels=Coef_names,limits=c(0.0,0.7)) +
  coord_flip()

#Save figure:
ggsave('./Figures/Coefs_CIs_Predicting_Headline_Link_ROBUST.png',height=8,width=12)


################################################################################################################

################################ Figure 3c: Study_5_Bar_Graph_Google_Search_HL.png  ##########################################

################################################################################################################

#Filter Robust Mode Articles:
Search_Results_FM_DF_R <- Search_Results_FM_DF %>% filter(Article_day %in% Robust_Articles)
Search_Results_T_DF_R <- Search_Results_T_DF %>% filter(Article_day %in% Robust_Articles_T)

#Bind the Study 5 data from search results:
Search_Results_T_FM_DF <- rbind(Search_Results_FM_DF_R,Search_Results_T_DF_R)

#Merge search results data and search engine coding results:
Search_Results_T_FM_DF <- merge(Search_Results_T_FM_DF,Headline_coding,by=c('ResponseId','Article_day'))


#Create dataset of search queries that does not use headline/link:
SQ_No_Headline <- Search_Results_T_FM_DF %>% filter(Headline_Link == 0)



#Create dataset of search queries that does use headline/link:
SQ_Headline_Data <- Search_Results_T_FM_DF %>% filter(Headline_Link == 1)

#Create Count of individuals that didnt see any unreliable news sites:
True_Count_Zero <- SQ_No_Headline %>% filter(Prop_Unreliable_Final == 0)
FM_Count_Zero <- SQ_Headline_Data %>% filter(Prop_Unreliable_Final == 0)

#Create Count of individuals that saw at lesat some unreliable news sites:
True_Count_Above_Zero <- SQ_No_Headline %>% filter(Prop_Unreliable_Final > 0)
FM_Count_Above_Zero <- SQ_Headline_Data %>% filter(Prop_Unreliable_Final > 0)



#Create matrix to plot:
Matrix_Dist <- matrix(c(nrow(True_Count_Zero)/nrow(SQ_No_Headline),'Zero','Other',
                        nrow(FM_Count_Zero)/nrow(SQ_Headline_Data),'Zero','Headline/Link',
                        nrow(True_Count_Above_Zero)/nrow(SQ_No_Headline),'One or more','Other',
                        nrow(FM_Count_Above_Zero)/nrow(SQ_Headline_Data),'One or more','Headline/Link'),ncol=3,byrow=T)
Matrix_Dist <- as.data.frame(Matrix_Dist)
colnames(Matrix_Dist) <- c('Proportion','Percentage','Article_Rating')
Matrix_Dist$Proportion <- as.character(Matrix_Dist$Proportion)
Matrix_Dist$Proportion <- as.numeric(Matrix_Dist$Proportion)
Matrix_Dist$Percentage <- factor(Matrix_Dist$Percentage,levels=c('Zero',
                                                                 'One or more'))
Matrix_Dist$Article_Rating <- factor(Matrix_Dist$Article_Rating,levels=c('Other',
                                                                         'Headline/Link'))
Matrix_Dist$Proportion <- round(Matrix_Dist$Proportion,2)
#Produce plot:
ggplot(Matrix_Dist, aes(fill=Percentage, y=Proportion, x=Article_Rating)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_manual(values=c('springgreen3','red3'), name = "Number of News Links \nReturned by Search\nEngines From\nUnreliable Sources") +
  geom_density(adjust=3, alpha=.4) +
  ylab('Proportion of Search Queries That Return Search Engine Results That    \n Return Unreliable News by Article Type         \n') +
  xlab('\nType of Search Query Used') +
  theme_bw() + 
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(size=24),
        axis.text.x  = element_text(size=22),
        axis.title.y = element_text(size=24),
        axis.text.y  = element_text(size=22),
        title =element_text(size=18, face='bold'),
        legend.text = element_text(size=18)) + guides(fill=guide_legend(
          keywidth=0.3,
          keyheight=0.3,
          default.unit="inch")) +
  geom_text(aes(label=Proportion), position=position_dodge(width=0.9), vjust=-0.25,size=6) +
  ylim(0,1)

#Save figure:
ggsave('./Figures/Study_5_Bar_Graph_Google_Search_HL_ROBUST.png',height=12,width=12)


################################################################################################################

################################ Figure 4b: Quantiles_High_DL_FULL_ROBUST.png ##########################################

################################################################################################################


#filter responses:
upper_half_df_high_diglit$Article_day <- gsub('Article_day','',upper_half_df_high_diglit$Article_day)
Robust_upper_half_df_high_diglit <- upper_half_df_high_diglit %>% filter(Article_day %in% Robust_Articles)

fit_1_1 = feols(Seven_Ordinal ~ Treatment + Age + Gender + Education_Score + Income_Score + Ideo_Congruence|Article_day, cluster = ~ResponseId+Article_day, data = Robust_upper_half_df_high_diglit)
#Produce confidence intervals with clustered standard errors:
CI_1_1 <- confint(fit_1_1)

#Run model:
fit_1_2 = feols(Four_Ordinal ~ Treatment + Age + Gender + Education_Score + Income_Score + Ideo_Congruence|Article_day, cluster = ~ResponseId+Article_day, data = Robust_upper_half_df_high_diglit)
#Produce confidence intervals with clustered standard errors:
CI_1_2 = confint(fit_1_2)

#Run model:
fit_1_3 = feols(True_Dummy ~ Treatment + Age + Gender + Education_Score + Income_Score + Ideo_Congruence|Article_day, cluster = ~ResponseId+Article_day, data = Robust_upper_half_df_high_diglit)
#Produce confidence intervals with clustered standard errors:
CI_1_3 = confint(fit_1_3)

Fig_4b_Mat <- matrix(ncol=5)

New_matr <- matrix(c(round(fit_1_1$coefficients[1]/sd(Robust_upper_half_df_high_diglit$Seven_Ordinal),4),round(CI_1_1[1,1]/sd(Robust_upper_half_df_high_diglit$Seven_Ordinal),4),round(CI_1_1[1,2]/sd(Robust_upper_half_df_high_diglit$Seven_Ordinal),4),'Ordinal (7)','Top 50% News Quality',
                     round(fit_1_2$coefficients[1]/sd(Robust_upper_half_df_high_diglit$Four_Ordinal),4),round(CI_1_2[1,1]/sd(Robust_upper_half_df_high_diglit$Four_Ordinal),4),round(CI_1_2[1,2]/sd(Robust_upper_half_df_high_diglit$Four_Ordinal),4),'Ordinal (4)','Top 50% News Quality',
                     round(fit_1_3$coefficients[1]/sd(Robust_upper_half_df_high_diglit$True_Dummy),4),round(CI_1_3[1,1]/sd(Robust_upper_half_df_high_diglit$True_Dummy),4),round(CI_1_3[1,2]/sd(Robust_upper_half_df_high_diglit$True_Dummy),4),'True (Dummy)','Top 50% News Quality'),ncol=5,byrow=T)

Fig_4b_Mat <- rbind(Fig_4b_Mat,New_matr)


#filter responses:
lower_half_df_high_diglit$Article_day <- gsub('Article_day','',lower_half_df_high_diglit$Article_day)
Robust_lower_half_df_high_diglit <- lower_half_df_high_diglit %>% filter(Article_day %in% Robust_Articles)

fit_2_1 = feols(Seven_Ordinal ~ Treatment + Age + Gender + Education_Score + Income_Score + Ideo_Congruence|Article_day, cluster = ~ResponseId+Article_day, data = Robust_lower_half_df_high_diglit)
#Produce confidence intervals with clustered standard errors:
CI_2_1 <- confint(fit_2_1)

#Run model:
fit_2_2 = feols(Four_Ordinal ~ Treatment + Age + Gender + Education_Score + Income_Score + Ideo_Congruence|Article_day, cluster = ~ResponseId+Article_day, data = Robust_lower_half_df_high_diglit)
#Produce confidence intervals with clustered standard errors:
CI_2_2 = confint(fit_2_2)

#Run model:
fit_2_3 = feols(True_Dummy ~ Treatment + Age + Gender + Education_Score + Income_Score + Ideo_Congruence|Article_day, cluster = ~ResponseId+Article_day, data = Robust_lower_half_df_high_diglit)
#Produce confidence intervals with clustered standard errors:
CI_2_3 = confint(fit_2_3)

#Add to matrix:
New_matr <- matrix(c(round(fit_2_1$coefficients[1]/sd(Robust_lower_half_df_high_diglit$Seven_Ordinal),4),round(CI_2_1[1,1]/sd(Robust_lower_half_df_high_diglit$Seven_Ordinal),4),round(CI_2_1[1,2]/sd(Robust_lower_half_df_high_diglit$Seven_Ordinal),4),'Ordinal (7)','Bottom 50% News Quality',
                     round(fit_2_2$coefficients[1]/sd(Robust_lower_half_df_high_diglit$Four_Ordinal),4),round(CI_2_2[1,1]/sd(Robust_lower_half_df_high_diglit$Four_Ordinal),4),round(CI_2_2[1,2]/sd(Robust_lower_half_df_high_diglit$Four_Ordinal),4),'Ordinal (4)','Bottom 50% News Quality',
                     round(fit_2_3$coefficients[1]/sd(Robust_lower_half_df_high_diglit$True_Dummy),4),round(CI_2_3[1,1]/sd(Robust_lower_half_df_high_diglit$True_Dummy),4),round(CI_2_3[1,2]/sd(Robust_lower_half_df_high_diglit$True_Dummy),4),'True (Dummy)','Bottom 50% News Quality'),ncol=5,byrow=T)
Fig_4b_Mat <- rbind(Fig_4b_Mat,New_matr)


#Use dataframe to create plot:
Fig_4b_Mat <- as.data.frame(Fig_4b_Mat)
Fig_4b_Mat <- na.omit(Fig_4b_Mat)
colnames(Fig_4b_Mat) <- c('Coef','Low_Conf','Upp_Conf','Measure','Type_News')
Fig_4b_Mat <- na.omit(Fig_4b_Mat)

Fig_4b_Mat$x<- c(1.4,1.5,1.6,0.8,0.9,1.0)
Fig_4b_Mat$Coef <- as.character(Fig_4b_Mat$Coef)
Fig_4b_Mat$Coef <- as.numeric(Fig_4b_Mat$Coef)
Fig_4b_Mat$Upp_Conf <- as.character(Fig_4b_Mat$Upp_Conf)
Fig_4b_Mat$Upp_Conf <- as.numeric(Fig_4b_Mat$Upp_Conf)
Fig_4b_Mat$Low_Conf <- as.character(Fig_4b_Mat$Low_Conf)
Fig_4b_Mat$Low_Conf <- as.numeric(Fig_4b_Mat$Low_Conf)
Fig_4b_Mat$Type_News <- factor(Fig_4b_Mat$Type_News,levels=c('Bottom 50% News Quality',
                                                             'Top 50% News Quality'))

#Produce Plot:
ggplot(data = Fig_4b_Mat, aes(x = x, y = Coef)) +
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
  ylim(-0.6,0.7) +
  scale_x_continuous("News Quality Returned by Search Engines \n",breaks=c(0.9,1.5),labels=c('Bottom 50%\nNews Quality',
                                                                                             'Top 50%\nNews Quality'),limits=c(0.5,2.0)) +
  coord_flip()


#Save Figure:
ggsave('./Figures/Quantiles_High_DL_FULL_ROBUST.png',height=8,width=8)


################################################################################################################

############################## Figure 4a: Quantiles_Low_DL_FULL_ROBUST.png ############################################

################################################################################################################

#filter responses
upper_half_df_low_diglit$Article_day <- gsub('Article_day','',upper_half_df_low_diglit$Article_day)
Robust_upper_half_df_low_diglit <- upper_half_df_low_diglit %>% filter(Article_day %in% Robust_Articles)

fit_1_1 = feols(Seven_Ordinal ~ Treatment + Age + Gender + Education_Score + Income_Score + Ideo_Congruence|Article_day, cluster = ~ResponseId+Article_day, data = Robust_upper_half_df_low_diglit)
#Produce confidence intervals with clustered standard errors:
CI_1_1 <- confint(fit_1_1)

#Run model:
fit_1_2 = feols(Four_Ordinal ~ Treatment + Age + Gender + Education_Score + Income_Score + Ideo_Congruence|Article_day, cluster = ~ResponseId+Article_day, data = Robust_upper_half_df_low_diglit)
#Produce confidence intervals with clustered standard errors:
CI_1_2 = confint(fit_1_2)

#Run model:
fit_1_3 = feols(True_Dummy ~ Treatment + Age + Gender + Education_Score + Income_Score + Ideo_Congruence|Article_day, cluster = ~ResponseId+Article_day, data = Robust_upper_half_df_low_diglit)
#Produce confidence intervals with clustered standard errors:
CI_1_3 = confint(fit_1_3)

Fig_4a_Mat <- matrix(ncol=5)

New_matr <- matrix(c(round(fit_1_1$coefficients[1]/sd(Robust_upper_half_df_low_diglit$Seven_Ordinal),4),round(CI_1_1[1,1]/sd(Robust_upper_half_df_low_diglit$Seven_Ordinal),4),round(CI_1_1[1,2]/sd(Robust_upper_half_df_low_diglit$Seven_Ordinal),4),'Ordinal (7)','Top 50% News Quality',
                     round(fit_1_2$coefficients[1]/sd(Robust_upper_half_df_low_diglit$Four_Ordinal),4),round(CI_1_2[1,1]/sd(Robust_upper_half_df_low_diglit$Four_Ordinal),4),round(CI_1_2[1,2]/sd(Robust_upper_half_df_low_diglit$Four_Ordinal),4),'Ordinal (4)','Top 50% News Quality',
                     round(fit_1_3$coefficients[1]/sd(Robust_upper_half_df_low_diglit$True_Dummy),4),round(CI_1_3[1,1]/sd(Robust_upper_half_df_low_diglit$True_Dummy),4),round(CI_1_3[1,2]/sd(Robust_upper_half_df_low_diglit$True_Dummy),4),'True (Dummy)','Top 50% News Quality'),ncol=5,byrow=T)

Fig_4a_Mat <- rbind(Fig_4a_Mat,New_matr)


#filter responses
lower_half_df_low_diglit$Article_day <- gsub('Article_day','',lower_half_df_low_diglit$Article_day)
Robust_lower_half_df_low_diglit <- lower_half_df_low_diglit %>% filter(Article_day %in% Robust_Articles)

#Run Model
fit_2_1 = feols(Seven_Ordinal ~ Treatment + Age + Gender + Education_Score + Income_Score + Ideo_Congruence|Article_day, cluster = ~ResponseId+Article_day, data = Robust_lower_half_df_low_diglit)
#Produce confidence intervals with clustered standard errors:
CI_2_1 <- confint(fit_2_1)

#Run model:
fit_2_2 = feols(Four_Ordinal ~ Treatment + Age + Gender + Education_Score + Income_Score + Ideo_Congruence|Article_day, cluster = ~ResponseId+Article_day, data = Robust_lower_half_df_low_diglit)
#Produce confidence intervals with clustered standard errors:
CI_2_2 = confint(fit_2_2)

#Run model:
fit_2_3 = feols(True_Dummy ~ Treatment + Age + Gender + Education_Score + Income_Score + Ideo_Congruence|Article_day, cluster = ~ResponseId+Article_day, data = Robust_lower_half_df_low_diglit)
#Produce confidence intervals with clustered standard errors:
CI_2_3 = confint(fit_2_3)

New_matr <- matrix(c(round(fit_2_1$coefficients[1]/sd(Robust_lower_half_df_low_diglit$Seven_Ordinal),4),round(CI_2_1[1,1]/sd(Robust_lower_half_df_low_diglit$Seven_Ordinal),4),round(CI_2_1[1,2]/sd(Robust_lower_half_df_low_diglit$Seven_Ordinal),4),'Ordinal (7)','Top 50% News Quality',
                     round(fit_2_2$coefficients[1]/sd(Robust_lower_half_df_low_diglit$Four_Ordinal),4),round(CI_2_2[1,1]/sd(Robust_lower_half_df_low_diglit$Four_Ordinal),4),round(CI_2_2[1,2]/sd(Robust_lower_half_df_low_diglit$Four_Ordinal),4),'Ordinal (4)','Top 50% News Quality',
                     round(fit_2_3$coefficients[1]/sd(Robust_lower_half_df_low_diglit$True_Dummy),4),round(CI_2_3[1,1]/sd(Robust_lower_half_df_low_diglit$True_Dummy),4),round(CI_2_3[1,2]/sd(Robust_lower_half_df_low_diglit$True_Dummy),4),'True (Dummy)','Top 50% News Quality'),ncol=5,byrow=T)

Fig_4a_Mat <- rbind(Fig_4a_Mat,New_matr)

#Use dataframe to create plot:
Fig_4a_Mat <- as.data.frame(Fig_4a_Mat)
Fig_4a_Mat <- na.omit(Fig_4a_Mat)
colnames(Fig_4a_Mat) <- c('Coef','Low_Conf','Upp_Conf','Measure','Type_News')
Fig_4a_Mat <- na.omit(Fig_4a_Mat)

Fig_4a_Mat$x<- c(1.4,1.5,1.6,0.8,0.9,1.0)
Fig_4a_Mat$Coef <- as.character(Fig_4a_Mat$Coef)
Fig_4a_Mat$Coef <- as.numeric(Fig_4a_Mat$Coef)
Fig_4a_Mat$Upp_Conf <- as.character(Fig_4a_Mat$Upp_Conf)
Fig_4a_Mat$Upp_Conf <- as.numeric(Fig_4a_Mat$Upp_Conf)
Fig_4a_Mat$Low_Conf <- as.character(Fig_4a_Mat$Low_Conf)
Fig_4a_Mat$Low_Conf <- as.numeric(Fig_4a_Mat$Low_Conf)
Fig_4a_Mat$Type_News <- factor(Fig_4a_Mat$Type_News,levels=c('Bottom 50% News Quality',
                                                             'Top 50% News Quality'))


#Produce Plot:
ggplot(data = Fig_4a_Mat, aes(x = x, y = Coef)) +
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
  ylim(-0.6,0.7) +
  scale_x_continuous("News Quality Returned by Search Engines \n",breaks=c(0.9,1.5),labels=c('Bottom 50%\nNews Quality',
                                                                                             'Top 50%\nNews Quality'),limits=c(0.5,2.0)) +
  coord_flip()

#Save Figure:
ggsave('./Figures/Quantiles_Low_DL_FULL_ROBUST.png',height=8,width=8)


################################################################################################################

############################### Figure 4c: Quantiles_Congruent_FULL_ROBUST.png ########################################

################################################################################################################

#filter responses
upper_half_df_congruent$Article_day <- gsub('Article_day','',upper_half_df_congruent$Article_day)
Robust_upper_half_df_congruent <- upper_half_df_congruent %>% filter(Article_day %in% Robust_Articles)

#Run Model
fit_1_1 = feols(Seven_Ordinal ~ Treatment + Age + Gender + Education_Score + Income_Score|Article_day, cluster = ~ResponseId+Article_day, data = Robust_upper_half_df_congruent)
#Produce confidence intervals with clustered standard errors:
CI_1_1 <- confint(fit_1_1)

#Run model:
fit_1_2 = feols(Four_Ordinal ~ Treatment + Age + Gender + Education_Score + Income_Score|Article_day, cluster = ~ResponseId+Article_day, data = Robust_upper_half_df_congruent)
#Produce confidence intervals with clustered standard errors:
CI_1_2 = confint(fit_1_2)

#Run model:
fit_1_3 = feols(True_Dummy ~ Treatment + Age + Gender + Education_Score + Income_Score|Article_day, cluster = ~ResponseId+Article_day, data = Robust_upper_half_df_congruent)
#Produce confidence intervals with clustered standard errors:
CI_1_3 = confint(fit_1_3)

Fig_4c_Mat <- matrix(ncol=5)

New_matr <- matrix(c(round(fit_1_1$coefficients[1]/sd(Robust_upper_half_df_congruent$Seven_Ordinal),4),round(CI_1_1[1,1]/sd(Robust_upper_half_df_congruent$Seven_Ordinal),4),round(CI_1_1[1,2]/sd(Robust_upper_half_df_congruent$Seven_Ordinal),4),'Ordinal (7)','Top 50% News Quality',
                     round(fit_1_2$coefficients[1]/sd(Robust_upper_half_df_congruent$Four_Ordinal),4),round(CI_1_2[1,1]/sd(Robust_upper_half_df_congruent$Four_Ordinal),4),round(CI_1_2[1,2]/sd(Robust_upper_half_df_congruent$Four_Ordinal),4),'Ordinal (4)','Top 50% News Quality',
                     round(fit_1_3$coefficients[1]/sd(Robust_upper_half_df_congruent$True_Dummy),4),round(CI_1_3[1,1]/sd(Robust_upper_half_df_congruent$True_Dummy),4),round(CI_1_3[1,2]/sd(Robust_upper_half_df_congruent$True_Dummy),4),'True (Dummy)','Top 50% News Quality'),ncol=5,byrow=T)

Fig_4c_Mat <- rbind(Fig_4c_Mat,New_matr)

#filter responses
lower_half_df_congruent$Article_day <- gsub('Article_day','',lower_half_df_congruent$Article_day)
Robust_lower_half_df_congruent <- lower_half_df_congruent %>% filter(Article_day %in% Robust_Articles)

#Run Model
fit_2_1 = feols(Seven_Ordinal ~ Treatment + Age + Gender + Education_Score + Income_Score|Article_day, cluster = ~ResponseId+Article_day, data = Robust_lower_half_df_congruent)
#Produce confidence intervals with clustered standard errors:
CI_2_1 <- confint(fit_2_1)

#Run model:
fit_2_2 = feols(Four_Ordinal ~ Treatment + Age + Gender + Education_Score + Income_Score|Article_day, cluster = ~ResponseId+Article_day, data = Robust_lower_half_df_congruent)
#Produce confidence intervals with clustered standard errors:
CI_2_2 = confint(fit_2_2)

#Run model:
fit_2_3 = feols(True_Dummy ~ Treatment + Age + Gender + Education_Score + Income_Score|Article_day, cluster = ~ResponseId+Article_day, data = Robust_lower_half_df_congruent)
#Produce confidence intervals with clustered standard errors:
CI_2_3 = confint(fit_2_3)

New_matr <- matrix(c(round(fit_2_1$coefficients[1]/sd(Robust_lower_half_df_congruent$Seven_Ordinal),4),round(CI_2_1[1,1]/sd(Robust_lower_half_df_congruent$Seven_Ordinal),4),round(CI_2_1[1,2]/sd(Robust_lower_half_df_congruent$Seven_Ordinal),4),'Ordinal (7)','Top 50% News Quality',
                     round(fit_2_2$coefficients[1]/sd(Robust_lower_half_df_congruent$Four_Ordinal),4),round(CI_2_2[1,1]/sd(Robust_lower_half_df_congruent$Four_Ordinal),4),round(CI_2_2[1,2]/sd(Robust_lower_half_df_congruent$Four_Ordinal),4),'Ordinal (4)','Top 50% News Quality',
                     round(fit_2_3$coefficients[1]/sd(Robust_lower_half_df_congruent$True_Dummy),4),round(CI_2_3[1,1]/sd(Robust_lower_half_df_congruent$True_Dummy),4),round(CI_2_3[1,2]/sd(Robust_lower_half_df_congruent$True_Dummy),4),'True (Dummy)','Top 50% News Quality'),ncol=5,byrow=T)

Fig_4c_Mat <- rbind(Fig_4c_Mat,New_matr)

#Use dataframe to create plot:
Fig_4c_Mat <- as.data.frame(Fig_4c_Mat)
Fig_4c_Mat <- na.omit(Fig_4c_Mat)
colnames(Fig_4c_Mat) <- c('Coef','Low_Conf','Upp_Conf','Measure','Type_News')
Fig_4c_Mat <- na.omit(Fig_4c_Mat)

Fig_4c_Mat$x<- c(1.4,1.5,1.6,0.8,0.9,1.0)
Fig_4c_Mat$Coef <- as.character(Fig_4c_Mat$Coef)
Fig_4c_Mat$Coef <- as.numeric(Fig_4c_Mat$Coef)
Fig_4c_Mat$Upp_Conf <- as.character(Fig_4c_Mat$Upp_Conf)
Fig_4c_Mat$Upp_Conf <- as.numeric(Fig_4c_Mat$Upp_Conf)
Fig_4c_Mat$Low_Conf <- as.character(Fig_4c_Mat$Low_Conf)
Fig_4c_Mat$Low_Conf <- as.numeric(Fig_4c_Mat$Low_Conf)
Fig_4c_Mat$Type_News <- factor(Fig_4c_Mat$Type_News,levels=c('Some Very\nUnreliable News',
                                                             'Only Very\nReliable News'))


#Create plot:
ggplot(data = Fig_4c_Mat, aes(x = x, y = Coef)) +
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
  scale_x_continuous("News Quality Returned by Search Engines \n",breaks=c(0.9,1.5),labels=c('Bottom 50%\nNews Quality',
                                                                                             'Top 50%\nNews Quality'),limits=c(0.5,2.0)) +
  coord_flip()

#Save figure:
ggsave('./Figures/Quantiles_Congruent_FULL_ROBUST.png',height=8,width=8)

################################################################################################################

################################ Figure 4d: Quantiles_Incong_FULL_ROBUST.png ##########################################

################################################################################################################

#filter responses
upper_half_df_incongruent$Article_day <- gsub('Article_day','',upper_half_df_incongruent$Article_day)
Robust_upper_half_df_incongruent <- upper_half_df_incongruent %>% filter(Article_day %in% Robust_Articles)


#Run Model
fit_1_1 = feols(Seven_Ordinal ~ Treatment + Age + Gender + Education_Score + Income_Score|Article_day, cluster = ~ResponseId+Article_day, data = Robust_upper_half_df_incongruent)
#Produce confidence intervals with clustered standard errors:
CI_1_1 <- confint(fit_1_1)

#Run model:
fit_1_2 = feols(Four_Ordinal ~ Treatment + Age + Gender + Education_Score + Income_Score|Article_day, cluster = ~ResponseId+Article_day, data = Robust_upper_half_df_incongruent)
#Produce confidence intervals with clustered standard errors:
CI_1_2 = confint(fit_1_2)

#Run model:
fit_1_3 = feols(True_Dummy ~ Treatment + Age + Gender + Education_Score + Income_Score|Article_day, cluster = ~ResponseId+Article_day, data = Robust_upper_half_df_incongruent)
#Produce confidence intervals with clustered standard errors:
CI_1_3 = confint(fit_1_3)

Fig_4d_Mat <- matrix(ncol=5)

New_matr <- matrix(c(round(fit_1_1$coefficients[1]/sd(Robust_upper_half_df_incongruent$Seven_Ordinal),4),round(CI_1_1[1,1]/sd(Robust_upper_half_df_incongruent$Seven_Ordinal),4),round(CI_1_1[1,2]/sd(Robust_upper_half_df_incongruent$Seven_Ordinal),4),'Ordinal (7)','Top 50% News Quality',
                     round(fit_1_2$coefficients[1]/sd(Robust_upper_half_df_incongruent$Four_Ordinal),4),round(CI_1_2[1,1]/sd(Robust_upper_half_df_incongruent$Four_Ordinal),4),round(CI_1_2[1,2]/sd(Robust_upper_half_df_incongruent$Four_Ordinal),4),'Ordinal (4)','Top 50% News Quality',
                     round(fit_1_3$coefficients[1]/sd(Robust_upper_half_df_incongruent$True_Dummy),4),round(CI_1_3[1,1]/sd(Robust_upper_half_df_incongruent$True_Dummy),4),round(CI_1_3[1,2]/sd(Robust_upper_half_df_incongruent$True_Dummy),4),'True (Dummy)','Top 50% News Quality'),ncol=5,byrow=T)

Fig_4d_Mat <- rbind(Fig_4d_Mat,New_matr)


#filter responses
lower_half_df_incongruent$Article_day <- gsub('Article_day','',lower_half_df_incongruent$Article_day)
Robust_lower_half_df_incongruent <- lower_half_df_incongruent %>% filter(Article_day %in% Robust_Articles)

#Run Model
fit_2_1 = feols(Seven_Ordinal ~ Treatment + Age + Gender + Education_Score + Income_Score|Article_day, cluster = ~ResponseId+Article_day, data = Robust_lower_half_df_incongruent)
#Produce confidence intervals with clustered standard errors:
CI_2_1 <- confint(fit_2_1)

#Run model:
fit_2_2 = feols(Four_Ordinal ~ Treatment + Age + Gender + Education_Score + Income_Score|Article_day, cluster = ~ResponseId+Article_day, data = Robust_lower_half_df_incongruent)
#Produce confidence intervals with clustered standard errors:
CI_2_2 = confint(fit_2_2)

#Run model:
fit_2_3 = feols(True_Dummy ~ Treatment + Age + Gender + Education_Score + Income_Score|Article_day, cluster = ~ResponseId+Article_day, data = Robust_lower_half_df_incongruent)
#Produce confidence intervals with clustered standard errors:
CI_2_3 = confint(fit_2_3)

New_matr <- matrix(c(round(fit_2_1$coefficients[1]/sd(Robust_lower_half_df_incongruent$Seven_Ordinal),4),round(CI_2_1[1,1]/sd(Robust_lower_half_df_incongruent$Seven_Ordinal),4),round(CI_2_1[1,2]/sd(Robust_lower_half_df_incongruent$Seven_Ordinal),4),'Ordinal (7)','Top 50% News Quality',
                     round(fit_2_2$coefficients[1]/sd(Robust_lower_half_df_incongruent$Four_Ordinal),4),round(CI_2_2[1,1]/sd(Robust_lower_half_df_incongruent$Four_Ordinal),4),round(CI_2_2[1,2]/sd(Robust_lower_half_df_incongruent$Four_Ordinal),4),'Ordinal (4)','Top 50% News Quality',
                     round(fit_2_3$coefficients[1]/sd(Robust_lower_half_df_incongruent$True_Dummy),4),round(CI_2_3[1,1]/sd(Robust_lower_half_df_incongruent$True_Dummy),4),round(CI_2_3[1,2]/sd(Robust_lower_half_df_incongruent$True_Dummy),4),'True (Dummy)','Top 50% News Quality'),ncol=5,byrow=T)

Fig_4d_Mat <- rbind(Fig_4d_Mat,New_matr)


#Create data for plot:
Fig_4d_Mat <- as.data.frame(Fig_4d_Mat)
Fig_4d_Mat <- na.omit(Fig_4d_Mat)
colnames(Fig_4d_Mat) <- c('Coef','Low_Conf','Upp_Conf','Measure','Type_News')
Fig_4d_Mat <- na.omit(Fig_4d_Mat)
Fig_4d_Mat$x<-c(1.4,1.5,1.6,0.8,0.9,1.0)
Fig_4d_Mat$Coef <- as.character(Fig_4d_Mat$Coef)
Fig_4d_Mat$Coef <- as.numeric(Fig_4d_Mat$Coef)
Fig_4d_Mat$Upp_Conf <- as.character(Fig_4d_Mat$Upp_Conf)
Fig_4d_Mat$Upp_Conf <- as.numeric(Fig_4d_Mat$Upp_Conf)
Fig_4d_Mat$Low_Conf <- as.character(Fig_4d_Mat$Low_Conf)
Fig_4d_Mat$Low_Conf <- as.numeric(Fig_4d_Mat$Low_Conf)

#Produce Plot:
ggplot(data = Fig_4d_Mat, aes(x = x, y = Coef)) +
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
  ylim(-0.6,0.7) +
  scale_x_continuous("News Quality Returned by Search Engines \n",breaks=c(0.9,1.5),labels=c('Bottom 50%\nNews Quality',
                                                                                             'Top 50%\nNews Quality'),limits=c(0.5,2.0)) +
  coord_flip()

#Save figure:
ggsave('./Figures/Quantiles_Incong_FULL_ROBUST.png',height=8,width=8)



#############################################   Predicting categorical response with likert score    ###########################################################

#Study 1:
#Filter control and treatment data:
Study_1_No_Search <- Study_1_df %>% filter(Treat_Search == 0)
Study_1_Search <- Study_1_df %>% filter(Treat_Search == 1)

#Run OLS Model with clustered standard errors:
Pred_fit_1_1 = feols(Likert_Evaluation ~ Susc_FN, cluster = ~ResponseId, data = Study_1_No_Search)
#Produce confidence intervals with clustered standard errors:
Pred_CI_1_1 <- confint(Pred_fit_1_1)

#Run OLS Model with clustered standard errors:
Pred_fit_1_2 = feols(Likert_Evaluation ~ Susc_FN, cluster = ~ResponseId, data = Study_1_Search)
#Produce confidence intervals with clustered standard errors:
Pred_CI_1_2 <- confint(Pred_fit_1_2)

#Study 2:
#Filter control and treatment data:
Study_2_No_Search <- Study_2_df %>% filter(Treat_Search == 0)
Study_2_Search <- Study_2_df %>% filter(Treat_Search == 1)

#Run OLS Model with clustered standard errors:
Pred_fit_2_1 = feols(Likert_Evaluation ~ Susc_FN, cluster = ~ResponseId, data = Study_2_No_Search)
#Produce confidence intervals with clustered standard errors:
Pred_CI_2_1 <- confint(Pred_fit_2_1)

#Run OLS Model with clustered standard errors:
Pred_fit_2_2 = feols(Likert_Evaluation ~ Susc_FN, cluster = ~ResponseId, data = Study_2_Search)
#Produce confidence intervals with clustered standard errors:
Pred_CI_2_2 <- confint(Pred_fit_2_2)

#Study 3:
#Filter control and treatment data:
Study_3_No_Search <- Study_3_df %>% filter(Treatment == 0)
Study_3_Search <- Study_3_df %>% filter(Treatment == 1)

#Run OLS Model with clustered standard errors:
Pred_fit_3_1 = feols(Likert_Evaluation ~ Susc_FN, cluster = ~ResponseId, data = Study_3_No_Search)
#Produce confidence intervals with clustered standard errors:
Pred_CI_3_1 <- confint(Pred_fit_3_1)

#Run OLS Model with clustered standard errors:
Pred_fit_3_2 = feols(Likert_Evaluation ~ Susc_FN, cluster = ~ResponseId, data = Study_3_Search)
#Produce confidence intervals with clustered standard errors:
Pred_CI_3_2 <- confint(Pred_fit_3_2)

#Study 4:
#Filter control and treatment data:
Study_4_No_Search <- Study_4_df %>% filter(Treat_Search == 0)
Study_4_Search <- Study_4_df %>% filter(Treat_Search == 1)

#Run OLS Model with clustered standard errors:
Pred_fit_4_1 = feols(Likert_Evaluation ~ Susc_FN, cluster = ~ResponseId, data = Study_4_No_Search)
#Produce confidence intervals with clustered standard errors:
Pred_CI_4_1 <- confint(Pred_fit_4_1)

#Run OLS Model with clustered standard errors:
Pred_fit_4_2 = feols(Likert_Evaluation ~ Susc_FN, cluster = ~ResponseId, data = Study_4_Search)
#Produce confidence intervals with clustered standard errors:
Pred_CI_4_2 <- confint(Pred_fit_4_2)

#Study 5:
Study_5_No_Search <- Study_5_df %>% filter(Treatment == 0)
Study_5_Search<- Study_5_df %>% filter(Treatment == 1)

#Run OLS Model with clustered standard errors:
Pred_fit_5_1 = feols(Likert_Evaluation ~ Susc_FN, cluster = ~ResponseId, data = Study_5_No_Search)
#Produce confidence intervals with clustered standard errors:
Pred_CI_5_1 <- confint(Pred_fit_5_1)

#Run OLS Model with clustered standard errors:
Pred_fit_5_2 = feols(Likert_Evaluation ~ Susc_FN, cluster = ~ResponseId, data = Study_5_Search)
#Produce confidence intervals with clustered standard errors:
Pred_CI_5_2 <- confint(Pred_fit_5_2)

Coef_names <- c('Study 1',
                'Study 1',
                'Study 2',
                'Study 2',
                'Study 3',
                'Study 3',
                'Study 4',
                'Study 4',
                'Study 5',
                'Study 5')

Group <- c('Pre-Treatment',
           'Post-Treatment',
           'Pre-Treatment',
           'Post-Treatment',
           'Pre-Treatment',
           'Post-Treatment',
           'Pre-Treatment',
           'Post-Treatment',
           'Pre-Treatment',
           'Post-Treatment')

Coefficients <- c(Pred_fit_1_1$coefficients[2],
                  Pred_fit_1_2$coefficients[2],
                  Pred_fit_2_1$coefficients[2],
                  Pred_fit_2_2$coefficients[2],
                  Pred_fit_3_1$coefficients[2],
                  Pred_fit_3_2$coefficients[2],
                  Pred_fit_4_1$coefficients[2],
                  Pred_fit_4_2$coefficients[2],
                  Pred_fit_5_1$coefficients[2],
                  Pred_fit_5_2$coefficients[2])


CI_Upper <- c(Pred_CI_1_1[2,2],
              Pred_CI_1_2[2,2],
              Pred_CI_2_1[2,2],
              Pred_CI_2_2[2,2],
              Pred_CI_3_1[2,2],
              Pred_CI_3_2[2,2],
              Pred_CI_4_1[2,2],
              Pred_CI_4_2[2,2],
              Pred_CI_5_1[2,2],
              Pred_CI_5_2[2,2])

CI_Lower <- c(Pred_CI_1_1[2,1],
              Pred_CI_1_2[2,1],
              Pred_CI_2_1[2,1],
              Pred_CI_2_2[2,1],
              Pred_CI_3_1[2,1],
              Pred_CI_3_2[2,1],
              Pred_CI_4_1[2,1],
              Pred_CI_4_2[2,1],
              Pred_CI_5_1[2,1],
              Pred_CI_5_2[2,1])          

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
d_matrix$x<-c(0.8,1.2,1.8,2.2,2.8,3.2,3.8,4.2,4.8,5.2)

ggplot(data = d_matrix, aes(x = x, y = Coefficients,color=Group)) +
  geom_hline(aes(yintercept = 0), color = "gray",
             linetype = 2, size = 1.2) +
  geom_point(size=4) +
  geom_linerange(aes(min = CI_Lower,
                     max = CI_Upper),
                 size=1.5) +
  scale_color_manual(values=c('red','blue','purple'), name = "Group") +
  ylab("\nEffect of Rating Misinformation as True on \n Seven-Point Ordinal Scale Veracity Rating") +
  theme_classic() +
  theme(axis.title.x = element_text(size=18),
        axis.text.x  = element_text(size=16),
        axis.title.y = element_text(size=16),
        axis.text.y  = element_text(size=22),
        plot.title = element_text(size = 16),
        legend.title = element_text(size=16),
        legend.text = element_text(size=14)) +
  ylim(0.0,4.0) +
  scale_x_continuous(" \n",breaks=c(1,2,3,4,5),labels=c('Study 5',
                                                        'Study 4',
                                                        'Study 3',
                                                        'Study 2',
                                                        'Study 1'),limits=c(0.5,5.5)) +
  coord_flip()

ggsave('./Figures/Pred_Categ_5_Studies.png',height=6,width=8)









#Models with pre-registered models - clustered standard errors around respondents:



#Run Model Testing Effect of Searching Online on Belief in Misinformation for Study 1:

#Pull in this data: Search Experiment 1: Study 1:
Study_1_df <- read.csv('./Data/Search_Exp_Misl_False.csv')

#Select variables of interest:
Study_1_df <- Study_1_df %>% select(Likert_Evaluation,Susc_FN,Treat_Search,Education_Score,Age,Gender,Income_Score,Ideology_Score,Dummy_Congruence,Familiar_Dummy,Article_day,ResponseId)
#Remove NA values:
Study_1_df = na.omit(Study_1_df)

#Create dataset with just control data:
Study_1_Control_df <- Study_1_df %>% filter(Treat_Search == 0)
Study_1_df <- Study_1_df %>% mutate(Gender = ifelse(Gender == 'Female',1,0))

#Run OLS Model with clustered standard errors:
lin_results_fit_1_1 = feols(Susc_FN ~ Treat_Search + Age +  + Education_Score +  Gender + Income_Score + Ideology_Score, cluster = ~ResponseId, data = Study_1_df)
#Produce confidence intervals with clustered standard errors:
CI_1_1 <- confint(lin_results_fit_1_1)

#Run OLS Model with clustered standard errors:
lin_results_fit_1_2 = feols(Likert_Evaluation ~ Treat_Search + Age + Education_Score +  Gender + Income_Score + Ideology_Score, cluster = ~ResponseId, data = Study_1_df)
#Produce confidence intervals with clustered standard errors:
CI_1_2 <- confint(lin_results_fit_1_2)

#Run Model Testing Effect of Searching Online on Belief in Misinformation for Study 2:
#Pull in this data:
Study_2_df <- read.csv('./Data/Data_Bef_Aft_Misl_False.csv')

Study_2_Ideo <- read.csv('./Data/Study_2_Respondent_Ideo.csv')

Study_2_df$ResponseId <- as.character(Study_2_df$ResponseId)
Study_2_Ideo$ResponseId <- as.character(Study_2_Ideo$ResponseId)

Study_2_df <- merge(Study_2_df,Study_2_Ideo,by='ResponseId')


#Select variables of interest:
Study_2_df <- Study_2_df %>% select(Likert_Evaluation,Susc_FN,Treat_Search,Education_Score,Age,Gender,Income_Score,Ideology_Score,Dummy_Congruence,Familiar_Dummy,Article_day,ResponseId)

Study_2_df <- na.omit(Study_2_df)

#Create dataset with just control data:
Study_2_Control_df <- Study_2_df %>% filter(Treat_Search == 0)
Study_2_df <- Study_2_df %>% mutate(Gender = ifelse(Gender == 'Female',1,0))

#Run OLS Model with clustered standard errors:
lin_results_fit_2_1 = feols(Susc_FN ~ Treat_Search + Age + Education_Score +  Gender + Income_Score + Ideology_Score, cluster = ~ResponseId,  data = Study_2_df)
#Produce confidence intervals with clustered standard errors:
CI_2_1 <- confint(lin_results_fit_2_1)

#Run OLS Model with clustered standard errors:
lin_results_fit_2_2 = feols(Likert_Evaluation ~ Treat_Search + Age + Education_Score +  Gender + Income_Score + Ideology_Score, cluster = ~ResponseId,  data = Study_2_df)
#Produce confidence intervals with clustered standard errors:
CI_2_2 <- confint(lin_results_fit_2_2)

#Run Model Testing Effect of Searching Online on Belief in Misinformation for Study 3:
#Load Data:
Latency_Data <- read.csv('./Data/Latency_FC_Data.csv')
Latency_Survey <- read.csv('./Data/Latency_Control_Survey.csv')

#Create Article_day data for purpose of merging
Latency_Survey <- Latency_Survey %>% mutate(Article_day = paste0(day,sep='_',Article))

#Select data needed:
Latency_Survey <- Latency_Survey %>% select(Evaluation,Likert_Evaluation,True_Likert_After_Info,Evaluation_After_Info,Age,Dig_Lit_Avg,Income_Score,CRT_Score,Ideology_Score,Familiar_Story,Education_Score,Duration,Article_day,Ideology_Score,Gender,ResponseId)

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
After_Evaluation <- Latency_Survey %>% select(True_Dummy_After,True_Likert_After_Info,Age,Dig_Lit_Avg,Income_Score,CRT_Score,Familiar_Story,Ideology_Score,Education_Score,Duration,Article_day,Ideology_Score,Gender,ResponseId)
colnames(After_Evaluation)[1] <- 'True_Dummy'
colnames(After_Evaluation)[2] <- 'Likert_Evaluation'
Before_Evaluation <- Latency_Survey %>% select(True_Dummy,Likert_Evaluation,Age,Dig_Lit_Avg,Income_Score,CRT_Score,Familiar_Story,Ideology_Score,Education_Score,Duration,Article_day,Ideology_Score,Gender,ResponseId)

#Create Treatment variables:
After_Evaluation$Treatment <- 1
Before_Evaluation$Treatment <- 0

#merge them together
Study_3_df <- rbind(Before_Evaluation,After_Evaluation)

#Create Ideological Congruence data:
Study_3_df <- Study_3_df %>% mutate(Dummy_Ideology = ifelse(Ideology_Score > 0,'Conservative','Moderate'))
Study_3_df <- Study_3_df %>% mutate(Dummy_Ideology = ifelse(Ideology_Score < 0,'Liberal',Dummy_Ideology))
Study_3_df <- merge(Study_3_df,Article_data,all=T)
Study_3_df$Article_Lean <- ifelse(Study_3_df$Article_Lean == 'None','Neutral',Study_3_df$Article_Lean)
Study_3_df <- Study_3_df %>% mutate(Dummy_Congruence = ifelse(Dummy_Ideology == Article_Lean,1,0))
Study_3_df$Susc_FN <- Study_3_df$True_Dummy
Study_3_df$Treat_Search <- Study_3_df$Treatment

Study_3_df <- na.omit(Study_3_df)

#Create Control dataframe
Study_3_Control_df <- Study_3_df %>% filter(Treatment == 0)

#Run linear regression and produce coefficient values:
lin_results_fit_3_1 = feols(True_Dummy ~ Treatment + Age + Education_Score +  Gender + Income_Score + Ideology_Score, cluster = ~ResponseId, data = Study_3_df)
#Produce confidence intervals using clustered standard errors:
CI_3_1 <- confint(lin_results_fit_3_1)

#Run linear regression and produce coefficient values:
lin_results_fit_3_2 = feols(Likert_Evaluation ~ Treatment + Age + Education_Score +  Gender + Income_Score + Ideology_Score, cluster = ~ResponseId, data = Study_3_df)
#Produce confidence intervals using clustered standard errors:
CI_3_2 <- confint(lin_results_fit_3_2)


#Run Model Testing Effect of Searching Online on Belief in Misinformation for Study 4:
#Pull in data
Study_4_df <- read.csv('./Data/Experiment_2_Study_2_Misl_False.csv')
Study_4_Ideo <- read.csv('./Data/Study_4_Respondent_Ideo.csv')

Study_4_df$ResponseId <- as.character(Study_4_df$ResponseId)
Study_4_Ideo$ResponseId <- as.character(Study_4_Ideo$ResponseId)

Study_4_df <- merge(Study_4_df,Study_4_Ideo,by='ResponseId')


#Select variables of interest:
Study_4_df <- Study_4_df %>% select(Likert_Evaluation,Susc_FN,Treat_Search,Education_Score,Age,Gender,Income_Score,Ideology_Score,Dummy_Congruence,Familiar_Dummy,Article_day,ResponseId)
Study_4_df <- na.omit(Study_4_df)

#Create dataframe with just control data
Study_4_Control_df <- Study_4_df %>% filter(Treat_Search == 0)
Study_4_df <- Study_4_df %>% mutate(Gender = ifelse(Gender == 'Female',1,0))

#Run OLS Model with clustered standard errors:
lin_results_fit_4_1 = feols(Susc_FN ~ Treat_Search + Age + Education_Score +  Gender + Income_Score + Ideology_Score, cluster = ~ResponseId,  data = Study_4_df)
#Produce confidence intervals with clustered standard errors:
CI_4_1 <- confint(lin_results_fit_4_1)

#Run OLS Model with clustered standard errors:
lin_results_fit_4_2 = feols(Likert_Evaluation ~ Treat_Search + Age + Education_Score +  Gender + Income_Score + Ideology_Score, cluster = ~ResponseId,  data = Study_4_df)
#Produce confidence intervals with clustered standard errors:
CI_4_2 <- confint(lin_results_fit_4_2)

#################################################### Categorical (Rate as True) ####################################################

#Create vector with Study names:
Coef_names <- rev(c('Study 1',
                    'Study 2',
                    'Study 3',
                    'Study 4'))

#Create vector with coefficients:
Coefficients <- rev(c(lin_results_fit_1_1$coefficients[2],
                      lin_results_fit_2_1$coefficients[2],
                      lin_results_fit_3_1$coefficients[2],
                      lin_results_fit_4_1$coefficients[2]))

#Create vector with upper confidence intervals:
CI_Upper <- rev(c(CI_1_1[2,2],
                  CI_2_1[2,2],
                  CI_3_1[2,2],
                  CI_4_1[2,2]))

#Create vector with lower confidence intervals:
CI_Lower <- rev(c(CI_1_1[2,1],
                  CI_2_1[2,1],
                  CI_3_1[2,1],
                  CI_4_1[2,1]))         

#Put together matrix with data for plot:
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

#Set points on Y-Axis:
d_matrix$x<-c(1,2,3,4)

#Produce plot:
ggplot(data = d_matrix, aes(x = x, y = Coefficients)) +
  geom_hline(aes(yintercept = 0), color = "gray",
             linetype = 2, size = 1.2) +
  geom_point(size=4) +
  geom_linerange(aes(min = CI_Lower,
                     max = CI_Upper),
                 size=1.5) +
  scale_color_manual(values=c('red','blue','purple'), name = "Measure") +
  ylab("\nEffect of Searching Online on Probability \nof Rating Misinformation as True") +
  theme_classic() +
  theme(axis.title.x = element_text(size=18),
        axis.text.x  = element_text(size=16),
        axis.title.y = element_text(size=16),
        axis.text.y  = element_text(size=22),
        plot.title = element_text(size = 16),
        legend.title = element_text(size=16),
        legend.text = element_text(size=14)) +
  ylim(-0.05,0.2) +
  scale_x_continuous(" \n",breaks=c(1,2,3,4),labels=c('Study 4',
                                                      'Study 3',
                                                      'Study 2',
                                                      'Study 1'),limits=c(0.5,4.5)) +
  coord_flip()

#Save figure:
ggsave('./Figures/All_4_Studies_Categorical_Preregistration.png',height=6,width=8)




#################################################### Ordinal Scale (Seven) ####################################################

#Create vector with Study names:
Coef_names <- rev(c('Study 1',
                    'Study 2',
                    'Study 3',
                    'Study 4'))

#Create vector with coefficients:
Coefficients <- rev(c(lin_results_fit_1_2$coefficients[2],
                      lin_results_fit_2_2$coefficients[2],
                      lin_results_fit_3_2$coefficients[2],
                      lin_results_fit_4_2$coefficients[2]))

#Create vector with upper confidence intervals:
CI_Upper <- rev(c(CI_1_2[2,2],
                  CI_2_2[2,2],
                  CI_3_2[2,2],
                  CI_4_2[2,2]))

#Create vector with lower confidence intervals:
CI_Lower <- rev(c(CI_1_2[2,1],
                  CI_2_2[2,1],
                  CI_3_2[2,1],
                  CI_4_2[2,1]))         

#Put together matrix with data for plot:
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

#Set points on Y-Axis:
d_matrix$x<-c(1,2,3,4)

#Produce plot:
ggplot(data = d_matrix, aes(x = x, y = Coefficients)) +
  geom_hline(aes(yintercept = 0), color = "gray",
             linetype = 2, size = 1.2) +
  geom_point(size=4) +
  geom_linerange(aes(min = CI_Lower,
                     max = CI_Upper),
                 size=1.5) +
  scale_color_manual(values=c('red','blue','purple'), name = "Measure") +
  ylab("\nEffect of Searching Online on the \nPerceived Veracity of Misinformation (7-point scale) ") +
  theme_classic() +
  theme(axis.title.x = element_text(size=18),
        axis.text.x  = element_text(size=16),
        axis.title.y = element_text(size=16),
        axis.text.y  = element_text(size=22),
        plot.title = element_text(size = 16),
        legend.title = element_text(size=16),
        legend.text = element_text(size=14)) +
  ylim(-0.10,0.8) +
  scale_x_continuous(" \n",breaks=c(1,2,3,4),labels=c('Study 4',
                                                      'Study 3',
                                                      'Study 2',
                                                      'Study 1'),limits=c(0.5,4.5)) +
  coord_flip()

#Save figure:
ggsave('./Figures/All_4_Studies_Ordinal_Preregistration.png',height=6,width=8)













