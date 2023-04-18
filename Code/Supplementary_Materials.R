
#Author: Kevin Aslett
#Code Title: Supplementary_Materials.R
#Paper Title: Do Your Own Research? Searching Online About Misinformation Increases Belief
#Purpose of code: Generate all figures that are located in the supplementary materials of the paper.

#FILES IN:
#Study 1 (False/Misleading Article Data): Study_1_df_FM.csv
#Study 2 (False/Misleading Article Data): Study_2_df_FM.csv
#Study 3 (False/Misleading Article Data): Study_3_df_FM.csv
#Study 4 (False/Misleading Article Data): Study_4_df_FM.csv
#Study 5 (False/Misleading Article Data): Study_5_df_FM.csv
#Study 5 (Web Tracking Data): Study_5_treat_data.csv
#Fact-Checker Ideology Data: FC_Ideo_Data.csv
#Study 5 (Only Control Data): Control_Data_Study_5.csv
# Study 5 Web Tracking Data (Only Control Data): output_Control_Survey_2.csv
#Search Term - Headline Coding: Headline_Coding_4.csv
#Study 1 (True Article Data): Study_1_df_T.csv
#Study 2 (True Article Data): Study_2_df_T.csv
#Study 3 (True Article Data): Study_3_df_T.csv
#Study 4 (True Article Data): Study_4_df_T.csv
#Study 5 (True Article Data): Study_5_df_T.csv
#Fact-checker data for articles in Study 1, 2, and 3: Fact_Checker_Data_Master.csv
#Fact-checker data for articles in Study 4: fcers_byarticle_all_sets_covid.csv
#Fact-checker data for articles in Study 5: FCers_Study_5.csv
#Study 5 (False/Misleading Article Data): Study_6_FM.csv
#Study 5 (True - Mainstream Article Data): Study_6_T_M.csv
#Study 5 (True - Low-Quality Article Data): Study_6_T_LQ.csv
#Study 5 (Web Tracking Data - without original link): Study_5_treat_data_wo_OG.csv
#Fact-checker dataset: FCer_details.csv
#Study 1 - Only Control - (False/Misleading Article Data): Study_1_Control_Only_Data_FM.csv
#Study 2 - Only Control - (False/Misleading Article Data): Study_2_Control_Only_Data_FM.csv
#Study 3 - Only Control - (False/Misleading Article Data): Study_3_Control_Only_Data_FM.csv
#Study 4 - Only Control - (False/Misleading Article Data): Study_4_Control_Only_Data_FM.csv
#Study 5 - Only Control - (False/Misleading Article Data): Study_5_Control_Only_Data_FM.csv
#Study 1 - Only Control - (True Mainstream Article Data): Study_1_Control_Only_Data_T_M.csv
#Study 2 - Only Control - (True Mainstream Article Data): Study_2_Control_Only_Data_T_M.csv
#Study 3 - Only Control - (True Mainstream Article Data): Study_3_Control_Only_Data_T_M.csv
#Study 4 - Only Control - (True Mainstream Article Data): Study_4_Control_Only_Data_T_M.csv
#Study 5 - Only Control - (True Mainstream Article Data): Study_5_Control_Only_Data_T_M.csv
#Study 1 - Only Control - (True Low-Quality Article Data): Study_1_Control_Only_Data_T_LQ.csv
#Study 2 - Only Control - (True Low-Quality Article Data): Study_2_Control_Only_Data_T_LQ.csv
#Study 3 - Only Control - (True Low-Quality Article Data): Study_3_Control_Only_Data_T_LQ.csv
#Study 4 - Only Control - (True Low-Quality Article Data): Study_4_Control_Only_Data_T_LQ.csv
#Study 5 - Only Control - (True Low-Quality Article Data): Study_5_Control_Only_Data_T_LQ.csv
#Search Engine results from Study 5: All_Search_Results_Combined.csv
#Study 5 Article Data: Study_5_Articles.csv
#Raw Study 1 Data (False/Misleading Articles): Search_Exp_Misl_False.csv
#Raw Study 2 Data (False/Misleading Articles): Data_Bef_Aft_Misl_False.csv
#Study 4 Ideology of Respondent Data: Study_4_Respondent_Ideo.csv

#FILES OUT:
#Figure 2: Pred_Categ_5_Studies.png
#Figure 3a: All_4_Studies_Ordinal_Robust.png
#Figure 3b: All_4_Studies_Categorical_Robust.png
#Figure 4a: Study_5_Bar_Graph_Google_Search_ROBUST.png
#Figure 4b: Study_5_1_ROBUST.png
#Figure 4c: Coefs_CIs_ROBUST.png
#Figure 4d: Coefs_CIs_2_ROBUST.png
#Figure 5a: Coefs_CIs_Predicting_Unrel_Dummy_ROBUST.png
#Figure 5b: Study_5_Bar_Graph_Google_Search_HL_ROBUST.png
#Figure 5c: Coefs_CIs_Predicting_Headline_Link_ROBUST.png
#Figure 6a: T_FM_Fig_1A_True_Dummy_ROBUST.png
#Figure 6b: Fig_1A_True_Dummy_ROBUST.png
#Figure 6c: Types_Fig_1A_True_Dummy_ROBUST.png
#Figure 7a: T_FM_Fig_1A_True_Ordinal.png
#Figure 7b: Fig_1A_True_Ordinal.png
#Figure 7c: Types_Fig_1A_True_Ordinal.png
#Figure 8a: All_4_Studies_Categorical_Preregistration.png
#Figure 8b: All_4_Studies_Ordinal_Preregistration.png
#Figure 9: Different_Treatment_True_Dummy_All.png
#Figure 10: Different_Treatment_Four_Ordinal_All.png
#Figure 11: Different_Treatment_Seven_Ordinal_All.png
#Figure 12a: Study_5_Bar_Graph_Google_Search_wo_OG.png
#Figure 12c: Coefs_CIs_wo_OG.png
#Figure 12d: Coefs_CIs_2_wo_OG.png
#Figure 13a: Coefs_CIs_Predicting_Unrel_Dummy_wo_OG.png
#Figure 13b: Study_5_Bar_Graph_Google_Search_HL_wo_OG.png
#Figure 13c: Coefs_CIs_Predicting_Headline_Link_wo_OG.png
#Figure 14a: Interaction_By_Study_True.png
#Figure 14b: Interaction_By_Study_Ordinal_7.png
#Figure 14c: Interaction_By_Study_True_Pooled.png
#Figure 14d: Interaction_By_Study_Ordinal_7_Pooled.png
#Figure 15: Fig_Searching_Online.png
#Figure 16a: Predicting_Search_Study_1.png
#Figure 16b: Predicting_Search_Study_2.png
#Figure 16c: Predicting_Search_Study_3.png
#Figure 16d: Predicting_Search_Study_4.png
#Figure 17: Search_Effect_wo_encouragement_True_Dummy.png
#Figure 18: Search_Effect_wo_encouragement_Ordinal_7.png
#Table 21: Study_1.txt
#Table 22: Study_2.txt
#Table 23: Study_3.txt
#Table 24: Study_4.txt
#Table 25: Study_5.txt
#Table 26: Figure_2c_1.txt
#Table 27: Figure_2c_2.txt
#Table 28: Figure_2d_1.txt
#Table 29: Figure_2d_2.txt
#Table 30: Figure_2d_3.txt
#Table 31: Figure_2d_4.txt
#Table 32: Figure_3_a.txt
#Table 33: Figure_3_c.txt
#Table 34: Figure_4_A_1.txt
#Table 35: Figure_4_A_2.txt
#Table 36: Figure_4_A_3.txt
#Table 37: Figure_4_A_4.txt
#Table 38: Figure_4_A_5.txt
#Table 39: Figure_4_A_6.txt
#Table 40: Figure_4_A_7.txt
#Table 41: Figure_4_A_8.txt
#Table 42: Figure_4_A_9.txt
#Table 43: Figure_4_A_10.txt
#Table 44: Figure_4b_1.txt
#Table 45: Figure_4b_2.txt
#Table 46: Figure_4b_3.txt
#Table 47: Figure_4b_4.txt
#Table 48: Figure_4b_5.txt
#Table 49: Figure_4b_6.txt
#Table 50: Figure_4b_7.txt
#Table 51: Figure_4b_8.txt
#Table 52: Figure_4b_9.txt
#Table 53: Figure_4b_10.txt
#Table 54: Figure_4b_11.txt
#Table 55: Figure_4b_12.txt
#Table 56: Figure_4b_13.txt
#Table 57: Figure_4b_14.txt
#Table 58: Figure_4b_15.txt
#Table 59: Figure_4c_1.txt
#Table 60: Figure_4c_2.txt
#Table 61: Figure_4c_3.txt
#Table 62: Figure_4c_4.txt
#Table 63: Figure_4c_5.txt
#Table 64: Figure_4c_6.txt

#Load in Libraries:
library(ggplot2)
library(dplyr)
library(xtable)
library(stats)
library(fixest)
library(irr)
library(lmtest)
library(Rmisc)
library(tm)
library(lsa)

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

#Study 1:
#Pull in Study 1 data:
Study_1_df <- read.csv('./Data/Study_1_df_FM.csv')

#Remove NA values:
Study_1_df = na.omit(Study_1_df)

#Run OLS Model with clustered standard errors:
lin_results_fit_1_1 = feols(Susc_FN ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Study_1_df)
#Produce confidence intervals with clustered standard errors:
CI_1_1 <- confint(lin_results_fit_1_1)

#Run OLS Model with clustered standard errors:
lin_results_fit_1_2 = feols(Likert_Evaluation ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Study_1_df)
#Produce confidence intervals with clustered standard errors:
CI_1_2 <- confint(lin_results_fit_1_2)

#Write Table:
etable(lin_results_fit_1_1,lin_results_fit_1_2, tex = TRUE,
       signifCode= c(`***` = 0.001, `**` = 0.01, `*` = 0.05),
       file='./Tables/Study_1.txt',
       replace=TRUE,
       title='Results from OLS Regression Results Presented in Figure 1a and 1b (Study 1)')

#Pull in Study 2 data:
Study_2_df <- read.csv('./Data/Study_2_df_FM.csv')

#Remove NA values:
Study_2_df = na.omit(Study_2_df)

#Run OLS Model with clustered standard errors:
lin_results_fit_2_1 = feols(Susc_FN ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Study_2_df)
#Produce confidence intervals with clustered standard errors:
CI_2_1 <- confint(lin_results_fit_2_1)

#Run OLS Model with clustered standard errors:
lin_results_fit_2_2 = feols(Likert_Evaluation ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Study_2_df)
#Produce confidence intervals with clustered standard errors:
CI_2_2 <- confint(lin_results_fit_2_2)

#Write Table:
etable(lin_results_fit_2_1,lin_results_fit_2_2, tex = TRUE,
       signifCode= c(`***` = 0.001, `**` = 0.01, `*` = 0.05),
       file='./Tables/Study_2.txt',
       replace=TRUE,
       title='Results from OLS Regression Results Presented in Figure 1a and 1b (Study 2)')


#Pull in Study 3 data:
Study_3_df <- read.csv('./Data/Study_3_df_FM.csv')

#Remove NA values:
Study_3_df = na.omit(Study_3_df)

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

#Write Table:
etable(lin_results_fit_3_1,lin_results_fit_3_2, tex = TRUE,
       signifCode= c(`***` = 0.001, `**` = 0.01, `*` = 0.05),
       file='./Tables/Study_3.txt',
       replace=TRUE,
       title='Results from OLS Regression Results Presented in Figure 1a and 1b (Study 3)')


#Pull in Study 4 data:
Study_4_df <- read.csv('./Data/Study_4_df_FM.csv')

#Remove NA values:
Study_4_df = na.omit(Study_4_df)

#Run OLS Model with clustered standard errors:
lin_results_fit_4_1 = feols(Susc_FN ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Study_4_df)
#Produce confidence intervals with clustered standard errors:
CI_4_1 <- confint(lin_results_fit_4_1)

#Run OLS Model with clustered standard errors:
lin_results_fit_4_2 = feols(Likert_Evaluation ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Study_4_df)
#Produce confidence intervals with clustered standard errors:
CI_4_2 <- confint(lin_results_fit_4_2)

#Write Table:
etable(lin_results_fit_4_1,lin_results_fit_4_2, tex = TRUE,
       signifCode= c(`***` = 0.001, `**` = 0.01, `*` = 0.05),
       file='./Tables/Study_4.txt',
       replace=TRUE,
       title='Results from OLS Regression Results Presented in Figure 1a and 1b (Study 4)')


####################################################################################

########################## Regression Table for Figure 2b ##########################

####################################################################################

#Pull in Study 5 data:
FM_Data_Study_5 <- read.csv('./Data/Study_5_df_FM.csv')

#Remove NA values:
FM_Data_Study_5 = na.omit(FM_Data_Study_5)

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


#Write Table:
etable(lin_results_fit_5_1,lin_results_fit_5_2,lin_results_fit_5_3, tex = TRUE,
       signifCode= c(`***` = 0.001, `**` = 0.01, `*` = 0.05),
       file='./Tables/Study_5.txt',
       replace=TRUE,
       title='Results from OLS Regression Results Presented in Figure 2b')


####################################################################################

########################## Regression Table for Figure 2c ##########################

####################################################################################

#Read in data:
Study_5_treat_data <- read.csv('./Data/Study_5_treat_data.csv')

#Create Treatment Data:

#Ideological Perspective of Articles:
FC_Ideo_Data <- read.csv('./Data/FC_Ideo_Data.csv')
FC_Ideo_Data$X <- NULL

#Pull in treatment data for Study 5:
T_Data <- Study_5_treat_data %>% filter(FC_Eval == 'FM')

#Filter only responses who only saw very reliable news sites in Google Search Results (85)
Treat_only_rel_data <- T_Data %>% filter(Total_Rel_85 > 0)

nrow(Treat_only_rel_data)/498

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

Treat_only_rel_data <- Treat_only_rel_data %>% select(Article_day,True_Dummy,Four_Ordinal,Seven_Ordinal,Age,Gender,ResponseId,Education_Score,Income_Score,Ideo_Congruence)
Treat_only_rel_data$ResponseId <- as.character(Treat_only_rel_data$ResponseId)
Control_Data$ResponseId <- as.character(Control_Data$ResponseId)
Treat_only_rel_data$Article_day <- as.character(Treat_only_rel_data$Article_day)

Treat_only_rel_data$Treatment <- 1


#Merge treatment and control articles:
Study_5_subset_1 <- rbind(Treat_only_rel_data,Control_Data)

Study_5_subset_1 <- Study_5_subset_1 %>% select(True_Dummy,Four_Ordinal,Seven_Ordinal,Treatment,Age,Ideo_Congruence,Education_Score,Gender,Income_Score,Article_day,ResponseId)

#Remove NAs
#Study_5_subset_1 <- na.omit(Study_5_subset_1)

#Run OLS Model with clustered standard errors:
fit_fig_2c_1 = feols(Seven_Ordinal ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=Study_5_subset_1)
CI_2_1 = confint(fit_fig_2c_1,se='twoway')

#Run OLS Model with clustered standard errors:
fit_fig_2c_2 = feols(Four_Ordinal ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=Study_5_subset_1)
CI_2_2 = confint(fit_fig_2c_2,se='twoway')

#Run OLS Model with clustered standard errors:
fit_fig_2c_3 = feols(True_Dummy ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=Study_5_subset_1)
CI_2_3 = confint(fit_fig_2c_3,se='twoway')


#Write Table:
etable(fit_fig_2c_3,fit_fig_2c_2,fit_fig_2c_1, tex = TRUE,
       signifCode= c(`***` = 0.001, `**` = 0.01, `*` = 0.05),
       file='./Tables/Figure_2c_1.txt',
       replace=TRUE,
       title='Results from OLS Regression Results Presented in Figure 2c (Only Very Reliable News Returned)',
       dict=c("Seven_Ordinal" = "Ordinal Scale (7-Point)",
              "Four_Ordinal" = "Ordinal Scale (4-Point)",
              "True_Dummy" = "Categorical Scale (True = 1)",
              "Treat_Search" = "Treatment",
              "Article_day" = "Article",
              "ResponseId" = "Respondent",
              "Income_Score" = "Income",
              "Education_Score" = "Education",
              "Ideo_Congruence" = "Ideological Congruence",
              "Dig_Lit_Score" = "Digital Literacy Score"))

#Filter only responses who only saw very reliable news sites in Google Search Results (85)
T_Data <- T_Data %>% mutate(Prop_60 = Total_Unrel_60/Total_Links)
Test_dataframe <- T_Data %>% select(True_Dummy,Four_Ordinal,Seven_Ordinal,Prop_60,Total_Unrel_60,avg_score)
Treatment_unrel_data <- T_Data %>% filter(Prop_60 >= 0.1) #125
Treatment_unrel_data <- Treatment_unrel_data %>% filter(FC_Eval == 'FM')

#Select Variables:
Treatment_unrel_data <- Treatment_unrel_data %>% select(Article_day,True_Dummy,Four_Ordinal,Seven_Ordinal,Age,Gender,ResponseId,Education_Score,Income_Score,Ideo_Congruence)
Treatment_unrel_data$ResponseId <- as.character(Treatment_unrel_data$ResponseId)
Treatment_unrel_data$Article_day <- as.character(Treatment_unrel_data$Article_day)
Treatment_unrel_data$Treatment <- 1
Treatment_unrel_data <- na.omit(Treatment_unrel_data)

real_T_Data <- na.omit(T_Data)
nrow(Treatment_unrel_data)/nrow(real_T_Data)

#Merge treatment and control articles:
Study_5_subset_2 <- rbind(Treatment_unrel_data,Control_Data)
#Remove NAs
Study_5_subset_2 <- na.omit(Study_5_subset_2)


#Run OLS Model with clustered standard errors:
fit_fig_2c_4 = feols(Seven_Ordinal ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=Study_5_subset_2)
CI_2_1 = confint(fit_fig_2c_4,se='twoway')

#Run OLS Model with clustered standard errors:
fit_fig_2c_5 = feols(Four_Ordinal ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=Study_5_subset_2)
CI_2_2 = confint(fit_fig_2c_5,se='twoway')

#Run OLS Model with clustered standard errors:
fit_fig_2c_6 = feols(True_Dummy ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=Study_5_subset_2)
CI_2_3 = confint(fit_fig_2c_6,se='twoway')


#Write Table:
etable(fit_fig_2c_6,fit_fig_2c_5,fit_fig_2c_4, tex = TRUE,
       signifCode= c(`***` = 0.001, `**` = 0.01, `*` = 0.05),
       file='./Tables/Figure_2c_2.txt',
       replace=TRUE,
       title='Results from OLS Regression Results Presented in Figure 2c (Some Unreliable News Returned)',
       dict=c("Seven_Ordinal" = "Ordinal Scale (7-Point)",
              "Four_Ordinal" = "Ordinal Scale (4-Point)",
              "True_Dummy" = "Categorical Scale (True = 1)",
              "Treat_Search" = "Treatment",
              "Article_day" = "Article",
              "ResponseId" = "Respondent",
              "Income_Score" = "Income",
              "Education_Score" = "Education",
              "Ideo_Congruence" = "Ideological Congruence",
              "Dig_Lit_Score" = "Digital Literacy Score"))



####################################################################################

########################## Regression Table for Figure 2d ##########################

####################################################################################

#Filter by quartile:
#Filter only responses who only saw very reliable news sites in Google Search Results (85)
lowest_quartile_T_data <- T_Data %>% filter(avg_score < quantile(T_Data$avg_score,na.rm=T)[2])

#Select Variables:
lowest_quartile_T_data <- lowest_quartile_T_data %>% select(Article_day,True_Dummy,Four_Ordinal,Seven_Ordinal,Age,Gender,ResponseId,Education_Score,Income_Score,Ideo_Congruence)
lowest_quartile_T_data$ResponseId <- as.character(lowest_quartile_T_data$ResponseId)
lowest_quartile_T_data$Article_day <- as.character(lowest_quartile_T_data$Article_day)
lowest_quartile_T_data$Treatment <- 1

#Merge treatment and control articles:
lowest_quartile_all_data <- rbind(lowest_quartile_T_data,Control_Data)

#Remove NAs
lowest_quartile_all_data <- na.omit(lowest_quartile_all_data)

#Run OLS Model with clustered standard errors:
lin_results_fit_subset_1_1 = feols(Seven_Ordinal ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=lowest_quartile_all_data)

#Run OLS Model with clustered standard errors:
lin_results_fit_subset_1_2 = feols(Four_Ordinal ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=lowest_quartile_all_data)

#Run OLS Model with clustered standard errors:
lin_results_fit_subset_1_3 = feols(True_Dummy ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=lowest_quartile_all_data)

etable(lin_results_fit_subset_1_3,lin_results_fit_subset_1_2,lin_results_fit_subset_1_1,
       tex = TRUE,
       signifCode= c(`***` = 0.001, `**` = 0.01, `*` = 0.05),
       file='./Tables/Figure_2d_1.txt',
       replace=TRUE,
       title='Results from OLS Regression Results Presented in Figure 2d (25-50 Percentage Quartile of News Quality)',
       dict=c("Seven_Ordinal" = "Ordinal Scale (7-Point)",
              "Four_Ordinal" = "Ordinal Scale (4-Point)",
              "True_Dummy" = "Categorical Scale (True = 1)",
              "Treat_Search" = "Treatment",
              "Article_day" = "Article",
              "ResponseId" = "Respondent",
              "Income_Score" = "Income",
              "Education_Score" = "Education",
              "Ideo_Congruence" = "Ideological Congruence",
              "Dig_Lit_Score" = "Digital Literacy Score"))


#Filter by quartile:
Second_lowest_quartile_T_data <- T_Data %>% filter(avg_score >= quantile(T_Data$avg_score,na.rm=T)[2] & avg_score < quantile(T_Data$avg_score,na.rm=T)[3])

#Select Variables:
Second_lowest_quartile_T_data <- Second_lowest_quartile_T_data %>% select(Article_day,True_Dummy,Four_Ordinal,Seven_Ordinal,Age,Gender,ResponseId,Education_Score,Income_Score,Ideo_Congruence)
Second_lowest_quartile_T_data$ResponseId <- as.character(Second_lowest_quartile_T_data$ResponseId)
Second_lowest_quartile_T_data$Article_day <- as.character(Second_lowest_quartile_T_data$Article_day)
Second_lowest_quartile_T_data$Treatment <- 1

#Merge treatment and control articles:
Second_lowest_quartile_all_data <- rbind(Second_lowest_quartile_T_data,Control_Data)
#Remove NAs
Second_lowest_quartile_all_data <- na.omit(Second_lowest_quartile_all_data)

#Run OLS Model with clustered standard errors:
lin_results_fit_subset_2_1 = feols(Seven_Ordinal ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=Second_lowest_quartile_all_data)

#Run OLS Model with clustered standard errors:
lin_results_fit_subset_2_2 = feols(Four_Ordinal ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=Second_lowest_quartile_all_data)

#Run OLS Model with clustered standard errors:
lin_results_fit_subset_2_3 = feols(True_Dummy ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=Second_lowest_quartile_all_data)

etable(lin_results_fit_subset_2_3,lin_results_fit_subset_2_2,lin_results_fit_subset_2_1,
       tex = TRUE,
       signifCode= c(`***` = 0.001, `**` = 0.01, `*` = 0.05),
       file='./Tables/Figure_2d_2.txt',
       replace=TRUE,
       title='Results from OLS Regression Results Presented in Figure 2d (25-50 Percentage Quartile of News Quality)',
       dict=c("Seven_Ordinal" = "Ordinal Scale (7-Point)",
              "Four_Ordinal" = "Ordinal Scale (4-Point)",
              "True_Dummy" = "Categorical Scale (True = 1)",
              "Treat_Search" = "Treatment",
              "Article_day" = "Article",
              "ResponseId" = "Respondent",
              "Income_Score" = "Income",
              "Education_Score" = "Education",
              "Ideo_Congruence" = "Ideological Congruence",
              "Dig_Lit_Score" = "Digital Literacy Score"))


#Filter by quartile:
Third_lowest_quartile_T_data <- T_Data %>% filter(avg_score >= quantile(T_Data$avg_score,na.rm=T)[3] & avg_score < quantile(T_Data$avg_score,na.rm=T)[4])

#Select Variables:
Third_lowest_quartile_T_data <- Third_lowest_quartile_T_data %>% select(Article_day,True_Dummy,Four_Ordinal,Seven_Ordinal,Age,Gender,ResponseId,Education_Score,Income_Score,Ideo_Congruence)
Third_lowest_quartile_T_data$ResponseId <- as.character(Third_lowest_quartile_T_data$ResponseId)
Third_lowest_quartile_T_data$Article_day <- as.character(Third_lowest_quartile_T_data$Article_day)
Third_lowest_quartile_T_data$Treatment <- 1

#Merge treatment and control articles:
Third_lowest_quartile_all_data <- rbind(Third_lowest_quartile_T_data,Control_Data)
#Remove NAs
Third_lowest_quartile_all_data <- na.omit(Third_lowest_quartile_all_data)

#Run OLS Model with clustered standard errors:
lin_results_fit_subset_3_1 = feols(Seven_Ordinal ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=Third_lowest_quartile_all_data)
CI_2_1 = confint(lin_results_fit_subset_3_1,se='twoway')

#Run OLS Model with clustered standard errors:
lin_results_fit_subset_3_2 = feols(Four_Ordinal ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=Third_lowest_quartile_all_data)
CI_2_2 = confint(lin_results_fit_subset_3_2,se='twoway')

#Run OLS Model with clustered standard errors:
lin_results_fit_subset_3_3 = feols(True_Dummy ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=Third_lowest_quartile_all_data)
CI_2_3 = confint(lin_results_fit_subset_3_3,se='twoway')


etable(lin_results_fit_subset_3_3,lin_results_fit_subset_3_2,lin_results_fit_subset_3_1,
       tex = TRUE,
       signifCode= c(`***` = 0.001, `**` = 0.01, `*` = 0.05),
       file='./Tables/Figure_2d_3.txt',
       replace=TRUE,
       title='Results from OLS Regression Results Presented in Figure 2d (50-75 Percentage Quartile of News Quality)',
       dict=c("Seven_Ordinal" = "Ordinal Scale (7-Point)",
              "Four_Ordinal" = "Ordinal Scale (4-Point)",
              "True_Dummy" = "Categorical Scale (True = 1)",
              "Treat_Search" = "Treatment",
              "Article_day" = "Article",
              "ResponseId" = "Respondent",
              "Income_Score" = "Income",
              "Education_Score" = "Education",
              "Ideo_Congruence" = "Ideological Congruence",
              "Dig_Lit_Score" = "Digital Literacy Score"))


#Filter by quartile:
highest_quartile_T_data <- T_Data %>% filter(avg_score >= quantile(T_Data$avg_score,na.rm=T)[4])

#Select Variables:
highest_quartile_T_data <- highest_quartile_T_data %>% select(Article_day,True_Dummy,Four_Ordinal,Seven_Ordinal,Age,Gender,ResponseId,Education_Score,Income_Score,Ideo_Congruence)
highest_quartile_T_data$ResponseId <- as.character(highest_quartile_T_data$ResponseId)
highest_quartile_T_data$Article_day <- as.character(highest_quartile_T_data$Article_day)
highest_quartile_T_data$Treatment <- 1

#Merge treatment and control articles:
highest_quartile_all_data <- rbind(highest_quartile_T_data,Control_Data)
#Remove NAs
highest_quartile_all_data <- na.omit(highest_quartile_all_data)


#Run OLS Model with clustered standard errors:
lin_results_fit_subset_highest_1 = feols(Seven_Ordinal ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=highest_quartile_all_data)

#Run OLS Model with clustered standard errors:
lin_results_fit_subset_highest_2 = feols(Four_Ordinal ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=highest_quartile_all_data)

#Run OLS Model with clustered standard errors:
lin_results_fit_subset_highest_3 = feols(True_Dummy ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=highest_quartile_all_data)


etable(lin_results_fit_subset_highest_3,lin_results_fit_subset_highest_2,lin_results_fit_subset_highest_1,
       tex = TRUE,
       signifCode= c(`***` = 0.001, `**` = 0.01, `*` = 0.05),
       file='./Tables/Figure_2d_4.txt',
       replace=TRUE,
       title='Results from OLS Regression Results Presented in Figure 2d (75-100 Percentage Quartile of News Quality)',
       dict=c("Seven_Ordinal" = "Ordinal Scale (7-Point)",
              "Four_Ordinal" = "Ordinal Scale (4-Point)",
              "True_Dummy" = "Categorical Scale (True = 1)",
              "Treat_Search" = "Treatment",
              "Article_day" = "Article",
              "ResponseId" = "Respondent",
              "Income_Score" = "Income",
              "Education_Score" = "Education",
              "Ideo_Congruence" = "Ideological Congruence",
              "Dig_Lit_Score" = "Digital Literacy Score"))


####################################################################################

########################## Regression Table for Figure 3A ##########################

####################################################################################

Study_5_treat_data <- read.csv('./Data/Study_5_treat_data.csv')

#Merge treatment data from study 5 and the search results data:

T_Data <- Study_5_treat_data %>% filter(FC_Eval == 'FM')

#Run OLS Model with clustered standard errors:

Prop_Dummy_results = feols(Unrel_contain ~ Age + Gender + Education_Score + Income_Score + Ideo_Congruence +Dig_Lit_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=T_Data)

etable(Prop_Dummy_results,
       tex = TRUE,
       signifCode= c(`***` = 0.001, `**` = 0.01, `*` = 0.05),
       file='./Tables/Figure_3_a.txt',
       replace=TRUE,
       title='Results from OLS Regression Results Presented in Figure 3a',
       dict=c("Unrel_contain" = "Returns Unreliable News Source",
              "Article_day" = "Article",
              "ResponseId" = "Respondent",
              "Income_Score" = "Income",
              "Education_Score" = "Education",
              "Ideo_Congruence" = "Ideological Congruence",
              "Dig_Lit_Score" = "Digital Literacy Score"))



####################################################################################

########################## Regression Table for Figure 3C ##########################

####################################################################################


#Pull-in search data data:
Headline_coding <- read.csv('./Data/Headline_Coding_4.csv')

#Select variables needed:
Headline_coding <- Headline_coding %>% select(ResponseId,Article_day,Headline_Link,Age,Gender,Education_Score,Income_Score,Ideo_Congruence,Dig_Lit_Score)

#Run OLS Model with clustered standard errors:
Headline_results = feols(Headline_Link ~ Age + Gender + Education_Score + Income_Score + Ideo_Congruence +Dig_Lit_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=Headline_coding)

#Write Table:
etable(Headline_results,
       tex = TRUE,
       signifCode= c(`***` = 0.001, `**` = 0.01, `*` = 0.05),
       file='./Tables/Figure_3_c.txt',
       replace=TRUE,
       title='Predicted Use of Headline or URL as a Search Query when Searching Online about Misinformation')

####################################################################################

########################## Regression Tables for Figure 4A ##########################

####################################################################################

#Study 1 (True Articles):

Study_1_df_T <- read.csv('./Data/Study_1_df_T.csv')

T_LQ_Data_Study_1 <- Study_1_df_T %>% filter(Article == "1" | Article == "2" | Article == "3")
T_Mainstream_Data_Study_1 <- Study_1_df_T %>% filter(Article == "4" | Article == "5")

All_T_Study_1 <- rbind(T_LQ_Data_Study_1,T_Mainstream_Data_Study_1)

#Run OLS Model with clustered standard errors:
lin_results_fit_1_1_1 = feols(True_Dummy ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = All_T_Study_1)

#Write Table:
etable(lin_results_fit_1_1_1, tex = TRUE,
       signifCode= c(`***` = 0.001, `**` = 0.01, `*` = 0.05),
       file='./Tables/Figure_4_A_1.txt',
       replace=TRUE,
       title='Results from OLS Regression Results Presented in Figure 4a (True Articles - Study 1)')


#Study 1 (False/Misleading Articles):
#Read in data (Study 1 - false/misleading articles):
Misl_False_Search_MF <- read.csv('./Data/Study_1_df_FM.csv')

#Run OLS Model with clustered standard errors:
lin_results_fit_1_3_1 = feols(Susc_FN ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Misl_False_Search_MF)


#Write Table:
etable(lin_results_fit_1_3_1, tex = TRUE,
       signifCode= c(`***` = 0.001, `**` = 0.01, `*` = 0.05),
       file='./Tables/Figure_4_A_2.txt',
       replace=TRUE,
       title='Results from OLS Regression Results Presented in Figure 4a (False/Misleading Articles - Study 1)')

#Study 2 (True Articles):
#Read in data (Study 2 - true articles):
Data_Bef_Aft_T <- read.csv('./Data/Study_2_df_T.csv')

T_LQ_Data_Study_2 <- Data_Bef_Aft_T %>% filter(Article == "1" | Article == "2" | Article == "3")
T_Mainstream_Data_Study_2 <- Data_Bef_Aft_T %>% filter(Article == "4" | Article == "5")


All_T_Study_2 <- rbind(T_LQ_Data_Study_2,T_Mainstream_Data_Study_2)

#Run OLS Model with clustered standard errors:
lin_results_fit_2_1_1 = feols(True_Dummy ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = All_T_Study_2)


#Write Table:
etable(lin_results_fit_2_1_1, tex = TRUE,
       signifCode= c(`***` = 0.001, `**` = 0.01, `*` = 0.05),
       file='./Tables/Figure_4_A_3.txt',
       replace=TRUE,
       title='Results from OLS Regression Results Presented in Figure 4a (True Articles - Study 2)')


#Study 2 (False/Misleading Articles):
#Pull in this data: Study 2 - False/Misleading Articles:
Data_Bef_Aft_MF <- read.csv('./Data/Study_2_df_FM.csv')

#Run OLS Model with clustered standard errors:
lin_results_fit_2_3_1 = feols(Susc_FN ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Data_Bef_Aft_MF)

#Write Table:
etable(lin_results_fit_2_3_1, tex = TRUE,
       signifCode= c(`***` = 0.001, `**` = 0.01, `*` = 0.05),
       file='./Tables/Figure_4_A_4.txt',
       replace=TRUE,
       title='Results from OLS Regression Results Presented in Figure 4a (False/Misleading Articles - Study 2)')

#Study 3 (True Articles):
Study_3_df_T <- read.csv('./Data/Study_3_df_T.csv')

T_LQ_Data_Study_3 <- Study_3_df_T %>% filter(Article == "1" | Article == "2" | Article == "3")
T_Mainstream_Data_Study_3 <- Study_3_df_T %>% filter(Article == "4" | Article == "5")


All_T_Study_3 <- rbind(T_LQ_Data_Study_3,T_Mainstream_Data_Study_3)


#Run linear regression and produce coefficient values:
lin_results_fit_3_1_1 = feols(True_Dummy ~ Treatment + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = All_T_Study_3)

#Write Table:
etable(lin_results_fit_3_1_1, tex = TRUE,
       signifCode= c(`***` = 0.001, `**` = 0.01, `*` = 0.05),
       file='./Tables/Figure_4_A_5.txt',
       replace=TRUE,
       title='Results from OLS Regression Results Presented in Figure 4a (True Articles - Study 3)')

#Read in data - false/misleading articles (Study 3):
Study_3_False_M <- read.csv('./Data/Study_3_df_FM.csv')

#Run linear regression and produce coefficient values:
lin_results_fit_3_3_1 = feols(True_Dummy ~ Treatment + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Study_3_False_M)


#Write Table:
etable(lin_results_fit_3_3_1, tex = TRUE,
       signifCode= c(`***` = 0.001, `**` = 0.01, `*` = 0.05),
       file='./Tables/Figure_4_A_6.txt',
       replace=TRUE,
       title='Results from OLS Regression Results Presented in Figure 4a (False/Misleading Articles - Study 3)')



#Study 4 (True Articles):
#Read in data:
Data_Bef_Aft_Covid_T <- read.csv('./Data/Study_4_df_T.csv')

T_LQ_Data_Study_4 <- Data_Bef_Aft_Covid_T %>% filter(Article == "1" | Article == "2" | Article == "3")
T_Mainstream_Data_Study_4 <- Data_Bef_Aft_Covid_T %>% filter(Article == "4" | Article == "5")

All_T_Study_4 <- rbind(T_LQ_Data_Study_4,T_Mainstream_Data_Study_4)


#Run OLS Model with clustered standard errors:
lin_results_fit_4_1_1 = feols(True_Dummy ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = All_T_Study_4)



#Write Table:
etable(lin_results_fit_4_1_1, tex = TRUE,
       signifCode= c(`***` = 0.001, `**` = 0.01, `*` = 0.05),
       file='./Tables/Figure_4_A_7.txt',
       replace=TRUE,
       title='Results from OLS Regression Results Presented in Figure 4a (True Articles - Study 4)')


#Study 4 (False Misleading Articles)
Study_4_False_M <- read.csv('./Data/Study_4_df_FM.csv')

#Run OLS Model with clustered standard errors:
lin_results_fit_4_3_1 = feols(Susc_FN ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Study_4_False_M)


#Write Table:
etable(lin_results_fit_4_3_1, tex = TRUE,
       signifCode= c(`***` = 0.001, `**` = 0.01, `*` = 0.05),
       file='./Tables/Figure_4_A_8.txt',
       replace=TRUE,
       title='Results from OLS Regression Results Presented in Figure 4a (False/Misleading Articles - Study 4)')


#Study 5 (True Articles):
#Read in data:
T_Data_Study_5 <- read.csv('./Data/Study_5_df_T.csv')

T_LQ_Data_Study_5 <- T_Data_Study_5 %>% filter(Article_Num == 1 | Article_Num == 2 | Article_Num == 3)
T_Mainstream_Data_Study_5 <- T_Data_Study_5 %>% filter(Article_Num == 4 | Article_Num == 5)


All_T_Study_5 <- rbind(T_LQ_Data_Study_5,T_Mainstream_Data_Study_5)


#Run OLS Model with clustered standard errors:
lin_results_fit_5_1_1 = feols(True_Dummy ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = All_T_Study_5)

#Write Table:
etable(lin_results_fit_5_1_1, tex = TRUE,
       signifCode= c(`***` = 0.001, `**` = 0.01, `*` = 0.05),
       file='./Tables/Figure_4_A_9.txt',
       replace=TRUE,
       title='Results from OLS Regression Results Presented in Figure 4a (True Articles - Study 5)')


#Study 5 (False/Misleading Articles):

Study_5_False_M <- read.csv('./Data/Study_5_df_FM.csv')

#Run OLS Model with clustered standard errors:
lin_results_fit_5_3_1 = feols(True_Dummy ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Study_5_False_M)

#Write Table:
etable(lin_results_fit_5_3_1, tex = TRUE,
       signifCode= c(`***` = 0.001, `**` = 0.01, `*` = 0.05),
       file='./Tables/Figure_4_A_10.txt',
       replace=TRUE,
       title='Results from OLS Regression Results Presented in Figure 4a (False/Misleading Articles - Study 5)')

####################################################################################

########################## Regression Tables for Figure 4B #########################

####################################################################################

#True Low-Quality Articles (Study 1):
#Run OLS Model with clustered standard errors:
lin_results_fit_1_1_1 = feols(True_Dummy ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = T_LQ_Data_Study_1)


#Write Table:
etable(lin_results_fit_1_1_1, tex = TRUE,
       signifCode= c(`***` = 0.001, `**` = 0.01, `*` = 0.05),
       file='./Tables/Figure_4b_1.txt',
       replace=TRUE,
       title='Results from OLS Regression Results Presented in Figure 4b (True Low Quality Articles - Study 1)')



#True Mainstream Articles (Study 1):
#Run OLS Model with clustered standard errors:
lin_results_fit_1_2_1 = feols(True_Dummy ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = T_Mainstream_Data_Study_1)

#Write Table:
etable(lin_results_fit_1_2_1, tex = TRUE,
       signifCode= c(`***` = 0.001, `**` = 0.01, `*` = 0.05),
       file='./Tables/Figure_4b_2.txt',
       replace=TRUE,
       title='Results from OLS Regression Results Presented in Figure 4b (True Mainstream Articles - Study 1)')



#False/Misleading Articles (Study 1):
#Run OLS Model with clustered standard errors:
lin_results_fit_1_3_1 = feols(Susc_FN ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Misl_False_Search_MF)


#Write Table:
etable(lin_results_fit_1_3_1, tex = TRUE,
       signifCode= c(`***` = 0.001, `**` = 0.01, `*` = 0.05),
       file='./Tables/Figure_4b_3.txt',
       replace=TRUE,
       title='Results from OLS Regression Results Presented in Figure 4b (False/Misleading Articles - Study 1)')



#True Low-Quality Articles (Study 2):
#Run OLS Model with clustered standard errors:
lin_results_fit_2_1_1 = feols(True_Dummy ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = T_LQ_Data_Study_2)

#Write Table:
etable(lin_results_fit_2_1_1, tex = TRUE,
       signifCode= c(`***` = 0.001, `**` = 0.01, `*` = 0.05),
       file='./Tables/Figure_4b_4.txt',
       replace=TRUE,
       title='Results from OLS Regression Results Presented in Figure 4b (True Low Quality Articles - Study 2)')





#True Mainstream Articles (Study 2):
#Run OLS Model with clustered standard errors:
lin_results_fit_2_2_1 = feols(True_Dummy ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = T_Mainstream_Data_Study_2)

#Write Table:
etable(lin_results_fit_2_2_1, tex = TRUE,
       signifCode= c(`***` = 0.001, `**` = 0.01, `*` = 0.05),
       file='./Tables/Figure_4b_5.txt',
       replace=TRUE,
       title='Results from OLS Regression Results Presented in Figure 4b (True Mainstream Articles - Study 2)')






#False/Misleading Articles (Study 2):
#Run OLS Model with clustered standard errors:
lin_results_fit_2_3_1 = feols(Susc_FN ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Data_Bef_Aft_MF)

#Write Table:
etable(lin_results_fit_2_3_1, tex = TRUE,
       signifCode= c(`***` = 0.001, `**` = 0.01, `*` = 0.05),
       file='./Tables/Figure_4b_6.txt',
       replace=TRUE,
       title='Results from OLS Regression Results Presented in Figure 4b (False/Misleading Articles - Study 2)')





#True Low-Quality Articles (Study 3):
#Run OLS Model with clustered standard errors:
lin_results_fit_3_1_1 = feols(True_Dummy ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = T_LQ_Data_Study_3)

#Write Table:
etable(lin_results_fit_3_1_1, tex = TRUE,
       signifCode= c(`***` = 0.001, `**` = 0.01, `*` = 0.05),
       file='./Tables/Figure_4b_7.txt',
       replace=TRUE,
       title='Results from OLS Regression Results Presented in Figure 4b (True Low Quality Articles - Study 3)')


#True Mainstream Articles (Study 3):
#Run OLS Model with clustered standard errors:
lin_results_fit_3_2_1 = feols(True_Dummy ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = T_Mainstream_Data_Study_3)

#Write Table:
etable(lin_results_fit_3_2_1, tex = TRUE,
       signifCode= c(`***` = 0.001, `**` = 0.01, `*` = 0.05),
       file='./Tables/Figure_4b_8.txt',
       replace=TRUE,
       title='Results from OLS Regression Results Presented in Figure 4b (True Mainstream Articles - Study 3)')




#False/Misleading Articles (Study 3):
#Run OLS Model with clustered standard errors:
lin_results_fit_3_3_1 = feols(True_Dummy ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Study_3_False_M)

#Write Table:
etable(lin_results_fit_3_3_1, tex = TRUE,
       signifCode= c(`***` = 0.001, `**` = 0.01, `*` = 0.05),
       file='./Tables/Figure_4b_9.txt',
       replace=TRUE,
       title='Results from OLS Regression Results Presented in Figure 4b (False/Misleading Articles - Study 3)')



#True Low-Quality Articles (Study 4):
#Run OLS Model with clustered standard errors:
lin_results_fit_4_1_1 = feols(True_Dummy ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = T_LQ_Data_Study_4)

#Write Table:
etable(lin_results_fit_4_1_1, tex = TRUE,
       signifCode= c(`***` = 0.001, `**` = 0.01, `*` = 0.05),
       file='./Tables/Figure_4b_10.txt',
       replace=TRUE,
       title='Results from OLS Regression Results Presented in Figure 4b (True Low Quality Articles - Study 4)')




#True Mainstream Articles (Study 4):
#Run OLS Model with clustered standard errors:
lin_results_fit_4_2_1 = feols(True_Dummy ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = T_Mainstream_Data_Study_4)

#Write Table:
etable(lin_results_fit_4_2_1, tex = TRUE,
       signifCode= c(`***` = 0.001, `**` = 0.01, `*` = 0.05),
       file='./Tables/Figure_4b_11.txt',
       replace=TRUE,
       title='Results from OLS Regression Results Presented in Figure 4b (True Mainstream Articles - Study 4)')



#False/Misleading Articles (Study 4):
#Run OLS Model with clustered standard errors:
lin_results_fit_4_3_1 = feols(Susc_FN ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Study_4_False_M)

#Write Table:
etable(lin_results_fit_4_3_1, tex = TRUE,
       signifCode= c(`***` = 0.001, `**` = 0.01, `*` = 0.05),
       file='./Tables/Figure_4b_12.txt',
       replace=TRUE,
       title='Results from OLS Regression Results Presented in Figure 4b (False/Misleading Articles - Study 4)')



#True Low-Quality Articles (Study 5):
#Run OLS Model with clustered standard errors:
lin_results_fit_5_1_1 = feols(True_Dummy ~ Treatment + Age + Ideo_Congruence  + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = T_LQ_Data_Study_5)

#Write Table:
etable(lin_results_fit_5_1_1, tex = TRUE,
       signifCode= c(`***` = 0.001, `**` = 0.01, `*` = 0.05),
       file='./Tables/Figure_4b_13.txt',
       replace=TRUE,
       title='Results from OLS Regression Results Presented in Figure 4b (True Low Quality Articles - Study 5)')




#True Mainstream Articles (Study 5):
#Run OLS Model with clustered standard errors:
lin_results_fit_5_2_1 = feols(True_Dummy ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = T_Mainstream_Data_Study_5)

#Write Table:
etable(lin_results_fit_5_2_1, tex = TRUE,
       signifCode= c(`***` = 0.001, `**` = 0.01, `*` = 0.05),
       file='./Tables/Figure_4b_14.txt',
       replace=TRUE,
       title='Results from OLS Regression Results Presented in Figure 4b (True Mainstream Articles - Study 5)')


#False/Misleading Articles (Study 5):
#Run OLS Model with clustered standard errors:
lin_results_fit_5_3_1 = feols(True_Dummy ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Study_5_False_M)

#Write Table:
etable(lin_results_fit_5_3_1, tex = TRUE,
       signifCode= c(`***` = 0.001, `**` = 0.01, `*` = 0.05),
       file='./Tables/Figure_4b_15.txt',
       replace=TRUE,
       title='Results from OLS Regression Results Presented in Figure 4b (False/Misleading Articles - Study 5)')

####################################################################################

########################## Regression Tables for Figure 4C #########################

####################################################################################

#Between-Respondent Experiment - True Low-Quality Articles:

T_LQ_Data_Study_1 <- T_LQ_Data_Study_1 %>% select(True_Dummy,Likert_Evaluation,Treat_Search,Age,Dummy_Congruence,Education_Score,Gender,Income_Score,Article_day,Article_day,ResponseId)
T_LQ_Data_Study_5 <- T_LQ_Data_Study_5 %>% select(True_Dummy,Seven_Ordinal,Treatment,Age,Ideo_Congruence,Education_Score,Gender,Income_Score,Article_day,Article_day,ResponseId)

T_LQ_Data_Study_5$Likert_Evaluation <- T_LQ_Data_Study_5$Seven_Ordinal
T_LQ_Data_Study_5$Treat_Search <- T_LQ_Data_Study_5$Treatment
T_LQ_Data_Study_5$Dummy_Congruence <- T_LQ_Data_Study_5$Ideo_Congruence

T_LQ_Data_Study_5$Seven_Ordinal <- NULL
T_LQ_Data_Study_5$Treatment <- NULL
T_LQ_Data_Study_5$Ideo_Congruence <- NULL

btwn_T_LQ_studies <- rbind(T_LQ_Data_Study_1,T_LQ_Data_Study_5)


#Run OLS Model with clustered standard errors:
btwn_T_LQ_results_1 = feols(True_Dummy ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = btwn_T_LQ_studies)


#Write Table:
etable(btwn_T_LQ_results_1, tex = TRUE,
       signifCode= c(`***` = 0.001, `**` = 0.01, `*` = 0.05),
       file='./Tables/Figure_4c_1.txt',
       replace=TRUE,
       title='Results from OLS Regression Results Presented in Figure 4c (Between-Respondent Experiment - True Low-Quality Articles)')



#Within-Respondent Experiment - True Low-Quality Articles

T_LQ_Data_Study_2 <- T_LQ_Data_Study_2 %>% select(True_Dummy,Likert_Evaluation,Treat_Search,Age,Dummy_Congruence,Education_Score,Gender,Income_Score,Article_day,ResponseId)
T_LQ_Data_Study_3 <- T_LQ_Data_Study_3 %>% select(True_Dummy,Likert_Evaluation,Treat_Search,Age,Dummy_Congruence,Education_Score,Gender,Income_Score,Article_day,ResponseId)
T_LQ_Data_Study_4 <- T_LQ_Data_Study_4 %>% select(True_Dummy,Likert_Evaluation,Treat_Search,Age,Dummy_Congruence,Education_Score,Gender,Income_Score,Article_day,ResponseId)

wthn_T_LQ_studies <- rbind(T_LQ_Data_Study_2,T_LQ_Data_Study_3,T_LQ_Data_Study_4)

#Run OLS Model with clustered standard errors:
wthn_T_LQ_results_1 = feols(True_Dummy ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = wthn_T_LQ_studies)


#Write Table:
etable(wthn_T_LQ_results_1, tex = TRUE,
       signifCode= c(`***` = 0.001, `**` = 0.01, `*` = 0.05),
       file='./Tables/Figure_4c_2.txt',
       replace=TRUE,
       title='Results from OLS Regression Results Presented in Figure 4c (Within-Respondent Experiment - True Low-Quality Articles)')


#Between-Respondent Experiment - True Mainstream Articles

T_Mainstream_Data_Study_1 <- T_Mainstream_Data_Study_1 %>% select(True_Dummy,Likert_Evaluation,Treat_Search,Age,Dummy_Congruence,Education_Score,Gender,Income_Score,Article_day,Article_day,ResponseId)
T_Mainstream_Data_Study_5 <- T_Mainstream_Data_Study_5 %>% select(True_Dummy,Seven_Ordinal,Treatment,Age,Ideo_Congruence,Education_Score,Gender,Income_Score,Article_day,Article_day,ResponseId)

T_Mainstream_Data_Study_5$Likert_Evaluation <- T_Mainstream_Data_Study_5$Seven_Ordinal
T_Mainstream_Data_Study_5$Treat_Search <- T_Mainstream_Data_Study_5$Treatment
T_Mainstream_Data_Study_5$Dummy_Congruence <- T_Mainstream_Data_Study_5$Ideo_Congruence

T_Mainstream_Data_Study_5$Seven_Ordinal <- NULL
T_Mainstream_Data_Study_5$Treatment <- NULL
T_Mainstream_Data_Study_5$Ideo_Congruence <- NULL

btwn_T_M_studies <- rbind(T_Mainstream_Data_Study_1,T_Mainstream_Data_Study_5)

#Run OLS Model with clustered standard errors:
btwn_T_M_results_1 = feols(True_Dummy ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = btwn_T_M_studies)

#Write Table:
etable(btwn_T_LQ_results_1, tex = TRUE,
       signifCode= c(`***` = 0.001, `**` = 0.01, `*` = 0.05),
       file='./Tables/Figure_4c_3.txt',
       replace=TRUE,
       title='Results from OLS Regression Results Presented in Figure 4c (Between-Respondent Experiment - True Mainstream Articles)')

#Within-Respondent Experiment - True Mainstream Articles

T_Mainstream_Data_Study_2 <- T_Mainstream_Data_Study_2 %>% select(True_Dummy,Likert_Evaluation,Treat_Search,Age,Dummy_Congruence,Education_Score,Gender,Income_Score,Article_day,ResponseId)
T_Mainstream_Data_Study_3 <- T_Mainstream_Data_Study_3 %>% select(True_Dummy,Likert_Evaluation,Treat_Search,Age,Dummy_Congruence,Education_Score,Gender,Income_Score,Article_day,ResponseId)
T_Mainstream_Data_Study_4 <- T_Mainstream_Data_Study_4 %>% select(True_Dummy,Likert_Evaluation,Treat_Search,Age,Dummy_Congruence,Education_Score,Gender,Income_Score,Article_day,ResponseId)

wthn_T_M_studies <- rbind(T_Mainstream_Data_Study_2,T_Mainstream_Data_Study_3,T_Mainstream_Data_Study_4)

#Run OLS Model with clustered standard errors:
wthn_T_M_results_1 = feols(True_Dummy ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = wthn_T_M_studies)

#Write Table:
etable(wthn_T_M_results_1, tex = TRUE,
       signifCode= c(`***` = 0.001, `**` = 0.01, `*` = 0.05),
       file='./Tables/Figure_4c_4.txt',
       replace=TRUE,
       title='Results from OLS Regression Results Presented in Figure 4c (Within-Respondent Experiment - True Mainstream Articles)')



#Within-Respondent Experiment - False/Misleading Articles

Data_Bef_Aft_MF <- Data_Bef_Aft_MF %>% select(Susc_FN,Likert_Evaluation,Treat_Search,Age,Dummy_Congruence,Education_Score,Gender,Income_Score,Article_day,ResponseId)
Study_3_False_M <- Study_3_False_M %>% select(True_Dummy,Likert_Evaluation,Treat_Search,Age,Dummy_Congruence,Education_Score,Gender,Income_Score,Article_day,ResponseId)
Study_4_False_M <- Study_4_False_M %>% select(Susc_FN,Likert_Evaluation,Treat_Search,Age,Dummy_Congruence,Education_Score,Gender,Income_Score,Article_day,ResponseId)

colnames(Data_Bef_Aft_MF)[1] <- 'True_Dummy'
colnames(Study_4_False_M)[1] <- 'True_Dummy'

Within_FM_Study <- rbind(Data_Bef_Aft_MF,Study_3_False_M,Study_4_False_M)

#Run OLS Model with clustered standard errors:
wthn_FM_results_1 = feols(True_Dummy ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Within_FM_Study)

#Write Table:
etable(wthn_FM_results_1, tex = TRUE,
       signifCode= c(`***` = 0.001, `**` = 0.01, `*` = 0.05),
       file='./Tables/Figure_4c_5.txt',
       replace=TRUE,
       title='Results from OLS Regression Results Presented in Figure 4c (Within-Respondent Experiment - False/Misleading Articles)')


#Betweenn-Respondent Experiment - False/Misleading Articles

Misl_False_Search_MF <- Misl_False_Search_MF %>% select(Susc_FN,Likert_Evaluation,Treat_Search,Age,Dummy_Congruence,Education_Score,Gender,Income_Score,Article_day,Article_day,ResponseId)
Study_5_False_M <- Study_5_False_M %>% select(True_Dummy,Seven_Ordinal,Treatment,Age,Ideo_Congruence,Education_Score,Gender,Income_Score,Article_day,Article_day,ResponseId)

colnames(Misl_False_Search_MF)[1] <- 'True_Dummy'

Study_5_False_M$Likert_Evaluation <- Study_5_False_M$Seven_Ordinal
Study_5_False_M$Treat_Search <- Study_5_False_M$Treatment
Study_5_False_M$Dummy_Congruence <- Study_5_False_M$Ideo_Congruence

Study_5_False_M$Seven_Ordinal <- NULL
Study_5_False_M$Treatment <- NULL
Study_5_False_M$Ideo_Congruence <- NULL

Between_FM_Study <- rbind(Misl_False_Search_MF,Study_5_False_M)

#Run OLS Model with clustered standard errors:
btwn_FM_results_1 = feols(True_Dummy ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Between_FM_Study)

#Write Table:
etable(btwn_FM_results_1, tex = TRUE,
       signifCode= c(`***` = 0.001, `**` = 0.01, `*` = 0.05),
       file='./Tables/Figure_4c_6.txt',
       replace=TRUE,
       title='Results from OLS Regression Results Presented in Figure 4c (Betweenn-Respondent Experiment - False/Misleading Articles)')





#################################################################################

#################### Figure 2 in Supplementary Materials ########################

#################################################################################

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
Study_5_No_Search <- FM_Data_Study_5 %>% filter(Treatment == 0)
Study_5_Search<- FM_Data_Study_5 %>% filter(Treatment == 1)

#Run OLS Model with clustered standard errors:
Pred_fit_5_1 = feols(Seven_Ordinal ~ True_Dummy, cluster = ~ResponseId, data = Study_5_No_Search)
#Produce confidence intervals with clustered standard errors:
Pred_CI_5_1 <- confint(Pred_fit_5_1)

#Run OLS Model with clustered standard errors:
Pred_fit_5_2 = feols(Seven_Ordinal ~ True_Dummy, cluster = ~ResponseId, data = Study_5_Search)
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





################################################################################################################

####Figures - Robust - Mode

##############################################################################################################

#Create list of robust mode articles:

#Pull Factchecking data from Studies 1-3:
FCer_Data <- read.csv('./Data/Fact_Checker_Data_Master.csv')
FCer_Data <- FCer_Data %>% select(Article_day,Pariticipant_1_Eval,Pariticipant_2_Eval,Pariticipant_3_Eval,Pariticipant_4_Eval,Pariticipant_5_Eval,Pariticipant_6_Eval)

#Normalize responses:
FCer_Data$Pariticipant_1_Eval <- tolower(substr(FCer_Data$Pariticipant_1_Eval,1,1))
FCer_Data$Pariticipant_2_Eval <- tolower(substr(FCer_Data$Pariticipant_2_Eval,1,1))
FCer_Data$Pariticipant_3_Eval <- tolower(substr(FCer_Data$Pariticipant_3_Eval,1,1))
FCer_Data$Pariticipant_4_Eval <- tolower(substr(FCer_Data$Pariticipant_4_Eval,1,1))
FCer_Data$Pariticipant_5_Eval <- tolower(substr(FCer_Data$Pariticipant_5_Eval,1,1))
FCer_Data$Pariticipant_6_Eval <- tolower(substr(FCer_Data$Pariticipant_6_Eval,1,1))

FCer_Data$Study <- '1,2,3'

#Study 4: Covid Articles
#Pull data:
FCer_Data_covid <- read.csv('./Data/fcers_byarticle_all_sets_covid.csv')
FCer_Data_covid$Article_day <- paste0(FCer_Data_covid$date,'_',FCer_Data_covid$article_type)
FCer_Data_covid <- FCer_Data_covid %>% select(Article_day,FC1,FC2,FC3,FC4,FC5)

#Normailize columns names:
colnames(FCer_Data_covid) <- c('Article_day','Pariticipant_1_Eval','Pariticipant_2_Eval','Pariticipant_3_Eval','Pariticipant_4_Eval','Pariticipant_5_Eval')

#Create column
FCer_Data_covid$Pariticipant_6_Eval <- NA
FCer_Data_covid$Study <- '4'

#Merge:
FCer_Data <- rbind(FCer_Data,FCer_Data_covid)

#Study 5:
#Pull data:
FCer_Data_S_5 <- read.csv('./Data/FCers_Study_5.csv')
FCer_Data_S_5 <- FCer_Data_S_5 %>% select(Day_Article,Pariticipant_1_Eval,Pariticipant_2_Eval,Pariticipant_3_Eval,Pariticipant_4_Eval,Pariticipant_5_Eval)
colnames(FCer_Data_S_5)[1] <- 'Article_day'

#Create column
FCer_Data_S_5$Pariticipant_6_Eval <- NA
FCer_Data_S_5$Study <- '5'
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

###############################################################################################

##################################### Figure 3a and 3b ########################################

###############################################################################################

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
  ylim(-0.06,0.2) +
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
  ylim(-0.15,0.75) +
  scale_x_continuous(" \n",breaks=c(1,2,3,4),labels=c('Study 4',
                                                      'Study 3',
                                                      'Study 2',
                                                      'Study 1'),limits=c(0.5,4.5)) +
  coord_flip()

#Save figure:
ggsave('./Figures/All_4_Studies_Ordinal_Robust.png',height=6,width=8)

###############################################################################################

##################################### Figure 4a ###############################################

###############################################################################################


Study_5_treat_data <- read.csv('./Data/Study_5_treat_data.csv')

Search_Results_FM <- Study_5_treat_data %>% filter(FC_Eval == 'FM')
Search_Results_FM <- Search_Results_FM %>% filter(Article_day %in% Robust_Articles)

Search_Results_T <- Study_5_treat_data %>% filter(FC_Eval == 'True')
Search_Results_T <- Search_Results_T %>% filter(Article_day %in% Robust_Articles_T)


#Create Count of individuals that didnt see any unreliable news sites:
True_Count_Zero <- Search_Results_T %>% filter(Unrel_contain == 0)
FM_Count_Zero <- Search_Results_FM %>% filter(Unrel_contain == 0)

#Create Count of individuals that saw at lesat some unreliable news sites:
True_Count_Above_Zero <- Search_Results_T %>% filter(Unrel_contain > 0)
FM_Count_Above_Zero <- Search_Results_FM %>% filter(Unrel_contain > 0)


#Create matrix to plot:
Matrix_Dist <- matrix(c(nrow(True_Count_Zero)/nrow(Search_Results_T),'Zero','True',
                        nrow(FM_Count_Zero)/nrow(Search_Results_FM),'Zero','FM',
                        nrow(True_Count_Above_Zero)/nrow(Search_Results_T),'One or more','True',
                        nrow(FM_Count_Above_Zero)/nrow(Search_Results_FM),'One or more','FM'),ncol=3,byrow=T)


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


###############################################################################################

##################################### Figure 4b ###############################################

###############################################################################################

Study_5_robust <- FM_Data_Study_5 %>% filter(Article_day %in% Robust_Articles)

#Run OLS Model with clustered standard errors:
lin_results_fit_5_1 = feols(True_Dummy ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Study_5_robust)
#Produce confidence intervals with clustered standard errors:
CI_5_1 <- confint(lin_results_fit_5_1)

#Run OLS Model with clustered standard errors:
lin_results_fit_5_2 = feols(Four_Ordinal~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Study_5_robust)
#Produce confidence intervals with clustered standard errors:
CI_5_2 <- confint(lin_results_fit_5_2)

#Run OLS Model with clustered standard errors:
lin_results_fit_5_3 = feols(Seven_Ordinal ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Study_5_robust)
#Produce confidence intervals with clustered standard errors:
CI_5_3 <- confint(lin_results_fit_5_3)

#Create vector with coefficients:
Coefficients <- c(lin_results_fit_5_1$coefficients[1]/sd(FM_Data_Study_5$True_Dummy),
                  lin_results_fit_5_2$coefficients[1]/sd(FM_Data_Study_5$Four_Ordinal),
                  lin_results_fit_5_3$coefficients[1]/sd(FM_Data_Study_5$Seven_Ordinal))

#Create upper confidence intervals:
CI_Upper <- c(CI_5_1[1,2]/sd(FM_Data_Study_5$True_Dummy),
              CI_5_2[1,2]/sd(FM_Data_Study_5$Four_Ordinal),
              CI_5_3[1,2]/sd(FM_Data_Study_5$Seven_Ordinal))            

#Create lower confidence intervals:
CI_Lower <- c(CI_5_1[1,1]/sd(FM_Data_Study_5$True_Dummy),
              CI_5_2[1,1]/sd(FM_Data_Study_5$Four_Ordinal),
              CI_5_3[1,1]/sd(FM_Data_Study_5$Seven_Ordinal))

Coef_names <- c('Rate as True',
                'Ordinal Scale (4)',
                'Ordinal Scale (7)')

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
  ylim(-0.2,0.7) +
  scale_x_continuous("Perceived Veracity Scale \n",breaks=c(0.3,0.2,0.1),labels=Coef_names,limits=c(0.0,0.4)) +
  coord_flip()

#Save plot:
ggsave('./Figures/Study_5_1_ROBUST.png',height=8,width=8)


###############################################################################################

##################################### Figure 4c ###############################################

###############################################################################################

#Create Treatment Data:
#Ideological Perspective of Articles:
FC_Ideo_Data <- read.csv('./Data/FC_Ideo_Data.csv')
FC_Ideo_Data$X <- NULL

#Pull in treatment data for Study 5:
Study_5_treat_data <- read.csv('./Data/Study_5_treat_data.csv')
T_Data <- Study_5_treat_data %>% filter(FC_Eval == 'FM')

#Filter only responses who only saw very reliable news sites in Google Search Results (85)
Treat_only_rel_data <- T_Data %>% filter(Total_Rel_85 > 0)

nrow(Treat_only_rel_data)/498

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

Treat_only_rel_data <- Treat_only_rel_data %>% select(Article_day,True_Dummy,Four_Ordinal,Seven_Ordinal,Age,Gender,ResponseId,Education_Score,Income_Score,Ideo_Congruence)
Treat_only_rel_data$ResponseId <- as.character(Treat_only_rel_data$ResponseId)
Control_Data$ResponseId <- as.character(Control_Data$ResponseId)
Treat_only_rel_data$Article_day <- as.character(Treat_only_rel_data$Article_day)

Treat_only_rel_data$Treatment <- 1


#Merge treatment and control articles:
Study_5_subset_1 <- rbind(Treat_only_rel_data,Control_Data)

Study_5_subset_1 <- Study_5_subset_1 %>% select(True_Dummy,Four_Ordinal,Seven_Ordinal,Treatment,Age,Ideo_Congruence,Education_Score,Gender,Income_Score,Article_day,ResponseId)

Study_5_subset_1 <- Study_5_subset_1 %>% filter(Article_day %in% Robust_Articles)

#Remove NAs
#Study_5_subset_1 <- na.omit(Study_5_subset_1)

#Run OLS Model with clustered standard errors:
fit_2_1 = feols(Seven_Ordinal ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=Study_5_subset_1)
CI_2_1 = confint(fit_2_1,se='twoway')

#Run OLS Model with clustered standard errors:
fit_2_2 = feols(Four_Ordinal ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=Study_5_subset_1)
CI_2_2 = confint(fit_2_2,se='twoway')

#Run OLS Model with clustered standard errors:
fit_2_3 = feols(True_Dummy ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=Study_5_subset_1)
CI_2_3 = confint(fit_2_3,se='twoway')

#Create empty matrix:
Fig_2c_Mat <- matrix(ncol=5)

#Merge data:
New_matr <- matrix(c(round(fit_2_1$coefficients[1]/sd(Study_5_subset_1$Seven_Ordinal),4),round(CI_2_1[1,1]/sd(Study_5_subset_1$Seven_Ordinal),4),round(CI_2_1[1,2]/sd(Study_5_subset_1$Seven_Ordinal),4),'Ordinal (7)','Only Very Reliable News',
                     round(fit_2_2$coefficients[1]/sd(Study_5_subset_1$Four_Ordinal),4),round(CI_2_2[1,1]/sd(Study_5_subset_1$Four_Ordinal),4),round(CI_2_2[1,2]/sd(Study_5_subset_1$Four_Ordinal),4),'Ordinal (4)','Only Very Reliable News',
                     round(fit_2_3$coefficients[1]/sd(Study_5_subset_1$True_Dummy),4),round(CI_2_3[1,1]/sd(Study_5_subset_1$True_Dummy),4),round(CI_2_3[1,2]/sd(Study_5_subset_1$True_Dummy),4),'True (Dummy)','Only Very Reliable News'),ncol=5,byrow=T)

Fig_2c_Mat <- rbind(Fig_2c_Mat,New_matr)


#Filter only responses who only saw very reliable news sites in Google Search Results (85)
T_Data <- T_Data %>% mutate(Prop_60 = Total_Unrel_60/Total_Links)
Test_dataframe <- T_Data %>% select(True_Dummy,Four_Ordinal,Seven_Ordinal,Prop_60,Total_Unrel_60,avg_score)
Treatment_unrel_data <- T_Data %>% filter(Prop_60 >= 0.1) #125
Treatment_unrel_data <- Treatment_unrel_data %>% filter(FC_Eval == 'FM')

#Select Variables:
Treatment_unrel_data <- Treatment_unrel_data %>% select(Article_day,True_Dummy,Four_Ordinal,Seven_Ordinal,Age,Gender,ResponseId,Education_Score,Income_Score,Ideo_Congruence)
Treatment_unrel_data$ResponseId <- as.character(Treatment_unrel_data$ResponseId)
Treatment_unrel_data$Article_day <- as.character(Treatment_unrel_data$Article_day)
Treatment_unrel_data$Treatment <- 1
Treatment_unrel_data <- na.omit(Treatment_unrel_data)

real_T_Data <- na.omit(T_Data)
nrow(Treatment_unrel_data)/nrow(real_T_Data)

#Merge treatment and control articles:
Study_5_subset_2 <- rbind(Treatment_unrel_data,Control_Data)
#Remove NAs
Study_5_subset_2 <- na.omit(Study_5_subset_2)


Study_5_subset_2 <- Study_5_subset_2 %>% filter(Article_day %in% Robust_Articles)


#Run OLS Model with clustered standard errors:
fit_2_1 = feols(Seven_Ordinal ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=Study_5_subset_2)
CI_2_1 = confint(fit_2_1,se='twoway')

#Run OLS Model with clustered standard errors:
fit_2_2 = feols(Four_Ordinal ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=Study_5_subset_2)
CI_2_2 = confint(fit_2_2,se='twoway')

#Run OLS Model with clustered standard errors:
fit_2_3 = feols(True_Dummy ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=Study_5_subset_2)
CI_2_3 = confint(fit_2_3,se='twoway')


#Merge data:
New_matr <- matrix(c(round(fit_2_1$coefficients[1]/sd(Study_5_subset_2$Seven_Ordinal),4),round(CI_2_1[1,1]/sd(Study_5_subset_2$Seven_Ordinal),4),round(CI_2_1[1,2]/sd(Study_5_subset_2$Seven_Ordinal),4),'Ordinal (7)','Some Unreliable News',
                     round(fit_2_2$coefficients[1]/sd(Study_5_subset_2$Four_Ordinal),4),round(CI_2_2[1,1]/sd(Study_5_subset_2$Four_Ordinal),4),round(CI_2_2[1,2]/sd(Study_5_subset_2$Four_Ordinal),4),'Ordinal (4)','Some Unreliable News',
                     round(fit_2_3$coefficients[1]/sd(Study_5_subset_2$True_Dummy),4),round(CI_2_3[1,1]/sd(Study_5_subset_2$True_Dummy),4),round(CI_2_3[1,2]/sd(Study_5_subset_2$True_Dummy),4),'True (Dummy)','Some Unreliable News'),ncol=5,byrow=T)

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
  theme(axis.title.x = element_text(size=20),
        axis.text.x  = element_text(size=26),
        axis.title.y = element_text(size=26),
        axis.text.y  = element_text(size=26),
        plot.title = element_text(size=26),
        legend.title = element_text(size=20),
        legend.text = element_text(size=20)) +
  ylim(-0.55,0.55) +
  scale_x_continuous("Type of News Returned by Google Search Engine \n",breaks=c(1.5,0.9),labels=c('At Least 10%\nof News URLs\nAre Unreliable',
                                                                                                   'Only Very Reliable\nNews Sources'),limits=c(0.5,2.0)) +
  coord_flip()

#Save Figure:
ggsave('./Figures/Coefs_CIs_ROBUST.png',height=12,width=10)


###############################################################################################

##################################### Figure 4d ###############################################

###############################################################################################

#Filter by quartile:
#Filter only responses who only saw very reliable news sites in Google Search Results (85)
lowest_quartile_T_data <- T_Data %>% filter(avg_score < quantile(T_Data$avg_score,na.rm=T)[2])

#Select Variables:
lowest_quartile_T_data <- lowest_quartile_T_data %>% select(Article_day,True_Dummy,Four_Ordinal,Seven_Ordinal,Age,Gender,ResponseId,Education_Score,Income_Score,Ideo_Congruence)
lowest_quartile_T_data$ResponseId <- as.character(lowest_quartile_T_data$ResponseId)
lowest_quartile_T_data$Article_day <- as.character(lowest_quartile_T_data$Article_day)
lowest_quartile_T_data$Treatment <- 1

#Merge treatment and control articles:
lowest_quartile_all_data <- rbind(lowest_quartile_T_data,Control_Data)

#Remove NAs
lowest_quartile_all_data <- na.omit(lowest_quartile_all_data)

lowest_quartile_all_data <- lowest_quartile_all_data %>% filter(Article_day %in% Robust_Articles)


#Run OLS Model with clustered standard errors:
fit_2_1 = feols(Seven_Ordinal ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=lowest_quartile_all_data)
CI_2_1 = confint(fit_2_1,se='twoway')

#Run OLS Model with clustered standard errors:
fit_2_2 = feols(Four_Ordinal ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=lowest_quartile_all_data)
CI_2_2 = confint(fit_2_2,se='twoway')

#Run OLS Model with clustered standard errors:
fit_2_3 = feols(True_Dummy ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=lowest_quartile_all_data)
CI_2_3 = confint(fit_2_3,se='twoway')

#Create empty matrix:
Fig_2d_Mat <- matrix(ncol=5)

Fig_Poster <- matrix(ncol=4)

#Merge data:
New_matr <- matrix(c(round(fit_2_1$coefficients[1]/sd(lowest_quartile_all_data$Seven_Ordinal),4),round(CI_2_1[1,1]/sd(lowest_quartile_all_data$Seven_Ordinal),4),round(CI_2_1[1,2]/sd(lowest_quartile_all_data$Seven_Ordinal),4),'Ordinal (7)','0-25%',
                     round(fit_2_2$coefficients[1]/sd(lowest_quartile_all_data$Four_Ordinal),4),round(CI_2_2[1,1]/sd(lowest_quartile_all_data$Four_Ordinal),4),round(CI_2_2[1,2]/sd(lowest_quartile_all_data$Four_Ordinal),4),'Ordinal (4)','0-25%',
                     round(fit_2_3$coefficients[1]/sd(lowest_quartile_all_data$True_Dummy),4),round(CI_2_3[1,1]/sd(lowest_quartile_all_data$True_Dummy),4),round(CI_2_3[1,2]/sd(lowest_quartile_all_data$True_Dummy),4),'True (Dummy)','0-25%'),ncol=5,byrow=T)

Fig_2d_Mat <- rbind(Fig_2d_Mat,New_matr)

poster_matr <- matrix(c(round(fit_2_3$coefficients[1],4),round(CI_2_3[1,1],4),round(CI_2_3[1,2],4),'0-25%'),ncol=4,byrow=T)

Fig_Poster <- rbind(Fig_Poster,poster_matr)


#Filter by quartile:
Second_lowest_quartile_T_data <- T_Data %>% filter(avg_score >= quantile(T_Data$avg_score,na.rm=T)[2] & avg_score < quantile(T_Data$avg_score,na.rm=T)[3])

#Select Variables:
Second_lowest_quartile_T_data <- Second_lowest_quartile_T_data %>% select(Article_day,True_Dummy,Four_Ordinal,Seven_Ordinal,Age,Gender,ResponseId,Education_Score,Income_Score,Ideo_Congruence)
Second_lowest_quartile_T_data$ResponseId <- as.character(Second_lowest_quartile_T_data$ResponseId)
Second_lowest_quartile_T_data$Article_day <- as.character(Second_lowest_quartile_T_data$Article_day)
Second_lowest_quartile_T_data$Treatment <- 1

#Merge treatment and control articles:
Second_lowest_quartile_all_data <- rbind(Second_lowest_quartile_T_data,Control_Data)
#Remove NAs
Second_lowest_quartile_all_data <- na.omit(Second_lowest_quartile_all_data)


Second_lowest_quartile_all_data <- Second_lowest_quartile_all_data %>% filter(Article_day %in% Robust_Articles)


#Run OLS Model with clustered standard errors:
fit_2_1 = feols(Seven_Ordinal ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=Second_lowest_quartile_all_data)
CI_2_1 = confint(fit_2_1,se='twoway')

#Run OLS Model with clustered standard errors:
fit_2_2 = feols(Four_Ordinal ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=Second_lowest_quartile_all_data)
CI_2_2 = confint(fit_2_2,se='twoway')

#Run OLS Model with clustered standard errors:
fit_2_3 = feols(True_Dummy ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=Second_lowest_quartile_all_data)
CI_2_3 = confint(fit_2_3,se='twoway')

#Merge data:
New_matr <- matrix(c(round(fit_2_1$coefficients[1]/sd(Second_lowest_quartile_all_data$Seven_Ordinal),4),round(CI_2_1[1,1]/sd(Second_lowest_quartile_all_data$Seven_Ordinal),4),round(CI_2_1[1,2]/sd(Second_lowest_quartile_all_data$Seven_Ordinal),4),'Ordinal (7)','25-50%',
                     round(fit_2_2$coefficients[1]/sd(Second_lowest_quartile_all_data$Four_Ordinal),4),round(CI_2_2[1,1]/sd(Second_lowest_quartile_all_data$Four_Ordinal),4),round(CI_2_2[1,2]/sd(Second_lowest_quartile_all_data$Four_Ordinal),4),'Ordinal (4)','25-50%',
                     round(fit_2_3$coefficients[1]/sd(Second_lowest_quartile_all_data$True_Dummy),4),round(CI_2_3[1,1]/sd(Second_lowest_quartile_all_data$True_Dummy),4),round(CI_2_3[1,2]/sd(Second_lowest_quartile_all_data$True_Dummy),4),'True (Dummy)','25-50%'),ncol=5,byrow=T)

Fig_2d_Mat <- rbind(Fig_2d_Mat,New_matr)

poster_matr <- matrix(c(round(fit_2_3$coefficients[1],4),round(CI_2_3[1,1],4),round(CI_2_3[1,2],4),'25-50%'),ncol=4,byrow=T)

Fig_Poster <- rbind(Fig_Poster,poster_matr)


#Filter by quartile:
Third_lowest_quartile_T_data <- T_Data %>% filter(avg_score >= quantile(T_Data$avg_score,na.rm=T)[3] & avg_score < quantile(T_Data$avg_score,na.rm=T)[4])

#Select Variables:
Third_lowest_quartile_T_data <- Third_lowest_quartile_T_data %>% select(Article_day,True_Dummy,Four_Ordinal,Seven_Ordinal,Age,Gender,ResponseId,Education_Score,Income_Score,Ideo_Congruence)
Third_lowest_quartile_T_data$ResponseId <- as.character(Third_lowest_quartile_T_data$ResponseId)
Third_lowest_quartile_T_data$Article_day <- as.character(Third_lowest_quartile_T_data$Article_day)
Third_lowest_quartile_T_data$Treatment <- 1

#Merge treatment and control articles:
Third_lowest_quartile_all_data <- rbind(Third_lowest_quartile_T_data,Control_Data)
#Remove NAs
Third_lowest_quartile_all_data <- na.omit(Third_lowest_quartile_all_data)

Third_lowest_quartile_all_data <- Third_lowest_quartile_all_data %>% filter(Article_day %in% Robust_Articles)


#Run OLS Model with clustered standard errors:
fit_2_1 = feols(Seven_Ordinal ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=Third_lowest_quartile_all_data)
CI_2_1 = confint(fit_2_1,se='twoway')

#Run OLS Model with clustered standard errors:
fit_2_2 = feols(Four_Ordinal ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=Third_lowest_quartile_all_data)
CI_2_2 = confint(fit_2_2,se='twoway')

#Run OLS Model with clustered standard errors:
fit_2_3 = feols(True_Dummy ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=Third_lowest_quartile_all_data)
CI_2_3 = confint(fit_2_3,se='twoway')

#Merge data:
New_matr <- matrix(c(round(fit_2_1$coefficients[1]/sd(Third_lowest_quartile_all_data$Seven_Ordinal),4),round(CI_2_1[1,1]/sd(Third_lowest_quartile_all_data$Seven_Ordinal),4),round(CI_2_1[1,2]/sd(Third_lowest_quartile_all_data$Seven_Ordinal),4),'Ordinal (7)','50-75%',
                     round(fit_2_2$coefficients[1]/sd(Third_lowest_quartile_all_data$Four_Ordinal),4),round(CI_2_2[1,1]/sd(Third_lowest_quartile_all_data$Four_Ordinal),4),round(CI_2_2[1,2]/sd(Third_lowest_quartile_all_data$Four_Ordinal),4),'Ordinal (4)','50-75%',
                     round(fit_2_3$coefficients[1]/sd(Third_lowest_quartile_all_data$True_Dummy),4),round(CI_2_3[1,1]/sd(Third_lowest_quartile_all_data$True_Dummy),4),round(CI_2_3[1,2]/sd(Third_lowest_quartile_all_data$True_Dummy),4),'True (Dummy)','50-75%'),ncol=5,byrow=T)

Fig_2d_Mat <- rbind(Fig_2d_Mat,New_matr)

poster_matr <- matrix(c(round(fit_2_3$coefficients[1],4),round(CI_2_3[1,1],4),round(CI_2_3[1,2],4),'50-75%'),ncol=4,byrow=T)

Fig_Poster <- rbind(Fig_Poster,poster_matr)



#Filter by quartile:
highest_quartile_T_data <- T_Data %>% filter(avg_score >= quantile(T_Data$avg_score,na.rm=T)[4])

#Select Variables:
highest_quartile_T_data <- highest_quartile_T_data %>% select(Article_day,True_Dummy,Four_Ordinal,Seven_Ordinal,Age,Gender,ResponseId,Education_Score,Income_Score,Ideo_Congruence)
highest_quartile_T_data$ResponseId <- as.character(highest_quartile_T_data$ResponseId)
highest_quartile_T_data$Article_day <- as.character(highest_quartile_T_data$Article_day)
highest_quartile_T_data$Treatment <- 1

#Merge treatment and control articles:
highest_quartile_all_data <- rbind(highest_quartile_T_data,Control_Data)
#Remove NAs
highest_quartile_all_data <- na.omit(highest_quartile_all_data)

highest_quartile_all_data <- highest_quartile_all_data %>% filter(Article_day %in% Robust_Articles)


#Run OLS Model with clustered standard errors:
fit_2_1 = feols(Seven_Ordinal ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=highest_quartile_all_data)
CI_2_1 = confint(fit_2_1,se='twoway')

#Run OLS Model with clustered standard errors:
fit_2_2 = feols(Four_Ordinal ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=highest_quartile_all_data)
CI_2_2 = confint(fit_2_2,se='twoway')

#Run OLS Model with clustered standard errors:
fit_2_3 = feols(True_Dummy ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=highest_quartile_all_data)
CI_2_3 = confint(fit_2_3,se='twoway')

#Merge data:
New_matr <- matrix(c(round(fit_2_1$coefficients[1]/sd(highest_quartile_all_data$Seven_Ordinal),4),round(CI_2_1[1,1]/sd(highest_quartile_all_data$Seven_Ordinal),4),round(CI_2_1[1,2]/sd(highest_quartile_all_data$Seven_Ordinal),4),'Ordinal (7)','75-100%',
                     round(fit_2_2$coefficients[1]/sd(highest_quartile_all_data$Four_Ordinal),4),round(CI_2_2[1,1]/sd(highest_quartile_all_data$Four_Ordinal),4),round(CI_2_2[1,2]/sd(highest_quartile_all_data$Four_Ordinal),4),'Ordinal (4)','75-100%',
                     round(fit_2_3$coefficients[1]/sd(highest_quartile_all_data$True_Dummy),4),round(CI_2_3[1,1]/sd(highest_quartile_all_data$True_Dummy),4),round(CI_2_3[1,2]/sd(highest_quartile_all_data$True_Dummy),4),'True (Dummy)','75-100%'),ncol=5,byrow=T)

Fig_2d_Mat <- rbind(Fig_2d_Mat,New_matr)

poster_matr <- matrix(c(round(fit_2_3$coefficients[1],4),round(CI_2_3[1,1],4),round(CI_2_3[1,2],4),'75-100%'),ncol=4,byrow=T)

Fig_Poster <- rbind(Fig_Poster,poster_matr)




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
  theme(axis.title.x = element_text(size=20),
        axis.text.x  = element_text(size=26),
        axis.title.y = element_text(size=26),
        axis.text.y  = element_text(size=26),
        plot.title = element_text(size=26),
        legend.title = element_text(size=20),
        legend.text = element_text(size=20)) +
  ylim(-0.6,0.7) +
  scale_x_continuous("Quartile of News Quality Returned by Google Search Engine \n",breaks=c(2.7,2.1,1.5,0.9),labels=c('75-100%',
                                                                                                                       '50-75%',
                                                                                                                       '25-50%',
                                                                                                                       '0-25%'),limits=c(0.5,3.0)) +
  coord_flip()

#Save Figure:
ggsave('./Figures/Coefs_CIs_2_ROBUST.png',height=12,width=10)


###############################################################################################

##################################### Figure 5A ###############################################

###############################################################################################

#Read in data. Only False/Misleading Articles:
Study_5_treat_data <- read.csv('./Data/Study_5_treat_data.csv')
T_Data <- Study_5_treat_data %>% filter(FC_Eval == 'FM')

#Filter only robust mode articles:
T_Data <- T_Data %>% filter(Article_day %in% Robust_Articles)

#Run OLS Model with clustered standard errors:
Prop_Dummy_results = feols(Unrel_contain ~ Age + Gender + Education_Score + Income_Score + Ideo_Congruence +Dig_Lit_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=T_Data)
CI_Prop_Dummy = confint(Prop_Dummy_results,se='twoway')

#Create dataframe with coefficients and confidence intervals:
Coefficients <- c(round(Prop_Dummy_results$coefficients[1]*sd(T_Data$Age),4),
                  round(Prop_Dummy_results$coefficients[2]*sd(T_Data$Gender),4),
                  round(Prop_Dummy_results$coefficients[3]*sd(T_Data$Education_Score),4),
                  round(Prop_Dummy_results$coefficients[4]*sd(T_Data$Income_Score),4),
                  round(Prop_Dummy_results$coefficients[5]*sd(T_Data$Ideo_Congruence),4),
                  round(Prop_Dummy_results$coefficients[6]*sd(T_Data$Dig_Lit_Score),4))

#Create vector with upper confidence intervals:
CI_Upper <- c(round(CI_Prop_Dummy[1,1]*sd(T_Data$Age),4),
              round(CI_Prop_Dummy[2,1]*sd(T_Data$Gender),4),
              round(CI_Prop_Dummy[3,1]*sd(T_Data$Education_Score),4),
              round(CI_Prop_Dummy[4,1]*sd(T_Data$Income_Score),4),
              round(CI_Prop_Dummy[5,1]*sd(T_Data$Ideo_Congruence),4),
              round(CI_Prop_Dummy[6,1]*sd(T_Data$Dig_Lit_Score),4))            

#Create vector with lower confidence intervals:
CI_Lower <- c(round(CI_Prop_Dummy[1,2]*sd(T_Data$Age),4),
              round(CI_Prop_Dummy[2,2]*sd(T_Data$Gender),4),
              round(CI_Prop_Dummy[3,2]*sd(T_Data$Education_Score),4),
              round(CI_Prop_Dummy[4,2]*sd(T_Data$Income_Score),4),
              round(CI_Prop_Dummy[5,2]*sd(T_Data$Ideo_Congruence),4),
              round(CI_Prop_Dummy[6,2]*sd(T_Data$Dig_Lit_Score),4))           

#Create vector with variable names:
Coef_names <- c('Age',
                'Gender',
                'Education',
                'Income',
                'Ideological\nCongruence',
                'Digital Literacy')

#Create data matrix with data for figure:
d_matrix <- cbind(Coef_names,Coefficients,CI_Upper,CI_Lower)
rownames(d_matrix) <- c()
d_matrix <- data.frame(d_matrix)
d_matrix$Coefficients <- as.character(d_matrix$Coefficients)
d_matrix$CI_Lower <- as.character(d_matrix$CI_Lower)
d_matrix$CI_Upper <- as.character(d_matrix$CI_Upper)
d_matrix$Coefficients <- as.numeric(d_matrix$Coefficients)
d_matrix$CI_Lower <- as.numeric(d_matrix$CI_Lower)
d_matrix$CI_Upper <- as.numeric(d_matrix$CI_Upper)
d_matrix$x<-c(0.1,0.2,0.3,0.4,0.5,0.6)

#Produce plot:
ggplot(data = d_matrix, aes(x = x, y = Coefficients)) +
  geom_hline(aes(yintercept = 0), color = "gray",
             linetype = 2, size = 1.2) +
  geom_point(size=4) +
  geom_linerange(aes(min = CI_Lower,
                     max = CI_Upper),
                 size=1.5) +
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
  scale_x_continuous("Demographic Covariates \n",breaks=c(0.1,0.2,0.3,0.4,0.5,0.6),labels=Coef_names,limits=c(0.0,0.7)) +
  coord_flip()

#Save figure:
ggsave('./Figures/Coefs_CIs_Predicting_Unrel_Dummy_ROBUST.png',height=8,width=12)



###############################################################################################

##################################### Figure 5B ###############################################

###############################################################################################

Headline_coding <- read.csv('./Data/Headline_Coding_4.csv')

#Remove NAs:
Headline_coding <- na.omit(Headline_coding)

#Filter only robust mode articles:
Headline_coding <- Headline_coding %>% filter(Article_day %in% Robust_Articles)

#Create dataset of search queries that does not use headline/link:
SQ_No_Headline <- Headline_coding %>% filter(Headline_Link == 0)

#Create dataset of search queries that does use headline/link:
SQ_Headline_Data <- Headline_coding %>% filter(Headline_Link == 1)

#Create Count of individuals that didnt see any unreliable news sites:
True_Count_Zero <- SQ_No_Headline %>% filter(Unrel_contain == 0)
FM_Count_Zero <- SQ_Headline_Data %>% filter(Unrel_contain == 0)

#Create Count of individuals that saw at lesat some unreliable news sites:
True_Count_Above_Zero <- SQ_No_Headline %>% filter(Unrel_contain > 0)
FM_Count_Above_Zero <- SQ_Headline_Data %>% filter(Unrel_contain > 0)



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


###############################################################################################

##################################### Figure 5C ###############################################

###############################################################################################


#Pull-in search data data:
Headline_coding <- read.csv('./Data/Headline_Coding_4.csv')

#Remove NA values:
Headline_coding <- na.omit(Headline_coding)

#Filter only robust mode articles:
Headline_coding <- Headline_coding %>% filter(Article_day %in% Robust_Articles)

#Select variables needed:
Headline_coding <- Headline_coding %>% select(ResponseId,Article_day,Headline_Link,Age,Gender,Education_Score,Income_Score,Ideo_Congruence,Dig_Lit_Score)

#Run OLS Model with clustered standard errors:
Headline_results = feols(Headline_Link ~ Age + Gender + Education_Score + Income_Score + Ideo_Congruence +Dig_Lit_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=Headline_coding)
CI_Headline = confint(Headline_results)

#List of coefficients
Coefficients <- c(round(Headline_results$coefficients[1]*sd(Headline_coding$Age),4),
                  round(Headline_results$coefficients[2]*sd(Headline_coding$Gender),4),
                  round(Headline_results$coefficients[3]*sd(Headline_coding$Education_Score),4),
                  round(Headline_results$coefficients[4]*sd(Headline_coding$Income_Score),4),
                  round(Headline_results$coefficients[5]*sd(Headline_coding$Ideo_Congruence),4),
                  round(Headline_results$coefficients[6]*sd(Headline_coding$Dig_Lit_Score),4))

#Vector of upper confidence intervals:
CI_Upper <- c(round(CI_Headline[1,1]*sd(Headline_coding$Age),4),
              round(CI_Headline[2,1]*sd(Headline_coding$Gender),4),
              round(CI_Headline[3,1]*sd(Headline_coding$Education_Score),4),
              round(CI_Headline[4,1]*sd(Headline_coding$Income_Score),4),
              round(CI_Headline[5,1]*sd(Headline_coding$Ideo_Congruence),4),
              round(CI_Headline[6,1]*sd(Headline_coding$Dig_Lit_Score),4))            

#Vector of lower confidence intervals:
CI_Lower <- c(round(CI_Headline[1,2]*sd(Headline_coding$Age),4),
              round(CI_Headline[2,2]*sd(Headline_coding$Gender),4),
              round(CI_Headline[3,2]*sd(Headline_coding$Education_Score),4),
              round(CI_Headline[4,2]*sd(Headline_coding$Income_Score),4),
              round(CI_Headline[5,2]*sd(Headline_coding$Ideo_Congruence),4),
              round(CI_Headline[6,2]*sd(Headline_coding$Dig_Lit_Score),4))           

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

###############################################################################################

##################################### Figure 6A ###############################################

###############################################################################################

#Study 1:
#Read in data:
Study_1_df_T <- read.csv('./Data/Study_1_df_T.csv')

#Filter only robust mode articles:
Study_1_df_T <- Study_1_df_T %>% filter(Article_day %in% Robust_Articles_T)

T_LQ_Data_Study_1 <- Study_1_df_T %>% filter(Article == "1" | Article == "2" | Article == "3")
T_Mainstream_Data_Study_1 <- Study_1_df_T %>% filter(Article == "4" | Article == "5")

All_T_Study_1 <- rbind(T_LQ_Data_Study_1,T_Mainstream_Data_Study_1)

#Run OLS Model with clustered standard errors:
lin_results_fit_1_1_1 = feols(True_Dummy ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = All_T_Study_1)
#Produce confidence intervals with clustered standard errors:
CI_1_1_1 <- confint(lin_results_fit_1_1_1,se='twoway')

#Read in data (Study 1 - false/misleading articles):
Misl_False_Search_MF <- read.csv('./Data/Study_1_df_FM.csv')

#Filter only robust mode articles:
Misl_False_Search_MF <- Misl_False_Search_MF %>% filter(Article_day %in% Robust_Articles)

#Run OLS Model with clustered standard errors:
lin_results_fit_1_3_1 = feols(Susc_FN ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Misl_False_Search_MF)
#Produce confidence intervals with clustered standard errors:
CI_1_3_1 <- confint(lin_results_fit_1_3_1,se='twoway')


#Study 2:
#Read in data:
Data_Bef_Aft_T <- read.csv('./Data/Study_2_df_T.csv')

#Filter only robust mode articles:
Data_Bef_Aft_T <- Data_Bef_Aft_T %>% filter(Article_day %in% Robust_Articles_T)


T_LQ_Data_Study_2 <- Data_Bef_Aft_T %>% filter(Article == "1" | Article == "2" | Article == "3")
T_Mainstream_Data_Study_2 <- Data_Bef_Aft_T %>% filter(Article == "4" | Article == "5")
All_T_Study_2 <- rbind(T_LQ_Data_Study_2,T_Mainstream_Data_Study_2)

#Run OLS Model with clustered standard errors:
lin_results_fit_2_1_1 = feols(True_Dummy ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = All_T_Study_2)
#Produce confidence intervals with clustered standard errors:
CI_2_1_1 <- confint(lin_results_fit_2_1_1,se='twoway')




#Pull in this data: Study 2 - False/Misleading Articles:
Data_Bef_Aft_MF <- read.csv('./Data/Study_2_df_FM.csv')

#Filter only robust mode articles:
Data_Bef_Aft_MF <- Data_Bef_Aft_MF %>% filter(Article_day %in% Robust_Articles)


#Run OLS Model with clustered standard errors:
lin_results_fit_2_3_1 = feols(Susc_FN ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Data_Bef_Aft_MF)
#Produce confidence intervals with clustered standard errors:
CI_2_3_1 <- confint(lin_results_fit_2_3_1,se='twoway')




#Study 3 (True Articles):
Study_3_df <- read.csv('./Data/Study_3_df_T.csv')

#Filter only robust mode articles:
Study_3_df <- Study_3_df %>% filter(Article_day %in% Robust_Articles_T)

T_LQ_Data_Study_3 <- Study_3_df %>% filter(Article == "1" | Article == "2" | Article == "3")
T_Mainstream_Data_Study_3 <- Study_3_df %>% filter(Article == "4" | Article == "5")

All_T_Study_3 <- rbind(T_LQ_Data_Study_3,T_Mainstream_Data_Study_3)

#Run linear regression and produce coefficient values:
lin_results_fit_3_1_1 = feols(Susc_FN ~ Treatment + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = All_T_Study_3)
#Produce confidence intervals using clustered standard errors:
CI_3_1_1 <- confint(lin_results_fit_3_1_1,se='twoway')


#Study 3 Analysis:
Study_3_False_M <- read.csv('./Data/Study_3_df_FM.csv')

#Filter only robust mode articles:
Study_3_False_M <- Study_3_False_M %>% filter(Article_day %in% Robust_Articles)


#Run linear regression and produce coefficient values:
lin_results_fit_3_3_1 = feols(True_Dummy ~ Treatment + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Study_3_False_M)
#Produce confidence intervals using clustered standard errors:
CI_3_3_1 <- confint(lin_results_fit_3_3_1,se='twoway')

#Study 4:
Data_Bef_Aft_Covid_T <- read.csv('./Data/Study_4_df_T.csv')

#Filter only robust mode articles:
Data_Bef_Aft_Covid_T <- Data_Bef_Aft_Covid_T %>% filter(Article_day %in% Robust_Articles_T)

T_LQ_Data_Study_4 <- Data_Bef_Aft_Covid_T %>% filter(Article == "1" | Article == "2" | Article == "3")
T_Mainstream_Data_Study_4 <- Data_Bef_Aft_Covid_T %>% filter(Article == "4" | Article == "5")

All_T_Study_4 <- rbind(T_LQ_Data_Study_4,T_Mainstream_Data_Study_4)


#Run OLS Model with clustered standard errors:
lin_results_fit_4_1_1 = feols(True_Dummy ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = All_T_Study_4)
#Produce confidence intervals with clustered standard errors:
CI_4_1_1 <- confint(lin_results_fit_4_1_1,se='twoway')


#Data from False/Misleading Articles from Study 4:
Study_4_False_M <- read.csv('./Data/Study_4_df_FM.csv')

#Filter only robust mode articles:
Study_4_False_M <- Study_4_False_M %>% filter(Article_day %in% Robust_Articles)


#Run OLS Model with clustered standard errors:
lin_results_fit_4_3_1 = feols(Susc_FN ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Study_4_False_M)
#Produce confidence intervals with clustered standard errors:
CI_4_3_1 <- confint(lin_results_fit_4_3_1,se='twoway')


#Study 5 Data:
#Create Treatment Data:
#Study 5 (True Articles):
T_Data_Study_5 <- read.csv('./Data/Study_5_df_T.csv')

#Filter only robust mode articles:
T_Data_Study_5 <- T_Data_Study_5 %>% filter(Article_day %in% Robust_Articles_T)

T_LQ_Data_Study_5 <- T_Data_Study_5 %>% filter(Article_Num == 1 | Article_Num == 2 | Article_Num == 3)
T_Mainstream_Data_Study_5 <- T_Data_Study_5 %>% filter(Article_Num == 4 | Article_Num == 5)

All_T_Study_5 <- rbind(T_LQ_Data_Study_5,T_Mainstream_Data_Study_5)


#Run OLS Model with clustered standard errors:
lin_results_fit_5_1_1 = feols(True_Dummy ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = All_T_Study_5)
#Produce confidence intervals with clustered standard errors:
CI_5_1_1 <- confint(lin_results_fit_5_1_1,se='twoway')



#Study 5 (False/Misleading Articles):
Study_5_False_M <- read.csv('./Data/Study_5_df_FM.csv')

#Filter only robust mode articles:
Study_5_False_M <- Study_5_False_M %>% filter(Article_day %in% Robust_Articles)

#Run OLS Model with clustered standard errors:
lin_results_fit_5_3_1 = feols(True_Dummy ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Study_5_False_M)
#Produce confidence intervals with clustered standard errors:
CI_5_3_1 <- confint(lin_results_fit_5_3_1,se='twoway')


#Create list for figure of Study numbers
Study <- c('True',
           'False/Misleading',
           'True',
           'False/Misleading',
           'True',  
           'False/Misleading',
           'True',  
           'False/Misleading',
           'True',  
           'False/Misleading')

#List of coefficient names in matrix:
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

Coefficients <- c(lin_results_fit_1_1_1$coefficients[1],
                  lin_results_fit_1_3_1$coefficients[1],
                  lin_results_fit_2_1_1$coefficients[1],
                  lin_results_fit_2_3_1$coefficients[1],
                  lin_results_fit_3_1_1$coefficients[1],
                  lin_results_fit_3_3_1$coefficients[1],
                  lin_results_fit_4_1_1$coefficients[1],
                  lin_results_fit_4_3_1$coefficients[1],
                  lin_results_fit_5_1_1$coefficients[1],
                  lin_results_fit_5_3_1$coefficients[1])


#Lists upper confidence interval (95%)
CI_Upper <- c(CI_1_1_1[1,2],
              CI_1_3_1[1,2],
              CI_2_1_1[1,2],
              CI_2_3_1[1,2],
              CI_3_1_1[1,2],
              CI_3_3_1[1,2],
              CI_4_1_1[1,2],
              CI_4_3_1[1,2],
              CI_5_1_1[1,2],
              CI_5_3_1[1,2])


#Lists lower confidence interval (95%)
CI_Lower <- c(CI_1_1_1[1,1],
              CI_1_3_1[1,1],
              CI_2_1_1[1,1],
              CI_2_3_1[1,1],
              CI_3_1_1[1,1],
              CI_3_3_1[1,1],
              CI_4_1_1[1,1],
              CI_4_3_1[1,1],
              CI_5_1_1[1,1],
              CI_5_3_1[1,1])

#Create matrix with lists:
d_matrix <- cbind(Study,Coef_names,Coefficients,CI_Upper,CI_Lower)
#Remove row names:
rownames(d_matrix) <- c()

#Transform matrix into data frame:
d_matrix <- data.frame(d_matrix)

#Ensure coeffisient and confidence interval variables are numeric:
d_matrix$Coefficients <- as.character(d_matrix$Coefficients)
d_matrix$CI_Lower <- as.character(d_matrix$CI_Lower)
d_matrix$CI_Upper <- as.character(d_matrix$CI_Upper)
d_matrix$Coefficients <- as.numeric(d_matrix$Coefficients)
d_matrix$CI_Lower <- as.numeric(d_matrix$CI_Lower)
d_matrix$CI_Upper <- as.numeric(d_matrix$CI_Upper)

#Transform coefficient names into a factor
d_matrix$Coef_names <- factor(d_matrix$Coef_names,levels=c('Study 1',
                                                           'Study 2',
                                                           'Study 3',
                                                           'Study 4',
                                                           'Study 5'))

#Arrange dataframe by coefficient names
d_matrix <- d_matrix %>% arrange(Coef_names)

#Create order in figure:
d_matrix$x <- c(5.1,4.9,4.1,3.9,3.1,2.9,2.1,1.9,1.1,0.9)

#Produce figure:
ggplot(data = d_matrix, aes(x = x, y = Coefficients)) +
  geom_hline(aes(yintercept = 0), color = "gray",
             linetype = 2, size = 1.2) +
  geom_point(aes(color = Study, shape=Study),size=3.5) +
  geom_linerange(aes(min = CI_Lower, 
                     max = CI_Upper, 
                     color = Study),
                 size=2) +
  scale_color_manual(values=c('red','black','blue'), name = "Type of News") +
  scale_shape_manual(values=c(15,16,17),"Type of News") +
  ylab("\n The Effect of Searching Online\n on Likelihood of Rating News as True") +
  theme_classic() +
  theme(axis.title.x = element_text(size=18),
        axis.text.x  = element_text(size=16),
        axis.title.y = element_text(size=18),
        axis.text.y  = element_text(size=16),
        plot.title = element_text(size = 18),
        legend.title = element_text(size=16),
        legend.text = element_text(size=14),
        title =element_text(size=20, face='bold')) +
  ylim(-0.1,0.2) +
  scale_x_continuous("Study Number\n",breaks=c(1,2,3,4,5),labels=rev(c('Study 1',
                                                                       'Study 2',
                                                                       'Study 3',
                                                                       'Study 4',
                                                                       'Study 5')),limits=c(0.2,5.8)) +
  coord_flip()


ggsave('./Figures/T_FM_Fig_1A_True_Dummy_ROBUST.png',height=12,width=10)



###############################################################################################

##################################### Figure 6B ###############################################

###############################################################################################


############## 
############## Study 1:
############## 

#Run OLS Model with clustered standard errors:
lin_results_fit_1_1_1 = feols(True_Dummy ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = T_LQ_Data_Study_1)
#Produce confidence intervals with clustered standard errors:
CI_1_1_1 <- confint(lin_results_fit_1_1_1,se='twoway')

#Run OLS Model with clustered standard errors:
lin_results_fit_1_2_1 = feols(True_Dummy ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = T_Mainstream_Data_Study_1)
#Produce confidence intervals with clustered standard errors:
CI_1_2_1 <- confint(lin_results_fit_1_2_1,se='twoway')


#Run OLS Model with clustered standard errors:
lin_results_fit_1_1_2 = feols(Likert_Evaluation ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = T_LQ_Data_Study_1)
#Produce confidence intervals with clustered standard errors:
CI_1_1_2 <- confint(lin_results_fit_1_1_2,se='twoway')

#Run OLS Model with clustered standard errors:
lin_results_fit_1_2_2 = feols(Likert_Evaluation ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = T_Mainstream_Data_Study_1)
#Produce confidence intervals with clustered standard errors:
CI_1_2_2 <- confint(lin_results_fit_1_2_2,se='twoway')


#Run OLS Model with clustered standard errors:
lin_results_fit_1_3_1 = feols(Susc_FN ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Misl_False_Search_MF)
#Produce confidence intervals with clustered standard errors:
CI_1_3_1 <- confint(lin_results_fit_1_3_1,se='twoway')

#Run OLS Model with clustered standard errors:
lin_results_fit_1_3_2 = feols(Likert_Evaluation ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Misl_False_Search_MF)
#Produce confidence intervals with clustered standard errors:
CI_1_3_2 <- confint(lin_results_fit_1_3_2,se='twoway')



############## 
############## Study 2:
############## 

#Run OLS Model with clustered standard errors:
lin_results_fit_2_1_1 = feols(True_Dummy ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = T_LQ_Data_Study_2)
#Produce confidence intervals with clustered standard errors:
CI_2_1_1 <- confint(lin_results_fit_2_1_1,se='twoway')

#Run OLS Model with clustered standard errors:
lin_results_fit_2_2_1 = feols(True_Dummy ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = T_Mainstream_Data_Study_2)
#Produce confidence intervals with clustered standard errors:
CI_2_2_1 <- confint(lin_results_fit_2_2_1,se='twoway')

#Run OLS Model with clustered standard errors:
lin_results_fit_2_1_2 = feols(Likert_Evaluation ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = T_LQ_Data_Study_2)
#Produce confidence intervals with clustered standard errors:
CI_2_1_2 <- confint(lin_results_fit_2_1_2,se='twoway')

#Run OLS Model with clustered standard errors:
lin_results_fit_2_2_2 = feols(Likert_Evaluation ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = T_Mainstream_Data_Study_2)
#Produce confidence intervals with clustered standard errors:
CI_2_2_2 <- confint(lin_results_fit_2_2_2,se='twoway')

#Run OLS Model with clustered standard errors:
lin_results_fit_2_3_1 = feols(Susc_FN ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Data_Bef_Aft_MF)
#Produce confidence intervals with clustered standard errors:
CI_2_3_1 <- confint(lin_results_fit_2_3_1,se='twoway')

#Run OLS Model with clustered standard errors:
lin_results_fit_2_3_2 = feols(Likert_Evaluation ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Data_Bef_Aft_MF)
#Produce confidence intervals with clustered standard errors:
CI_2_3_2 <- confint(lin_results_fit_2_3_2,se='twoway')


############## 
############## Study 3:
############## 

#Run linear regression and produce coefficient values:
lin_results_fit_3_1_1 = feols(True_Dummy ~ Treatment + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = T_LQ_Data_Study_3)
#Produce confidence intervals using clustered standard errors:
CI_3_1_1 <- confint(lin_results_fit_3_1_1,se='twoway')

#Run linear regression and produce coefficient values:
lin_results_fit_3_2_1 = feols(True_Dummy ~ Treatment + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = T_Mainstream_Data_Study_3)
#Produce confidence intervals using clustered standard errors:
CI_3_2_1 <- confint(lin_results_fit_3_2_1,se='twoway')

#Run linear regression and produce coefficient values:
lin_results_fit_3_1_2 = feols(Likert_Evaluation ~ Treatment + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = T_LQ_Data_Study_3)
#Produce confidence intervals using clustered standard errors:
CI_3_1_2 <- confint(lin_results_fit_3_1_2,se='twoway')

#Run linear regression and produce coefficient values:
lin_results_fit_3_2_2 = feols(Likert_Evaluation ~ Treatment + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = T_Mainstream_Data_Study_3)
#Produce confidence intervals using clustered standard errors:
CI_3_2_2 <- confint(lin_results_fit_3_2_2,se='twoway')


#Run linear regression and produce coefficient values:
lin_results_fit_3_3_1 = feols(Susc_FN ~ Treatment + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Study_3_False_M)
#Produce confidence intervals using clustered standard errors:
CI_3_3_1 <- confint(lin_results_fit_3_3_1,se='twoway')

#Run linear regression and produce coefficient values:
lin_results_fit_3_3_2 = feols(Likert_Evaluation ~ Treatment + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Study_3_False_M)
#Produce confidence intervals using clustered standard errors:
CI_3_3_2 <- confint(lin_results_fit_3_3_2,se='twoway')


############## 
############## Study 4:
############## 

#Run OLS Model with clustered standard errors:
lin_results_fit_4_1_1 = feols(True_Dummy ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = T_LQ_Data_Study_4)
#Produce confidence intervals with clustered standard errors:
CI_4_1_1 <- confint(lin_results_fit_4_1_1,se='twoway')

#Run OLS Model with clustered standard errors:
lin_results_fit_4_2_1 = feols(True_Dummy ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = T_Mainstream_Data_Study_4)
#Produce confidence intervals with clustered standard errors:
CI_4_2_1 <- confint(lin_results_fit_4_2_1,se='twoway')

#Run OLS Model with clustered standard errors:
lin_results_fit_4_1_2 = feols(Likert_Evaluation ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = T_LQ_Data_Study_4)
#Produce confidence intervals with clustered standard errors:
CI_4_1_2 <- confint(lin_results_fit_4_1_2,se='twoway')

#Run OLS Model with clustered standard errors:
lin_results_fit_4_2_2 = feols(Likert_Evaluation ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = T_Mainstream_Data_Study_4)
#Produce confidence intervals with clustered standard errors:
CI_4_2_2 <- confint(lin_results_fit_4_2_2,se='twoway')


#Run OLS Model with clustered standard errors:
lin_results_fit_4_3_1 = feols(Susc_FN ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Study_4_False_M)
#Produce confidence intervals with clustered standard errors:
CI_4_3_1 <- confint(lin_results_fit_4_3_1,se='twoway')

#Run OLS Model with clustered standard errors:
lin_results_fit_4_3_2 = feols(Likert_Evaluation ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Study_4_False_M)
#Produce confidence intervals with clustered standard errors:
CI_4_3_2 <- confint(lin_results_fit_4_3_2,se='twoway')

############## 
############## Study 5:
############## 

#Run OLS Model with clustered standard errors:
lin_results_fit_5_1_1 = feols(True_Dummy ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = T_LQ_Data_Study_5)
#Produce confidence intervals with clustered standard errors:
CI_5_1_1 <- confint(lin_results_fit_5_1_1,se='twoway')

#Run OLS Model with clustered standard errors:
lin_results_fit_5_2_1 = feols(True_Dummy ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = T_Mainstream_Data_Study_5)
#Produce confidence intervals with clustered standard errors:
CI_5_2_1 <- confint(lin_results_fit_5_2_1,se='twoway')

#Run OLS Model with clustered standard errors:
lin_results_fit_5_1_2 = feols(Seven_Ordinal ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = T_LQ_Data_Study_5)
#Produce confidence intervals with clustered standard errors:
CI_5_1_2 <- confint(lin_results_fit_5_1_2,se='twoway')

#Run OLS Model with clustered standard errors:
lin_results_fit_5_2_2 = feols(Seven_Ordinal ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = T_Mainstream_Data_Study_5)
#Produce confidence intervals with clustered standard errors:
CI_5_2_2 <- confint(lin_results_fit_5_2_2,se='twoway')

#Run OLS Model with clustered standard errors:
lin_results_fit_5_1_3 = feols(Four_Ordinal ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = T_LQ_Data_Study_5)
#Produce confidence intervals with clustered standard errors:
CI_5_1_3 <- confint(lin_results_fit_5_1_3,se='twoway')

#Run OLS Model with clustered standard errors:
lin_results_fit_5_2_3 = feols(Four_Ordinal ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = T_Mainstream_Data_Study_5)
#Produce confidence intervals with clustered standard errors:
CI_5_2_3 <- confint(lin_results_fit_5_2_3,se='twoway')

#Run OLS Model with clustered standard errors:
lin_results_fit_5_3_1 = feols(True_Dummy ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Study_5_False_M)
#Produce confidence intervals with clustered standard errors:
CI_5_3_1 <- confint(lin_results_fit_5_3_1,se='twoway')

#Run OLS Model with clustered standard errors:
lin_results_fit_5_3_2 = feols(Seven_Ordinal ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Study_5_False_M)
#Produce confidence intervals with clustered standard errors:
CI_5_3_2 <- confint(lin_results_fit_5_3_2,se='twoway')

#Run OLS Model with clustered standard errors:
lin_results_fit_5_3_3 = feols(Four_Ordinal ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Study_5_False_M)
#Produce confidence intervals with clustered standard errors:
CI_5_3_3 <- confint(lin_results_fit_5_3_3,se='twoway')




#Create list for figure of Study numbers
Study <- c('True - Low Quality',
           'True - Mainstream',  
           'False/Misleading',
           'True - Low Quality',
           'True - Mainstream',  
           'False/Misleading',
           'True - Low Quality',
           'True - Mainstream',  
           'False/Misleading',
           'True - Low Quality',
           'True - Mainstream',  
           'False/Misleading',
           'True - Low Quality',
           'True - Mainstream',  
           'False/Misleading')

#List of coefficient names in matrix:
Coef_names <- c('Study 1',
                'Study 1',
                'Study 1',
                'Study 2',
                'Study 2',
                'Study 2',
                'Study 3',
                'Study 3',
                'Study 3',
                'Study 4',
                'Study 4',
                'Study 4',
                'Study 5',
                'Study 5',
                'Study 5')

Coefficients <- c(lin_results_fit_1_1_1$coefficients[1],
                  lin_results_fit_1_2_1$coefficients[1],
                  lin_results_fit_1_3_1$coefficients[1],
                  lin_results_fit_2_1_1$coefficients[1],
                  lin_results_fit_2_2_1$coefficients[1],
                  lin_results_fit_2_3_1$coefficients[1],
                  lin_results_fit_3_1_1$coefficients[1],
                  lin_results_fit_3_2_1$coefficients[1],
                  lin_results_fit_3_3_1$coefficients[1],
                  lin_results_fit_4_1_1$coefficients[1],
                  lin_results_fit_4_2_1$coefficients[1],
                  lin_results_fit_4_3_1$coefficients[1],
                  lin_results_fit_5_1_1$coefficients[1],
                  lin_results_fit_5_2_1$coefficients[1],
                  lin_results_fit_5_3_1$coefficients[1])


#Lists upper confidence interval (95%)
CI_Upper <- c(CI_1_1_1[1,2],
              CI_1_2_1[1,2],
              CI_1_3_1[1,2],
              CI_2_1_1[1,2],
              CI_2_2_1[1,2],
              CI_2_3_1[1,2],
              CI_3_1_1[1,2],
              CI_3_2_1[1,2],
              CI_3_3_1[1,2],
              CI_4_1_1[1,2],
              CI_4_2_1[1,2],
              CI_4_3_1[1,2],
              CI_5_1_1[1,2],
              CI_5_2_1[1,2],
              CI_5_3_1[1,2])


#Lists lower confidence interval (95%)
CI_Lower <- c(CI_1_1_1[1,1],
              CI_1_2_1[1,1],
              CI_1_3_1[1,1],
              CI_2_1_1[1,1],
              CI_2_2_1[1,1],
              CI_2_3_1[1,1],
              CI_3_1_1[1,1],
              CI_3_2_1[1,1],
              CI_3_3_1[1,1],
              CI_4_1_1[1,1],
              CI_4_2_1[1,1],
              CI_4_3_1[1,1],
              CI_5_1_1[1,1],
              CI_5_2_1[1,1],
              CI_5_3_1[1,1])

#Create matrix with lists:
d_matrix <- cbind(Study,Coef_names,Coefficients,CI_Upper,CI_Lower)
#Remove row names:
rownames(d_matrix) <- c()

#Transform matrix into data frame:
d_matrix <- data.frame(d_matrix)

#Ensure coeffisient and confidence interval variables are numeric:
d_matrix$Coefficients <- as.character(d_matrix$Coefficients)
d_matrix$CI_Lower <- as.character(d_matrix$CI_Lower)
d_matrix$CI_Upper <- as.character(d_matrix$CI_Upper)
d_matrix$Coefficients <- as.numeric(d_matrix$Coefficients)
d_matrix$CI_Lower <- as.numeric(d_matrix$CI_Lower)
d_matrix$CI_Upper <- as.numeric(d_matrix$CI_Upper)

#Transform coefficient names into a factor
d_matrix$Coef_names <- factor(d_matrix$Coef_names,levels=c('Study 1',
                                                           'Study 2',
                                                           'Study 3',
                                                           'Study 4'))

#Arrange dataframe by coefficient names
d_matrix <- d_matrix %>% arrange(Coef_names)

#Create order in figure:
d_matrix$x <- c(5.2,5.0,4.8,4.2,4.0,3.8,3.2,3.0,2.8,2.2,2.0,1.8,1.2,1.0,0.8)

#Produce figure:
ggplot(data = d_matrix, aes(x = x, y = Coefficients)) +
  geom_hline(aes(yintercept = 0), color = "gray",
             linetype = 2, size = 1.2) +
  geom_point(aes(color = Study, shape=Study),size=3.5) +
  geom_linerange(aes(min = CI_Lower, 
                     max = CI_Upper, 
                     color = Study),
                 size=2) +
  scale_color_manual(values=c('red','black','blue'), name = "Type of News") +
  scale_shape_manual(values=c(15,16,17),"Type of News") +
  ylab("\n The Effect of Searching Online\n on Likelihood of Rating News as True") +
  theme_classic() +
  theme(axis.title.x = element_text(size=18),
        axis.text.x  = element_text(size=16),
        axis.title.y = element_text(size=18),
        axis.text.y  = element_text(size=16),
        plot.title = element_text(size = 18),
        legend.title = element_text(size=16),
        legend.text = element_text(size=14),
        title =element_text(size=20, face='bold')) +
  ylim(-0.1,0.35) +
  scale_x_continuous("Study Number\n",breaks=c(1,2,3,4,5),labels=rev(c('Study 1',
                                                                       'Study 2',
                                                                       'Study 3',
                                                                       'Study 4',
                                                                       'Study 5')),limits=c(0.2,5.8)) +
  coord_flip()


ggsave('./Figures/Fig_1A_True_Dummy_ROBUST.png',height=12,width=10)


###############################################################################################

##################################### Figure 6C ###############################################

###############################################################################################

#Between-Respondent Experiment True Low Quality Articles:

T_LQ_Data_Study_1 <- T_LQ_Data_Study_1 %>% select(True_Dummy,Likert_Evaluation,Treat_Search,Age,Dummy_Congruence,Education_Score,Gender,Income_Score,Article_day,Article_day,ResponseId)
T_LQ_Data_Study_5 <- T_LQ_Data_Study_5 %>% select(True_Dummy,Seven_Ordinal,Treatment,Age,Ideo_Congruence,Education_Score,Gender,Income_Score,Article_day,Article_day,ResponseId)

T_LQ_Data_Study_5$Likert_Evaluation <- T_LQ_Data_Study_5$Seven_Ordinal
T_LQ_Data_Study_5$Treat_Search <- T_LQ_Data_Study_5$Treatment
T_LQ_Data_Study_5$Dummy_Congruence <- T_LQ_Data_Study_5$Ideo_Congruence

T_LQ_Data_Study_5$Seven_Ordinal <- NULL
T_LQ_Data_Study_5$Treatment <- NULL
T_LQ_Data_Study_5$Ideo_Congruence <- NULL


btwn_T_LQ_studies <- rbind(T_LQ_Data_Study_1,T_LQ_Data_Study_5)

#Run OLS Model with clustered standard errors:
btwn_T_LQ_results_1 = feols(True_Dummy ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = btwn_T_LQ_studies)
#Produce confidence intervals with clustered standard errors:
CI_btwn_T_LQ_1 <- confint(btwn_T_LQ_results_1,se='twoway')

#Within-Respondent Experiment True Low Quality Articles:


T_LQ_Data_Study_2 <- T_LQ_Data_Study_2 %>% select(True_Dummy,Likert_Evaluation,Treat_Search,Age,Dummy_Congruence,Education_Score,Gender,Income_Score,Article_day,ResponseId)
T_LQ_Data_Study_3 <- T_LQ_Data_Study_3 %>% select(True_Dummy,Likert_Evaluation,Treat_Search,Age,Dummy_Congruence,Education_Score,Gender,Income_Score,Article_day,ResponseId)
T_LQ_Data_Study_4 <- T_LQ_Data_Study_4 %>% select(True_Dummy,Likert_Evaluation,Treat_Search,Age,Dummy_Congruence,Education_Score,Gender,Income_Score,Article_day,ResponseId)

wthn_T_LQ_studies <- rbind(T_LQ_Data_Study_2,T_LQ_Data_Study_3,T_LQ_Data_Study_4)

#Run OLS Model with clustered standard errors:
wthn_T_LQ_results_1 = feols(True_Dummy ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = wthn_T_LQ_studies)
#Produce confidence intervals with clustered standard errors:
CI_wthn_T_LQ_1 <- confint(wthn_T_LQ_results_1,se='twoway')

#Run OLS Model with clustered standard errors:
wthn_T_LQ_results_2 = feols(Likert_Evaluation ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = wthn_T_LQ_studies)
#Produce confidence intervals with clustered standard errors:
CI_wthn_T_LQ_2 <- confint(wthn_T_LQ_results_2,se='twoway')


#Between-Respondent Experiment True Mainstream Articles:

T_Mainstream_Data_Study_1 <- T_Mainstream_Data_Study_1 %>% select(True_Dummy,Likert_Evaluation,Treat_Search,Age,Dummy_Congruence,Education_Score,Gender,Income_Score,Article_day,Article_day,ResponseId)
T_Mainstream_Data_Study_5 <- T_Mainstream_Data_Study_5 %>% select(True_Dummy,Seven_Ordinal,Treatment,Age,Ideo_Congruence,Education_Score,Gender,Income_Score,Article_day,Article_day,ResponseId)

T_Mainstream_Data_Study_5$Likert_Evaluation <- T_Mainstream_Data_Study_5$Seven_Ordinal
T_Mainstream_Data_Study_5$Treat_Search <- T_Mainstream_Data_Study_5$Treatment
T_Mainstream_Data_Study_5$Dummy_Congruence <- T_Mainstream_Data_Study_5$Ideo_Congruence

T_Mainstream_Data_Study_5$Seven_Ordinal <- NULL
T_Mainstream_Data_Study_5$Treatment <- NULL
T_Mainstream_Data_Study_5$Ideo_Congruence <- NULL

btwn_T_M_studies <- rbind(T_Mainstream_Data_Study_1,T_Mainstream_Data_Study_5)

#Run OLS Model with clustered standard errors:
btwn_T_M_results_1 = feols(True_Dummy ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = btwn_T_M_studies)
#Produce confidence intervals with clustered standard errors:
CI_btwn_T_M_1 <- confint(btwn_T_M_results_1,se='twoway')

#Run OLS Model with clustered standard errors:
btwn_T_M_results_2 = feols(Likert_Evaluation ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = btwn_T_M_studies)
#Produce confidence intervals with clustered standard errors:
CI_btwn_T_M_2 <- confint(btwn_T_M_results_2,se='twoway')


#Within-Respondent Experiment True Mainstream Articles:


T_Mainstream_Data_Study_2 <- T_Mainstream_Data_Study_2 %>% select(True_Dummy,Likert_Evaluation,Treat_Search,Age,Dummy_Congruence,Education_Score,Gender,Income_Score,Article_day,ResponseId)
T_Mainstream_Data_Study_3 <- T_Mainstream_Data_Study_3 %>% select(True_Dummy,Likert_Evaluation,Treat_Search,Age,Dummy_Congruence,Education_Score,Gender,Income_Score,Article_day,ResponseId)
T_Mainstream_Data_Study_4 <- T_Mainstream_Data_Study_4 %>% select(True_Dummy,Likert_Evaluation,Treat_Search,Age,Dummy_Congruence,Education_Score,Gender,Income_Score,Article_day,ResponseId)

wthn_T_M_studies <- rbind(T_Mainstream_Data_Study_2,T_Mainstream_Data_Study_3,T_Mainstream_Data_Study_4)

#Run OLS Model with clustered standard errors:
wthn_T_M_results_1 = feols(True_Dummy ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = wthn_T_M_studies)
#Produce confidence intervals with clustered standard errors:
CI_wthn_T_M_1 <- confint(wthn_T_M_results_1,se='twoway')

#Run OLS Model with clustered standard errors:
wthn_T_M_results_2 = feols(Likert_Evaluation ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = wthn_T_M_studies)
#Produce confidence intervals with clustered standard errors:
CI_wthn_T_M_2 <- confint(wthn_T_M_results_2,se='twoway')


#Within-Respondent Experiment False/Misleading Articles:
Data_Bef_Aft_MF <- Data_Bef_Aft_MF %>% select(Susc_FN,Likert_Evaluation,Treat_Search,Age,Dummy_Congruence,Education_Score,Gender,Income_Score,Article_day,ResponseId)
Study_3_False_M <- Study_3_False_M %>% select(True_Dummy,Likert_Evaluation,Treat_Search,Age,Dummy_Congruence,Education_Score,Gender,Income_Score,Article_day,ResponseId)
Study_4_False_M <- Study_4_False_M %>% select(Susc_FN,Likert_Evaluation,Treat_Search,Age,Dummy_Congruence,Education_Score,Gender,Income_Score,Article_day,ResponseId)

colnames(Data_Bef_Aft_MF)[1] <- 'True_Dummy'
colnames(Study_4_False_M)[1] <- 'True_Dummy'

Within_FM_Study <- rbind(Data_Bef_Aft_MF,Study_3_False_M,Study_4_False_M)


#Run OLS Model with clustered standard errors:
wthn_FM_results_1 = feols(True_Dummy ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Within_FM_Study)
#Produce confidence intervals with clustered standard errors:
CI_wthn_FM_1 <- confint(wthn_FM_results_1,se='twoway')

#Run OLS Model with clustered standard errors:
wthn_FM_results_2 = feols(Likert_Evaluation ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Within_FM_Study)
#Produce confidence intervals with clustered standard errors:
CI_wthn_FM_2 <- confint(wthn_FM_results_2,se='twoway')



#Between-Respondent Experiment False/Misleading Articles:

Misl_False_Search_MF <- Misl_False_Search_MF %>% select(Susc_FN,Likert_Evaluation,Treat_Search,Age,Dummy_Congruence,Education_Score,Gender,Income_Score,Article_day,Article_day,ResponseId)
Study_5_False_M <- Study_5_False_M %>% select(True_Dummy,Seven_Ordinal,Treatment,Age,Ideo_Congruence,Education_Score,Gender,Income_Score,Article_day,Article_day,ResponseId)

Study_5_False_M$Likert_Evaluation <- Study_5_False_M$Seven_Ordinal
Study_5_False_M$Treat_Search <- Study_5_False_M$Treatment
Study_5_False_M$Dummy_Congruence <- Study_5_False_M$Ideo_Congruence

Study_5_False_M$Seven_Ordinal <- NULL
Study_5_False_M$Treatment <- NULL
Study_5_False_M$Ideo_Congruence <- NULL

colnames(Misl_False_Search_MF)[1] <- 'True_Dummy'

Between_FM_Study <- rbind(Misl_False_Search_MF,Study_5_False_M)



#Run OLS Model with clustered standard errors:
btwn_FM_results_1 = feols(True_Dummy ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Between_FM_Study)
#Produce confidence intervals with clustered standard errors:
CI_btwn_FM_1 <- confint(btwn_FM_results_1,se='twoway')

#Run OLS Model with clustered standard errors:
btwn_FM_results_2 = feols(Likert_Evaluation ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Between_FM_Study)
#Produce confidence intervals with clustered standard errors:
CI_btwn_FM_2 <- confint(btwn_FM_results_2,se='twoway')





#Create list for figure of Study numbers
Study <- c('True - Low Quality',
           'True - Mainstream',
           'False/Misleading',
           'True - Low Quality',
           'True - Mainstream',
           'False/Misleading')

#List of coefficient names in matrix:
Coef_names <- c('Between',
                'Between',
                'Between',
                'Within',
                'Within',
                'Within')

Coefficients <- c(btwn_T_LQ_results_1$coefficients[1],
                  btwn_T_M_results_1$coefficients[1],
                  btwn_FM_results_1$coefficients[1],
                  wthn_T_LQ_results_1$coefficients[1],
                  wthn_T_M_results_1$coefficients[1],
                  wthn_FM_results_1$coefficients[1])


#Lists upper confidence interval (95%)
CI_Upper <- c(CI_btwn_T_LQ_1[1,2],
              CI_btwn_T_M_1[1,2],
              CI_btwn_FM_1[1,2],
              CI_wthn_T_LQ_1[1,2],
              CI_wthn_T_M_1[1,2],
              CI_wthn_FM_1[1,2])


#Lists lower confidence interval (95%)
CI_Lower <- c(CI_btwn_T_LQ_1[1,1],
              CI_btwn_T_M_1[1,1],
              CI_btwn_FM_1[1,1],
              CI_wthn_T_LQ_1[1,1],
              CI_wthn_T_M_1[1,1],
              CI_wthn_FM_1[1,1])


#Create matrix with lists:
d_matrix <- cbind(Study,Coef_names,Coefficients,CI_Upper,CI_Lower)
#Remove row names:
rownames(d_matrix) <- c()

#Transform matrix into data frame:
d_matrix <- data.frame(d_matrix)

#Ensure coeffisient and confidence interval variables are numeric:
d_matrix$Coefficients <- as.character(d_matrix$Coefficients)
d_matrix$CI_Lower <- as.character(d_matrix$CI_Lower)
d_matrix$CI_Upper <- as.character(d_matrix$CI_Upper)
d_matrix$Coefficients <- as.numeric(d_matrix$Coefficients)
d_matrix$CI_Lower <- as.numeric(d_matrix$CI_Lower)
d_matrix$CI_Upper <- as.numeric(d_matrix$CI_Upper)



#Transform coefficient names into a factor
d_matrix$Coef_names <- factor(d_matrix$Coef_names,levels=c('Between',
                                                           'Within'))

#Arrange dataframe by coefficient names
d_matrix <- d_matrix %>% arrange(Coef_names)

#Create order in figure:
d_matrix$x <- c(2.2,2,1.8,1.2,1,0.8)

#Produce figure:
ggplot(data = d_matrix, aes(x = x, y = Coefficients)) +
  geom_hline(aes(yintercept = 0), color = "gray",
             linetype = 2, size = 1.2) +
  geom_point(aes(color = Study, shape=Study),size=3.5) +
  geom_linerange(aes(min = CI_Lower, 
                     max = CI_Upper, 
                     color = Study),
                 size=2) +
  scale_color_manual(values=c('red','black','blue'), name = "Type of News") +
  scale_shape_manual(values=c(15,16,17),"Type of News") +
  ylab("\n The Effect of Searching Online\n on Likelihood of Rating News as True") +
  theme_classic() +
  theme(axis.title.x = element_text(size=18),
        axis.text.x  = element_text(size=16),
        axis.title.y = element_text(size=18),
        axis.text.y  = element_text(size=16),
        plot.title = element_text(size = 18),
        legend.title = element_text(size=16),
        legend.text = element_text(size=14),
        title =element_text(size=20, face='bold')) +
  ylim(-0.1,0.25) +
  scale_x_continuous("Study Type\n",breaks=c(1,2),labels=rev(c('Between',
                                                               'Within')),limits=c(0,3)) +
  coord_flip()


ggsave('./Figures/Types_Fig_1A_True_Dummy_ROBUST.png',height=12,width=10)


######################################################################################

############################# Figures with Ordinal Scale #############################

######################################################################################

#Study 1:
#Read in data:
Study_1_df_T <- read.csv('./Data/Study_1_df_T.csv')

T_LQ_Data_Study_1 <- Study_1_df_T %>% filter(Article == "1" | Article == "2" | Article == "3")
T_Mainstream_Data_Study_1 <- Study_1_df_T %>% filter(Article == "4" | Article == "5")

All_T_Study_1 <- rbind(T_LQ_Data_Study_1,T_Mainstream_Data_Study_1)

#Run OLS Model with clustered standard errors:
lin_results_fit_1_1_2 = feols(Likert_Evaluation ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = All_T_Study_1)
#Produce confidence intervals with clustered standard errors:
CI_1_1_2 <- confint(lin_results_fit_1_1_2,se='twoway')


#Read in data (Study 1 - false/misleading articles):
Misl_False_Search_MF <- read.csv('./Data/Study_1_df_FM.csv')

#Run OLS Model with clustered standard errors:
lin_results_fit_1_3_2 = feols(Likert_Evaluation ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Misl_False_Search_MF)
#Produce confidence intervals with clustered standard errors:
CI_1_3_2 <- confint(lin_results_fit_1_3_2,se='twoway')



#Study 2:
#Read in data (Study 2 - true articles):
Data_Bef_Aft_T <- read.csv('./Data/Study_2_df_T.csv')

T_LQ_Data_Study_2 <- Data_Bef_Aft_T %>% filter(Article == "1" | Article == "2" | Article == "3")
T_Mainstream_Data_Study_2 <- Data_Bef_Aft_T %>% filter(Article == "4" | Article == "5")

All_T_Study_2 <- rbind(T_LQ_Data_Study_2,T_Mainstream_Data_Study_2)

#Run OLS Model with clustered standard errors:
lin_results_fit_2_1_2 = feols(Likert_Evaluation ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = All_T_Study_2)
#Produce confidence intervals with clustered standard errors:
CI_2_1_2 <- confint(lin_results_fit_2_1_2,se='twoway')


#Pull in this data: Study 2 - False/Misleading Articles:
Data_Bef_Aft_MF <- read.csv('./Data/Study_2_df_FM.csv')

#Run OLS Model with clustered standard errors:
lin_results_fit_2_3_2 = feols(Likert_Evaluation ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Data_Bef_Aft_MF)
#Produce confidence intervals with clustered standard errors:
CI_2_3_2 <- confint(lin_results_fit_2_3_2,se='twoway')


#Study 3 (True Articles):
Study_3_df <- read.csv('./Data/Study_3_df_T.csv')

T_LQ_Data_Study_3 <- Study_3_df %>% filter(Article == "1" | Article == "2" | Article == "3")
T_Mainstream_Data_Study_3 <- Study_3_df %>% filter(Article == "4" | Article == "5")
All_T_Study_3 <- rbind(T_LQ_Data_Study_3,T_Mainstream_Data_Study_3)

#Run linear regression and produce coefficient values:
lin_results_fit_3_1_2 = feols(Likert_Evaluation ~ Treatment + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = All_T_Study_3)
#Produce confidence intervals using clustered standard errors:
CI_3_1_2 <- confint(lin_results_fit_3_1_2,se='twoway')

#Study 3 Analysis:
Study_3_False_M <- read.csv('./Data/Study_3_df_FM.csv')

#Run linear regression and produce coefficient values:
lin_results_fit_3_3_2 = feols(Likert_Evaluation ~ Treatment + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Study_3_False_M)
#Produce confidence intervals using clustered standard errors:
CI_3_3_2 <- confint(lin_results_fit_3_3_2,se='twoway')


#Study 4:
Data_Bef_Aft_Covid_T <- read.csv('./Data/Study_4_df_T.csv')
T_LQ_Data_Study_4 <- Data_Bef_Aft_Covid_T %>% filter(Article == "1" | Article == "2" | Article == "3")
T_Mainstream_Data_Study_4 <- Data_Bef_Aft_Covid_T %>% filter(Article == "4" | Article == "5")
All_T_Study_4 <- rbind(T_LQ_Data_Study_4,T_Mainstream_Data_Study_4)

#Run OLS Model with clustered standard errors:
lin_results_fit_4_1_2 = feols(Likert_Evaluation ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = All_T_Study_4)
#Produce confidence intervals with clustered standard errors:
CI_4_1_2 <- confint(lin_results_fit_4_1_2,se='twoway')


#Study 4 (False Misleading Articles)
Study_4_False_M <- read.csv('./Data/Study_4_df_FM.csv')

#Run OLS Model with clustered standard errors:
lin_results_fit_4_3_2 = feols(Likert_Evaluation ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Study_4_False_M)
#Produce confidence intervals with clustered standard errors:
CI_4_3_2 <- confint(lin_results_fit_4_3_2,se='twoway')


#Study 5 (True Articles):
T_Data_Study_5 <- read.csv('./Data/Study_5_df_T.csv')
T_LQ_Data_Study_5 <- T_Data_Study_5 %>% filter(Article_Num == 1 | Article_Num == 2 | Article_Num == 3)
T_Mainstream_Data_Study_5 <- T_Data_Study_5 %>% filter(Article_Num == 4 | Article_Num == 5)
All_T_Study_5 <- rbind(T_LQ_Data_Study_5,T_Mainstream_Data_Study_5)


#Run OLS Model with clustered standard errors:
lin_results_fit_5_1_2 = feols(Seven_Ordinal ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = All_T_Study_5)
#Produce confidence intervals with clustered standard errors:
CI_5_1_2 <- confint(lin_results_fit_5_1_2,se='twoway')


#Study 5 (False/Misleading Articles):
Study_5_False_M <- read.csv('./Data/Study_5_df_FM.csv')

#Run OLS Model with clustered standard errors:
lin_results_fit_5_3_2 = feols(Seven_Ordinal ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Study_5_False_M)
#Produce confidence intervals with clustered standard errors:
CI_5_3_2 <- confint(lin_results_fit_5_3_2,se='twoway')

#Create list for figure of Study numbers
Study <- c('True',
           'False/Misleading',
           'True',
           'False/Misleading',
           'True',  
           'False/Misleading',
           'True',  
           'False/Misleading',
           'True',  
           'False/Misleading')

#List of coefficient names in matrix:
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

Coefficients <- c(lin_results_fit_1_1_2$coefficients[1],
                  lin_results_fit_1_3_2$coefficients[1],
                  lin_results_fit_2_1_2$coefficients[1],
                  lin_results_fit_2_3_2$coefficients[1],
                  lin_results_fit_3_1_2$coefficients[1],
                  lin_results_fit_3_3_2$coefficients[1],
                  lin_results_fit_4_1_2$coefficients[1],
                  lin_results_fit_4_3_2$coefficients[1],
                  lin_results_fit_5_1_2$coefficients[1],
                  lin_results_fit_5_3_2$coefficients[1])


#Lists upper confidence interval (95%)
CI_Upper <- c(CI_1_1_2[1,2],
              CI_1_3_2[1,2],
              CI_2_1_2[1,2],
              CI_2_3_2[1,2],
              CI_3_1_2[1,2],
              CI_3_3_2[1,2],
              CI_4_1_2[1,2],
              CI_4_3_2[1,2],
              CI_5_1_2[1,2],
              CI_5_3_2[1,2])


#Lists lower confidence interval (95%)
CI_Lower <- c(CI_1_1_2[1,1],
              CI_1_3_2[1,1],
              CI_2_1_2[1,1],
              CI_2_3_2[1,1],
              CI_3_1_2[1,1],
              CI_3_3_2[1,1],
              CI_4_1_2[1,1],
              CI_4_3_2[1,1],
              CI_5_1_2[1,1],
              CI_5_3_2[1,1])

#Create matrix with lists:
d_matrix <- cbind(Study,Coef_names,Coefficients,CI_Upper,CI_Lower)
#Remove row names:
rownames(d_matrix) <- c()

#Transform matrix into data frame:
d_matrix <- data.frame(d_matrix)

#Ensure coeffisient and confidence interval variables are numeric:
d_matrix$Coefficients <- as.character(d_matrix$Coefficients)
d_matrix$CI_Lower <- as.character(d_matrix$CI_Lower)
d_matrix$CI_Upper <- as.character(d_matrix$CI_Upper)
d_matrix$Coefficients <- as.numeric(d_matrix$Coefficients)
d_matrix$CI_Lower <- as.numeric(d_matrix$CI_Lower)
d_matrix$CI_Upper <- as.numeric(d_matrix$CI_Upper)

#Transform coefficient names into a factor
d_matrix$Coef_names <- factor(d_matrix$Coef_names,levels=c('Study 1',
                                                           'Study 2',
                                                           'Study 3',
                                                           'Study 4',
                                                           'Study 5'))

#Arrange dataframe by coefficient names
d_matrix <- d_matrix %>% arrange(Coef_names)

#Create order in figure:
d_matrix$x <- c(5.1,4.9,4.1,3.9,3.1,2.9,2.1,1.9,1.1,0.9)

#Produce figure:
ggplot(data = d_matrix, aes(x = x, y = Coefficients)) +
  geom_hline(aes(yintercept = 0), color = "gray",
             linetype = 2, size = 1.2) +
  geom_point(aes(color = Study, shape=Study),size=3.5) +
  geom_linerange(aes(min = CI_Lower, 
                     max = CI_Upper, 
                     color = Study),
                 size=2) +
  scale_color_manual(values=c('red','black','blue'), name = "Type of News") +
  scale_shape_manual(values=c(15,16,17),"Type of News") +
  ylab("\n The Effect of Searching Online\n on Likelihood of Rating News as True") +
  theme_classic() +
  theme(axis.title.x = element_text(size=18),
        axis.text.x  = element_text(size=16),
        axis.title.y = element_text(size=18),
        axis.text.y  = element_text(size=16),
        plot.title = element_text(size = 18),
        legend.title = element_text(size=16),
        legend.text = element_text(size=14),
        title =element_text(size=20, face='bold')) +
  ylim(-0.1,0.7) +
  scale_x_continuous("Study Number\n",breaks=c(1,2,3,4,5),labels=rev(c('Study 1',
                                                                       'Study 2',
                                                                       'Study 3',
                                                                       'Study 4',
                                                                       'Study 5')),limits=c(0.2,5.8)) +
  coord_flip()


ggsave('./Figures/T_FM_Fig_1A_True_Ordinal.png',height=12,width=10)



################################################################################################################

################################# Figure 4b: Fig_1A_True_Dummy.png #############################################

################################################################################################################

############## 
############## Study 1:
############## 

#Run OLS Model with clustered standard errors:
lin_results_fit_1_1_2 = feols(Likert_Evaluation ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = T_LQ_Data_Study_1)
#Produce confidence intervals with clustered standard errors:
CI_1_1_2 <- confint(lin_results_fit_1_1_2,se='twoway')

#Run OLS Model with clustered standard errors:
lin_results_fit_1_2_2 = feols(Likert_Evaluation ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = T_Mainstream_Data_Study_1)
#Produce confidence intervals with clustered standard errors:
CI_1_2_2 <- confint(lin_results_fit_1_2_2,se='twoway')

#Run OLS Model with clustered standard errors:
lin_results_fit_1_3_2 = feols(Likert_Evaluation ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Misl_False_Search_MF)
#Produce confidence intervals with clustered standard errors:
CI_1_3_2 <- confint(lin_results_fit_1_3_2,se='twoway')


############## 
############## Study 2:
############## 

#Run OLS Model with clustered standard errors:
lin_results_fit_2_1_2 = feols(Likert_Evaluation ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = T_LQ_Data_Study_2)
#Produce confidence intervals with clustered standard errors:
CI_2_1_2 <- confint(lin_results_fit_2_1_2,se='twoway')

#Run OLS Model with clustered standard errors:
lin_results_fit_2_2_2 = feols(Likert_Evaluation ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = T_Mainstream_Data_Study_2)
#Produce confidence intervals with clustered standard errors:
CI_2_2_2 <- confint(lin_results_fit_2_2_2,se='twoway')

#Run OLS Model with clustered standard errors:
lin_results_fit_2_3_2 = feols(Likert_Evaluation ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Data_Bef_Aft_MF)
#Produce confidence intervals with clustered standard errors:
CI_2_3_2 <- confint(lin_results_fit_2_3_2,se='twoway')


############## 
############## Study 3:
############## 

#Run linear regression and produce coefficient values:
lin_results_fit_3_1_2 = feols(Likert_Evaluation ~ Treatment + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = T_LQ_Data_Study_3)
#Produce confidence intervals using clustered standard errors:
CI_3_1_2 <- confint(lin_results_fit_3_1_2,se='twoway')

#Run linear regression and produce coefficient values:
lin_results_fit_3_2_2 = feols(Likert_Evaluation ~ Treatment + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = T_Mainstream_Data_Study_3)
#Produce confidence intervals using clustered standard errors:
CI_3_2_2 <- confint(lin_results_fit_3_2_2,se='twoway')

#Run linear regression and produce coefficient values:
lin_results_fit_3_3_2 = feols(Likert_Evaluation ~ Treatment + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Study_3_False_M)
#Produce confidence intervals using clustered standard errors:
CI_3_3_2 <- confint(lin_results_fit_3_3_2,se='twoway')


############## 
############## Study 4:
############## 

#Run OLS Model with clustered standard errors:
lin_results_fit_4_1_2 = feols(Likert_Evaluation ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = T_LQ_Data_Study_4)
#Produce confidence intervals with clustered standard errors:
CI_4_1_2 <- confint(lin_results_fit_4_1_2,se='twoway')

#Run OLS Model with clustered standard errors:
lin_results_fit_4_2_2 = feols(Likert_Evaluation ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = T_Mainstream_Data_Study_4)
#Produce confidence intervals with clustered standard errors:
CI_4_2_2 <- confint(lin_results_fit_4_2_2,se='twoway')

#Run OLS Model with clustered standard errors:
lin_results_fit_4_3_2 = feols(Likert_Evaluation ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Study_4_False_M)
#Produce confidence intervals with clustered standard errors:
CI_4_3_2 <- confint(lin_results_fit_4_3_2,se='twoway')

############## 
############## Study 5:
############## 

#Run OLS Model with clustered standard errors:
lin_results_fit_5_1_2 = feols(Seven_Ordinal ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = T_LQ_Data_Study_5)
#Produce confidence intervals with clustered standard errors:
CI_5_1_2 <- confint(lin_results_fit_5_1_2,se='twoway')

#Run OLS Model with clustered standard errors:
lin_results_fit_5_2_2 = feols(Seven_Ordinal ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = T_Mainstream_Data_Study_5)
#Produce confidence intervals with clustered standard errors:
CI_5_2_2 <- confint(lin_results_fit_5_2_2,se='twoway')

#Run OLS Model with clustered standard errors:
lin_results_fit_5_3_2 = feols(Seven_Ordinal ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Study_5_False_M)
#Produce confidence intervals with clustered standard errors:
CI_5_3_2 <- confint(lin_results_fit_5_3_2,se='twoway')





#Create list for figure of Study numbers
Study <- c('True - Low Quality',
           'True - Mainstream',  
           'False/Misleading',
           'True - Low Quality',
           'True - Mainstream',  
           'False/Misleading',
           'True - Low Quality',
           'True - Mainstream',  
           'False/Misleading',
           'True - Low Quality',
           'True - Mainstream',  
           'False/Misleading',
           'True - Low Quality',
           'True - Mainstream',  
           'False/Misleading')

#List of coefficient names in matrix:
Coef_names <- c('Study 1',
                'Study 1',
                'Study 1',
                'Study 2',
                'Study 2',
                'Study 2',
                'Study 3',
                'Study 3',
                'Study 3',
                'Study 4',
                'Study 4',
                'Study 4',
                'Study 5',
                'Study 5',
                'Study 5')

Coefficients <- c(lin_results_fit_1_1_2$coefficients[1],
                  lin_results_fit_1_2_2$coefficients[1],
                  lin_results_fit_1_3_2$coefficients[1],
                  lin_results_fit_2_1_2$coefficients[1],
                  lin_results_fit_2_2_2$coefficients[1],
                  lin_results_fit_2_3_2$coefficients[1],
                  lin_results_fit_3_1_2$coefficients[1],
                  lin_results_fit_3_2_2$coefficients[1],
                  lin_results_fit_3_3_2$coefficients[1],
                  lin_results_fit_4_1_2$coefficients[1],
                  lin_results_fit_4_2_2$coefficients[1],
                  lin_results_fit_4_3_2$coefficients[1],
                  lin_results_fit_5_1_2$coefficients[1],
                  lin_results_fit_5_2_2$coefficients[1],
                  lin_results_fit_5_3_2$coefficients[1])


#Lists upper confidence interval (95%)
CI_Upper <- c(CI_1_1_2[1,2],
              CI_1_2_2[1,2],
              CI_1_3_2[1,2],
              CI_2_1_2[1,2],
              CI_2_2_2[1,2],
              CI_2_3_2[1,2],
              CI_3_1_2[1,2],
              CI_3_2_2[1,2],
              CI_3_3_2[1,2],
              CI_4_1_2[1,2],
              CI_4_2_2[1,2],
              CI_4_3_2[1,2],
              CI_5_1_2[1,2],
              CI_5_2_2[1,2],
              CI_5_3_2[1,2])


#Lists lower confidence interval (95%)
CI_Lower <- c(CI_1_1_2[1,1],
              CI_1_2_2[1,1],
              CI_1_3_2[1,1],
              CI_2_1_2[1,1],
              CI_2_2_2[1,1],
              CI_2_3_2[1,1],
              CI_3_1_2[1,1],
              CI_3_2_2[1,1],
              CI_3_3_2[1,1],
              CI_4_1_2[1,1],
              CI_4_2_2[1,1],
              CI_4_3_2[1,1],
              CI_5_1_2[1,1],
              CI_5_2_2[1,1],
              CI_5_3_2[1,1])

#Create matrix with lists:
d_matrix <- cbind(Study,Coef_names,Coefficients,CI_Upper,CI_Lower)
#Remove row names:
rownames(d_matrix) <- c()

#Transform matrix into data frame:
d_matrix <- data.frame(d_matrix)

#Ensure coeffisient and confidence interval variables are numeric:
d_matrix$Coefficients <- as.character(d_matrix$Coefficients)
d_matrix$CI_Lower <- as.character(d_matrix$CI_Lower)
d_matrix$CI_Upper <- as.character(d_matrix$CI_Upper)
d_matrix$Coefficients <- as.numeric(d_matrix$Coefficients)
d_matrix$CI_Lower <- as.numeric(d_matrix$CI_Lower)
d_matrix$CI_Upper <- as.numeric(d_matrix$CI_Upper)


#Transform coefficient names into a factor
d_matrix$Coef_names <- factor(d_matrix$Coef_names,levels=c('Study 1',
                                                           'Study 2',
                                                           'Study 3',
                                                           'Study 4'))

#Arrange dataframe by coefficient names
d_matrix <- d_matrix %>% arrange(Coef_names)

#Create order in figure:
d_matrix$x <- c(5.2,5.0,4.8,4.2,4.0,3.8,3.2,3.0,2.8,2.2,2.0,1.8,1.2,1.0,0.8)

#Produce figure:
ggplot(data = d_matrix, aes(x = x, y = Coefficients)) +
  geom_hline(aes(yintercept = 0), color = "gray",
             linetype = 2, size = 1.2) +
  geom_point(aes(color = Study, shape=Study),size=3.5) +
  geom_linerange(aes(min = CI_Lower, 
                     max = CI_Upper, 
                     color = Study),
                 size=2) +
  scale_color_manual(values=c('red','black','blue'), name = "Type of News") +
  scale_shape_manual(values=c(15,16,17),"Type of News") +
  ylab("\n The Effect of Searching Online\n on Likelihood of Rating News as True") +
  theme_classic() +
  theme(axis.title.x = element_text(size=18),
        axis.text.x  = element_text(size=16),
        axis.title.y = element_text(size=18),
        axis.text.y  = element_text(size=16),
        plot.title = element_text(size = 18),
        legend.title = element_text(size=16),
        legend.text = element_text(size=14),
        title =element_text(size=20, face='bold')) +
  ylim(-0.1,1.0) +
  scale_x_continuous("Study Number\n",breaks=c(1,2,3,4,5),labels=rev(c('Study 1',
                                                                       'Study 2',
                                                                       'Study 3',
                                                                       'Study 4',
                                                                       'Study 5')),limits=c(0.2,5.8)) +
  coord_flip()


ggsave('./Figures/Fig_1A_True_Ordinal.png',height=12,width=10)



################################################################################################################

################################# Figure 4c: Types_Fig_1A_True_Dummy.png #######################################

################################################################################################################

#Between-Respondent Experiment True Low Quality Articles:

T_LQ_Data_Study_1 <- T_LQ_Data_Study_1 %>% select(True_Dummy,Likert_Evaluation,Treat_Search,Age,Dummy_Congruence,Education_Score,Gender,Income_Score,Article_day,Article_day,ResponseId)
T_LQ_Data_Study_5 <- T_LQ_Data_Study_5 %>% select(True_Dummy,Seven_Ordinal,Treatment,Age,Ideo_Congruence,Education_Score,Gender,Income_Score,Article_day,Article_day,ResponseId)

T_LQ_Data_Study_5$Likert_Evaluation <- T_LQ_Data_Study_5$Seven_Ordinal
T_LQ_Data_Study_5$Treat_Search <- T_LQ_Data_Study_5$Treatment
T_LQ_Data_Study_5$Dummy_Congruence <- T_LQ_Data_Study_5$Ideo_Congruence

T_LQ_Data_Study_5$Seven_Ordinal <- NULL
T_LQ_Data_Study_5$Treatment <- NULL
T_LQ_Data_Study_5$Ideo_Congruence <- NULL


btwn_T_LQ_studies <- rbind(T_LQ_Data_Study_1,T_LQ_Data_Study_5)

#Run OLS Model with clustered standard errors:
btwn_T_LQ_results_1 = feols(True_Dummy ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = btwn_T_LQ_studies)
#Produce confidence intervals with clustered standard errors:
CI_btwn_T_LQ_1 <- confint(btwn_T_LQ_results_1,se='twoway')

#Run OLS Model with clustered standard errors:
btwn_T_LQ_results_2 = feols(Likert_Evaluation ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = btwn_T_LQ_studies)
#Produce confidence intervals with clustered standard errors:
CI_btwn_T_LQ_2 <- confint(btwn_T_LQ_results_2,se='twoway')


#Within-Respondent Experiment True Low Quality Articles:


T_LQ_Data_Study_2 <- T_LQ_Data_Study_2 %>% select(True_Dummy,Likert_Evaluation,Treat_Search,Age,Dummy_Congruence,Education_Score,Gender,Income_Score,Article_day,ResponseId)
T_LQ_Data_Study_3 <- T_LQ_Data_Study_3 %>% select(True_Dummy,Likert_Evaluation,Treat_Search,Age,Dummy_Congruence,Education_Score,Gender,Income_Score,Article_day,ResponseId)
T_LQ_Data_Study_4 <- T_LQ_Data_Study_4 %>% select(True_Dummy,Likert_Evaluation,Treat_Search,Age,Dummy_Congruence,Education_Score,Gender,Income_Score,Article_day,ResponseId)

wthn_T_LQ_studies <- rbind(T_LQ_Data_Study_2,T_LQ_Data_Study_3,T_LQ_Data_Study_4)

#Run OLS Model with clustered standard errors:
wthn_T_LQ_results_1 = feols(True_Dummy ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = wthn_T_LQ_studies)
#Produce confidence intervals with clustered standard errors:
CI_wthn_T_LQ_1 <- confint(wthn_T_LQ_results_1,se='twoway')

#Run OLS Model with clustered standard errors:
wthn_T_LQ_results_2 = feols(Likert_Evaluation ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = wthn_T_LQ_studies)
#Produce confidence intervals with clustered standard errors:
CI_wthn_T_LQ_2 <- confint(wthn_T_LQ_results_2,se='twoway')


#Between-Respondent Experiment True Mainstream Articles:

T_Mainstream_Data_Study_1 <- T_Mainstream_Data_Study_1 %>% select(True_Dummy,Likert_Evaluation,Treat_Search,Age,Dummy_Congruence,Education_Score,Gender,Income_Score,Article_day,Article_day,ResponseId)
T_Mainstream_Data_Study_5 <- T_Mainstream_Data_Study_5 %>% select(True_Dummy,Seven_Ordinal,Treatment,Age,Ideo_Congruence,Education_Score,Gender,Income_Score,Article_day,Article_day,ResponseId)

T_Mainstream_Data_Study_5$Likert_Evaluation <- T_Mainstream_Data_Study_5$Seven_Ordinal
T_Mainstream_Data_Study_5$Treat_Search <- T_Mainstream_Data_Study_5$Treatment
T_Mainstream_Data_Study_5$Dummy_Congruence <- T_Mainstream_Data_Study_5$Ideo_Congruence

T_Mainstream_Data_Study_5$Seven_Ordinal <- NULL
T_Mainstream_Data_Study_5$Treatment <- NULL
T_Mainstream_Data_Study_5$Ideo_Congruence <- NULL

btwn_T_M_studies <- rbind(T_Mainstream_Data_Study_1,T_Mainstream_Data_Study_5)

#Run OLS Model with clustered standard errors:
btwn_T_M_results_1 = feols(True_Dummy ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = btwn_T_M_studies)
#Produce confidence intervals with clustered standard errors:
CI_btwn_T_M_1 <- confint(btwn_T_M_results_1,se='twoway')

#Run OLS Model with clustered standard errors:
btwn_T_M_results_2 = feols(Likert_Evaluation ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = btwn_T_M_studies)
#Produce confidence intervals with clustered standard errors:
CI_btwn_T_M_2 <- confint(btwn_T_M_results_2,se='twoway')


#Within-Respondent Experiment True Mainstream Articles:


T_Mainstream_Data_Study_2 <- T_Mainstream_Data_Study_2 %>% select(True_Dummy,Likert_Evaluation,Treat_Search,Age,Dummy_Congruence,Education_Score,Gender,Income_Score,Article_day,ResponseId)
T_Mainstream_Data_Study_3 <- T_Mainstream_Data_Study_3 %>% select(True_Dummy,Likert_Evaluation,Treat_Search,Age,Dummy_Congruence,Education_Score,Gender,Income_Score,Article_day,ResponseId)
T_Mainstream_Data_Study_4 <- T_Mainstream_Data_Study_4 %>% select(True_Dummy,Likert_Evaluation,Treat_Search,Age,Dummy_Congruence,Education_Score,Gender,Income_Score,Article_day,ResponseId)

wthn_T_M_studies <- rbind(T_Mainstream_Data_Study_2,T_Mainstream_Data_Study_3,T_Mainstream_Data_Study_4)

#Run OLS Model with clustered standard errors:
wthn_T_M_results_1 = feols(True_Dummy ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = wthn_T_M_studies)
#Produce confidence intervals with clustered standard errors:
CI_wthn_T_M_1 <- confint(wthn_T_M_results_1,se='twoway')

#Run OLS Model with clustered standard errors:
wthn_T_M_results_2 = feols(Likert_Evaluation ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = wthn_T_M_studies)
#Produce confidence intervals with clustered standard errors:
CI_wthn_T_M_2 <- confint(wthn_T_M_results_2,se='twoway')




#Within-Respondent Experiment False/Misleading Articles:
Data_Bef_Aft_MF <- Data_Bef_Aft_MF %>% select(Susc_FN,Likert_Evaluation,Treat_Search,Age,Dummy_Congruence,Education_Score,Gender,Income_Score,Article_day,ResponseId)
Study_3_False_M <- Study_3_False_M %>% select(True_Dummy,Likert_Evaluation,Treat_Search,Age,Dummy_Congruence,Education_Score,Gender,Income_Score,Article_day,ResponseId)
Study_4_False_M <- Study_4_False_M %>% select(Susc_FN,Likert_Evaluation,Treat_Search,Age,Dummy_Congruence,Education_Score,Gender,Income_Score,Article_day,ResponseId)

colnames(Data_Bef_Aft_MF)[1] <- 'True_Dummy'
colnames(Study_4_False_M)[1] <- 'True_Dummy'

Within_FM_Study <- rbind(Data_Bef_Aft_MF,Study_3_False_M,Study_4_False_M)


#Run OLS Model with clustered standard errors:
wthn_FM_results_1 = feols(True_Dummy ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Within_FM_Study)
#Produce confidence intervals with clustered standard errors:
CI_wthn_FM_1 <- confint(wthn_FM_results_1,se='twoway')

#Run OLS Model with clustered standard errors:
wthn_FM_results_2 = feols(Likert_Evaluation ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Within_FM_Study)
#Produce confidence intervals with clustered standard errors:
CI_wthn_FM_2 <- confint(wthn_FM_results_2,se='twoway')



#Between-Respondent Experiment False/Misleading Articles:
Misl_False_Search_MF <- Misl_False_Search_MF %>% select(Susc_FN,Likert_Evaluation,Treat_Search,Age,Dummy_Congruence,Education_Score,Gender,Income_Score,Article_day,Article_day,ResponseId)
Study_5_False_M <- Study_5_False_M %>% select(True_Dummy,Seven_Ordinal,Treatment,Age,Ideo_Congruence,Education_Score,Gender,Income_Score,Article_day,Article_day,ResponseId)

colnames(Misl_False_Search_MF)[1] <- 'True_Dummy'

Study_5_False_M$Likert_Evaluation <- Study_5_False_M$Seven_Ordinal
Study_5_False_M$Treat_Search <- Study_5_False_M$Treatment
Study_5_False_M$Dummy_Congruence <- Study_5_False_M$Ideo_Congruence

Study_5_False_M$Seven_Ordinal <- NULL
Study_5_False_M$Treatment <- NULL
Study_5_False_M$Ideo_Congruence <- NULL

Between_FM_Study <- rbind(Misl_False_Search_MF,Study_5_False_M)



#Run OLS Model with clustered standard errors:
btwn_FM_results_1 = feols(True_Dummy ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Between_FM_Study)
#Produce confidence intervals with clustered standard errors:
CI_btwn_FM_1 <- confint(btwn_FM_results_1,se='twoway')

#Run OLS Model with clustered standard errors:
btwn_FM_results_2 = feols(Likert_Evaluation ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Between_FM_Study)
#Produce confidence intervals with clustered standard errors:
CI_btwn_FM_2 <- confint(btwn_FM_results_2,se='twoway')





#Create list for figure of Study numbers
Study <- c('True - Low Quality',
           'True - Mainstream',
           'False/Misleading',
           'True - Low Quality',
           'True - Mainstream',
           'False/Misleading')

#List of coefficient names in matrix:
Coef_names <- c('Between',
                'Between',
                'Between',
                'Within',
                'Within',
                'Within')

Coefficients <- c(btwn_T_LQ_results_2$coefficients[1],
                  btwn_T_M_results_2$coefficients[1],
                  btwn_FM_results_2$coefficients[1],
                  wthn_T_LQ_results_2$coefficients[1],
                  wthn_T_M_results_2$coefficients[1],
                  wthn_FM_results_2$coefficients[1])


#Lists upper confidence interval (95%)
CI_Upper <- c(CI_btwn_T_LQ_2[1,2],
              CI_btwn_T_M_2[1,2],
              CI_btwn_FM_2[1,2],
              CI_wthn_T_LQ_2[1,2],
              CI_wthn_T_M_2[1,2],
              CI_wthn_FM_2[1,2])


#Lists lower confidence interval (95%)
CI_Lower <- c(CI_btwn_T_LQ_2[1,1],
              CI_btwn_T_M_2[1,1],
              CI_btwn_FM_2[1,1],
              CI_wthn_T_LQ_2[1,1],
              CI_wthn_T_M_2[1,1],
              CI_wthn_FM_2[1,1])


#Create matrix with lists:
d_matrix <- cbind(Study,Coef_names,Coefficients,CI_Upper,CI_Lower)
#Remove row names:
rownames(d_matrix) <- c()

#Transform matrix into data frame:
d_matrix <- data.frame(d_matrix)

#Ensure coeffisient and confidence interval variables are numeric:
d_matrix$Coefficients <- as.character(d_matrix$Coefficients)
d_matrix$CI_Lower <- as.character(d_matrix$CI_Lower)
d_matrix$CI_Upper <- as.character(d_matrix$CI_Upper)
d_matrix$Coefficients <- as.numeric(d_matrix$Coefficients)
d_matrix$CI_Lower <- as.numeric(d_matrix$CI_Lower)
d_matrix$CI_Upper <- as.numeric(d_matrix$CI_Upper)



#Transform coefficient names into a factor
d_matrix$Coef_names <- factor(d_matrix$Coef_names,levels=c('Between',
                                                           'Within'))

#Arrange dataframe by coefficient names
d_matrix <- d_matrix %>% arrange(Coef_names)

#Create order in figure:
d_matrix$x <- c(2.2,2,1.8,1.2,1,0.8)

#Produce figure:
ggplot(data = d_matrix, aes(x = x, y = Coefficients)) +
  geom_hline(aes(yintercept = 0), color = "gray",
             linetype = 2, size = 1.2) +
  geom_point(aes(color = Study, shape=Study),size=3.5) +
  geom_linerange(aes(min = CI_Lower, 
                     max = CI_Upper, 
                     color = Study),
                 size=2) +
  scale_color_manual(values=c('red','black','blue'), name = "Type of News") +
  scale_shape_manual(values=c(15,16,17),"Type of News") +
  ylab("\n The Effect of Searching Online\n on Likelihood of Rating News as True") +
  theme_classic() +
  theme(axis.title.x = element_text(size=18),
        axis.text.x  = element_text(size=16),
        axis.title.y = element_text(size=18),
        axis.text.y  = element_text(size=16),
        plot.title = element_text(size = 18),
        legend.title = element_text(size=16),
        legend.text = element_text(size=14),
        title =element_text(size=20, face='bold')) +
  ylim(-0.1,0.7) +
  scale_x_continuous("Study Type\n",breaks=c(1,2),labels=rev(c('Between',
                                                               'Within')),limits=c(0,3)) +
  coord_flip()


ggsave('./Figures/Types_Fig_1A_True_Ordinal.png',height=12,width=10)


################################################################################

################ Figures 1a and 1b Using the Preregistered Models ##############

################################################################################

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
Study_3_df <- read.csv('./Data/Study_3_df_FM.csv')

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
Study_4_df <- read.csv('./Data/Study_4_df_FM.csv')
Study_4_Ideo <- read.csv('./Data/Study_4_Respondent_Ideo.csv')

Study_4_df$ResponseId <- as.character(Study_4_df$ResponseId)
Study_4_Ideo$ResponseId <- as.character(Study_4_Ideo$ResponseId)

Study_4_df <- merge(Study_4_df,Study_4_Ideo,by='ResponseId')

#Select variables of interest:
Study_4_df <- Study_4_df %>% select(Likert_Evaluation,Susc_FN,Treat_Search,Education_Score,Age,Gender,Income_Score,Ideology_Score,Dummy_Congruence,Familiar_Dummy,Article_day,ResponseId)
Study_4_df <- na.omit(Study_4_df)

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


##############################################################################

########################## Effect of Instructions ############################

##############################################################################

#Read in data:
Data_1_FM <- read.csv('./Data/Study_6_FM.csv')
Data_1_T_M <- read.csv('./Data/Study_6_T_M.csv')
Data_1_T_LQ <- read.csv('./Data/Study_6_T_LQ.csv')

#Run models
D_1_result_1_1 = feols(T_Dummy ~ Treatment_1 + Treatment_2 + Treatment_3 + Age + Ideo_Congruence + Education_Score +  Q_Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=Data_1_FM)
CI_1_1_1 = confint(D_1_result_1_1)

D_1_result_2_1 = feols(T_Dummy ~ Treatment_1 + Treatment_2 + Treatment_3 + Age + Ideo_Congruence + Education_Score +  Q_Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=Data_1_T_M)
CI_1_2_1 = confint(D_1_result_2_1)

D_1_result_3_1 = feols(T_Dummy ~ Treatment_1 + Treatment_2 + Treatment_3 + Age + Ideo_Congruence + Education_Score +  Q_Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=Data_1_T_LQ)
CI_1_3_1 = confint(D_1_result_3_1)

D_1_result_1_2 = feols(Ordinal_4 ~ Treatment_1 + Treatment_2 + Treatment_3 + Age + Ideo_Congruence + Education_Score +  Q_Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=Data_1_FM)
CI_1_1_2 = confint(D_1_result_1_2)

D_1_result_2_2 = feols(Ordinal_4 ~ Treatment_1 + Treatment_2 + Treatment_3 + Age + Ideo_Congruence + Education_Score +  Q_Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=Data_1_T_M)
CI_1_2_2 = confint(D_1_result_2_2)

D_1_result_3_2 = feols(Ordinal_4 ~ Treatment_1 + Treatment_2 + Treatment_3 + Age + Ideo_Congruence + Education_Score +  Q_Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=Data_1_T_LQ)
CI_1_3_2 = confint(D_1_result_3_2)

D_1_result_1_3 = feols(Ordinal_7 ~ Treatment_1 + Treatment_2 + Treatment_3 + Age + Ideo_Congruence + Education_Score +  Q_Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=Data_1_FM)
CI_1_1_3 = confint(D_1_result_1_3)

D_1_result_2_3 = feols(Ordinal_7 ~ Treatment_1 + Treatment_2 + Treatment_3 + Age + Ideo_Congruence + Education_Score +  Q_Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=Data_1_T_M)
CI_1_2_3 = confint(D_1_result_2_3)

D_1_result_3_3 = feols(Ordinal_7 ~ Treatment_1 + Treatment_2 + Treatment_3 + Age + Ideo_Congruence + Education_Score +  Q_Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=Data_1_T_LQ)
CI_1_3_3 = confint(D_1_result_3_3)


################################ Categorical Scale Figure ################################

New_matr <- matrix(c(D_1_result_1_1$coefficients[1],CI_1_1_1[1,1],CI_1_1_1[1,2],'False/Misleading','Treatment Group 1\n',
                     D_1_result_1_1$coefficients[2],CI_1_1_1[2,1],CI_1_1_1[2,2],'False/Misleading','Treatment Group 2\n',
                     D_1_result_1_1$coefficients[3],CI_1_1_1[3,1],CI_1_1_1[3,2],'False/Misleading','Treatment Group 3\n',
                     D_1_result_2_1$coefficients[1],CI_1_2_1[1,1],CI_1_2_1[1,2],'True-Mainstream','Treatment Group 1\n',
                     D_1_result_2_1$coefficients[2],CI_1_2_1[2,1],CI_1_2_1[2,2],'True-Mainstream','Treatment Group 2\n',
                     D_1_result_2_1$coefficients[3],CI_1_2_1[3,1],CI_1_2_1[3,2],'True-Mainstream','Treatment Group 3\n',
                     D_1_result_3_1$coefficients[1],CI_1_3_1[1,1],CI_1_3_1[1,2],'True-Low Quality','Treatment Group 1\n',
                     D_1_result_3_1$coefficients[2],CI_1_3_1[2,1],CI_1_3_1[2,2],'True-Low Quality','Treatment Group 2\n',
                     D_1_result_3_1$coefficients[3],CI_1_3_1[3,1],CI_1_3_1[3,2],'True-Low Quality','Treatment Group 3\n'),ncol=5,byrow=T)

#Transform matrix into dataframe:
Fig_2d_Mat <- as.data.frame(New_matr)


#Create dataframe to produce plot
Fig_2d_Mat <- na.omit(Fig_2d_Mat)
colnames(Fig_2d_Mat) <- c('Coef','Low_Conf','Upp_Conf','Measure','Instructions')
Fig_2d_Mat <- na.omit(Fig_2d_Mat)
Fig_2d_Mat$x<- rev(c(0.8,0.9,1.0,1.4,1.5,1.6,2.0,2.1,2.2))
Fig_2d_Mat$Coef <- as.character(Fig_2d_Mat$Coef)
Fig_2d_Mat$Coef <- as.numeric(Fig_2d_Mat$Coef)
Fig_2d_Mat$Upp_Conf <- as.character(Fig_2d_Mat$Upp_Conf)
Fig_2d_Mat$Upp_Conf <- as.numeric(Fig_2d_Mat$Upp_Conf)
Fig_2d_Mat$Low_Conf <- as.character(Fig_2d_Mat$Low_Conf)
Fig_2d_Mat$Low_Conf <- as.numeric(Fig_2d_Mat$Low_Conf)


#Produce Plot:
ggplot(data = Fig_2d_Mat, aes(x = x, y = Coef,color=Instructions, shape=Instructions)) +
  geom_hline(aes(yintercept = 0), color = "gray",
             linetype = 2, size = 1.2) +
  geom_point(aes(color=Instructions, shape=Instructions),size=4) +
  geom_linerange(aes(min = Low_Conf, 
                     max = Upp_Conf, 
                     color = Instructions),
                 size=1.5) +
  scale_color_manual(values=c('red','blue','purple'), name = "Instructions") +
  ylab("\n Effect of Searching Online on Belief in News\n Dependent on Type of Instructions") +
  theme_classic() +
  theme(axis.title.x = element_text(size=20),
        axis.text.x  = element_text(size=26),
        axis.title.y = element_text(size=26),
        axis.text.y  = element_text(size=26),
        plot.title = element_text(size=26),
        legend.title = element_text(size=20),
        legend.text = element_text(size=20)) +
  ylim(-0.1,0.2) +
  scale_x_continuous("Type of News \n",breaks=c(2.1,1.5,0.9),labels=c('False/Misleading','True-Mainstream','True-Low Quality'),limits=c(0.5,2.5)) +
  coord_flip()

#Save Figure:
ggsave('./Figures/Different_Treatment_True_Dummy_All.png',height=12,width=10)

################################ Four-Point Ordinal Scale Figure ################################


New_matr <- matrix(c(D_1_result_1_2$coefficients[1],CI_1_1_2[1,1],CI_1_1_2[1,2],'False/Misleading','Treatment Group 1\n',
                     D_1_result_1_2$coefficients[2],CI_1_1_2[2,1],CI_1_1_2[2,2],'False/Misleading','Treatment Group 2\n',
                     D_1_result_1_2$coefficients[3],CI_1_1_2[3,1],CI_1_1_2[3,2],'False/Misleading','Treatment Group 3\n',
                     D_1_result_2_2$coefficients[1],CI_1_2_2[1,1],CI_1_2_2[1,2],'True-Mainstream','Treatment Group 1\n',
                     D_1_result_2_2$coefficients[2],CI_1_2_2[2,1],CI_1_2_2[2,2],'True-Mainstream','Treatment Group 2\n',
                     D_1_result_2_2$coefficients[3],CI_1_2_2[3,1],CI_1_2_2[3,2],'True-Mainstream','Treatment Group 3\n',
                     D_1_result_3_2$coefficients[1],CI_1_3_2[1,1],CI_1_3_2[1,2],'True-Low Quality','Treatment Group 1\n',
                     D_1_result_3_2$coefficients[2],CI_1_3_2[2,1],CI_1_3_2[2,2],'True-Low Quality','Treatment Group 2\n',
                     D_1_result_3_2$coefficients[3],CI_1_3_2[3,1],CI_1_3_2[3,2],'True-Low Quality','Treatment Group 3\n'),ncol=5,byrow=T)

#Transform matrix into dataframe:
Fig_2d_Mat <- as.data.frame(New_matr)


#Create dataframe to produce plot
Fig_2d_Mat <- na.omit(Fig_2d_Mat)
colnames(Fig_2d_Mat) <- c('Coef','Low_Conf','Upp_Conf','Measure','Instructions')
Fig_2d_Mat <- na.omit(Fig_2d_Mat)
Fig_2d_Mat$x<- rev(c(0.8,0.9,1.0,1.4,1.5,1.6,2.0,2.1,2.2))
Fig_2d_Mat$Coef <- as.character(Fig_2d_Mat$Coef)
Fig_2d_Mat$Coef <- as.numeric(Fig_2d_Mat$Coef)
Fig_2d_Mat$Upp_Conf <- as.character(Fig_2d_Mat$Upp_Conf)
Fig_2d_Mat$Upp_Conf <- as.numeric(Fig_2d_Mat$Upp_Conf)
Fig_2d_Mat$Low_Conf <- as.character(Fig_2d_Mat$Low_Conf)
Fig_2d_Mat$Low_Conf <- as.numeric(Fig_2d_Mat$Low_Conf)


#Produce Plot:
ggplot(data = Fig_2d_Mat, aes(x = x, y = Coef,color=Instructions, shape=Instructions)) +
  geom_hline(aes(yintercept = 0), color = "gray",
             linetype = 2, size = 1.2) +
  geom_point(aes(color=Instructions, shape=Instructions),size=4) +
  geom_linerange(aes(min = Low_Conf, 
                     max = Upp_Conf, 
                     color = Instructions),
                 size=1.5) +
  scale_color_manual(values=c('red','blue','purple'), name = "Instructions") +
  ylab("\n Effect of Searching Online on Belief in News\n Dependent on Type of Instructions") +
  theme_classic() +
  theme(axis.title.x = element_text(size=20),
        axis.text.x  = element_text(size=26),
        axis.title.y = element_text(size=26),
        axis.text.y  = element_text(size=26),
        plot.title = element_text(size=26),
        legend.title = element_text(size=20),
        legend.text = element_text(size=20)) +
  ylim(-0.1,0.3) +
  scale_x_continuous("Type of News \n",breaks=c(2.1,1.5,0.9),labels=c('False/Misleading','True-Mainstream','True-Low Quality'),limits=c(0.5,2.5)) +
  coord_flip()

#Save Figure:
ggsave('./Figures/Different_Treatment_Four_Ordinal_All.png',height=12,width=10)






################################ Seven-Point Ordinal Scale Figure ################################


New_matr <- matrix(c(D_1_result_1_3$coefficients[1],CI_1_1_3[1,1],CI_1_1_3[1,2],'False/Misleading','Treatment Group 1\n',
                     D_1_result_1_3$coefficients[2],CI_1_1_3[2,1],CI_1_1_3[2,2],'False/Misleading','Treatment Group 2\n',
                     D_1_result_1_3$coefficients[3],CI_1_1_3[3,1],CI_1_1_3[3,2],'False/Misleading','Treatment Group 3\n',
                     D_1_result_2_3$coefficients[1],CI_1_2_3[1,1],CI_1_2_3[1,2],'True-Mainstream','Treatment Group 1\n',
                     D_1_result_2_3$coefficients[2],CI_1_2_3[2,1],CI_1_2_3[2,2],'True-Mainstream','Treatment Group 2\n',
                     D_1_result_2_3$coefficients[3],CI_1_2_3[3,1],CI_1_2_3[3,2],'True-Mainstream','Treatment Group 3\n',
                     D_1_result_3_3$coefficients[1],CI_1_3_3[1,1],CI_1_3_3[1,2],'True-Low Quality','Treatment Group 1\n',
                     D_1_result_3_3$coefficients[2],CI_1_3_3[2,1],CI_1_3_3[2,2],'True-Low Quality','Treatment Group 2\n',
                     D_1_result_3_3$coefficients[3],CI_1_3_3[3,1],CI_1_3_3[3,2],'True-Low Quality','Treatment Group 3\n'),ncol=5,byrow=T)

#Transform matrix into dataframe:
Fig_2d_Mat <- as.data.frame(New_matr)


#Create dataframe to produce plot
Fig_2d_Mat <- na.omit(Fig_2d_Mat)
colnames(Fig_2d_Mat) <- c('Coef','Low_Conf','Upp_Conf','Measure','Instructions')
Fig_2d_Mat <- na.omit(Fig_2d_Mat)
Fig_2d_Mat$x<- rev(c(0.8,0.9,1.0,1.4,1.5,1.6,2.0,2.1,2.2))
Fig_2d_Mat$Coef <- as.character(Fig_2d_Mat$Coef)
Fig_2d_Mat$Coef <- as.numeric(Fig_2d_Mat$Coef)
Fig_2d_Mat$Upp_Conf <- as.character(Fig_2d_Mat$Upp_Conf)
Fig_2d_Mat$Upp_Conf <- as.numeric(Fig_2d_Mat$Upp_Conf)
Fig_2d_Mat$Low_Conf <- as.character(Fig_2d_Mat$Low_Conf)
Fig_2d_Mat$Low_Conf <- as.numeric(Fig_2d_Mat$Low_Conf)


#Produce Plot:
ggplot(data = Fig_2d_Mat, aes(x = x, y = Coef,color=Instructions, shape=Instructions)) +
  geom_hline(aes(yintercept = 0), color = "gray",
             linetype = 2, size = 1.2) +
  geom_point(aes(color=Instructions, shape=Instructions),size=4) +
  geom_linerange(aes(min = Low_Conf, 
                     max = Upp_Conf, 
                     color = Instructions),
                 size=1.5) +
  scale_color_manual(values=c('red','blue','purple'), name = "Instructions") +
  ylab("\n Effect of Searching Online on Belief in News\n Dependent on Type of Instructions") +
  theme_classic() +
  theme(axis.title.x = element_text(size=20),
        axis.text.x  = element_text(size=26),
        axis.title.y = element_text(size=26),
        axis.text.y  = element_text(size=26),
        plot.title = element_text(size=26),
        legend.title = element_text(size=20),
        legend.text = element_text(size=20)) +
  ylim(-0.12,0.63) +
  scale_x_continuous("Type of News \n",breaks=c(2.1,1.5,0.9),labels=c('False/Misleading','True-Mainstream','True-Low Quality'),limits=c(0.5,2.5)) +
  coord_flip()

#Save Figure:
ggsave('./Figures/Different_Treatment_Seven_Ordinal_All.png',height=12,width=10)




######################################################################################################

##################################### Balance Table for Study 6 ######################################

######################################################################################################

Study_6_Full <- rbind(Data_1_FM,Data_1_T_M,Data_1_T_LQ)
Study_6_Full <- Study_6_Full %>% select(Treatment_1,Treatment_2,Treatment_3,Age,Education_Score,Q_Gender,Income_Score,ResponseId)
Study_6_Full <- unique(Study_6_Full)

#Confirm class of variable:
Study_6_Full$ResponseId <- as.character(Study_6_Full$ResponseId)

#Read in ideology score data:
Ideo_Resp_st_6 <- read.csv('./Data/Study_6_Ideology_Respondents.csv')
Ideo_Resp_st_6 <- Ideo_Resp_st_6 %>% select(ResponseId,Ideology_Score)

#Confirm class of variable:
Ideo_Resp_st_6$ResponseId <- as.character(Ideo_Resp_st_6$ResponseId)
Ideo_Resp_st_6$Ideology_Score <- as.character(Ideo_Resp_st_6$Ideology_Score)
Ideo_Resp_st_6$Ideology_Score <- as.numeric(Ideo_Resp_st_6$Ideology_Score)


Study_6_Full <- merge(Study_6_Full,Ideo_Resp_st_6,by='ResponseId')

#Create dataset with each group:
Control_group <- Study_6_Full %>% filter(Treatment_1 == 0 & Treatment_2 == 0 & Treatment_3 == 0)
Treat_1_group <- Study_6_Full %>% filter(Treatment_1 == 1)
Treat_2_group <- Study_6_Full %>% filter(Treatment_2 == 1)
Treat_3_group <- Study_6_Full %>% filter(Treatment_3 == 1)


Ctrl_T1 <- rbind(Control_group,Treat_1_group)
Ctrl_T2 <- rbind(Control_group,Treat_2_group)
Ctrl_T3 <- rbind(Control_group,Treat_3_group)


##################### Control and Treatment Group 1 #############################


#Education:
Educ_C <- round(mean(Control_group$Education_Score),2)
Educ_T <- round(mean(Treat_1_group$Education_Score),2)
Diff_Educ <- round((Educ_T - Educ_C),2)

Model_Educ <- coeftest(lm(Education_Score ~ Treatment_1,data=Ctrl_T1))
Educ_P <- Model_Educ[2,4]

Educ_Stars <- ''
if(Educ_P < 0.05){
        Educ_Stars <- '*'
        if(Educ_P < 0.01){
                Educ_Stars <- '**'
                if(Educ_P < 0.001){
                        Educ_Stars <- '***'
                }
        }
} 

Diff_Educ <- paste0(as.character(Diff_Educ),Educ_Stars)


#Age:
Age_C <- round(mean(Control_group$Age),2)
Age_T <- round(mean(Treat_1_group$Age),2)
Diff_Age <- round((Age_T - Age_C),2)

Model_Age <- coeftest(lm(Age ~ Treatment_1,data=Ctrl_T1))
Age_P <- Model_Age[2,4]

Age_Stars <- ''
if(Age_P < 0.05){
        Age_Stars <- '*'
        if(Age_P < 0.01){
                Age_Stars <- '**'
                if(Age_P < 0.001){
                        Age_Stars <- '***'
                }
        }
} 

Diff_Age <- paste0(as.character(Diff_Age),Age_Stars)


#Gender:
Gender_C <- round(mean(Control_group$Q_Gender),2)
Gender_T <- round(mean(Treat_1_group$Q_Gender),2)
Diff_Gender <- round((Gender_T - Gender_C),2)

Model_Gender <- coeftest(lm(Q_Gender ~ Treatment_1,data=Ctrl_T1))
Gender_P <- Model_Gender[2,4]

Gender_Stars <- ''
if(Gender_P < 0.05){
        Gender_Stars <- '*'
        if(Gender_P < 0.01){
                Gender_Stars <- '**'
                if(Gender_P < 0.001){
                        Gender_Stars <- '***'
                }
        }
} 

Diff_Gender <- paste0(as.character(Diff_Gender),Gender_Stars)


#Income:
Income_C <- round(mean(Control_group$Income_Score),2)
Income_T <- round(mean(Treat_1_group$Income_Score),2)
Diff_Income <- round((Income_T - Income_C),2)

Model_Income <- coeftest(lm(Income_Score ~ Treatment_1,data=Ctrl_T1))
Income_P <- Model_Income[2,4]

Income_Stars <- ''
if(Income_P < 0.05){
        Income_Stars <- '*'
        if(Income_P < 0.01){
                Income_Stars <- '**'
                if(Income_P < 0.001){
                        Income_Stars <- '***'
                }
        }
} 

Diff_Income <- paste0(as.character(Diff_Income),Income_Stars)


#Ideology:
Ideology_C <- round(mean(Control_group$Ideology_Score),2)
Ideology_T <- round(mean(Treat_1_group$Ideology_Score),2)
Diff_Ideology <- round((Ideology_T - Ideology_C),2)

Model_Ideology <- coeftest(lm(Ideology_Score ~ Treatment_1,data=Ctrl_T1))
Ideology_P <- Model_Ideology[2,4]

Ideology_Stars <- ''
if(Ideology_P < 0.05){
        Ideology_Stars <- '*'
        if(Ideology_P < 0.01){
                Ideology_Stars <- '**'
                if(Ideology_P < 0.001){
                        Ideology_Stars <- '***'
                }
        }
} 

Diff_Ideology <- paste0(as.character(Diff_Ideology),Ideology_Stars)


Balance_T <- matrix(c('Education',Educ_T,Educ_C,Diff_Educ,
                      'Age',Age_T,Age_C,Diff_Age,
                      'Gender',Gender_T,Gender_C,Diff_Gender,
                      'Income',Income_T,Income_C,Diff_Income,
                      'Ideology',Ideology_T,Ideology_C,Diff_Ideology),byrow=T,ncol=4)


xt <- xtable(Balance_T,
             digits=2,
             align=c(
                     "|p{1.5cm}|","|p{3cm}|","|p{3cm}|",
                     "p{3cm}|","p{3cm}|"))

colnames(xt) <- c('Demographic','Average (Treatment)','Average (Control)','Difference')


comment          <- list()
comment$pos      <- list()
comment$pos[[1]] <- c(nrow(Balance_T))
comment$command  <- c(paste("\\hline \n",  # we`ll replace all default hlines with this and the ones below
                            '\\multicolumn{3}{l}{\\textsuperscript{***}$p<0.001$, \\textsuperscript{**}$p<0.01$, \\textsuperscript{*}$p<0.05$}',
                            sep = ""))


#Write Table:
write(print(xt,include.rownames=FALSE,
            sanitize.colnames.function = identity,
            add.to.row = comment),
      file='./Tables/Balance_St_6_1.txt')



##################### Control and Treatment Group 2 #############################

#Education:
Educ_C <- round(mean(Control_group$Education_Score),2)
Educ_T <- round(mean(Treat_2_group$Education_Score),2)
Diff_Educ <- round((Educ_T - Educ_C),2)

Model_Educ <- coeftest(lm(Education_Score ~ Treatment_2,data=Ctrl_T2))
Educ_P <- Model_Educ[2,4]

Educ_Stars <- ''
if(Educ_P < 0.05){
        Educ_Stars <- '*'
        if(Educ_P < 0.01){
                Educ_Stars <- '**'
                if(Educ_P < 0.001){
                        Educ_Stars <- '***'
                }
        }
} 

Diff_Educ <- paste0(as.character(Diff_Educ),Educ_Stars)


#Age:
Age_C <- round(mean(Control_group$Age),2)
Age_T <- round(mean(Treat_2_group$Age),2)
Diff_Age <- round((Age_T - Age_C),2)

Model_Age <- coeftest(lm(Age ~ Treatment_2,data=Ctrl_T2))
Age_P <- Model_Age[2,4]

Age_Stars <- ''
if(Age_P < 0.05){
        Age_Stars <- '*'
        if(Age_P < 0.01){
                Age_Stars <- '**'
                if(Age_P < 0.001){
                        Age_Stars <- '***'
                }
        }
} 

Diff_Age <- paste0(as.character(Diff_Age),Age_Stars)


#Gender:
Gender_C <- round(mean(Control_group$Q_Gender),2)
Gender_T <- round(mean(Treat_2_group$Q_Gender),2)
Diff_Gender <- round((Gender_T - Gender_C),2)

Model_Gender <- coeftest(lm(Q_Gender ~ Treatment_2,data=Ctrl_T2))
Gender_P <- Model_Gender[2,4]

Gender_Stars <- ''
if(Gender_P < 0.05){
        Gender_Stars <- '*'
        if(Gender_P < 0.01){
                Gender_Stars <- '**'
                if(Gender_P < 0.001){
                        Gender_Stars <- '***'
                }
        }
} 

Diff_Gender <- paste0(as.character(Diff_Gender),Gender_Stars)


#Income:
Income_C <- round(mean(Control_group$Income_Score),2)
Income_T <- round(mean(Treat_2_group$Income_Score),2)
Diff_Income <- round((Income_T - Income_C),2)

Model_Income <- coeftest(lm(Income_Score ~ Treatment_2,data=Ctrl_T2))
Income_P <- Model_Income[2,4]

Income_Stars <- ''
if(Income_P < 0.05){
        Income_Stars <- '*'
        if(Income_P < 0.01){
                Income_Stars <- '**'
                if(Income_P < 0.001){
                        Income_Stars <- '***'
                }
        }
} 

Diff_Income <- paste0(as.character(Diff_Income),Income_Stars)


#Ideology:
Ideology_C <- round(mean(Control_group$Ideology_Score),2)
Ideology_T <- round(mean(Treat_2_group$Ideology_Score),2)
Diff_Ideology <- round((Ideology_T - Ideology_C),2)

Model_Ideology <- coeftest(lm(Ideology_Score ~ Treatment_2,data=Ctrl_T2))
Ideology_P <- Model_Ideology[2,4]

Ideology_Stars <- ''
if(Ideology_P < 0.05){
        Ideology_Stars <- '*'
        if(Ideology_P < 0.01){
                Ideology_Stars <- '**'
                if(Ideology_P < 0.001){
                        Ideology_Stars <- '***'
                }
        }
} 

Diff_Ideology <- paste0(as.character(Diff_Ideology),Ideology_Stars)


Balance_T <- matrix(c('Education',Educ_T,Educ_C,Diff_Educ,
                      'Age',Age_T,Age_C,Diff_Age,
                      'Gender',Gender_T,Gender_C,Diff_Gender,
                      'Income',Income_T,Income_C,Diff_Income,
                      'Ideology',Ideology_T,Ideology_C,Diff_Ideology),byrow=T,ncol=4)


xt <- xtable(Balance_T,
             digits=2,
             align=c(
                     "|p{1.5cm}|","|p{3cm}|","|p{3cm}|",
                     "p{3cm}|","p{3cm}|"))

colnames(xt) <- c('Demographic','Average (Treatment)','Average (Control)','Difference')


comment          <- list()
comment$pos      <- list()
comment$pos[[1]] <- c(nrow(Balance_T))
comment$command  <- c(paste("\\hline \n",  # we`ll replace all default hlines with this and the ones below
                            '\\multicolumn{3}{l}{\\textsuperscript{***}$p<0.001$, \\textsuperscript{**}$p<0.01$, \\textsuperscript{*}$p<0.05$}',
                            sep = ""))


#Write Table:
write(print(xt,include.rownames=FALSE,
            sanitize.colnames.function = identity,
            add.to.row = comment),
      file='./Tables/Balance_St_6_2.txt')



##################### Control and Treatment Group 2 #############################

#Education:
Educ_C <- round(mean(Control_group$Education_Score),2)
Educ_T <- round(mean(Treat_3_group$Education_Score),2)
Diff_Educ <- round((Educ_T - Educ_C),2)

Model_Educ <- coeftest(lm(Education_Score ~ Treatment_3,data=Ctrl_T3))
Educ_P <- Model_Educ[2,4]

Educ_Stars <- ''
if(Educ_P < 0.05){
        Educ_Stars <- '*'
        if(Educ_P < 0.01){
                Educ_Stars <- '**'
                if(Educ_P < 0.001){
                        Educ_Stars <- '***'
                }
        }
} 

Diff_Educ <- paste0(as.character(Diff_Educ),Educ_Stars)


#Age:
Age_C <- round(mean(Control_group$Age),2)
Age_T <- round(mean(Treat_3_group$Age),2)
Diff_Age <- round((Age_T - Age_C),2)

Model_Age <- coeftest(lm(Age ~ Treatment_3,data=Ctrl_T3))
Age_P <- Model_Age[2,4]

Age_Stars <- ''
if(Age_P < 0.05){
        Age_Stars <- '*'
        if(Age_P < 0.01){
                Age_Stars <- '**'
                if(Age_P < 0.001){
                        Age_Stars <- '***'
                }
        }
} 

Diff_Age <- paste0(as.character(Diff_Age),Age_Stars)


#Gender:
Gender_C <- round(mean(Control_group$Q_Gender),2)
Gender_T <- round(mean(Treat_3_group$Q_Gender),2)
Diff_Gender <- round((Gender_T - Gender_C),2)

Model_Gender <- coeftest(lm(Q_Gender ~ Treatment_3,data=Ctrl_T3))
Gender_P <- Model_Gender[2,4]

Gender_Stars <- ''
if(Gender_P < 0.05){
        Gender_Stars <- '*'
        if(Gender_P < 0.01){
                Gender_Stars <- '**'
                if(Gender_P < 0.001){
                        Gender_Stars <- '***'
                }
        }
} 

Diff_Gender <- paste0(as.character(Diff_Gender),Gender_Stars)


#Income:
Income_C <- round(mean(Control_group$Income_Score),2)
Income_T <- round(mean(Treat_3_group$Income_Score),2)
Diff_Income <- round((Income_T - Income_C),2)

Model_Income <- coeftest(lm(Income_Score ~ Treatment_3,data=Ctrl_T3))
Income_P <- Model_Income[2,4]

Income_Stars <- ''
if(Income_P < 0.05){
        Income_Stars <- '*'
        if(Income_P < 0.01){
                Income_Stars <- '**'
                if(Income_P < 0.001){
                        Income_Stars <- '***'
                }
        }
} 

Diff_Income <- paste0(as.character(Diff_Income),Income_Stars)


#Ideology:
Ideology_C <- round(mean(Control_group$Ideology_Score),2)
Ideology_T <- round(mean(Treat_3_group$Ideology_Score),2)
Diff_Ideology <- round((Ideology_T - Ideology_C),2)

Model_Ideology <- coeftest(lm(Ideology_Score ~ Treatment_3,data=Ctrl_T3))
Ideology_P <- Model_Ideology[2,4]

Ideology_Stars <- ''
if(Ideology_P < 0.05){
        Ideology_Stars <- '*'
        if(Ideology_P < 0.01){
                Ideology_Stars <- '**'
                if(Ideology_P < 0.001){
                        Ideology_Stars <- '***'
                }
        }
} 

Diff_Ideology <- paste0(as.character(Diff_Ideology),Ideology_Stars)


Balance_T <- matrix(c('Education',Educ_T,Educ_C,Diff_Educ,
                      'Age',Age_T,Age_C,Diff_Age,
                      'Gender',Gender_T,Gender_C,Diff_Gender,
                      'Income',Income_T,Income_C,Diff_Income,
                      'Ideology',Ideology_T,Ideology_C,Diff_Ideology),byrow=T,ncol=4)


xt <- xtable(Balance_T,
             digits=2,
             align=c(
                     "|p{1.5cm}|","|p{3cm}|","|p{3cm}|",
                     "p{3cm}|","p{3cm}|"))

colnames(xt) <- c('Demographic','Average (Treatment)','Average (Control)','Difference')


comment          <- list()
comment$pos      <- list()
comment$pos[[1]] <- c(nrow(Balance_T))
comment$command  <- c(paste("\\hline \n",  # we`ll replace all default hlines with this and the ones below
                            '\\multicolumn{3}{l}{\\textsuperscript{***}$p<0.001$, \\textsuperscript{**}$p<0.01$, \\textsuperscript{*}$p<0.05$}',
                            sep = ""))


#Write Table:
write(print(xt,include.rownames=FALSE,
            sanitize.colnames.function = identity,
            add.to.row = comment),
      file='./Tables/Balance_St_6_3.txt')

########################################################################################################

########################## Figures without original link in search results #############################

########################################################################################################

################################################################################################################

########################################## Figure 2a: Study_5_Bar_Graph_Google_Search_wo_OG.png ######################

################################################################################################################

Study_5_treat_data <- read.csv('./Data/Study_5_treat_data_wo_OG.csv')

Search_Results_FM <- Study_5_treat_data %>% filter(FC_Eval == 'FM')
Search_Results_T <- Study_5_treat_data %>% filter(FC_Eval == 'True')

#Create Count of individuals that didnt see any unreliable news sites:
True_Count_Zero <- Search_Results_T %>% filter(Unrel_contain == 0)
FM_Count_Zero <- Search_Results_FM %>% filter(Unrel_contain == 0)

#Create Count of individuals that saw at lesat some unreliable news sites:
True_Count_Above_Zero <- Search_Results_T %>% filter(Unrel_contain > 0)
FM_Count_Above_Zero <- Search_Results_FM %>% filter(Unrel_contain > 0)


#Create matrix to plot:
Matrix_Dist <- matrix(c(nrow(True_Count_Zero)/nrow(Search_Results_T),'Zero','True',
                        nrow(FM_Count_Zero)/nrow(Search_Results_FM),'Zero','FM',
                        nrow(True_Count_Above_Zero)/nrow(Search_Results_T),'One or more','True',
                        nrow(FM_Count_Above_Zero)/nrow(Search_Results_FM),'One or more','FM'),ncol=3,byrow=T)


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
ggsave('./Figures/Study_5_Bar_Graph_Google_Search_wo_OG.png',height=12,width=12)


################################################################################################################

########################################## Figure 2c: Coefs_CIs_wo_OG.png ############################################

################################################################################################################

#Read in data:
Study_5_treat_data <- read.csv('./Data/Study_5_treat_data_wo_OG.csv')

#Create Treatment Data:
#Ideological Perspective of Articles:
FC_Ideo_Data <- read.csv('./Data/FC_Ideo_Data.csv')
FC_Ideo_Data$X <- NULL

#Pull in treatment data for Study 5:
T_Data <- Study_5_treat_data %>% filter(FC_Eval == 'FM')

#Filter only responses who only saw very reliable news sites in Google Search Results (85)
Treat_only_rel_data <- T_Data %>% filter(Total_Rel_85 > 0)

nrow(Treat_only_rel_data)/498

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

Treat_only_rel_data <- Treat_only_rel_data %>% select(Article_day,True_Dummy,Four_Ordinal,Seven_Ordinal,Age,Gender,ResponseId,Education_Score,Income_Score,Ideo_Congruence)
Treat_only_rel_data$ResponseId <- as.character(Treat_only_rel_data$ResponseId)
Control_Data$ResponseId <- as.character(Control_Data$ResponseId)
Treat_only_rel_data$Article_day <- as.character(Treat_only_rel_data$Article_day)

Treat_only_rel_data$Treatment <- 1


#Merge treatment and control articles:
Study_5_subset_1 <- rbind(Treat_only_rel_data,Control_Data)

Study_5_subset_1 <- Study_5_subset_1 %>% select(True_Dummy,Four_Ordinal,Seven_Ordinal,Treatment,Age,Ideo_Congruence,Education_Score,Gender,Income_Score,Article_day,ResponseId)

#Remove NAs
#Study_5_subset_1 <- na.omit(Study_5_subset_1)

#Run OLS Model with clustered standard errors:
fit_2_1 = feols(Seven_Ordinal ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=Study_5_subset_1)
CI_2_1 = confint(fit_2_1,se='twoway')

#Run OLS Model with clustered standard errors:
fit_2_2 = feols(Four_Ordinal ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=Study_5_subset_1)
CI_2_2 = confint(fit_2_2,se='twoway')

#Run OLS Model with clustered standard errors:
fit_2_3 = feols(True_Dummy ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=Study_5_subset_1)
CI_2_3 = confint(fit_2_3,se='twoway')

#Create empty matrix:
Fig_2c_Mat <- matrix(ncol=5)

#Merge data:
New_matr <- matrix(c(round(fit_2_1$coefficients[1]/sd(Study_5_subset_1$Seven_Ordinal),4),round(CI_2_1[1,1]/sd(Study_5_subset_1$Seven_Ordinal),4),round(CI_2_1[1,2]/sd(Study_5_subset_1$Seven_Ordinal),4),'Ordinal (7)','Only Very Reliable News',
                     round(fit_2_2$coefficients[1]/sd(Study_5_subset_1$Four_Ordinal),4),round(CI_2_2[1,1]/sd(Study_5_subset_1$Four_Ordinal),4),round(CI_2_2[1,2]/sd(Study_5_subset_1$Four_Ordinal),4),'Ordinal (4)','Only Very Reliable News',
                     round(fit_2_3$coefficients[1]/sd(Study_5_subset_1$True_Dummy),4),round(CI_2_3[1,1]/sd(Study_5_subset_1$True_Dummy),4),round(CI_2_3[1,2]/sd(Study_5_subset_1$True_Dummy),4),'True (Dummy)','Only Very Reliable News'),ncol=5,byrow=T)

Fig_2c_Mat <- rbind(Fig_2c_Mat,New_matr)


#Filter only responses who only saw very reliable news sites in Google Search Results (85)
T_Data <- T_Data %>% mutate(Prop_60 = Total_Unrel_60/Total_Links)
Test_dataframe <- T_Data %>% select(True_Dummy,Four_Ordinal,Seven_Ordinal,Prop_60,Total_Unrel_60,avg_score)
Treatment_unrel_data <- T_Data %>% filter(Prop_60 >= 0.1) #125
Treatment_unrel_data <- Treatment_unrel_data %>% filter(FC_Eval == 'FM')

#Select Variables:
Treatment_unrel_data <- Treatment_unrel_data %>% select(Article_day,True_Dummy,Four_Ordinal,Seven_Ordinal,Age,Gender,ResponseId,Education_Score,Income_Score,Ideo_Congruence)
Treatment_unrel_data$ResponseId <- as.character(Treatment_unrel_data$ResponseId)
Treatment_unrel_data$Article_day <- as.character(Treatment_unrel_data$Article_day)
Treatment_unrel_data$Treatment <- 1
Treatment_unrel_data <- na.omit(Treatment_unrel_data)

real_T_Data <- na.omit(T_Data)
nrow(Treatment_unrel_data)/nrow(real_T_Data)

#Merge treatment and control articles:
Study_5_subset_2 <- rbind(Treatment_unrel_data,Control_Data)
#Remove NAs
Study_5_subset_2 <- na.omit(Study_5_subset_2)

#Run OLS Model with clustered standard errors:
fit_2_1 = feols(Seven_Ordinal ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=Study_5_subset_2)
CI_2_1 = confint(fit_2_1,se='twoway')

#Run OLS Model with clustered standard errors:
fit_2_2 = feols(Four_Ordinal ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=Study_5_subset_2)
CI_2_2 = confint(fit_2_2,se='twoway')

#Run OLS Model with clustered standard errors:
fit_2_3 = feols(True_Dummy ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=Study_5_subset_2)
CI_2_3 = confint(fit_2_3,se='twoway')


#Merge data:
New_matr <- matrix(c(round(fit_2_1$coefficients[1]/sd(Study_5_subset_2$Seven_Ordinal),4),round(CI_2_1[1,1]/sd(Study_5_subset_2$Seven_Ordinal),4),round(CI_2_1[1,2]/sd(Study_5_subset_2$Seven_Ordinal),4),'Ordinal (7)','Some Unreliable News',
                     round(fit_2_2$coefficients[1]/sd(Study_5_subset_2$Four_Ordinal),4),round(CI_2_2[1,1]/sd(Study_5_subset_2$Four_Ordinal),4),round(CI_2_2[1,2]/sd(Study_5_subset_2$Four_Ordinal),4),'Ordinal (4)','Some Unreliable News',
                     round(fit_2_3$coefficients[1]/sd(Study_5_subset_2$True_Dummy),4),round(CI_2_3[1,1]/sd(Study_5_subset_2$True_Dummy),4),round(CI_2_3[1,2]/sd(Study_5_subset_2$True_Dummy),4),'True (Dummy)','Some Unreliable News'),ncol=5,byrow=T)

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
  theme(axis.title.x = element_text(size=20),
        axis.text.x  = element_text(size=26),
        axis.title.y = element_text(size=26),
        axis.text.y  = element_text(size=26),
        plot.title = element_text(size=26),
        legend.title = element_text(size=20),
        legend.text = element_text(size=20)) +
  ylim(-0.55,0.55) +
  scale_x_continuous("Type of News Returned by Google Search Engine \n",breaks=c(1.5,0.9),labels=c('At Least 10%\nof News URLs\nAre Unreliable',
                                                                                                   'Only Very Reliable\nNews Sources'),limits=c(0.5,2.0)) +
  coord_flip()

#Save Figure:
ggsave('./Figures/Coefs_CIs_wo_OG.png',height=12,width=10)

################################################################################################################

########################################## Figure 2d: Coefs_CIs_2_wo_OG.png ##########################################

################################################################################################################

#Filter by quartile:
#Filter only responses who only saw very reliable news sites in Google Search Results (85)
lowest_quartile_T_data <- T_Data %>% filter(avg_score < quantile(T_Data$avg_score,na.rm=T)[2])

#Select Variables:
lowest_quartile_T_data <- lowest_quartile_T_data %>% select(Article_day,True_Dummy,Four_Ordinal,Seven_Ordinal,Age,Gender,ResponseId,Education_Score,Income_Score,Ideo_Congruence)
lowest_quartile_T_data$ResponseId <- as.character(lowest_quartile_T_data$ResponseId)
lowest_quartile_T_data$Article_day <- as.character(lowest_quartile_T_data$Article_day)
lowest_quartile_T_data$Treatment <- 1

#Merge treatment and control articles:
lowest_quartile_all_data <- rbind(lowest_quartile_T_data,Control_Data)

#Remove NAs
lowest_quartile_all_data <- na.omit(lowest_quartile_all_data)

#Run OLS Model with clustered standard errors:
fit_2_1 = feols(Seven_Ordinal ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=lowest_quartile_all_data)
CI_2_1 = confint(fit_2_1,se='twoway')

#Run OLS Model with clustered standard errors:
fit_2_2 = feols(Four_Ordinal ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=lowest_quartile_all_data)
CI_2_2 = confint(fit_2_2,se='twoway')

#Run OLS Model with clustered standard errors:
fit_2_3 = feols(True_Dummy ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=lowest_quartile_all_data)
CI_2_3 = confint(fit_2_3,se='twoway')

#Create empty matrix:
Fig_2d_Mat <- matrix(ncol=5)

Fig_Poster <- matrix(ncol=4)

#Merge data:
New_matr <- matrix(c(round(fit_2_1$coefficients[1]/sd(lowest_quartile_all_data$Seven_Ordinal),4),round(CI_2_1[1,1]/sd(lowest_quartile_all_data$Seven_Ordinal),4),round(CI_2_1[1,2]/sd(lowest_quartile_all_data$Seven_Ordinal),4),'Ordinal (7)','0-25%',
                     round(fit_2_2$coefficients[1]/sd(lowest_quartile_all_data$Four_Ordinal),4),round(CI_2_2[1,1]/sd(lowest_quartile_all_data$Four_Ordinal),4),round(CI_2_2[1,2]/sd(lowest_quartile_all_data$Four_Ordinal),4),'Ordinal (4)','0-25%',
                     round(fit_2_3$coefficients[1]/sd(lowest_quartile_all_data$True_Dummy),4),round(CI_2_3[1,1]/sd(lowest_quartile_all_data$True_Dummy),4),round(CI_2_3[1,2]/sd(lowest_quartile_all_data$True_Dummy),4),'True (Dummy)','0-25%'),ncol=5,byrow=T)

Fig_2d_Mat <- rbind(Fig_2d_Mat,New_matr)

poster_matr <- matrix(c(round(fit_2_3$coefficients[1],4),round(CI_2_3[1,1],4),round(CI_2_3[1,2],4),'0-25%'),ncol=4,byrow=T)

Fig_Poster <- rbind(Fig_Poster,poster_matr)


#Filter by quartile:
Second_lowest_quartile_T_data <- T_Data %>% filter(avg_score >= quantile(T_Data$avg_score,na.rm=T)[2] & avg_score < quantile(T_Data$avg_score,na.rm=T)[3])

#Select Variables:
Second_lowest_quartile_T_data <- Second_lowest_quartile_T_data %>% select(Article_day,True_Dummy,Four_Ordinal,Seven_Ordinal,Age,Gender,ResponseId,Education_Score,Income_Score,Ideo_Congruence)
Second_lowest_quartile_T_data$ResponseId <- as.character(Second_lowest_quartile_T_data$ResponseId)
Second_lowest_quartile_T_data$Article_day <- as.character(Second_lowest_quartile_T_data$Article_day)
Second_lowest_quartile_T_data$Treatment <- 1

#Merge treatment and control articles:
Second_lowest_quartile_all_data <- rbind(Second_lowest_quartile_T_data,Control_Data)
#Remove NAs
Second_lowest_quartile_all_data <- na.omit(Second_lowest_quartile_all_data)

#Run OLS Model with clustered standard errors:
fit_2_1 = feols(Seven_Ordinal ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=Second_lowest_quartile_all_data)
CI_2_1 = confint(fit_2_1,se='twoway')

#Run OLS Model with clustered standard errors:
fit_2_2 = feols(Four_Ordinal ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=Second_lowest_quartile_all_data)
CI_2_2 = confint(fit_2_2,se='twoway')

#Run OLS Model with clustered standard errors:
fit_2_3 = feols(True_Dummy ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=Second_lowest_quartile_all_data)
CI_2_3 = confint(fit_2_3,se='twoway')

#Merge data:
New_matr <- matrix(c(round(fit_2_1$coefficients[1]/sd(Second_lowest_quartile_all_data$Seven_Ordinal),4),round(CI_2_1[1,1]/sd(Second_lowest_quartile_all_data$Seven_Ordinal),4),round(CI_2_1[1,2]/sd(Second_lowest_quartile_all_data$Seven_Ordinal),4),'Ordinal (7)','25-50%',
                     round(fit_2_2$coefficients[1]/sd(Second_lowest_quartile_all_data$Four_Ordinal),4),round(CI_2_2[1,1]/sd(Second_lowest_quartile_all_data$Four_Ordinal),4),round(CI_2_2[1,2]/sd(Second_lowest_quartile_all_data$Four_Ordinal),4),'Ordinal (4)','25-50%',
                     round(fit_2_3$coefficients[1]/sd(Second_lowest_quartile_all_data$True_Dummy),4),round(CI_2_3[1,1]/sd(Second_lowest_quartile_all_data$True_Dummy),4),round(CI_2_3[1,2]/sd(Second_lowest_quartile_all_data$True_Dummy),4),'True (Dummy)','25-50%'),ncol=5,byrow=T)

Fig_2d_Mat <- rbind(Fig_2d_Mat,New_matr)

poster_matr <- matrix(c(round(fit_2_3$coefficients[1],4),round(CI_2_3[1,1],4),round(CI_2_3[1,2],4),'25-50%'),ncol=4,byrow=T)

Fig_Poster <- rbind(Fig_Poster,poster_matr)


#Filter by quartile:
Third_lowest_quartile_T_data <- T_Data %>% filter(avg_score >= quantile(T_Data$avg_score,na.rm=T)[3] & avg_score < quantile(T_Data$avg_score,na.rm=T)[4])

#Select Variables:
Third_lowest_quartile_T_data <- Third_lowest_quartile_T_data %>% select(Article_day,True_Dummy,Four_Ordinal,Seven_Ordinal,Age,Gender,ResponseId,Education_Score,Income_Score,Ideo_Congruence)
Third_lowest_quartile_T_data$ResponseId <- as.character(Third_lowest_quartile_T_data$ResponseId)
Third_lowest_quartile_T_data$Article_day <- as.character(Third_lowest_quartile_T_data$Article_day)
Third_lowest_quartile_T_data$Treatment <- 1

#Merge treatment and control articles:
Third_lowest_quartile_all_data <- rbind(Third_lowest_quartile_T_data,Control_Data)
#Remove NAs
Third_lowest_quartile_all_data <- na.omit(Third_lowest_quartile_all_data)

#Run OLS Model with clustered standard errors:
fit_2_1 = feols(Seven_Ordinal ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=Third_lowest_quartile_all_data)
CI_2_1 = confint(fit_2_1,se='twoway')

#Run OLS Model with clustered standard errors:
fit_2_2 = feols(Four_Ordinal ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=Third_lowest_quartile_all_data)
CI_2_2 = confint(fit_2_2,se='twoway')

#Run OLS Model with clustered standard errors:
fit_2_3 = feols(True_Dummy ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=Third_lowest_quartile_all_data)
CI_2_3 = confint(fit_2_3,se='twoway')

#Merge data:
New_matr <- matrix(c(round(fit_2_1$coefficients[1]/sd(Third_lowest_quartile_all_data$Seven_Ordinal),4),round(CI_2_1[1,1]/sd(Third_lowest_quartile_all_data$Seven_Ordinal),4),round(CI_2_1[1,2]/sd(Third_lowest_quartile_all_data$Seven_Ordinal),4),'Ordinal (7)','50-75%',
                     round(fit_2_2$coefficients[1]/sd(Third_lowest_quartile_all_data$Four_Ordinal),4),round(CI_2_2[1,1]/sd(Third_lowest_quartile_all_data$Four_Ordinal),4),round(CI_2_2[1,2]/sd(Third_lowest_quartile_all_data$Four_Ordinal),4),'Ordinal (4)','50-75%',
                     round(fit_2_3$coefficients[1]/sd(Third_lowest_quartile_all_data$True_Dummy),4),round(CI_2_3[1,1]/sd(Third_lowest_quartile_all_data$True_Dummy),4),round(CI_2_3[1,2]/sd(Third_lowest_quartile_all_data$True_Dummy),4),'True (Dummy)','50-75%'),ncol=5,byrow=T)

Fig_2d_Mat <- rbind(Fig_2d_Mat,New_matr)

poster_matr <- matrix(c(round(fit_2_3$coefficients[1],4),round(CI_2_3[1,1],4),round(CI_2_3[1,2],4),'50-75%'),ncol=4,byrow=T)

Fig_Poster <- rbind(Fig_Poster,poster_matr)



#Filter by quartile:
highest_quartile_T_data <- T_Data %>% filter(avg_score >= quantile(T_Data$avg_score,na.rm=T)[4])

#Select Variables:
highest_quartile_T_data <- highest_quartile_T_data %>% select(Article_day,True_Dummy,Four_Ordinal,Seven_Ordinal,Age,Gender,ResponseId,Education_Score,Income_Score,Ideo_Congruence)
highest_quartile_T_data$ResponseId <- as.character(highest_quartile_T_data$ResponseId)
highest_quartile_T_data$Article_day <- as.character(highest_quartile_T_data$Article_day)
highest_quartile_T_data$Treatment <- 1

#Merge treatment and control articles:
highest_quartile_all_data <- rbind(highest_quartile_T_data,Control_Data)
#Remove NAs
highest_quartile_all_data <- na.omit(highest_quartile_all_data)


#Run OLS Model with clustered standard errors:
fit_2_1 = feols(Seven_Ordinal ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=highest_quartile_all_data)
CI_2_1 = confint(fit_2_1,se='twoway')

#Run OLS Model with clustered standard errors:
fit_2_2 = feols(Four_Ordinal ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=highest_quartile_all_data)
CI_2_2 = confint(fit_2_2,se='twoway')

#Run OLS Model with clustered standard errors:
fit_2_3 = feols(True_Dummy ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=highest_quartile_all_data)
CI_2_3 = confint(fit_2_3,se='twoway')

#Merge data:
New_matr <- matrix(c(round(fit_2_1$coefficients[1]/sd(highest_quartile_all_data$Seven_Ordinal),4),round(CI_2_1[1,1]/sd(highest_quartile_all_data$Seven_Ordinal),4),round(CI_2_1[1,2]/sd(highest_quartile_all_data$Seven_Ordinal),4),'Ordinal (7)','75-100%',
                     round(fit_2_2$coefficients[1]/sd(highest_quartile_all_data$Four_Ordinal),4),round(CI_2_2[1,1]/sd(highest_quartile_all_data$Four_Ordinal),4),round(CI_2_2[1,2]/sd(highest_quartile_all_data$Four_Ordinal),4),'Ordinal (4)','75-100%',
                     round(fit_2_3$coefficients[1]/sd(highest_quartile_all_data$True_Dummy),4),round(CI_2_3[1,1]/sd(highest_quartile_all_data$True_Dummy),4),round(CI_2_3[1,2]/sd(highest_quartile_all_data$True_Dummy),4),'True (Dummy)','75-100%'),ncol=5,byrow=T)

Fig_2d_Mat <- rbind(Fig_2d_Mat,New_matr)

poster_matr <- matrix(c(round(fit_2_3$coefficients[1],4),round(CI_2_3[1,1],4),round(CI_2_3[1,2],4),'75-100%'),ncol=4,byrow=T)

Fig_Poster <- rbind(Fig_Poster,poster_matr)




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
  theme(axis.title.x = element_text(size=20),
        axis.text.x  = element_text(size=26),
        axis.title.y = element_text(size=26),
        axis.text.y  = element_text(size=26),
        plot.title = element_text(size=26),
        legend.title = element_text(size=20),
        legend.text = element_text(size=20)) +
  ylim(-0.5,0.7) +
  scale_x_continuous("Quartile of News Quality Returned by Google Search Engine \n",breaks=c(2.7,2.1,1.5,0.9),labels=c('75-100%',
                                                                                                                       '50-75%',
                                                                                                                       '25-50%',
                                                                                                                       '0-25%'),limits=c(0.5,3.0)) +
  coord_flip()

#Save Figure:
ggsave('./Figures/Coefs_CIs_2_wo_OG.png',height=12,width=10)


################################################################################################################

################################# Figure 3a: Coefs_CIs_Predicting_Unrel_Dummy_wo_OG.png ###############################

################################################################################################################

#Merge treatment data from study 5 and the search results data:

T_Data <- Study_5_treat_data %>% filter(FC_Eval == 'FM')

#Run OLS Model with clustered standard errors:

Prop_Dummy_results = feols(Unrel_contain ~ Age + Gender + Education_Score + Income_Score + Ideo_Congruence +Dig_Lit_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=T_Data)
CI_Prop_Dummy = confint(Prop_Dummy_results,se='twoway')

#Create dataframe with coefficients and confidence intervals:
Coefficients <- c(round(Prop_Dummy_results$coefficients[1]*sd(T_Data$Age),4),
                  round(Prop_Dummy_results$coefficients[2]*sd(T_Data$Gender),4),
                  round(Prop_Dummy_results$coefficients[3]*sd(T_Data$Education_Score),4),
                  round(Prop_Dummy_results$coefficients[4]*sd(T_Data$Income_Score),4),
                  round(Prop_Dummy_results$coefficients[5]*sd(T_Data$Ideo_Congruence),4),
                  round(Prop_Dummy_results$coefficients[6]*sd(T_Data$Dig_Lit_Score),4))

#Create vector with upper confidence intervals:
CI_Upper <- c(round(CI_Prop_Dummy[1,1]*sd(T_Data$Age),4),
              round(CI_Prop_Dummy[2,1]*sd(T_Data$Gender),4),
              round(CI_Prop_Dummy[3,1]*sd(T_Data$Education_Score),4),
              round(CI_Prop_Dummy[4,1]*sd(T_Data$Income_Score),4),
              round(CI_Prop_Dummy[5,1]*sd(T_Data$Ideo_Congruence),4),
              round(CI_Prop_Dummy[6,1]*sd(T_Data$Dig_Lit_Score),4))            

#Create vector with lower confidence intervals:
CI_Lower <- c(round(CI_Prop_Dummy[1,2]*sd(T_Data$Age),4),
              round(CI_Prop_Dummy[2,2]*sd(T_Data$Gender),4),
              round(CI_Prop_Dummy[3,2]*sd(T_Data$Education_Score),4),
              round(CI_Prop_Dummy[4,2]*sd(T_Data$Income_Score),4),
              round(CI_Prop_Dummy[5,2]*sd(T_Data$Ideo_Congruence),4),
              round(CI_Prop_Dummy[6,2]*sd(T_Data$Dig_Lit_Score),4))           

#Create vector with variable names:
Coef_names <- c('Age',
                'Gender',
                'Education',
                'Income',
                'Ideological\nCongruence',
                'Digital Literacy')

#Create data matrix with data for figure:
d_matrix <- cbind(Coef_names,Coefficients,CI_Upper,CI_Lower)
rownames(d_matrix) <- c()
d_matrix <- data.frame(d_matrix)
d_matrix$Coefficients <- as.character(d_matrix$Coefficients)
d_matrix$CI_Lower <- as.character(d_matrix$CI_Lower)
d_matrix$CI_Upper <- as.character(d_matrix$CI_Upper)
d_matrix$Coefficients <- as.numeric(d_matrix$Coefficients)
d_matrix$CI_Lower <- as.numeric(d_matrix$CI_Lower)
d_matrix$CI_Upper <- as.numeric(d_matrix$CI_Upper)
d_matrix$x<-c(0.1,0.2,0.3,0.4,0.5,0.6)

#Produce plot:
ggplot(data = d_matrix, aes(x = x, y = Coefficients)) +
  geom_hline(aes(yintercept = 0), color = "gray",
             linetype = 2, size = 1.2) +
  geom_point(size=4) +
  geom_linerange(aes(min = CI_Lower,
                     max = CI_Upper),
                 size=1.5) +
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
  scale_x_continuous("Demographic Covariates \n",breaks=c(0.1,0.2,0.3,0.4,0.5,0.6),labels=Coef_names,limits=c(0.0,0.7)) +
  coord_flip()

#Save figure:
ggsave('./Figures/Coefs_CIs_Predicting_Unrel_Dummy_wo_OG.png',height=8,width=12)



################################################################################################################

################################# Figure 3b: Study_5_Bar_Graph_Google_Search_HL_wo_OG.png ############################

################################################################################################################

Headline_coding <- read.csv('./Data/Headline_coding_4_wo_OG.csv')

Headline_coding <- na.omit(Headline_coding)

#Create dataset of search queries that does not use headline/link:
SQ_No_Headline <- Headline_coding %>% filter(Headline_Link == 0)

#Create dataset of search queries that does use headline/link:
SQ_Headline_Data <- Headline_coding %>% filter(Headline_Link == 1)

#Create Count of individuals that didnt see any unreliable news sites:
True_Count_Zero <- SQ_No_Headline %>% filter(Unrel_contain == 0)
FM_Count_Zero <- SQ_Headline_Data %>% filter(Unrel_contain == 0)

#Create Count of individuals that saw at lesat some unreliable news sites:
True_Count_Above_Zero <- SQ_No_Headline %>% filter(Unrel_contain > 0)
FM_Count_Above_Zero <- SQ_Headline_Data %>% filter(Unrel_contain > 0)



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
ggsave('./Figures/Study_5_Bar_Graph_Google_Search_HL_wo_OG.png',height=12,width=12)


################################################################################################################

################################# Figure 3c: Coefs_CIs_Predicting_Headline_Link_wo_OG.png ############################

################################################################################################################


#Pull-in search data data:
Headline_coding <- read.csv('./Data/Headline_coding_4_wo_OG.csv')

Headline_coding <- na.omit(Headline_coding)

#Select variables needed:
Headline_coding <- Headline_coding %>% select(ResponseId,Article_day,Headline_Link,Age,Gender,Education_Score,Income_Score,Ideo_Congruence,Dig_Lit_Score)

#Run OLS Model with clustered standard errors:
Headline_results = feols(Headline_Link ~ Age + Gender + Education_Score + Income_Score + Ideo_Congruence +Dig_Lit_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=Headline_coding)
CI_Headline = confint(Headline_results)

#List of coefficients
Coefficients <- c(round(Headline_results$coefficients[1]*sd(Headline_coding$Age),4),
                  round(Headline_results$coefficients[2]*sd(Headline_coding$Gender),4),
                  round(Headline_results$coefficients[3]*sd(Headline_coding$Education_Score),4),
                  round(Headline_results$coefficients[4]*sd(Headline_coding$Income_Score),4),
                  round(Headline_results$coefficients[5]*sd(Headline_coding$Ideo_Congruence),4),
                  round(Headline_results$coefficients[6]*sd(Headline_coding$Dig_Lit_Score),4))

#Vector of upper confidence intervals:
CI_Upper <- c(round(CI_Headline[1,1]*sd(Headline_coding$Age),4),
              round(CI_Headline[2,1]*sd(Headline_coding$Gender),4),
              round(CI_Headline[3,1]*sd(Headline_coding$Education_Score),4),
              round(CI_Headline[4,1]*sd(Headline_coding$Income_Score),4),
              round(CI_Headline[5,1]*sd(Headline_coding$Ideo_Congruence),4),
              round(CI_Headline[6,1]*sd(Headline_coding$Dig_Lit_Score),4))            

#Vector of lower confidence intervals:
CI_Lower <- c(round(CI_Headline[1,2]*sd(Headline_coding$Age),4),
              round(CI_Headline[2,2]*sd(Headline_coding$Gender),4),
              round(CI_Headline[3,2]*sd(Headline_coding$Education_Score),4),
              round(CI_Headline[4,2]*sd(Headline_coding$Income_Score),4),
              round(CI_Headline[5,2]*sd(Headline_coding$Ideo_Congruence),4),
              round(CI_Headline[6,2]*sd(Headline_coding$Dig_Lit_Score),4))           

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
ggsave('./Figures/Coefs_CIs_Predicting_Headline_Link_wo_OG.png',height=8,width=12)

####################################################################################################

############# Determine whether treatment effect increases as fact-checkers agree more #############

####################################################################################################


#Read in data:
FCer_details <- read.csv('./Data/FCer_details.csv')

#Select certain data in the dataframe:
FCer_details <- FCer_details %>% select(Article_day,Prop_F)
FCer_details$Article_day <- as.character(FCer_details$Article_day)

###### Study 1:

Study_1_df <- read.csv('./Data/Study_1_df_FM.csv')
Study_1_df$Article_day <- as.character(Study_1_df$Article_day)
Study_1_df$ResponseId <- as.character(Study_1_df$ResponseId)
Study_1_df <- merge(Study_1_df,FCer_details,by='Article_day')

#Run OLS Model with clustered standard errors:
lin_results_fit_1_1 = feols(Susc_FN ~ Treat_Search*Prop_F + Treat_Search + Prop_F + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score,  cluster = ~Article_day+ResponseId,se="twoway", data = Study_1_df)
#Produce confidence intervals with clustered standard errors:
CI_1_1 <- confint(lin_results_fit_1_1)

#Run OLS Model with clustered standard errors:
lin_results_fit_1_2 = feols(Likert_Evaluation ~ Treat_Search*Prop_F + Treat_Search + Prop_F + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score, cluster = ~Article_day+ResponseId,se="twoway", data = Study_1_df)
#Produce confidence intervals with clustered standard errors:
CI_1_2 <- confint(lin_results_fit_1_2)

###### Study 2:

Study_2_df <- read.csv('./Data/Study_2_df_FM.csv')
Study_2_df$Article_day <- as.character(Study_2_df$Article_day)
Study_2_df$ResponseId <- as.character(Study_2_df$ResponseId)

Study_2_df <- merge(Study_2_df,FCer_details,by='Article_day')


#Run OLS Model with clustered standard errors:
lin_results_fit_2_1 = feols(Susc_FN ~ Treat_Search*Prop_F + Treat_Search + Prop_F + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score, cluster = ~Article_day+ResponseId,se="twoway", data = Study_2_df)
#Produce confidence intervals with clustered standard errors:
CI_2_1 <- confint(lin_results_fit_2_1)

#Run OLS Model with clustered standard errors:
lin_results_fit_2_2 = feols(Likert_Evaluation ~ Treat_Search*Prop_F + Treat_Search + Prop_F + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score, cluster = ~Article_day+ResponseId,se="twoway", data = Study_2_df)
#Produce confidence intervals with clustered standard errors:
CI_2_2 <- confint(lin_results_fit_2_2)

###### Study 3:
Study_3_df <- read.csv('./Data/Study_3_df_FM.csv')
Study_3_df$Article_day <- as.character(Study_3_df$Article_day)
Study_3_df$ResponseId <- as.character(Study_3_df$ResponseId)

Study_3_df <- merge(Study_3_df,FCer_details,by='Article_day')

#Run linear regression and produce coefficient values:
lin_results_fit_3_1 = feols(True_Dummy ~ Treatment*Prop_F + Treatment + Prop_F + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score, cluster = ~Article_day+ResponseId,se="twoway", data = Study_3_df)
#Produce confidence intervals using clustered standard errors:
CI_3_1 <- confint(lin_results_fit_3_1)

#Run linear regression and produce coefficient values:
lin_results_fit_3_2 = feols(Likert_Evaluation ~ Treatment*Prop_F + Treatment + Prop_F + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score, cluster = ~Article_day+ResponseId,se="twoway", data = Study_3_df)
#Produce confidence intervals using clustered standard errors:
CI_3_2 <- confint(lin_results_fit_3_2)

###### Study 4:
Study_4_df <- read.csv('./Data/Study_4_df_FM.csv')
Study_4_df$Article_day <- as.character(Study_4_df$Article_day)
Study_4_df$ResponseId <- as.character(Study_4_df$ResponseId)

Study_4_df <- merge(Study_4_df,FCer_details,by='Article_day')


#Run OLS Model with clustered standard errors:
lin_results_fit_4_1 = feols(Susc_FN ~ Treat_Search*Prop_F + Treat_Search + Prop_F  + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score, cluster = ~Article_day+ResponseId,se="twoway", data = Study_4_df)
#Produce confidence intervals with clustered standard errors:
CI_4_1 <- confint(lin_results_fit_4_1)

#Run OLS Model with clustered standard errors:
lin_results_fit_4_2 = feols(Likert_Evaluation ~ Treat_Search*Prop_F + Treat_Search + Prop_F  + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score, cluster = ~Article_day+ResponseId,se="twoway", data = Study_4_df)
#Produce confidence intervals with clustered standard errors:
CI_4_2 <- confint(lin_results_fit_4_2)

###### Study 5:
FM_Data_Study_5 <- read.csv('./Data/Study_5_df_FM.csv')
FM_Data_Study_5$Article_day <- as.character(FM_Data_Study_5$Article_day)
FM_Data_Study_5$ResponseId <- as.character(FM_Data_Study_5$ResponseId)

FM_Data_Study_5 <- merge(FM_Data_Study_5,FCer_details,by='Article_day')

#Run OLS Model with clustered standard errors:
lin_results_fit_5_1 = feols(True_Dummy ~ Treatment*Prop_F + Treatment + Prop_F + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score, cluster = ~Article_day+ResponseId,se="twoway", data = FM_Data_Study_5)
#Produce confidence intervals with clustered standard errors:
CI_5_1 <- confint(lin_results_fit_5_1)

#Run OLS Model with clustered standard errors:
lin_results_fit_5_2 = feols(Seven_Ordinal ~ Treatment*Prop_F + Treatment + Prop_F + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score, cluster = ~Article_day+ResponseId,se="twoway", data = FM_Data_Study_5)
#Produce confidence intervals with clustered standard errors:
CI_5_2 <- confint(lin_results_fit_5_2)

Final_Mat <- matrix(ncol=5)

New_matr <- matrix(c(round(lin_results_fit_1_1$coefficients[9],4),round(CI_1_1[9,1],4),round(CI_1_1[9,2],4),'Rate as True','Study 1',
                     round(lin_results_fit_1_2$coefficients[9],4),round(CI_1_2[9,1],4),round(CI_1_2[9,2],4),'Ordinal (7)','Study 1',
                     round(lin_results_fit_2_1$coefficients[9],4),round(CI_2_1[9,1],4),round(CI_2_1[9,2],4),'Rate as True','Study 2',
                     round(lin_results_fit_2_2$coefficients[9],4),round(CI_2_2[9,1],4),round(CI_2_2[9,2],4),'Ordinal (7)','Study 2',
                     round(lin_results_fit_3_1$coefficients[9],4),round(CI_3_1[9,1],4),round(CI_3_1[9,2],4),'Rate as True','Study 3',
                     round(lin_results_fit_3_2$coefficients[9],4),round(CI_3_2[9,1],4),round(CI_3_2[9,2],4),'Ordinal (7)','Study 3',
                     round(lin_results_fit_4_1$coefficients[9],4),round(CI_4_1[9,1],4),round(CI_4_1[9,2],4),'Rate as True','Study 4',
                     round(lin_results_fit_4_2$coefficients[9],4),round(CI_4_2[9,1],4),round(CI_4_2[9,2],4),'Ordinal (7)','Study 4',
                     round(lin_results_fit_5_1$coefficients[9],4),round(CI_5_1[9,1],4),round(CI_5_1[9,2],4),'Rate as True','Study 5',
                     round(lin_results_fit_5_2$coefficients[9],4),round(CI_5_2[9,1],4),round(CI_5_2[9,2],4),'Ordinal (7)','Study 5'),ncol=5,byrow=T)

Final_Mat <- rbind(Final_Mat,New_matr)

Final_Mat <- as.data.frame(Final_Mat)

Final_Mat <- na.omit(Final_Mat)

colnames(Final_Mat) <- c('Coef','Lower','Upper','Measure','Study')

Rate_T <- Final_Mat %>% filter(Measure == 'Rate as True')


Rate_T$x <- c(0.1,0.2,0.3,0.4,0.5)


Rate_T$Coef <- as.character(Rate_T$Coef)
Rate_T$Coef <- as.numeric(Rate_T$Coef)

Rate_T$Upper <- as.character(Rate_T$Upper)
Rate_T$Upper <- as.numeric(Rate_T$Upper)

Rate_T$Lower <- as.character(Rate_T$Lower)
Rate_T$Lower <- as.numeric(Rate_T$Lower)



ggplot(data = Rate_T, aes(x = x, y = Coef)) +
  geom_hline(aes(yintercept = 0), color = "gray",
             linetype = 2, size = 1.2) +
  geom_point(size=4) +
  geom_linerange(aes(min = Lower, 
                     max = Upper),
                 size=1.5) +
  ylab("\n Effect of Searching for Information Quality on Belief in Misinformation\n Dependent on Group \n(1 unit is 1 standard deviation of rating misinformation as true)") +
  theme_classic() +
  theme(axis.title.x = element_text(size=16),
        axis.text.x  = element_text(size=16),
        axis.title.y = element_text(size=16),
        axis.text.y  = element_text(size=16),
        plot.title = element_text(size = 16),
        legend.title = element_text(size=16),
        legend.text = element_text(size=14)) +
  ylim(-0.78,0.4) +
  scale_x_continuous("Study \n",breaks=c(0.1,0.2,0.3,0.4,0.5),labels=c('Study 1',
                                                                       'Study 2',
                                                                       'Study 3',
                                                                       'Study 4',
                                                                       'Study 5'),limits=c(0,0.6)) +
  coord_flip()


ggsave('./Figures/Interaction_By_Study_True.png',height=12,width=10)



Rate_Ordinal_7 <- Final_Mat %>% filter(Measure == 'Ordinal (7)')
Rate_Ordinal_7$x <- c(0.1,0.2,0.3,0.4,0.5)

Rate_Ordinal_7$Coef <- as.character(Rate_Ordinal_7$Coef)
Rate_Ordinal_7$Coef <- as.numeric(Rate_Ordinal_7$Coef)

Rate_Ordinal_7$Upper <- as.character(Rate_Ordinal_7$Upper)
Rate_Ordinal_7$Upper <- as.numeric(Rate_Ordinal_7$Upper)

Rate_Ordinal_7$Lower <- as.character(Rate_Ordinal_7$Lower)
Rate_Ordinal_7$Lower <- as.numeric(Rate_Ordinal_7$Lower)



ggplot(data = Rate_Ordinal_7, aes(x = x, y = Coef)) +
  geom_hline(aes(yintercept = 0), color = "gray",
             linetype = 2, size = 1.2) +
  geom_point(size=4) +
  geom_linerange(aes(min = Lower, 
                     max = Upper),
                 size=1.5) +
  ylab("\n Effect of Searching for Information Quality on Belief in Misinformation\n (7-Point Ordinal Scale)") +
  theme_classic() +
  theme(axis.title.x = element_text(size=16),
        axis.text.x  = element_text(size=16),
        axis.title.y = element_text(size=16),
        axis.text.y  = element_text(size=16),
        plot.title = element_text(size = 16),
        legend.title = element_text(size=16),
        legend.text = element_text(size=14)) +
  ylim(-3,1) +
  scale_x_continuous("Study \n",breaks=c(0.1,0.2,0.3,0.4,0.5),labels=c('Study 1',
                                                                       'Study 2',
                                                                       'Study 3',
                                                                       'Study 4',
                                                                       'Study 5'),limits=c(0,0.6)) +
  coord_flip()


ggsave('./Figures/Interaction_By_Study_Ordinal_7.png',height=12,width=10)


#Pooled
#Within
Study_2_df <- Study_2_df %>% select(Susc_FN,Likert_Evaluation,Treat_Search,Prop_F,Age,Dummy_Congruence,Education_Score,Gender,Income_Score,Article_day,ResponseId)
Study_3_df <- Study_3_df %>% select(Susc_FN,Likert_Evaluation,Treat_Search,Prop_F,Age,Dummy_Congruence,Education_Score,Gender,Income_Score,Article_day,ResponseId)
Study_4_df <- Study_4_df %>% select(Susc_FN,Likert_Evaluation,Treat_Search,Prop_F,Age,Dummy_Congruence,Education_Score,Gender,Income_Score,Article_day,ResponseId)

Study_2_df$Article_day <- as.character(Study_2_df$Article_day)
Study_2_df$ResponseId <- as.character(Study_2_df$ResponseId)
Study_3_df$Article_day <- as.character(Study_3_df$Article_day)
Study_3_df$ResponseId <- as.character(Study_3_df$ResponseId)
Study_4_df$Article_day <- as.character(Study_4_df$Article_day)
Study_4_df$ResponseId <- as.character(Study_4_df$ResponseId)

Within_Pooled_Interaction <- rbind(Study_2_df,Study_3_df,Study_4_df)

#Run OLS Model with clustered standard errors:
lin_results_fit_1_1 = feols(Susc_FN ~ Treat_Search*Prop_F + Treat_Search + Prop_F + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score,  cluster = ~Article_day+ResponseId,se="twoway", data = Within_Pooled_Interaction)
#Produce confidence intervals with clustered standard errors:
CI_1_1 <- confint(lin_results_fit_1_1)

#Run OLS Model with clustered standard errors:
lin_results_fit_1_2 = feols(Likert_Evaluation ~ Treat_Search*Prop_F + Treat_Search + Prop_F + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score, cluster = ~Article_day+ResponseId,se="twoway", data = Within_Pooled_Interaction)
#Produce confidence intervals with clustered standard errors:
CI_1_2 <- confint(lin_results_fit_1_2)


#Between
Study_1_df <- Study_1_df %>% select(Susc_FN,Treat_Search,Prop_F,Age,Dummy_Congruence,Education_Score,Gender,Income_Score,Article_day,ResponseId,Likert_Evaluation)
colnames(Study_1_df)[1] = 'True_Dummy'
colnames(Study_1_df)[2] = 'Treatment'
colnames(Study_1_df)[5] = 'Ideo_Congruence'

FM_Data_Study_5 <- FM_Data_Study_5 %>% select(True_Dummy,Treatment,Prop_F,Age,Ideo_Congruence,Education_Score,Gender,Income_Score,Article_day,ResponseId,Seven_Ordinal)
colnames(FM_Data_Study_5)[11] = 'Likert_Evaluation'


Study_1_df$Article_day <- as.character(Study_1_df$Article_day)
Study_1_df$ResponseId <- as.character(Study_1_df$ResponseId)

FM_Data_Study_5$Article_day <- as.character(FM_Data_Study_5$Article_day)
FM_Data_Study_5$ResponseId <- as.character(FM_Data_Study_5$ResponseId)

Between_Pooled_Interaction <- rbind(Study_1_df,FM_Data_Study_5)


#Run OLS Model with clustered standard errors:
lin_results_fit_2_1 = feols(True_Dummy ~ Treatment*Prop_F + Treatment + Prop_F + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score, cluster = ~Article_day+ResponseId,se="twoway", data = Between_Pooled_Interaction)
#Produce confidence intervals with clustered standard errors:
CI_2_1 <- confint(lin_results_fit_2_1)

#Run OLS Model with clustered standard errors:
lin_results_fit_2_2 = feols(Likert_Evaluation ~ Treatment*Prop_F + Treatment + Prop_F + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score, cluster = ~Article_day+ResponseId,se="twoway", data = Between_Pooled_Interaction)
#Produce confidence intervals with clustered standard errors:
CI_2_2 <- confint(lin_results_fit_2_2)

Final_Mat <- matrix(ncol=5)

New_matr <- matrix(c(round(lin_results_fit_1_1$coefficients[9],4),round(CI_1_1[9,1],4),round(CI_1_1[9,2],4),'Rate as True','Within',
                     round(lin_results_fit_1_2$coefficients[9],4),round(CI_1_2[9,1],4),round(CI_1_2[9,2],4),'Ordinal (7)','Within',
                     round(lin_results_fit_2_1$coefficients[9],4),round(CI_2_1[9,1],4),round(CI_2_1[9,2],4),'Rate as True','Between',
                     round(lin_results_fit_2_2$coefficients[9],4),round(CI_2_2[9,1],4),round(CI_2_2[9,2],4),'Ordinal (7)','Between'),ncol=5,byrow=T)

Final_Mat <- rbind(Final_Mat,New_matr)




Final_Mat <- as.data.frame(Final_Mat)

Final_Mat <- na.omit(Final_Mat)

colnames(Final_Mat) <- c('Coef','Lower','Upper','Measure','Study')

Rate_T <- Final_Mat %>% filter(Measure == 'Rate as True')


Rate_T$x <- c(0.1,0.2)


Rate_T$Coef <- as.character(Rate_T$Coef)
Rate_T$Coef <- as.numeric(Rate_T$Coef)

Rate_T$Upper <- as.character(Rate_T$Upper)
Rate_T$Upper <- as.numeric(Rate_T$Upper)

Rate_T$Lower <- as.character(Rate_T$Lower)
Rate_T$Lower <- as.numeric(Rate_T$Lower)



ggplot(data = Rate_T, aes(x = x, y = Coef)) +
  geom_hline(aes(yintercept = 0), color = "gray",
             linetype = 2, size = 1.2) +
  geom_point(size=4) +
  geom_linerange(aes(min = Lower, 
                     max = Upper),
                 size=1.5) +
  ylab("\n Effect of Searching for Information Quality on Rating Misinformation as True") +
  theme_classic() +
  theme(axis.title.x = element_text(size=16),
        axis.text.x  = element_text(size=16),
        axis.title.y = element_text(size=16),
        axis.text.y  = element_text(size=16),
        plot.title = element_text(size = 16),
        legend.title = element_text(size=16),
        legend.text = element_text(size=14)) +
  ylim(-0.6,0.4) +
  scale_x_continuous("Experiment Type \n",breaks=c(0.1,0.2),labels=c('Within',
                                                                     'Between'),limits=c(0,0.3)) +
  coord_flip()


ggsave('./Figures/Interaction_By_Study_True_Pooled.png',height=12,width=10)

Rate_Ordinal_7 <- Final_Mat %>% filter(Measure == 'Ordinal (7)')
Rate_Ordinal_7$x <- c(0.1,0.2)

Rate_Ordinal_7$Coef <- as.character(Rate_Ordinal_7$Coef)
Rate_Ordinal_7$Coef <- as.numeric(Rate_Ordinal_7$Coef)

Rate_Ordinal_7$Upper <- as.character(Rate_Ordinal_7$Upper)
Rate_Ordinal_7$Upper <- as.numeric(Rate_Ordinal_7$Upper)

Rate_Ordinal_7$Lower <- as.character(Rate_Ordinal_7$Lower)
Rate_Ordinal_7$Lower <- as.numeric(Rate_Ordinal_7$Lower)



ggplot(data = Rate_Ordinal_7, aes(x = x, y = Coef)) +
  geom_hline(aes(yintercept = 0), color = "gray",
             linetype = 2, size = 1.2) +
  geom_point(size=4) +
  geom_linerange(aes(min = Lower, 
                     max = Upper),
                 size=1.5) +
  ylab("\n Effect of Searching for Information Quality on Belief in Misinformation\n (7-Point Ordinal Scale)") +
  theme_classic() +
  theme(axis.title.x = element_text(size=16),
        axis.text.x  = element_text(size=16),
        axis.title.y = element_text(size=16),
        axis.text.y  = element_text(size=16),
        plot.title = element_text(size = 16),
        legend.title = element_text(size=16),
        legend.text = element_text(size=14)) +
  ylim(-1.28,0.75) +
  scale_x_continuous("Study \n",breaks=c(0.1,0.2),labels=c('Within',
                                                           'Between'),limits=c(0,0.3)) +
  coord_flip()


ggsave('./Figures/Interaction_By_Study_Ordinal_7_Pooled.png',height=12,width=10)


############################################################################################

###################################### Figure 16 ###########################################

############################################################################################


#Study 1:
T_Mainstream_Data_Study_1 <- read.csv('./Data/Study_1_Control_Only_Data_T_M.csv')
T_LQ_Data_Study_1 <- read.csv('./Data/Study_1_Control_Only_Data_T_LQ.csv')
FM_Data_Study_1 <- read.csv('./Data/Study_1_Control_Only_Data_FM.csv')

#Study 2:
T_Mainstream_Data_Study_2 <- read.csv('./Data/Study_2_Control_Only_Data_T_M.csv')
T_LQ_Data_Study_2 <- read.csv('./Data/Study_2_Control_Only_Data_T_LQ.csv')
FM_Data_Study_2 <- read.csv('./Data/Study_2_Control_Only_Data_FM.csv')

#Study 3:
T_Mainstream_Data_Study_3 <- read.csv('./Data/Study_3_Control_Only_Data_T_M.csv')
T_LQ_Data_Study_3 <- read.csv('./Data/Study_3_Control_Only_Data_T_LQ.csv')
FM_Data_Study_3 <- read.csv('./Data/Study_3_Control_Only_Data_FM.csv')


#Study 4:
T_Mainstream_Data_Study_4 <- read.csv('./Data/Study_4_Control_Only_Data_T_M.csv')
T_LQ_Data_Study_4 <- read.csv('./Data/Study_4_Control_Only_Data_T_LQ.csv')
FM_Data_Study_4 <- read.csv('./Data/Study_4_Control_Only_Data_FM.csv')

#STUDY 1:
mean(FM_Data_Study_1$Search_Online,na.rm=T)
mean(T_LQ_Data_Study_1$Search_Online)
mean(T_Mainstream_Data_Study_1$Search_Online)

#STUDY 2:
mean(FM_Data_Study_2$Search_Online,na.rm=T)
mean(T_LQ_Data_Study_2$Search_Online)
mean(T_Mainstream_Data_Study_2$Search_Online)

#STUDY 3:
mean(FM_Data_Study_3$Search_Online)
mean(T_LQ_Data_Study_3$Search_Online)
mean(T_Mainstream_Data_Study_3$Search_Online)

#STUDY 4:
mean(FM_Data_Study_4$Search_Online)
mean(T_LQ_Data_Study_4$Search_Online)
mean(T_Mainstream_Data_Study_4$Search_Online)


#Create Confidence Intervals
#Study 1:
data_1_1 <- summarySE(FM_Data_Study_1, measurevar="Search_Online",na.rm=T, conf.interval = 0.95)
data_1_2 <- summarySE(T_LQ_Data_Study_1, measurevar="Search_Online",na.rm=T, conf.interval = 0.95)
data_1_3 <- summarySE(T_Mainstream_Data_Study_1, measurevar="Search_Online",na.rm=T, conf.interval = 0.95)
data_1_1$Study <- 'Study 1'
data_1_2$Study <- 'Study 1'
data_1_3$Study <- 'Study 1'
data_1_1$Type <- 'False/Misleading'
data_1_2$Type <- 'True-Low Quality'
data_1_3$Type <- 'True-Mainstream'

#Study 2:
data_2_1 <- summarySE(FM_Data_Study_2, measurevar="Search_Online",na.rm=T, conf.interval = 0.95)
data_2_2 <- summarySE(T_LQ_Data_Study_2, measurevar="Search_Online",na.rm=T, conf.interval = 0.95)
data_2_3 <- summarySE(T_Mainstream_Data_Study_2, measurevar="Search_Online",na.rm=T, conf.interval = 0.95)
data_2_1$Study <- 'Study 2'
data_2_2$Study <- 'Study 2'
data_2_3$Study <- 'Study 2'
data_2_1$Type <- 'False/Misleading'
data_2_2$Type <- 'True-Low Quality'
data_2_3$Type <- 'True-Mainstream'

#Study 3:
data_3_1 <- summarySE(FM_Data_Study_3, measurevar="Search_Online",na.rm=T, conf.interval = 0.95)
data_3_2 <- summarySE(T_LQ_Data_Study_3, measurevar="Search_Online",na.rm=T, conf.interval = 0.95)
data_3_3 <- summarySE(T_Mainstream_Data_Study_3, measurevar="Search_Online",na.rm=T, conf.interval = 0.95)
data_3_1$Study <- 'Study 3'
data_3_2$Study <- 'Study 3'
data_3_3$Study <- 'Study 3'
data_3_1$Type <- 'False/Misleading'
data_3_2$Type <- 'True-Low Quality'
data_3_3$Type <- 'True-Mainstream'

#Study 4:
data_4_1 <- summarySE(FM_Data_Study_4, measurevar="Search_Online",na.rm=T, conf.interval = 0.95)
data_4_2 <- summarySE(T_LQ_Data_Study_4, measurevar="Search_Online",na.rm=T, conf.interval = 0.95)
data_4_3 <- summarySE(T_Mainstream_Data_Study_4, measurevar="Search_Online",na.rm=T, conf.interval = 0.95)
data_4_1$Study <- 'Study 4'
data_4_2$Study <- 'Study 4'
data_4_3$Study <- 'Study 4'
data_4_1$Type <- 'False/Misleading'
data_4_2$Type <- 'True-Low Quality'
data_4_3$Type <- 'True-Mainstream'



Misl_False_Data_bars <- rbind(data_1_1,data_1_2,data_1_3,
                              data_2_1,data_2_2,data_2_3,
                              data_3_1,data_3_2,data_3_3,
                              data_4_1,data_4_2,data_4_3)


ggplot(data = Misl_False_Data_bars, aes(x=as.factor(Study), y=Search_Online,fill=Type)) + 
  geom_bar(position=position_dodge(),
           stat="identity") +
  geom_errorbar(aes(ymin=Search_Online-ci, ymax=Search_Online+ci),
                width=.25,
                size = 1,
                position=position_dodge(0.9)) +
  coord_cartesian(ylim= c(0,0.2)) +
  ylab("Proportion That Self-Reported Searching Online\n to Help Them Evaluate an Articles\n") +
  scale_x_discrete(name='\nStudy',limits=c('Study 1','Study 2','Study 3','Study 4')) +
  theme_bw() + 
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(size=18),
        axis.text.x  = element_text(size=16),
        axis.title.y = element_text(size=18),
        axis.text.y  = element_text(size=16),
        title =element_text(size=20, face='bold'),
        legend.text = element_text(size=16))

ggsave("./Figures/Fig_Searching_Online.png", width = 9)





############################################################################################

###################################### Figure 15 ###########################################

############################################################################################


FM_Data_Study_1$Dig_Lit_Avg <- as.numeric(FM_Data_Study_1$Dig_Lit_Avg)

#Run OLS Model with clustered standard errors:
lin_results_fit_1 = feols(Search_Online ~  Age + Gender + Education_Score + Income_Score + Dummy_Congruence +  Dig_Lit_Avg  | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = FM_Data_Study_1)
#Produce confidence intervals with clustered standard errors:
CI_1 <- confint(lin_results_fit_1,se='twoway')

FM_Data_Study_2$Dig_Lit_Avg <- as.numeric(FM_Data_Study_2$Dig_Lit_Avg)

#Run OLS Model with clustered standard errors:
lin_results_fit_2 = feols(Search_Online ~  Age + Gender + Education_Score + Income_Score + Dummy_Congruence +  Dig_Lit_Avg  | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = FM_Data_Study_2)
#Produce confidence intervals with clustered standard errors:
CI_2 <- confint(lin_results_fit_2,se='twoway')

#Run OLS Model with clustered standard errors:
lin_results_fit_3 = feols(Search_Online ~  Age + Gender + Education_Score + Income_Score + Dummy_Congruence +  Dig_Lit_Avg  | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = FM_Data_Study_3)
#Produce confidence intervals with clustered standard errors:
CI_3 <- confint(lin_results_fit_3,se='twoway')

FM_Data_Study_4$Dig_Lit_Avg <- as.numeric(FM_Data_Study_4$Dig_Lit_Avg)

#Run OLS Model with clustered standard errors:
lin_results_fit_4 = feols(Search_Online ~  Age + Gender + Education_Score + Income_Score + Dummy_Congruence +  Dig_Lit_Avg | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = FM_Data_Study_4)
#Produce confidence intervals with clustered standard errors:
CI_4 <- confint(lin_results_fit_4,se='twoway')

#Create dataframe with coefficients and confidence intervals:
Coefficients <- c(round(lin_results_fit_1$coefficients[1]*sd(FM_Data_Study_1$Age),4),
                  round(lin_results_fit_1$coefficients[2]*sd(FM_Data_Study_1$Gender),4),
                  round(lin_results_fit_1$coefficients[3]*sd(FM_Data_Study_1$Education_Score),4),
                  round(lin_results_fit_1$coefficients[4]*sd(FM_Data_Study_1$Income_Score),4),
                  round(lin_results_fit_1$coefficients[5]*sd(FM_Data_Study_1$Dummy_Congruence,na.rm=T),4),
                  round(lin_results_fit_1$coefficients[6]*sd(FM_Data_Study_1$Dig_Lit_Avg),4))

#Create vector with upper confidence intervals:
CI_Upper <- c(round(CI_1[1,1]*sd(FM_Data_Study_1$Age),4),
              round(CI_1[2,1]*sd(FM_Data_Study_1$Gender),4),
              round(CI_1[3,1]*sd(FM_Data_Study_1$Education_Score),4),
              round(CI_1[4,1]*sd(FM_Data_Study_1$Income_Score),4),
              round(CI_1[5,1]*sd(FM_Data_Study_1$Dummy_Congruence,na.rm=T),4),
              round(CI_1[6,1]*sd(FM_Data_Study_1$Dig_Lit_Avg),4))            

#Create vector with lower confidence intervals:
CI_Lower <- c(round(CI_1[1,2]*sd(FM_Data_Study_1$Age),4),
              round(CI_1[2,2]*sd(FM_Data_Study_1$Gender),4),
              round(CI_1[3,2]*sd(FM_Data_Study_1$Education_Score),4),
              round(CI_1[4,2]*sd(FM_Data_Study_1$Income_Score),4),
              round(CI_1[5,2]*sd(FM_Data_Study_1$Dummy_Congruence,na.rm=T),4),
              round(CI_1[6,2]*sd(FM_Data_Study_1$Dig_Lit_Avg),4))           

#Create vector with variable names:
Coef_names <- c('Age',
                'Gender',
                'Education',
                'Income',
                'Ideological\nCongruence',
                'Digital Literacy')

#Create data matrix with data for figure:
d_matrix <- cbind(Coef_names,Coefficients,CI_Upper,CI_Lower)
rownames(d_matrix) <- c()
d_matrix <- data.frame(d_matrix)
d_matrix$Coefficients <- as.character(d_matrix$Coefficients)
d_matrix$CI_Lower <- as.character(d_matrix$CI_Lower)
d_matrix$CI_Upper <- as.character(d_matrix$CI_Upper)
d_matrix$Coefficients <- as.numeric(d_matrix$Coefficients)
d_matrix$CI_Lower <- as.numeric(d_matrix$CI_Lower)
d_matrix$CI_Upper <- as.numeric(d_matrix$CI_Upper)
d_matrix$x<-c(0.1,0.2,0.3,0.4,0.5,0.6)

#Produce plot:
ggplot(data = d_matrix, aes(x = x, y = Coefficients)) +
  geom_hline(aes(yintercept = 0), color = "gray",
             linetype = 2, size = 1.2) +
  geom_point(size=4) +
  geom_linerange(aes(min = CI_Lower,
                     max = CI_Upper),
                 size=1.5) +
  ylab("\n The Effect of a One Standard Deviation Increase of Ind. Variable\n on Probability of Searching Online without Encouragement") +
  theme_classic() +
  theme(axis.title.x = element_text(size=22),
        axis.text.x  = element_text(size=20),
        axis.title.y = element_text(size=20),
        axis.text.y  = element_text(size=22),
        plot.title = element_text(size = 20),
        legend.title = element_text(size=20),
        legend.text = element_text(size=20)) +
  ylim(-0.1,0.15) +
  scale_x_continuous("Demographic Covariates \n",breaks=c(0.1,0.2,0.3,0.4,0.5,0.6),labels=Coef_names,limits=c(0.0,0.7)) +
  coord_flip()

#Save figure:
ggsave('./Figures/Predicting_Search_Study_1.png',height=8,width=12)


#Study 2:

#Create dataframe with coefficients and confidence intervals:
Coefficients <- c(round(lin_results_fit_2$coefficients[1]*sd(FM_Data_Study_2$Age),4),
                  round(lin_results_fit_2$coefficients[2]*sd(FM_Data_Study_2$Gender),4),
                  round(lin_results_fit_2$coefficients[3]*sd(FM_Data_Study_2$Education_Score),4),
                  round(lin_results_fit_2$coefficients[4]*sd(FM_Data_Study_2$Income_Score),4),
                  round(lin_results_fit_2$coefficients[5]*sd(FM_Data_Study_2$Dummy_Congruence,na.rm=T),4),
                  round(lin_results_fit_2$coefficients[6]*sd(FM_Data_Study_2$Dig_Lit_Avg),4))

#Create vector with upper confidence intervals:
CI_Upper <- c(round(CI_2[1,1]*sd(FM_Data_Study_2$Age),4),
              round(CI_2[2,1]*sd(FM_Data_Study_2$Gender),4),
              round(CI_2[3,1]*sd(FM_Data_Study_2$Education_Score),4),
              round(CI_2[4,1]*sd(FM_Data_Study_2$Income_Score),4),
              round(CI_2[5,1]*sd(FM_Data_Study_2$Dummy_Congruence,na.rm=T),4),
              round(CI_2[6,1]*sd(FM_Data_Study_2$Dig_Lit_Avg),4))            

#Create vector with lower confidence intervals:
CI_Lower <- c(round(CI_2[1,2]*sd(FM_Data_Study_2$Age),4),
              round(CI_2[2,2]*sd(FM_Data_Study_2$Gender),4),
              round(CI_2[3,2]*sd(FM_Data_Study_2$Education_Score),4),
              round(CI_2[4,2]*sd(FM_Data_Study_2$Income_Score),4),
              round(CI_2[5,2]*sd(FM_Data_Study_2$Dummy_Congruence,na.rm=T),4),
              round(CI_2[6,2]*sd(FM_Data_Study_2$Dig_Lit_Avg),4))           

#Create vector with variable names:
Coef_names <- c('Age',
                'Gender',
                'Education',
                'Income',
                'Ideological\nCongruence',
                'Digital Literacy')

#Create data matrix with data for figure:
d_matrix <- cbind(Coef_names,Coefficients,CI_Upper,CI_Lower)
rownames(d_matrix) <- c()
d_matrix <- data.frame(d_matrix)
d_matrix$Coefficients <- as.character(d_matrix$Coefficients)
d_matrix$CI_Lower <- as.character(d_matrix$CI_Lower)
d_matrix$CI_Upper <- as.character(d_matrix$CI_Upper)
d_matrix$Coefficients <- as.numeric(d_matrix$Coefficients)
d_matrix$CI_Lower <- as.numeric(d_matrix$CI_Lower)
d_matrix$CI_Upper <- as.numeric(d_matrix$CI_Upper)
d_matrix$x<-c(0.1,0.2,0.3,0.4,0.5,0.6)

#Produce plot:
ggplot(data = d_matrix, aes(x = x, y = Coefficients)) +
  geom_hline(aes(yintercept = 0), color = "gray",
             linetype = 2, size = 1.2) +
  geom_point(size=4) +
  geom_linerange(aes(min = CI_Lower,
                     max = CI_Upper),
                 size=1.5) +
  ylab("\n The Effect of a One Standard Deviation Increase of Ind. Variable\n on Probability of Searching Online without Encouragement") +
  theme_classic() +
  theme(axis.title.x = element_text(size=22),
        axis.text.x  = element_text(size=20),
        axis.title.y = element_text(size=20),
        axis.text.y  = element_text(size=22),
        plot.title = element_text(size = 20),
        legend.title = element_text(size=20),
        legend.text = element_text(size=20)) +
  ylim(-0.1,0.15) +
  scale_x_continuous("Demographic Covariates \n",breaks=c(0.1,0.2,0.3,0.4,0.5,0.6),labels=Coef_names,limits=c(0.0,0.7)) +
  coord_flip()

#Save figure:
ggsave('./Figures/Predicting_Search_Study_2.png',height=8,width=12)




#Study 3:

#Create dataframe with coefficients and confidence intervals:
Coefficients <- c(round(lin_results_fit_3$coefficients[1]*sd(FM_Data_Study_3$Age),4),
                  round(lin_results_fit_3$coefficients[2]*sd(FM_Data_Study_3$Gender),4),
                  round(lin_results_fit_3$coefficients[3]*sd(FM_Data_Study_3$Education_Score),4),
                  round(lin_results_fit_3$coefficients[4]*sd(FM_Data_Study_3$Income_Score),4),
                  round(lin_results_fit_3$coefficients[5]*sd(FM_Data_Study_3$Dummy_Congruence,na.rm=T),4),
                  round(lin_results_fit_3$coefficients[6]*sd(FM_Data_Study_3$Dig_Lit_Avg),4))

#Create vector with upper confidence intervals:
CI_Upper <- c(round(CI_3[1,1]*sd(FM_Data_Study_3$Age),4),
              round(CI_3[2,1]*sd(FM_Data_Study_3$Gender),4),
              round(CI_3[3,1]*sd(FM_Data_Study_3$Education_Score),4),
              round(CI_3[4,1]*sd(FM_Data_Study_3$Income_Score),4),
              round(CI_3[5,1]*sd(FM_Data_Study_3$Dummy_Congruence,na.rm=T),4),
              round(CI_3[6,1]*sd(FM_Data_Study_3$Dig_Lit_Avg),4))            

#Create vector with lower confidence intervals:
CI_Lower <- c(round(CI_3[1,2]*sd(FM_Data_Study_3$Age),4),
              round(CI_3[2,2]*sd(FM_Data_Study_3$Gender),4),
              round(CI_3[3,2]*sd(FM_Data_Study_3$Education_Score),4),
              round(CI_3[4,2]*sd(FM_Data_Study_3$Income_Score),4),
              round(CI_3[5,2]*sd(FM_Data_Study_3$Dummy_Congruence,na.rm=T),4),
              round(CI_3[6,2]*sd(FM_Data_Study_3$Dig_Lit_Avg),4))           

#Create vector with variable names:
Coef_names <- c('Age',
                'Gender',
                'Education',
                'Income',
                'Ideological\nCongruence',
                'Digital Literacy')

#Create data matrix with data for figure:
d_matrix <- cbind(Coef_names,Coefficients,CI_Upper,CI_Lower)
rownames(d_matrix) <- c()
d_matrix <- data.frame(d_matrix)
d_matrix$Coefficients <- as.character(d_matrix$Coefficients)
d_matrix$CI_Lower <- as.character(d_matrix$CI_Lower)
d_matrix$CI_Upper <- as.character(d_matrix$CI_Upper)
d_matrix$Coefficients <- as.numeric(d_matrix$Coefficients)
d_matrix$CI_Lower <- as.numeric(d_matrix$CI_Lower)
d_matrix$CI_Upper <- as.numeric(d_matrix$CI_Upper)
d_matrix$x<-c(0.1,0.2,0.3,0.4,0.5,0.6)

#Produce plot:
ggplot(data = d_matrix, aes(x = x, y = Coefficients)) +
  geom_hline(aes(yintercept = 0), color = "gray",
             linetype = 2, size = 1.2) +
  geom_point(size=4) +
  geom_linerange(aes(min = CI_Lower,
                     max = CI_Upper),
                 size=1.5) +
  ylab("\n The Effect of a One Standard Deviation Increase of Ind. Variable\n on Probability of Searching Online without Encouragement") +
  theme_classic() +
  theme(axis.title.x = element_text(size=22),
        axis.text.x  = element_text(size=20),
        axis.title.y = element_text(size=20),
        axis.text.y  = element_text(size=22),
        plot.title = element_text(size = 20),
        legend.title = element_text(size=20),
        legend.text = element_text(size=20)) +
  ylim(-0.1,0.15) +
  scale_x_continuous("Demographic Covariates \n",breaks=c(0.1,0.2,0.3,0.4,0.5,0.6),labels=Coef_names,limits=c(0.0,0.7)) +
  coord_flip()

#Save figure:
ggsave('./Figures/Predicting_Search_Study_3.png',height=8,width=12)

#Study 4:
#Create dataframe with coefficients and confidence intervals:
Coefficients <- c(round(lin_results_fit_4$coefficients[1]*sd(FM_Data_Study_4$Age),4),
                  round(lin_results_fit_4$coefficients[2]*sd(FM_Data_Study_4$Gender),4),
                  round(lin_results_fit_4$coefficients[3]*sd(FM_Data_Study_4$Education_Score),4),
                  round(lin_results_fit_4$coefficients[4]*sd(FM_Data_Study_4$Income_Score),4),
                  round(lin_results_fit_4$coefficients[5]*sd(FM_Data_Study_4$Dummy_Congruence,na.rm=T),4),
                  round(lin_results_fit_4$coefficients[6]*sd(FM_Data_Study_4$Dig_Lit_Avg),4))

#Create vector with upper confidence intervals:
CI_Upper <- c(round(CI_4[1,1]*sd(FM_Data_Study_4$Age),4),
              round(CI_4[2,1]*sd(FM_Data_Study_4$Gender),4),
              round(CI_4[3,1]*sd(FM_Data_Study_4$Education_Score),4),
              round(CI_4[4,1]*sd(FM_Data_Study_4$Income_Score),4),
              round(CI_4[5,1]*sd(FM_Data_Study_4$Dummy_Congruence,na.rm=T),4),
              round(CI_4[6,1]*sd(FM_Data_Study_4$Dig_Lit_Avg),4))            

#Create vector with lower confidence intervals:
CI_Lower <- c(round(CI_4[1,2]*sd(FM_Data_Study_4$Age),4),
              round(CI_4[2,2]*sd(FM_Data_Study_4$Gender),4),
              round(CI_4[3,2]*sd(FM_Data_Study_4$Education_Score),4),
              round(CI_4[4,2]*sd(FM_Data_Study_4$Income_Score),4),
              round(CI_4[5,2]*sd(FM_Data_Study_4$Dummy_Congruence,na.rm=T),4),
              round(CI_4[6,2]*sd(FM_Data_Study_4$Dig_Lit_Avg),4))           

#Create vector with variable names:
Coef_names <- c('Age',
                'Gender',
                'Education',
                'Income',
                'Ideological\nCongruence',
                'Digital Literacy')

#Create data matrix with data for figure:
d_matrix <- cbind(Coef_names,Coefficients,CI_Upper,CI_Lower)
rownames(d_matrix) <- c()
d_matrix <- data.frame(d_matrix)
d_matrix$Coefficients <- as.character(d_matrix$Coefficients)
d_matrix$CI_Lower <- as.character(d_matrix$CI_Lower)
d_matrix$CI_Upper <- as.character(d_matrix$CI_Upper)
d_matrix$Coefficients <- as.numeric(d_matrix$Coefficients)
d_matrix$CI_Lower <- as.numeric(d_matrix$CI_Lower)
d_matrix$CI_Upper <- as.numeric(d_matrix$CI_Upper)
d_matrix$x<-c(0.1,0.2,0.3,0.4,0.5,0.6)

#Produce plot:
ggplot(data = d_matrix, aes(x = x, y = Coefficients)) +
  geom_hline(aes(yintercept = 0), color = "gray",
             linetype = 2, size = 1.2) +
  geom_point(size=4) +
  geom_linerange(aes(min = CI_Lower,
                     max = CI_Upper),
                 size=1.5) +
  ylab("\n The Effect of a One Standard Deviation Increase of Ind. Variable\n on Probability of Searching Online without Encouragement") +
  theme_classic() +
  theme(axis.title.x = element_text(size=22),
        axis.text.x  = element_text(size=20),
        axis.title.y = element_text(size=20),
        axis.text.y  = element_text(size=22),
        plot.title = element_text(size = 20),
        legend.title = element_text(size=20),
        legend.text = element_text(size=20)) +
  ylim(-0.15,0.15) +
  scale_x_continuous("Demographic Covariates \n",breaks=c(0.1,0.2,0.3,0.4,0.5,0.6),labels=Coef_names,limits=c(0.0,0.7)) +
  coord_flip()

#Save figure:
ggsave('./Figures/Predicting_Search_Study_4.png',height=8,width=12)





############################################################################################

###################################### Figure ##############################################

############################################################################################


#Run OLS Model with clustered standard errors:
lin_results_fit_1 = feols(True_Dummy ~ Search_Online + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = FM_Data_Study_1)
#Produce confidence intervals with clustered standard errors:
CI_1 <- confint(lin_results_fit_1,se='twoway')

#Run OLS Model with clustered standard errors:
lin_results_fit_2 = feols(True_Dummy ~ Search_Online + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = FM_Data_Study_2)
#Produce confidence intervals with clustered standard errors:
CI_2 <- confint(lin_results_fit_2,se='twoway')


#Run OLS Model with clustered standard errors:
lin_results_fit_3 = feols(True_Dummy ~ Search_Online + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = FM_Data_Study_3)
#Produce confidence intervals with clustered standard errors:
CI_3 <- confint(lin_results_fit_3,se='twoway')

#Run OLS Model with clustered standard errors:
lin_results_fit_4 = feols(True_Dummy ~ Search_Online + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = FM_Data_Study_4)
#Produce confidence intervals with clustered standard errors:
CI_4 <- confint(lin_results_fit_4,se='twoway')




FM_Data_Study_1 <- FM_Data_Study_1 %>% select(True_Dummy,Likert_Evaluation,Search_Online,Age,Dummy_Congruence,Education_Score,Gender,Income_Score,Article_day,ResponseId)
FM_Data_Study_2 <- FM_Data_Study_2 %>% select(True_Dummy,Likert_Evaluation,Search_Online,Age,Dummy_Congruence,Education_Score,Gender,Income_Score,Article_day,ResponseId)
FM_Data_Study_3 <- FM_Data_Study_3 %>% select(True_Dummy,Likert_Evaluation,Search_Online,Age,Dummy_Congruence,Education_Score,Gender,Income_Score,Article_day,ResponseId)
FM_Data_Study_4 <- FM_Data_Study_4 %>% select(True_Dummy,Likert_Evaluation,Search_Online,Age,Dummy_Congruence,Education_Score,Gender,Income_Score,Article_day,ResponseId)


pooled <- rbind(FM_Data_Study_1,FM_Data_Study_2,FM_Data_Study_3,FM_Data_Study_4)

lin_results_fit_5 = feols(True_Dummy ~ Search_Online + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = pooled)
#Produce confidence intervals with clustered standard errors:
CI_5 <- confint(lin_results_fit_5,se='twoway')


#Create dataframe with coefficients and confidence intervals:
Coefficients <- c(round(lin_results_fit_1$coefficients[1],4),
                  round(lin_results_fit_2$coefficients[1],4),
                  round(lin_results_fit_3$coefficients[1],4),
                  round(lin_results_fit_4$coefficients[1],4),
                  round(lin_results_fit_5$coefficients[1],4))

#Create vector with upper confidence intervals:
CI_Upper <- c(round(CI_1[1,1],4),
              round(CI_2[1,1],4),
              round(CI_3[1,1],4),
              round(CI_4[1,1],4),
              round(CI_5[1,1],4))            

#Create vector with lower confidence intervals:
CI_Lower <- c(round(CI_1[1,2],4),
              round(CI_2[1,2],4),
              round(CI_3[1,2],4),
              round(CI_4[1,2],4),
              round(CI_5[1,2],4))          

#Create vector with variable names:
Coef_names <- c('Study 1',
                'Study 2',
                'Study 3',
                'Study 4',
                'Pooled')

#Create data matrix with data for figure:
d_matrix <- cbind(Coef_names,Coefficients,CI_Upper,CI_Lower)
rownames(d_matrix) <- c()
d_matrix <- data.frame(d_matrix)
d_matrix$Coefficients <- as.character(d_matrix$Coefficients)
d_matrix$CI_Lower <- as.character(d_matrix$CI_Lower)
d_matrix$CI_Upper <- as.character(d_matrix$CI_Upper)
d_matrix$Coefficients <- as.numeric(d_matrix$Coefficients)
d_matrix$CI_Lower <- as.numeric(d_matrix$CI_Lower)
d_matrix$CI_Upper <- as.numeric(d_matrix$CI_Upper)
d_matrix$x<-c(0.1,0.2,0.3,0.4,0.5)

#Produce plot:
ggplot(data = d_matrix, aes(x = x, y = Coefficients)) +
  geom_hline(aes(yintercept = 0), color = "gray",
             linetype = 2, size = 1.2) +
  geom_point(size=4) +
  geom_linerange(aes(min = CI_Lower,
                     max = CI_Upper),
                 size=1.5) +
  ylab("\n The Effect of Searching Online Without Encouragement\n on Rating a False/Misleading Article as True") +
  theme_classic() +
  theme(axis.title.x = element_text(size=22),
        axis.text.x  = element_text(size=20),
        axis.title.y = element_text(size=20),
        axis.text.y  = element_text(size=22),
        plot.title = element_text(size = 20),
        legend.title = element_text(size=20),
        legend.text = element_text(size=20)) +
  ylim(-0.1,0.3) +
  scale_x_continuous("Demographic Covariates \n",breaks=c(0.1,0.2,0.3,0.4,0.5),labels=Coef_names,limits=c(0.0,0.6)) +
  coord_flip()

#Save figure:
ggsave('./Figures/Search_Effect_wo_encouragement_True_Dummy.png',height=8,width=12)






#Run OLS Model with clustered standard errors:
lin_results_fit_1 = feols(Likert_Evaluation ~ Search_Online + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = FM_Data_Study_1)
#Produce confidence intervals with clustered standard errors:
CI_1 <- confint(lin_results_fit_1,se='twoway')

#Run OLS Model with clustered standard errors:
lin_results_fit_2 = feols(Likert_Evaluation ~ Search_Online + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = FM_Data_Study_2)
#Produce confidence intervals with clustered standard errors:
CI_2 <- confint(lin_results_fit_2,se='twoway')


#Run OLS Model with clustered standard errors:
lin_results_fit_3 = feols(Likert_Evaluation ~ Search_Online + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = FM_Data_Study_3)
#Produce confidence intervals with clustered standard errors:
CI_3 <- confint(lin_results_fit_3,se='twoway')

#Run OLS Model with clustered standard errors:
lin_results_fit_4 = feols(Likert_Evaluation ~ Search_Online + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = FM_Data_Study_4)
#Produce confidence intervals with clustered standard errors:
CI_4 <- confint(lin_results_fit_4,se='twoway')



lin_results_fit_5 = feols(Likert_Evaluation ~ Search_Online + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = pooled)
#Produce confidence intervals with clustered standard errors:
CI_5 <- confint(lin_results_fit_5,se='twoway')


#Create dataframe with coefficients and confidence intervals:
Coefficients <- c(round(lin_results_fit_1$coefficients[1],4),
                  round(lin_results_fit_2$coefficients[1],4),
                  round(lin_results_fit_3$coefficients[1],4),
                  round(lin_results_fit_4$coefficients[1],4),
                  round(lin_results_fit_5$coefficients[1],4))

#Create vector with upper confidence intervals:
CI_Upper <- c(round(CI_1[1,1],4),
              round(CI_2[1,1],4),
              round(CI_3[1,1],4),
              round(CI_4[1,1],4),
              round(CI_5[1,1],4))            

#Create vector with lower confidence intervals:
CI_Lower <- c(round(CI_1[1,2],4),
              round(CI_2[1,2],4),
              round(CI_3[1,2],4),
              round(CI_4[1,2],4),
              round(CI_5[1,2],4))          

#Create vector with variable names:
Coef_names <- c('Study 1',
                'Study 2',
                'Study 3',
                'Study 4',
                'Pooled')

#Create data matrix with data for figure:
d_matrix <- cbind(Coef_names,Coefficients,CI_Upper,CI_Lower)
rownames(d_matrix) <- c()
d_matrix <- data.frame(d_matrix)
d_matrix$Coefficients <- as.character(d_matrix$Coefficients)
d_matrix$CI_Lower <- as.character(d_matrix$CI_Lower)
d_matrix$CI_Upper <- as.character(d_matrix$CI_Upper)
d_matrix$Coefficients <- as.numeric(d_matrix$Coefficients)
d_matrix$CI_Lower <- as.numeric(d_matrix$CI_Lower)
d_matrix$CI_Upper <- as.numeric(d_matrix$CI_Upper)
d_matrix$x<-c(0.1,0.2,0.3,0.4,0.5)

#Produce plot:
ggplot(data = d_matrix, aes(x = x, y = Coefficients)) +
  geom_hline(aes(yintercept = 0), color = "gray",
             linetype = 2, size = 1.2) +
  geom_point(size=4) +
  geom_linerange(aes(min = CI_Lower,
                     max = CI_Upper),
                 size=1.5) +
  ylab("\n The Effect of Searching Online Without Encouragement\n on Rating a False/Misleading Article as True\n(7-Point Ordinal Scale)") +
  theme_classic() +
  theme(axis.title.x = element_text(size=22),
        axis.text.x  = element_text(size=20),
        axis.title.y = element_text(size=20),
        axis.text.y  = element_text(size=22),
        plot.title = element_text(size = 20),
        legend.title = element_text(size=20),
        legend.text = element_text(size=20)) +
  ylim(-0.5,1.52) +
  scale_x_continuous("Demographic Covariates \n",breaks=c(0.1,0.2,0.3,0.4,0.5),labels=Coef_names,limits=c(0.0,0.6)) +
  coord_flip()

#Save figure:
ggsave('./Figures/Search_Effect_wo_encouragement_Ordinal_7.png',height=8,width=12)



############################################################################################

################################# Section W Cosine Similarity ##############################

############################################################################################


#Read-in data:
Article_data_1 <- read.csv('./Data/All_Search_Results_Combined.csv')
Article_data_1 <- Article_data_1 %>% select(Article_day,Article_Lean)
Article_data_1 <- unique(Article_data_1)

#Read-in data:
Article_data_2 <- read.csv('./Data/Study_5_Articles.csv')
Article_data_2 <- Article_data_2 %>% select(Article_day,Headline)
Article_data_2 <- unique(Article_data_2)

#Merge data:
Article_data <- merge(Article_data_1,Article_data_2,by='Article_day')

#Read-in data:
Search_data <- read.csv('./Data/Headline_Coding_4.csv')

#Merge data:
Search_data <- merge(Search_data,Article_data,by='Article_day')

Search_data$Search_Term <- as.character(Search_data$Search_Term)
Search_data$Article_day <- as.character(Search_data$Article_day)

Search_data <- na.omit(Search_data)

get_cos_sim <- function(corpus) {
  # pre-process corpus
  doc <- corpus %>%
    VectorSource %>%
    tm::VCorpus()
  # get term frequency matrix
  tfm <- doc %>%
    DocumentTermMatrix(
      control = corpus %>% list(
        removePunctuation = TRUE,
        wordLengths = c(1, Inf),
        weighting = weightTf)) %>%
    as.matrix()
  # get row-wise similarity
  sim <- NULL
  for(i in 1:nrow(tfm)) {
    sim_i <- apply(
      X = tfm, 
      MARGIN = 1, 
      FUN = lsa::cosine, 
      tfm[i,])
    sim <- rbind(sim, sim_i)
  }
  # set identity diagonal to zero
  diag(sim) <- 0
  # label and return
  rownames(sim) <- corpus
  return(sim)
}

Article_list <- c("Day_1_1",
                  "Day_5_1",
                  "Day_6_1",
                  "Day_11_1",
                  "Day_12_1",
                  "Day_3_2",
                  "Day_5_2",
                  "Day_1_3",
                  "Day_4_3",
                  "Day_5_3",
                  "Day_9_3",
                  "Day_10_3",
                  "Day_11_3",
                  "Day_12_3",
                  "Day_5_4",
                  "Day_6_4")

search_term_analysis <- matrix(ncol=31)

colnames(search_term_analysis) <- c('Article_day','Headline','Article_lean','average_cosine_similarity_liberals','average_cosine_similarity_conservatives','average_cosine_similarity_moderates','average_cosine_similarity_everyone','average_characters_in_search_query_liberals','average_characters_in_search_query_conservatives','average_characters_in_search_query_moderates','average_characters_in_search_query_everyone',
                                    'most_popular_word_liberals','most_popular_word_conservatives','most_popular_word_moderates','most_popular_word_everyone','proportion_searches_with_popular_word_liberals','proportion_searches_with_popular_word_conservatives','proportion_searches_with_popular_word_moderates','proportion_searches_with_popular_word_everyone',
                                    'most_popular_search_query_liberals','most_popular_search_query_conservatives','most_popular_search_query_moderates','most_popular_search_query_everyone','proportion_searches_with_popular_query_liberals','proportion_searches_with_popular_query_conservatives','proportion_searches_with_popular_query_moderates','proportion_searches_with_popular_query_everyone',
                                    'total_num_searches_liberals','total_num_searches_conservatives','total_num_searches_moderates','total_num_searches_everyone')


stopWords <- stopwords("en")
class(stopWords)

Search_data$Headline <- as.character(Search_data$Headline)
Search_data$Article_Lean <- as.character(Search_data$Article_Lean)

Search_data$Search_Term <- tolower(Search_data$Search_Term)

i=2

for(i in 1:length(Article_list)){
  Search_Term_Day <- Search_data %>% filter(Article_day == Article_list[i])
  Liberal_Day <- Search_Term_Day %>% filter(Dummy_Ideology == 'Liberal')
  Conservative_Day <- Search_Term_Day %>% filter(Dummy_Ideology == 'Conservative')
  Moderate_Day <- Search_Term_Day %>% filter(Dummy_Ideology == 'Moderate')
  #Average cosine similarity between search keywords:
  mean_lib_day <- mean(get_cos_sim(Liberal_Day$Search_Term))
  mean_con_day <- mean(get_cos_sim(Conservative_Day$Search_Term))
  mean_mod_day <- mean(get_cos_sim(Moderate_Day$Search_Term))
  mean_all_day <- mean(get_cos_sim(Search_Term_Day$Search_Term))
  #Average characters in search query:
  mean_ch_lib_day <- mean(nchar(Liberal_Day$Search_Term))
  mean_ch_con_day <- mean(nchar(Conservative_Day$Search_Term))
  mean_ch_mod_day <- mean(nchar(Moderate_Day$Search_Term))
  mean_ch_all_day <- mean(nchar(Search_Term_Day$Search_Term))
  #Number of search_queries:
  n_lib_search <- nrow(Liberal_Day)
  n_con_search <- nrow(Conservative_Day)
  n_mod_search <- nrow(Moderate_Day)
  n_all_search <- nrow(Search_Term_Day)
  #Most popular word used by different groups:
  words <- unlist(strsplit(Liberal_Day$Search_Term,"[[:space:]]"),recursive = FALSE)
  words <- words[!(words %in% stopWords)]
  words <- sub("%22", "",  words)
  words <- sub("%3a", "",  words)
  l_t <- table(words)
  most_pop_word_l <- names(l_t[l_t==max(l_t)])
  perc_lib_word = max(l_t)/n_lib_search
  if(length(most_pop_word_l) > 1){
    most_pop_word_l <- paste(most_pop_word_l, collapse = " | ")
  }
  words <- unlist(strsplit(Conservative_Day$Search_Term,"[[:space:]]"),recursive = FALSE)
  words <- words[!(words %in% stopWords)]
  words <- sub("%22", "",  words)
  words <- sub("%3a", "",  words)
  c_t <- table(words)
  most_pop_word_c <- names(c_t[c_t==max(c_t)])
  perc_con_word = max(c_t)/n_con_search
  if(length(most_pop_word_c) > 1){
    most_pop_word_c <- paste0(most_pop_word_c, collapse = " | ")
  }
  words <- unlist(strsplit(Moderate_Day$Search_Term,"[[:space:]]"),recursive = FALSE)
  words <- words[!(words %in% stopWords)]
  words <- sub("%22", "",  words)
  words <- sub("%3a", "",  words)
  m_t <- table(words)
  most_pop_word_m <- names(m_t[m_t==max(m_t)])
  perc_mod_word = max(m_t)/n_mod_search
  if(length(most_pop_word_m) > 1){
    most_pop_word_m <- paste(most_pop_word_m, collapse = " | ")
  }
  words <- unlist(strsplit(Search_Term_Day$Search_Term,"[[:space:]]"),recursive = FALSE)
  words <- words[!(words %in% stopWords)]
  words <- sub("%22", "",  words)
  words <- sub("%3a", "",  words)
  a_t <- table(words)
  most_pop_word_a <- names(a_t[a_t==max(a_t)])
  perc_all_word = max(a_t)/n_all_search
  if(length(most_pop_word_a) > 1){
    most_pop_word_a <- paste(most_pop_word_a, collapse = " | ")
  }
  #Most popular search query:
  l_t <- table(Liberal_Day$Search_Term)
  most_pop_search_l <- names(l_t[l_t==max(l_t)])
  perc_lib_search = max(l_t)/n_lib_search
  if(length(most_pop_search_l) > 1){
    most_pop_search_l <- paste(most_pop_search_l, collapse = " | ")
  }
  c_t <- table(Conservative_Day$Search_Term)
  most_pop_search_c <- names(c_t[c_t==max(c_t)])
  perc_con_search = max(c_t)/n_con_search
  if(length(most_pop_search_c) > 1){
    most_pop_search_c <- paste0(most_pop_search_c, collapse = " | ")
  }
  m_t <- table(Moderate_Day$Search_Term)
  most_pop_search_m <- names(m_t[m_t==max(m_t)])
  perc_mod_search = max(m_t)/n_mod_search
  if(length(most_pop_search_m) > 1){
    most_pop_search_m <- paste(most_pop_search_m, collapse = " | ")
  }
  a_t <- table(Search_Term_Day$Search_Term)
  most_pop_search_a <- names(a_t[a_t==max(a_t)])
  perc_all_search = max(a_t)/n_all_search
  if(length(most_pop_search_a) > 1){
    most_pop_search_a <- paste(most_pop_search_a, collapse = " | ")
  }  
  Article_lean <- Search_Term_Day$Article_Lean[i]
  Article_d <- Article_list[i]
  Article_headline <- Search_Term_Day$Headline[i]
  search_term_analysis <- rbind(search_term_analysis,matrix(c(Article_d,Article_headline,Article_lean,mean_lib_day,mean_con_day,mean_mod_day,mean_all_day,mean_ch_lib_day,mean_ch_con_day,mean_ch_mod_day,mean_ch_all_day,most_pop_word_l,most_pop_word_c,most_pop_word_m,most_pop_word_a,perc_lib_word,perc_con_word,perc_mod_word,perc_all_word,
                                                              most_pop_search_l,most_pop_search_c,most_pop_search_m,most_pop_search_a,perc_lib_search,perc_con_search,perc_mod_search,perc_all_search,n_lib_search,n_con_search,n_mod_search,n_all_search),ncol=31))
}

search_term_analysis <- as.data.frame(search_term_analysis)

search_term_analysis <- search_term_analysis %>% select(Headline,Article_lean,average_cosine_similarity_liberals,
                                                        average_cosine_similarity_conservatives,
                                                        average_cosine_similarity_moderates,
                                                        average_cosine_similarity_everyone)

colnames(search_term_analysis) <- c('Headline','Article_Lean',
                                    'Similarity_Among_Liberals',
                                    'Similarity_Among_Conservatives',
                                    'Similarity_Among_Moderates',
                                    'Similarity_Among_Everyone')

search_term_analysis$Similarity_Among_Liberals <- as.character(search_term_analysis$Similarity_Among_Liberals)
search_term_analysis$Similarity_Among_Conservatives <- as.character(search_term_analysis$Similarity_Among_Conservatives)
search_term_analysis$Similarity_Among_Moderates <- as.character(search_term_analysis$Similarity_Among_Moderates)
search_term_analysis$Similarity_Among_Everyone <- as.character(search_term_analysis$Similarity_Among_Everyone)

search_term_analysis$Similarity_Among_Liberals <- as.numeric(search_term_analysis$Similarity_Among_Liberals)
search_term_analysis$Similarity_Among_Conservatives <- as.numeric(search_term_analysis$Similarity_Among_Conservatives)
search_term_analysis$Similarity_Among_Moderates <- as.numeric(search_term_analysis$Similarity_Among_Moderates)
search_term_analysis$Similarity_Among_Everyone <- as.numeric(search_term_analysis$Similarity_Among_Everyone)

search_term_analysis$Similarity_Among_Liberals <- round(search_term_analysis$Similarity_Among_Liberals,3)
search_term_analysis$Similarity_Among_Conservatives <- round(search_term_analysis$Similarity_Among_Conservatives,3)
search_term_analysis$Similarity_Among_Moderates <- round(search_term_analysis$Similarity_Among_Moderates,3)
search_term_analysis$Similarity_Among_Everyone <- round(search_term_analysis$Similarity_Among_Everyone,3)

search_term_analysis <- na.omit(search_term_analysis)

#search_term_analysis$Headlines <- c("AZ State Senator Demands Biden Electors Be Recalled, New Election Be Held After Shocking Audit Revelations",
#               "Enthusiasm for Trump's rally in Arizona dwarfs President Biden's town hall just days earlier",
#               "Ashli Babbitt's Mom: Nancy Pelosi Orchestrated the Killing of My Daughter",
#               "Foreign News Media Laughs at Joe Biden: He 'Needs a Retirement Home and a Warm Bowl of Soup'",
#               "Indictment shows it was Hillary who colluded with Russia",
#               "Looks like New York prosecutors have a witness directly incriminating Donald Trump",
#               "Rand Paul is left speechless after Dr. Fauci tears into him for lying in Senate hearing",
#               "This Is Worrying Me Quite A Bit: mRNA Vaccine Inventor Shares Viral Thread Showing COVID Surge In Most-Vaxxed Countries",
#               "Zero COVID Catastrophe: Participating Nations See New Records Across the Board",
#               "Chicago Chamber Of Commerce Rages As Average Unemployed Illinoisan Parent Earns \$35/Hour Sitting On The Couch",
#               "NIH Director Shredded Over Risky Research In Wuhan After CNN Interview Goes Sideways",
#               "U.S. faces engineered famine as COVID lockdowns and vax mandates could lead to widespread hunger, unrest this winter",
#               "'Falsified Data': Pfizer Vaccine Trial Had Major Flaws, Whistleblower Tells Peer-Reviewed Journal",
#               "Biden Gang Reportedly Running Two Secret Lists Used to Prevent Outspoken Conservatives from Owning Guns – Laura Loomer Speaks Out on This Injustice",
#               "Banks: Pelosi Doesn't Want 'Tough Questions' Because She's Responsible for Breakdown of Security on Jan. 6'",
#               "Trump Rejects 'Fake' Jan. 6 Panel: 'Will Nancy Investigate Herself?'")




xt <- xtable(search_term_analysis,
             digits=3,
             caption = "Cosine Similarity of Search Queries by False/Misleading Articles",
             align=c("|p{1cm}","|p{11cm}",
                     "|p{2cm}","|p{2cm}",
                     "|p{2cm}","|p{2cm}","|p{2cm}|"))

#Name Columns:
names(xt) <- c('Headline','Article Lean',
               'Similarity Among Liberals',
               'Similarity Among Conservatives',
               'Similarity Among Moderates',
               'Similarity Among Everyone')

#Write Table:
write(print(xt,include.rownames=FALSE,
            caption.placement = "top",
            sanitize.colnames.function = identity),file='./Tables/Table_Cosine_Similarity.txt')


############################################################################################

################################# Section Z: Search Terms ##################################

############################################################################################

######## LaTex Table:

Search_Term_df <- read.csv('./Data/All_Search_Results_Combined.csv')
Search_Term_Day <- na.omit(Search_Term_Day)
Search_Term_Day <- Search_Term_df %>% filter(Article_day == 'Day_10_3')


Search_Term_Day <- Search_Term_Day %>% select(Search_Term,
                                              Unrel_contain,
                                              Ideo_Congruence,
                                              Dig_Lit_Score)

Search_Term_Day <- na.omit(Search_Term_Day)

Search_Term_Day$Search_Term <- as.character(Search_Term_Day$Search_Term)
Search_Term_Day$Unrel_contain <- as.numeric(Search_Term_Day$Unrel_contain)
Search_Term_Day$Ideo_Congruence  <- as.numeric(Search_Term_Day$Ideo_Congruence)
Search_Term_Day$Dig_Lit_Score <- as.numeric(Search_Term_Day$Dig_Lit_Score)

Search_Term_Day$Unrel_contain <- round(Search_Term_Day$Unrel_contain,3)
Search_Term_Day$Ideo_Congruence <- round(Search_Term_Day$Ideo_Congruence,0)
Search_Term_Day$Dig_Lit_Score <- round(Search_Term_Day$Dig_Lit_Score,0)




xt <- xtable(Search_Term_Day,
             digits=0,
             caption = "Search Terms and Quality of Information Returned",
             align=c("|p{1cm}","|p{11cm}",
                     "|p{3cm}","|p{3cm}","|p{3cm}|"))

#Name Columns:
names(xt) <- c('Search Term',
               'Exposed to Unreliable Result',
               'Ideologically Congruent',
               'Digital Literacy Score')

#Write Table:
write(print(xt,include.rownames=FALSE,
            caption.placement = "top",
            sanitize.colnames.function = identity),file='./Tables/Table_Article_1.txt')

######## LaTex Table 2:

Search_Term_df <- read.csv('./Data/All_Search_Results_Combined.csv')
Search_Term_Day <- Search_Term_df %>% filter(Article_day == 'Day_3_2')


Search_Term_Day <- Search_Term_Day %>% select(Search_Term,
                                              Unrel_contain,
                                              Ideo_Congruence,
                                              Dig_Lit_Score)

Search_Term_Day <- na.omit(Search_Term_Day)

Search_Term_Day$Search_Term <- as.character(Search_Term_Day$Search_Term)
Search_Term_Day$Unrel_contain <- as.numeric(Search_Term_Day$Unrel_contain)
Search_Term_Day$Ideo_Congruence  <- as.numeric(Search_Term_Day$Ideo_Congruence)
Search_Term_Day$Dig_Lit_Score <- as.numeric(Search_Term_Day$Dig_Lit_Score)

Search_Term_Day$Unrel_contain <- round(Search_Term_Day$Unrel_contain,3)
Search_Term_Day$Ideo_Congruence <- round(Search_Term_Day$Ideo_Congruence,0)
Search_Term_Day$Dig_Lit_Score <- round(Search_Term_Day$Dig_Lit_Score,0)




xt <- xtable(Search_Term_Day,
             digits=0,
             caption = "Search Terms and Quality of Information Returned",
             align=c("|p{1cm}","|p{11cm}",
                     "|p{3cm}","|p{3cm}","|p{3cm}|"))

#Name Columns:
names(xt) <- c('Search Term',
               'Exposed to Unreliable Result',
               'Ideologically Congruent',
               'Digital Literacy Score')

#Write Table:
write(print(xt,include.rownames=FALSE,
            caption.placement = "top",
            sanitize.colnames.function = identity),file='./Tables/Table_Article_2.txt')



writeLines(capture.output(sessionInfo()), "sessionInfo_suppl_mat.txt")



