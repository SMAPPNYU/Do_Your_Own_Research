
#Author: Kevin Aslett
#Code Title: Supplementary_Materials.R
#Paper Title: Online Searches to Evaluate Misinformation Can Increase Its Perceived Veracity
#Purpose of code: Generate all figures and tables that are located in the supplementary materials of the paper.

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
#Figure 2: Pred_Categ_5_Studies.jpg
#Figure 3a: All_4_Studies_Ordinal_Robust.jpg
#Figure 3b: All_4_Studies_Categorical_Robust.jpg
#Figure 4a: Study_5_Bar_Graph_Google_Search_ROBUST.jpg
#Figure 4b: Study_5_1_ROBUST.jpg
#Figure 4c: Coefs_CIs_ROBUST.jpg
#Figure 4d: Coefs_CIs_2_ROBUST.jpg
#Figure 5a: Coefs_CIs_Predicting_Unrel_Dummy_ROBUST.jpg
#Figure 5b: Study_5_Bar_Graph_Google_Search_HL_ROBUST.jpg
#Figure 5c: Coefs_CIs_Predicting_Headline_Link_ROBUST.jpg
#Figure 6a: T_FM_Fig_1A_True_Dummy_ROBUST.jpg
#Figure 6b: Fig_1A_True_Dummy_ROBUST.jpg
#Figure 6c: Types_Fig_1A_True_Dummy_ROBUST.jpg
#Figure 7a: T_FM_Fig_1A_True_Ordinal.jpg
#Figure 7b: Fig_1A_True_Ordinal.jpg
#Figure 7c: Types_Fig_1A_True_Ordinal.jpg
#Figure 8a: All_4_Studies_Categorical_Preregistration.jpg
#Figure 8b: All_4_Studies_Ordinal_Preregistration.jpg
#Figure 9: Different_Treatment_True_Dummy_All.jpg
#Figure 10: Different_Treatment_Four_Ordinal_All.jpg
#Figure 11: Different_Treatment_Seven_Ordinal_All.jpg
#Figure 12a: Study_5_Bar_Graph_Google_Search_wo_OG.jpg
#Figure 12c: Coefs_CIs_wo_OG.jpg
#Figure 12d: Coefs_CIs_2_wo_OG.jpg
#Figure 13a: Coefs_CIs_Predicting_Unrel_Dummy_wo_OG.jpg
#Figure 13b: Study_5_Bar_Graph_Google_Search_HL_wo_OG.jpg
#Figure 13c: Coefs_CIs_Predicting_Headline_Link_wo_OG.jpg
#Figure 14a: Interaction_By_Study_True.jpg
#Figure 14b: Interaction_By_Study_Ordinal_7.jpg
#Figure 14c: Interaction_By_Study_True_Pooled.jpg
#Figure 14d: Interaction_By_Study_Ordinal_7_Pooled.jpg
#Figure 15: Fig_Searching_Online.jpg
#Figure 16a: Predicting_Search_Study_1.jpg
#Figure 16b: Predicting_Search_Study_2.jpg
#Figure 16c: Predicting_Search_Study_3.jpg
#Figure 16d: Predicting_Search_Study_4.jpg
#Figure 17: Search_Effect_wo_encouragement_True_Dummy.jpg
#Figure 18: Search_Effect_wo_encouragement_Ordinal_7.jpg
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

#Table 1: Balance_1.txt
#Table 2: Balance_2.txt
#Table 3: Balance_3.txt
#Table 4: Balance_4.txt
#Table 5: Balance_5.txt
#Table 6: Balance_6.txt
#Table 7: Balance_7.txt
#Table 8: Balance_8.txt

#Readin Libraries:
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
library(ggpubr)

#Set color palettes to use:
cbbPalette_1 <- c("#009E73","#E69F00","#56B4E9","#F0E442", "#0072B2", "#D55E00", "#CC79A7")
cbbPalette_2 <- c("#E69F00","#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
cbbPalette_3 <- c("#E69F00", "#009E73","#56B4E9", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

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
       title='Results from OLS Regression Results Presented in Figure 1a and 1b (Study 1). This table presents the average treatment effects for linear regression models testing the effect of SOTEN. All effects are estimated using ordinary least squares (OLS) with article fixed effects and standard errors clustered at the individual and article level.')

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
       title='Results from OLS Regression Results Presented in Figure 1a and 1b (Study 2). This table presents the average treatment effects for linear regression models testing the effect of SOTEN. All effects are estimated using ordinary least squares (OLS) with article fixed effects and standard errors clustered at the individual and article level.')


#Pull in Study 3 data:
Study_3_df <- read.csv('./Data/Study_3_df_FM.csv')
Study_3_df$Article_day <- as.character(Study_3_df$Article_day)
Study_3_df$ResponseId <- as.character(Study_3_df$ResponseId)

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
       signif.code= c(`***` = 0.001, `**` = 0.01, `*` = 0.05),
       file='./Tables/Study_3.txt',
       replace=TRUE,
       title='Results from OLS Regression Results Presented in Figure 1a and 1b (Study 3). This table presents the average treatment effects for linear regression models testing the effect of SOTEN. All effects are estimated using ordinary least squares (OLS) with article fixed effects and standard errors clustered at the individual and article level.')


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
       title='Results from OLS Regression Results Presented in Figure 1a and 1b (Study 4). This table presents the average treatment effects for linear regression models testing the effect of SOTEN. All effects are estimated using ordinary least squares (OLS) with article fixed effects and standard errors clustered at the individual and article level.')


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
       title='Results from OLS Regression Results Presented in Figure 2b. This table presents the average treatment effects for linear regression models testing the effect of SOTEN. All effects are estimated using ordinary least squares (OLS) with article fixed effects and standard errors clustered at the individual and article level.')


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
       title='Results from OLS Regression Results Presented in Figure 2c (Only Very Reliable News Returned). This table presents the average treatment effects for linear regression models testing the effect of SOTEN. All effects are estimated using ordinary least squares (OLS) with article fixed effects and standard errors clustered at the individual and article level.',
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
       title='Results from OLS Regression Results Presented in Figure 2c (Some Unreliable News Returned). This table presents the average treatment effects for linear regression models testing the effect of SOTEN. All effects are estimated using ordinary least squares (OLS) with article fixed effects and standard errors clustered at the individual and article level.',
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
       title='Results from OLS Regression Results Presented in Figure 2d (0-25 Percentage Quartile of News Quality). This table presents the average treatment effects for linear regression models testing the effect of SOTEN. All effects are estimated using ordinary least squares (OLS) with article fixed effects and standard errors clustered at the individual and article level.',
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
       title='Results from OLS Regression Results Presented in Figure 2d (25-50 Percentage Quartile of News Quality). This table presents the average treatment effects for linear regression models testing the effect of SOTEN. All effects are estimated using ordinary least squares (OLS) with article fixed effects and standard errors clustered at the individual and article level.',
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
       title='Results from OLS Regression Results Presented in Figure 2d (50-75 Percentage Quartile of News Quality). This table presents the average treatment effects for linear regression models testing the effect of SOTEN. All effects are estimated using ordinary least squares (OLS) with article fixed effects and standard errors clustered at the individual and article level.',
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
       title='Results from OLS Regression Results Presented in Figure 2d (75-100 Percentage Quartile of News Quality). This table presents the average treatment effects for linear regression models testing the effect of SOTEN. All effects are estimated using ordinary least squares (OLS) with article fixed effects and standard errors clustered at the individual and article level.',
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
       title='Results from OLS Regression Results Presented in Figure 3a. This table presents effects for a linear regression model. All effects are estimated using ordinary least squares (OLS) with article fixed effects and standard errors clustered at the individual and article level.',
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

#Remove searches without a news quality score:
Headline_coding <- na.omit(Headline_coding)

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
       title='Predicted Use of Headline or URL as a Search Query when Searching Online about Misinformation. All effects are estimated using ordinary least squares (OLS) with article fixed effects and standard errors clustered at the individual and article level.')

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
       title='Results from OLS Regression Results Presented in Figure 4a (True Articles - Study 1). This table presents the average treatment effects for linear regression models testing the effect of SOTEN. All effects are estimated using ordinary least squares (OLS) with article fixed effects and standard errors clustered at the individual and article level.')


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
       title='Results from OLS Regression Results Presented in Figure 4a (False/Misleading Articles - Study 1). This table presents the average treatment effects for linear regression models testing the effect of SOTEN. All effects are estimated using ordinary least squares (OLS) with article fixed effects and standard errors clustered at the individual and article level.')

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
       title='Results from OLS Regression Results Presented in Figure 4a (True Articles - Study 2). This table presents the average treatment effects for linear regression models testing the effect of SOTEN. All effects are estimated using ordinary least squares (OLS) with article fixed effects and standard errors clustered at the individual and article level.')


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
       title='Results from OLS Regression Results Presented in Figure 4a (False/Misleading Articles - Study 2). This table presents the average treatment effects for linear regression models testing the effect of SOTEN. All effects are estimated using ordinary least squares (OLS) with article fixed effects and standard errors clustered at the individual and article level.')

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
       title='Results from OLS Regression Results Presented in Figure 4a (True Articles - Study 3). This table presents the average treatment effects for linear regression models testing the effect of SOTEN. All effects are estimated using ordinary least squares (OLS) with article fixed effects and standard errors clustered at the individual and article level.')

#Read in data - false/misleading articles (Study 3):
Study_3_False_M <- read.csv('./Data/Study_3_df_FM.csv')

#Run linear regression and produce coefficient values:
lin_results_fit_3_3_1 = feols(True_Dummy ~ Treatment + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Study_3_False_M)


#Write Table:
etable(lin_results_fit_3_3_1, tex = TRUE,
       signifCode= c(`***` = 0.001, `**` = 0.01, `*` = 0.05),
       file='./Tables/Figure_4_A_6.txt',
       replace=TRUE,
       title='Results from OLS Regression Results Presented in Figure 4a (False/Misleading Articles - Study 3). This table presents the average treatment effects for linear regression models testing the effect of SOTEN. All effects are estimated using ordinary least squares (OLS) with article fixed effects and standard errors clustered at the individual and article level.')



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
       title='Results from OLS Regression Results Presented in Figure 4a (True Articles - Study 4). This table presents the average treatment effects for linear regression models testing the effect of SOTEN. All effects are estimated using ordinary least squares (OLS) with article fixed effects and standard errors clustered at the individual and article level.')


#Study 4 (False Misleading Articles)
Study_4_False_M <- read.csv('./Data/Study_4_df_FM.csv')

#Run OLS Model with clustered standard errors:
lin_results_fit_4_3_1 = feols(Susc_FN ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Study_4_False_M)


#Write Table:
etable(lin_results_fit_4_3_1, tex = TRUE,
       signifCode= c(`***` = 0.001, `**` = 0.01, `*` = 0.05),
       file='./Tables/Figure_4_A_8.txt',
       replace=TRUE,
       title='Results from OLS Regression Results Presented in Figure 4a (False/Misleading Articles - Study 4). This table presents the average treatment effects for linear regression models testing the effect of SOTEN. All effects are estimated using ordinary least squares (OLS) with article fixed effects and standard errors clustered at the individual and article level.')


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
       title='Results from OLS Regression Results Presented in Figure 4a (True Articles - Study 5). This table presents the average treatment effects for linear regression models testing the effect of SOTEN. All effects are estimated using ordinary least squares (OLS) with article fixed effects and standard errors clustered at the individual and article level.')


#Study 5 (False/Misleading Articles):

Study_5_False_M <- read.csv('./Data/Study_5_df_FM.csv')

#Run OLS Model with clustered standard errors:
lin_results_fit_5_3_1 = feols(True_Dummy ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Study_5_False_M)

#Write Table:
etable(lin_results_fit_5_3_1, tex = TRUE,
       signifCode= c(`***` = 0.001, `**` = 0.01, `*` = 0.05),
       file='./Tables/Figure_4_A_10.txt',
       replace=TRUE,
       title='Results from OLS Regression Results Presented in Figure 4a (False/Misleading Articles - Study 5). This table presents the average treatment effects for linear regression models testing the effect of SOTEN. All effects are estimated using ordinary least squares (OLS) with article fixed effects and standard errors clustered at the individual and article level.')

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
       title='Results from OLS Regression Results Presented in Figure 4b (True Low Quality Articles - Study 1). This table presents the average treatment effects for linear regression models testing the effect of SOTEN. All effects are estimated using ordinary least squares (OLS) with article fixed effects and standard errors clustered at the individual and article level.')



#True Mainstream Articles (Study 1):
#Run OLS Model with clustered standard errors:
lin_results_fit_1_2_1 = feols(True_Dummy ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = T_Mainstream_Data_Study_1)

#Write Table:
etable(lin_results_fit_1_2_1, tex = TRUE,
       signifCode= c(`***` = 0.001, `**` = 0.01, `*` = 0.05),
       file='./Tables/Figure_4b_2.txt',
       replace=TRUE,
       title='Results from OLS Regression Results Presented in Figure 4b (True Mainstream Articles - Study 1). This table presents the average treatment effects for linear regression models testing the effect of SOTEN. All effects are estimated using ordinary least squares (OLS) with article fixed effects and standard errors clustered at the individual and article level.')



#False/Misleading Articles (Study 1):
#Run OLS Model with clustered standard errors:
lin_results_fit_1_3_1 = feols(Susc_FN ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Misl_False_Search_MF)


#Write Table:
etable(lin_results_fit_1_3_1, tex = TRUE,
       signifCode= c(`***` = 0.001, `**` = 0.01, `*` = 0.05),
       file='./Tables/Figure_4b_3.txt',
       replace=TRUE,
       title='Results from OLS Regression Results Presented in Figure 4b (False/Misleading Articles - Study 1). This table presents the average treatment effects for linear regression models testing the effect of SOTEN. All effects are estimated using ordinary least squares (OLS) with article fixed effects and standard errors clustered at the individual and article level.')



#True Low-Quality Articles (Study 2):
#Run OLS Model with clustered standard errors:
lin_results_fit_2_1_1 = feols(True_Dummy ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = T_LQ_Data_Study_2)

#Write Table:
etable(lin_results_fit_2_1_1, tex = TRUE,
       signifCode= c(`***` = 0.001, `**` = 0.01, `*` = 0.05),
       file='./Tables/Figure_4b_4.txt',
       replace=TRUE,
       title='Results from OLS Regression Results Presented in Figure 4b (True Low Quality Articles - Study 2). This table presents the average treatment effects for linear regression models testing the effect of SOTEN. All effects are estimated using ordinary least squares (OLS) with article fixed effects and standard errors clustered at the individual and article level.')





#True Mainstream Articles (Study 2):
#Run OLS Model with clustered standard errors:
lin_results_fit_2_2_1 = feols(True_Dummy ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = T_Mainstream_Data_Study_2)

#Write Table:
etable(lin_results_fit_2_2_1, tex = TRUE,
       signifCode= c(`***` = 0.001, `**` = 0.01, `*` = 0.05),
       file='./Tables/Figure_4b_5.txt',
       replace=TRUE,
       title='Results from OLS Regression Results Presented in Figure 4b (True Mainstream Articles - Study 2). This table presents the average treatment effects for linear regression models testing the effect of SOTEN. All effects are estimated using ordinary least squares (OLS) with article fixed effects and standard errors clustered at the individual and article level.')






#False/Misleading Articles (Study 2):
#Run OLS Model with clustered standard errors:
lin_results_fit_2_3_1 = feols(Susc_FN ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Data_Bef_Aft_MF)

#Write Table:
etable(lin_results_fit_2_3_1, tex = TRUE,
       signifCode= c(`***` = 0.001, `**` = 0.01, `*` = 0.05),
       file='./Tables/Figure_4b_6.txt',
       replace=TRUE,
       title='Results from OLS Regression Results Presented in Figure 4b (False/Misleading Articles - Study 2). This table presents the average treatment effects for linear regression models testing the effect of SOTEN. All effects are estimated using ordinary least squares (OLS) with article fixed effects and standard errors clustered at the individual and article level.')





#True Low-Quality Articles (Study 3):
#Run OLS Model with clustered standard errors:
lin_results_fit_3_1_1 = feols(True_Dummy ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = T_LQ_Data_Study_3)

#Write Table:
etable(lin_results_fit_3_1_1, tex = TRUE,
       signifCode= c(`***` = 0.001, `**` = 0.01, `*` = 0.05),
       file='./Tables/Figure_4b_7.txt',
       replace=TRUE,
       title='Results from OLS Regression Results Presented in Figure 4b (True Low Quality Articles - Study 3). This table presents the average treatment effects for linear regression models testing the effect of SOTEN. All effects are estimated using ordinary least squares (OLS) with article fixed effects and standard errors clustered at the individual and article level.')


#True Mainstream Articles (Study 3):
#Run OLS Model with clustered standard errors:
lin_results_fit_3_2_1 = feols(True_Dummy ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = T_Mainstream_Data_Study_3)

#Write Table:
etable(lin_results_fit_3_2_1, tex = TRUE,
       signifCode= c(`***` = 0.001, `**` = 0.01, `*` = 0.05),
       file='./Tables/Figure_4b_8.txt',
       replace=TRUE,
       title='Results from OLS Regression Results Presented in Figure 4b (True Mainstream Articles - Study 3). This table presents the average treatment effects for linear regression models testing the effect of SOTEN. All effects are estimated using ordinary least squares (OLS) with article fixed effects and standard errors clustered at the individual and article level.')




#False/Misleading Articles (Study 3):
#Run OLS Model with clustered standard errors:
lin_results_fit_3_3_1 = feols(True_Dummy ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Study_3_False_M)

#Write Table:
etable(lin_results_fit_3_3_1, tex = TRUE,
       signifCode= c(`***` = 0.001, `**` = 0.01, `*` = 0.05),
       file='./Tables/Figure_4b_9.txt',
       replace=TRUE,
       title='Results from OLS Regression Results Presented in Figure 4b (False/Misleading Articles - Study 3). This table presents the average treatment effects for linear regression models testing the effect of SOTEN. All effects are estimated using ordinary least squares (OLS) with article fixed effects and standard errors clustered at the individual and article level.')



#True Low-Quality Articles (Study 4):
#Run OLS Model with clustered standard errors:
lin_results_fit_4_1_1 = feols(True_Dummy ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = T_LQ_Data_Study_4)

#Write Table:
etable(lin_results_fit_4_1_1, tex = TRUE,
       signifCode= c(`***` = 0.001, `**` = 0.01, `*` = 0.05),
       file='./Tables/Figure_4b_10.txt',
       replace=TRUE,
       title='Results from OLS Regression Results Presented in Figure 4b (True Low Quality Articles - Study 4). This table presents the average treatment effects for linear regression models testing the effect of SOTEN. All effects are estimated using ordinary least squares (OLS) with article fixed effects and standard errors clustered at the individual and article level.')




#True Mainstream Articles (Study 4):
#Run OLS Model with clustered standard errors:
lin_results_fit_4_2_1 = feols(True_Dummy ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = T_Mainstream_Data_Study_4)

#Write Table:
etable(lin_results_fit_4_2_1, tex = TRUE,
       signifCode= c(`***` = 0.001, `**` = 0.01, `*` = 0.05),
       file='./Tables/Figure_4b_11.txt',
       replace=TRUE,
       title='Results from OLS Regression Results Presented in Figure 4b (True Mainstream Articles - Study 4). This table presents the average treatment effects for linear regression models testing the effect of SOTEN. All effects are estimated using ordinary least squares (OLS) with article fixed effects and standard errors clustered at the individual and article level.')



#False/Misleading Articles (Study 4):
#Run OLS Model with clustered standard errors:
lin_results_fit_4_3_1 = feols(Susc_FN ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Study_4_False_M)

#Write Table:
etable(lin_results_fit_4_3_1, tex = TRUE,
       signifCode= c(`***` = 0.001, `**` = 0.01, `*` = 0.05),
       file='./Tables/Figure_4b_12.txt',
       replace=TRUE,
       title='Results from OLS Regression Results Presented in Figure 4b (False/Misleading Articles - Study 4). This table presents the average treatment effects for linear regression models testing the effect of SOTEN. All effects are estimated using ordinary least squares (OLS) with article fixed effects and standard errors clustered at the individual and article level.')



#True Low-Quality Articles (Study 5):
#Run OLS Model with clustered standard errors:
lin_results_fit_5_1_1 = feols(True_Dummy ~ Treatment + Age + Ideo_Congruence  + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = T_LQ_Data_Study_5)

#Write Table:
etable(lin_results_fit_5_1_1, tex = TRUE,
       signifCode= c(`***` = 0.001, `**` = 0.01, `*` = 0.05),
       file='./Tables/Figure_4b_13.txt',
       replace=TRUE,
       title='Results from OLS Regression Results Presented in Figure 4b (True Low Quality Articles - Study 5). This table presents the average treatment effects for linear regression models testing the effect of SOTEN. All effects are estimated using ordinary least squares (OLS) with article fixed effects and standard errors clustered at the individual and article level.')




#True Mainstream Articles (Study 5):
#Run OLS Model with clustered standard errors:
lin_results_fit_5_2_1 = feols(True_Dummy ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = T_Mainstream_Data_Study_5)

#Write Table:
etable(lin_results_fit_5_2_1, tex = TRUE,
       signifCode= c(`***` = 0.001, `**` = 0.01, `*` = 0.05),
       file='./Tables/Figure_4b_14.txt',
       replace=TRUE,
       title='Results from OLS Regression Results Presented in Figure 4b (True Mainstream Articles - Study 5). This table presents the average treatment effects for linear regression models testing the effect of SOTEN. All effects are estimated using ordinary least squares (OLS) with article fixed effects and standard errors clustered at the individual and article level.')


#False/Misleading Articles (Study 5):
#Run OLS Model with clustered standard errors:
lin_results_fit_5_3_1 = feols(True_Dummy ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Study_5_False_M)

#Write Table:
etable(lin_results_fit_5_3_1, tex = TRUE,
       signifCode= c(`***` = 0.001, `**` = 0.01, `*` = 0.05),
       file='./Tables/Figure_4b_15.txt',
       replace=TRUE,
       title='Results from OLS Regression Results Presented in Figure 4b (False/Misleading Articles - Study 5). This table presents the average treatment effects for linear regression models testing the effect of SOTEN. All effects are estimated using ordinary least squares (OLS) with article fixed effects and standard errors clustered at the individual and article level.')

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
       title='Results from OLS Regression Results Presented in Figure 4c (Between-Respondent Experiment - True Low-Quality Articles). This table presents the average treatment effects for linear regression models testing the effect of SOTEN. All effects are estimated using ordinary least squares (OLS) with article fixed effects and standard errors clustered at the individual and article level.')



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
       title='Results from OLS Regression Results Presented in Figure 4c (Within-Respondent Experiment - True Low-Quality Articles). This table presents the average treatment effects for linear regression models testing the effect of SOTEN. All effects are estimated using ordinary least squares (OLS) with article fixed effects and standard errors clustered at the individual and article level.')


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
etable(btwn_T_M_results_1, tex = TRUE,
       signifCode= c(`***` = 0.001, `**` = 0.01, `*` = 0.05),
       file='./Tables/Figure_4c_3.txt',
       replace=TRUE,
       title='Results from OLS Regression Results Presented in Figure 4c (Between-Respondent Experiment - True Mainstream Articles). This table presents the average treatment effects for linear regression models testing the effect of SOTEN. All effects are estimated using ordinary least squares (OLS) with article fixed effects and standard errors clustered at the individual and article level.')

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
       title='Results from OLS Regression Results Presented in Figure 4c (Within-Respondent Experiment - True Mainstream Articles). This table presents the average treatment effects for linear regression models testing the effect of SOTEN. All effects are estimated using ordinary least squares (OLS) with article fixed effects and standard errors clustered at the individual and article level.')



#Within-Respondent Experiment - False/Misleading Articles

Data_Bef_Aft_MF <- Data_Bef_Aft_MF %>% select(Susc_FN,Likert_Evaluation,Treat_Search,Age,Dummy_Congruence,Education_Score,Gender,Income_Score,Article_day,ResponseId)
Study_3_False_M <- Study_3_False_M %>% select(True_Dummy,Likert_Evaluation,Treat_Search,Age,Dummy_Congruence,Education_Score,Gender,Income_Score,Article_day,ResponseId)
Study_4_False_M <- Study_4_False_M %>% select(Susc_FN,Likert_Evaluation,Treat_Search,Age,Dummy_Congruence,Education_Score,Gender,Income_Score,Article_day,ResponseId)

colnames(Data_Bef_Aft_MF)[1] <- 'True_Dummy'
colnames(Study_4_False_M)[1] <- 'True_Dummy'

Within_FM_Study <- rbind(Data_Bef_Aft_MF,Study_3_False_M,Study_4_False_M)

#Run OLS Model with clustered standard errors:
Between_FM_results_1 = feols(True_Dummy ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Within_FM_Study)

#Write Table:
etable(Between_FM_results_1, tex = TRUE,
       signifCode= c(`***` = 0.001, `**` = 0.01, `*` = 0.05),
       file='./Tables/Figure_4c_5.txt',
       replace=TRUE,
       title='Results from OLS Regression Results Presented in Figure 4c (Between-Respondent Experiment - False/Misleading Articles). This table presents the average treatment effects for linear regression models testing the effect of SOTEN. All effects are estimated using ordinary least squares (OLS) with article fixed effects and standard errors clustered at the individual and article level.')


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

wthn_FM_Study <- rbind(Misl_False_Search_MF,Study_5_False_M)

#Run OLS Model with clustered standard errors:
wthn_FM_results_1 = feols(True_Dummy ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = wthn_FM_Study)

#Write Table:
etable(wthn_FM_results_1, tex = TRUE,
       signifCode= c(`***` = 0.001, `**` = 0.01, `*` = 0.05),
       file='./Tables/Figure_4c_6.txt',
       replace=TRUE,
       title='Results from OLS Regression Results Presented in Figure 4c (Within-Respondent Experiment - False/Misleading Articles). This table presents the average treatment effects for linear regression models testing the effect of SOTEN. All effects are estimated using ordinary least squares (OLS) with article fixed effects and standard errors clustered at the individual and article level.')









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
  ylab("\nEffect of Searching Online on Probability \nof Rating Misinformation as True") +
  theme_classic() +
  theme(axis.title.x = element_text(size=18),
        axis.text.x  = element_text(size=16),
        axis.title.y = element_text(size=18),
        axis.text.y  = element_text(size=16),
        plot.title = element_text(size = 22),
        legend.title = element_text(size=18),
        legend.text = element_text(size=18),
        title =element_text(size=18, face='bold')) +
  ylim(-0.06,0.2) +
  scale_x_continuous(" \n",breaks=c(1,2,3,4),labels=c('Study 4',
                                                      'Study 3',
                                                      'Study 2',
                                                      'Study 1'),limits=c(0.5,4.5)) +
  coord_flip()

#Save figure:
ggsave('./Figures/All_4_Studies_Categorical_Robust.jpg',height=14,width=18,units='cm')
ggsave('./Figures/All_4_Studies_Categorical_Robust.pdf',height=14,width=18,units='cm')


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
#Produce plot:
ggplot(data = d_matrix, aes(x = x, y = Coefficients)) +
  geom_hline(aes(yintercept = 0), color = "gray",
             linetype = 2, size = 1.2) +
  geom_point(size=4) +
  geom_linerange(aes(min = CI_Lower,
                     max = CI_Upper),
                 size=1.5) +
  ylab("\nEffect of Searching Online on the Perceived      \n Veracity of Misinformation (7-point scale)       ") +
  theme_classic() +
  theme(axis.title.x = element_text(size=18),
        axis.text.x  = element_text(size=16),
        axis.title.y = element_text(size=18),
        axis.text.y  = element_text(size=16),
        plot.title = element_text(size = 24),
        legend.title = element_text(size=18),
        legend.text = element_text(size=18),
        title =element_text(size=18, face='bold')) +
  ylim(-0.12,0.8) +
  scale_x_continuous(" \n",breaks=c(1,2,3,4),labels=c('Study 4',
                                                      'Study 3',
                                                      'Study 2',
                                                      'Study 1'),limits=c(0.5,4.5)) +
  coord_flip()

#Save figure:
ggsave('./Figures/All_4_Studies_Ordinal_Robust.jpg',height=14,width=18,units='cm')
ggsave('./Figures/All_4_Studies_Ordinal_Robust.pdf',height=14,width=18,units='cm')

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
  scale_fill_manual(values=cbbPalette_1, name = "Number of News Links \nReturned by Search\nEngines From\nUnreliable Sources") +
  ylab('Proportion of Individuals Whose Search Engine Results         \n Return Unreliable News by Article Type         \n') +
  xlab('\nFact-Checker Rating of Article Individual\n Queries Search Engines About') +
  theme_bw() + 
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(size=14),
        axis.text.x  = element_text(size=14),
        axis.title.y = element_text(size=14),
        axis.text.y  = element_text(size=14),
        title =element_text(size=14, face='bold'),
        legend.text = element_text(size=14)) + guides(fill=guide_legend(
          keywidth=0.3,
          keyheight=0.3,
          default.unit="inch")) +
  geom_text(aes(label=Proportion), position=position_dodge(width=0.9), vjust=-0.25,size=4) +
  ylim(0,1)


#Save figure:
ggsave('./Figures/Study_5_Bar_Graph_Google_Search_ROBUST.jpg',height=16,width=18,units='cm')
ggsave('./Figures/Study_5_Bar_Graph_Google_Search_ROBUST.pdf',height=16,width=18,units='cm')


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
  ylab("\nEffect of Searching for Information on \nPerceived Veracity of Misinformation \n(1 unit is 1 standard deviation) ") +
  theme_classic() +
  theme(axis.title.x = element_text(size=18),
        axis.text.x  = element_text(size=16),
        axis.title.y = element_text(size=18),
        axis.text.y  = element_text(size=16),
        plot.title = element_text(size = 20),
        legend.title = element_text(size=18),
        legend.text = element_text(size=18),
        title =element_text(size=18, face='bold')) +
  ylim(-0.17,0.6) +
  scale_x_continuous("Perceived Veracity Scale \n",breaks=c(0.3,0.2,0.1),labels=Coef_names,limits=c(0.0,0.4)) +
  coord_flip()

#Save plot:
ggsave('./Figures/Study_5_1_ROBUST.jpg',height=16,width=18,units='cm')
ggsave('./Figures/Study_5_1_ROBUST.pdf',height=16,width=18,units='cm')


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
  scale_color_manual(values=cbbPalette_1, name = "Measure") +
  ylab("\n Effect of Searching for Information Quality on Belief in Misinformation\n Dependent on Quality of Information Returned from Google Search Results \n(1 unit is 1 standard deviation of that measure) ") +
  theme_classic() +
  theme(axis.title.x = element_text(size=12),
        axis.text.x  = element_text(size=12),
        axis.title.y = element_text(size=12),
        axis.text.y  = element_text(size=12),
        plot.title = element_text(size = 12),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12),
        title =element_text(size=12, face='bold')) +
  ylim(-0.55,0.55) +
  scale_x_continuous("Type of News Returned by Google Search Engine \n",breaks=c(1.5,0.9),labels=c('At Least 10%\nof News URLs\nAre Unreliable',
                                                                                                   'Only Very Reliable\nNews Sources'),limits=c(0.5,2.0)) +
  coord_flip()

#Save Figure:
ggsave('./Figures/Coefs_CIs_ROBUST.jpg',height=16,width=18,units='cm')
ggsave('./Figures/Coefs_CIs_ROBUST.pdf',height=16,width=18,units='cm')



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


ggplot(data = Fig_2d_Mat, aes(x = x, y = Coef)) +
  geom_hline(aes(yintercept = 0), color = "gray",
             linetype = 2, size = 1.2) +
  geom_point(aes(color = Measure, shape=Measure),size=4) +
  geom_linerange(aes(min = Low_Conf, 
                     max = Upp_Conf, 
                     color = Measure),
                 size=1.5) +
  scale_color_manual(values=cbbPalette_1, name = "Measure") +
  ylab("\n Effect of Searching for Information Quality on Belief in Misinformation\n Dependent on Quality of Information Returned from Google Search Results \n(1 unit is 1 standard deviation of that measure) ") +
  theme_classic() +
  theme(axis.title.x = element_text(size=12),
        axis.text.x  = element_text(size=12),
        axis.title.y = element_text(size=12),
        axis.text.y  = element_text(size=12),
        plot.title = element_text(size = 12),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12),
        title =element_text(size=12, face='bold')) +
  ylim(-0.6,0.7) +
  scale_x_continuous("Quartile of News Quality Returned by Google Search Engine \n",breaks=c(2.7,2.1,1.5,0.9),labels=c('75-100%',
                                                                                                                       '50-75%',
                                                                                                                       '25-50%',
                                                                                                                       '0-25%'),limits=c(0.5,3.0)) +
  coord_flip()

#Save Figure:
ggsave('./Figures/Coefs_CIs_2_ROBUST.jpg',height=16,width=18,units='cm')
ggsave('./Figures/Coefs_CIs_2_ROBUST.pdf',height=16,width=18,units='cm')


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
  ylab("\n The Effect of a 1 SD Increase of Indep. Var.\n on Probability of Exposure to Unreliable News") +
  theme_classic() +
  theme(axis.title.x = element_text(size=16),
        axis.text.x  = element_text(size=12),
        axis.title.y = element_text(size=16),
        axis.text.y  = element_text(size=12),
        plot.title = element_text(size = 12),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12),
        title =element_text(size=12, face='bold')) +
  ylim(-0.1,0.15) +
  scale_x_continuous("Demographic Covariates \n",breaks=c(0.1,0.2,0.3,0.4,0.5,0.6),labels=Coef_names,limits=c(0.0,0.7)) +
  coord_flip()

#Save figure:
ggsave('./Figures/Coefs_CIs_Predicting_Unrel_Dummy_ROBUST.jpg',height=12,width=18,units='cm')
ggsave('./Figures/Coefs_CIs_Predicting_Unrel_Dummy_ROBUST.pdf',height=12,width=18,units='cm')



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
  scale_fill_manual(values=cbbPalette_1, name = "Number of News Links \nReturned by Search\nEngines From\nUnreliable Sources") +
  ylab('Proportion of Search Queries That Return Results \n That Return Unreliable News by Article Type     \n') +
  xlab('\nType of Search Query Used') +
  theme_classic() + 
  theme(axis.title.x = element_text(size=16),
        axis.text.x  = element_text(size=12),
        axis.title.y = element_text(size=16),
        axis.text.y  = element_text(size=12),
        plot.title = element_text(size = 12),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12),
        title =element_text(size=12, face='bold')) +
  guides(fill=guide_legend(
    keywidth=0.3,
    keyheight=0.3,
    default.unit="inch")) +
  geom_text(aes(label=Proportion), position=position_dodge(width=0.9), vjust=-0.25,size=4) +
  ylim(0,1)

#Save figure:
ggsave('./Figures/Study_5_Bar_Graph_Google_Search_HL_ROBUST.jpg',height=16,width=18,units='cm')
ggsave('./Figures/Study_5_Bar_Graph_Google_Search_HL_ROBUST.pdf',height=16,width=18,units='cm')


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
  ylab("\nThe Effect of a 1 SD Increase of Indep. Var.     \n on Probability of Using Headline/Link as Search Term        ") +
  theme_classic() +
  theme(axis.title.x = element_text(size=16),
        axis.text.x  = element_text(size=12),
        axis.title.y = element_text(size=16),
        axis.text.y  = element_text(size=12),
        plot.title = element_text(size = 12),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12),
        title =element_text(size=12, face='bold')) +
  ylim(-0.1,0.1) +
  scale_x_continuous("Demographic Covariates \n",breaks=c(0.1,0.2,0.3,0.4,0.5,0.6),labels=Coef_names,limits=c(0.0,0.7)) +
  coord_flip()

#Save figure:
ggsave('./Figures/Coefs_CIs_Predicting_Headline_Link_ROBUST.jpg',height=16,width=18,units='cm')
ggsave('./Figures/Coefs_CIs_Predicting_Headline_Link_ROBUST.pdf',height=16,width=18,units='cm')



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
  scale_color_manual(values=cbbPalette_3, name = "Type of News") +
  scale_shape_manual(values=c(15,17),"Type of News") +
  ylab("\n The Effect of Searching Online\n on Probability of Rating News as True") +
  theme_classic() +
  theme(axis.title.x = element_text(size=16),
        axis.text.x  = element_text(size=12),
        axis.title.y = element_text(size=16),
        axis.text.y  = element_text(size=12),
        plot.title = element_text(size = 12),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12),
        title =element_text(size=12, face='bold')) +
  ylim(-0.1,0.2) +
  scale_x_continuous("Study Number\n",breaks=c(1,2,3,4,5),labels=rev(c('Study 1',
                                                                       'Study 2',
                                                                       'Study 3',
                                                                       'Study 4',
                                                                       'Study 5')),limits=c(0.2,5.8)) +
  coord_flip()


ggsave('./Figures/T_FM_Fig_1A_True_Dummy_ROBUST.jpg',height=16,width=18,units='cm')
ggsave('./Figures/T_FM_Fig_1A_True_Dummy_ROBUST.pdf',height=16,width=18,units='cm')

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
  scale_color_manual(values=cbbPalette_2, name = "Type of News") +
  scale_shape_manual(values=c(15,16,17),"Type of News") +
  ylab("\n The Effect of Searching Online\n on Probability of Rating News as True") +
  theme_classic() +
  theme(axis.title.x = element_text(size=16),
        axis.text.x  = element_text(size=12),
        axis.title.y = element_text(size=16),
        axis.text.y  = element_text(size=12),
        plot.title = element_text(size = 12),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12),
        title =element_text(size=12, face='bold')) +
  ylim(-0.1,0.35) +
  scale_x_continuous("Study Number\n",breaks=c(1,2,3,4,5),labels=rev(c('Study 1',
                                                                       'Study 2',
                                                                       'Study 3',
                                                                       'Study 4',
                                                                       'Study 5')),limits=c(0.2,5.8)) +
  coord_flip()


ggsave('./Figures/Fig_1A_True_Dummy_ROBUST.jpg',height=16,width=18,units='cm')
ggsave('./Figures/Fig_1A_True_Dummy_ROBUST.pdf',height=16,width=18,units='cm')

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
  scale_color_manual(values=cbbPalette_2, name = "Type of News") +
  scale_shape_manual(values=c(15,16,17),"Type of News") +
  ylab("\n The Effect of Searching Online\n on Probability of Rating News as True") +
  theme_classic() +
  theme(axis.title.x = element_text(size=16),
        axis.text.x  = element_text(size=12),
        axis.title.y = element_text(size=16),
        axis.text.y  = element_text(size=12),
        plot.title = element_text(size = 12),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12),
        title =element_text(size=12, face='bold')) +
  ylim(-0.1,0.25) +
  scale_x_continuous("Study Type\n",breaks=c(1,2),labels=rev(c('Between',
                                                               'Within')),limits=c(0,3)) +
  coord_flip()


ggsave('./Figures/Types_Fig_1A_True_Dummy_ROBUST.jpg',height=16,width=18,units='cm')
ggsave('./Figures/Types_Fig_1A_True_Dummy_ROBUST.pdf',height=16,width=18,units='cm')



######################################################################################################

##################################### Balance Table for Study 6 ######################################

######################################################################################################

#Read in data:
Data_1_FM <- read.csv('./Data/Study_6_FM.csv')
Data_1_T_M <- read.csv('./Data/Study_6_T_M.csv')
Data_1_T_LQ <- read.csv('./Data/Study_6_T_LQ.csv')

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


############################################################################################

################################# Section Z: Search Terms ##################################

############################################################################################

######## LaTex Table:

Search_Term_df <- read.csv('./Data/All_Search_Results_Combined.csv')
Search_Term_Day <- na.omit(Search_Term_df)
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


#Statistics rpoerted in main text::
xt_1 <- xt %>% filter(grepl('engineered famine',`Search Term`))
xt_2 <- xt %>% filter(grepl('famine',`Search Term`))
xt_2 <- xt_2 %>% filter(!grepl('engineered famine',`Search Term`))

#Average stats reported in main text:

#Ideological Congurence:
mean(xt_1$`Ideologically Congruent`)
mean(xt_2$`Ideologically Congruent`)

#Digital Literacy Score:
mean(xt_1$`Digital Literacy Score`)
mean(xt_2$`Digital Literacy Score`)


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



############################################## Balance Tables ################################################


###############################################################################################################

################################################## Table 1 ####################################################

###############################################################################################################

#Pull in this data: Search Experiment 1: Study 1:
Misl_False_Search <- read.csv('./Data/Search_Exp_Misl_False.csv')

#Select variables of interest:
Model_Data_7 <- Misl_False_Search %>% select(Treat_Search,Education_Score,Age,Gender,Income_Score,Ideology_Score,Article_day,ResponseId)
Model_Data_7$Gender <- ifelse(Model_Data_7$Gender == 'Female',1,0)

#Remove NA values:
Model_Data_7 = na.omit(Model_Data_7)


Model_Data_7 <- Model_Data_7 %>% select(Education_Score,Age,Gender,Income_Score,Ideology_Score,ResponseId,Treat_Search)

Control_Data <- Model_Data_7 %>% filter(Treat_Search == 0)
Treatment_Data <- Model_Data_7 %>% filter(Treat_Search == 1)

Control_Data_Rs <- unique(Control_Data)
Treatment_Data_Rs <- unique(Treatment_Data)

Model_Data_7 <- rbind(Control_Data_Rs,Treatment_Data_Rs)

#Education:
Educ_C <- round(mean(Control_Data_Rs$Education_Score),2)
Educ_T <- round(mean(Treatment_Data_Rs$Education_Score),2)
Diff_Educ <- round((Educ_T - Educ_C),2)

Model_Educ <- coeftest(lm(Education_Score ~ Treat_Search,data=Model_Data_7))
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
Age_C <- round(mean(Control_Data_Rs$Age),2)
Age_T <- round(mean(Treatment_Data_Rs$Age),2)
Diff_Age <- round((Age_T - Age_C),2)

Model_Age <- coeftest(lm(Age ~ Treat_Search,data=Model_Data_7))
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
Gender_C <- round(mean(Control_Data_Rs$Gender),2)
Gender_T <- round(mean(Treatment_Data_Rs$Gender),2)
Diff_Gender <- round((Gender_T - Gender_C),2)

Model_Gender <- coeftest(lm(Gender ~ Treat_Search,data=Model_Data_7))
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
Income_C <- round(mean(Control_Data_Rs$Income_Score),2)
Income_T <- round(mean(Treatment_Data_Rs$Income_Score),2)
Diff_Income <- round((Income_T - Income_C),2)

Model_Income <- coeftest(lm(Income_Score ~ Treat_Search,data=Model_Data_7))
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
Ideology_C <- round(mean(Control_Data_Rs$Ideology_Score),2)
Ideology_T <- round(mean(Treatment_Data_Rs$Ideology_Score),2)
Diff_Ideology <- round((Ideology_T - Ideology_C),2)

Model_Ideology <- coeftest(lm(Ideology_Score ~ Treat_Search,data=Model_Data_7))
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
      file='./Tables/Balance_1.txt')

###############################################################################################################

################################################## Table 2 ####################################################

###############################################################################################################


#Pull in this data:
Data_Bef_Aft <- read.csv('./Data/Data_Bef_Aft_Misl_False.csv')

Study_2_Ideo <- read.csv('./Data/Study_2_Respondent_Ideo.csv')

Data_Bef_Aft$ResponseId <- as.character(Data_Bef_Aft$ResponseId)
Study_2_Ideo$ResponseId <- as.character(Study_2_Ideo$ResponseId)

Study_2_Ideo <- unique(Study_2_Ideo)


Data_Bef_Aft <- merge(Data_Bef_Aft,Study_2_Ideo,by='ResponseId')



#Select variables of interest:
Model_Data_7 <- Data_Bef_Aft %>% select(Education_Score,Age,Gender,Income_Score,Ideology_Score,Article_day,ResponseId)

Model_Data_7 <- unique(Model_Data_7)

Model_Data_7$Gender <- ifelse(Model_Data_7$Gender == 'Female',1,0)
Model_Data_7$Age <- Model_Data_7$Age*10


#Education:
Educ_M <- round(mean(Model_Data_7$Education_Score),2)
Educ_sd <- round(sd(Model_Data_7$Education_Score),2)

#Age:
Age_M <- round(mean(Model_Data_7$Age,na.rm=T),2)
Age_sd <- round(sd(Model_Data_7$Age,na.rm=T),2)

#Age:
Income_M <- round(mean(Model_Data_7$Income_Score),2)
Income_sd <- round(sd(Model_Data_7$Income_Score),2)

#Gender:
Gender_M <- round(mean(Model_Data_7$Gender,na.rm = T),2)
Gender_sd <- round(sd(Model_Data_7$Gender,na.rm = T),2)

#Ideology:
Ideology_M <- round(mean(Model_Data_7$Ideology_Score,na.rm = T),2)
Ideology_sd <- round(sd(Model_Data_7$Ideology_Score,na.rm = T),2)



Balance_T <- matrix(c('Education',Educ_M,Educ_sd,
                      'Age',Age_M,Age_sd,
                      'Gender',Gender_M,Gender_sd,
                      'Income',Income_M,Income_sd,
                      'Ideology',Ideology_M,Ideology_sd),byrow=T,ncol=3)


xt <- xtable(Balance_T,
             digits=2,
             align=c(
               "|p{1.5cm}|","|p{3cm}|","|p{3cm}|",
               "p{3cm}|"))

colnames(xt) <- c('Demographic','Mean','Standard \\newline Deviation')


comment          <- list()
comment$pos      <- list()
comment$pos[[1]] <- c(nrow(Balance_T))
comment$command  <- c(paste("\\hline \n",  # we`ll replace all default hlines with this and the ones below
                            '\\multicolumn{4}{l}{\\textsuperscript{***}$p<0.001$, \\textsuperscript{**}$p<0.01$, \\textsuperscript{*}$p<0.05$}',
                            sep = ""))


#Write Table:
write(print(xt,include.rownames=FALSE,
            sanitize.colnames.function = identity,
            add.to.row = comment),
      file='./Tables/Balance_2.txt')


###############################################################################################################

################################################## Table 3 ####################################################

###############################################################################################################

#Read in sources in the google search results for false/misleading articels in Study 1:
Latency_Data <- read.csv('./Data/Latency_FC_Data.csv')
Latency_Survey <- read.csv('./Data/Latency_Control_Survey.csv')
Latency_Survey <- Latency_Survey %>% mutate(Article_day = paste0(day,sep='_',Article))
Latency_Survey <- Latency_Survey %>% select(Evaluation,Likert_Evaluation,True_Likert_After_Info,Evaluation_After_Info,Age,Dig_Lit_Avg,Income_Score,CRT_Score,Familiar_Story,Education_Score,Duration,Article_day,Ideology_Score,Gender,ResponseId)

Latency_Survey$Gender <- as.character(Latency_Survey$Gender)
Latency_Survey$Gender <- ifelse(Latency_Survey$Gender == 'Female',1,0)

Latency_Survey$True_Likert_After_Info <- as.character(Latency_Survey$True_Likert_After_Info)
Latency_Survey <- Latency_Survey %>% filter(True_Likert_After_Info != 'not asked')
Latency_Survey$True_Likert_After_Info <- substr(Latency_Survey$True_Likert_After_Info, start = 1, stop = 2)
Latency_Survey$True_Likert_After_Info <- as.numeric(Latency_Survey$True_Likert_After_Info)

Latency_Survey$Likert_Evaluation <- as.character(Latency_Survey$Likert_Evaluation)
Latency_Survey$Likert_Evaluation <- substr(Latency_Survey$Likert_Evaluation, start = 1, stop = 2)
Latency_Survey$Likert_Evaluation <- as.numeric(Latency_Survey$Likert_Evaluation)

Latency_Survey$Evaluation <- as.character(Latency_Survey$Evaluation)
Latency_Survey$Evaluation_After_Info <- as.character(Latency_Survey$Evaluation_After_Info)
Latency_Survey$Evaluation <- substr(Latency_Survey$Evaluation, start = 1, stop = 4)
Latency_Survey$Evaluation_After_Info <- substr(Latency_Survey$Evaluation_After_Info, start = 1, stop = 4)

Latency_Survey$True_Dummy <- ifelse(Latency_Survey$Evaluation == 'True',1,0)
Latency_Survey$True_Dummy_After <- ifelse(Latency_Survey$Evaluation_After_Info == 'True',1,0)

Latency_Survey$Duration <- Latency_Survey$Duration/600
Latency_Survey$Age <- ifelse(Latency_Survey$Age > 85,NA,Latency_Survey$Age)

Data_Bef_Aft <- read.csv('./Data/Data_Bef_Aft_Misl_False.csv')
Data_Bef_Aft$Article_day <- as.character(Data_Bef_Aft$Article_day)

Article_Days <- unique(Data_Bef_Aft$Article_day)


Latency_Survey <- Latency_Survey %>% filter(Article_day %in% Article_Days)


After_Evaluation <- Latency_Survey %>% select(True_Dummy_After,True_Likert_After_Info,Age,Dig_Lit_Avg,Income_Score,CRT_Score,Familiar_Story,Education_Score,Duration,Article_day,Ideology_Score,Gender,ResponseId)
colnames(After_Evaluation)[1] <- 'True_Dummy'
colnames(After_Evaluation)[2] <- 'Likert_Evaluation'
Before_Evaluation <- Latency_Survey %>% select(True_Dummy,Likert_Evaluation,Age,Dig_Lit_Avg,Income_Score,CRT_Score,Familiar_Story,Education_Score,Duration,Article_day,Ideology_Score,Gender,ResponseId)

After_Evaluation$Treatment <- 1
Before_Evaluation$Treatment <- 0

Latency_Search <- rbind(Before_Evaluation,After_Evaluation)


Model_Data_7 <- Latency_Search

Model_Data_7 <- Model_Data_7 %>% select(Education_Score,Age,Gender,Income_Score,Ideology_Score,ResponseId)
Model_Data_7 <- unique(Model_Data_7)

#Education:
Educ_M <- round(mean(Model_Data_7$Education_Score),2)
Educ_sd <- round(sd(Model_Data_7$Education_Score),2)

#Age:
Age_M <- round(mean(Model_Data_7$Age,na.rm=T),2)
Age_sd <- round(sd(Model_Data_7$Age,na.rm=T),2)

#Age:
Income_M <- round(mean(Model_Data_7$Income_Score),2)
Income_sd <- round(sd(Model_Data_7$Income_Score),2)

#Gender:
Gender_M <- round(mean(Model_Data_7$Gender),2)
Gender_sd <- round(sd(Model_Data_7$Gender),2)

#Ideology:
Ideology_M <- round(mean(Model_Data_7$Ideology_Score,na.rm=T),2)
Ideology_sd <- round(sd(Model_Data_7$Ideology_Score,na.rm=T),2)



Balance_T <- matrix(c('Education',Educ_M,Educ_sd,
                      'Age',Age_M,Age_sd,
                      'Gender',Gender_M,Gender_sd,
                      'Income',Income_M,Income_sd,
                      'Ideology',Ideology_M,Ideology_sd),byrow=T,ncol=3)


xt <- xtable(Balance_T,
             digits=2,
             align=c(
               "|p{1.5cm}|","|p{3cm}|","|p{3cm}|",
               "p{3cm}|"))

colnames(xt) <- c('Demographic','Mean','Standard \\newline Deviation')


comment          <- list()
comment$pos      <- list()
comment$pos[[1]] <- c(nrow(Balance_T))
comment$command  <- c(paste("\\hline \n",  # we`ll replace all default hlines with this and the ones below
                            '\\multicolumn{4}{l}{\\textsuperscript{***}$p<0.001$, \\textsuperscript{**}$p<0.01$, \\textsuperscript{*}$p<0.05$}',
                            sep = ""))


#Write Table:
write(print(xt,include.rownames=FALSE,
            sanitize.colnames.function = identity,
            add.to.row = comment),
      file='./Tables/Balance_3.txt')



###############################################################################################################

################################################## Table 4 ####################################################

###############################################################################################################


Data_Bef_Aft <- read.csv('./Data/Experiment_2_Study_2_Misl_False.csv')

Study_4_Ideo <- read.csv('./Data/Study_4_Respondent_Ideo.csv')

Data_Bef_Aft$ResponseId <- as.character(Data_Bef_Aft$ResponseId)
Study_4_Ideo$ResponseId <- as.character(Study_4_Ideo$ResponseId)


Data_Bef_Aft <- merge(Data_Bef_Aft,Study_4_Ideo,by='ResponseId')


#Select variables of interest:
Model_Data_7 <- Data_Bef_Aft %>% select(Education_Score,Age,Gender,Income_Score,Ideology_Score,ResponseId)

Model_Data_7 <- unique(Model_Data_7)

Model_Data_7$Gender <- ifelse(Model_Data_7$Gender == 'Female',1,0)


#Education:
Educ_M <- round(mean(Model_Data_7$Education_Score),2)
Educ_sd <- round(sd(Model_Data_7$Education_Score),2)

#Age:
Age_M <- round(mean(Model_Data_7$Age,na.rm=T),2)
Age_sd <- round(sd(Model_Data_7$Age,na.rm=T),2)

#Age:
Income_M <- round(mean(Model_Data_7$Income_Score),2)
Income_sd <- round(sd(Model_Data_7$Income_Score),2)

#Gender:
Gender_M <- round(mean(Model_Data_7$Gender,na.rm=T),2)
Gender_sd <- round(sd(Model_Data_7$Gender,na.rm=T),2)

#Ideology:
Ideology_M <- round(mean(Model_Data_7$Ideology_Score,na.rm=T),2)
Ideology_sd <- round(sd(Model_Data_7$Ideology_Score,na.rm=T),2)



Balance_T <- matrix(c('Education',Educ_M,Educ_sd,
                      'Age',Age_M,Age_sd,
                      'Gender',Gender_M,Gender_sd,
                      'Income',Income_M,Income_sd,
                      'Ideology',Ideology_M,Ideology_sd),byrow=T,ncol=3)


xt <- xtable(Balance_T,
             digits=2,
             align=c(
               "|p{1.5cm}|","|p{3cm}|","|p{3cm}|",
               "p{3cm}|"))

colnames(xt) <- c('Demographic','Mean','Standard \\newline Deviation')


comment          <- list()
comment$pos      <- list()
comment$pos[[1]] <- c(nrow(Balance_T))
comment$command  <- c(paste("\\hline \n",  # we`ll replace all default hlines with this and the ones below
                            '\\multicolumn{4}{l}{\\textsuperscript{***}$p<0.001$, \\textsuperscript{**}$p<0.01$, \\textsuperscript{*}$p<0.05$}',
                            sep = ""))

#Write Table:
write(print(xt,include.rownames=FALSE,
            sanitize.colnames.function = identity,
            add.to.row = comment),
      file='./Tables/Balance_4.txt')



###############################################################################################################

################################################## Table 5 ####################################################

###############################################################################################################

Control_Data <- read.csv('./Data/Control_Data_Study_5.csv')

#Read CSV (Treatment):
Treatment_Data <- read.csv('./Data/Treatment_Data_Study_5.csv')
Treatment_Data$Link_1 <- NULL
Treatment_Data$Link_2 <- NULL

All_Data <- rbind(Treatment_Data,Control_Data)
FM_Data <- All_Data %>% filter(FC_Eval == 'FM')

#Select variables of interest:
Model_Data_7 <- FM_Data %>% select(Treatment,Education_Score,Age,Gender,Income_Score,Ideology_Score,Article_day,ResponseId)

#Remove NA values:
Model_Data_7 = na.omit(Model_Data_7)


Model_Data_7 <- Model_Data_7 %>% select(Education_Score,Age,Gender,Income_Score,Ideology_Score,ResponseId,Treatment)

Control_Data <- Model_Data_7 %>% filter(Treatment == 0)
Treatment_Data <- Model_Data_7 %>% filter(Treatment == 1)

Control_Data_Rs <- unique(Control_Data)
Treatment_Data_Rs <- unique(Treatment_Data)

Model_Data_7 <- rbind(Control_Data_Rs,Treatment_Data_Rs)

#Education:
Educ_C <- round(mean(Control_Data_Rs$Education_Score),2)
Educ_T <- round(mean(Treatment_Data_Rs$Education_Score),2)
Diff_Educ <- round((Educ_T - Educ_C),2)

Model_Educ <- coeftest(lm(Education_Score ~ Treatment,data=Model_Data_7))
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
Age_C <- round(mean(Control_Data_Rs$Age),2)
Age_T <- round(mean(Treatment_Data_Rs$Age),2)
Diff_Age <- round((Age_T - Age_C),2)

Model_Age <- coeftest(lm(Age ~ Treatment,data=Model_Data_7))
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
Gender_C <- round(mean(Control_Data_Rs$Gender),2)
Gender_T <- round(mean(Treatment_Data_Rs$Gender),2)
Diff_Gender <- round((Gender_T - Gender_C),2)

Model_Gender <- coeftest(lm(Gender ~ Treatment,data=Model_Data_7))
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
Income_C <- round(mean(Control_Data_Rs$Income_Score),2)
Income_T <- round(mean(Treatment_Data_Rs$Income_Score),2)
Diff_Income <- round((Income_T - Income_C),2)

Model_Income <- coeftest(lm(Income_Score ~ Treatment,data=Model_Data_7))
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
Ideology_C <- round(mean(Control_Data_Rs$Ideology_Score),2)
Ideology_T <- round(mean(Treatment_Data_Rs$Ideology_Score),2)
Diff_Ideology <- round((Ideology_T - Ideology_C),2)

Model_Ideology <- coeftest(lm(Ideology_Score ~ Treatment,data=Model_Data_7))
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
                            '\\multicolumn{4}{l}{\\textsuperscript{***}$p<0.001$, \\textsuperscript{**}$p<0.01$, \\textsuperscript{*}$p<0.05$}',
                            sep = ""))


#Write Table:
write(print(xt,include.rownames=FALSE,
            sanitize.colnames.function = identity,
            add.to.row = comment),
      file='./Tables/Balance_5.txt')


###############################################################################################################

################################################## Table 6 ####################################################

###############################################################################################################


#Ideological Perspective of Articles:
FC_Ideo_Data <- read.csv('./Data/FC_Ideo_Data.csv')
FC_Ideo_Data$X <- NULL

#Pull in treatment data for Study 5:
Treatment_Data <- read.csv('./Data/Treatment_Data_Study_5.csv')
Treatment_Data$Link_1 <- NULL
Treatment_Data$Link_2 <- NULL

Study_5_treat_data <- read.csv('./Data/Study_5_treat_data.csv')


#Merge datasets:
Treatment_Data <- merge(Treatment_Data,FC_Ideo_Data,by='Article_day')

T_Data <- Treatment_Data %>% filter(FC_Eval == 'FM')

Installed_Treatment <- Study_5_treat_data %>% select(ResponseId,Article_day)

Full_Treatment_Data <- T_Data
GS_Treatment_Data <- Study_5_treat_data

Installed_Treatment_Data <- merge(T_Data,Installed_Treatment,by=c('ResponseId','Article_day'))

Installed_Treatment_Data <- Installed_Treatment_Data %>% select(ResponseId,Article_day)
Full_Treatment_Data <- Full_Treatment_Data %>% select(ResponseId,Article_day)


Installed_Treatment_Data$ResponseId <- as.character(Installed_Treatment_Data$ResponseId)
Installed_Treatment_Data$Article_day <- as.character(Installed_Treatment_Data$Article_day)

Full_Treatment_Data$ResponseId <- as.character(Full_Treatment_Data$ResponseId)
Full_Treatment_Data$Article_day <- as.character(Full_Treatment_Data$Article_day)

#Create non-compliance group:

Not_Installed_Treatment_Data <- setdiff(Full_Treatment_Data,Installed_Treatment_Data)


Installed_Treatment_Data <- merge(T_Data,Installed_Treatment_Data,by=c('ResponseId','Article_day'))

Not_Installed_Treatment_Data <- merge(T_Data,Not_Installed_Treatment_Data,by=c('ResponseId','Article_day'))

nrow(Not_Installed_Treatment_Data)
nrow(Installed_Treatment_Data)

Not_Installed_Treatment_Data <- Not_Installed_Treatment_Data %>% ungroup() %>% select(ResponseId,Age,Gender,Income_Score,Education_Score,Ideology_Score,Dig_Lit_Score)
Installed_Treatment_Data <- Installed_Treatment_Data %>% select(ResponseId,Age,Gender,Income_Score,Education_Score,Ideology_Score,Dig_Lit_Score)

Not_Installed_Treatment_Data$Installed <- 0
Installed_Treatment_Data$Installed <- 1

Full_data <- rbind(Not_Installed_Treatment_Data,Installed_Treatment_Data)

Full_data <- na.omit(Full_data)

#Education:
Educ_C <- round(mean(Not_Installed_Treatment_Data$Education_Score),2)
Educ_T <- round(mean(Installed_Treatment_Data$Education_Score),2)
Diff_Educ <- round((Educ_T - Educ_C),2)

Model_Educ <- coeftest(lm(Education_Score ~ Installed,data=Full_data))
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
Age_C <- round(mean(Not_Installed_Treatment_Data$Age),2)
Age_T <- round(mean(Installed_Treatment_Data$Age),2)
Diff_Age <- round((Age_T - Age_C),2)

Model_Age <- coeftest(lm(Age ~ Installed,data=Full_data))
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
Gender_C <- round(mean(Not_Installed_Treatment_Data$Gender),2)
Gender_T <- round(mean(Installed_Treatment_Data$Gender),2)
Diff_Gender <- round((Gender_T - Gender_C),2)

Model_Gender <- coeftest(lm(Gender ~ Installed,data=Full_data))
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
Income_C <- round(mean(Not_Installed_Treatment_Data$Income_Score),2)
Income_T <- round(mean(Installed_Treatment_Data$Income_Score),2)
Diff_Income <- round((Income_T - Income_C),2)

Model_Income <- coeftest(lm(Income_Score ~ Installed,data=Full_data))
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
Ideology_C <- round(mean(Not_Installed_Treatment_Data$Ideology_Score,na.rm=T),2)
Ideology_T <- round(mean(Installed_Treatment_Data$Ideology_Score,na.rm=T),2)
Diff_Ideology <- round((Ideology_T - Ideology_C),2)

Mod_Ideo <- lm(Ideology_Score ~ Installed,data=Full_data)

Model_Ideology <- coeftest(lm(Ideology_Score ~ Installed,data=Full_data))
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

#F-statistic
glm.0 <- glm(Ideology_Score ~ 1,data=Full_data)
Results <- anova(Mod_Ideo, glm.0, test="F")
F_Tab_1_1 <- Results$F[2]



Diff_Ideology <- paste0(as.character(Diff_Ideology),Ideology_Stars)


#Ideology:
Digital_C <- round(mean(Not_Installed_Treatment_Data$Dig_Lit_Score,na.rm=T),2)
Digital_T <- round(mean(Installed_Treatment_Data$Dig_Lit_Score,na.rm=T),2)
Diff_Digital <- round((Digital_T - Digital_C),2)

Model_Digital <- coeftest(lm(Dig_Lit_Score ~ Installed,data=Full_data))
Digital_P <- Model_Digital[2,4]

Digital_Stars <- ''
if(Digital_P < 0.05){
  Digital_Stars <- '*'
  if(Digital_P < 0.01){
    Digital_Stars <- '**'
    if(Digital_P < 0.001){
      Digital_Stars <- '***'
    }
  }
} 


Diff_Digital <- paste0(as.character(Diff_Digital),Digital_Stars)


Balance_T <- matrix(c('Education',Educ_T,Educ_C,Diff_Educ,
                      'Age',Age_T,Age_C,Diff_Age,
                      'Gender',Gender_T,Gender_C,Diff_Gender,
                      'Income',Income_T,Income_C,Diff_Income,
                      'Ideology',Ideology_T,Ideology_C,Diff_Ideology,
                      'Digital Literacy',Digital_T,Digital_C,Diff_Digital),byrow=T,ncol=4)


xt <- xtable(Balance_T,
             digits=2,
             align=c(
               "|p{1.5cm}|","|p{3cm}|","|p{3cm}|",
               "p{3cm}|","p{3cm}|"))

colnames(xt) <- c('Demographic','Average (Complier)','Average (Non-Complier)','Difference')


comment          <- list()
comment$pos      <- list()
comment$pos[[1]] <- c(nrow(Balance_T))
comment$command  <- c(paste("\\hline \n",  # we`ll replace all default hlines with this and the ones below
                            '\\multicolumn{4}{l}{\\textsuperscript{***}$p<0.001$, \\textsuperscript{**}$p<0.01$, \\textsuperscript{*}$p<0.05$}',
                            sep = ""))


#Write Table:
write(print(xt,include.rownames=FALSE,
            sanitize.colnames.function = identity,
            add.to.row = comment),
      file='./Tables/Balance_6.txt')

#############################################################################################################

############################################## Table 7 ######################################################

#############################################################################################################

#Control:
Control_Data <- read.csv('./Data/Control_Data_Study_5.csv')
Control_Data <- Control_Data %>% filter(FC_Eval == 'FM')
Control_Data$Article_day <- as.character(Control_Data$Article_day)
Control_Data$ResponseId <- as.character(Control_Data$ResponseId)

#Installed:
Control_Installed <- read.csv('./Data/output_Control_Survey_2.csv')
Control_Installed$Response_id <- as.character(Control_Installed$Response_id)
Control_Installed$Article_day <- as.character(Control_Installed$Article_day)
Control_Installed <- Control_Installed %>% select(Response_id,Article_day)
Control_Installed <- unique(Control_Installed)
Control_Data <- merge(Control_Data,Control_Installed,by.x=c('ResponseId','Article_day'),by.y=c('Response_id','Article_day'))
Control_Left <- Control_Data


Control_Data <- read.csv('./Data/Control_Data_Study_5.csv')
Control_Data <- Control_Data %>% filter(FC_Eval == 'FM')

Not_Installed_Control_Data <- subset(Control_Data,!(ResponseId%in%Control_Left$ResponseId))
Installed_Control_Data <- subset(Control_Data,(ResponseId%in%Control_Left$ResponseId))

Not_Installed_Control_Data <- Not_Installed_Control_Data %>% select(ResponseId,Age,Gender,Income_Score,Education_Score,Ideology_Score,Dig_Lit_Score)
Installed_Control_Data <- Installed_Control_Data %>% select(ResponseId,Age,Gender,Income_Score,Education_Score,Ideology_Score,Dig_Lit_Score)



Not_Installed_Control_Data$Installed <- 0
Installed_Control_Data$Installed <- 1

Full_data <- rbind(Not_Installed_Control_Data,Installed_Control_Data)

Full_data <- na.omit(Full_data)

#Education:
Educ_C <- round(mean(Not_Installed_Control_Data$Education_Score),2)
Educ_T <- round(mean(Installed_Control_Data$Education_Score),2)
Diff_Educ <- round((Educ_T - Educ_C),2)

Model_Educ <- coeftest(lm(Education_Score ~ Installed,data=Full_data))
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
Age_C <- round(mean(Not_Installed_Control_Data$Age),2)
Age_T <- round(mean(Installed_Control_Data$Age),2)
Diff_Age <- round((Age_T - Age_C),2)

Model_Age <- coeftest(lm(Age ~ Installed,data=Full_data))
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
Gender_C <- round(mean(Not_Installed_Control_Data$Gender),2)
Gender_T <- round(mean(Installed_Control_Data$Gender),2)
Diff_Gender <- round((Gender_T - Gender_C),2)

Model_Gender <- coeftest(lm(Gender ~ Installed,data=Full_data))
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
Income_C <- round(mean(Not_Installed_Control_Data$Income_Score),2)
Income_T <- round(mean(Installed_Control_Data$Income_Score),2)
Diff_Income <- round((Income_T - Income_C),2)

Model_Income <- coeftest(lm(Income_Score ~ Installed,data=Full_data))
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
Ideology_C <- round(mean(Not_Installed_Control_Data$Ideology_Score,na.rm=T),2)
Ideology_T <- round(mean(Installed_Control_Data$Ideology_Score,na.rm=T),2)
Diff_Ideology <- round((Ideology_T - Ideology_C),2)

Model_Ideology <- coeftest(lm(Ideology_Score ~ Installed,data=Full_data))
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


#Ideology:
Digital_C <- round(mean(Not_Installed_Control_Data$Dig_Lit_Score,na.rm=T),2)
Digital_T <- round(mean(Installed_Control_Data$Dig_Lit_Score,na.rm=T),2)
Diff_Digital <- round((Digital_T - Digital_C),2)

Model_Digital <- coeftest(lm(Dig_Lit_Score ~ Installed,data=Full_data))
Digital_P <- Model_Digital[2,4]

Digital_Stars <- ''
if(Digital_P < 0.05){
  Digital_Stars <- '*'
  if(Digital_P < 0.01){
    Digital_Stars <- '**'
    if(Digital_P < 0.001){
      Digital_Stars <- '***'
    }
  }
} 


Diff_Digital <- paste0(as.character(Diff_Digital),Digital_Stars)


Balance_T <- matrix(c('Education',Educ_T,Educ_C,Diff_Educ,
                      'Age',Age_T,Age_C,Diff_Age,
                      'Gender',Gender_T,Gender_C,Diff_Gender,
                      'Income',Income_T,Income_C,Diff_Income,
                      'Ideology',Ideology_T,Ideology_C,Diff_Ideology,
                      'Digital Literacy',Digital_T,Digital_C,Diff_Digital),byrow=T,ncol=4)


xt <- xtable(Balance_T,
             digits=2,
             align=c(
               "|p{1.5cm}|","|p{3cm}|","|p{3cm}|",
               "p{3cm}|","p{3cm}|"))

colnames(xt) <- c('Demographic','Average (Complier)','Average (Non-Complier)','Difference')


comment          <- list()
comment$pos      <- list()
comment$pos[[1]] <- c(nrow(Balance_T))
comment$command  <- c(paste("\\hline \n",  # we`ll replace all default hlines with this and the ones below
                            '\\multicolumn{4}{l}{\\textsuperscript{***}$p<0.001$, \\textsuperscript{**}$p<0.01$, \\textsuperscript{*}$p<0.05$}',
                            sep = ""))


#Write Table:
write(print(xt,include.rownames=FALSE,
            sanitize.colnames.function = identity,
            add.to.row = comment),
      file='./Tables/Balance_7.txt')


######################################################################################################

############################################## Table 8 ###############################################

######################################################################################################

Control_Data_Rs <- Installed_Control_Data
Treatment_Data_Rs <- Installed_Treatment_Data

Control_Data_Rs$Treatment <- 0
Treatment_Data_Rs$Treatment <- 1

Model_Data_7 <- rbind(Control_Data_Rs,Treatment_Data_Rs)

Model_Data_7 <- na.omit(Model_Data_7)

#Education:
Educ_C <- round(mean(Control_Data_Rs$Education_Score),2)
Educ_T <- round(mean(Treatment_Data_Rs$Education_Score),2)
Diff_Educ <- round((Educ_T - Educ_C),2)

Model_Educ <- coeftest(lm(Education_Score ~ Treatment,data=Model_Data_7))
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
Age_C <- round(mean(Control_Data_Rs$Age),2)
Age_T <- round(mean(Treatment_Data_Rs$Age),2)
Diff_Age <- round((Age_T - Age_C),2)

Model_Age <- coeftest(lm(Age ~ Treatment,data=Model_Data_7))
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
Gender_C <- round(mean(Control_Data_Rs$Gender),2)
Gender_T <- round(mean(Treatment_Data_Rs$Gender),2)
Diff_Gender <- round((Gender_T - Gender_C),2)

Model_Gender <- coeftest(lm(Gender ~ Treatment,data=Model_Data_7))
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
Income_C <- round(mean(Control_Data_Rs$Income_Score),2)
Income_T <- round(mean(Treatment_Data_Rs$Income_Score),2)
Diff_Income <- round((Income_T - Income_C),2)

Model_Income <- coeftest(lm(Income_Score ~ Treatment,data=Model_Data_7))
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
Ideology_C <- round(mean(Control_Data_Rs$Ideology_Score),2)
Ideology_T <- round(mean(Treatment_Data_Rs$Ideology_Score),2)
Diff_Ideology <- round((Ideology_T - Ideology_C),2)

Model_Ideology <- coeftest(lm(Ideology_Score ~ Treatment,data=Model_Data_7))
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

#Ideology:
Digital_C <- round(mean(Control_Data_Rs$Dig_Lit_Score,na.rm=T),2)
Digital_T <- round(mean(Treatment_Data_Rs$Dig_Lit_Score,na.rm=T),2)
Diff_Digital <- round((Digital_T - Digital_C),2)

Model_Digital <- coeftest(lm(Dig_Lit_Score ~ Treatment,data=Model_Data_7))
Digital_P <- Model_Digital[2,4]

Digital_Stars <- ''
if(Digital_P < 0.05){
  Digital_Stars <- '*'
  if(Digital_P < 0.01){
    Digital_Stars <- '**'
    if(Digital_P < 0.001){
      Digital_Stars <- '***'
    }
  }
} 


Diff_Digital <- paste0(as.character(Diff_Digital),Digital_Stars)


Balance_T <- matrix(c('Education',Educ_T,Educ_C,Diff_Educ,
                      'Age',Age_T,Age_C,Diff_Age,
                      'Gender',Gender_T,Gender_C,Diff_Gender,
                      'Income',Income_T,Income_C,Diff_Income,
                      'Ideology',Ideology_T,Ideology_C,Diff_Ideology,
                      'Digital Literacy',Digital_T,Digital_C,Diff_Digital),byrow=T,ncol=4)


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
                            '\\multicolumn{4}{l}{\\textsuperscript{***}$p<0.001$, \\textsuperscript{**}$p<0.01$, \\textsuperscript{*}$p<0.05$}',
                            sep = ""))


#Write Table:
write(print(xt,include.rownames=FALSE,
            sanitize.colnames.function = identity,
            add.to.row = comment),
      file='./Tables/Balance_8.txt')



########################## Write Output for script ###########################
writeLines(capture.output(sessionInfo()), "sessionInfo_suppl_mat.txt")










