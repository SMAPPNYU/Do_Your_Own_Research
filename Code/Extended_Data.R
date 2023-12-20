
#Author: Kevin Aslett
#Code Title: Extended_Data.R
#Paper Title: Online Searches to Evaluate Misinformation Can Increase Its Perceived Veracity
#Purpose of code: Generate all the extended data figures.

######################################################### FILES IN #########################################################

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

######################################################### FILES OUT #########################################################

#Figures:
#Extended Data 1: ./Figures/ED_fig_1.eps
#Extended Data 2: ./Figures/ED_fig_2.eps
#Extended Data 3: ./Figures/ED_fig_3.eps
#Extended Data 4: ./Figures/ED_fig_4.eps
#Extended Data 5: ./Figures/ED_fig_5.eps
#Extended Data 6: ./Figures/ED_fig_6.eps
#Extended Data 7: ./Figures/ED_fig_7.eps
#Extended Data 8: ./Figures/ED_fig_8.eps
#Extended Data 9: ./Figures/ED_fig_9.eps
#Extended Data 10: ./Figures/ED_fig_10.eps

#Source Data:
#SD for ED1: ./source_data/ED_fig_1_source_data.csv
#SD for ED2: ./source_data/ED_fig_2_source_data.csv
#SD for ED3: ./source_data/ED_fig_3_source_data.csv
#SD for ED4: ./source_data/ED_fig_4_source_data.csv
#SD for ED5: ./source_data/ED_fig_5_source_data.csv
#SD for ED6: ./source_data/ED_fig_6_source_data.csv
#SD for ED7: ./source_data/ED_fig_7_source_data.csv
#SD for ED8: ./source_data/ED_fig_8_source_data.csv
#SD for ED9: ./source_data/ED_fig_9_source_data.csv
#SD for ED10: ./source_data/ED_fig_10_source_data.csv


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


#################################################################################

########################### Extended Data 1 #####################################

#################################################################################

#Read in data:
Data_1_FM <- read.csv('./Data/Study_6_FM.csv')
Data_1_T_M <- read.csv('./Data/Study_6_T_M.csv')
Data_1_T_LQ <- read.csv('./Data/Study_6_T_LQ.csv')


nrow(Data_1_FM %>% filter(Treatment_1 == 1))
nrow(Data_1_FM %>% filter(Treatment_2 == 1))
nrow(Data_1_FM %>% filter(Treatment_3 == 1))
nrow(Data_1_FM %>% filter(Treatment_1 == 0 & Treatment_2 == 0 & Treatment_3 == 0))

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

New_matr <- matrix(c(D_1_result_1_1$coefficients[1],CI_1_1_1[1,1],CI_1_1_1[1,2],'False/Misleading','Treatment Group 1\n',D_1_result_1_1$coeftable[1,4],D_1_result_1_1$coeftable[1,2],
                     D_1_result_1_1$coefficients[2],CI_1_1_1[2,1],CI_1_1_1[2,2],'False/Misleading','Treatment Group 2\n',D_1_result_1_1$coeftable[2,4],D_1_result_1_1$coeftable[2,2],
                     D_1_result_1_1$coefficients[3],CI_1_1_1[3,1],CI_1_1_1[3,2],'False/Misleading','Treatment Group 3\n',D_1_result_1_1$coeftable[3,4],D_1_result_1_1$coeftable[3,2],
                     D_1_result_2_1$coefficients[1],CI_1_2_1[1,1],CI_1_2_1[1,2],'True-Mainstream','Treatment Group 1\n',D_1_result_1_1$coeftable[1,4],D_1_result_1_1$coeftable[1,2],
                     D_1_result_2_1$coefficients[2],CI_1_2_1[2,1],CI_1_2_1[2,2],'True-Mainstream','Treatment Group 2\n',D_1_result_1_1$coeftable[2,4],D_1_result_1_1$coeftable[2,2],
                     D_1_result_2_1$coefficients[3],CI_1_2_1[3,1],CI_1_2_1[3,2],'True-Mainstream','Treatment Group 3\n',D_1_result_1_1$coeftable[3,4],D_1_result_1_1$coeftable[3,2],
                     D_1_result_3_1$coefficients[1],CI_1_3_1[1,1],CI_1_3_1[1,2],'True-Low Quality','Treatment Group 1\n',D_1_result_1_1$coeftable[1,4],D_1_result_1_1$coeftable[1,2],
                     D_1_result_3_1$coefficients[2],CI_1_3_1[2,1],CI_1_3_1[2,2],'True-Low Quality','Treatment Group 2\n',D_1_result_1_1$coeftable[2,4],D_1_result_1_1$coeftable[2,2],
                     D_1_result_3_1$coefficients[3],CI_1_3_1[3,1],CI_1_3_1[3,2],'True-Low Quality','Treatment Group 3\n',D_1_result_1_1$coeftable[3,4],D_1_result_1_1$coeftable[3,2]),ncol=7,byrow=T)

#Transform matrix into dataframe:
Fig_2d_Mat <- as.data.frame(New_matr)


#Create dataframe to produce plot
Fig_2d_Mat <- na.omit(Fig_2d_Mat)
colnames(Fig_2d_Mat) <- c('Coef','Low_Conf','Upp_Conf','Measure','Instructions','P_Value','Standard_Error')
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
  scale_shape_manual(values= c(19,17,15), name = "Instructions Group") +
  scale_color_manual(values=cbbPalette_1, name = "Instructions Group") +
  ylab("\n Effect of Searching Online on Belief in News\n Dependent on Type of Instructions") +
  theme_classic() +
  theme(axis.title.x = element_text(size=16),
        axis.text.x  = element_text(size=14),
        axis.title.y = element_text(size=14),
        axis.text.y  = element_text(size=14),
        plot.title = element_text(size=14),
        legend.title = element_text(size=14),
        legend.text = element_text(size=14)) +
  ylim(-0.1,0.2) +
  scale_x_continuous("Type of News \n",breaks=c(2.1,1.5,0.9),labels=c('False/Misleading','True-Mainstream','True-Low Quality'),limits=c(0.5,2.5)) +
  coord_flip()

#Save Figure:
ggsave('./Figures/ED_fig_1.eps',height=16,width=18,units='cm',device='eps')

Fig_2d_Mat$x <- NULL

#Round variables:
Fig_2d_Mat$P_Value <- round(as.numeric(Fig_2d_Mat$P_Value),6)
Fig_2d_Mat$Standard_Error <- round(as.numeric(Fig_2d_Mat$Standard_Error),6)
Fig_2d_Mat$Coef <- round(as.numeric(Fig_2d_Mat$Coef),6)
Fig_2d_Mat$Low_Conf <- round(as.numeric(Fig_2d_Mat$Low_Conf),6)
Fig_2d_Mat$Upp_Conf <- round(as.numeric(Fig_2d_Mat$Upp_Conf),6)

#Rename columns:
colnames(Fig_2d_Mat) <- c('Effect','CI_Lower','CI_Upper','DV_Measure','Instructions_Group','P_Value','Standard_Error')

#Reorder columns;
Fig_2d_Mat <- Fig_2d_Mat %>% select(DV_Measure,Instructions_Group,Effect,CI_Lower,CI_Upper,P_Value,Standard_Error)

write.csv(Fig_2d_Mat,'./source_data/ED_fig_1_source_data.csv')



###########################################################################################################

################################################# Extended Data 2 #########################################

###########################################################################################################

#### Ideology by Study 1:
library(tidyverse)
library(fixest)

#Pull in Study 1 data:
Study_1_df <- read.csv('./Data/Study_1_df_FM.csv')
Study_1_df$Article_day <- as.character(Study_1_df$Article_day)
Study_1_df$ResponseId <- as.character(Study_1_df$ResponseId)

#Remove NA values:
Study_1_df = na.omit(Study_1_df)

#Read study 1 ideology csv
Study_1_ideology <- read.csv('./Data/Study_1_ideology.csv')

#Merge ideology:
Study_1_df <- merge(Study_1_df,Study_1_ideology,by='ResponseId')

###### Liberals:
Study_1_Lib <- Study_1_df %>% filter(groups_4_Ideology == 'Liberal')

#Run OLS Model with clustered standard errors:
lin_results_fit_1_1 = feols(Susc_FN ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Study_1_Lib)
#Produce confidence intervals with clustered standard errors:
CI_1_1 <- confint(lin_results_fit_1_1)

#Cohen's D:
lin_results_fit_1_1$coefficients[1]/sd(Study_1_df$Susc_FN)

#Conservatives:
Study_1_Cons <- Study_1_df %>% filter(groups_4_Ideology == 'Conservative')

#Run OLS Model with clustered standard errors:
lin_results_fit_1_2 = feols(Susc_FN ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Study_1_Cons)
#Produce confidence intervals with clustered standard errors:
CI_1_2 <- confint(lin_results_fit_1_2)

#Moderates:
Study_1_Mod <- Study_1_df %>% filter(groups_4_Ideology == 'Moderate')

#Run OLS Model with clustered standard errors:
lin_results_fit_1_3 = feols(Susc_FN ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Study_1_Mod)
#Produce confidence intervals with clustered standard errors:
CI_1_3 <- confint(lin_results_fit_1_3)




#Run Model Testing Effect of Searching Online on Belief in Misinformation for Study 2:
#Pull in this data: Search Experiment 1: Study 1:
Study_2_df <- read.csv('./Data/Study_2_df_FM.csv')
Study_2_df$Article_day <- as.character(Study_2_df$Article_day)
Study_2_df$ResponseId <- as.character(Study_2_df$ResponseId)

#Remove NA values:
Study_2_df = na.omit(Study_2_df)

#Read study 1 ideology csv
Study_2_ideology <- read.csv('./Data/Study_2_ideology.csv')

#Merge ideology:
Study_2_df <- merge(Study_2_df,Study_2_ideology,by='ResponseId')

###### Liberals:
Study_2_Lib <- Study_2_df %>% filter(groups_4_Ideology == 'Liberal')

#Run OLS Model with clustered standard errors:
lin_results_fit_2_1 = feols(Susc_FN ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Study_2_Lib)
#Produce confidence intervals with clustered standard errors:
CI_2_1 <- confint(lin_results_fit_2_1)

#Cohen's D:
lin_results_fit_1_1$coefficients[1]/sd(Study_2_df$Susc_FN)

#Conservatives:
Study_2_Cons <- Study_2_df %>% filter(groups_4_Ideology == 'Conservative')

#Run OLS Model with clustered standard errors:
lin_results_fit_2_2 = feols(Susc_FN ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Study_2_Cons)
#Produce confidence intervals with clustered standard errors:
CI_2_2 <- confint(lin_results_fit_2_2)

#Moderates:
Study_2_Mod <- Study_2_df %>% filter(groups_4_Ideology == 'Moderate')

#Run OLS Model with clustered standard errors:
lin_results_fit_2_3 = feols(Susc_FN ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Study_2_Mod)
#Produce confidence intervals with clustered standard errors:
CI_2_3 <- confint(lin_results_fit_2_3)



#Run Model Testing Effect of Searching Online on Belief in Misinformation for Study 3:
#Pull in this data: Search Experiment 1: Study 1:
Study_3_df <- read.csv('./Data/Study_3_df_FM.csv')
Study_3_df$Article_day <- as.character(Study_3_df$Article_day)
Study_3_df$ResponseId <- as.character(Study_3_df$ResponseId)

#Remove NA values:
Study_3_df = na.omit(Study_3_df)

#Read study 1 ideology csv
Study_3_ideology <- read.csv('./Data/Study_3_ideology.csv')

#Merge ideology:
Study_3_df <- merge(Study_3_df,Study_3_ideology,by='ResponseId')

###### Liberals:
Study_3_Lib <- Study_3_df %>% filter(groups_4_Ideology == 'Liberal')

#Run OLS Model with clustered standard errors:
lin_results_fit_3_1 = feols(Susc_FN ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Study_3_Lib)
#Produce confidence intervals with clustered standard errors:
CI_3_1 <- confint(lin_results_fit_3_1)

#Cohen's D:
lin_results_fit_3_1$coefficients[1]/sd(Study_3_df$Susc_FN)

#Conservatives:
Study_3_Cons <- Study_3_df %>% filter(groups_4_Ideology == 'Conservative')

#Run OLS Model with clustered standard errors:
lin_results_fit_3_2 = feols(Susc_FN ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Study_3_Cons)
#Produce confidence intervals with clustered standard errors:
CI_3_2 <- confint(lin_results_fit_3_2)

#Moderates:
Study_3_Mod <- Study_3_df %>% filter(groups_4_Ideology == 'Moderate')

#Run OLS Model with clustered standard errors:
lin_results_fit_3_3 = feols(Susc_FN ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Study_3_Mod)
#Produce confidence intervals with clustered standard errors:
CI_3_3 <- confint(lin_results_fit_3_3)

library(negligible)

#Run Model Testing Effect of Searching Online on Belief in Misinformation for Study 4:
#Pull in this data: Search Experiment 1: Study 1:
Study_4_df <- read.csv('./Data/Study_4_df_FM.csv')
Study_4_df$Article_day <- as.character(Study_4_df$Article_day)
Study_4_df$ResponseId <- as.character(Study_4_df$ResponseId)

#Remove NA values:
Study_4_df = na.omit(Study_4_df)

#Read study 1 ideology csv
Study_4_ideology <- read.csv('./Data/Study_4_ideology.csv')

#Merge ideology:
Study_4_df <- merge(Study_4_df,Study_4_ideology,by='ResponseId')

###### Liberals:
Study_4_Lib <- Study_4_df %>% filter(groups_4_Ideology == 'Liberal')

#Run OLS Model with clustered standard errors:
lin_results_fit_4_1 = feols(Susc_FN ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Study_4_Lib)
#Produce confidence intervals with clustered standard errors:
CI_4_1 <- confint(lin_results_fit_4_1)

#Cohen's D:
lin_results_fit_1_1$coefficients[1]/sd(Study_4_df$Susc_FN)

#Conservatives:
Study_4_Cons <- Study_4_df %>% filter(groups_4_Ideology == 'Conservative')

#Run OLS Model with clustered standard errors:
lin_results_fit_4_2 = feols(Susc_FN ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Study_4_Cons)
#Produce confidence intervals with clustered standard errors:
CI_4_2 <- confint(lin_results_fit_4_2)

#Moderates:
Study_4_Mod <- Study_4_df %>% filter(groups_4_Ideology == 'Moderate')

#Run OLS Model with clustered standard errors:
lin_results_fit_4_3 = feols(Susc_FN ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Study_4_Mod)
#Produce confidence intervals with clustered standard errors:
CI_4_3 <- confint(lin_results_fit_4_3)


#Pull in Fact-Checker Ideological Perspective
#Pull in this data: Search Experiment 1: Study 1:
Study_5_df <- read.csv('./Data/Study_5_df_FM.csv')
FCer_details <- read.csv('./Data/FCer_details.csv')
Study_5_df$Article_day <- as.character(Study_5_df$Article_day)
Study_5_df$ResponseId <- as.character(Study_5_df$ResponseId)
Study_5_df <- merge(Study_5_df,FCer_details,by='Article_day')

#Remove NA values:
Study_5_df = na.omit(Study_5_df)


#Read study 1 ideology csv
Study_5_ideology <- read.csv('./Data/Study_5_ideology.csv')

#Merge ideology:
Study_5_df <- merge(Study_5_df,Study_5_ideology,by='ResponseId')


Study_5_df <- unique(Study_5_df)

###### Liberals:
Study_5_Lib <- Study_5_df %>% filter(groups_4_Ideology == 'Liberal')

#Run OLS Model with clustered standard errors:
lin_results_fit_5_1 = feols(True_Dummy ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Study_5_Lib)
#Produce confidence intervals with clustered standard errors:
CI_5_1 <- confint(lin_results_fit_5_1)

#Cohen's D:
lin_results_fit_5_1$coefficients[1]/sd(Study_5_df$Susc_FN)

#Conservatives:
Study_5_Cons <- Study_5_df %>% filter(groups_4_Ideology == 'Conservative')

#Run OLS Model with clustered standard errors:
lin_results_fit_5_2 = feols(True_Dummy ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Study_5_Cons)
#Produce confidence intervals with clustered standard errors:
CI_5_2 <- confint(lin_results_fit_5_2)

#Moderates:
Study_5_Mod <- Study_5_df %>% filter(groups_4_Ideology == 'Moderate')

#Run OLS Model with clustered standard errors:
lin_results_fit_5_3 = feols(True_Dummy ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Study_5_Mod)
#Produce confidence intervals with clustered standard errors:
CI_5_3 <- confint(lin_results_fit_5_3)





########################################### Figure 1A: Categorical (Rate as True) ####################################################

#Create vector with Study names:
Coef_names <- rev(c('Study 1',
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
                    'Study 5'))

#Create vector with coefficients:
Coefficients <- rev(c(lin_results_fit_1_2$coefficients[1],
                      lin_results_fit_1_3$coefficients[1],
                      lin_results_fit_1_1$coefficients[1],
                      lin_results_fit_2_2$coefficients[1],
                      lin_results_fit_2_3$coefficients[1],
                      lin_results_fit_2_1$coefficients[1],
                      lin_results_fit_3_2$coefficients[1],
                      lin_results_fit_3_3$coefficients[1],
                      lin_results_fit_3_1$coefficients[1],
                      lin_results_fit_4_2$coefficients[1],
                      lin_results_fit_4_3$coefficients[1],
                      lin_results_fit_4_1$coefficients[1],
                      lin_results_fit_5_2$coefficients[1],
                      lin_results_fit_5_3$coefficients[1],
                      lin_results_fit_5_1$coefficients[1]))

#Create vector with upper confidence intervals:
CI_Upper <- rev(c(CI_1_2[1,2],
                  CI_1_3[1,2],
                  CI_1_1[1,2],
                  CI_2_2[1,2],
                  CI_2_3[1,2],
                  CI_2_1[1,2],
                  CI_3_2[1,2],
                  CI_3_3[1,2],
                  CI_3_1[1,2],
                  CI_4_2[1,2],
                  CI_4_3[1,2],
                  CI_4_1[1,2],
                  CI_5_2[1,2],
                  CI_5_3[1,2],
                  CI_5_1[1,2]))

#Create vector with lower confidence intervals:
CI_Lower <- rev(c(CI_1_2[1,1],
                  CI_1_3[1,1],
                  CI_1_1[1,1],
                  CI_2_2[1,1],
                  CI_2_3[1,1],
                  CI_2_1[1,1],
                  CI_3_2[1,1],
                  CI_3_3[1,1],
                  CI_3_1[1,1],
                  CI_4_2[1,1],
                  CI_4_3[1,1],
                  CI_4_1[1,1],
                  CI_5_2[1,1],
                  CI_5_3[1,1],
                  CI_5_1[1,1]))

Resp_Ideology <- rev(c('Conservative',
                       'Moderate',
                       'Liberal',
                       'Conservative',
                       'Moderate',
                       'Liberal',
                       'Conservative',
                       'Moderate',
                       'Liberal',
                       'Conservative',
                       'Moderate',
                       'Liberal',
                       'Conservative',
                       'Moderate',
                       'Liberal')) 


P_Value <- rev(c(lin_results_fit_1_2$coeftable[1,4],
                 lin_results_fit_1_3$coeftable[1,4],
                 lin_results_fit_1_1$coeftable[1,4],
                 lin_results_fit_2_2$coeftable[1,4],
                 lin_results_fit_2_3$coeftable[1,4],
                 lin_results_fit_2_1$coeftable[1,4],
                 lin_results_fit_3_2$coeftable[1,4],
                 lin_results_fit_3_3$coeftable[1,4],
                 lin_results_fit_3_1$coeftable[1,4],
                 lin_results_fit_4_2$coeftable[1,4],
                 lin_results_fit_4_3$coeftable[1,4],
                 lin_results_fit_4_1$coeftable[1,4],
                 lin_results_fit_5_2$coeftable[1,4],
                 lin_results_fit_5_3$coeftable[1,4],
                 lin_results_fit_5_1$coeftable[1,4]))

Standard_Error <- rev(c(lin_results_fit_1_2$coeftable[1,2],
                        lin_results_fit_1_3$coeftable[1,2],
                        lin_results_fit_1_1$coeftable[1,2],
                        lin_results_fit_2_2$coeftable[1,2],
                        lin_results_fit_2_3$coeftable[1,2],
                        lin_results_fit_2_1$coeftable[1,2],
                        lin_results_fit_3_2$coeftable[1,2],
                        lin_results_fit_3_3$coeftable[1,2],
                        lin_results_fit_3_1$coeftable[1,2],
                        lin_results_fit_4_2$coeftable[1,2],
                        lin_results_fit_4_3$coeftable[1,2],
                        lin_results_fit_4_1$coeftable[1,2],
                        lin_results_fit_5_2$coeftable[1,2],
                        lin_results_fit_5_3$coeftable[1,2],
                        lin_results_fit_5_1$coeftable[1,2]))

#Put together matrix with data for plot:
d_matrix <- cbind(Coef_names,Coefficients,CI_Upper,CI_Lower,Resp_Ideology,P_Value,Standard_Error)
rownames(d_matrix) <- c()
d_matrix <- data.frame(d_matrix)
d_matrix$Coefficients <- as.character(d_matrix$Coefficients)
d_matrix$CI_Lower <- as.character(d_matrix$CI_Lower)
d_matrix$CI_Upper <- as.character(d_matrix$CI_Upper)
d_matrix$Coefficients <- as.numeric(d_matrix$Coefficients)
d_matrix$CI_Lower <- as.numeric(d_matrix$CI_Lower)
d_matrix$CI_Upper <- as.numeric(d_matrix$CI_Upper)

#Set points on Y-Axis:
d_matrix$x<-c(0.3,0.5,0.7,
              1.3,1.5,1.7,
              2.3,2.5,2.7,
              3.3,3.5,3.7,
              4.3,4.5,4.7)


d_matrix$Resp_Ideology <- factor(d_matrix$Resp_Ideology,levels=c("Conservative",
                                                                 "Moderate",
                                                                 "Liberal"))


#Produce plot:
ggplot(data = d_matrix, aes(x = x, y = Coefficients,color=Resp_Ideology)) +
  geom_hline(aes(yintercept = 0), color = "gray",
             linetype = 2, size = 1.2) +
  geom_point(size=4) +
  geom_linerange(aes(min = CI_Lower,
                     max = CI_Upper),
                 size=1.5) +
  scale_color_manual(values=cbbPalette_3, name = "Ideology") +
  ylab("\nEffect of Searching Online on Probability \nof Rating Misinformation as True") +
  theme_classic() +
  theme(axis.title.x = element_text(size=18),
        axis.text.x  = element_text(size=16),
        axis.title.y = element_text(size=16),
        axis.text.y  = element_text(size=22),
        plot.title = element_text(size = 16),
        legend.title = element_text(size=16),
        legend.text = element_text(size=14),
        title =element_text(size=16, face='bold')) +
  ylim(-0.1,0.25) +
  scale_x_continuous(" \n",breaks=c(0.5,1.5,2.5,3.5,4.5),labels=c('Study 5',
                                                                  'Study 4',
                                                                  'Study 3',
                                                                  'Study 2',
                                                                  'Study 1'),limits=c(0,5)) +
  coord_flip()

#Save figure:
ggsave('./Figures/ED_fig_2.eps',height=16,width=18,units='cm',device="eps")



d_matrix$x <- NULL


#Round variables:
d_matrix$P_Value <- round(as.numeric(d_matrix$P_Value),6)
d_matrix$Standard_Error <- round(as.numeric(d_matrix$Standard_Error),6)
d_matrix$Coefficients <- round(as.numeric(d_matrix$Coefficients),6)
d_matrix$CI_Lower <- round(as.numeric(d_matrix$CI_Lower),6)
d_matrix$CI_Upper <- round(as.numeric(d_matrix$CI_Upper),6)

#Rename columns:
colnames(d_matrix) <- c('Study','Effect','CI_Upper','CI_Lower','Respondent_Ideology','P_Value','Standard_Error')

#Reorder columns;
d_matrix <- d_matrix %>% select(Study,Respondent_Ideology,Effect,CI_Lower,CI_Upper,P_Value,Standard_Error)

write.csv(d_matrix,'./source_data/extended_data_2_source_data.csv')





################################################################################################################

############################### Figure 4c: Quantiles_Congruent_FULL.jpg ########################################

################################################################################################################

Study_5_treat_data <- read.csv('./Data/Study_5_treat_data.csv')

#Pull in treatment data for Study 5:
T_Data <- Study_5_treat_data %>% filter(FC_Eval == 'FM')

#Filter only responses who only saw very reliable news sites in Google Search Results (85)
lowest_half_T_data <- T_Data %>% filter(avg_score < median(T_Data$avg_score,na.rm=T))

#Pull control data:
Control_Data <- read.csv('./Data/Control_Data_Study_5.csv')

#Ideological Perspective of Articles:
FC_Ideo_Data <- read.csv('./Data/FC_Ideo_Data.csv')
FC_Ideo_Data$X <- NULL


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
Control_Data <- Control_Data %>% select(Article_day,True_Dummy,Four_Ordinal,Seven_Ordinal,Age,Gender,ResponseId,Education_Score,Income_Score,Ideo_Congruence,Dummy_Ideology,Article_Lean)
Control_Data$Treatment <- 0

lowest_half_T_data <- lowest_half_T_data %>% select(Article_day,True_Dummy,Four_Ordinal,Seven_Ordinal,Age,Gender,ResponseId,Education_Score,Income_Score,Ideo_Congruence,Dummy_Ideology,Article_Lean)
lowest_half_T_data$ResponseId <- as.character(lowest_half_T_data$ResponseId)
Control_Data$ResponseId <- as.character(Control_Data$ResponseId)
lowest_half_T_data$Article_day <- as.character(lowest_half_T_data$Article_day)

lowest_half_T_data$Treatment <- 1


#Merge treatment and control articles:
Study_5_subset_1 <- rbind(lowest_half_T_data,Control_Data)

Study_5_subset_1 <- Study_5_subset_1 %>% select(True_Dummy,Four_Ordinal,Seven_Ordinal,Treatment,Age,Ideo_Congruence,Education_Score,Gender,Income_Score,Article_day,ResponseId,Dummy_Ideology,Article_Lean)

Study_5_subset_1 <- Study_5_subset_1 %>% filter(Ideo_Congruence == 1)

#Remove NAs
#Study_5_subset_1 <- na.omit(Study_5_subset_1)

#Run OLS Model with clustered standard errors:
fit_2_1 = feols(Seven_Ordinal ~ Treatment + Age + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=Study_5_subset_1)
CI_2_1 = confint(fit_2_1,se='twoway')

#Run OLS Model with clustered standard errors:
fit_2_2 = feols(Four_Ordinal ~ Treatment + Age + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=Study_5_subset_1)
CI_2_2 = confint(fit_2_2,se='twoway')

#Run OLS Model with clustered standard errors:
fit_2_3 = feols(True_Dummy ~ Treatment + Age + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=Study_5_subset_1)
CI_2_3 = confint(fit_2_3,se='twoway')

#Create empty matrix:
Fig_Mat <- matrix(ncol=8)

#Merge data:
New_matr <- matrix(c(round(fit_2_1$coefficients[1]/sd(Study_5_subset_1$Seven_Ordinal),4),round(CI_2_1[1,1]/sd(Study_5_subset_1$Seven_Ordinal),4),round(CI_2_1[1,2]/sd(Study_5_subset_1$Seven_Ordinal),4),'Ordinal (7)','Bottom 50%\nNews Quality',fit_2_1$coeftable[1,4],fit_2_1$coeftable[1,2],'Ideologically Congruent',
                     round(fit_2_2$coefficients[1]/sd(Study_5_subset_1$Four_Ordinal),4),round(CI_2_2[1,1]/sd(Study_5_subset_1$Four_Ordinal),4),round(CI_2_2[1,2]/sd(Study_5_subset_1$Four_Ordinal),4),'Ordinal (4)','Bottom 50%\nNews Quality',fit_2_2$coeftable[1,4],fit_2_2$coeftable[1,2],'Ideologically Congruent',
                     round(fit_2_3$coefficients[1]/sd(Study_5_subset_1$True_Dummy),4),round(CI_2_3[1,1]/sd(Study_5_subset_1$True_Dummy),4),round(CI_2_3[1,2]/sd(Study_5_subset_1$True_Dummy),4),'True (Dummy)','Bottom 50%\nNews Quality',fit_2_3$coeftable[1,4],fit_2_3$coeftable[1,2],'Ideologically Congruent'),ncol=8,byrow=T)

Fig_Mat <- rbind(Fig_Mat,New_matr)



upper_half_T_data <- T_Data %>% filter(avg_score >= median(T_Data$avg_score,na.rm=T))

#Select Variables:
upper_half_T_data <- upper_half_T_data %>% select(Article_day,True_Dummy,Four_Ordinal,Seven_Ordinal,Age,Gender,ResponseId,Education_Score,Income_Score,Ideo_Congruence,Dummy_Ideology,Article_Lean)
upper_half_T_data$ResponseId <- as.character(upper_half_T_data$ResponseId)
upper_half_T_data$Article_day <- as.character(upper_half_T_data$Article_day)
upper_half_T_data$Treatment <- 1

#Merge treatment and control articles:
upper_half_all_data <- rbind(upper_half_T_data,Control_Data)

#Remove NAs
upper_half_all_data <- na.omit(upper_half_all_data)

Study_5_subset_2 <- rbind(upper_half_all_data,Control_Data)

Study_5_subset_2 <- Study_5_subset_2 %>% select(True_Dummy,Four_Ordinal,Seven_Ordinal,Treatment,Age,Ideo_Congruence,Education_Score,Gender,Income_Score,Article_day,ResponseId,Dummy_Ideology,Article_Lean)

Study_5_subset_2 <- Study_5_subset_2 %>% filter(Ideo_Congruence == 1)


#Run OLS Model with clustered standard errors:
fit_2_1 = feols(Seven_Ordinal ~ Treatment + Age+ Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=Study_5_subset_2)
CI_2_1 = confint(fit_2_1,se='twoway')

#Run OLS Model with clustered standard errors:
fit_2_2 = feols(Four_Ordinal ~ Treatment + Age + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=Study_5_subset_2)
CI_2_2 = confint(fit_2_2,se='twoway')

#Run OLS Model with clustered standard errors:
fit_2_3 = feols(True_Dummy ~ Treatment + Age + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=Study_5_subset_2)
CI_2_3 = confint(fit_2_3,se='twoway')

#Merge data:
New_matr <- matrix(c(round(fit_2_1$coefficients[1]/sd(Study_5_subset_2$Seven_Ordinal),4),round(CI_2_1[1,1]/sd(Study_5_subset_2$Seven_Ordinal),4),round(CI_2_1[1,2]/sd(Study_5_subset_2$Seven_Ordinal),4),'Ordinal (7)','Top 50%\nNews Quality',fit_2_1$coeftable[1,4],fit_2_1$coeftable[1,2],'Ideologically Congruent',
                     round(fit_2_2$coefficients[1]/sd(Study_5_subset_2$Four_Ordinal),4),round(CI_2_2[1,1]/sd(Study_5_subset_2$Four_Ordinal),4),round(CI_2_2[1,2]/sd(Study_5_subset_2$Four_Ordinal),4),'Ordinal (4)','Top 50%\nNews Quality',fit_2_2$coeftable[1,4],fit_2_2$coeftable[1,2],'Ideologically Congruent',
                     round(fit_2_3$coefficients[1]/sd(Study_5_subset_2$True_Dummy),4),round(CI_2_3[1,1]/sd(Study_5_subset_2$True_Dummy),4),round(CI_2_3[1,2]/sd(Study_5_subset_2$True_Dummy),4),'True (Dummy)','Top 50%\nNews Quality',fit_2_3$coeftable[1,4],fit_2_3$coeftable[1,2],'Ideologically Congruent'),ncol=8,byrow=T)


Fig_Mat <- rbind(Fig_Mat,New_matr)

#Use dataframe to create plot:
Fig_Mat <- as.data.frame(Fig_Mat)
Fig_Mat <- na.omit(Fig_Mat)
colnames(Fig_Mat) <- c('Coef','Low_Conf','Upp_Conf','Measure','Type_News','P_Value','Standard_Error','Ideological_Congruence')
Fig_Mat <- na.omit(Fig_Mat)

Fig_Mat$x<- c(0.8,0.9,1.0,1.4,1.5,1.6)
Fig_Mat$Coef <- as.character(Fig_Mat$Coef)
Fig_Mat$Coef <- as.numeric(Fig_Mat$Coef)
Fig_Mat$Upp_Conf <- as.character(Fig_Mat$Upp_Conf)
Fig_Mat$Upp_Conf <- as.numeric(Fig_Mat$Upp_Conf)
Fig_Mat$Low_Conf <- as.character(Fig_Mat$Low_Conf)
Fig_Mat$Low_Conf <- as.numeric(Fig_Mat$Low_Conf)
Fig_Mat$Type_News <- factor(Fig_Mat$Type_News,levels=c('Bottom 50%\nNews Quality',
                                                       'Top 50%\nNews Quality'))

Fig_Mat_1 <- Fig_Mat

Fig_Mat$x <- NULL

write.csv(Fig_Mat,'./source_data/extended_data_3a.csv')



################################################################################################################

################################ Figure 4d: Quantiles_Incong_FULL.jpg ##########################################

################################################################################################################

Study_5_subset_1 <- rbind(lowest_half_T_data,Control_Data)
Study_5_subset_1 <- Study_5_subset_1 %>% select(True_Dummy,Four_Ordinal,Seven_Ordinal,Treatment,Age,Ideo_Congruence,Education_Score,Gender,Income_Score,Article_day,ResponseId,Dummy_Ideology,Article_Lean)

Study_5_subset_1a <- Study_5_subset_1 %>% filter(Article_Lean == 'Liberal' & Dummy_Ideology == 'Conservative')
Study_5_subset_1b <- Study_5_subset_1 %>% filter(Article_Lean == 'Conservative' & Dummy_Ideology == 'Liberal')

Study_5_subset_1 <- rbind(Study_5_subset_1a,Study_5_subset_1b)

#Remove NAs
#Study_5_subset_1 <- na.omit(Study_5_subset_1)

#Run OLS Model with clustered standard errors:
fit_2_1 = feols(Seven_Ordinal ~ Treatment + Age + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=Study_5_subset_1)
CI_2_1 = confint(fit_2_1,se='twoway')

#Run OLS Model with clustered standard errors:
fit_2_2 = feols(Four_Ordinal ~ Treatment + Age + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=Study_5_subset_1)
CI_2_2 = confint(fit_2_2,se='twoway')

#Run OLS Model with clustered standard errors:
fit_2_3 = feols(True_Dummy ~ Treatment + Age + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=Study_5_subset_1)
CI_2_3 = confint(fit_2_3,se='twoway')

#Create empty matrix:
Fig_Mat <- matrix(ncol=8)

#Merge data:
New_matr <- matrix(c(round(fit_2_1$coefficients[1]/sd(Study_5_subset_1$Seven_Ordinal),4),round(CI_2_1[1,1]/sd(Study_5_subset_1$Seven_Ordinal),4),round(CI_2_1[1,2]/sd(Study_5_subset_1$Seven_Ordinal),4),'Ordinal (7)','Bottom 50%\nNews Quality',fit_2_1$coeftable[1,4],fit_2_1$coeftable[1,2],'Ideologically Incongruent',
                     round(fit_2_2$coefficients[1]/sd(Study_5_subset_1$Four_Ordinal),4),round(CI_2_2[1,1]/sd(Study_5_subset_1$Four_Ordinal),4),round(CI_2_2[1,2]/sd(Study_5_subset_1$Four_Ordinal),4),'Ordinal (4)','Bottom 50%\nNews Quality',fit_2_2$coeftable[1,4],fit_2_2$coeftable[1,2],'Ideologically Incongruent',
                     round(fit_2_3$coefficients[1]/sd(Study_5_subset_1$True_Dummy),4),round(CI_2_3[1,1]/sd(Study_5_subset_1$True_Dummy),4),round(CI_2_3[1,2]/sd(Study_5_subset_1$True_Dummy),4),'True (Dummy)','Bottom 50%\nNews Quality',fit_2_3$coeftable[1,4],fit_2_3$coeftable[1,2],'Ideologically Incongruent'),ncol=8,byrow=T)

Fig_Mat <- rbind(Fig_Mat,New_matr)


#Filter
Study_5_subset_2 <- rbind(upper_half_all_data,Control_Data)
Study_5_subset_2 <- Study_5_subset_2 %>% select(True_Dummy,Four_Ordinal,Seven_Ordinal,Treatment,Age,Ideo_Congruence,Education_Score,Gender,Income_Score,Article_day,ResponseId,Dummy_Ideology,Article_Lean)

Study_5_subset_2a <- Study_5_subset_2 %>% filter(Article_Lean == 'Liberal' & Dummy_Ideology == 'Conservative')
Study_5_subset_2b <- Study_5_subset_2 %>% filter(Article_Lean == 'Conservative' & Dummy_Ideology == 'Liberal')

Study_5_subset_2 <- rbind(Study_5_subset_2a,Study_5_subset_2b)

#Run OLS Model with clustered standard errors:
fit_2_1 = feols(Seven_Ordinal ~ Treatment + Age+ Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=Study_5_subset_2)
CI_2_1 = confint(fit_2_1,se='twoway')

#Run OLS Model with clustered standard errors:
fit_2_2 = feols(Four_Ordinal ~ Treatment + Age + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=Study_5_subset_2)
CI_2_2 = confint(fit_2_2,se='twoway')

#Run OLS Model with clustered standard errors:
fit_2_3 = feols(True_Dummy ~ Treatment + Age + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=Study_5_subset_2)
CI_2_3 = confint(fit_2_3,se='twoway')

#Merge data:
New_matr <- matrix(c(round(fit_2_1$coefficients[1]/sd(Study_5_subset_2$Seven_Ordinal),4),round(CI_2_1[1,1]/sd(Study_5_subset_2$Seven_Ordinal),4),round(CI_2_1[1,2]/sd(Study_5_subset_2$Seven_Ordinal),4),'Ordinal (7)','Top 50%\nNews Quality',fit_2_1$coeftable[1,4],fit_2_1$coeftable[1,2],'Ideologically Incongruent',
                     round(fit_2_2$coefficients[1]/sd(Study_5_subset_2$Four_Ordinal),4),round(CI_2_2[1,1]/sd(Study_5_subset_2$Four_Ordinal),4),round(CI_2_2[1,2]/sd(Study_5_subset_2$Four_Ordinal),4),'Ordinal (4)','Top 50%\nNews Quality',fit_2_2$coeftable[1,4],fit_2_2$coeftable[1,2],'Ideologically Incongruent',
                     round(fit_2_3$coefficients[1]/sd(Study_5_subset_2$True_Dummy),4),round(CI_2_3[1,1]/sd(Study_5_subset_2$True_Dummy),4),round(CI_2_3[1,2]/sd(Study_5_subset_2$True_Dummy),4),'True (Dummy)','Top 50%\nNews Quality',fit_2_3$coeftable[1,4],fit_2_3$coeftable[1,2],'Ideologically Incongruent'),ncol=8,byrow=T)


Fig_Mat <- rbind(Fig_Mat,New_matr)

#Use dataframe to create plot:
Fig_Mat <- as.data.frame(Fig_Mat)
Fig_Mat <- na.omit(Fig_Mat)
colnames(Fig_Mat) <- c('Coef','Low_Conf','Upp_Conf','Measure','Type_News','P_Value','Standard_Error','Ideological_Congruence')
Fig_Mat <- na.omit(Fig_Mat)

Fig_Mat$x<- c(0.8,0.9,1.0,1.4,1.5,1.6)
Fig_Mat$Coef <- as.character(Fig_Mat$Coef)
Fig_Mat$Coef <- as.numeric(Fig_Mat$Coef)
Fig_Mat$Upp_Conf <- as.character(Fig_Mat$Upp_Conf)
Fig_Mat$Upp_Conf <- as.numeric(Fig_Mat$Upp_Conf)
Fig_Mat$Low_Conf <- as.character(Fig_Mat$Low_Conf)
Fig_Mat$Low_Conf <- as.numeric(Fig_Mat$Low_Conf)
Fig_Mat$Type_News <- factor(Fig_Mat$Type_News,levels=c('Bottom 50%\nNews Quality',
                                                       'Top 50%\nNews Quality'))

Fig_Mat_2 <- Fig_Mat

#Create plot:
ED_fig_3b <- ggplot(data = Fig_Mat_2, aes(x = x, y = Coef)) +
  geom_hline(aes(yintercept = 0), color = "gray",
             linetype = 2, size = 0.75) +
  geom_point(aes(color = Measure, shape=Measure),size=1) +
  geom_linerange(aes(min = Low_Conf, 
                     max = Upp_Conf, 
                     color = Measure),
                 size=0.75) +
  scale_color_manual(values=cbbPalette_1, name = "Measure") +
  labs(color  = "Measure", shape  = "Measure") +
  ylab("\nEffect of Encouragement to Search for Information on \n Likelihood of Rating Misinformation True") +
  theme_classic() +
  theme(axis.title.x = element_text(size=8),
        axis.text.x  = element_text(size=6),
        axis.title.y = element_text(size=8),
        axis.text.y  = element_text(size=6),
        plot.title = element_text(size = 6),
        legend.title = element_text(size=8),
        legend.text = element_text(size=6)) +
  ylim(-0.8,0.8) +
  scale_x_continuous("News Quality Returned by Search Engines \n",breaks=c(0.9,1.5),labels=c('Bottom 50%\nNews Quality',
                                                                                             'Top 50%\nNews Quality'),limits=c(0.5,2.0)) +
  coord_flip()

ED_fig_3a <- ggplot(data = Fig_Mat_1, aes(x = x, y = Coef)) +
  geom_hline(aes(yintercept = 0), color = "gray",
             linetype = 2, size = 0.75) +
  geom_point(aes(color = Measure, shape=Measure),size=1) +
  geom_linerange(aes(min = Low_Conf, 
                     max = Upp_Conf, 
                     color = Measure),
                 size=0.75) +
  scale_color_manual(values=cbbPalette_1, name = "Measure") +
  labs(color  = "Measure", shape  = "Measure") +
  ylab("\nEffect of Encouragement to Search for Information on \n Likelihood of Rating Misinformation True") +
  theme_classic() +
  theme(axis.title.x = element_text(size=8),
        axis.text.x  = element_text(size=6),
        axis.title.y = element_text(size=8),
        axis.text.y  = element_text(size=6),
        plot.title = element_text(size = 6),
        legend.title = element_text(size=8),
        legend.text = element_text(size=6)) +
  ylim(-0.8,0.8) +
  scale_x_continuous("News Quality Returned by Search Engines \n",breaks=c(0.9,1.5),labels=c('Bottom 50%\nNews Quality',
                                                                                             'Top 50%\nNews Quality'),limits=c(0.5,2.0)) +
  coord_flip()


ggarrange(ED_fig_3a, ED_fig_3b,
          labels="auto",
          ncol = 2,
          font.label=list(color="black",size=8))

ggsave('./Figures/ED_fig_3.eps',height=8,width=18,units='cm',device="eps")





Fig_Mat$x <- NULL

write.csv(Fig_Mat,'./source_data/extended_data_3b.csv')


########################################################################

########################## Extended Data 4 #############################

########################################################################

########################################################################

#################### Extended Data 4b ##################################

########################################################################

#Pull in Fact-Checker Ideological Perspective
#Pull in this data: Search Experiment 1: Study 1:
Study_5_df <- read.csv('./Data/Study_5_df_FM.csv')
FCer_details <- read.csv('./Data/FCer_details.csv')
Study_5_df$Article_day <- as.character(Study_5_df$Article_day)
Study_5_df$ResponseId <- as.character(Study_5_df$ResponseId)
Study_5_df <- merge(Study_5_df,FCer_details,by='Article_day')

#Remove NA values:
Study_5_df = na.omit(Study_5_df)

#Number of unique respondents in study who evaluated FM articles:
length(unique(Study_5_df$ResponseId))

#Number of unique respondents in study who evaluated FM articles for control and treatment group:
treat_5_study <- Study_5_df %>% filter(Treatment == 1)
control_5_study <- Study_5_df %>% filter(Treatment == 0)
#Treatment group:
length(unique(treat_5_study$ResponseId))
#Control group:
length(unique(control_5_study$ResponseId))




#Run OLS Model with clustered standard errors:
lin_results_fit_5_1 = feols(True_Dummy ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Study_5_df)
#Produce confidence intervals with clustered standard errors:
CI_5_1 <- confint(lin_results_fit_5_1)

#Run OLS Model with clustered standard errors:
lin_results_fit_5_2 = feols(Four_Ordinal~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Study_5_df)
#Produce confidence intervals with clustered standard errors:
CI_5_2 <- confint(lin_results_fit_5_2)

#Run OLS Model with clustered standard errors:
lin_results_fit_5_3 = feols(Seven_Ordinal ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Study_5_df)
#Produce confidence intervals with clustered standard errors:
CI_5_3 <- confint(lin_results_fit_5_3)

#Create vector with coefficients:
Coefficients <- c(lin_results_fit_5_1$coefficients[1]/sd(Study_5_df$True_Dummy),
                  lin_results_fit_5_2$coefficients[1]/sd(Study_5_df$Four_Ordinal),
                  lin_results_fit_5_3$coefficients[1]/sd(Study_5_df$Seven_Ordinal))

#Create upper confidence intervals:
CI_Upper <- c(CI_5_1[1,2]/sd(Study_5_df$True_Dummy),
              CI_5_2[1,2]/sd(Study_5_df$Four_Ordinal),
              CI_5_3[1,2]/sd(Study_5_df$Seven_Ordinal))            

#Create lower confidence intervals:
CI_Lower <- c(CI_5_1[1,1]/sd(Study_5_df$True_Dummy),
              CI_5_2[1,1]/sd(Study_5_df$Four_Ordinal),
              CI_5_3[1,1]/sd(Study_5_df$Seven_Ordinal))

Coef_names <- c('Rate as True',
                'Ordinal Scale (4)',
                'Ordinal Scale (7)')
P_Value <- rev(c(lin_results_fit_5_1$coeftable[1,4],
                 lin_results_fit_5_2$coeftable[1,4],
                 lin_results_fit_5_3$coeftable[1,4]))

Standard_Error <- rev(c(lin_results_fit_5_1$coeftable[1,2],
                        lin_results_fit_5_2$coeftable[1,2],
                        lin_results_fit_5_3$coeftable[1,2]))



#Put together matrix with data for plot:
d_matrix <- cbind(Coef_names,Coefficients,CI_Upper,CI_Lower,P_Value,Standard_Error)
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
d_matrix_ed_4 <- d_matrix

ED_fig_4b <- ggplot(data = d_matrix_ed_4, aes(x = x, y = Coefficients)) +
  geom_hline(aes(yintercept = 0), color = "gray",
             linetype = 2, size = 1) +
  geom_point(size=2) +
  geom_linerange(aes(min = CI_Lower,
                     max = CI_Upper),
                 size=1) +
  ylab("\nEffect of Searching for Information on         \nPerceived Veracity of Misinformation        \n(1 unit is 1 standard deviation)        ") +
  theme_classic() +
  theme(axis.title.x = element_text(size=8),
        axis.text.x  = element_text(size=6),
        axis.title.y = element_text(size=8),
        axis.text.y  = element_text(size=6),
        plot.title = element_text(size = 8),
        legend.title = element_text(size=6),
        legend.text = element_text(size=8),
        title =element_text(size=18, face='bold')) +
  ylim(-0.13,0.7) +
  scale_x_continuous("Perceived Veracity Scale \n",breaks=c(0.3,0.2,0.1),labels=Coef_names,limits=c(0.0,0.4)) +
  coord_flip()


##################################################################################

########################################## Extended Data 4a ######################

##################################################################################

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
ED_fig_4a <- ggplot(Matrix_Dist, aes(fill=Percentage, y=Proportion, x=Article_Rating)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_manual(values=cbbPalette_1, name = "Number of News Links \nReturned by Search\nEngines From\nUnreliable Sources") +
  ylab('Proportion of Individuals Whose       \n Search Engine Results Return      \n Unreliable News by Article Type         \n') +
  xlab('\nFact-Checker Rating of Article Individual\n Queries Search Engines About') +
  theme_bw() + 
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(size=8),
        axis.text.x  = element_text(size=6),
        axis.title.y = element_text(size=8),
        axis.text.y  = element_text(size=6),
        title =element_text(size=6, face='bold'),
        legend.text = element_text(size=6)) + guides(fill=guide_legend(
          keywidth=0.2,
          keyheight=0.2,
          default.unit="inch")) +
  geom_text(aes(label=Proportion), position=position_dodge(width=0.9), vjust=-0.25,size=2) +
  ylim(0,1)



colnames(Matrix_Dist)[2] <- 'Number_low_quality_links'

write.csv(Matrix_Dist,'./source_data/extended_data_4a.csv')


######################################################################################################

########################################## Extended Data 4c ##########################################

######################################################################################################

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
Fig_2c_Mat <- matrix(ncol=7)

#Merge data:
New_matr <- matrix(c(round(fit_2_1$coefficients[1]/sd(Study_5_subset_1$Seven_Ordinal),4),round(CI_2_1[1,1]/sd(Study_5_subset_1$Seven_Ordinal),4),round(CI_2_1[1,2]/sd(Study_5_subset_1$Seven_Ordinal),4),'Ordinal (7)','Only Very Reliable News',fit_2_1$coeftable[1,4],fit_2_1$coeftable[1,2],
                     round(fit_2_2$coefficients[1]/sd(Study_5_subset_1$Four_Ordinal),4),round(CI_2_2[1,1]/sd(Study_5_subset_1$Four_Ordinal),4),round(CI_2_2[1,2]/sd(Study_5_subset_1$Four_Ordinal),4),'Ordinal (4)','Only Very Reliable News',fit_2_2$coeftable[1,4],fit_2_2$coeftable[1,2],
                     round(fit_2_3$coefficients[1]/sd(Study_5_subset_1$True_Dummy),4),round(CI_2_3[1,1]/sd(Study_5_subset_1$True_Dummy),4),round(CI_2_3[1,2]/sd(Study_5_subset_1$True_Dummy),4),'True (Dummy)','Only Very Reliable News',fit_2_3$coeftable[1,4],fit_2_3$coeftable[1,2]),ncol=7,byrow=T)

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
New_matr <- matrix(c(round(fit_2_1$coefficients[1]/sd(Study_5_subset_2$Seven_Ordinal),4),round(CI_2_1[1,1]/sd(Study_5_subset_2$Seven_Ordinal),4),round(CI_2_1[1,2]/sd(Study_5_subset_2$Seven_Ordinal),4),'Ordinal (7)','Some Unreliable News',fit_2_1$coeftable[1,4],fit_2_1$coeftable[1,2],
                     round(fit_2_2$coefficients[1]/sd(Study_5_subset_2$Four_Ordinal),4),round(CI_2_2[1,1]/sd(Study_5_subset_2$Four_Ordinal),4),round(CI_2_2[1,2]/sd(Study_5_subset_2$Four_Ordinal),4),'Ordinal (4)','Some Unreliable News',fit_2_2$coeftable[1,4],fit_2_2$coeftable[1,2],
                     round(fit_2_3$coefficients[1]/sd(Study_5_subset_2$True_Dummy),4),round(CI_2_3[1,1]/sd(Study_5_subset_2$True_Dummy),4),round(CI_2_3[1,2]/sd(Study_5_subset_2$True_Dummy),4),'True (Dummy)','Some Unreliable News',fit_2_3$coeftable[1,4],fit_2_3$coeftable[1,2]),ncol=7,byrow=T)

Fig_2c_Mat <- rbind(Fig_2c_Mat,New_matr)


#Transform matrix into dataframe:
Fig_2c_Mat <- as.data.frame(Fig_2c_Mat)

#Name matrix:
colnames(Fig_2c_Mat) <- c('Coef','Low_Conf','Upp_Conf','Measure','Type_News','P_Value','Standard_Error')

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
Fig_2c <- Fig_2c_Mat

ED_fig_4c <- ggplot(data = Fig_2c, aes(x = x, y = Coef)) +
  geom_hline(aes(yintercept = 0), color = "gray",
             linetype = 2, size = 0.75) +
  geom_point(aes(color = Measure, shape=Measure),size=1.5) +
  geom_linerange(aes(min = Low_Conf, 
                     max = Upp_Conf, 
                     color = Measure),
                 size=0.75) +
  scale_color_manual(values=cbbPalette_1, name = "Measure") +
  ylab("\n Effect of Searching Online on Belief in Misinformation\n Dependent on Quality of Search Results \n(1 unit is 1 standard deviation of that measure) ") +
  theme_classic() +
  theme(axis.title.x = element_text(size=8),
        axis.text.x  = element_text(size=6),
        axis.title.y = element_text(size=8),
        axis.text.y  = element_text(size=6),
        plot.title = element_text(size = 8),
        legend.title = element_text(size=8),
        legend.text = element_text(size=8),
        title =element_text(size=8, face='bold')) +
  ylim(-0.55,0.55) +
  scale_x_continuous("Type of News Returned by \n Search Engine \n",breaks=c(1.5,0.9),labels=c('At Least 10%\nof News URLs\nAre Unreliable',
                                                                                               'Only Very Reliable\nNews Sources'),limits=c(0.5,2.0)) +
  coord_flip()

Fig_2c_Mat$x <- NULL

write.csv(Fig_2c_Mat,'./source_data/extended_data_4c_source_data.csv')


######################################################################################################

########################################## Extended Data 4d ##########################################

######################################################################################################

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
Fig_2d_Mat <- matrix(ncol=7)

Fig_Poster <- matrix(ncol=4)

#Merge data:
New_matr <- matrix(c(round(fit_2_1$coefficients[1]/sd(lowest_quartile_all_data$Seven_Ordinal),4),round(CI_2_1[1,1]/sd(lowest_quartile_all_data$Seven_Ordinal),4),round(CI_2_1[1,2]/sd(lowest_quartile_all_data$Seven_Ordinal),4),'Ordinal (7)','0-25%',fit_2_1$coeftable[1,4],fit_2_1$coeftable[1,2],
                     round(fit_2_2$coefficients[1]/sd(lowest_quartile_all_data$Four_Ordinal),4),round(CI_2_2[1,1]/sd(lowest_quartile_all_data$Four_Ordinal),4),round(CI_2_2[1,2]/sd(lowest_quartile_all_data$Four_Ordinal),4),'Ordinal (4)','0-25%',fit_2_2$coeftable[1,4],fit_2_2$coeftable[1,2],
                     round(fit_2_3$coefficients[1]/sd(lowest_quartile_all_data$True_Dummy),4),round(CI_2_3[1,1]/sd(lowest_quartile_all_data$True_Dummy),4),round(CI_2_3[1,2]/sd(lowest_quartile_all_data$True_Dummy),4),'True (Dummy)','0-25%',fit_2_3$coeftable[1,4],fit_2_3$coeftable[1,2]),ncol=7,byrow=T)

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
New_matr <- matrix(c(round(fit_2_1$coefficients[1]/sd(Second_lowest_quartile_all_data$Seven_Ordinal),4),round(CI_2_1[1,1]/sd(Second_lowest_quartile_all_data$Seven_Ordinal),4),round(CI_2_1[1,2]/sd(Second_lowest_quartile_all_data$Seven_Ordinal),4),'Ordinal (7)','25-50%',fit_2_1$coeftable[1,4],fit_2_1$coeftable[1,2],
                     round(fit_2_2$coefficients[1]/sd(Second_lowest_quartile_all_data$Four_Ordinal),4),round(CI_2_2[1,1]/sd(Second_lowest_quartile_all_data$Four_Ordinal),4),round(CI_2_2[1,2]/sd(Second_lowest_quartile_all_data$Four_Ordinal),4),'Ordinal (4)','25-50%',fit_2_2$coeftable[1,4],fit_2_2$coeftable[1,2],
                     round(fit_2_3$coefficients[1]/sd(Second_lowest_quartile_all_data$True_Dummy),4),round(CI_2_3[1,1]/sd(Second_lowest_quartile_all_data$True_Dummy),4),round(CI_2_3[1,2]/sd(Second_lowest_quartile_all_data$True_Dummy),4),'True (Dummy)','25-50%',fit_2_3$coeftable[1,4],fit_2_3$coeftable[1,2]),ncol=7,byrow=T)

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
New_matr <- matrix(c(round(fit_2_1$coefficients[1]/sd(Third_lowest_quartile_all_data$Seven_Ordinal),4),round(CI_2_1[1,1]/sd(Third_lowest_quartile_all_data$Seven_Ordinal),4),round(CI_2_1[1,2]/sd(Third_lowest_quartile_all_data$Seven_Ordinal),4),'Ordinal (7)','50-75%',fit_2_1$coeftable[1,4],fit_2_1$coeftable[1,2],
                     round(fit_2_2$coefficients[1]/sd(Third_lowest_quartile_all_data$Four_Ordinal),4),round(CI_2_2[1,1]/sd(Third_lowest_quartile_all_data$Four_Ordinal),4),round(CI_2_2[1,2]/sd(Third_lowest_quartile_all_data$Four_Ordinal),4),'Ordinal (4)','50-75%',fit_2_2$coeftable[1,4],fit_2_2$coeftable[1,2],
                     round(fit_2_3$coefficients[1]/sd(Third_lowest_quartile_all_data$True_Dummy),4),round(CI_2_3[1,1]/sd(Third_lowest_quartile_all_data$True_Dummy),4),round(CI_2_3[1,2]/sd(Third_lowest_quartile_all_data$True_Dummy),4),'True (Dummy)','50-75%',fit_2_3$coeftable[1,4],fit_2_3$coeftable[1,2]),ncol=7,byrow=T)

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
New_matr <- matrix(c(round(fit_2_1$coefficients[1]/sd(highest_quartile_all_data$Seven_Ordinal),4),round(CI_2_1[1,1]/sd(highest_quartile_all_data$Seven_Ordinal),4),round(CI_2_1[1,2]/sd(highest_quartile_all_data$Seven_Ordinal),4),'Ordinal (7)','75-100%',fit_2_1$coeftable[1,4],fit_2_1$coeftable[1,2],
                     round(fit_2_2$coefficients[1]/sd(highest_quartile_all_data$Four_Ordinal),4),round(CI_2_2[1,1]/sd(highest_quartile_all_data$Four_Ordinal),4),round(CI_2_2[1,2]/sd(highest_quartile_all_data$Four_Ordinal),4),'Ordinal (4)','75-100%',fit_2_1$coeftable[1,4],fit_2_1$coeftable[1,2],
                     round(fit_2_3$coefficients[1]/sd(highest_quartile_all_data$True_Dummy),4),round(CI_2_3[1,1]/sd(highest_quartile_all_data$True_Dummy),4),round(CI_2_3[1,2]/sd(highest_quartile_all_data$True_Dummy),4),'True (Dummy)','75-100%',fit_2_1$coeftable[1,4],fit_2_1$coeftable[1,2]),ncol=7,byrow=T)

Fig_2d_Mat <- rbind(Fig_2d_Mat,New_matr)

poster_matr <- matrix(c(round(fit_2_3$coefficients[1],4),round(CI_2_3[1,1],4),round(CI_2_3[1,2],4),'75-100%'),ncol=4,byrow=T)

Fig_Poster <- rbind(Fig_Poster,poster_matr)




#Create dataframe to produce plot
Fig_2d_Mat <- as.data.frame(Fig_2d_Mat)
Fig_2d_Mat <- na.omit(Fig_2d_Mat)
colnames(Fig_2d_Mat) <- c('Coef','Low_Conf','Upp_Conf','Measure','Type_News','P_Value','Standard_Error')
Fig_2d_Mat <- na.omit(Fig_2d_Mat)
Fig_2d_Mat$x<-c(0.8,0.9,1.0,1.4,1.5,1.6,2.0,2.1,2.2,2.6,2.7,2.8)
Fig_2d_Mat$Coef <- as.character(Fig_2d_Mat$Coef)
Fig_2d_Mat$Coef <- as.numeric(Fig_2d_Mat$Coef)
Fig_2d_Mat$Upp_Conf <- as.character(Fig_2d_Mat$Upp_Conf)
Fig_2d_Mat$Upp_Conf <- as.numeric(Fig_2d_Mat$Upp_Conf)
Fig_2d_Mat$Low_Conf <- as.character(Fig_2d_Mat$Low_Conf)
Fig_2d_Mat$Low_Conf <- as.numeric(Fig_2d_Mat$Low_Conf)


#Produce Plot:
Fig_2d <- Fig_2d_Mat


ED_fig_4d <- ggplot(data = Fig_2d, aes(x = x, y = Coef)) +
  geom_hline(aes(yintercept = 0), color = "gray",
             linetype = 2, size = 0.75) +
  geom_point(aes(color = Measure, shape=Measure),size=1.5) +
  geom_linerange(aes(min = Low_Conf, 
                     max = Upp_Conf, 
                     color = Measure),
                 size=0.75) +
  scale_color_manual(values=cbbPalette_1, name = "Measure") +
  ylab("\n Effect of Searching Online on Belief in Misinformation\n Dependent on Quality of Search Results \n(1 unit is 1 standard deviation of that measure) ") +
  theme_classic() +
  theme(axis.title.x = element_text(size=8),
        axis.text.x  = element_text(size=6),
        axis.title.y = element_text(size=8),
        axis.text.y  = element_text(size=6),
        plot.title = element_text(size = 8),
        legend.title = element_text(size=8),
        legend.text = element_text(size=8),
        title =element_text(size=8, face='bold')) +
  ylim(-0.5,0.7) +
  scale_x_continuous("Quartile of News Quality \n Returned by Search Engine \n",breaks=c(2.7,2.1,1.5,0.9),labels=c('75-100%',
                                                                                                                   '50-75%',
                                                                                                                   '25-50%',
                                                                                                                   '0-25%'),limits=c(0.5,3.0)) +
  coord_flip()




Fig_2d_Mat$x <- NULL
write.csv(Fig_2d_Mat,'./source_data/extended_data_4d_source_data.csv')


ggarrange(ED_fig_4a, ED_fig_4b, ED_fig_4c, ED_fig_4d,
          labels="auto",
          ncol = 2,
          nrow = 2,
          font.label=list(color="black",size=8))

ggsave('./Figures/ED_fig_4.eps',height=16,width=18,units='cm',device="eps")




##############################################################################

################################# Extended Data 5 ############################

##############################################################################



###############################################################################

################################# Extended Data 5a ############################

###############################################################################

#Merge treatment data from study 5 and the search results data:
T_Data <- Study_5_treat_data %>% filter(FC_Eval == 'FM')

nrow(T_Data)

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

P_Value <- c(round(Prop_Dummy_results$coeftable[1,4],4),
             round(Prop_Dummy_results$coeftable[2,4],4),
             round(Prop_Dummy_results$coeftable[3,4],4),
             round(Prop_Dummy_results$coeftable[4,4],4),
             round(Prop_Dummy_results$coeftable[5,4],4),
             round(Prop_Dummy_results$coeftable[6,4],4))


Standard_Error <- c(round(Prop_Dummy_results$coeftable[1,2],4),
                    round(Prop_Dummy_results$coeftable[2,2],4),
                    round(Prop_Dummy_results$coeftable[3,2],4),
                    round(Prop_Dummy_results$coeftable[4,2],4),
                    round(Prop_Dummy_results$coeftable[5,2],4),
                    round(Prop_Dummy_results$coeftable[6,2],4))

#Create vector with variable names:
Coef_names <- c('Age',
                'Gender',
                'Education',
                'Income',
                'Ideological\nCongruence',
                'Digital Literacy')




#Create data matrix with data for figure:
d_matrix <- cbind(Coef_names,Coefficients,CI_Upper,CI_Lower,P_Value,Standard_Error)
rownames(d_matrix) <- c()
d_matrix <- data.frame(d_matrix)
d_matrix$Coefficients <- as.character(d_matrix$Coefficients)
d_matrix$CI_Lower <- as.character(d_matrix$CI_Lower)
d_matrix$CI_Upper <- as.character(d_matrix$CI_Upper)
d_matrix$Coefficients <- as.numeric(d_matrix$Coefficients)
d_matrix$CI_Lower <- as.numeric(d_matrix$CI_Lower)
d_matrix$CI_Upper <- as.numeric(d_matrix$CI_Upper)
d_matrix$x<-c(0.1,0.2,0.3,0.4,0.5,0.6)

d_matrix_3a <- d_matrix

#Produce plot:
ED_fig_5a <- ggplot(data = d_matrix_3a, aes(x = x, y = Coefficients)) +
  geom_hline(aes(yintercept = 0), color = "gray",
             linetype = 2, size = 0.5) +
  geom_point(size=1) +
  geom_linerange(aes(min = CI_Lower,
                     max = CI_Upper),
                 size=0.5) +
  ylab("\n The Effect of a 1 SD Increase of \nIndep. Var on Probability of Exposure\n to Unreliable News") +
  theme_classic() +
  theme(axis.title.x = element_text(size=6),
        axis.text.x  = element_text(size=6),
        axis.title.y = element_text(size=6),
        axis.text.y  = element_text(size=6),
        plot.title = element_text(size = 6),
        legend.title = element_text(size=6),
        legend.text = element_text(size=6),
        title =element_text(size=6, face='bold')) +
  ylim(-0.1,0.15) +
  scale_x_continuous("Demographic Covariates \n",breaks=c(0.1,0.2,0.3,0.4,0.5,0.6),labels=Coef_names,limits=c(0.0,0.7)) +
  coord_flip()

d_matrix$x <- NULL

write.csv(d_matrix,'./source_data/extended_data_5a_source_data.csv')

###############################################################################

################################# Extended Data 5b ############################

###############################################################################

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

Matrix_Dist_5b <- Matrix_Dist

#Produce plot:
ED_fig_5b <- ggplot(Matrix_Dist_5b, aes(fill=Percentage, y=Proportion, x=Article_Rating)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_manual(values=cbbPalette_1, name = "Number of \nNews Links \nReturned by \nSearch Engines \nFrom Unreliable \nSources") +
  ylab('Proportion of Search Queries That Return Results \n That Return Unreliable News by Article Type     \n') +
  xlab('\nType of Search Query Used') +
  theme_classic() + 
  theme(axis.title.x = element_text(size=6),
        axis.text.x  = element_text(size=5),
        axis.title.y = element_text(size=6),
        axis.text.y  = element_text(size=5),
        plot.title = element_text(size = 6),
        legend.title = element_text(size=5),
        legend.text = element_text(size=5),
        title =element_text(size=6, face='bold')) +
  guides(fill=guide_legend(
    keywidth=0.1,
    keyheight=0.1,
    default.unit="inch")) +
  geom_text(aes(label=Proportion), position=position_dodge(width=0.9), vjust=-0.25,size=2) +
  ylim(0,1)



write.csv(Matrix_Dist,'./source_data/extended_data_5b_source_data.csv')


#Comparative Statistic:

#Run simple linear regression:
fit_lm_3b <- lm(Unrel_contain ~ Headline_Link,data=Headline_coding)

#Print output:
summary(fit_lm_3b)



###############################################################################

################################# Extended Data 5c ############################

###############################################################################


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


P_Value <- c(round(Headline_results$coeftable[1,4],4),
             round(Headline_results$coeftable[2,4],4),
             round(Headline_results$coeftable[3,4],4),
             round(Headline_results$coeftable[4,4],4),
             round(Headline_results$coeftable[5,4],4),
             round(Headline_results$coeftable[6,4],4))


Standard_Error <- c(round(Headline_results$coeftable[1,2],4),
                    round(Headline_results$coeftable[2,2],4),
                    round(Headline_results$coeftable[3,2],4),
                    round(Headline_results$coeftable[4,2],4),
                    round(Headline_results$coeftable[5,2],4),
                    round(Headline_results$coeftable[6,2],4))

#Create vector with variable names:
Coef_names <- c('Age',
                'Gender',
                'Education',
                'Income',
                'Ideological\nCongruence',
                'Digital Literacy')


#Create data matrix with data for figure:
d_matrix <- cbind(Coef_names,Coefficients,CI_Upper,CI_Lower,P_Value,Standard_Error)
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
d_matrix_5c <- d_matrix

ED_fig_5c <- ggplot(data = d_matrix_5c, aes(x = x, y = Coefficients)) +
  geom_hline(aes(yintercept = 0), color = "gray",
             linetype = 2, size = 0.5) +
  geom_point(size=1) +
  geom_linerange(aes(min = CI_Lower,
                     max = CI_Upper),
                 size=0.5) +
  ylab("\n The Effect of a 1 SD Increase of \nIndep. Var on Probability of \nUsing Headline/Link as Search Term ") +
  theme_classic() +
  theme(axis.title.x = element_text(size=6),
        axis.text.x  = element_text(size=6),
        axis.title.y = element_text(size=6),
        axis.text.y  = element_text(size=6),
        plot.title = element_text(size = 6),
        legend.title = element_text(size=6),
        legend.text = element_text(size=6),
        title =element_text(size=6, face='bold')) +
  ylim(-0.1,0.1) +
  scale_x_continuous("Demographic Covariates \n",breaks=c(0.1,0.2,0.3,0.4,0.5,0.6),labels=Coef_names,limits=c(0.0,0.7)) +
  coord_flip()




d_matrix$x <- NULL
write.csv(d_matrix,'./source_data/extended_data_5c_source_data.csv')




ggarrange(ED_fig_5a, ED_fig_5b, ED_fig_5c,
          labels="auto",
          ncol = 3,
          font.label=list(color="black",size=8))

ggsave('./Figures/ED_fig_5.eps',height=8,width=18,units='cm',device="eps")



#################################################################################

########################### Extended Data 6 #####################################

#################################################################################

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
           'False/\nMisleading',
           'True',
           'False/\nMisleading',
           'True',  
           'False/\nMisleading',
           'True',  
           'False/\nMisleading',
           'True',  
           'False/\nMisleading')

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


P_Value <- c(round(lin_results_fit_1_1_2$coeftable[1,4],4),
             round(lin_results_fit_1_3_2$coeftable[1,4],4),
             round(lin_results_fit_2_1_2$coeftable[1,4],4),
             round(lin_results_fit_2_3_2$coeftable[1,4],4),
             round(lin_results_fit_3_1_2$coeftable[1,4],4),
             round(lin_results_fit_3_3_2$coeftable[1,4],4),
             round(lin_results_fit_4_1_2$coeftable[1,4],4),
             round(lin_results_fit_4_3_2$coeftable[1,4],4),
             round(lin_results_fit_5_1_2$coeftable[1,4],4),
             round(lin_results_fit_5_3_2$coeftable[1,4],4))

Standard_Error <- c(round(lin_results_fit_1_1_2$coeftable[1,2],4),
                    round(lin_results_fit_1_3_2$coeftable[1,2],4),
                    round(lin_results_fit_2_1_2$coeftable[1,2],4),
                    round(lin_results_fit_2_3_2$coeftable[1,2],4),
                    round(lin_results_fit_3_1_2$coeftable[1,2],4),
                    round(lin_results_fit_3_3_2$coeftable[1,2],4),
                    round(lin_results_fit_4_1_2$coeftable[1,2],4),
                    round(lin_results_fit_4_3_2$coeftable[1,2],4),
                    round(lin_results_fit_5_1_2$coeftable[1,2],4),
                    round(lin_results_fit_5_3_2$coeftable[1,2],4))

#Create data matrix with data for figure:
d_matrix <- cbind(Study,Coef_names,Coefficients,CI_Upper,CI_Lower,P_Value,Standard_Error)

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

d_matrix_6a <- d_matrix

d_matrix$x <- NULL
write.csv(d_matrix,'./source_data/extended_data_6a_source_data.csv')



################################################################################################

################################# Extended Data 6b #############################################

################################################################################################

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
Study <- c('True\nLow Quality',
           'True\nMainstream',  
           'False/\nMisleading',
           'True\nLow Quality',
           'True\nMainstream',  
           'False/\nMisleading',
           'True\nLow Quality',
           'True\nMainstream',  
           'False/\nMisleading',
           'True\nLow Quality',
           'True\nMainstream',  
           'False/\nMisleading',
           'True\nLow Quality',
           'True\nMainstream',  
           'False/\nMisleading')

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

P_Value <- c(round(lin_results_fit_1_1_2$coeftable[1,4],4),
             round(lin_results_fit_1_2_2$coeftable[1,4],4),
             round(lin_results_fit_1_3_2$coeftable[1,4],4),
             round(lin_results_fit_2_1_2$coeftable[1,4],4),
             round(lin_results_fit_2_2_2$coeftable[1,4],4),
             round(lin_results_fit_2_3_2$coeftable[1,4],4),
             round(lin_results_fit_3_1_2$coeftable[1,4],4),
             round(lin_results_fit_3_2_2$coeftable[1,4],4),
             round(lin_results_fit_3_3_2$coeftable[1,4],4),
             round(lin_results_fit_4_1_2$coeftable[1,4],4),
             round(lin_results_fit_4_2_2$coeftable[1,4],4),
             round(lin_results_fit_4_3_2$coeftable[1,4],4),
             round(lin_results_fit_5_1_2$coeftable[1,4],4),
             round(lin_results_fit_5_2_2$coeftable[1,4],4),
             round(lin_results_fit_5_3_2$coeftable[1,4],4))

Standard_Error <- c(round(lin_results_fit_1_1_2$coeftable[1,2],4),
                    round(lin_results_fit_1_2_2$coeftable[1,2],4),
                    round(lin_results_fit_1_3_2$coeftable[1,2],4),
                    round(lin_results_fit_2_1_2$coeftable[1,2],4),
                    round(lin_results_fit_2_2_2$coeftable[1,2],4),
                    round(lin_results_fit_2_3_2$coeftable[1,2],4),
                    round(lin_results_fit_3_1_2$coeftable[1,2],4),
                    round(lin_results_fit_3_2_2$coeftable[1,2],4),
                    round(lin_results_fit_3_3_2$coeftable[1,2],4),
                    round(lin_results_fit_4_1_2$coeftable[1,2],4),
                    round(lin_results_fit_4_2_2$coeftable[1,2],4),
                    round(lin_results_fit_4_3_2$coeftable[1,2],4),
                    round(lin_results_fit_5_1_2$coeftable[1,2],4),
                    round(lin_results_fit_5_2_2$coeftable[1,2],4),
                    round(lin_results_fit_5_3_2$coeftable[1,2],4))

#Create data matrix with data for figure:
d_matrix <- cbind(Study,Coef_names,Coefficients,CI_Upper,CI_Lower,P_Value,Standard_Error)
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
d_matrix$x <- c(5.2,5.0,4.8,4.2,4.0,3.8,3.2,3.0,2.8,2.2,2.0,1.8,1.2,1.0,0.8)

d_matrix_6b <- d_matrix

d_matrix$x <- NULL
write.csv(d_matrix,'./source_data/extended_data_6b_source_data.csv')


##########################################################################################

################################# Extended Data 6c #######################################

##########################################################################################

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
Study <- c('True\nLow Quality',
           'True\nMainstream',
           'False/\nMisleading',
           'True\nLow Quality',
           'True\nMainstream',
           'False/\nMisleading')

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

P_Value <- c(round(btwn_T_LQ_results_2$coeftable[1,4],4),
             round(btwn_T_M_results_2$coeftable[1,4],4),
             round(btwn_FM_results_2$coeftable[1,4],4),
             round(wthn_T_LQ_results_2$coeftable[1,4],4),
             round(wthn_T_M_results_2$coeftable[1,4],4),
             round(wthn_FM_results_2$coeftable[1,4],4))

Standard_Error <- c(round(btwn_T_LQ_results_2$coeftable[1,2],4),
                    round(btwn_T_M_results_2$coeftable[1,2],4),
                    round(btwn_FM_results_2$coeftable[1,2],4),
                    round(wthn_T_LQ_results_2$coeftable[1,2],4),
                    round(wthn_T_M_results_2$coeftable[1,2],4),
                    round(wthn_FM_results_2$coeftable[1,2],4))

#Create data matrix with data for figure:
d_matrix <- cbind(Study,Coef_names,Coefficients,CI_Upper,CI_Lower,P_Value,Standard_Error)

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


d_matrix_6c <- d_matrix


ED_fig_6a <- ggplot(data = d_matrix_6a, aes(x = x, y = Coefficients,color=)) +
  geom_hline(aes(yintercept = 0), color = "gray",
             linetype = 2, size = 0.75) +
  geom_point(aes(color = Study, shape=Study),size=1.5) +
  geom_linerange(aes(min = CI_Lower, 
                     max = CI_Upper, 
                     color = Study),
                 size=0.75) +
  scale_color_manual(values=cbbPalette_2, name = "Type of News") +
  scale_shape_manual(values=c(15,16,17),"Type of News") +
  theme_classic() +
  theme(axis.title.x = element_text(size=8),
        axis.text.x  = element_text(size=6),
        axis.title.y = element_text(size=8),
        axis.text.y  = element_text(size=6),
        plot.title = element_text(size = 6),
        legend.title = element_text(size=4),
        legend.text = element_text(size=4),
        title =element_text(size=6, face='bold')) +
  ylim(-0.1,0.7) +
  scale_y_continuous("\n The Effect of Searching Online\n on Perceived Veracity\n(7-point Ordinal Scale)",breaks=c(0,0.25,0.5),limits=c(-0.1,0.7)) +
  scale_x_continuous("Study Number\n",breaks=c(1,2,3,4,5),labels=rev(c('Study 1',
                                                                       'Study 2',
                                                                       'Study 3',
                                                                       'Study 4',
                                                                       'Study 5')),limits=c(0.2,5.8)) +
  coord_flip()



ED_fig_6b <- ggplot(data = d_matrix_6b, aes(x = x, y = Coefficients,color=Study)) +
  geom_hline(aes(yintercept = 0), color = "gray",
             linetype = 2, size = 0.75) +
  geom_point(aes(color = Study, shape=Study),size=1.5) +
  geom_linerange(aes(min = CI_Lower, 
                     max = CI_Upper, 
                     color = Study),
                 size=0.75) +
  scale_color_manual(values=cbbPalette_2, name = "Type of News") +
  scale_shape_manual(values=c(15,16,17),"Type of News") +
  ylab("\n The Effect of Searching Online\n on Perceived Veracity\n(7-point Ordinal Scale)") +
  theme_classic() +
  theme(axis.title.x = element_text(size=8),
        axis.text.x  = element_text(size=6),
        axis.title.y = element_text(size=8),
        axis.text.y  = element_text(size=6),
        plot.title = element_text(size = 6),
        legend.title = element_text(size=4),
        legend.text = element_text(size=4),
        title =element_text(size=6, face='bold')) +
  scale_y_continuous("\n The Effect of Searching Online\n on Perceived Veracity\n(7-point Ordinal Scale)",breaks=c(0,0.50,1),limits=c(-0.1,1.0)) +
  scale_x_continuous("Study Number\n",breaks=c(1,2,3,4,5),labels=rev(c('Study 1',
                                                                       'Study 2',
                                                                       'Study 3',
                                                                       'Study 4',
                                                                       'Study 5')),limits=c(0.2,5.8)) +
  coord_flip()


ED_fig_6c <- ggplot(data = d_matrix_6c, aes(x = x, y = Coefficients,color=Study)) +
  geom_hline(aes(yintercept = 0), color = "gray",
             linetype = 2, size = 0.75) +
  geom_point(aes(color = Study, shape=Study),size=1.5) +
  geom_linerange(aes(min = CI_Lower, 
                     max = CI_Upper, 
                     color = Study),
                 size=0.75) +
  scale_color_manual(values=cbbPalette_2, name = "Type of News") +
  scale_shape_manual(values=c(15,16,17),"Type of News") +
  theme_classic() +
  theme(axis.title.x = element_text(size=8),
        axis.text.x  = element_text(size=6),
        axis.title.y = element_text(size=8),
        axis.text.y  = element_text(size=6),
        plot.title = element_text(size = 6),
        legend.title = element_text(size=4),
        legend.text = element_text(size=4),
        title =element_text(size=6, face='bold')) +
  scale_y_continuous("\n The Effect of Searching Online\n on Perceived Veracity\n(7-point Ordinal Scale)",breaks=c(0,0.25,0.5),limits=c(-0.1,0.7)) +
  scale_x_continuous("Study Type\n",breaks=c(1,2),labels=rev(c('Between',
                                                               'Within')),limits=c(0,3)) +
  coord_flip()


ggarrange(ED_fig_6a, ED_fig_6b, ED_fig_6c,
          labels="auto",
          ncol = 3,
          font.label=list(color="black",size=8))




ggsave('./Figures/ED_fig_6.eps',height=8,width=18,units='cm',device="eps")



















################################################################################################

################################## Extended Data 7 #############################################

################################################################################################


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


lin_results_fit_1$nobs
lin_results_fit_2$nobs
lin_results_fit_3$nobs
lin_results_fit_4$nobs
lin_results_fit_5$nobs

#Create dataframe with coefficients and confidence intervals:
Coefficients <- c(round(lin_results_fit_1$coefficients[1],4),
                  round(lin_results_fit_2$coefficients[1],4),
                  round(lin_results_fit_3$coefficients[1],4),
                  round(lin_results_fit_4$coefficients[1],4),
                  round(lin_results_fit_5$coefficients[1],4))

lin_results_fit_5$coefficients[1]/sd(pooled$True_Dummy)

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

P_Value <- c(round(lin_results_fit_1$coeftable[1,4],4),
             round(lin_results_fit_2$coeftable[1,4],4),
             round(lin_results_fit_3$coeftable[1,4],4),
             round(lin_results_fit_4$coeftable[1,4],4),
             round(lin_results_fit_5$coeftable[1,4],4))

Standard_Error <- c(round(lin_results_fit_1$coeftable[1,2],4),
                    round(lin_results_fit_2$coeftable[1,2],4),
                    round(lin_results_fit_3$coeftable[1,2],4),
                    round(lin_results_fit_4$coeftable[1,2],4),
                    round(lin_results_fit_5$coeftable[1,2],4))

#Create data matrix with data for figure:
d_matrix <- cbind(Coef_names,Coefficients,CI_Upper,CI_Lower,P_Value,Standard_Error)
rownames(d_matrix) <- c()
d_matrix <- data.frame(d_matrix)
d_matrix$Coefficients <- as.character(d_matrix$Coefficients)
d_matrix$CI_Lower <- as.character(d_matrix$CI_Lower)
d_matrix$CI_Upper <- as.character(d_matrix$CI_Upper)
d_matrix$Coefficients <- as.numeric(d_matrix$Coefficients)
d_matrix$CI_Lower <- as.numeric(d_matrix$CI_Lower)
d_matrix$CI_Upper <- as.numeric(d_matrix$CI_Upper)
d_matrix$x<-c(0.1,0.2,0.3,0.4,0.5)


d_matrix_7a <- d_matrix

write.csv(d_matrix,'./source_data/extended_data_7a_source_data.csv')


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

lin_results_fit_5$coefficients[1]/sd(pooled$Likert_Evaluation)

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

P_Value <- c(round(lin_results_fit_1$coeftable[1,4],4),
             round(lin_results_fit_2$coeftable[1,4],4),
             round(lin_results_fit_3$coeftable[1,4],4),
             round(lin_results_fit_4$coeftable[1,4],4),
             round(lin_results_fit_5$coeftable[1,4],4))

Standard_Error <- c(round(lin_results_fit_1$coeftable[1,2],4),
                    round(lin_results_fit_2$coeftable[1,2],4),
                    round(lin_results_fit_3$coeftable[1,2],4),
                    round(lin_results_fit_4$coeftable[1,2],4),
                    round(lin_results_fit_5$coeftable[1,2],4))

#Create data matrix with data for figure:
d_matrix <- cbind(Coef_names,Coefficients,CI_Upper,CI_Lower,P_Value,Standard_Error)
rownames(d_matrix) <- c()
d_matrix <- data.frame(d_matrix)
d_matrix$Coefficients <- as.character(d_matrix$Coefficients)
d_matrix$CI_Lower <- as.character(d_matrix$CI_Lower)
d_matrix$CI_Upper <- as.character(d_matrix$CI_Upper)
d_matrix$Coefficients <- as.numeric(d_matrix$Coefficients)
d_matrix$CI_Lower <- as.numeric(d_matrix$CI_Lower)
d_matrix$CI_Upper <- as.numeric(d_matrix$CI_Upper)
d_matrix$x<-c(0.1,0.2,0.3,0.4,0.5)


d_matrix_7b <- d_matrix

d_matrix$x <- NULL
write.csv(d_matrix,'./source_data/extended_data_7b_source_data.csv')




#Produce plots:
ED_fig_7a <- ggplot(data = d_matrix_7a, aes(x = x, y = Coefficients)) +
  geom_hline(aes(yintercept = 0), color = "gray",
             linetype = 2, size = 1) +
  geom_point(size=2) +
  geom_linerange(aes(min = CI_Lower,
                     max = CI_Upper),
                 size=1) +
  ylab("\n The Effect of Searching Online   \n Without Encouragement on Rating a     \nFalse/Misleading Article as True    ") +
  theme_classic() +
  theme(axis.title.x = element_text(size=10),
        axis.text.x  = element_text(size=8),
        axis.title.y = element_text(size=10),
        axis.text.y  = element_text(size=8),
        plot.title = element_text(size = 10),
        legend.title = element_text(size=10),
        legend.text = element_text(size=10),
        title =element_text(size=18, face='bold')) +
  ylim(-0.1,0.3) +
  scale_x_continuous("Study \n",breaks=c(0.1,0.2,0.3,0.4,0.5),labels=Coef_names,limits=c(0.0,0.6)) +
  coord_flip()



ED_fig_7b <- ggplot(data = d_matrix_7b, aes(x = x, y = Coefficients)) +
  geom_hline(aes(yintercept = 0), color = "gray",
             linetype = 2, size = 1) +
  geom_point(size=2) +
  geom_linerange(aes(min = CI_Lower,
                     max = CI_Upper),
                 size=1) +
  ylab("\n The Effect of Searching Online   \n Without Encouragement on Rating a     \nFalse/Misleading Article as True   ") +
  theme_classic() +
  theme(axis.title.x = element_text(size=10),
        axis.text.x  = element_text(size=8),
        axis.title.y = element_text(size=10),
        axis.text.y  = element_text(size=8),
        plot.title = element_text(size = 10),
        legend.title = element_text(size=10),
        legend.text = element_text(size=10),
        title =element_text(size=18, face='bold')) +
  ylim(-0.5,1.52) +
  scale_x_continuous("Study \n",breaks=c(0.1,0.2,0.3,0.4,0.5),labels=Coef_names,limits=c(0.0,0.6)) +
  coord_flip()

ggarrange(ED_fig_7a, ED_fig_7b,
          labels="auto",
          ncol = 2,
          font.label=list(color="black",size=8))

ggsave('./Figures/ED_fig_7.eps',height=8,width=18,units='cm',device="eps")



#################################################################################

########################### Extended Data 8 #####################################

#################################################################################

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


P_Value <- rev(c(lin_results_fit_1_1$coeftable[1,4],
                 lin_results_fit_2_1$coeftable[1,4],
                 lin_results_fit_3_1$coeftable[1,4],
                 lin_results_fit_4_1$coeftable[1,4]))

Standard_Error <- rev(c(lin_results_fit_1_1$coeftable[1,2],
                        lin_results_fit_2_1$coeftable[1,2],
                        lin_results_fit_3_1$coeftable[1,2],
                        lin_results_fit_4_1$coeftable[1,2]))

#Put together matrix with data for plot:
d_matrix <- cbind(Coef_names,Coefficients,CI_Upper,CI_Lower,P_Value,Standard_Error)
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

d_matrix_fig_8a <- d_matrix

#Produce plot:
ED_fig_8a <- ggplot(data = d_matrix_fig_8a, aes(x = x, y = Coefficients)) +
  geom_hline(aes(yintercept = 0), color = "gray",
             linetype = 2, size = 1) +
  geom_point(size=2) +
  geom_linerange(aes(min = CI_Lower,
                     max = CI_Upper),
                 size=1) +
  ylab("\nEffect of Searching Online on Probability \nof Rating Misinformation as True") +
  theme_classic() +
  theme(axis.title.x = element_text(size=10),
        axis.text.x  = element_text(size=8),
        axis.title.y = element_text(size=10),
        axis.text.y  = element_text(size=8),
        plot.title = element_text(size = 10),
        legend.title = element_text(size=10),
        legend.text = element_text(size=10),
        title =element_text(size=18, face='bold')) +
  ylim(-0.05,0.2) +
  scale_x_continuous(" \n",breaks=c(1,2,3,4),labels=c('Study 4',
                                                      'Study 3',
                                                      'Study 2',
                                                      'Study 1'),limits=c(0.5,4.5)) +
  coord_flip()


colnames(d_matrix)[1] <- 'Study'
d_matrix$x <- NULL

write.csv(d_matrix,'./source_data/extended_data_8a_source_data.csv')


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

P_Value <- rev(c(lin_results_fit_1_2$coeftable[1,4],
                 lin_results_fit_2_2$coeftable[1,4],
                 lin_results_fit_3_2$coeftable[1,4],
                 lin_results_fit_4_2$coeftable[1,4]))

Standard_Error <- rev(c(lin_results_fit_1_2$coeftable[1,2],
                        lin_results_fit_2_2$coeftable[1,2],
                        lin_results_fit_3_2$coeftable[1,2],
                        lin_results_fit_4_2$coeftable[1,2]))



#Put together matrix with data for plot:
d_matrix <- cbind(Coef_names,Coefficients,CI_Upper,CI_Lower,P_Value,Standard_Error)
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
d_matrix_fig_8b <- d_matrix

ED_fig_8b <- ggplot(data = d_matrix_fig_8b, aes(x = x, y = Coefficients)) +
  geom_hline(aes(yintercept = 0), color = "gray",
             linetype = 2, size = 1) +
  geom_point(size=2) +
  geom_linerange(aes(min = CI_Lower,
                     max = CI_Upper),
                 size=1) +
  ylab("\nEffect of Searching Online on the Perceived       \nVeracity of Misinformation (7-point scale)       ") +
  theme_classic() +
  theme(axis.title.x = element_text(size=10),
        axis.text.x  = element_text(size=8),
        axis.title.y = element_text(size=10),
        axis.text.y  = element_text(size=8),
        plot.title = element_text(size = 10),
        legend.title = element_text(size=10),
        legend.text = element_text(size=10),
        title =element_text(size=18, face='bold')) +
  ylim(-0.10,0.8) +
  scale_x_continuous(" \n",breaks=c(1,2,3,4),labels=c('Study 4',
                                                      'Study 3',
                                                      'Study 2',
                                                      'Study 1'),limits=c(0.5,4.5)) +
  coord_flip()



ggarrange(ED_fig_8a, ED_fig_8b,
          labels="auto",
          ncol = 2,
          font.label=list(color="black",size=8))

ggsave('./Figures/ED_fig_8.eps',height=8,width=18,units='cm',device="eps")



colnames(d_matrix)[1] <- 'Study'
d_matrix$x <- NULL

write.csv(d_matrix,'./source_data/extended_data_8b_source_data.csv')




#################################################################################

########################### Extended Data 9 #####################################

#################################################################################


#Study 1:
#Pull in Study 1 data:
Study_1_df <- read.csv('./Data/Study_1_df_FM.csv')
#Remove NA values:
Study_1_df = na.omit(Study_1_df)

#Study 2:
#Pull in Study 2 data:
Study_2_df <- read.csv('./Data/Study_2_df_FM.csv')
#Remove NA values:
Study_2_df = na.omit(Study_2_df)

#Study 3:
#Pull in Study 3 data:
Study_3_df <- read.csv('./Data/Study_3_df_FM.csv')
Study_3_df$Article_day <- as.character(Study_3_df$Article_day)
Study_3_df$ResponseId <- as.character(Study_3_df$ResponseId)

#Remove NA values:
Study_3_df = na.omit(Study_3_df)

#Study 4:
#Pull in Study 4 data:
Study_4_df <- read.csv('./Data/Study_4_df_FM.csv')

#Remove NA values:
Study_4_df = na.omit(Study_4_df)

#Study 5:
#Pull in Study 5 data:
FM_Data_Study_5 <- read.csv('./Data/Study_5_df_FM.csv')

#Remove NA values:
FM_Data_Study_5 = na.omit(FM_Data_Study_5)

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




P_Value <- rev(c(Pred_fit_1_1$coeftable[2,4],
                 Pred_fit_1_2$coeftable[2,4],
                 Pred_fit_2_1$coeftable[2,4],
                 Pred_fit_2_2$coeftable[2,4],
                 Pred_fit_3_1$coeftable[2,4],
                 Pred_fit_3_2$coeftable[2,4],
                 Pred_fit_4_1$coeftable[2,4],
                 Pred_fit_4_2$coeftable[2,4],
                 Pred_fit_5_1$coeftable[2,4],
                 Pred_fit_5_2$coeftable[2,4]))

Standard_Error <-  rev(c(Pred_fit_1_1$coeftable[2,2],
                         Pred_fit_1_2$coeftable[2,2],
                         Pred_fit_2_1$coeftable[2,2],
                         Pred_fit_2_2$coeftable[2,2],
                         Pred_fit_3_1$coeftable[2,2],
                         Pred_fit_3_2$coeftable[2,2],
                         Pred_fit_4_1$coeftable[2,2],
                         Pred_fit_4_2$coeftable[2,2],
                         Pred_fit_5_1$coeftable[2,2],
                         Pred_fit_5_2$coeftable[2,2]))



#Put together matrix with data for plot:
d_matrix <- cbind(Coef_names,Group,Coefficients,CI_Upper,CI_Lower,P_Value,Standard_Error)
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
  scale_color_manual(values=cbbPalette_1, name = "Group") +
  ylab("\nEffect of Rating Misinformation as True on \n Seven-Point Ordinal Scale Veracity Rating") +
  theme_classic() +
  theme(axis.title.x = element_text(size=18),
        axis.text.x  = element_text(size=16),
        axis.title.y = element_text(size=18),
        axis.text.y  = element_text(size=16),
        plot.title = element_text(size = 20),
        legend.title = element_text(size=18),
        legend.text = element_text(size=18),
        title =element_text(size=18, face='bold')) +
  ylim(0.0,4.0) +
  scale_x_continuous(" \n",breaks=c(1,2,3,4,5),labels=c('Study 5',
                                                        'Study 4',
                                                        'Study 3',
                                                        'Study 2',
                                                        'Study 1'),limits=c(0.5,5.5)) +
  coord_flip()


ggsave('./Figures/ED_fig_9.eps',height=16,width=18,units='cm',device='eps')

colnames(d_matrix)[1] <- 'Study'
d_matrix$x <- NULL

#Round variables:
d_matrix$P_Value <- round(as.numeric(d_matrix$P_Value),6)
d_matrix$Standard_Error <- round(as.numeric(d_matrix$Standard_Error),6)

colnames(d_matrix) <- c('Study','Group','Effect','CI_Upper','CI_Lower','P_Value','Standard_Error')

write.csv(d_matrix,'./source_data/extended_data_9_source_data.csv')






##########################################################################################################

############################################ Extended Data 10 ############################################

##########################################################################################################

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

Final_Mat <- matrix(ncol=7)




New_matr <- matrix(c(round(lin_results_fit_1_1$coefficients[9],4),round(CI_1_1[9,1],4),round(CI_1_1[9,2],4),'Rate as True','Study 1',lin_results_fit_1_1$coeftable[9,4],lin_results_fit_1_1$coeftable[9,2],
                     round(lin_results_fit_1_2$coefficients[9],4),round(CI_1_2[9,1],4),round(CI_1_2[9,2],4),'Ordinal (7)','Study 1',lin_results_fit_1_2$coeftable[9,4],lin_results_fit_1_2$coeftable[9,2],
                     round(lin_results_fit_2_1$coefficients[9],4),round(CI_2_1[9,1],4),round(CI_2_1[9,2],4),'Rate as True','Study 2',lin_results_fit_2_1$coeftable[9,4],lin_results_fit_2_1$coeftable[9,2],
                     round(lin_results_fit_2_2$coefficients[9],4),round(CI_2_2[9,1],4),round(CI_2_2[9,2],4),'Ordinal (7)','Study 2',lin_results_fit_2_2$coeftable[9,4],lin_results_fit_2_2$coeftable[9,2],
                     round(lin_results_fit_3_1$coefficients[9],4),round(CI_3_1[9,1],4),round(CI_3_1[9,2],4),'Rate as True','Study 3',lin_results_fit_3_1$coeftable[9,4],lin_results_fit_3_1$coeftable[9,2],
                     round(lin_results_fit_3_2$coefficients[9],4),round(CI_3_2[9,1],4),round(CI_3_2[9,2],4),'Ordinal (7)','Study 3',lin_results_fit_3_2$coeftable[9,4],lin_results_fit_3_2$coeftable[9,2],
                     round(lin_results_fit_4_1$coefficients[9],4),round(CI_4_1[9,1],4),round(CI_4_1[9,2],4),'Rate as True','Study 4',lin_results_fit_4_1$coeftable[9,4],lin_results_fit_1_1$coeftable[9,2],
                     round(lin_results_fit_4_2$coefficients[9],4),round(CI_4_2[9,1],4),round(CI_4_2[9,2],4),'Ordinal (7)','Study 4',lin_results_fit_4_2$coeftable[9,4],lin_results_fit_4_2$coeftable[9,2],
                     round(lin_results_fit_5_1$coefficients[9],4),round(CI_5_1[9,1],4),round(CI_5_1[9,2],4),'Rate as True','Study 5',lin_results_fit_5_1$coeftable[9,4],lin_results_fit_5_1$coeftable[9,2],
                     round(lin_results_fit_5_2$coefficients[9],4),round(CI_5_2[9,1],4),round(CI_5_2[9,2],4),'Ordinal (7)','Study 5',lin_results_fit_5_2$coeftable[9,4],lin_results_fit_5_2$coeftable[9,2]),ncol=7,byrow=T)

Final_Mat <- rbind(Final_Mat,New_matr)

Final_Mat <- as.data.frame(Final_Mat)

Final_Mat <- na.omit(Final_Mat)

colnames(Final_Mat) <- c('Coef','Lower','Upper','Measure','Study','P_Value','Standard_Error')

Rate_T <- Final_Mat %>% filter(Measure == 'Rate as True')


Rate_T$x <- c(0.1,0.2,0.3,0.4,0.5)


Rate_T$Coef <- as.character(Rate_T$Coef)
Rate_T$Coef <- as.numeric(Rate_T$Coef)

Rate_T$Upper <- as.character(Rate_T$Upper)
Rate_T$Upper <- as.numeric(Rate_T$Upper)

Rate_T$Lower <- as.character(Rate_T$Lower)
Rate_T$Lower <- as.numeric(Rate_T$Lower)



Rate_T_1 <- Rate_T

ED_fig_10a <- ggplot(data = Rate_T_1, aes(x = x, y = Coef)) +
  geom_hline(aes(yintercept = 0), color = "gray",
             linetype = 2, size = 1) +
  geom_point(size=2) +
  geom_linerange(aes(min = Lower, 
                     max = Upper),
                 size=1) +
  ylab("\n The change in the effect of searching online on    
  rating a false/misleading article as true when        
  fact-checker agreement increases from 0 to 1      ") +
  theme_classic() +
  theme(axis.title.x = element_text(size=8),
        axis.text.x  = element_text(size=6),
        axis.title.y = element_text(size=8),
        axis.text.y  = element_text(size=6),
        plot.title = element_text(size = 8),
        legend.title = element_text(size=8),
        legend.text = element_text(size=8),
        title =element_text(size=8, face='bold')) +
  ylim(-0.78,0.4) +
  scale_x_continuous("Study \n",breaks=c(0.1,0.2,0.3,0.4,0.5),labels=c('Study 1',
                                                                       'Study 2',
                                                                       'Study 3',
                                                                       'Study 4',
                                                                       'Study 5'),limits=c(0,0.6)) +
  coord_flip()


write.csv(Rate_T,'./source_data/extended_data_10a_source_data.csv')



Rate_Ordinal_7 <- Final_Mat %>% filter(Measure == 'Ordinal (7)')
Rate_Ordinal_7$x <- c(0.1,0.2,0.3,0.4,0.5)

Rate_Ordinal_7$Coef <- as.character(Rate_Ordinal_7$Coef)
Rate_Ordinal_7$Coef <- as.numeric(Rate_Ordinal_7$Coef)

Rate_Ordinal_7$Upper <- as.character(Rate_Ordinal_7$Upper)
Rate_Ordinal_7$Upper <- as.numeric(Rate_Ordinal_7$Upper)

Rate_Ordinal_7$Lower <- as.character(Rate_Ordinal_7$Lower)
Rate_Ordinal_7$Lower <- as.numeric(Rate_Ordinal_7$Lower)


write.csv(Rate_Ordinal_7,'./source_data/extended_data_10b_source_data.csv')


Rate_Ordinal_7_1 <- Rate_Ordinal_7

ED_fig_10b <- ggplot(data = Rate_Ordinal_7_1, aes(x = x, y = Coef)) +
  geom_hline(aes(yintercept = 0), color = "gray",
             linetype = 2, size = 1) +
  geom_point(size=2) +
  geom_linerange(aes(min = Lower, 
                     max = Upper),
                 size=1) +
  ylab("\n The change in the effect of searching online on       
  belief in a false/misleading article when
       fact-checker agreement increases from 0 to 1        ") +
  theme_classic() +
  theme(axis.title.x = element_text(size=8),
        axis.text.x  = element_text(size=6),
        axis.title.y = element_text(size=8),
        axis.text.y  = element_text(size=6),
        plot.title = element_text(size = 8),
        legend.title = element_text(size=8),
        legend.text = element_text(size=8),
        title =element_text(size=8, face='bold')) +
  ylim(-3,1) +
  scale_x_continuous("Study \n",breaks=c(0.1,0.2,0.3,0.4,0.5),labels=c('Study 1',
                                                                       'Study 2',
                                                                       'Study 3',
                                                                       'Study 4',
                                                                       'Study 5'),limits=c(0,0.6)) +
  coord_flip()



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


lin_results_fit_1_1$nobs
lin_results_fit_2_1$nobs

Final_Mat <- matrix(ncol=7)

New_matr <- matrix(c(round(lin_results_fit_1_1$coefficients[9],4),round(CI_1_1[9,1],4),round(CI_1_1[9,2],4),'Rate as True','Within',lin_results_fit_1_1$coeftable[9,4],lin_results_fit_1_1$coeftable[9,2],
                     round(lin_results_fit_1_2$coefficients[9],4),round(CI_1_2[9,1],4),round(CI_1_2[9,2],4),'Ordinal (7)','Within',lin_results_fit_1_2$coeftable[9,4],lin_results_fit_1_2$coeftable[9,2],
                     round(lin_results_fit_2_1$coefficients[9],4),round(CI_2_1[9,1],4),round(CI_2_1[9,2],4),'Rate as True','Between',lin_results_fit_2_1$coeftable[9,4],lin_results_fit_2_1$coeftable[9,2],
                     round(lin_results_fit_2_2$coefficients[9],4),round(CI_2_2[9,1],4),round(CI_2_2[9,2],4),'Ordinal (7)','Between',lin_results_fit_2_2$coeftable[9,4],lin_results_fit_2_2$coeftable[9,2]),ncol=7,byrow=T)

Final_Mat <- rbind(Final_Mat,New_matr)




Final_Mat <- as.data.frame(Final_Mat)

Final_Mat <- na.omit(Final_Mat)

colnames(Final_Mat) <- c('Coef','Lower','Upper','Measure','Study','P_Value','Standard_Error')

Rate_T <- Final_Mat %>% filter(Measure == 'Rate as True')


Rate_T$x <- c(0.1,0.2)


Rate_T$Coef <- as.character(Rate_T$Coef)
Rate_T$Coef <- as.numeric(Rate_T$Coef)

Rate_T$Upper <- as.character(Rate_T$Upper)
Rate_T$Upper <- as.numeric(Rate_T$Upper)

Rate_T$Lower <- as.character(Rate_T$Lower)
Rate_T$Lower <- as.numeric(Rate_T$Lower)


write.csv(Rate_T,'./source_data/extended_data_10c_source_data.csv')


Rate_Ordinal_7 <- Final_Mat %>% filter(Measure == 'Ordinal (7)')
Rate_Ordinal_7$x <- c(0.1,0.2)

Rate_Ordinal_7$Coef <- as.character(Rate_Ordinal_7$Coef)
Rate_Ordinal_7$Coef <- as.numeric(Rate_Ordinal_7$Coef)

Rate_Ordinal_7$Upper <- as.character(Rate_Ordinal_7$Upper)
Rate_Ordinal_7$Upper <- as.numeric(Rate_Ordinal_7$Upper)

Rate_Ordinal_7$Lower <- as.character(Rate_Ordinal_7$Lower)
Rate_Ordinal_7$Lower <- as.numeric(Rate_Ordinal_7$Lower)


write.csv(Rate_Ordinal_7,'./source_data/extended_data_10d_source_data.csv')



Rate_T_2 <- Rate_T


ED_fig_10c <- ggplot(data = Rate_T_2, aes(x = x, y = Coef)) +
  geom_hline(aes(yintercept = 0), color = "gray",
             linetype = 2, size = 1) +
  geom_point(size=2) +
  geom_linerange(aes(min = Lower, 
                     max = Upper),
                 size=1) +
  ylab("\n The change in the effect of searching online on    
  rating a false/misleading article as true when        
  fact-checker agreement increases from 0 to 1      ") +
  theme_classic() +
  theme(axis.title.x = element_text(size=8),
        axis.text.x  = element_text(size=6),
        axis.title.y = element_text(size=8),
        axis.text.y  = element_text(size=6),
        plot.title = element_text(size = 8),
        legend.title = element_text(size=8),
        legend.text = element_text(size=8),
        title =element_text(size=8, face='bold')) +
  ylim(-0.6,0.4) +
  scale_x_continuous("Experiment Type \n",breaks=c(0.1,0.2),labels=c('Within',
                                                                     'Between'),limits=c(0,0.3)) +
  coord_flip()


Rate_Ordinal_7_2 <- Rate_Ordinal_7


ED_fig_10d <- ggplot(data = Rate_Ordinal_7_2, aes(x = x, y = Coef)) +
  geom_hline(aes(yintercept = 0), color = "gray",
             linetype = 2, size = 1) +
  geom_point(size=2) +
  geom_linerange(aes(min = Lower, 
                     max = Upper),
                 size=1) +
  ylab("\n The change in the effect of searching online on       
  belief in a false/misleading article when
       fact-checker agreement increases from 0 to 1        ") +
  theme_classic() +
  theme(axis.title.x = element_text(size=8),
        axis.text.x  = element_text(size=6),
        axis.title.y = element_text(size=8),
        axis.text.y  = element_text(size=6),
        plot.title = element_text(size = 8),
        legend.title = element_text(size=8),
        legend.text = element_text(size=8),
        title =element_text(size=8, face='bold')) +
  ylim(-1.28,0.75) +
  scale_x_continuous("Experiment Type \n",breaks=c(0.1,0.2),labels=c('Within',
                                                                     'Between'),limits=c(0,0.3)) +
  coord_flip()



ggarrange(ED_fig_10a, ED_fig_10b, ED_fig_10c, ED_fig_10d,
          labels="auto",
          ncol = 2,
          nrow = 2,
          font.label=list(color="black",size=8))

ggsave('./Figures/ED_fig_10.eps',height=16,width=18,units='cm',device="eps")






########################### Source Data ##############################


############################# Extended Data 3 ########################

#Read in data:
fig_3a_sd <- read_csv("./source_data/extended_data_3a.csv")
fig_3b_sd <- read_csv("./source_data/extended_data_3b.csv")

#Rename first column:
colnames(fig_3a_sd)[1] <- 'X'
colnames(fig_3b_sd)[1] <- 'X'

#Remove unnecessary column:
fig_3a_sd$X <- NULL
fig_3b_sd$X <- NULL

#Add variable:
fig_3a_sd$Panel <- 'A'
fig_3b_sd$Panel <- 'B'


#Rename columns:
colnames(fig_3a_sd) <- c('Effect','CI_Lower','CI_Upper','DV_Measure','Type_News_Quality','P_Value','Standard_Error','Group','Panel')
colnames(fig_3b_sd) <- c('Effect','CI_Lower','CI_Upper','DV_Measure','Type_News_Quality','P_Value','Standard_Error','Group','Panel')

#Reorder columns:
fig_3a_sd <- fig_3a_sd %>% select(Panel,Group,DV_Measure,Type_News_Quality,Effect,CI_Upper,CI_Lower,P_Value,Standard_Error)
fig_3b_sd <- fig_3b_sd %>% select(Panel,Group,DV_Measure,Type_News_Quality,Effect,CI_Upper,CI_Lower,P_Value,Standard_Error)

fig_3_sd <- rbind(fig_3a_sd,fig_3b_sd)

write_csv(fig_3_sd,'./source_data/ED_fig_3_source_data.csv')


############################# Extended Data 4 ########################


#Read in data:
fig_4a_sd <- read_csv("./source_data/extended_data_4a.csv")
fig_4b_sd <- read_csv("./source_data/fig_2b_source_data.csv")
fig_4c_sd <- read_csv("./source_data/extended_data_4c_source_data.csv")
fig_4d_sd <- read_csv("./source_data/extended_data_4d_source_data.csv")

#Rename first column:
colnames(fig_4a_sd)[1] <- 'X'
colnames(fig_4b_sd)[1] <- 'X'
colnames(fig_4c_sd)[1] <- 'X'
colnames(fig_4d_sd)[1] <- 'X'

#Remove unnecessary column:
fig_4a_sd$X <- NULL
fig_4b_sd$X <- NULL
fig_4c_sd$X <- NULL
fig_4d_sd$X <- NULL

#Remove unnecessary column:
fig_4a_sd$Panel <- 'A'
fig_4b_sd$Panel <- 'B'
fig_4c_sd$Panel <- 'C'
fig_4d_sd$Panel <- 'D'

#Add columns that panel A have
fig_4b_sd$Proportion <- NA
fig_4b_sd$Number_low_quality_links <- NA
fig_4b_sd$Article_Rating <- NA

fig_4c_sd$Proportion <- NA
fig_4c_sd$Number_low_quality_links <- NA
fig_4c_sd$Article_Rating <- NA

fig_4d_sd$Proportion <- NA
fig_4d_sd$Number_low_quality_links <- NA
fig_4d_sd$Article_Rating <- NA


#Change column names from panels c and d to match panel b:

colnames(fig_4b_sd) <- c('DV_Measure','Effect','CI_Upper','CI_Lower','P_Value','Standard_Error','Panel','Proportion','Number_low_quality_links','Article_Rating')
colnames(fig_4c_sd) <- c('Effect','CI_Lower','CI_Upper','DV_Measure','Type_News','P_Value','Standard_Error','Panel','Proportion','Number_low_quality_links','Article_Rating')
colnames(fig_4d_sd) <- c('Effect','CI_Lower','CI_Upper','DV_Measure','Type_News','P_Value','Standard_Error','Panel','Proportion','Number_low_quality_links','Article_Rating')

#Add columns that panel C and D have:
fig_4b_sd$Type_News <- NA

#Add columns that panel B, C and D have:
fig_4a_sd$DV_Measure <- NA
fig_4a_sd$Effect <- NA
fig_4a_sd$CI_Upper <- NA
fig_4a_sd$CI_Lower <- NA
fig_4a_sd$P_Value <- NA
fig_4a_sd$Standard_Error <- NA
fig_4a_sd$DV_Measure <- NA
fig_4a_sd$Type_News <- NA

#Order columns: 

fig_4a_sd <- fig_4a_sd %>% select(Panel,DV_Measure,Type_News,Effect,CI_Upper,CI_Lower,P_Value,Standard_Error,Proportion,Number_low_quality_links,Article_Rating)
fig_4b_sd <- fig_4b_sd %>% select(Panel,DV_Measure,Type_News,Effect,CI_Upper,CI_Lower,P_Value,Standard_Error,Proportion,Number_low_quality_links,Article_Rating)
fig_4c_sd <- fig_4c_sd %>% select(Panel,DV_Measure,Type_News,Effect,CI_Upper,CI_Lower,P_Value,Standard_Error,Proportion,Number_low_quality_links,Article_Rating)
fig_4d_sd <- fig_4d_sd %>% select(Panel,DV_Measure,Type_News,Effect,CI_Upper,CI_Lower,P_Value,Standard_Error,Proportion,Number_low_quality_links,Article_Rating)

fig_4_sd <- rbind(fig_4a_sd,fig_4b_sd)
fig_4_sd <- rbind(fig_4_sd,fig_4c_sd)
fig_4_sd <- rbind(fig_4_sd,fig_4d_sd)

write_csv(fig_4_sd,'./source_data/extended_data_4_source_data.csv')


############################# Extended Data 5 ########################

#Read in data:
fig_5a_sd <- read_csv("./source_data/extended_data_5a_source_data.csv")
fig_5b_sd <- read_csv("./source_data/extended_data_5b_source_data.csv")
fig_5c_sd <- read_csv("./source_data/extended_data_5c_source_data.csv")

#Rename first column:
colnames(fig_5a_sd)[1] <- 'X'
colnames(fig_5b_sd)[1] <- 'X'
colnames(fig_5c_sd)[1] <- 'X'

#Remove unnecessary column:
fig_5a_sd$X <- NULL
fig_5b_sd$X <- NULL
fig_5c_sd$X <- NULL

#Remove unnecessary column:
fig_5a_sd$Panel <- 'A'
fig_5b_sd$Panel <- 'B'
fig_5c_sd$Panel <- 'C'

#Change columns names
colnames(fig_5a_sd) <- c('Variable_Name','Effect','CI_Upper','CI_Lower','P_Value','Standard_Error','Panel')
colnames(fig_5b_sd) <- c('Proportion','Number_low_quality_links','Article_Rating','Panel')
colnames(fig_5c_sd) <- c('Variable_Name','Effect','CI_Upper','CI_Lower','P_Value','Standard_Error','Panel')


#Add columns that panel B have:
fig_5a_sd$Proportion <- NA
fig_5a_sd$Number_low_quality_links <- NA
fig_5a_sd$Article_Rating <- NA

fig_5c_sd$Proportion <- NA
fig_5c_sd$Number_low_quality_links <- NA
fig_5c_sd$Article_Rating <- NA

#Add columns that panels A and C have:
fig_5b_sd$Variable_Name <- NA
fig_5b_sd$Effect <- NA
fig_5b_sd$CI_Upper <- NA
fig_5b_sd$CI_Lower <- NA
fig_5b_sd$P_Value <- NA
fig_5b_sd$Standard_Error <- NA

#Order columns: 
fig_5a_sd <- fig_5a_sd %>% select(Panel,Variable_Name,Effect,CI_Upper,CI_Lower,P_Value,Standard_Error,Proportion,Number_low_quality_links,Article_Rating)
fig_5b_sd <- fig_5b_sd %>% select(Panel,Variable_Name,Effect,CI_Upper,CI_Lower,P_Value,Standard_Error,Proportion,Number_low_quality_links,Article_Rating)
fig_5c_sd <- fig_5c_sd %>% select(Panel,Variable_Name,Effect,CI_Upper,CI_Lower,P_Value,Standard_Error,Proportion,Number_low_quality_links,Article_Rating)

fig_5_sd <- rbind(fig_5a_sd,fig_5b_sd)
fig_5_sd <- rbind(fig_5_sd,fig_5c_sd)

write_csv(fig_5_sd,'./source_data/fig_5_source_data.csv')

############################# Extended Data 6 ########################

#Read in data:
fig_6a_sd <- read_csv("./source_data/extended_data_6a_source_data.csv")
fig_6b_sd <- read_csv("./source_data/extended_data_6b_source_data.csv")
fig_6c_sd <- read_csv("./source_data/extended_data_6c_source_data.csv")

#Rename first column:
colnames(fig_6a_sd)[1] <- 'X'
colnames(fig_6b_sd)[1] <- 'X'
colnames(fig_6c_sd)[1] <- 'X'

#Remove unnecessary column:
fig_6a_sd$X <- NULL
fig_6b_sd$X <- NULL
fig_6c_sd$X <- NULL

#Remove unnecessary column:
fig_6a_sd$Panel <- 'A'
fig_6b_sd$Panel <- 'B'
fig_6c_sd$Panel <- 'C'

#Rename columns:
colnames(fig_6a_sd) <- c('Article_Type','Study','Effect','CI_Upper','CI_Lower','P_Value','Standard_Error','Panel')
colnames(fig_6b_sd) <- c('Article_Type','Study','Effect','CI_Upper','CI_Lower','P_Value','Standard_Error','Panel')
colnames(fig_6c_sd) <- c('Article_Type','Study','Effect','CI_Upper','CI_Lower','P_Value','Standard_Error','Panel')

#Reorder columns:
fig_6a_sd <- fig_6a_sd %>% select(Panel,Article_Type,Study,Effect,CI_Upper,CI_Lower,P_Value,Standard_Error)
fig_6b_sd <- fig_6b_sd %>% select(Panel,Article_Type,Study,Effect,CI_Upper,CI_Lower,P_Value,Standard_Error)
fig_6c_sd <- fig_6c_sd %>% select(Panel,Article_Type,Study,Effect,CI_Upper,CI_Lower,P_Value,Standard_Error)

fig_6_sd <- rbind(fig_6a_sd,fig_6b_sd)
fig_6_sd <- rbind(fig_6_sd,fig_6c_sd)

write_csv(fig_6_sd,'./source_data/fig_6_source_data.csv')

############################# Extended Data 7 ########################

#Read in data:
fig_7a_sd <- read_csv("./source_data/extended_data_7a_source_data.csv")
fig_7b_sd <- read_csv("./source_data/extended_data_7b_source_data.csv")

#Rename first column:
colnames(fig_7a_sd)[1] <- 'X'
colnames(fig_7b_sd)[1] <- 'X'

#Remove unnecessary column:
fig_7a_sd$X <- NULL
fig_7b_sd$X <- NULL

#Add variable:
fig_7a_sd$Panel <- 'A'
fig_7b_sd$Panel <- 'B'
fig_7a_sd$DV_Measure <- 'True Dummy'
fig_7b_sd$DV_Measure <- '7-Point Ordinal'


#Rename columns:
colnames(fig_7a_sd) <- c('Study','Effect','CI_Upper','CI_Lower','P_Value','Standard_Error','Panel','DV_Measure')
colnames(fig_7b_sd) <- c('Study','Effect','CI_Upper','CI_Lower','P_Value','Standard_Error','Panel','DV_Measure')

#Reorder columns:
fig_7a_sd <- fig_7a_sd %>% select(Panel,DV_Measure,Study,Effect,CI_Upper,CI_Lower,P_Value,Standard_Error)
fig_7b_sd <- fig_7b_sd %>% select(Panel,DV_Measure,Study,Effect,CI_Upper,CI_Lower,P_Value,Standard_Error)

fig_7_sd <- rbind(fig_7a_sd,fig_7b_sd)

write_csv(fig_7_sd,'./source_data/fig_7_source_data.csv')

############################# Extended Data 8 ########################

#Read in data:
fig_8a_sd <- read_csv("./source_data/extended_data_8a_source_data.csv")
fig_8b_sd <- read_csv("./source_data/extended_data_8b_source_data.csv")

#Rename first column:
colnames(fig_8a_sd)[1] <- 'X'
colnames(fig_8b_sd)[1] <- 'X'

#Remove unnecessary column:
fig_8a_sd$X <- NULL
fig_8b_sd$X <- NULL

#Add variable:
fig_8a_sd$Panel <- 'A'
fig_8b_sd$Panel <- 'B'
fig_8a_sd$DV_Measure <- 'True Dummy'
fig_8b_sd$DV_Measure <- '7-Point Ordinal'


#Rename columns:
colnames(fig_8a_sd) <- c('Study','Effect','CI_Upper','CI_Lower','P_Value','Standard_Error','Panel','DV_Measure')
colnames(fig_8b_sd) <- c('Study','Effect','CI_Upper','CI_Lower','P_Value','Standard_Error','Panel','DV_Measure')

#Reorder columns:
fig_8a_sd <- fig_8a_sd %>% select(Panel,DV_Measure,Study,Effect,CI_Upper,CI_Lower,P_Value,Standard_Error)
fig_8b_sd <- fig_8b_sd %>% select(Panel,DV_Measure,Study,Effect,CI_Upper,CI_Lower,P_Value,Standard_Error)

fig_8_sd <- rbind(fig_8a_sd,fig_8b_sd)

write_csv(fig_8_sd,'./source_data/fig_8_source_data.csv')


############################# Extended Data 10 ########################

#Read in data:
fig_10a_sd <- read_csv("./source_data/extended_data_10a_source_data.csv")
fig_10b_sd <- read_csv("./source_data/extended_data_10b_source_data.csv")
fig_10c_sd <- read_csv("./source_data/extended_data_10c_source_data.csv")
fig_10d_sd <- read_csv("./source_data/extended_data_10d_source_data.csv")

#Rename first column:
colnames(fig_10a_sd)[1] <- 'X'
colnames(fig_10b_sd)[1] <- 'X'
colnames(fig_10c_sd)[1] <- 'X'
colnames(fig_10d_sd)[1] <- 'X'

#Remove unnecessary column:
fig_10a_sd$X <- NULL
fig_10b_sd$X <- NULL
fig_10c_sd$X <- NULL
fig_10d_sd$X <- NULL
fig_10a_sd$x <- NULL
fig_10b_sd$x <- NULL
fig_10c_sd$x <- NULL
fig_10d_sd$x <- NULL

#Add variable:
fig_10a_sd$Panel <- 'A'
fig_10b_sd$Panel <- 'B'
fig_10c_sd$Panel <- 'C'
fig_10d_sd$Panel <- 'D'
fig_10a_sd$DV_Measure <- fig_10a_sd$Measure
fig_10b_sd$DV_Measure <- fig_10b_sd$Measure
fig_10c_sd$DV_Measure <- fig_10c_sd$Measure
fig_10d_sd$DV_Measure <- fig_10d_sd$Measure

fig_10a_sd$Measure <- NULL
fig_10b_sd$Measure <- NULL
fig_10c_sd$Measure <- NULL
fig_10d_sd$Measure <- NULL

#Rename columns:
colnames(fig_10a_sd) <- c('Effect','CI_Lower','CI_Upper','Study','P_Value','Standard_Error','Panel','DV_Measure')
colnames(fig_10b_sd) <- c('Effect','CI_Lower','CI_Upper','Study','P_Value','Standard_Error','Panel','DV_Measure')
colnames(fig_10c_sd) <- c('Effect','CI_Lower','CI_Upper','Study','P_Value','Standard_Error','Panel','DV_Measure')
colnames(fig_10d_sd) <- c('Effect','CI_Lower','CI_Upper','Study','P_Value','Standard_Error','Panel','DV_Measure')

#Reorder columns:
fig_10a_sd <- fig_10a_sd %>% select(Panel,DV_Measure,Study,Effect,CI_Upper,CI_Lower,P_Value,Standard_Error)
fig_10b_sd <- fig_10b_sd %>% select(Panel,DV_Measure,Study,Effect,CI_Upper,CI_Lower,P_Value,Standard_Error)
fig_10c_sd <- fig_10c_sd %>% select(Panel,DV_Measure,Study,Effect,CI_Upper,CI_Lower,P_Value,Standard_Error)
fig_10d_sd <- fig_10d_sd %>% select(Panel,DV_Measure,Study,Effect,CI_Upper,CI_Lower,P_Value,Standard_Error)

fig_10_sd <- rbind(fig_10a_sd,fig_10b_sd)
fig_10_sd <- rbind(fig_10_sd,fig_10c_sd)
fig_10_sd <- rbind(fig_10_sd,fig_10d_sd)

write_csv(fig_10_sd,'./source_data/ED_fig_10_source_data.csv')


########################## Write Output for script ###########################
writeLines(capture.output(sessionInfo()), "sessionInfo_extended_data.txt")


