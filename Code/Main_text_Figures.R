

###################################################################################################

###################################################################################################

###################################################################################################

#Author: Kevin Aslett
#Code Title: Main_text_Figures.R
#Paper Title: Online Searches to Evaluate Misinformation Can Increase Its Perceived Veracity
#Purpose of code: Generate all figures that are located in the main text of the paper.

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

#FILES OUT:
#Figures:
#Figure 1: fig_1.eps
#Figure 2: fig_2.eps
#Figure 3: fig_3.eps
#Figure 4: fig_4.eps
#Source Data:
#SD for Figure 1: ./source_data/fig_1_source_data.csv 
#SD for Figure 2: ./source_data/fig_2_source_data.csv
#SD for Figure 3: ./source_data/fig_3_source_data.csv
#SD for Figure 4: ./source_data/fig_4_source_data.csv


###################################################################################################

###################################################################################################

###################################################################################################

#Read in Libraries:
library(ggplot2)
library(dplyr)
library(xtable)
library(stats)
library(fixest)
library(irr)
library(lmtest)
library(Rmisc)

#Set color palettes to use:
cbbPalette_1 <- c("#009E73","#E69F00","#56B4E9","#F0E442", "#0072B2", "#D55E00", "#CC79A7")
cbbPalette_2 <- c("#E69F00","#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
cbbPalette_3 <- c("#E69F00", "#009E73","#56B4E9", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")



########################################################################################

# Run Model Testing Effect of Searching Online on Belief in Misinformation for Study 1 #

########################################################################################

#Read in Study 1 data:
Study_1_df <- read.csv('./Data/Study_1_df_FM.csv')

#Remove NA values:
Study_1_df = na.omit(Study_1_df)

#Run OLS Model with clustered standard errors (Categorical Measure - True Dummy):
lin_results_fit_1_1 = feols(Susc_FN ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Study_1_df)

#Calculate 95% confidence intervals:
CI_1_1 <- confint(lin_results_fit_1_1)

#Calculate Cohen's d:
lin_results_fit_1_1$coefficients[1]/sd(Study_1_df$Susc_FN)

#Percentage change:
lin_results_fit_1_1$coefficients[1]/mean(Study_1_df$Susc_FN[Study_1_df$Treat_Search==0])

#Run OLS Model with clustered standard errors (7-point ordinal scale):
lin_results_fit_1_2 = feols(Likert_Evaluation ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Study_1_df)

#Calculate 95% confidence intervals:
CI_1_2 <- confint(lin_results_fit_1_2)

#Calculate Cohen's d:
lin_results_fit_1_2$coefficients[1]/sd(Study_1_df$Likert_Evaluation,na.rm=T)


########################################################################################

# Run Model Testing Effect of Searching Online on Belief in Misinformation for Study 2 #

########################################################################################


#Read in Study 2 data:
Study_2_df <- read.csv('./Data/Study_2_df_FM.csv')

#Remove NA values:
Study_2_df = na.omit(Study_2_df)

#Run OLS Model with clustered standard errors (Categorical Measure - True Dummy):
lin_results_fit_2_1 = feols(Susc_FN ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Study_2_df)

#Calculate 95% confidence intervals:
CI_2_1 <- confint(lin_results_fit_2_1)

#Calculate Cohen's d:
lin_results_fit_2_1$coefficients[1]/sd(Study_2_df$Susc_FN)

#Percentage change:
lin_results_fit_2_1$coefficients[1]/mean(Study_2_df$Susc_FN[Study_2_df$Treat_Search==0])


#Run OLS Model with clustered standard errors (7-point ordinal scale):
lin_results_fit_2_2 = feols(Likert_Evaluation ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Study_2_df)

#Calculate 95% confidence intervals:
CI_2_2 <- confint(lin_results_fit_2_2)

#Calculate Cohen's s:
lin_results_fit_2_2$coefficients[1]/sd(Study_2_df$Likert_Evaluation)


########################################################################################

# Run Model Testing Effect of Searching Online on Belief in Misinformation for Study 3 #

########################################################################################

#Read in data for Study 3:
Study_3_df <- read.csv('./Data/Study_3_df_FM.csv')

#Remove NA values:
Study_3_df = na.omit(Study_3_df)

#Run OLS Model with clustered standard errors (Categorical Measure - True Dummy):
lin_results_fit_3_1 = feols(True_Dummy ~ Treatment + Age + Dummy_Ideology + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Study_3_df)

#Calculate 95% confidence intervals:
CI_3_1 <- confint(lin_results_fit_3_1)

#Calculate Cohen's d:
lin_results_fit_3_1$coefficients[1]/sd(Study_3_df$Susc_FN)

#Run OLS Model with clustered standard errors (7-point ordinal scale):
lin_results_fit_3_2 = feols(Likert_Evaluation ~ Treatment + Age + Dummy_Ideology + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Study_3_df)

#Calculate 95% confidence intervals:
CI_3_2 <- confint(lin_results_fit_3_2)

#Calculate Cohen's d:
lin_results_fit_3_2$coefficients[1]/sd(Study_3_df$Likert_Evaluation)

#Run Model Testing Effect of Searching Online on Belief in Misinformation for Study 4:

#Read in data from Study 4
Study_4_df <- read.csv('./Data/Study_4_df_FM.csv')

#Remove NA values:
Study_4_df = na.omit(Study_4_df)

#Run OLS Model with clustered standard errors (Categorical Measure - True Dummy):
lin_results_fit_4_1 = feols(Susc_FN ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Study_4_df)

#Calculate 95% confidence intervals:
CI_4_1 <- confint(lin_results_fit_4_1)

#Calculate Cohen's d:
lin_results_fit_4_1$coefficients[1]/sd(Study_4_df$Susc_FN)

#Run OLS Model with clustered standard errors (7-point ordinal scale):
lin_results_fit_4_2 = feols(Likert_Evaluation ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Study_4_df)

#Calculate 95% confidence intervals:
CI_4_2 <- confint(lin_results_fit_4_2)

#Cohen's d:
lin_results_fit_4_2$coefficients[1]/sd(Study_4_df$Likert_Evaluation)

#########################################

# Figure 1A: Categorical (Rate as True) #

#########################################

#Create vector with Study names:
Coef_names <- rev(c('Study 1',
                    'Study 2',
                    'Study 3',
                    'Study 4'))

#Create vector with coefficients:
Coefficients <- rev(c(lin_results_fit_1_1$coefficients[1],
                      lin_results_fit_2_1$coefficients[1],
                      lin_results_fit_3_1$coefficients[1],
                      lin_results_fit_4_1$coefficients[1]))

#Create vector with upper confidence intervals:
CI_Upper <- rev(c(CI_1_1[1,2],
                  CI_2_1[1,2],
                  CI_3_1[1,2],
                  CI_4_1[1,2]))

#Create vector with lower confidence intervals:
CI_Lower <- rev(c(CI_1_1[1,1],
                  CI_2_1[1,1],
                  CI_3_1[1,1],
                  CI_4_1[1,1])) 

#Create vector with p-values:
P_Value <- rev(c(lin_results_fit_1_1$coeftable[1,4],
                 lin_results_fit_2_1$coeftable[1,4],
                 lin_results_fit_3_1$coeftable[1,4],
                 lin_results_fit_4_1$coeftable[1,4]))

#Create vector with standard errors:
Standard_Error <- rev(c(lin_results_fit_1_1$coeftable[1,2],
                        lin_results_fit_2_1$coeftable[1,2],
                        lin_results_fit_3_1$coeftable[1,2],
                        lin_results_fit_4_1$coeftable[1,2]))



#Bind together previously created vectors:
d_matrix_1a <- cbind(Coef_names,Coefficients,CI_Upper,CI_Lower,P_Value,Standard_Error)

#Create blank rownames:
rownames(d_matrix_1a) <- c()

#Convert matrix to dataframe:
d_matrix_1a <- data.frame(d_matrix_1a)

#Convert variables to numeric:
d_matrix_1a$Coefficients <- as.numeric(d_matrix_1a$Coefficients)
d_matrix_1a$CI_Lower <- as.numeric(d_matrix_1a$CI_Lower)
d_matrix_1a$CI_Upper <- as.numeric(d_matrix_1a$CI_Upper)

#Set points on x-axis:
d_matrix_1a$x<-c(1,2,3,4)

#Save plot as fig_1a:
fig_1a <- ggplot(data = d_matrix_1a, aes(x = x, y = Coefficients)) +
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

#Change 1st column name to "Study"
colnames(d_matrix_1a)[1] <- 'Study'

#Remove x-axis points
d_matrix_1a$x <- NULL

#Write zource data to a csv:
write.csv(d_matrix_1a,'./source_data/fig_1a_source_data.csv')


####################################

# Figure 1B: Ordinal Scale (Seven) #

####################################

#Create vector with Study names:
Coef_names <- rev(c('Study 1',
                    'Study 2',
                    'Study 3',
                    'Study 4'))

#Create vector with coefficients:
Coefficients <- rev(c(lin_results_fit_1_2$coefficients[1],
                      lin_results_fit_2_2$coefficients[1],
                      lin_results_fit_3_2$coefficients[1],
                      lin_results_fit_4_2$coefficients[1]))

#Create vector with upper confidence intervals:
CI_Upper <- rev(c(CI_1_2[1,2],
                  CI_2_2[1,2],
                  CI_3_2[1,2],
                  CI_4_2[1,2]))

#Create vector with lower confidence intervals:
CI_Lower <- rev(c(CI_1_2[1,1],
                  CI_2_2[1,1],
                  CI_3_2[1,1],
                  CI_4_2[1,1]))         

#Create vector with p-values:
P_Value <- rev(c(lin_results_fit_1_2$coeftable[1,4],
                 lin_results_fit_2_2$coeftable[1,4],
                 lin_results_fit_3_2$coeftable[1,4],
                 lin_results_fit_4_2$coeftable[1,4]))

#Create vector with standard errors:
Standard_Error <- rev(c(lin_results_fit_1_2$coeftable[1,2],
                        lin_results_fit_2_2$coeftable[1,2],
                        lin_results_fit_3_2$coeftable[1,2],
                        lin_results_fit_4_2$coeftable[1,2]))



#Bind vectors together:
d_matrix_1b <- cbind(Coef_names,Coefficients,CI_Upper,CI_Lower,P_Value,Standard_Error)

#Ensure rownames are empty:
rownames(d_matrix_1b) <- c()

#Convert matrix to dataframe:
d_matrix_1b <- data.frame(d_matrix_1b)

#Convert variable to numeric
d_matrix_1b$Coefficients <- as.numeric(d_matrix_1b$Coefficients)
d_matrix_1b$CI_Lower <- as.numeric(d_matrix_1b$CI_Lower)
d_matrix_1b$CI_Upper <- as.numeric(d_matrix_1b$CI_Upper)

#Set points on x-axis:
d_matrix_1b$x<-c(1,2,3,4)

#Save plot to fig_1b:

fig_1b <- ggplot(data = d_matrix_1b, aes(x = x, y = Coefficients)) +
  geom_hline(aes(yintercept = 0), color = "gray",
             linetype = 2, size = 1.2) +
  geom_point(size=2) +
  geom_linerange(aes(min = CI_Lower,
                     max = CI_Upper),
                 size=1) +
  ylab("\nEffect of Searching Online on the Perceived      \n Veracity of Misinformation (7-point scale)       ") +
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


#Change 1st column name to "Study"
colnames(d_matrix_1b)[1] <- 'Study'

#Remove x-axis points
d_matrix_1b$x <- NULL

#Write zource data to a csv:
write.csv(d_matrix_1b,'./source_data/fig_1b_source_data.csv')

###########################################################################

############################### Figure 2b   ###############################

###########################################################################

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
ggplot(data = d_matrix, aes(x = x, y = Coefficients)) +
  geom_hline(aes(yintercept = 0), color = "gray",
             linetype = 2, size = 1) +
  geom_point(size=2) +
  geom_linerange(aes(min = CI_Lower,
                     max = CI_Upper),
                 size=1) +
  ylab("\nEffect of Searching for Information on \nPerceived Veracity of Misinformation \n(1 unit is 1 standard deviation) ") +
  theme_classic() +
  theme(axis.title.x = element_text(size=10),
        axis.text.x  = element_text(size=8),
        axis.title.y = element_text(size=10),
        axis.text.y  = element_text(size=8),
        plot.title = element_text(size = 10),
        legend.title = element_text(size=10),
        legend.text = element_text(size=10),
        title =element_text(size=18, face='bold')) +
  ylim(-0.13,0.7) +
  scale_x_continuous("Perceived Veracity Scale \n",breaks=c(0.3,0.2,0.1),labels=Coef_names,limits=c(0.0,0.4)) +
  coord_flip()

fig_2b <- ggplot(data = d_matrix, aes(x = x, y = Coefficients)) +
  geom_hline(aes(yintercept = 0), color = "gray",
             linetype = 2, size = 1) +
  geom_point(size=2) +
  geom_linerange(aes(min = CI_Lower,
                     max = CI_Upper),
                 size=1) +
  ylab("\nEffect of Searching for Information on         \nPerceived Veracity of Misinformation        \n(1 unit is 1 standard deviation)        ") +
  theme_classic() +
  theme(axis.title.x = element_text(size=8),
        axis.text.x  = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.text.y  = element_text(size=8),
        plot.title = element_text(size = 8),
        legend.title = element_text(size=6),
        legend.text = element_text(size=8),
        title =element_text(size=18, face='bold')) +
  ylim(-0.13,0.7) +
  scale_x_continuous("Perceived Veracity Scale \n",breaks=c(0.3,0.2,0.1),labels=Coef_names,limits=c(0.0,0.4)) +
  coord_flip()





colnames(d_matrix)[1] <- 'Measure'
d_matrix$x <- NULL

write.csv(d_matrix,'./source_data/fig_2b_source_data.csv')
write.csv(d_matrix,'./source_data/extended_data_4b_source_data.csv')



################################################################################################################

########################################## Figure 2a ###########################################################

################################################################################################################

Study_5_treat_data <- read.csv('./Data/Study_5_treat_data.csv')

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
fig_2a <- ggplot(Matrix_Dist, aes(fill=Percentage, y=Proportion, x=Article_Rating)) + 
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
        axis.text.x  = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.text.y  = element_text(size=8),
        title =element_text(size=6, face='bold'),
        legend.text = element_text(size=6)) + guides(fill=guide_legend(
          keywidth=0.2,
          keyheight=0.2,
          default.unit="inch")) +
  geom_text(aes(label=Proportion), position=position_dodge(width=0.9), vjust=-0.25,size=2) +
  ylim(0,1)


fig_2a


colnames(Matrix_Dist)[2] <- 'Number_low_quality_links'

write.csv(Matrix_Dist,'./source_data/fig_2a_source_data.csv')




#Comparative Statistic:

#Create true article dummy variable:
Search_Results_T$True_Article = 1
Search_Results_FM$True_Article = 0

#Combine dataframes:
df_2a <- rbind(Search_Results_T,Search_Results_FM)

#Run simple linear regression:
fit_lm_2a <- lm(Unrel_contain ~ True_Article,data=df_2a)

#Print output:
summary(fit_lm_2a)

################################################################################################################

########################################## Figure 2c ###########################################################

################################################################################################################


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

#Cohen's D:
fit_2_3$coefficients[1]/sd(Control_Data$True_Dummy)

#Confirm the null hypothesis: 
library(BayesFactor)

bf = ttestBF(formula = True_Dummy ~ Treatment, data = Study_5_subset_1)

bf

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

nrow(Study_5_subset_2)

#Run OLS Model with clustered standard errors:
fit_2_1 = feols(Seven_Ordinal ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=Study_5_subset_2)
CI_2_1 = confint(fit_2_1,se='twoway')

#Run OLS Model with clustered standard errors:
fit_2_2 = feols(Four_Ordinal ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=Study_5_subset_2)
CI_2_2 = confint(fit_2_2,se='twoway')

#Run OLS Model with clustered standard errors:
fit_2_3 = feols(True_Dummy ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=Study_5_subset_2)
CI_2_3 = confint(fit_2_3,se='twoway')

#Cohen's D:
fit_2_3$coefficients[1]/sd(Control_Data$True_Dummy)


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
fig_2c <- ggplot(data = Fig_2c_Mat, aes(x = x, y = Coef)) +
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
        axis.text.x  = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.text.y  = element_text(size=8),
        plot.title = element_text(size = 8),
        legend.title = element_text(size=8),
        legend.text = element_text(size=8),
        title =element_text(size=8, face='bold')) +
  guides(fill=guide_legend(
    keywidth=0.2,
    keyheight=0.2,
    default.unit="inch")) +
  scale_x_continuous("Type of News Returned by \n Search Engine \n",breaks=c(1.5,0.9),labels=c('At Least 10%\nof News URLs\nAre Unreliable',
                                                                                                   'Only Very\nReliable\nSources'),limits=c(0.5,2.0)) +
  scale_y_continuous(breaks=c(-0.4,0.0,0.4),limits=c(-0.55,0.55)) +
  coord_flip()


Fig_2c_Mat$x <- NULL

write.csv(Fig_2c_Mat,'./source_data/fig_2c_source_data.csv')




################################################################################################################

########################################## Figure 2d ###########################################################

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

nrow(highest_quartile_all_data)


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
fig_2d <- ggplot(data = Fig_2d_Mat, aes(x = x, y = Coef)) +
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
        axis.text.x  = element_text(size=8),
        axis.title.y = element_text(size=8),
        axis.text.y  = element_text(size=8),
        plot.title = element_text(size = 8),
        legend.title = element_text(size=8),
        legend.text = element_text(size=8),
        title =element_text(size=8, face='bold')) +
  ylim(-0.5,0.7) +
  scale_x_continuous("Quartile of News Quality \n Returned by Search Engine \n",breaks=c(2.7,2.1,1.5,0.9),labels=c('75-100%',
                                                                                                                       '50-75%',
                                                                                                                       '25-50%',
                                                                                                                       '0-25%'),limits=c(0.5,3.0)) +
  scale_y_continuous(breaks=c(-0.3,0.0,0.3,0.6),limits=c(-0.5,0.7)) +
  coord_flip()



Fig_2d_Mat$x <- NULL

write.csv(Fig_2d_Mat,'./source_data/fig_2d_source_data.csv')





#Confirm the null hypothesis: 
library(BayesFactor)

bf = ttestBF(formula = True_Dummy ~ Treatment, data = highest_quartile_all_data)

bf



top_half <- rbind(highest_quartile_T_data,Third_lowest_quartile_T_data,Control_Data)

#Run OLS Model with clustered standard errors:
fit_2_3 = feols(True_Dummy ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=top_half)


#Confirm the null hypothesis: 
library(BayesFactor)

bf = ttestBF(formula = True_Dummy ~ Treatment, data = top_half)

bf





################################################################################################################

################################# Figure 3a ####################################################################

################################################################################################################

#Merge treatment data from study 5 and the search results data:

T_Data <- Study_5_treat_data %>% filter(FC_Eval == 'FM')

nrow(T_Data)

#Run OLS Model with clustered standard errors:

Prop_Dummy_results = feols(Unrel_contain ~ Age + Gender + Education_Score + Income_Score + Ideo_Congruence +Dig_Lit_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=T_Data)
CI_Prop_Dummy = confint(Prop_Dummy_results,se='twoway')


#Cohen's D (Dig. Lit.): 
(Prop_Dummy_results$coefficients[6]*sd(T_Data$Dig_Lit_Score))/sd(Study_4_df$Susc_FN)

#Cohen's D (Ideological Congruence): 
(Prop_Dummy_results$coefficients[5]*sd(T_Data$Ideo_Congruence))/sd(Study_4_df$Susc_FN)

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

#Produce plot:
fig_3a <- ggplot(data = d_matrix, aes(x = x, y = Coefficients)) +
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

write.csv(d_matrix,'./source_data/fig_3a_source_data.csv')


################################################################################################################

################################# Figure 3b ####################################################################

################################################################################################################


Headline_coding <- read.csv('./Data/Headline_Coding_4.csv')

Headline_coding <- na.omit(Headline_coding)


nrow(Headline_coding)

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
fig_3b <- ggplot(Matrix_Dist, aes(fill=Percentage, y=Proportion, x=Article_Rating)) + 
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



write.csv(Matrix_Dist,'./source_data/fig_3b_source_data.csv')


#Comparative Statistic:

#Run simple linear regression:
fit_lm_3b <- lm(Unrel_contain ~ Headline_Link,data=Headline_coding)

#Print output:
summary(fit_lm_3b)


################################################################################################################

################################# Figure 3c ####################################################################

################################################################################################################


#Pull-in search data data:
Headline_coding <- read.csv('./Data/Headline_Coding_4.csv')

nrow(Headline_coding)

#Remove searches without a news quality score:
Headline_coding <- na.omit(Headline_coding)

#Select variables needed:
Headline_coding <- Headline_coding %>% select(ResponseId,Article_day,Headline_Link,Age,Gender,Education_Score,Income_Score,Ideo_Congruence,Dig_Lit_Score)

#Run OLS Model with clustered standard errors:
Headline_results = feols(Headline_Link ~ Age + Gender + Education_Score + Income_Score + Ideo_Congruence +Dig_Lit_Score | Article_day, cluster = ~Article_day+ResponseId, se="twoway", data=Headline_coding)
CI_Headline = confint(Headline_results)

#Cohen's D (digital literacy):
Headline_results$coefficients[6]*sd(Headline_coding$Dig_Lit_Score)/sd(Headline_coding$Headline_Link)

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
fig_3c <- ggplot(data = d_matrix, aes(x = x, y = Coefficients)) +
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
write.csv(d_matrix,'./source_data/fig_3c_source_data.csv')


################################################################################################################

################################# Figure 4a ####################################################################

################################################################################################################

#Study 1:
#Read in data:
Study_1_df_T <- read.csv('./Data/Study_1_df_T.csv')

T_LQ_Data_Study_1 <- Study_1_df_T %>% filter(Article == "1" | Article == "2" | Article == "3")
T_Mainstream_Data_Study_1 <- Study_1_df_T %>% filter(Article == "4" | Article == "5")

All_T_Study_1 <- rbind(T_LQ_Data_Study_1,T_Mainstream_Data_Study_1)

All_T_Study_1 <- All_T_Study_1 %>% select(True_Dummy,Treat_Search,Age,Dummy_Congruence,Education_Score,Gender,Income_Score,Article_day,ResponseId)

All_T_Study_1 <- na.omit(All_T_Study_1)

nrow(All_T_Study_1)

#Run OLS Model with clustered standard errors:
lin_results_fit_1_1_1 = feols(True_Dummy ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = All_T_Study_1)
#Produce confidence intervals with clustered standard errors:
CI_1_1_1 <- confint(lin_results_fit_1_1_1,se='twoway')

#Cohen's D: 
lin_results_fit_1_1_1$coefficients[1]/sd(All_T_Study_1$True_Dummy,na.rm=T)


#Read in data (Study 1 - false/misleading articles):
Misl_False_Search_MF <- read.csv('./Data/Study_1_df_FM.csv')

Misl_False_Search_MF <- Misl_False_Search_MF %>% select(Susc_FN,Treat_Search,Age,Dummy_Congruence,Education_Score,Gender,Income_Score,Article_day,ResponseId)

Misl_False_Search_MF <- na.omit(Misl_False_Search_MF)

nrow(Misl_False_Search_MF)


#Run OLS Model with clustered standard errors:
lin_results_fit_1_3_1 = feols(Susc_FN ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Misl_False_Search_MF)
#Produce confidence intervals with clustered standard errors:
CI_1_3_1 <- confint(lin_results_fit_1_3_1,se='twoway')


#Read in data (Study 2 - true articles):
Data_Bef_Aft_T <- read.csv('./Data/Study_2_df_T.csv')

T_LQ_Data_Study_2 <- Data_Bef_Aft_T %>% filter(Article == "1" | Article == "2" | Article == "3")
T_Mainstream_Data_Study_2 <- Data_Bef_Aft_T %>% filter(Article == "4" | Article == "5")


All_T_Study_2 <- rbind(T_LQ_Data_Study_2,T_Mainstream_Data_Study_2)

All_T_Study_2 <- All_T_Study_2 %>% select(True_Dummy,Treat_Search,Age,Dummy_Congruence,Education_Score,Gender,Income_Score,Article_day,ResponseId)

All_T_Study_2 <- na.omit(All_T_Study_2)

nrow(All_T_Study_2)

#Run OLS Model with clustered standard errors:
lin_results_fit_2_1_1 = feols(True_Dummy ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = All_T_Study_2)
#Produce confidence intervals with clustered standard errors:
CI_2_1_1 <- confint(lin_results_fit_2_1_1,se='twoway')

#Cohen's D: 
lin_results_fit_2_1_1$coefficients[1]/sd(All_T_Study_2$True_Dummy,na.rm=T)


#Pull in this data: Study 2 - False/Misleading Articles:
Data_Bef_Aft_MF <- read.csv('./Data/Study_2_df_FM.csv')

Data_Bef_Aft_MF <- Data_Bef_Aft_MF %>% select(Susc_FN,Treat_Search,Age,Dummy_Congruence,Education_Score,Gender,Income_Score,Article_day,ResponseId)

Data_Bef_Aft_MF <- na.omit(Data_Bef_Aft_MF)

nrow(Data_Bef_Aft_MF)


#Run OLS Model with clustered standard errors:
lin_results_fit_2_3_1 = feols(Susc_FN ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Data_Bef_Aft_MF)
#Produce confidence intervals with clustered standard errors:
CI_2_3_1 <- confint(lin_results_fit_2_3_1,se='twoway')

#Study 3 (True Articles):
Study_3_df <- read.csv('./Data/Study_3_df_T.csv')

T_LQ_Data_Study_3 <- Study_3_df %>% filter(Article == "1" | Article == "2" | Article == "3")
T_Mainstream_Data_Study_3 <- Study_3_df %>% filter(Article == "4" | Article == "5")


All_T_Study_3 <- rbind(T_LQ_Data_Study_3,T_Mainstream_Data_Study_3)


All_T_Study_3 <- All_T_Study_3 %>% select(True_Dummy,Treatment,Age,Dummy_Congruence,Education_Score,Gender,Income_Score,Article_day,ResponseId)

All_T_Study_3 <- na.omit(All_T_Study_3)

nrow(All_T_Study_3)

#Run linear regression and produce coefficient values:
lin_results_fit_3_1_1 = feols(True_Dummy ~ Treatment + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = All_T_Study_3)
#Produce confidence intervals using clustered standard errors:
CI_3_1_1 <- confint(lin_results_fit_3_1_1,se='twoway')

#Cohen's D: 
lin_results_fit_3_1_1$coefficients[1]/sd(All_T_Study_3$True_Dummy,na.rm=T)

#Study 3 Analysis:
Study_3_False_M <- read.csv('./Data/Study_3_df_FM.csv')

Study_3_False_M <- Study_3_False_M %>% select(True_Dummy,Treatment,Age,Dummy_Congruence,Education_Score,Gender,Income_Score,Article_day,ResponseId)

Study_3_False_M <- na.omit(Study_3_False_M)

nrow(Study_3_False_M)


#Run linear regression and produce coefficient values:
lin_results_fit_3_3_1 = feols(True_Dummy ~ Treatment + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Study_3_False_M)
#Produce confidence intervals using clustered standard errors:
CI_3_3_1 <- confint(lin_results_fit_3_3_1,se='twoway')

#Study 4:
Data_Bef_Aft_Covid_T <- read.csv('./Data/Study_4_df_T.csv')

T_LQ_Data_Study_4 <- Data_Bef_Aft_Covid_T %>% filter(Article == "1" | Article == "2" | Article == "3")
T_Mainstream_Data_Study_4 <- Data_Bef_Aft_Covid_T %>% filter(Article == "4" | Article == "5")

All_T_Study_4 <- rbind(T_LQ_Data_Study_4,T_Mainstream_Data_Study_4)

All_T_Study_4 <- All_T_Study_4 %>% select(True_Dummy,Treat_Search,Age,Dummy_Congruence,Education_Score,Gender,Income_Score,Article_day,ResponseId)

All_T_Study_4 <- na.omit(All_T_Study_4)

nrow(All_T_Study_4)



#Confirm the null hypothesis: 
library(BayesFactor)

bf = ttestBF(formula = True_Dummy ~ Treat_Search, data = All_T_Study_4)

bf



#Run OLS Model with clustered standard errors:
lin_results_fit_4_1_1 = feols(True_Dummy ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = All_T_Study_4)
#Produce confidence intervals with clustered standard errors:
CI_4_1_1 <- confint(lin_results_fit_4_1_1,se='twoway')


#Cohen's D: 
lin_results_fit_4_1_1$coefficients[1]/sd(All_T_Study_4$True_Dummy,na.rm=T)


#Study 4 (False Misleading Articles)
Study_4_False_M <- read.csv('./Data/Study_4_df_FM.csv')

Study_4_False_M <- Study_4_False_M %>% select(Susc_FN,Treat_Search,Age,Dummy_Congruence,Education_Score,Gender,Income_Score,Article_day,ResponseId)

Study_4_False_M <- na.omit(Study_4_False_M)

nrow(Study_4_False_M)

#Run OLS Model with clustered standard errors:
lin_results_fit_4_3_1 = feols(Susc_FN ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Study_4_False_M)
#Produce confidence intervals with clustered standard errors:
CI_4_3_1 <- confint(lin_results_fit_4_3_1,se='twoway')


#Study 5 (True Articles):
T_Data_Study_5 <- read.csv('./Data/Study_5_df_T.csv')

T_LQ_Data_Study_5 <- T_Data_Study_5 %>% filter(Article_Num == 1 | Article_Num == 2 | Article_Num == 3)
T_Mainstream_Data_Study_5 <- T_Data_Study_5 %>% filter(Article_Num == 4 | Article_Num == 5)


All_T_Study_5 <- rbind(T_LQ_Data_Study_5,T_Mainstream_Data_Study_5)

All_T_Study_5 <- All_T_Study_5 %>% select(True_Dummy,Treatment,Age,Ideo_Congruence,Education_Score,Gender,Income_Score,Article_day,ResponseId)

All_T_Study_5 <- na.omit(All_T_Study_5)

nrow(All_T_Study_5)

#Run OLS Model with clustered standard errors:
lin_results_fit_5_1_1 = feols(True_Dummy ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = All_T_Study_5)
#Produce confidence intervals with clustered standard errors:
CI_5_1_1 <- confint(lin_results_fit_5_1_1,se='twoway')


#Cohen's D: 
lin_results_fit_5_1_1$coefficients[1]/sd(All_T_Study_5$True_Dummy,na.rm=T)


#Study 5 (False/Misleading Articles):
Study_5_False_M <- read.csv('./Data/Study_5_df_FM.csv')

Study_5_False_M <- Study_5_False_M %>% select(True_Dummy,Treatment,Age,Ideo_Congruence,Education_Score,Gender,Income_Score,Article_day,ResponseId)

Study_5_False_M <- na.omit(Study_5_False_M)

nrow(Study_5_False_M)



#Run OLS Model with clustered standard errors:
lin_results_fit_5_3_1 = feols(True_Dummy ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Study_5_False_M)
#Produce confidence intervals with clustered standard errors:
CI_5_3_1 <- confint(lin_results_fit_5_3_1,se='twoway')

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

P_Value <- c(round(lin_results_fit_1_1_1$coeftable[1,4],4),
             round(lin_results_fit_1_3_1$coeftable[1,4],4),
             round(lin_results_fit_2_1_1$coeftable[1,4],4),
             round(lin_results_fit_2_3_1$coeftable[1,4],4),
             round(lin_results_fit_3_1_1$coeftable[1,4],4),
             round(lin_results_fit_3_3_1$coeftable[1,4],4),
             round(lin_results_fit_4_1_1$coeftable[1,4],4),
             round(lin_results_fit_4_3_1$coeftable[1,4],4),
             round(lin_results_fit_5_1_1$coeftable[1,4],4),
             round(lin_results_fit_5_3_1$coeftable[1,4],4))

Standard_Error <- c(round(lin_results_fit_1_1_1$coeftable[1,2],4),
                    round(lin_results_fit_1_3_1$coeftable[1,2],4),
                    round(lin_results_fit_2_1_1$coeftable[1,2],4),
                    round(lin_results_fit_2_3_1$coeftable[1,2],4),
                    round(lin_results_fit_3_1_1$coeftable[1,2],4),
                    round(lin_results_fit_3_3_1$coeftable[1,2],4),
                    round(lin_results_fit_4_1_1$coeftable[1,2],4),
                    round(lin_results_fit_4_3_1$coeftable[1,2],4),
                    round(lin_results_fit_5_1_1$coeftable[1,2],4),
                    round(lin_results_fit_5_3_1$coeftable[1,2],4))

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

d_matrix_1 <- d_matrix

#Produce figure:
fig_4a <- ggplot(data = d_matrix_1, aes(x = x, y = Coefficients)) +
  geom_hline(aes(yintercept = 0), color = "gray",
             linetype = 2, size = 0.75) +
  geom_point(aes(color = Study, shape=Study),size=1.5) +
  geom_linerange(aes(min = CI_Lower, 
                     max = CI_Upper, 
                     color = Study),
                 size=0.75) +
  scale_color_manual(values=cbbPalette_2, name = "Type of News") +
  scale_shape_manual(values=c(15,16,17),"Type of News") +
  ylab("\n The Effect of Searching Online\n on Probability of Rating News as True") +
  theme_classic() +
  theme(axis.title.x = element_text(size=8),
        axis.text.x  = element_text(size=6),
        axis.title.y = element_text(size=8),
        axis.text.y  = element_text(size=6),
        plot.title = element_text(size = 6),
        legend.title = element_text(size=4),
        legend.text = element_text(size=4),
        title =element_text(size=6, face='bold')) +
  ylim(-0.1,0.2) +
  scale_x_continuous("Study Number\n",breaks=c(1,2,3,4,5),labels=rev(c('Study 1',
                                                                       'Study 2',
                                                                       'Study 3',
                                                                       'Study 4',
                                                                       'Study 5')),limits=c(0.2,5.8)) +
  coord_flip()









d_matrix$x <- NULL

write.csv(d_matrix,'./source_data/fig_4a_source_data.csv')


################################################################################################################

################################# Figure 4b ####################################################################

################################################################################################################

############## 
############## Study 1:
############## 

T_LQ_Data_Study_1 <- T_LQ_Data_Study_1 %>% select(True_Dummy,Treat_Search,Age,Dummy_Congruence,Education_Score,Gender,Income_Score,Article_day,ResponseId)

T_LQ_Data_Study_1 <- na.omit(T_LQ_Data_Study_1)

nrow(T_LQ_Data_Study_1)

T_Mainstream_Data_Study_1 <- T_Mainstream_Data_Study_1 %>% select(True_Dummy,Treat_Search,Age,Dummy_Congruence,Education_Score,Gender,Income_Score,Article_day,ResponseId)

T_Mainstream_Data_Study_1 <- na.omit(T_Mainstream_Data_Study_1)

nrow(T_Mainstream_Data_Study_1)



#Run OLS Model with clustered standard errors:
lin_results_fit_1_1_1 = feols(True_Dummy ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = T_LQ_Data_Study_1)
#Produce confidence intervals with clustered standard errors:
CI_1_1_1 <- confint(lin_results_fit_1_1_1,se='twoway')

#Run OLS Model with clustered standard errors:
lin_results_fit_1_2_1 = feols(True_Dummy ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = T_Mainstream_Data_Study_1)
#Produce confidence intervals with clustered standard errors:
CI_1_2_1 <- confint(lin_results_fit_1_2_1,se='twoway')


#Run OLS Model with clustered standard errors:
lin_results_fit_1_3_1 = feols(Susc_FN ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Misl_False_Search_MF)
#Produce confidence intervals with clustered standard errors:
CI_1_3_1 <- confint(lin_results_fit_1_3_1,se='twoway')


############## 
############## Study 2:
############## 

T_LQ_Data_Study_2 <- T_LQ_Data_Study_2 %>% select(True_Dummy,Treat_Search,Age,Dummy_Congruence,Education_Score,Gender,Income_Score,Article_day,ResponseId)

T_LQ_Data_Study_2 <- na.omit(T_LQ_Data_Study_2)

nrow(T_LQ_Data_Study_2)

T_Mainstream_Data_Study_2 <- T_Mainstream_Data_Study_2 %>% select(True_Dummy,Treat_Search,Age,Dummy_Congruence,Education_Score,Gender,Income_Score,Article_day,ResponseId)

T_Mainstream_Data_Study_2 <- na.omit(T_Mainstream_Data_Study_2)

nrow(T_Mainstream_Data_Study_2)

#Run OLS Model with clustered standard errors:
lin_results_fit_2_1_1 = feols(True_Dummy ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = T_LQ_Data_Study_2)
#Produce confidence intervals with clustered standard errors:
CI_2_1_1 <- confint(lin_results_fit_2_1_1,se='twoway')

#Run OLS Model with clustered standard errors:
lin_results_fit_2_2_1 = feols(True_Dummy ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = T_Mainstream_Data_Study_2)
#Produce confidence intervals with clustered standard errors:
CI_2_2_1 <- confint(lin_results_fit_2_2_1,se='twoway')

#Run OLS Model with clustered standard errors:
lin_results_fit_2_3_1 = feols(Susc_FN ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Data_Bef_Aft_MF)
#Produce confidence intervals with clustered standard errors:
CI_2_3_1 <- confint(lin_results_fit_2_3_1,se='twoway')

############## 
############## Study 3:
############## 

T_LQ_Data_Study_3 <- T_LQ_Data_Study_3 %>% select(True_Dummy,Treatment,Age,Dummy_Congruence,Education_Score,Gender,Income_Score,Article_day,ResponseId)

T_LQ_Data_Study_3 <- na.omit(T_LQ_Data_Study_3)

nrow(T_LQ_Data_Study_3)

T_Mainstream_Data_Study_3 <- T_Mainstream_Data_Study_3 %>% select(True_Dummy,Treatment,Age,Dummy_Congruence,Education_Score,Gender,Income_Score,Article_day,ResponseId)

T_Mainstream_Data_Study_3 <- na.omit(T_Mainstream_Data_Study_3)

nrow(T_Mainstream_Data_Study_3)

#Run linear regression and produce coefficient values:
lin_results_fit_3_1_1 = feols(True_Dummy ~ Treatment + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = T_LQ_Data_Study_3)
#Produce confidence intervals using clustered standard errors:
CI_3_1_1 <- confint(lin_results_fit_3_1_1,se='twoway')

#Run linear regression and produce coefficient values:
lin_results_fit_3_2_1 = feols(True_Dummy ~ Treatment + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = T_Mainstream_Data_Study_3)
#Produce confidence intervals using clustered standard errors:
CI_3_2_1 <- confint(lin_results_fit_3_2_1,se='twoway')


#Confirm the null hypothesis: 
library(BayesFactor)

bf = ttestBF(formula = True_Dummy ~ Treatment, data = T_Mainstream_Data_Study_3)

bf



#Run linear regression and produce coefficient values:
lin_results_fit_3_3_1 = feols(Susc_FN ~ Treatment + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Study_3_df)
#Produce confidence intervals using clustered standard errors:
CI_3_3_1 <- confint(lin_results_fit_3_3_1,se='twoway')




############## 
############## Study 4:
############## 

T_LQ_Data_Study_4 <- T_LQ_Data_Study_4 %>% select(True_Dummy,Treat_Search,Age,Dummy_Congruence,Education_Score,Gender,Income_Score,Article_day,ResponseId)

T_LQ_Data_Study_4 <- na.omit(T_LQ_Data_Study_4)

nrow(T_LQ_Data_Study_4)

T_Mainstream_Data_Study_4 <- T_Mainstream_Data_Study_4 %>% select(True_Dummy,Treat_Search,Age,Dummy_Congruence,Education_Score,Gender,Income_Score,Article_day,ResponseId)

T_Mainstream_Data_Study_4 <- na.omit(T_Mainstream_Data_Study_4)

nrow(T_Mainstream_Data_Study_4)


#Run OLS Model with clustered standard errors:
lin_results_fit_4_1_1 = feols(True_Dummy ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = T_LQ_Data_Study_4)
#Produce confidence intervals with clustered standard errors:
CI_4_1_1 <- confint(lin_results_fit_4_1_1,se='twoway')


#Run OLS Model with clustered standard errors:
lin_results_fit_4_2_1 = feols(True_Dummy ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = T_Mainstream_Data_Study_4)
#Produce confidence intervals with clustered standard errors:
CI_4_2_1 <- confint(lin_results_fit_4_2_1,se='twoway')



#Confirm the null hypothesis: 
library(BayesFactor)

bf = ttestBF(formula = True_Dummy ~ Treat_Search, data = T_Mainstream_Data_Study_4)

bf


#Run OLS Model with clustered standard errors:
lin_results_fit_4_3_1 = feols(Susc_FN ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Study_4_False_M)
#Produce confidence intervals with clustered standard errors:
CI_4_3_1 <- confint(lin_results_fit_4_3_1,se='twoway')


############## 
############## Study 5:
############## 

T_LQ_Data_Study_5 <- T_LQ_Data_Study_5 %>% select(True_Dummy,Treatment,Age,Ideo_Congruence,Education_Score,Gender,Income_Score,Article_day,ResponseId)

T_LQ_Data_Study_5 <- na.omit(T_LQ_Data_Study_5)

nrow(T_LQ_Data_Study_5)

T_Mainstream_Data_Study_5 <- T_Mainstream_Data_Study_5 %>% select(True_Dummy,Treatment,Age,Ideo_Congruence,Education_Score,Gender,Income_Score,Article_day,ResponseId)

T_Mainstream_Data_Study_5 <- na.omit(T_Mainstream_Data_Study_5)

nrow(T_Mainstream_Data_Study_5)


#Run OLS Model with clustered standard errors:
lin_results_fit_5_1_1 = feols(True_Dummy ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = T_LQ_Data_Study_5)
#Produce confidence intervals with clustered standard errors:
CI_5_1_1 <- confint(lin_results_fit_5_1_1,se='twoway')

#Run OLS Model with clustered standard errors:
lin_results_fit_5_2_1 = feols(True_Dummy ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = T_Mainstream_Data_Study_5)
#Produce confidence intervals with clustered standard errors:
CI_5_2_1 <- confint(lin_results_fit_5_2_1,se='twoway')

#Run OLS Model with clustered standard errors:
lin_results_fit_5_3_1 = feols(True_Dummy ~ Treatment + Age + Ideo_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Study_5_False_M)
#Produce confidence intervals with clustered standard errors:
CI_5_3_1 <- confint(lin_results_fit_5_3_1,se='twoway')




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


P_Value <- c(round(lin_results_fit_1_1_1$coeftable[1,4],4),
             round(lin_results_fit_1_2_1$coeftable[1,4],4),
             round(lin_results_fit_1_3_1$coeftable[1,4],4),
             round(lin_results_fit_2_1_1$coeftable[1,4],4),
             round(lin_results_fit_2_2_1$coeftable[1,4],4),
             round(lin_results_fit_2_3_1$coeftable[1,4],4),
             round(lin_results_fit_3_1_1$coeftable[1,4],4),
             round(lin_results_fit_3_2_1$coeftable[1,4],4),
             round(lin_results_fit_3_3_1$coeftable[1,4],4),
             round(lin_results_fit_4_1_1$coeftable[1,4],4),
             round(lin_results_fit_4_2_1$coeftable[1,4],4),
             round(lin_results_fit_4_3_1$coeftable[1,4],4),
             round(lin_results_fit_5_1_1$coeftable[1,4],4),
             round(lin_results_fit_5_2_1$coeftable[1,4],4),
             round(lin_results_fit_5_3_1$coeftable[1,4],4))

Standard_Error <- c(round(lin_results_fit_1_1_1$coeftable[1,2],4),
                    round(lin_results_fit_1_2_1$coeftable[1,2],4),
                    round(lin_results_fit_1_3_1$coeftable[1,2],4),
                    round(lin_results_fit_2_1_1$coeftable[1,2],4),
                    round(lin_results_fit_2_2_1$coeftable[1,2],4),
                    round(lin_results_fit_2_3_1$coeftable[1,2],4),
                    round(lin_results_fit_3_1_1$coeftable[1,2],4),
                    round(lin_results_fit_3_2_1$coeftable[1,2],4),
                    round(lin_results_fit_3_3_1$coeftable[1,2],4),
                    round(lin_results_fit_4_1_1$coeftable[1,2],4),
                    round(lin_results_fit_4_2_1$coeftable[1,2],4),
                    round(lin_results_fit_4_3_1$coeftable[1,2],4),
                    round(lin_results_fit_5_1_1$coeftable[1,2],4),
                    round(lin_results_fit_5_2_1$coeftable[1,2],4),
                    round(lin_results_fit_5_3_1$coeftable[1,2],4))

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

#Produce figure:
d_matrix_2 <- d_matrix

fig_4b <- ggplot(data = d_matrix_2, aes(x = x, y = Coefficients)) +
  geom_hline(aes(yintercept = 0), color = "gray",
             linetype = 2, size = 0.75) +
  geom_point(aes(color = Study, shape=Study),size=1.5) +
  geom_linerange(aes(min = CI_Lower, 
                     max = CI_Upper, 
                     color = Study),
                 size=0.75) +
  scale_color_manual(values=cbbPalette_2, name = "Type of News") +
  scale_shape_manual(values=c(15,16,17),"Type of News") +
  ylab("\n The Effect of Searching Online\n on Probability of Rating News as True") +
  theme_classic() +
  theme(axis.title.x = element_text(size=8),
        axis.text.x  = element_text(size=6),
        axis.title.y = element_text(size=8),
        axis.text.y  = element_text(size=6),
        plot.title = element_text(size = 6),
        legend.title = element_text(size=4),
        legend.text = element_text(size=4),
        title =element_text(size=6, face='bold')) +
  ylim(-0.1,0.3) +
  scale_x_continuous("Study Number\n",breaks=c(1,2,3,4,5),labels=rev(c('Study 1',
                                                                       'Study 2',
                                                                       'Study 3',
                                                                       'Study 4',
                                                                       'Study 5')),limits=c(0.2,5.8)) +
  coord_flip()


d_matrix$x <- NULL

write.csv(d_matrix,'./source_data/fig_4b_source_data.csv')

################################################################################################################

################################# Figure 4c ####################################################################

################################################################################################################

#Between-Respondent Experiment True Low Quality Articles:

T_LQ_Data_Study_1 <- T_LQ_Data_Study_1 %>% select(True_Dummy,Treat_Search,Age,Dummy_Congruence,Education_Score,Gender,Income_Score,Article_day,Article_day,ResponseId)
T_LQ_Data_Study_5 <- T_LQ_Data_Study_5 %>% select(True_Dummy,Treatment,Age,Ideo_Congruence,Education_Score,Gender,Income_Score,Article_day,Article_day,ResponseId)

T_LQ_Data_Study_5$Treat_Search <- T_LQ_Data_Study_5$Treatment
T_LQ_Data_Study_5$Dummy_Congruence <- T_LQ_Data_Study_5$Ideo_Congruence

T_LQ_Data_Study_5$Treatment <- NULL
T_LQ_Data_Study_5$Ideo_Congruence <- NULL


btwn_T_LQ_studies <- rbind(T_LQ_Data_Study_1,T_LQ_Data_Study_5)

btwn_T_LQ_studies <- na.omit(btwn_T_LQ_studies)

nrow(btwn_T_LQ_studies)


#Run OLS Model with clustered standard errors:
btwn_T_LQ_results_1 = feols(True_Dummy ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = btwn_T_LQ_studies)
#Produce confidence intervals with clustered standard errors:
CI_btwn_T_LQ_1 <- confint(btwn_T_LQ_results_1,se='twoway')


#Within-Respondent Experiment True Low Quality Articles:

T_LQ_Data_Study_3$Treat_Search <- T_LQ_Data_Study_3$Treatment
T_LQ_Data_Study_3$Treatment <- NULL

T_LQ_Data_Study_2 <- T_LQ_Data_Study_2 %>% select(True_Dummy,Treat_Search,Age,Dummy_Congruence,Education_Score,Gender,Income_Score,Article_day,ResponseId)
T_LQ_Data_Study_3 <- T_LQ_Data_Study_3 %>% select(True_Dummy,Treat_Search,Age,Dummy_Congruence,Education_Score,Gender,Income_Score,Article_day,ResponseId)
T_LQ_Data_Study_4 <- T_LQ_Data_Study_4 %>% select(True_Dummy,Treat_Search,Age,Dummy_Congruence,Education_Score,Gender,Income_Score,Article_day,ResponseId)

wthn_T_LQ_studies <- rbind(T_LQ_Data_Study_2,T_LQ_Data_Study_3,T_LQ_Data_Study_4)

wthn_T_LQ_studies <- na.omit(wthn_T_LQ_studies)

nrow(wthn_T_LQ_studies)

#Run OLS Model with clustered standard errors:
wthn_T_LQ_results_1 = feols(True_Dummy ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = wthn_T_LQ_studies)
#Produce confidence intervals with clustered standard errors:
CI_wthn_T_LQ_1 <- confint(wthn_T_LQ_results_1,se='twoway')


#Between-Respondent Experiment True Mainstream Articles:

T_Mainstream_Data_Study_1 <- T_Mainstream_Data_Study_1 %>% select(True_Dummy,Treat_Search,Age,Dummy_Congruence,Education_Score,Gender,Income_Score,Article_day,Article_day,ResponseId)
T_Mainstream_Data_Study_5 <- T_Mainstream_Data_Study_5 %>% select(True_Dummy,Treatment,Age,Ideo_Congruence,Education_Score,Gender,Income_Score,Article_day,Article_day,ResponseId)

T_Mainstream_Data_Study_5$Treat_Search <- T_Mainstream_Data_Study_5$Treatment
T_Mainstream_Data_Study_5$Dummy_Congruence <- T_Mainstream_Data_Study_5$Ideo_Congruence

T_Mainstream_Data_Study_5$Treatment <- NULL
T_Mainstream_Data_Study_5$Ideo_Congruence <- NULL

btwn_T_M_studies <- rbind(T_Mainstream_Data_Study_1,T_Mainstream_Data_Study_5)

btwn_T_M_studies <- na.omit(btwn_T_M_studies)

nrow(btwn_T_M_studies)

#Run OLS Model with clustered standard errors:
btwn_T_M_results_1 = feols(True_Dummy ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = btwn_T_M_studies)
#Produce confidence intervals with clustered standard errors:
CI_btwn_T_M_1 <- confint(btwn_T_M_results_1,se='twoway')

#Within-Respondent Experiment True Mainstream Articles:

T_Mainstream_Data_Study_3$Treat_Search <- T_Mainstream_Data_Study_3$Treatment
T_Mainstream_Data_Study_3$Treatment <- NULL

T_Mainstream_Data_Study_2 <- T_Mainstream_Data_Study_2 %>% select(True_Dummy,Treat_Search,Age,Dummy_Congruence,Education_Score,Gender,Income_Score,Article_day,ResponseId)
T_Mainstream_Data_Study_3 <- T_Mainstream_Data_Study_3 %>% select(True_Dummy,Treat_Search,Age,Dummy_Congruence,Education_Score,Gender,Income_Score,Article_day,ResponseId)
T_Mainstream_Data_Study_4 <- T_Mainstream_Data_Study_4 %>% select(True_Dummy,Treat_Search,Age,Dummy_Congruence,Education_Score,Gender,Income_Score,Article_day,ResponseId)

wthn_T_M_studies <- rbind(T_Mainstream_Data_Study_2,T_Mainstream_Data_Study_3,T_Mainstream_Data_Study_4)

wthn_T_M_studies <- na.omit(wthn_T_M_studies)

nrow(wthn_T_M_studies)


#Run OLS Model with clustered standard errors:
wthn_T_M_results_1 = feols(True_Dummy ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = wthn_T_M_studies)
#Produce confidence intervals with clustered standard errors:
CI_wthn_T_M_1 <- confint(wthn_T_M_results_1,se='twoway')


#Within-Respondent Experiment False/Misleading Articles:

Study_3_False_M$Treat_Search <- Study_3_False_M$Treatment


Data_Bef_Aft_MF <- Data_Bef_Aft_MF %>% select(Susc_FN,Treat_Search,Age,Dummy_Congruence,Education_Score,Gender,Income_Score,Article_day,ResponseId)
Study_3_False_M <- Study_3_False_M %>% select(True_Dummy,Treat_Search,Age,Dummy_Congruence,Education_Score,Gender,Income_Score,Article_day,ResponseId)
Study_4_False_M <- Study_4_False_M %>% select(Susc_FN,Treat_Search,Age,Dummy_Congruence,Education_Score,Gender,Income_Score,Article_day,ResponseId)

colnames(Data_Bef_Aft_MF)[1] <- 'True_Dummy'
colnames(Study_4_False_M)[1] <- 'True_Dummy'

Within_FM_Study <- rbind(Data_Bef_Aft_MF,Study_3_False_M,Study_4_False_M)

Within_FM_Study <- na.omit(Within_FM_Study)

nrow(Within_FM_Study)


#Run OLS Model with clustered standard errors:
wthn_FM_results_1 = feols(True_Dummy ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Within_FM_Study)
#Produce confidence intervals with clustered standard errors:
CI_wthn_FM_1 <- confint(wthn_FM_results_1,se='twoway')


#Between-Respondent Experiment False/Misleading Articles:

Misl_False_Search_MF <- Misl_False_Search_MF %>% select(Susc_FN,Treat_Search,Age,Dummy_Congruence,Education_Score,Gender,Income_Score,Article_day,Article_day,ResponseId)
Study_5_False_M <- Study_5_False_M %>% select(True_Dummy,Treatment,Age,Ideo_Congruence,Education_Score,Gender,Income_Score,Article_day,Article_day,ResponseId)

colnames(Misl_False_Search_MF)[1] <- 'True_Dummy'

Study_5_False_M$Likert_Evaluation <- Study_5_False_M$Seven_Ordinal
Study_5_False_M$Treat_Search <- Study_5_False_M$Treatment
Study_5_False_M$Dummy_Congruence <- Study_5_False_M$Ideo_Congruence

Study_5_False_M$Seven_Ordinal <- NULL
Study_5_False_M$Treatment <- NULL
Study_5_False_M$Ideo_Congruence <- NULL

Between_FM_Study <- rbind(Misl_False_Search_MF,Study_5_False_M)

Between_FM_Study <- na.omit(Between_FM_Study)

nrow(Between_FM_Study)

#Run OLS Model with clustered standard errors:
btwn_FM_results_1 = feols(True_Dummy ~ Treat_Search + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score | Article_day, cluster = ~Article_day+ResponseId,se="twoway", data = Between_FM_Study)
#Produce confidence intervals with clustered standard errors:
CI_btwn_FM_1 <- confint(btwn_FM_results_1,se='twoway')


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


P_Value <- c(round(btwn_T_LQ_results_1$coeftable[1,4],4),
             round(btwn_T_M_results_1$coeftable[1,4],4),
             round(btwn_FM_results_1$coeftable[1,4],4),
             round(wthn_T_LQ_results_1$coeftable[1,4],4),
             round(wthn_T_M_results_1$coeftable[1,4],4),
             round(wthn_FM_results_1$coeftable[1,4],4))

Standard_Error <- c(round(btwn_T_LQ_results_1$coeftable[1,2],4),
                    round(btwn_T_M_results_1$coeftable[1,2],4),
                    round(btwn_FM_results_1$coeftable[1,2],4),
                    round(wthn_T_LQ_results_1$coeftable[1,2],4),
                    round(wthn_T_M_results_1$coeftable[1,2],4),
                    round(wthn_FM_results_1$coeftable[1,2],4))

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

d_matrix$x <- NULL
write.csv(d_matrix,'./source_data/fig_4c_source_data.csv')

#Produce figure:
d_matrix_3 <- d_matrix

fig_4c <- ggplot(data = d_matrix_3, aes(x = x, y = Coefficients)) +
  geom_hline(aes(yintercept = 0), color = "gray",
             linetype = 2, size = 0.75) +
  geom_point(aes(color = Study, shape=Study),size=1.5) +
  geom_linerange(aes(min = CI_Lower, 
                     max = CI_Upper, 
                     color = Study),
                 size=0.75) +
  scale_color_manual(values=cbbPalette_2, name = "Type of News") +
  scale_shape_manual(values=c(15,16,17),"Type of News") +
  ylab("\n The Effect of Searching Online\n on Probability of Rating News as True") +
  theme_classic() +
  theme(axis.title.x = element_text(size=8),
        axis.text.x  = element_text(size=6),
        axis.title.y = element_text(size=8),
        axis.text.y  = element_text(size=6),
        plot.title = element_text(size = 6),
        legend.title = element_text(size=4),
        legend.text = element_text(size=4),
        title =element_text(size=6, face='bold')) +
  ylim(-0.1,0.25) +
  scale_x_continuous("Study Type\n",breaks=c(1,2),labels=rev(c('Between',
                                                               'Within')),limits=c(0,3)) +
  coord_flip()




########## Comparative Statistics ##############

wthn_T_M_studies$Mainstream <- 1
wthn_T_LQ_studies$Mainstream <- 0


wthn_T_stud <- rbind(wthn_T_M_studies,wthn_T_LQ_studies)


wthn_T_stud_results_1 = feols(True_Dummy ~ Treat_Search*Mainstream + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score, data = wthn_T_stud)


btwn_T_M_studies$Mainstream <- 1
btwn_T_LQ_studies$Mainstream <- 0


btwn_T_stud <- rbind(btwn_T_M_studies,btwn_T_LQ_studies)


btwn_T_stud_results_1 = feols(True_Dummy ~ Treat_Search*Mainstream + Age + Dummy_Congruence + Education_Score +  Gender + Income_Score, data = btwn_T_stud)


########################### Source Data ##############################

library(tidyverse)
library(readr)

######################################################################

############################ Figure 1 ################################

######################################################################

#Read in data:
fig_1a_sd <- read_csv("./source_data/fig_1a_source_data.csv")
fig_1b_sd <- read_csv("./source_data/fig_1b_source_data.csv")

#Rename first column:
colnames(fig_1a_sd)[1] <- 'X'
colnames(fig_1b_sd)[1] <- 'X'

#Remove unnecessary column:
fig_1a_sd$X <- NULL
fig_1b_sd$X <- NULL

#Remove unnecessary column:
fig_1a_sd$Panel <- 'A'
fig_1b_sd$Panel <- 'B'

colnames(fig_1a_sd)
colnames(fig_1b_sd)

fig_1a_sd <- fig_1a_sd %>% select(Panel,Study,Coefficients,CI_Upper,CI_Lower,P_Value,Standard_Error)
fig_1b_sd <- fig_1b_sd %>% select(Panel,Study,Coefficients,CI_Upper,CI_Lower,P_Value,Standard_Error)

fig_1_sd <- rbind(fig_1a_sd,fig_1b_sd )

write_csv(fig_1_sd,'./source_data/fig_1_source_data.csv')


######################################################################

############################ Figure 2 ################################

######################################################################

#Read in data:
fig_2a_sd <- read_csv("./source_data/fig_2a_source_data.csv")
fig_2b_sd <- read_csv("./source_data/fig_2b_source_data.csv")
fig_2c_sd <- read_csv("./source_data/fig_2c_source_data.csv")
fig_2d_sd <- read_csv("./source_data/fig_2d_source_data.csv")

#Rename first column:
colnames(fig_2a_sd)[1] <- 'X'
colnames(fig_2b_sd)[1] <- 'X'
colnames(fig_2c_sd)[1] <- 'X'
colnames(fig_2d_sd)[1] <- 'X'

#Remove unnecessary column:
fig_2a_sd$X <- NULL
fig_2b_sd$X <- NULL
fig_2c_sd$X <- NULL
fig_2d_sd$X <- NULL

#Remove unnecessary column:
fig_2a_sd$Panel <- 'A'
fig_2b_sd$Panel <- 'B'
fig_2c_sd$Panel <- 'C'
fig_2d_sd$Panel <- 'D'

#Add columns that panel A have
fig_2b_sd$Proportion <- NA
fig_2b_sd$Number_low_quality_links <- NA
fig_2b_sd$Article_Rating <- NA

fig_2c_sd$Proportion <- NA
fig_2c_sd$Number_low_quality_links <- NA
fig_2c_sd$Article_Rating <- NA

fig_2d_sd$Proportion <- NA
fig_2d_sd$Number_low_quality_links <- NA
fig_2d_sd$Article_Rating <- NA


#Change column names from panels c and d to match panel b:

colnames(fig_2b_sd) <- c('DV_Measure','Effect','CI_Upper','CI_Lower','P_Value','Standard_Error','Panel','Proportion','Number_low_quality_links','Article_Rating')
colnames(fig_2c_sd) <- c('Effect','CI_Lower','CI_Upper','DV_Measure','Type_News','P_Value','Standard_Error','Panel','Proportion','Number_low_quality_links','Article_Rating')
colnames(fig_2d_sd) <- c('Effect','CI_Lower','CI_Upper','DV_Measure','Type_News','P_Value','Standard_Error','Panel','Proportion','Number_low_quality_links','Article_Rating')

#Add columns that panel C and D have:
fig_2b_sd$Type_News <- NA

#Add columns that panel B, C and D have:
fig_2a_sd$DV_Measure <- NA
fig_2a_sd$Effect <- NA
fig_2a_sd$CI_Upper <- NA
fig_2a_sd$CI_Lower <- NA
fig_2a_sd$P_Value <- NA
fig_2a_sd$Standard_Error <- NA
fig_2a_sd$DV_Measure <- NA
fig_2a_sd$Type_News <- NA

#Order columns: 

fig_2a_sd <- fig_2a_sd %>% select(Panel,DV_Measure,Type_News,Effect,CI_Upper,CI_Lower,P_Value,Standard_Error,Proportion,Number_low_quality_links,Article_Rating)
fig_2b_sd <- fig_2b_sd %>% select(Panel,DV_Measure,Type_News,Effect,CI_Upper,CI_Lower,P_Value,Standard_Error,Proportion,Number_low_quality_links,Article_Rating)
fig_2c_sd <- fig_2c_sd %>% select(Panel,DV_Measure,Type_News,Effect,CI_Upper,CI_Lower,P_Value,Standard_Error,Proportion,Number_low_quality_links,Article_Rating)
fig_2d_sd <- fig_2d_sd %>% select(Panel,DV_Measure,Type_News,Effect,CI_Upper,CI_Lower,P_Value,Standard_Error,Proportion,Number_low_quality_links,Article_Rating)

fig_2_sd <- rbind(fig_2a_sd,fig_2b_sd)
fig_2_sd <- rbind(fig_2_sd,fig_2c_sd)
fig_2_sd <- rbind(fig_2_sd,fig_2d_sd)

write_csv(fig_2_sd,'./source_data/fig_2_source_data.csv')

######################################################################

############################ Figure 3 ################################

######################################################################

#Read in data:
fig_3a_sd <- read_csv("./source_data/fig_3a_source_data.csv")
fig_3b_sd <- read_csv("./source_data/fig_3b_source_data.csv")
fig_3c_sd <- read_csv("./source_data/fig_3c_source_data.csv")

#Rename first column:
colnames(fig_3a_sd)[1] <- 'X'
colnames(fig_3b_sd)[1] <- 'X'
colnames(fig_3c_sd)[1] <- 'X'

#Remove unnecessary column:
fig_3a_sd$X <- NULL
fig_3b_sd$X <- NULL
fig_3c_sd$X <- NULL

#Remove unnecessary column:
fig_3a_sd$Panel <- 'A'
fig_3b_sd$Panel <- 'B'
fig_3c_sd$Panel <- 'C'

#Change columns names
colnames(fig_3a_sd) <- c('Variable_Name','Effect','CI_Upper','CI_Lower','P_Value','Standard_Error','Panel')
colnames(fig_3b_sd) <- c('Proportion','Number_low_quality_links','Article_Rating','Panel')
colnames(fig_3c_sd) <- c('Variable_Name','Effect','CI_Upper','CI_Lower','P_Value','Standard_Error','Panel')


#Add columns that panel B have:
fig_3a_sd$Proportion <- NA
fig_3a_sd$Number_low_quality_links <- NA
fig_3a_sd$Article_Rating <- NA

fig_3c_sd$Proportion <- NA
fig_3c_sd$Number_low_quality_links <- NA
fig_3c_sd$Article_Rating <- NA

#Add columns that panels A and C have:
fig_3b_sd$Variable_Name <- NA
fig_3b_sd$Effect <- NA
fig_3b_sd$CI_Upper <- NA
fig_3b_sd$CI_Lower <- NA
fig_3b_sd$P_Value <- NA
fig_3b_sd$Standard_Error <- NA

#Order columns: 
fig_3a_sd <- fig_3a_sd %>% select(Panel,Variable_Name,Effect,CI_Upper,CI_Lower,P_Value,Standard_Error,Proportion,Number_low_quality_links,Article_Rating)
fig_3b_sd <- fig_3b_sd %>% select(Panel,Variable_Name,Effect,CI_Upper,CI_Lower,P_Value,Standard_Error,Proportion,Number_low_quality_links,Article_Rating)
fig_3c_sd <- fig_3c_sd %>% select(Panel,Variable_Name,Effect,CI_Upper,CI_Lower,P_Value,Standard_Error,Proportion,Number_low_quality_links,Article_Rating)

fig_3_sd <- rbind(fig_3a_sd,fig_3b_sd)
fig_3_sd <- rbind(fig_3_sd,fig_3c_sd)

write_csv(fig_3_sd,'./source_data/fig_3_source_data.csv')


######################################################################

############################ Figure 4 ################################

######################################################################


#Read in data:
fig_4a_sd <- read_csv("./source_data/fig_4a_source_data.csv")
fig_4b_sd <- read_csv("./source_data/fig_4b_source_data.csv")
fig_4c_sd <- read_csv("./source_data/fig_4c_source_data.csv")

#Rename first column:
colnames(fig_4a_sd)[1] <- 'X'
colnames(fig_4b_sd)[1] <- 'X'
colnames(fig_4c_sd)[1] <- 'X'

#Remove unnecessary column:
fig_4a_sd$X <- NULL
fig_4b_sd$X <- NULL
fig_4c_sd$X <- NULL

#Remove unnecessary column:
fig_4a_sd$Panel <- 'A'
fig_4b_sd$Panel <- 'B'
fig_4c_sd$Panel <- 'C'

#Rename columns:
colnames(fig_4a_sd) <- c('Article_Type','Study','Effect','CI_Upper','CI_Lower','P_Value','Standard_Error','Panel')
colnames(fig_4b_sd) <- c('Article_Type','Study','Effect','CI_Upper','CI_Lower','P_Value','Standard_Error','Panel')
colnames(fig_4c_sd) <- c('Article_Type','Study','Effect','CI_Upper','CI_Lower','P_Value','Standard_Error','Panel')

#Reorder columns:
fig_4a_sd <- fig_4a_sd %>% select(Panel,Article_Type,Study,Effect,CI_Upper,CI_Lower,P_Value,Standard_Error)
fig_4b_sd <- fig_4b_sd %>% select(Panel,Article_Type,Study,Effect,CI_Upper,CI_Lower,P_Value,Standard_Error)
fig_4c_sd <- fig_4c_sd %>% select(Panel,Article_Type,Study,Effect,CI_Upper,CI_Lower,P_Value,Standard_Error)

fig_4_sd <- rbind(fig_4a_sd,fig_4b_sd)
fig_4_sd <- rbind(fig_4_sd,fig_4c_sd)

write_csv(fig_4_sd,'./source_data/fig_4_source_data.csv')


#################### Write Figures ####################################


####################### Figure 1 ######################################

#Load library:
library("ggpubr")

ggarrange(fig_1a, fig_1b,
          labels="auto",
          ncol = 2,
          font.label=list(color="black",size=8))

ggsave('./Figures/fig_1.eps',height=8,width=18,units='cm',device="eps")


####################### Figure 2 ######################################

ggarrange(fig_2a, fig_2b, fig_2c, fig_2d,
          labels="auto",
          ncol = 2,
          nrow = 2,
          font.label=list(color="black",size=8))

ggsave('./Figures/fig_2.eps',height=16,width=18,units='cm',device="eps")

####################### Figure 3 ######################################

ggarrange(fig_3a, fig_3b, fig_3c,
          labels="auto",
          ncol = 3,
          font.label=list(color="black",size=8))

ggsave('./Figures/fig_3.eps',height=8,width=18,units='cm',device="eps")

####################### Figure 4 ######################################

ggarrange(fig_4a, fig_4b, fig_4c,
          labels="auto",
          ncol = 3,
          font.label=list(color="black",size=8))

ggsave('./Figures/fig_4.eps',height=8,width=18,units='cm',device="eps")


########################## Write Output for script ###########################
writeLines(capture.output(sessionInfo()), "sessionInfo_main.txt")





