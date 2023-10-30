library(dplyr)




#Study 1:
#Read in data:
Study_1_df_T <- read.csv('./Data/Study_1_df_T.csv')

T_LQ_Data_Study_1 <- Study_1_df_T %>% filter(Article == "1" | Article == "2" | Article == "3")
T_Mainstream_Data_Study_1 <- Study_1_df_T %>% filter(Article == "4" | Article == "5")

All_T_Study_1 <- rbind(T_LQ_Data_Study_1,T_Mainstream_Data_Study_1)

All_T_Study_1 <- All_T_Study_1 %>% select(True_Dummy,Treat_Search,Age,Dummy_Congruence,Education_Score,Gender,Income_Score,Article_day,ResponseId)

All_T_Study_1 <- na.omit(All_T_Study_1)

nrow(All_T_Study_1)







#Read in data (Study 1 - false/misleading articles):
Misl_False_Search_MF <- read.csv('./Data/Study_1_df_FM.csv')
Misl_False_Search_MF <- Misl_False_Search_MF %>% select(Susc_FN,Treat_Search,Age,Dummy_Congruence,Education_Score,Gender,Income_Score,Article_day,ResponseId)
Misl_False_Search_MF <- na.omit(Misl_False_Search_MF)


#Read in data (Study 2 - True articles):
Data_Bef_Aft_T <- read.csv('./Data/Study_2_df_T.csv')
T_LQ_Data_Study_2 <- Data_Bef_Aft_T %>% filter(Article == "1" | Article == "2" | Article == "3")
T_Mainstream_Data_Study_2 <- Data_Bef_Aft_T %>% filter(Article == "4" | Article == "5")
All_T_Study_2 <- rbind(T_LQ_Data_Study_2,T_Mainstream_Data_Study_2)
All_T_Study_2 <- All_T_Study_2 %>% select(True_Dummy,Treat_Search,Age,Dummy_Congruence,Education_Score,Gender,Income_Score,Article_day,ResponseId)
All_T_Study_2 <- na.omit(All_T_Study_2)
nrow(All_T_Study_2)


#Pull in this data: Study 2 - False/Misleading Articles:
Data_Bef_Aft_MF <- read.csv('./Data/Study_2_df_FM.csv')
Data_Bef_Aft_MF <- Data_Bef_Aft_MF %>% select(Susc_FN,Treat_Search,Age,Dummy_Congruence,Education_Score,Gender,Income_Score,Article_day,ResponseId)
Data_Bef_Aft_MF <- na.omit(Data_Bef_Aft_MF)
nrow(Data_Bef_Aft_MF)


#Study 3 (True Articles):
Study_3_df <- read.csv('./Data/Study_3_df_T.csv')
T_LQ_Data_Study_3 <- Study_3_df %>% filter(Article == "1" | Article == "2" | Article == "3")
T_Mainstream_Data_Study_3 <- Study_3_df %>% filter(Article == "4" | Article == "5")
All_T_Study_3 <- rbind(T_LQ_Data_Study_3,T_Mainstream_Data_Study_3)
All_T_Study_3 <- All_T_Study_3 %>% select(True_Dummy,Treatment,Age,Dummy_Congruence,Education_Score,Gender,Income_Score,Article_day,ResponseId)
All_T_Study_3 <- na.omit(All_T_Study_3)
nrow(All_T_Study_3)

#Study 3 Analysis (FM):
Study_3_False_M <- read.csv('./Data/Study_3_df_FM.csv')
Study_3_False_M <- Study_3_False_M %>% select(True_Dummy,Treatment,Age,Dummy_Congruence,Education_Score,Gender,Income_Score,Article_day,ResponseId)
Study_3_False_M <- na.omit(Study_3_False_M)
nrow(Study_3_False_M)

#Study 4 (True Articles):
Data_Bef_Aft_Covid_T <- read.csv('./Data/Study_4_df_T.csv')
T_LQ_Data_Study_4 <- Data_Bef_Aft_Covid_T %>% filter(Article == "1" | Article == "2" | Article == "3")
T_Mainstream_Data_Study_4 <- Data_Bef_Aft_Covid_T %>% filter(Article == "4" | Article == "5")
All_T_Study_4 <- rbind(T_LQ_Data_Study_4,T_Mainstream_Data_Study_4)
All_T_Study_4 <- All_T_Study_4 %>% select(True_Dummy,Treat_Search,Age,Dummy_Congruence,Education_Score,Gender,Income_Score,Article_day,ResponseId)
All_T_Study_4 <- na.omit(All_T_Study_4)
nrow(All_T_Study_4)

#Study 4 (False Misleading Articles)
Study_4_False_M <- read.csv('./Data/Study_4_df_FM.csv')
Study_4_False_M <- Study_4_False_M %>% select(Susc_FN,Treat_Search,Age,Dummy_Congruence,Education_Score,Gender,Income_Score,Article_day,ResponseId)
Study_4_False_M <- na.omit(Study_4_False_M)
nrow(Study_4_False_M)


#Study 5 (True Articles):
T_Data_Study_5 <- read.csv('./Data/Study_5_df_T.csv')
T_LQ_Data_Study_5 <- T_Data_Study_5 %>% filter(Article_Num == 1 | Article_Num == 2 | Article_Num == 3)
T_Mainstream_Data_Study_5 <- T_Data_Study_5 %>% filter(Article_Num == 4 | Article_Num == 5)
All_T_Study_5 <- rbind(T_LQ_Data_Study_5,T_Mainstream_Data_Study_5)
All_T_Study_5 <- All_T_Study_5 %>% select(True_Dummy,Treatment,Age,Ideo_Congruence,Education_Score,Gender,Income_Score,Article_day,ResponseId)
All_T_Study_5 <- na.omit(All_T_Study_5)
nrow(All_T_Study_5)

#Study 5 (False/Misleading Articles):
Study_5_False_M <- read.csv('./Data/Study_5_df_FM.csv')
Study_5_False_M <- Study_5_False_M %>% select(True_Dummy,Treatment,Age,Ideo_Congruence,Education_Score,Gender,Income_Score,Article_day,ResponseId)
Study_5_False_M <- na.omit(Study_5_False_M)
nrow(Study_5_False_M)

T_LQ_Data_Study_1 <- T_LQ_Data_Study_1 %>% select(True_Dummy,Treat_Search,Age,Dummy_Congruence,Education_Score,Gender,Income_Score,Article_day,ResponseId)
T_LQ_Data_Study_1 <- na.omit(T_LQ_Data_Study_1)

T_Mainstream_Data_Study_1 <- T_Mainstream_Data_Study_1 %>% select(True_Dummy,Treat_Search,Age,Dummy_Congruence,Education_Score,Gender,Income_Score,Article_day,ResponseId)
T_Mainstream_Data_Study_1 <- na.omit(T_Mainstream_Data_Study_1)

############## 
############## Study 2:
############## 
T_LQ_Data_Study_2 <- T_LQ_Data_Study_2 %>% select(True_Dummy,Treat_Search,Age,Dummy_Congruence,Education_Score,Gender,Income_Score,Article_day,ResponseId)
T_LQ_Data_Study_2 <- na.omit(T_LQ_Data_Study_2)

T_Mainstream_Data_Study_2 <- T_Mainstream_Data_Study_2 %>% select(True_Dummy,Treat_Search,Age,Dummy_Congruence,Education_Score,Gender,Income_Score,Article_day,ResponseId)
T_Mainstream_Data_Study_2 <- na.omit(T_Mainstream_Data_Study_2)

############## 
############## Study 3:
############## 

T_LQ_Data_Study_3 <- T_LQ_Data_Study_3 %>% select(True_Dummy,Treatment,Age,Dummy_Congruence,Education_Score,Gender,Income_Score,Article_day,ResponseId)
T_LQ_Data_Study_3 <- na.omit(T_LQ_Data_Study_3)

T_Mainstream_Data_Study_3 <- T_Mainstream_Data_Study_3 %>% select(True_Dummy,Treatment,Age,Dummy_Congruence,Education_Score,Gender,Income_Score,Article_day,ResponseId)
T_Mainstream_Data_Study_3 <- na.omit(T_Mainstream_Data_Study_3)


############## 
############## Study 4:
############## 

T_LQ_Data_Study_4 <- T_LQ_Data_Study_4 %>% select(True_Dummy,Treat_Search,Age,Dummy_Congruence,Education_Score,Gender,Income_Score,Article_day,ResponseId)
T_LQ_Data_Study_4 <- na.omit(T_LQ_Data_Study_4)

T_Mainstream_Data_Study_4 <- T_Mainstream_Data_Study_4 %>% select(True_Dummy,Treat_Search,Age,Dummy_Congruence,Education_Score,Gender,Income_Score,Article_day,ResponseId)
T_Mainstream_Data_Study_4 <- na.omit(T_Mainstream_Data_Study_4)

############## 
############## Study 5:
############## 

T_LQ_Data_Study_5 <- T_LQ_Data_Study_5 %>% select(True_Dummy,Treatment,Age,Ideo_Congruence,Education_Score,Gender,Income_Score,Article_day,ResponseId)
T_LQ_Data_Study_5 <- na.omit(T_LQ_Data_Study_5)

T_Mainstream_Data_Study_5 <- T_Mainstream_Data_Study_5 %>% select(True_Dummy,Treatment,Age,Ideo_Congruence,Education_Score,Gender,Income_Score,Article_day,ResponseId)
T_Mainstream_Data_Study_5 <- na.omit(T_Mainstream_Data_Study_5)




#Study 1 (Number of Evaluations):
nrow(T_LQ_Data_Study_1 %>% filter(Treat_Search == 1))
nrow(T_LQ_Data_Study_1 %>% filter(Treat_Search == 0))
nrow(T_Mainstream_Data_Study_1 %>% filter(Treat_Search == 1))
nrow(T_Mainstream_Data_Study_1 %>% filter(Treat_Search == 0))
nrow(Misl_False_Search_MF %>% filter(Treat_Search == 1))
nrow(Misl_False_Search_MF %>% filter(Treat_Search == 0))

#Study 1 (Unique Respondents):
nrow(T_LQ_Data_Study_1 %>% group_by(ResponseId) %>% filter(Treat_Search == 1) %>% count())
nrow(T_LQ_Data_Study_1 %>% group_by(ResponseId) %>% filter(Treat_Search == 0) %>% count())
nrow(T_Mainstream_Data_Study_1 %>% group_by(ResponseId) %>% filter(Treat_Search == 1) %>% count())
nrow(T_Mainstream_Data_Study_1 %>% group_by(ResponseId) %>% filter(Treat_Search == 0) %>% count())
nrow(Misl_False_Search_MF %>% group_by(ResponseId) %>% filter(Treat_Search == 1) %>% count())
nrow(Misl_False_Search_MF %>% group_by(ResponseId) %>% filter(Treat_Search == 0) %>% count())

#Study 2:
nrow(T_LQ_Data_Study_2 %>% filter(Treat_Search == 1))
nrow(T_LQ_Data_Study_2 %>% filter(Treat_Search == 0))
nrow(T_Mainstream_Data_Study_2 %>% filter(Treat_Search == 1))
nrow(T_Mainstream_Data_Study_2 %>% filter(Treat_Search == 0))
nrow(Data_Bef_Aft_MF %>% filter(Treat_Search == 1))
nrow(Data_Bef_Aft_MF %>% filter(Treat_Search == 0))

#Study 3:
nrow(T_LQ_Data_Study_3 %>% filter(Treatment == 1))
nrow(T_LQ_Data_Study_3 %>% filter(Treatment == 0))
nrow(T_Mainstream_Data_Study_3 %>% filter(Treatment == 1))
nrow(T_Mainstream_Data_Study_3 %>% filter(Treatment == 0))
nrow(Study_3_False_M %>% filter(Treatment == 1))
nrow(Study_3_False_M %>% filter(Treatment == 0))

#Study 4:
nrow(T_LQ_Data_Study_4 %>% filter(Treat_Search == 1))
nrow(T_LQ_Data_Study_4 %>% filter(Treat_Search == 0))
nrow(T_Mainstream_Data_Study_4 %>% filter(Treat_Search == 1))
nrow(T_Mainstream_Data_Study_4 %>% filter(Treat_Search == 0))
nrow(Study_4_False_M %>% filter(Treat_Search == 1))
nrow(Study_4_False_M %>% filter(Treat_Search == 0))

#Study 5:
nrow(T_LQ_Data_Study_5 %>% filter(Treatment == 1))
nrow(T_LQ_Data_Study_5 %>% filter(Treatment == 0))
nrow(T_Mainstream_Data_Study_5 %>% filter(Treatment == 1))
nrow(T_Mainstream_Data_Study_5 %>% filter(Treatment == 0))
nrow(Study_5_False_M %>% filter(Treatment == 1))
nrow(Study_5_False_M %>% filter(Treatment == 0))

#Study 5 (Unique Respondents):
nrow(T_LQ_Data_Study_5 %>% group_by(ResponseId) %>% filter(Treatment == 1) %>% count())
nrow(T_LQ_Data_Study_5 %>% group_by(ResponseId) %>% filter(Treatment == 0) %>% count())
nrow(T_Mainstream_Data_Study_5 %>% group_by(ResponseId) %>% filter(Treatment == 1) %>% count())
nrow(T_Mainstream_Data_Study_5 %>% group_by(ResponseId) %>% filter(Treatment == 0) %>% count())
nrow(Study_5_False_M %>% group_by(ResponseId) %>% filter(Treatment == 1) %>% count())
nrow(Study_5_False_M %>% group_by(ResponseId) %>% filter(Treatment == 0) %>% count())









