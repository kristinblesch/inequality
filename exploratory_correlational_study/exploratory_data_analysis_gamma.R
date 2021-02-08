library(dplyr)
library(tidyr)
library(tidyverse)
library('RVAideMemoire')
df <- read.csv("/Users/kristinblesch/Library/Mobile Documents/com~apple~CloudDocs/Paper_inequality/exploratory_correlational_study/df_exploratory_data_analysis.csv")  %>% 
  select(-"NAME_E")

# covariates: all columns except for first 10
# calculate bonferroni corrected significance level
bonf <- 1- 0.05/length(10:ncol(df))

# correlate aspects and Gini, correlate aspects and ortega parameters while controlling for the other (partial correlation)
# extract information on estimated correlation and confidence interval 
# o1 = ortega 1 = ortega alpha
# o2 = ortega 2 = ortega beta
gin <- apply(df[,10:ncol(df)], 2, function(a){unlist(cor.test(x = df$acs_gini, y=a, conf.level = bonf, method = "pearson")[c(4,9)])})
o1 <- apply(df[,10:ncol(df)], 2, function(a){unlist(pcor.test(x = df$ortega_1, y=a, z=df$ortega_2, conf.level = bonf, method = "pearson")[c(4,8)])})
o2 <- apply(df[,10:ncol(df)], 2, function(a){unlist(pcor.test(x = df$ortega_2, y=a, z=df$ortega_1, conf.level = bonf, method = "pearson")[c(4,8)])})

# how often is Gini not significant? 
sum((gin[2,] > 0 & gin[3,] < 0) | (gin[2,] < 0 & gin[3,] > 0)) # Gini is 41 out of 100 times not significant
which((gin[2,] > 0 & gin[3,] < 0) | (gin[2,] < 0 & gin[3,] > 0))

TRUE_gini_not_sig <- (gin[2,] > 0 & gin[3,] < 0) | (gin[2,] < 0 & gin[3,] > 0) # TRUE if Gini is not significantly dif. from zero
TRUE_o1_not_sig <- (o1[1,] > 0 & o1[2,] < 0) | (o1[1,] < 0 & o1[2,] > 0) #  TRUE if Ortega alpha is not significantly dif. from zero
TRUE_o2_not_sig <- (o2[1,] > 0 & o2[2,] < 0) | (o2[1,] < 0 & o2[2,] > 0)# TRUE if Ortega beta is not significantly dif. from zero

# Case 1: Gini = 0,and both Ortega parameters are != 0
sum(TRUE_gini_not_sig == TRUE & TRUE_o1_not_sig == FALSE & TRUE_o2_not_sig == FALSE)

# Case 2: Gini = 0, and exactly one Ortega != 0
sum((TRUE_gini_not_sig == TRUE & TRUE_o1_not_sig == TRUE & TRUE_o2_not_sig == FALSE) |
      (TRUE_gini_not_sig == TRUE & TRUE_o1_not_sig == FALSE & TRUE_o2_not_sig == TRUE))

# Case 3: Gini = 0, Ortega_1 = 0, Ortega_2 = 0 
sum(TRUE_gini_not_sig == TRUE & TRUE_o1_not_sig == TRUE & TRUE_o2_not_sig == TRUE)

# Case 4: Gini != 0, and exactly one Ortega !=0
sum((TRUE_gini_not_sig == FALSE & TRUE_o1_not_sig == FALSE & TRUE_o2_not_sig == TRUE) | 
      (TRUE_gini_not_sig == FALSE & TRUE_o1_not_sig == TRUE & TRUE_o2_not_sig == FALSE))

# Case 5: Gini != 0, and both Ortega are !=0
sum(TRUE_gini_not_sig == FALSE & TRUE_o1_not_sig == FALSE & TRUE_o2_not_sig == FALSE)

# Case 6: Gini !=0, but both Ortega are =0
sum(TRUE_gini_not_sig == FALSE & TRUE_o1_not_sig == TRUE & TRUE_o2_not_sig == TRUE)


#######
# make a correlations plot with confidence intervals
# transform ortega beta to ortega gamma for ease of interpretation (higher gamma = higher top concentrated inequalits
# --> ortega gamma = - ortega beta (--> simply take negative value of ortega 2)
# 
#######

library(tidyverse)
require(ggplot2)
df <- cbind(t(gin),t(o1),t(o2), c(1:ncol(gin))) %>% data.frame() %>% rownames_to_column()
names(df) <- c("aspect_name", "gini_est", "gini_lower", "gini_upper", "o1_lower", "o1_upper", "o1_est", "o2_lower", "o2_upper","o2_est", "index")
df <- df %>% arrange(desc(gini_est)) 
## add column with variables full description of what they mean from codebook
codebook <- read.csv("/Users/kristinblesch/Library/Mobile Documents/com~apple~CloudDocs/Paper_inequality/exploratory_correlational_study/codebook_df_exploratory_data_analysis.csv",
                     sep = ";")
df_with_codebook <- merge(df, codebook, by.x = "aspect_name", by.y = "variable")

## plot for all correlations
ggplot(df, aes(x = 1:nrow(df), y = gini_est, colour="Gini"))+ geom_hline(yintercept=0) +geom_point()+
  geom_errorbar(aes(ymax = gini_upper, ymin = gini_lower, colour="Gini")) + 
  geom_point(data =df, mapping = aes(x = 1:nrow(df),y=o1_est, colour="Ortega alpha"))+
  geom_errorbar(aes(ymax = o1_upper, ymin = o1_lower,colour="Ortega alpha")) + 
  geom_point(data =df, mapping = aes(x = 1:nrow(df),y=-o2_est, colour="Ortega gamma"))+
  geom_errorbar(aes(ymax = -o2_lower, ymin = -o2_upper, colour="Ortega gamma")) + labs(x="index aspect", y="pearson correlation", colour ="", title = "Correlation and CI for Inequality Measures with a Variety of Aspects",
                                                                                 subtitle = "Confidence level: 0.9995 (using a Bonferroni Correction)")+
  scale_color_manual(values=c("#FF9996", "#99CCFF", "#9999FF")) + scale_x_continuous(minor_breaks = seq(1, 100, 1))

## plot for Case 1: Gini = 0,and both Ortega parameters are != 0
case_1 <- TRUE_gini_not_sig == TRUE & TRUE_o1_not_sig == FALSE & TRUE_o2_not_sig == FALSE
df<- cbind(t(gin),t(o1),t(o2), c(1:ncol(gin))) %>% data.frame() %>% rownames_to_column() %>% filter(case_1)
names(df) <- c("aspect_name", "gini_est", "gini_lower", "gini_upper", "o1_lower", "o1_upper", "o1_est", "o2_lower", "o2_upper","o2_est", "index")
df_with_codebook <- merge(df, codebook, by.x = "aspect_name", by.y = "variable")
df <- df_with_codebook
df <- df %>% arrange(desc(gini_est)) 
ggplot(df, aes(x = full_variable.description, y = gini_est, colour="Gini"))+ geom_hline(yintercept=0) +geom_point()+
  geom_errorbar(aes(ymax = gini_upper, ymin = gini_lower, colour="Gini")) + 
  geom_point(data =df, mapping = aes(x = full_variable.description,y=o1_est, colour="Ortega alpha"))+
  geom_errorbar(aes(ymax = o1_upper, ymin = o1_lower,colour="Ortega alpha")) + 
  geom_point(data =df, mapping = aes(x = full_variable.description,y=-o2_est, colour="Ortega gamma"))+
  geom_errorbar(aes(ymax = -o2_lower, ymin = -o2_upper, colour="Ortega gamma")) + labs(x="", y="pearson correlation", colour ="", 
                                                                                 title = "Case 1: Gini = 0 and both Ortega parameters are != 0",
                                                                                 subtitle = "Confidence level: 0.9995 (using a Bonferroni Correction)")+
  scale_color_manual(values=c("#FF9996", "#99CCFF", "#9999FF")) +
  theme(axis.text.x = element_text(angle = 90))

## plot for Case 2: Gini = 0, and exactly one Ortega != 0
case_2 <- (TRUE_gini_not_sig == TRUE & TRUE_o1_not_sig == TRUE & TRUE_o2_not_sig == FALSE) |
  (TRUE_gini_not_sig == TRUE & TRUE_o1_not_sig == FALSE & TRUE_o2_not_sig == TRUE)
df<- cbind(t(gin),t(o1),t(o2), c(1:ncol(gin))) %>% data.frame() %>% rownames_to_column() %>% filter(case_2)
names(df) <- c("aspect_name", "gini_est", "gini_lower", "gini_upper", "o1_lower", "o1_upper", "o1_est", "o2_lower", "o2_upper","o2_est", "index")
df_with_codebook <- merge(df, codebook, by.x = "aspect_name", by.y = "variable")
df <- df_with_codebook
df <- df %>% arrange(desc(gini_est)) 
ggplot(df, aes(x = as.factor(1:nrow(df)), y = gini_est, colour="Gini"))+ geom_hline(yintercept=0) +geom_point()+
  geom_errorbar(aes(ymax = gini_upper, ymin = gini_lower, colour="Gini")) + 
  geom_point(data =df, mapping = aes(x = as.factor(1:nrow(df)),y=o1_est, colour="Ortega alpha"))+
  geom_errorbar(aes(ymax = o1_upper, ymin = o1_lower,colour="Ortega alpha")) + 
  geom_point(data =df, mapping = aes(x = as.factor(1:nrow(df)),y=-o2_est, colour="Ortega gamma"))+
  geom_errorbar(aes(ymax = -o2_lower, ymin = -o2_upper, colour="Ortega gamma")) + labs(x="", y="pearson correlation",
                                                                                 colour ="", title = "Case 2: Gini = 0, and exactly one Ortega != 0",
                                                                                 subtitle = "Confidence level: 0.9995 (using a Bonferroni Correction)")+
  scale_color_manual(values=c("#FF9996", "#99CCFF", "#9999FF")) + 
  scale_x_discrete(breaks = as.factor(1:nrow(df)), labels=df$full_variable.description) + 
  theme(axis.text.x = element_text(angle = 90))

## plot combined plot Case 1 & 2: Gini = 0, (one or two) Ortega !=0

case_1_or_2 <- c(case_1 | case_2)
df<- cbind(t(gin),t(o1),t(o2), c(1:ncol(gin))) %>% data.frame() %>% rownames_to_column() %>% filter(case_1_or_2)
names(df) <- c("aspect_name", "gini_est", "gini_lower", "gini_upper", "o1_lower", "o1_upper", "o1_est", "o2_lower", "o2_upper","o2_est", "index")
df_with_codebook <- merge(df, codebook, by.x = "aspect_name", by.y = "variable")
df <- df_with_codebook
df <- df %>% arrange(desc(gini_est)) 
# only Gini
ggplot(df, aes(x = as.factor(1:nrow(df)), y = gini_est, colour="Gini"))+ geom_hline(yintercept=0) +geom_point()+
  geom_errorbar(aes(ymax = gini_upper, ymin = gini_lower, colour="Gini")) + 
  labs(x="", y="pearson correlation",
       colour ="", title = "Case 1+2: Gini = 0, one/two Ortega != 0",
       subtitle = "Confidence level: 0.9995 (using a Bonferroni Correction)")+
  scale_x_discrete(breaks = as.factor(1:nrow(df)), labels=df$full_variable.description)+
  theme(axis.text.x = element_text(angle = 90))
  
# only Ortegas, but now gamma = negative beta
ggplot(df, aes(x = as.factor(1:nrow(df)),y=o1_est, colour="Ortega alpha"))+ geom_hline(yintercept=0) +geom_point()+
  geom_errorbar(aes(ymax = o1_upper, ymin = o1_lower,colour="Ortega alpha")) + 
  geom_point(data =df, mapping = aes(x = as.factor(1:nrow(df)),y=-o2_est, colour="Ortega gamma"))+
  geom_errorbar(aes(ymax = -o2_lower, ymin = -o2_upper, colour="Ortega gamma")) + labs(x="", y="pearson correlation",
                                                                                    colour ="", title = "Case 1+2: Gini = 0, one/two Ortega != 0",
                                                                                    subtitle = "Confidence level: 0.9995 (using a Bonferroni Correction)")+
  scale_color_manual(values=c("#99CCFF", "#9999FF")) + 
  scale_x_discrete(breaks = as.factor(1:nrow(df)), labels=df$full_variable.description) + 
  theme(axis.text.x = element_text(angle = 90))

## plot for Case 3: Gini = 0, Ortega_1 = 0, Ortega_2 = 0 
case_3 <- TRUE_gini_not_sig == TRUE & TRUE_o1_not_sig == TRUE & TRUE_o2_not_sig == TRUE
df<- cbind(t(gin),t(o1),t(o2), c(1:ncol(gin))) %>% data.frame() %>% rownames_to_column() %>% filter(case_3)
names(df) <- c("aspect_name", "gini_est", "gini_lower", "gini_upper", "o1_lower", "o1_upper", "o1_est", "o2_lower", "o2_upper","o2_est", "index")
df_with_codebook <- merge(df, codebook, by.x = "aspect_name", by.y = "variable")
df <- df_with_codebook
df <- df %>% arrange(desc(gini_est)) 
ggplot(df, aes(x = as.factor(1:nrow(df)), y = gini_est, colour="Gini"))+ geom_hline(yintercept=0) +geom_point()+
  geom_errorbar(aes(ymax = gini_upper, ymin = gini_lower, colour="Gini")) + 
  geom_point(data =df, mapping = aes(x = as.factor(1:nrow(df)),y=o1_est, colour="Ortega alpha"))+
  geom_errorbar(aes(ymax = o1_upper, ymin = o1_lower,colour="Ortega alpha")) + 
  geom_point(data =df, mapping = aes(x = as.factor(1:nrow(df)),y=-o2_est, colour="Ortega gamma"))+
  geom_errorbar(aes(ymax = -o2_lower, ymin = -o2_upper, colour="Ortega gamma")) + labs(x="", y="pearson correlation",
                                                                                 colour ="", title = "Case 3: Gini = 0, Ortega_1 = 0, Ortega_2 = 0",
                                                                                 subtitle = "Confidence level: 0.9995 (using a Bonferroni Correction)")+
  scale_color_manual(values=c("#FF9996", "#99CCFF", "#9999FF")) + 
  scale_x_discrete(breaks = as.factor(1:nrow(df)), labels=df$full_variable.description) + 
  theme(axis.text.x = element_text(angle = 90))


## plot for Case 4: Gini != 0, and exactly one Ortega !=0
case_4 <- (TRUE_gini_not_sig == FALSE & TRUE_o1_not_sig == FALSE & TRUE_o2_not_sig == TRUE) | 
  (TRUE_gini_not_sig == FALSE & TRUE_o1_not_sig == TRUE & TRUE_o2_not_sig == FALSE)
df<- cbind(t(gin),t(o1),t(o2), c(1:ncol(gin))) %>% data.frame() %>% rownames_to_column() %>% filter(case_4)
names(df) <- c("aspect_name", "gini_est", "gini_lower", "gini_upper", "o1_lower", "o1_upper", "o1_est", "o2_lower", "o2_upper","o2_est", "index")
df_with_codebook <- merge(df, codebook, by.x = "aspect_name", by.y = "variable")
df <- df_with_codebook
df <- df %>% arrange(desc(gini_est)) 
ggplot(df, aes(x = as.factor(1:nrow(df)), y = gini_est, colour="Gini"))+ geom_hline(yintercept=0) +geom_point()+
  geom_errorbar(aes(ymax = gini_upper, ymin = gini_lower, colour="Gini")) + 
  geom_point(data =df, mapping = aes(x = as.factor(1:nrow(df)),y=o1_est, colour="Ortega alpha"))+
  geom_errorbar(aes(ymax = o1_upper, ymin = o1_lower,colour="Ortega alpha")) + 
  geom_point(data =df, mapping = aes(x = as.factor(1:nrow(df)),y=-o2_est, colour="Ortega gamma"))+
  geom_errorbar(aes(ymax = -o2_lower, ymin = -o2_upper, colour="Ortega gamma")) + labs(x="", y="pearson correlation",
                                                                                 colour ="", title = "Case 4: Gini != 0, and exactly one Ortega !=0",
                                                                                 subtitle = "Confidence level: 0.9995 (using a Bonferroni Correction)")+
  scale_color_manual(values=c("#FF9996", "#99CCFF", "#9999FF")) + 
  scale_x_discrete(breaks = as.factor(1:nrow(df)), labels=df$full_variable.description) + 
  theme(axis.text.x = element_text(angle = 90))

## plot for Case  5: Gini != 0, and both Ortega are !=0
case_5 <- TRUE_gini_not_sig == FALSE & TRUE_o1_not_sig == FALSE & TRUE_o2_not_sig == FALSE
df<- cbind(t(gin),t(o1),t(o2), c(1:ncol(gin))) %>% data.frame() %>% rownames_to_column() %>% filter(case_5)
names(df) <- c("aspect_name", "gini_est", "gini_lower", "gini_upper", "o1_lower", "o1_upper", "o1_est", "o2_lower", "o2_upper","o2_est", "index")
df_with_codebook <- merge(df, codebook, by.x = "aspect_name", by.y = "variable")
df <- df_with_codebook
df <- df %>% arrange(desc(gini_est)) 
ggplot(df, aes(x = as.factor(1:nrow(df)), y = gini_est, colour="Gini"))+ geom_hline(yintercept=0) +geom_point()+
  geom_errorbar(aes(ymax = gini_upper, ymin = gini_lower, colour="Gini")) + 
  geom_point(data =df, mapping = aes(x = as.factor(1:nrow(df)),y=o1_est, colour="Ortega alpha"))+
  geom_errorbar(aes(ymax = o1_upper, ymin = o1_lower,colour="Ortega alpha")) + 
  geom_point(data =df, mapping = aes(x = as.factor(1:nrow(df)),y=-o2_est, colour="Ortega gamma"))+
  geom_errorbar(aes(ymax = -o2_lower, ymin = -o2_upper, colour="Ortega gamma")) + labs(x="", y="pearson correlation",
                                                                                 colour ="", title = "Case  5: Gini != 0, and both Ortega are !=0",
                                                                                 subtitle = "Confidence level: 0.9995 (using a Bonferroni Correction)")+
  scale_color_manual(values=c("#FF9996", "#99CCFF", "#9999FF")) + 
  scale_x_discrete(breaks = as.factor(1:nrow(df)), labels=df$full_variable.description) + 
  theme(axis.text.x = element_text(angle = 90))

# Case 6: Gini !=0, but both Ortega are =0
case_6 <- TRUE_gini_not_sig == FALSE & TRUE_o1_not_sig == TRUE & TRUE_o2_not_sig == TRUE

###################
# which aspects are the cases? 
###################
df<- cbind(t(gin),t(o1),t(o2), c(1:ncol(gin))) %>% data.frame() %>% rownames_to_column() 
df$rowname[case_1]
df$rowname[case_2]
df$rowname[case_3]
df$rowname[case_4]
df$rowname[case_5]
df$rowname[case_6]
