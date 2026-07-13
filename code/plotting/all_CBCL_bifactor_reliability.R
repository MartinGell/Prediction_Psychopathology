
library(tidyverse)
library(corrplot)
library(psych)
library(lme4)


old_path = '/home/mgell/Work/pfactor_reliability/'
new_path = '/Users/mgell/Work/pfactor/'

plt_out = paste0(new_path,"plots/")


# Functions 
calculate_rel_ICC <- function(T1, T2) {
  rel_ICC <- numeric(length(T1))
  rel_ICC_upper <- numeric(length(T1))
  rel_ICC_lower <- numeric(length(T1))
  i <- 1
  
  for (factor_i in 1:length(T1)) {
    x <- ICC(data.frame(T1[,factor_i], T2[,factor_i]))
    ICCi <- x$results$ICC[3]
    rel_ICC[i] <- ICCi
    rel_ICC_upper[i] <- x$results$`upper bound`[2]
    rel_ICC_lower[i] <- x$results$`lower bound`[2]
    i <- i + 1
  }
  
  return(data.frame('rel_ICC3' = rel_ICC, 'rel_ICC_upper' = rel_ICC_upper, 'rel_ICC_lower' = rel_ICC_lower))
}

calculate_corrected_test_retest <- function(factors, df1, df2) {
  test_retest <- numeric(length(factors))
  test_retest_corrected <- numeric(length(factors))
  i <- 1
  
  for (factor_i in factors) {
    x1 <- data.frame('subject_id' = df1$EID, 'age' = df1$interview_age, 'values' = df1[,factor_i], 'sesh' = 1)
    x2 <- data.frame('subject_id' = df1$EID, 'age' = df2$interview_age, 'values' = df2[,factor_i], 'sesh' = 2)
    
    colnames(x1) <- c('subject_id', 'age', 'values', 'sesh')
    colnames(x2) <- c('subject_id', 'age', 'values', 'sesh')
    
    x <- rbind(x1, x2)
    
    corrected_model <- lmer(values ~ age * sesh + (1 | subject_id), data = x)
    
    corrected <- residuals(corrected_model)
    
    test_retest_corrected[i] <- cor(corrected[x$sesh == 1], corrected[x$sesh == 2]) * -1
    
    i <- i + 1
  }
  
  return(test_retest_corrected)
}



# Data
df1 = read_csv(paste0(new_path,'res/ABCD_CBCL_factor_scores_baseline.csv'))
df2 = read_csv(paste0(new_path,'res/ABCD_CBCL_factor_scores_followup.csv'))

df1$retest_interval <- df2$interview_age - df1$interview_age

df2 <- df2[df1$retest_interval > 1,]
df1 <- df1[df1$retest_interval > 1,] # 1 sub with retest interval of 1 - likely a mistake?

# MEAN RETEST FOR WHOLE SAMPLE
print(mean(df1$retest_interval))
print(min(df1$retest_interval))
print(max(df1$retest_interval))


# Rename
column_mapping_df <- data.frame(
  old_name = c(
    "P_ACH2F", "IN_ACH2F", "EX_ACH2F", 
    "P_ACH8", "ANX_ACH8", "WIT_ACH8", "SOM_ACH8", "RUL_ACH8", "AGG_ACH8", "SOC_ACH8", "THO_ACH8", "ATT_ACH8",
    "P_MOOR3S", "I_MOOR3S", "E_MOOR3S", "A_MOOR3S", 
    "P_MOOR4S", "I_MOOR4S", "S_MOOR4S", "E_MOOR4S", "A_MOOR4S", 
    "P_MCELRY", "I_MCELRY", "E_MCELRY", "A_MCELRY",
    "P_DTZGP", "I_DTZGP", "E_DTZGP", 
    "P_DTZDP", "AD_DTZDP", "AG_DTZDP", "AT_DTZDP",
    "P_HALTGN", "I_HALTGN", "E_HALTGN", "T_HALTGN", "A_HALTGN",
    "P_CLRK2S", "I_CLRK2S", "E_CLRK2S",
    "P_CLRK3S", "I_CLRK3S", "E_CLRK3S", "A_CLRK3S", 
    "P_CLRK4S", "I_CLRK4S", "S_CLRK4S", "E_CLRK4S", "A_CLRK4S"
  ),
  new_name = c(
    "P-factor (Achenbach 2S)", "Int. (Achenbach 2S)", "Ext. (Achenbach 2S)",
    "P-factor (Achenbach 8S)", "Anxiety-dep. (Achenbach 8S)", "Withdrawn-dep. (Achenbach 8S)", "Som. (Achenbach 8S)", "Rule-Breaking (Achenbach 8S)", "Aggressive (Achenbach 8S)","Social (Achenbach 8S)", "Tho. (Achenbach 8S)", "Att. (Achenbach 8S)", 
    "P-factor (Moore 3S)", "Int. (Moore 3S)", "Ext. (Moore 3S)", "Att. (Moore 3S)",
    "P-factor (Moore 4S)", "Int. (Moore 4S)", "Som. (Moore 4S)", "Ext. (Moore 4S)", "Att. (Moore 4S)", 
    "P-factor (McElroy)", "Int. (McElroy)", "Ext. (McElroy)", "Att. (McElroy)", 
    "P-factor (Deutz GP)", "Int. (Deutz GP)", "Ext. (Deutz GP)",
    "P-factor (Deutz DP)", "Anxiety-dep. (Deutz DP)", "Aggressive (Deutz DP)", "Att. (Deutz DP)",
    "P-factor (Haltigan GP)", "Int. (Haltigan GP)", "Ext. (Haltigan GP)", "Tho. (Haltigan GP)","Att. (Haltigan GP)", 
    "P-factor (Clark 2S)", "Int. (Clark 2S)", "Ext. (Clark 2S)",
    "P-factor (Clark 3S)", "Int. (Clark 3S)", "Ext. (Clark 3S)", "Att. (Clark 3S)",
    "P-factor (Clark 4S)", "Int. (Clark 4S)", "Som. (Clark 4S)", "Ext. (Clark 4S)", "Att. (Clark 4S)"
  )
)

# df1 <- df1 %>%
#   rename(!!!setNames(column_mapping_df$old_name,as.character(column_mapping_df$new_name))) %>%
#   select(c(everything(), column_mapping_df$new_name))
# df2 <- df2 %>%
#   rename(!!!setNames(column_mapping_df$old_name,as.character(column_mapping_df$new_name))) %>%
#   select(c(everything(), column_mapping_df$new_name))





# ABCD full sample
all(df1$EID == df2$EID)

factors <- colnames(df2)[13:length(df2)]
cor_rel <- calculate_corrected_test_retest(factors,df1,df2)

#T1 <- df1 %>% select(`P-factor (Achenbach 2S)`:`Att. (Clark 4S)`)
#T2 <- df2 %>% select(`P-factor (Achenbach 2S)`:`Att. (Clark 4S)`)
T1 <- df1 %>% select(P_ACH2F:A_CLRK4S)
T2 <- df2 %>% select(P_ACH2F:A_CLRK4S)

# corr all factors with each other as well
cormat <- cor(T1,T1,use = "pairwise.complete.obs")

cormat <- cor(T1,T2,use = "pairwise.complete.obs")
rel <- cormat[row(cormat)==col(cormat)]

ICCs <- calculate_rel_ICC(T1,T2)

df <- cbind(data.frame('Factor' = colnames(cormat), 'Reliability_r' = rel, 'Reliability_r_corrected' = cor_rel[1:length(rel)]),ICCs)
df$Dataset <- paste0('ABCD(',max(df1$retest_interval),'/',length(df1$EID),')')


df1y = df1 %>% 
  summarise(
    across(c(interview_age), 
           list(mean = ~mean(. , na.rm = TRUE)/12,
                n = ~sum(!is.na(.))))
  )

length(df1$sex[df1$sex == 1])






# ABCD 1 year only
df1y <- df1[df1$retest_interval <= 12,] 
df2y <- df2 %>% filter(EID %in% c(df1y$EID))

all(df1y$EID == df2y$EID)

cor_rel <- calculate_corrected_test_retest(factors,df1y,df2y)

T1 <- df1y %>% select(P_ACH2F:A_CLRK4S)
T2 <- df2y %>% select(P_ACH2F:A_CLRK4S)

cormat <- cor(T1,T2,use = "pairwise.complete.obs")
rel <- cormat[row(cormat)==col(cormat)]

ICCs <- calculate_rel_ICC(T1,T2)

df_year <- cbind(data.frame('Factor' = colnames(cormat), 'Reliability_r' = rel, 'Reliability_r_corrected' = cor_rel[1:length(rel)]),ICCs)
df_year$Dataset <- paste0('ABCD(',max(df1y$retest_interval),'/',length(df1y$EID),')')

df <- rbind(df,df_year)

ddd <- df_year
ddd$model <- 'specific'
ddd$model[grepl('P_', ddd$Factor, fixed = TRUE)] <- 'general'

mean(ddd$Reliability_r_corrected[ddd$model == 'general'])
max(ddd$Reliability_r_corrected[ddd$model == 'general'])
min(ddd$Reliability_r_corrected[ddd$model == 'general'])

mean(ddd$Reliability_r_corrected[ddd$model == 'specific'])
max(ddd$Reliability_r_corrected[ddd$model == 'specific'])
min(ddd$Reliability_r_corrected[ddd$model == 'specific'])

df1y %>% 
  summarise(
    across(c(interview_age), 
           list(mean = ~mean(. , na.rm = TRUE)/12,
                n = ~sum(!is.na(.))))
  )

length(df1y$sex[df1y$sex == 1])

mean(df1y$retest_interval)
max(df1y$retest_interval)

write_csv(ddd, '/Users/mgell/Work/pfactor/manuscript_tables/ABCD_CBCL_factor_reliability.csv')



# ABCD 10m only
df1y <- df1[df1$retest_interval <= 10,] 
df2y <- df2 %>% filter(EID %in% c(df1y$EID))

all(df1y$EID == df2y$EID)

cor_rel <- calculate_corrected_test_retest(factors,df1y,df2y)

T1 <- df1y %>% select(P_ACH2F:A_CLRK4S)
T2 <- df2y %>% select(P_ACH2F:A_CLRK4S)

cormat <- cor(T1,T2,use = "pairwise.complete.obs")
rel <- cormat[row(cormat)==col(cormat)]

ICCs <- calculate_rel_ICC(T1,T2)

df_year <- cbind(data.frame('Factor' = colnames(cormat), 'Reliability_r' = rel, 'Reliability_r_corrected' = cor_rel[1:length(rel)]),ICCs)
df_year$Dataset <- paste0('ABCD(',max(df1y$retest_interval),'/',length(df1y$EID),')')

df <- rbind(df,df_year)

#write_csv(df, '/home/mgell/Work/pfactor_reliability/data/abcd_pfactor_reliability.csv')

if (all(rep(column_mapping_df$old_name,3) == df$Factor)) {
  df$Factor = rep(column_mapping_df$new_name)
}

cor.test(df$rel_ICC3[df$Dataset == 'ABCD(12/7250)'],df$Reliability_r_corrected[df$Dataset == 'ABCD(12/7250)'])#, method = "spearman")


df_formatted <- df %>% 
  mutate_if(is.numeric, ~ signif(., 3)) %>%
  mutate(
    `ICC 95% CI` = paste0("[", rel_ICC_lower, ", ", rel_ICC_upper, "]")
  ) %>% 
  dplyr::select(Factor, Reliability_r, Reliability_r_corrected, rel_ICC3, `ICC 95% CI`, Dataset)


#write_csv(df_formatted, '/home/mgell/Work/pfactor_reliability/data/abcd_pfactor_reliability_formatted.csv')




# BHRC full sample
# load stuff
data = readRDS(paste0(new_path,'data/BHRC_CBCL_Household_Neuroimage_scores.Rds'))
#consistency = read.csv('/home/mgell/Work/pfactor_reliability/data/reliability_consistency_models.csv')

data = data[-595,]

colnames(data)[3] <- 'interview_age'

data$Interview_day_difference = abs(data$Interview_day_difference)

d = data


sum(d$Gender == 'Female')
max(d$interview_age)
min(d$interview_age)


d1 <- d[,c(1,3,298:346)]
d2 <- d[,c(1,3,347:395)]
d2$interview_age <- d2$interview_age + d$Interview_day_difference/365

# retest interval
print(mean(d$Interview_day_difference)/30.44)
print(min(d$Interview_day_difference)/30.44)
print(max(d$Interview_day_difference)/30.44)

# calculate reliability
colnames(d1)[3:51] <- str_replace(colnames(d1)[3:51], " - Household", "")
colnames(d2)[3:51] <- str_replace(colnames(d2)[3:51], " - Neuroimage", "")

T1 <- d1 %>% select(`P-factor (Achenbach 2S)`:`Att. (Clark 4S)`)
T2 <- d2 %>% select(`P-factor (Achenbach 2S)`:`Att. (Clark 4S)`)
#T1 <- d[,298:346]
#T2 <- d[,347:395]

factors <- colnames(T1)
cor_rel <- calculate_corrected_test_retest(factors,d1,d2)


d %>% group_by(Selection) %>% summarise_if(is.numeric, list(mean,sd), na.rm = TRUE)

cormat <- cor(T1,T2,use = "pairwise.complete.obs")
rel <- cormat[row(cormat)==col(cormat)]

#colnames(cormat)<-str_replace(colnames(cormat), " - Neuroimage", "")

ICCs <- calculate_rel_ICC(T1,T2)

df_bhrc <- cbind(data.frame('Factor' = colnames(cormat), 'Reliability_r' = rel, 'Reliability_r_corrected' = cor_rel[1:length(rel)]), ICCs, 'Dataset' = 'BHRC(17/772)')

df <- rbind(df,df_bhrc)

ddd <- df_bhrc
ddd$model <- 'specific'
ddd$model[grepl('P-factor', ddd$Factor, fixed = TRUE)] <- 'general'

mean(ddd$Reliability_r_corrected[ddd$model == 'general'])
max(ddd$Reliability_r_corrected[ddd$model == 'general'])
min(ddd$Reliability_r_corrected[ddd$model == 'general'])

mean(ddd$Reliability_r_corrected[ddd$model == 'specific'])
max(ddd$Reliability_r_corrected[ddd$model == 'specific'])
min(ddd$Reliability_r_corrected[ddd$model == 'specific'])




# BHRC 6 months
#data = readRDS('/home/mgell/Work/pfactor_reliability/data/BHRC_CBCL_Household_Neuroimage_scores.Rds')

d = data
d = d[d$Interview_day_difference <= 180,]
#d = d[d$Interview_day_difference <= 90,]
#d = d[d$Selection != 'Community',]

d1 <- d[,c(1,3,298:346)]
d2 <- d[,c(1,3,347:395)]
d2$interview_age <- d2$interview_age + d$Interview_day_difference/365

# retest interval
print(mean(d$Interview_day_difference)/30.44)
print(min(d$Interview_day_difference)/30.44)
print(max(d$Interview_day_difference)/30.44)

colnames(d1)[3:51] <- str_replace(colnames(d1)[3:51], " - Household", "")
colnames(d2)[3:51] <- str_replace(colnames(d2)[3:51], " - Neuroimage", "")

T1 <- d1 %>% select(`P-factor (Achenbach 2S)`:`Att. (Clark 4S)`)
T2 <- d2 %>% select(`P-factor (Achenbach 2S)`:`Att. (Clark 4S)`)
#T1 <- d[,298:346]
#T2 <- d[,347:395]

factors <- colnames(T1)
cor_rel <- calculate_corrected_test_retest(factors,d1,d2)


d %>% group_by(Selection) %>% summarise_if(is.numeric, list(mean,sd), na.rm = TRUE)

cormat <- cor(T1,T2,use = "pairwise.complete.obs")
rel <- cormat[row(cormat)==col(cormat)]

#colnames(cormat)<-str_replace(colnames(cormat), " - Neuroimage", "")

ICCs <- calculate_rel_ICC(T1,T2)

df_bhrc <- cbind(data.frame('Factor' = colnames(cormat), 'Reliability_r' = rel, 'Reliability_r_corrected' = cor_rel[1:length(rel)]), ICCs, 'Dataset' = 'BHRC(6/235)')

df <- rbind(df,df_bhrc)

ddd <- df_bhrc
ddd$model <- 'specific'
ddd$model[grepl('P-factor', ddd$Factor, fixed = TRUE)] <- 'general'

mean(ddd$Reliability_r_corrected[ddd$model == 'general'])
max(ddd$Reliability_r_corrected[ddd$model == 'general'])
min(ddd$Reliability_r_corrected[ddd$model == 'general'])

mean(ddd$Reliability_r_corrected[ddd$model == 'specific'])
max(ddd$Reliability_r_corrected[ddd$model == 'specific'])
min(ddd$Reliability_r_corrected[ddd$model == 'specific'])

summary(d$Gender)

df_bhrc$beh <- 'Other'
df_bhrc$beh[grepl('P-factor', df_bhrc$Factor, fixed = TRUE)] <- 'P-Factor'
df_bhrc$beh[grepl('Att', df_bhrc$Factor, fixed = TRUE)] <- 'Attention'
df_bhrc$beh[grepl('Int', df_bhrc$Factor, fixed = TRUE)] <- 'Internalising'
df_bhrc$beh[grepl('Ext', df_bhrc$Factor, fixed = TRUE)] <- 'Externalising'

df_bhrc$beh[grepl('Anxiety-dep. (Deutz DP)', df_bhrc$Factor, fixed = TRUE)] <- 'Internalising'
df_bhrc$beh[grepl('Aggressive (Deutz DP)', df_bhrc$Factor, fixed = TRUE)] <- 'Externalising'

mean(df_bhrc$Reliability_r_corrected[df_bhrc$beh == 'Other'])
min(df_bhrc$Reliability_r_corrected[df_bhrc$beh == 'Other'])
max(df_bhrc$Reliability_r_corrected[df_bhrc$beh == 'Other'])

#write_csv(ddd, '/Users/mgell/Work/pfactor/manuscript_tables/BHRC_CBCL_factor_reliability.csv')



# 3 month
d = data[data$Interview_day_difference <= 90,]

d1 <- d[,c(1,3,298:346)]
d2 <- d[,c(1,3,347:395)]
d2$interview_age <- d2$interview_age + d$Interview_day_difference/365

colnames(d1)[3:51] <- str_replace(colnames(d1)[3:51], " - Household", "")
colnames(d2)[3:51] <- str_replace(colnames(d2)[3:51], " - Neuroimage", "")

T1 <- d1 %>% select(`P-factor (Achenbach 2S)`:`Att. (Clark 4S)`)
T2 <- d2 %>% select(`P-factor (Achenbach 2S)`:`Att. (Clark 4S)`)
#T1 <- d[,298:346]
#T2 <- d[,347:395]

factors <- colnames(T1)
cor_rel <- calculate_corrected_test_retest(factors,d1,d2)


d %>% group_by(Selection) %>% summarise_if(is.numeric, list(mean,sd), na.rm = TRUE)

cormat <- cor(T1,T2,use = "pairwise.complete.obs")
rel <- cormat[row(cormat)==col(cormat)]

#colnames(cormat)<-str_replace(colnames(cormat), " - Neuroimage", "")

ICCs <- calculate_rel_ICC(T1,T2)

df_bhrc <- cbind(data.frame('Factor' = colnames(cormat), 'Reliability_r' = rel, 'Reliability_r_corrected' = cor_rel[1:length(rel)]), ICCs, 'Dataset' = 'BHRC(3/71)')

df <- rbind(df,df_bhrc)



if (all(rep(column_mapping_df$new_name,6) == df$Factor)) {
  df$Factor_short = rep(column_mapping_df$old_name)
}

write_csv(df, '/home/mgell/Work/pfactor_reliability/data/all_pfactor_reliability.csv')


cor.test(df$Reliability_r_corrected[df$Dataset == 'BHRC(6/235)'],df$rel_ICC3[df$Dataset == 'BHRC(6/235)'])




colorscheme = c("#D95F02", "#E6AB02", "#1B9E77",
                "#313695", "#7570B3", "#E7298A")


# Define the desired order for the Dataset variable
df$Dataset <- factor(df$Dataset, levels = c("ABCD(20/10897)", "ABCD(12/7250)", "ABCD(10/1215)", 
                                            "BHRC(17/772)", "BHRC(6/235)", "BHRC(3/71)"))

# Now plot with the reordered Dataset variable
plt = ggplot(data = df, aes(x = Factor, y = Reliability_r_corrected, group = as.factor(Dataset), fill = as.factor(Dataset))) +
  geom_bar(stat = "identity", width = .75, position = 'dodge') +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90),
        axis.title.x=element_blank()) +
  ylim(c(0,1.0)) +
  ylab('Correlation(T1,T2)') +
  guides(fill = guide_legend(title = "Dataset")) +
  scale_fill_manual(values = colorscheme) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13))

print(plt)

ggsave(paste0(plt_out,'all_datasets_test_retest_corrected_CBCL_pfactor_all_intervals.png'), plot = plt, widt = 11, height = 4.5)





colorscheme2 = c("#D95F02","#313695")

dd <- df

dd$Dataset <- fct_recode(dd$Dataset,"ABCD" = "ABCD(20/10897)","BHRC" = "BHRC(6/235)")

dd <- dd[dd$Dataset %in% c("ABCD","BHRC"),]

plt = ggplot(data = dd, aes(x = Factor, y = Reliability_r_corrected, group = as.factor(Dataset), fill = as.factor(Dataset))) +
  geom_bar(stat = "identity", width = .75, position = 'dodge') +
  #geom_errorbar(aes(ymin = rel_ICC_lower, ymax = rel_ICC_upper), width = 0.2, position = position_dodge(0.75)) + # Adding error bars
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90),axis.title.x = element_blank()) +
  ylim(c(0,1.0)) +
  ylab('Correlation(T1,T2)') +
  guides(fill = guide_legend(title = "Dataset")) +
  scale_fill_manual(values = colorscheme2) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13))

plt

ggsave(paste0(plt_out,'all_datasets_test_retest_corrected_CBCL_pfactor.png'), plot = plt, widt = 11, height = 4.5)


dd$grp <- 'Other'
dd$grp[grepl('P-factor', dd$Factor, fixed = TRUE)] <- 'P-Factor'
dd$grp[grepl('Att', dd$Factor, fixed = TRUE)] <- 'Attention'
dd$grp[grepl('Int', dd$Factor, fixed = TRUE)] <- 'Internalising'
dd$grp[grepl('Ext', dd$Factor, fixed = TRUE)] <- 'Externalising'

plt = ggplot(data = dd, aes(x = grp, y = Reliability_r_corrected, group = as.factor(Dataset), fill = as.factor(Dataset))) +
  geom_bar(stat = "identity", width = .75, position = 'dodge') +
  #geom_errorbar(aes(ymin = rel_ICC_lower, ymax = rel_ICC_upper), width = 0.2, position = position_dodge(0.75)) + # Adding error bars
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90),axis.title.x = element_blank()) +
  ylim(c(0,1.0)) +
  ylab('Correlation(T1,T2)') +
  guides(fill = guide_legend(title = "Dataset")) +
  scale_fill_manual(values = colorscheme2) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13))

plt

ggsave(paste0(plt_out,'all_datasets_test_retest_corrected_CBCL_pfactor_mean.png'), plot = plt, widt = 4, height = 4.5)

dd %>%
  group_by(Dataset, grp) %>%  
  summarise(across(Reliability_r_corrected, list(
    mean = ~mean(.x, na.rm = TRUE),
    min = ~min(.x, na.rm = TRUE),
    max = ~max(.x, na.rm = TRUE)
    )))






# ICC
plt = ggplot(data = df, aes(x = Factor, y = rel_ICC3, group = as.factor(Dataset), fill = as.factor(Dataset))) +
  geom_bar(stat = "identity", width = .75, position = 'dodge') +
  #geom_errorbar(aes(ymin = rel_ICC_lower, ymax = rel_ICC_upper), width = 0.2, position = position_dodge(0.75)) + # Adding error bars
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90),axis.title.x = element_blank()) +
  ylim(c(0,1.0)) +
  ylab('ICC(3,1)') +
  guides(fill = guide_legend(title = "Dataset")) +
  scale_fill_manual(values = colorscheme) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13))

plt

ggsave(paste0(plt_out,'all_datasets_ICC_CBCL_pfactor.png'), plot = plt, widt = 11, height = 4.5)
#ggsave(paste0(plt_out,'all_datasets_ICC_CBCL_pfactor.png'), plot = plt, widt = 13, height = 4.5)


# Correlate those:
cor.test(df$rel_ICC3,df$Reliability_r)





# BHRC Community and high risk samples separately
# load stuff
data = readRDS('/home/mgell/Work/pfactor_reliability/data/BHRC_CBCL_Household_Neuroimage_scores.Rds')
#consistency = read.csv('/home/mgell/Work/pfactor_reliability/data/reliability_consistency_models.csv')
data = data[-595,]
colnames(data)[3] <- 'interview_age'

data$Interview_day_difference = abs(data$Interview_day_difference)

d = data
d = d[d$Selection == 'Community',]


sum(d$Gender == 'Female')
max(d$interview_age)
min(d$interview_age)



d %>% group_by(Selection) %>% summarise_if(is.numeric, list(mean,sd), na.rm = TRUE)

d1 <- d[,c(1,3,298:346)]
d2 <- d[,c(1,3,347:395)]
d2$interview_age <- d2$interview_age + d$Interview_day_difference/365

colnames(d1)[3:51] <- str_replace(colnames(d1)[3:51], " - Household", "")
colnames(d2)[3:51] <- str_replace(colnames(d2)[3:51], " - Neuroimage", "")

T1 <- d1 %>% select(`P-factor (Achenbach 2S)`:`Att. (Clark 4S)`)
T2 <- d2 %>% select(`P-factor (Achenbach 2S)`:`Att. (Clark 4S)`)
#T1 <- d[,298:346]
#T2 <- d[,347:395]

factors <- colnames(T1)
cor_rel <- calculate_corrected_test_retest(factors,d1,d2)

cormat <- cor(T1,T2,use = "pairwise.complete.obs")
rel <- cormat[row(cormat)==col(cormat)]

ICCs <- calculate_rel_ICC(T1,T2)

df_bhrc <- cbind(data.frame('Factor' = colnames(cormat), 'Reliability_r' = rel, 'Reliability_r_corrected' = cor_rel[1:length(rel)]), ICCs,
                 'Dataset' = paste0('BHRC-Community(',round(max(d$Interview_day_difference)/30.44),'/',length(T1$`P-factor (Achenbach 2S)`), ')'))

df <- df_bhrc


# BHRC High risk
#data = readRDS('/home/mgell/Work/pfactor_reliability/data/BHRC_CBCL_Household_Neuroimage_scores.Rds')

d = data
d = d[d$Selection != 'Community',]


sum(d$Gender == 'Female')
max(d$interview_age)
min(d$interview_age)


d %>% group_by(Selection) %>% summarise_if(is.numeric, list(mean,sd), na.rm = TRUE)

d1 <- d[,c(1,3,298:346)]
d2 <- d[,c(1,3,347:395)]
d2$interview_age <- d2$interview_age + d$Interview_day_difference/365

colnames(d1)[3:51] <- str_replace(colnames(d1)[3:51], " - Household", "")
colnames(d2)[3:51] <- str_replace(colnames(d2)[3:51], " - Neuroimage", "")

T1 <- d1 %>% select(`P-factor (Achenbach 2S)`:`Att. (Clark 4S)`)
T2 <- d2 %>% select(`P-factor (Achenbach 2S)`:`Att. (Clark 4S)`)
#T1 <- d[,298:346]
#T2 <- d[,347:395]

factors <- colnames(T1)
cor_rel <- calculate_corrected_test_retest(factors,d1,d2)


cormat <- cor(T1,T2,use = "pairwise.complete.obs")
rel <- cormat[row(cormat)==col(cormat)]

ICCs <- calculate_rel_ICC(T1,T2)

df_bhrc <- cbind(data.frame('Factor' = colnames(cormat), 'Reliability_r' = rel, 'Reliability_r_corrected' = cor_rel[1:length(rel)]), ICCs,
                 'Dataset' = paste0('BHRC-Highrisk(',round(max(d$Interview_day_difference)/30.44),'/',length(T1$`P-factor (Achenbach 2S)`), ')'))

df <- rbind(df,df_bhrc)





# BHRC Community and high risk at 
# 6 months
#data = readRDS('/home/mgell/Work/pfactor_reliability/data/BHRC_CBCL_Household_Neuroimage_scores.Rds')
# Community
d = data
d = d[d$Interview_day_difference <= 180,]
d = d[d$Selection == 'Community',]

d %>% group_by(Selection) %>% summarise_if(is.numeric, list(mean,sd), na.rm = TRUE)

d1 <- d[,c(1,3,298:346)]
d2 <- d[,c(1,3,347:395)]
d2$interview_age <- d2$interview_age + d$Interview_day_difference/365

colnames(d1)[3:51] <- str_replace(colnames(d1)[3:51], " - Household", "")
colnames(d2)[3:51] <- str_replace(colnames(d2)[3:51], " - Neuroimage", "")

T1 <- d1 %>% select(`P-factor (Achenbach 2S)`:`Att. (Clark 4S)`)
T2 <- d2 %>% select(`P-factor (Achenbach 2S)`:`Att. (Clark 4S)`)
#T1 <- d[,298:346]
#T2 <- d[,347:395]

factors <- colnames(T1)
cor_rel <- calculate_corrected_test_retest(factors,d1,d2)


cormat <- cor(T1,T2,use = "pairwise.complete.obs")
rel <- cormat[row(cormat)==col(cormat)]

ICCs <- calculate_rel_ICC(T1,T2)

df_bhrc <- cbind(data.frame('Factor' = colnames(cormat), 'Reliability_r' = rel, 'Reliability_r_corrected' = cor_rel[1:length(rel)]), ICCs,
                 'Dataset' = paste0('BHRC-Community(',round(max(d$Interview_day_difference)/30.44),'/',length(T1$`P-factor (Achenbach 2S)`), ')'))

df <- rbind(df,df_bhrc)

ddd <- df_bhrc
ddd$model <- 'specific'
ddd$model[grepl('P-factor', ddd$Factor, fixed = TRUE)] <- 'general'

mean(ddd$Reliability_r_corrected[ddd$model == 'general'])
max(ddd$Reliability_r_corrected[ddd$model == 'general'])
min(ddd$Reliability_r_corrected[ddd$model == 'general'])

mean(ddd$Reliability_r_corrected[ddd$model == 'specific'])
max(ddd$Reliability_r_corrected[ddd$model == 'specific'])
min(ddd$Reliability_r_corrected[ddd$model == 'specific'])




# High risk
# 6 months
d = data
d = d[d$Interview_day_difference <= 180,]
d = d[d$Selection != 'Community',]

d %>% group_by(Selection) %>% summarise_if(is.numeric, list(mean,sd), na.rm = TRUE)

d1 <- d[,c(1,3,298:346)]
d2 <- d[,c(1,3,347:395)]
d2$interview_age <- d2$interview_age + d$Interview_day_difference/365

colnames(d1)[3:51] <- str_replace(colnames(d1)[3:51], " - Household", "")
colnames(d2)[3:51] <- str_replace(colnames(d2)[3:51], " - Neuroimage", "")

T1 <- d1 %>% select(`P-factor (Achenbach 2S)`:`Att. (Clark 4S)`)
T2 <- d2 %>% select(`P-factor (Achenbach 2S)`:`Att. (Clark 4S)`)
#T1 <- d[,298:346]
#T2 <- d[,347:395]

factors <- colnames(T1)
cor_rel <- calculate_corrected_test_retest(factors,d1,d2)

cormat <- cor(T1,T2,use = "pairwise.complete.obs")
rel <- cormat[row(cormat)==col(cormat)]

ICCs <- calculate_rel_ICC(T1,T2)

df_bhrc <- cbind(data.frame('Factor' = colnames(cormat), 'Reliability_r' = rel, 'Reliability_r_corrected' = cor_rel[1:length(rel)]), ICCs,
                 'Dataset' = paste0('BHRC-Highrisk(',round(max(d$Interview_day_difference)/30.44),'/',length(T1$`P-factor (Achenbach 2S)`), ')'))

df <- rbind(df,df_bhrc)


ddd <- df_bhrc
ddd$model <- 'specific'
ddd$model[grepl('P-factor', ddd$Factor, fixed = TRUE)] <- 'general'

mean(ddd$Reliability_r_corrected[ddd$model == 'general'])
max(ddd$Reliability_r_corrected[ddd$model == 'general'])
min(ddd$Reliability_r_corrected[ddd$model == 'general'])

mean(ddd$Reliability_r_corrected[ddd$model == 'specific'])
max(ddd$Reliability_r_corrected[ddd$model == 'specific'])
min(ddd$Reliability_r_corrected[ddd$model == 'specific'])





dff <- df[df$Dataset %in% c("BHRC-Community(6/109)","BHRC-Highrisk(6/125)"),]

colorscheme3 = c("#E6AB02", "#1B9E77")

# PLOT
plt = ggplot(data = dff, aes(x = Factor, y = Reliability_r_corrected, group = as.factor(Dataset), fill = as.factor(Dataset))) +
  geom_bar(stat = "identity", width = .75, position = 'dodge') +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90),
        axis.title.x=element_blank()) +
  ylim(c(0,1.0)) +
  ylab('Correlation(T1,T2)') +
  guides(fill = guide_legend(title = "Dataset")) +
  scale_fill_manual(values = colorscheme3) +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
              axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13))

plt

ggsave(paste0(plt_out,'BHRC_test_retest_correctd_CBCL_pfactor.png'), plot = plt, widt = 11, height = 4.5)



