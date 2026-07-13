
library(tidyverse)
library(corrplot)
library(psych)
library(lme4)


old_path = '/home/mgell/Work/pfactor_reliability/'
new_path = '/Users/mgell/Work/pfactor/'

plt_out = paste0(new_path,"plots/")


colorscheme = c("#D95F02", "#E6AB02", "#1B9E77",
                "#313695", "#7570B3", "#E7298A")

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
subs_w_factors = read_csv(paste0(new_path,'res/ABCD_CBCL_factor_scores_baseline.csv')) # for sublist


df1 = read_csv(paste0(new_path,'data/summary_scores/ABCD_CBCL_baseline.csv'))
df2 = read_csv(paste0(new_path,'data/summary_scores/ABCD_CBCL_followup.csv'))


df1 <- df1 %>% filter(EID %in% c(subs_w_factors$EID))
df2 <- df2 %>% filter(EID %in% c(subs_w_factors$EID))

df1 <- df1[order(df1$EID),]
df2 <- df2[order(df2$EID),]

all(df1$EID == df2$EID)

df1$retest_interval <- df2$interview_age - df1$interview_age

df2 <- df2[df1$retest_interval > 1,]
df1 <- df1[df1$retest_interval > 1,] # 1 sub with retest interval of 1 - likely a mistake?

print(mean(df1$retest_interval))
print(min(df1$retest_interval))
print(max(df1$retest_interval))


# T scores

# df1 <- df1 %>% select(EID,Interview,interview_age,
#                      cbcl_scr_syn_anxdep_r,cbcl_scr_syn_withdep_r,cbcl_scr_syn_somatic_r,cbcl_scr_syn_social_r,
#                      cbcl_scr_syn_thought_r,cbcl_scr_syn_attention_r,cbcl_scr_syn_rulebreak_r,cbcl_scr_syn_aggressive_r,
#                      cbcl_scr_syn_internal_r,cbcl_scr_syn_external_r,cbcl_scr_syn_totprob_r)

df1 <- df1 %>% select(EID,Interview,interview_age,
                     cbcl_scr_syn_anxdep_t,cbcl_scr_syn_withdep_t,cbcl_scr_syn_somatic_t,cbcl_scr_syn_social_t,
                     cbcl_scr_syn_thought_t,cbcl_scr_syn_attention_t,cbcl_scr_syn_rulebreak_t,cbcl_scr_syn_aggressive_t,
                     cbcl_scr_syn_internal_t,cbcl_scr_syn_external_t,cbcl_scr_syn_totprob_t,retest_interval)

# df2 <- df2 %>% select(EID,Interview,interview_age,
#                      cbcl_scr_syn_anxdep_r,cbcl_scr_syn_withdep_r,cbcl_scr_syn_somatic_r,cbcl_scr_syn_social_r,
#                      cbcl_scr_syn_thought_r,cbcl_scr_syn_attention_r,cbcl_scr_syn_rulebreak_r,cbcl_scr_syn_aggressive_r,
#                      cbcl_scr_syn_internal_r,cbcl_scr_syn_external_r,cbcl_scr_syn_totprob_r)

df2 <- df2 %>% select(EID,Interview,interview_age,
                     cbcl_scr_syn_anxdep_t,cbcl_scr_syn_withdep_t,cbcl_scr_syn_somatic_t,cbcl_scr_syn_social_t,
                     cbcl_scr_syn_thought_t,cbcl_scr_syn_attention_t,cbcl_scr_syn_rulebreak_t,cbcl_scr_syn_aggressive_t,
                     cbcl_scr_syn_internal_t,cbcl_scr_syn_external_t,cbcl_scr_syn_totprob_t)




# 20 months - whole data
all(df1$EID == df2$EID)

factors <- colnames(df2)[4:length(df2)]
cor_rel <- calculate_corrected_test_retest(factors,df1,df2)

T1 <- df1 %>% select(cbcl_scr_syn_anxdep_t:cbcl_scr_syn_totprob_t)
T2 <- df2 %>% select(cbcl_scr_syn_anxdep_t:cbcl_scr_syn_totprob_t)

cormat <- cor(T1,T2,use = "pairwise.complete.obs")#,method = "spearman")
rel <- cormat[row(cormat)==col(cormat)]

ICCs <- calculate_rel_ICC(T1,T2)

df <- cbind(data.frame('section_totals' = colnames(cormat), 'Reliability_r' = rel, 'Reliability_r_corrected' = cor_rel[1:length(rel)]),ICCs)
df$Dataset <- paste0('ABCD(',max(df1$retest_interval),'/',length(df1$EID),')')

mean(df$Reliability_r_corrected)


# ABCD 1 year only
df1y <- df1[df1$retest_interval <= 12,]
df2y <- df2 %>% filter(EID %in% c(df1y$EID))

all(df1y$EID == df2y$EID)

cor_rel <- calculate_corrected_test_retest(factors,df1y,df2y)

T1 <- df1y %>% select(cbcl_scr_syn_anxdep_t:cbcl_scr_syn_totprob_t)
T2 <- df2y %>% select(cbcl_scr_syn_anxdep_t:cbcl_scr_syn_totprob_t)

cormat <- cor(T1,T2,use = "pairwise.complete.obs")
rel <- cormat[row(cormat)==col(cormat)]

ICCs <- calculate_rel_ICC(T1,T2)

df_year <- cbind(data.frame('section_totals' = colnames(cormat), 'Reliability_r' = rel, 'Reliability_r_corrected' = cor_rel[1:length(rel)]),ICCs)
df_year$Dataset <- paste0('ABCD(',max(df1y$retest_interval),'/',length(df1y$EID),')')

df <- rbind(df,df_year)



# ABCD 10m only
df1y <- df1[df1$retest_interval <= 10,]
df2y <- df2 %>% filter(EID %in% c(df1y$EID))

all(df1y$EID == df2y$EID)

cor_rel <- calculate_corrected_test_retest(factors,df1y,df2y)

T1 <- df1y %>% select(cbcl_scr_syn_anxdep_t:cbcl_scr_syn_totprob_t)
T2 <- df2y %>% select(cbcl_scr_syn_anxdep_t:cbcl_scr_syn_totprob_t)

cormat <- cor(T1,T2,use = "pairwise.complete.obs")
rel <- cormat[row(cormat)==col(cormat)]

ICCs <- calculate_rel_ICC(T1,T2)

df_year <- cbind(data.frame('section_totals' = colnames(cormat), 'Reliability_r' = rel, 'Reliability_r_corrected' = cor_rel[1:length(rel)]),ICCs)
df_year$Dataset <- paste0('ABCD(',max(df1y$retest_interval),'/',length(df1y$EID),')')

df <- rbind(df,df_year)

#write_csv(df, '/home/mgell/Work/pfactor_reliability/data/abcd_sumscores_reliability.csv')


#check these are the same actually - they are
names = c("CBCL_Anx_Dep","CBCL_Withdraw_Dep","CBCL_Somatic","CBCL_social","CBCL_Thought",
          "CBCL_Attention","CBCL_Rule_break","CBCL_aggress","CBCL_Internalizing",
          "CBCL_Externalizing","CBCL_Total_score")

df$section_totals = rep(names)




### BHRC ###
# full sample
data = readRDS(paste0(new_path,'data/BHRC_CBCL_Household_Neuroimage_scores.Rds'))
colnames(data)[3] <- 'interview_age'

data = data[-595,] # missing info

data$Interview_day_difference = abs(data$Interview_day_difference)

data = as.data.frame(lapply(data, function(x) { attributes(x) <- NULL; x }))

d = data
d1 <- d[,c(1,3,261:272)]
d2 <- d[,c(1,3,273:284)]
d2$interview_age <- d2$interview_age + d$Interview_day_difference/365

# retest interval
print(mean(d$Interview_day_difference)/30.44)
print(min(d$Interview_day_difference)/30.44)
print(max(d$Interview_day_difference)/30.44)

# calculate reliability
colnames(d2)[3:14] <- str_replace(colnames(d2)[3:14], "Neuro_", "")

T1 <- d1 %>% select(`CBCL_Anx_Dep`:`CBCL_Total_score`)
T2 <- d2 %>% select(`CBCL_Anx_Dep`:`CBCL_Total_score`)

factors <- colnames(T1)
cor_rel <- calculate_corrected_test_retest(factors,d1,d2)

d %>% group_by(Selection) %>% summarise_if(is.numeric, list(mean,sd), na.rm = TRUE)

cormat <- cor(T1,T2,use = "pairwise.complete.obs")
rel <- cormat[row(cormat)==col(cormat)]

ICCs <- calculate_rel_ICC(T1,T2)

df_bhrc <- cbind(data.frame('section_totals' = colnames(cormat), 'Reliability_r' = rel, 'Reliability_r_corrected' = cor_rel[1:length(rel)]), ICCs,
                 'Dataset' = paste0('BHRC(',round(max(d$Interview_day_difference)/30.44),'/',length(T1$CBCL_Anx_Dep), ')'))

df <- rbind(df,df_bhrc)

mean(df_bhrc$Reliability_r_corrected)
max(df_bhrc$Reliability_r_corrected)
min(df_bhrc$Reliability_r_corrected)





# 6 months
d = data[data$Interview_day_difference <= 180,]
d1 <- d[,c(1,3,261:272)]
d2 <- d[,c(1,3,273:284)]
d2$interview_age <- d2$interview_age + d$Interview_day_difference/365

# retest interval
print(mean(d$Interview_day_difference)/30.44)
print(min(d$Interview_day_difference)/30.44)
print(max(d$Interview_day_difference)/30.44)

# calculate reliability
colnames(d2)[3:14] <- str_replace(colnames(d2)[3:14], "Neuro_", "")

T1 <- d1 %>% select(`CBCL_Anx_Dep`:`CBCL_Total_score`)
T2 <- d2 %>% select(`CBCL_Anx_Dep`:`CBCL_Total_score`)

factors <- colnames(T1)
cor_rel <- calculate_corrected_test_retest(factors,d1,d2)

d %>% group_by(Selection) %>% summarise_if(is.numeric, list(mean,sd), na.rm = TRUE)

cormat <- cor(T1,T2,use = "pairwise.complete.obs")
rel <- cormat[row(cormat)==col(cormat)]

ICCs <- calculate_rel_ICC(T1,T2)

df_bhrc <- cbind(data.frame('section_totals' = colnames(cormat), 'Reliability_r' = rel, 'Reliability_r_corrected' = cor_rel[1:length(rel)]), ICCs,
                 'Dataset' = paste0('BHRC(',round(max(d$Interview_day_difference)/30.44),'/',length(T1$CBCL_Anx_Dep), ')'))

df <- rbind(df,df_bhrc)

mean(df_bhrc$Reliability_r_corrected)
max(df_bhrc$Reliability_r_corrected)
min(df_bhrc$Reliability_r_corrected)




# 3 months
d = data[data$Interview_day_difference <= 90,]
d1 <- d[,c(1,3,261:272)]
d2 <- d[,c(1,3,273:284)]
d2$interview_age <- d2$interview_age + d$Interview_day_difference/365

# retest interval
print(mean(d$Interview_day_difference)/30.44)
print(min(d$Interview_day_difference)/30.44)
print(max(d$Interview_day_difference)/30.44)

# calculate reliability
colnames(d2)[3:14] <- str_replace(colnames(d2)[3:14], "Neuro_", "")

T1 <- d1 %>% select(`CBCL_Anx_Dep`:`CBCL_Total_score`)
T2 <- d2 %>% select(`CBCL_Anx_Dep`:`CBCL_Total_score`)

factors <- colnames(T1)
cor_rel <- calculate_corrected_test_retest(factors,d1,d2)

d %>% group_by(Selection) %>% summarise_if(is.numeric, list(mean,sd), na.rm = TRUE)

cormat <- cor(T1,T2,use = "pairwise.complete.obs")
rel <- cormat[row(cormat)==col(cormat)]

ICCs <- calculate_rel_ICC(T1,T2)

df_bhrc <- cbind(data.frame('section_totals' = colnames(cormat), 'Reliability_r' = rel, 'Reliability_r_corrected' = cor_rel[1:length(rel)]), ICCs,
                 'Dataset' = paste0('BHRC(',round(max(d$Interview_day_difference)/30.44),'/',length(T1$CBCL_Anx_Dep), ')'))

df <- rbind(df,df_bhrc)



### PLOT ###
df = df[!df$section_totals == 'CBCL_Other',]
names = c("Anxious Depressed","Withdrawn Depressed","Somatic Complaints","Social Problems","Thought Problems",
          "Attention Problems","Rule Breaking","Aggression Problems","Internalising",
          "Externalising","Tototal problems")

df$section_totals = rep(names)


df$Dataset <- factor(df$Dataset, levels = c("ABCD(20/10897)", "ABCD(12/7250)", "ABCD(10/1215)", 
                                            "BHRC(17/771)", "BHRC(6/234)", "BHRC(3/69)"))

# Now plot with the reordered Dataset variable
plt = ggplot(data = df, aes(x = section_totals, y = Reliability_r_corrected, group = as.factor(Dataset), fill = as.factor(Dataset))) +
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

ggsave(paste0(plt_out,'all_datasets_test_retest_corrected_CBCL_all_intervals.png'), plot = plt, widt = 5, height = 4)




# PLOT ONLY subsection of samples for main results

colorscheme2 = c("#D95F02","#313695")

dd <- df

dd$Dataset <- fct_recode(dd$Dataset,"ABCD" = "ABCD(20/10897)","BHRC" = "BHRC(6/234)")

dd <- dd[dd$Dataset %in% c("ABCD","BHRC"),]

plt = ggplot(data = dd, aes(x = section_totals, y = Reliability_r_corrected, group = as.factor(Dataset), fill = as.factor(Dataset))) +
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

ggsave(paste0(plt_out,'all_datasets_test_retest_corrected_CBCL.png'), plot = plt, widt = 5, height = 4)







# BHRC Community and high risk samples separately
# load stuff
data = readRDS('/home/mgell/Work/pfactor_reliability/data/BHRC_CBCL_Household_Neuroimage_scores.Rds')
#consistency = read.csv('/home/mgell/Work/pfactor_reliability/data/reliability_consistency_models.csv')
colnames(data)[3] <- 'interview_age'

data = data[-595,] # missing info

data$Interview_day_difference = abs(data$Interview_day_difference)

data = as.data.frame(lapply(data, function(x) { attributes(x) <- NULL; x }))

d = data
d = d[d$Selection == 'Community',]

d1 <- d[,c(1,3,261:272)]
d2 <- d[,c(1,3,273:284)]
d2$interview_age <- d2$interview_age + d$Interview_day_difference/365

# retest interval
print(mean(d$Interview_day_difference)/30.44)
print(min(d$Interview_day_difference)/30.44)
print(max(d$Interview_day_difference)/30.44)

# calculate reliability
colnames(d2)[3:14] <- str_replace(colnames(d2)[3:14], "Neuro_", "")

T1 <- d1 %>% select(`CBCL_Anx_Dep`:`CBCL_Total_score`)
T2 <- d2 %>% select(`CBCL_Anx_Dep`:`CBCL_Total_score`)

factors <- colnames(T1)
cor_rel <- calculate_corrected_test_retest(factors,d1,d2)

cormat <- cor(T1,T2,use = "pairwise.complete.obs")
rel <- cormat[row(cormat)==col(cormat)]

ICCs <- calculate_rel_ICC(T1,T2)

df_bhrc <- cbind(data.frame('section_totals' = colnames(cormat), 'Reliability_r' = rel, 'Reliability_r_corrected' = cor_rel[1:length(rel)]), ICCs,
                 'Dataset' = paste0('BHRC-Community(',round(max(d$Interview_day_difference)/30.44),'/',length(T1$CBCL_Anx_Dep), ')'))

df <- df_bhrc


# High risk
d = data
d = d[d$Selection != 'Community',]

d1 <- d[,c(1,3,261:272)]
d2 <- d[,c(1,3,273:284)]
d2$interview_age <- d2$interview_age + d$Interview_day_difference/365

# retest interval
print(mean(d$Interview_day_difference)/30.44)
print(min(d$Interview_day_difference)/30.44)
print(max(d$Interview_day_difference)/30.44)

# calculate reliability
colnames(d2)[3:14] <- str_replace(colnames(d2)[3:14], "Neuro_", "")

T1 <- d1 %>% select(`CBCL_Anx_Dep`:`CBCL_Total_score`)
T2 <- d2 %>% select(`CBCL_Anx_Dep`:`CBCL_Total_score`)

factors <- colnames(T1)
cor_rel <- calculate_corrected_test_retest(factors,d1,d2)

cormat <- cor(T1,T2,use = "pairwise.complete.obs")
rel <- cormat[row(cormat)==col(cormat)]

ICCs <- calculate_rel_ICC(T1,T2)

df_bhrc <- cbind(data.frame('section_totals' = colnames(cormat), 'Reliability_r' = rel, 'Reliability_r_corrected' = cor_rel[1:length(rel)]), ICCs,
                 'Dataset' = paste0('BHRC-Highrisk(',round(max(d$Interview_day_difference)/30.44),'/',length(T1$CBCL_Anx_Dep), ')'))

df <- rbind(df,df_bhrc)


# BHRC Community and high risk
# 6 months
#data = readRDS('/home/mgell/Work/pfactor_reliability/data/BHRC_CBCL_Household_Neuroimage_scores.Rds')

d = data
d = d[d$Interview_day_difference <= 180,]
d = d[d$Selection == 'Community',]

d1 <- d[,c(1,3,261:272)]
d2 <- d[,c(1,3,273:284)]
d2$interview_age <- d2$interview_age + d$Interview_day_difference/365

# retest interval
print(mean(d$Interview_day_difference)/30.44)
print(min(d$Interview_day_difference)/30.44)
print(max(d$Interview_day_difference)/30.44)

# calculate reliability
colnames(d2)[3:14] <- str_replace(colnames(d2)[3:14], "Neuro_", "")

T1 <- d1 %>% select(`CBCL_Anx_Dep`:`CBCL_Total_score`)
T2 <- d2 %>% select(`CBCL_Anx_Dep`:`CBCL_Total_score`)

factors <- colnames(T1)
cor_rel <- calculate_corrected_test_retest(factors,d1,d2)

cormat <- cor(T1,T2,use = "pairwise.complete.obs")
rel <- cormat[row(cormat)==col(cormat)]

ICCs <- calculate_rel_ICC(T1,T2)

df_bhrc <- cbind(data.frame('section_totals' = colnames(cormat), 'Reliability_r' = rel, 'Reliability_r_corrected' = cor_rel[1:length(rel)]), ICCs,
                 'Dataset' = paste0('BHRC-Community(',round(max(d$Interview_day_difference)/30.44),'/',length(T1$CBCL_Anx_Dep), ')'))

df <- rbind(df,df_bhrc)

# High risk
d = data
d = d[d$Interview_day_difference <= 180,]
d = d[d$Selection != 'Community',]

d1 <- d[,c(1,3,261:272)]
d2 <- d[,c(1,3,273:284)]
d2$interview_age <- d2$interview_age + d$Interview_day_difference/365

# retest interval
print(mean(d$Interview_day_difference)/30.44)
print(min(d$Interview_day_difference)/30.44)
print(max(d$Interview_day_difference)/30.44)

# calculate reliability
colnames(d2)[3:14] <- str_replace(colnames(d2)[3:14], "Neuro_", "")

T1 <- d1 %>% select(`CBCL_Anx_Dep`:`CBCL_Total_score`)
T2 <- d2 %>% select(`CBCL_Anx_Dep`:`CBCL_Total_score`)

factors <- colnames(T1)
cor_rel <- calculate_corrected_test_retest(factors,d1,d2)

cormat <- cor(T1,T2,use = "pairwise.complete.obs")
rel <- cormat[row(cormat)==col(cormat)]

ICCs <- calculate_rel_ICC(T1,T2)

df_bhrc <- cbind(data.frame('section_totals' = colnames(cormat), 'Reliability_r' = rel, 'Reliability_r_corrected' = cor_rel[1:length(rel)]), ICCs,
                 'Dataset' = paste0('BHRC-Highrisk(',round(max(d$Interview_day_difference)/30.44),'/',length(T1$CBCL_Anx_Dep), ')'))

df <- rbind(df,df_bhrc)


### PLOT ###
df = df[!df$section_totals == 'CBCL_Other',]

names = c("Anxious Depressed","Withdrawn Depressed","Somatic Complaints","Social Problems","Thought Problems",
          "Attention Problems","Rule Breaking","Aggression Problems","Internalising",
          "Externalising","Tototal problems")

df$section_totals = rep(names)

dff <- df[df$Dataset %in% c("BHRC-Community(6/109)","BHRC-Highrisk(6/125)"),]

colorscheme3 = c("#E6AB02", "#1B9E77")

# PLOT
plt = ggplot(data = dff, aes(x = section_totals, y = Reliability_r_corrected, group = as.factor(Dataset), fill = as.factor(Dataset))) +
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

ggsave(paste0(plt_out,'BHRC_test_retest_correctd_CBCL.png'), plot = plt, widt = 5.5, height = 4)





# plt = ggplot(data = NULL, aes(x = colnames(df), y = as.numeric(df[1,]))) +
#   geom_bar(stat = "identity", width = .75) +
#   theme_classic() +
#   theme(axis.text.x = element_text(angle = 90),
#         axis.title.x=element_blank()) +
#   ylim(c(0,1.0)) +
#   ylab('correlation(T1,T2)')