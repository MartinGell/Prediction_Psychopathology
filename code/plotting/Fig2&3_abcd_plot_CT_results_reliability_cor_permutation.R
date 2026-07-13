
# Plot results

#library(ggrepel)
library(tidyverse)
library(RColorBrewer)


pallet <- c("#0072B2", "#009E73", "#D55E00", "#F0E442", "#CC79A7")

old_path = '/home/mgell/Work/pfactor_reliability/'
new_path = '/Users/mgell/Work/pfactor/'

plt_out = paste0(new_path,"plots/prediction/")

# Prediction accuracy FC
df <- read_csv(paste0(new_path,'res/collected/ridgeCV_zscore_group_2Fold_confound_removal_wcategorical_averaged-source_HCP2016FreeSurferSubcortical_abcd_baselineYear1Arm1_thickness_fsLR32k_6975-beh_abcd_cbcl_grps_model_fits_baseline_all_behs_permutation.csv'))

df <- df[-1,]

rel_cbcl <- read_csv(paste0(new_path,'data/abcd_sumscores_reliability.csv'))
rel_cbcl <- rel_cbcl[rel_cbcl$Dataset == "ABCD(12/7250)",]
colnames(rel_cbcl)[1] <- c('beh')
rel_cbcl$full_name <- 'cbcl'
rel_cbcl$Factor <- 'cbcl'


rel_p <- read_csv(paste0(new_path,'data/all_pfactor_reliability.csv'))
rel_p <- rel_p[rel_p$Dataset == "ABCD(12/7250)",]
rel_p <- rel_p %>% select(Factor_short,Reliability_r:Dataset,Factor)
colnames(rel_p)[1] <- c('beh')
colnames(rel_p)[8] <- c('full_name')

rel_p$Factor <- 'Other'
rel_p$Factor[grepl('P-factor', rel_p$full_name, fixed = TRUE)] <- 'P-Factor'
rel_p$Factor[grepl('Att', rel_p$full_name, fixed = TRUE)] <- 'Attention'
rel_p$Factor[grepl('Int', rel_p$full_name, fixed = TRUE)] <- 'Internalising'
rel_p$Factor[grepl('Ext', rel_p$full_name, fixed = TRUE)] <- 'Externalising'

con_p <- read_csv(paste0(new_path,'res/abcd_pfactor_consistency.csv'))
colnames(con_p)[1] <- c('beh')

sig <- read_csv(paste0(new_path,'res/collected/ridgeCV_zscore_group_2Fold_confound_removal_wcategorical_averaged-source_HCP2016FreeSurferSubcortical_abcd_baselineYear1Arm1_thickness_fsLR32k_6975-beh_abcd_cbcl_grps_model_fits_baseline_all_behs_signific.csv'))
#df$Factor <- as.factor(df$Factor)

# factor_short <- rel_p$Factor_short
# rel_p <- rel_p %>% select(-Factor_short)

rel <- rbind(rel_cbcl,rel_p)

rel$beh == df$beh

df <- left_join(rel,df, by='beh')
df <- left_join(df, sig, by='beh')

d <- df

df <- df[df$Factor != 'cbcl',]
df <- left_join(df,con_p, by="beh")

plt <- ggplot(data=df, aes(Reliability_r_corrected,test_Pearson_r,colour=Factor)) +
  #geom_point(data=df, aes(Reliability_r_corrected,test_Pearson_r,colour=Factor), size = 2) +
  geom_line(
    data = subset(df, Factor == "P-Factor"),
    stat = "smooth", method = "lm", se = FALSE, , alpha = 0.5, size = 1) +
  geom_point(data = ~subset(., sig == "1"), size = 2, alpha = 1) + 
  geom_point(data = ~subset(., sig == "0"), size = 2, shape = 1) +
  theme_classic() +
  ylab('Accuracy r(pred,observed)') + xlab('Test-retest Reliability (r)') +
  scale_x_continuous(limits = c(0.35,0.8), breaks = c(0.4,0.5,0.6,0.7,0.8)) +
  scale_y_continuous(limits = c(-0.01,0.11), breaks = c(0,0.02,0.04,0.06,0.08,0.10))+
  #scale_y_continuous(limits = c(-0.03,0.17), breaks = c(0,0.04,0.08,0.12,0.16))+
  scale_colour_manual(values = pallet, name = 'Factor') +
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 13),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13))

#ggsave(paste0(plt_out, 'TC_cor_rel_test_Pearson_r.png'), plt, width = 4.5, height = 3)
ggsave(paste0(plt_out, 'confs_CT_cor_rel_test_Pearson_r_sig.png'), plt, width = 4.5, height = 3)
# plt = plt + theme(axis.text=element_text(size=12),
#                   axis.title=element_text(size=12)) +
#   scale_x_continuous(limits = c(0.35,0.8), breaks = c(0.4,0.6,0.8))
# ggsave(paste0(plt_out, 'OHBM_confs_TC_cor_rel_test_Pearson_r_sig.png'), plt, width = 3.5, height = 2.5)




plt <- ggplot(data=df, aes(ECV_SG,test_Pearson_r,colour=Factor)) +
  #geom_line(stat = "smooth", method = 'lm', se = FALSE, aes(colour = Factor), alpha = 0.5, size = 1)  +
  geom_line(
    data = subset(df, Factor == "P-Factor"),
    stat = "smooth", method = "lm", se = FALSE, , alpha = 0.5, size = 1) +
  geom_point(data = ~subset(., sig == "1"), size = 2, alpha = 1) + 
  geom_point(data = ~subset(., sig == "0"), size = 2, shape = 1) +  
  theme_classic() +
  scale_x_continuous(limits = c(0,0.81), breaks = c(0,0.2,0.4,0.6,0.8)) +
  scale_y_continuous(limits = c(-0.01,0.11), breaks = c(0,0.02,0.04,0.06,0.08,0.10))+
  ylab('Accuracy r(pred,observed)') + xlab('Explained variance in CBCL items')  +
  scale_colour_manual(values = pallet, name = 'Factor') +
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 13),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13))

ggsave(paste0(plt_out, 'confs_CT_cor_ECV_SG_test_Pearson_r_sig.png'), plt, width = 4.5, height = 3)




### as bar plot
d_mean <- data.frame("p" = mean(df$test_Pearson_r[df$Factor == "P-Factor"]),
                     "internalising" = mean(df$test_Pearson_r[df$Factor == "Internalising"]),
                     "externalising" = mean(df$test_Pearson_r[df$Factor == "Externalising"]),
                     "attention" = mean(df$test_Pearson_r[df$Factor == "Attention"]),
                     "other" = mean(df$test_Pearson_r[df$Factor == "Other"]))
d_sd <- data.frame("p" = sd(df$test_Pearson_r[df$Factor == "P-Factor"]),
                   "internalising" = sd(df$test_Pearson_r[df$Factor == "Internalising"]),
                   "externalising" = sd(df$test_Pearson_r[df$Factor == "Externalising"]),
                   "attention" = sd(df$test_Pearson_r[df$Factor == "Attention"]),
                   "other" = sd(df$test_Pearson_r[df$Factor == "Other"]))
df_mean <- data.frame("mean_accuracy" = t(d_mean),"sd_accuracy" = t(d_sd), "Factor" = colnames(d_mean))

plt = ggplot(data=df_mean, aes(Factor,mean_accuracy, fill = Factor)) +
  geom_col() +
  geom_bar(stat = "identity", width = .75, position = 'dodge') +
  geom_errorbar(aes(ymin = mean_accuracy - 1*sd_accuracy, ymax = mean_accuracy + 1*sd_accuracy), width = 0.2, position = position_dodge(0.75)) +
  ylab('Mean Accuracy') +
  theme_classic() + 
  scale_y_continuous(limits = c(-0.02,0.17), breaks = c(0,0.08,0.16))+
  scale_fill_manual(values = pallet, name = 'Factor') +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 12))
ggsave(paste0(plt_out, 'confs_CT_mean_accuracy_test_Pearson_r.png'), plt, width = 1.6, height = 1.6)



data.frame("p" = mean(df$test_R2[df$Factor == "P-Factor"]),
                     "internalising" = mean(df$test_R2[df$Factor == "Internalising"]),
                     "externalising" = mean(df$test_R2[df$Factor == "Externalising"]),
                     "attention" = mean(df$test_R2[df$Factor == "Attention"]),
                     "other" = mean(df$test_R2[df$Factor == "Other"]))



# TESTING
# first print 
pallet2 <- c("#CC79A7", "#D55E00", "#009E73", "#0072B2")

df_mean = df_mean[-5,]

df_mean$Factor <- factor(
  df_mean$Factor,
  levels = c("p",
             "externalising",
             "internalising",
             "attention"))

plt = ggplot(data=NULL, aes(Factor,mean_accuracy, fill = Factor)) +
  geom_col(data=df_mean, colour = 'black', alpha = 0.85) +
  scale_fill_manual(values = pallet2, name = 'Factor', labels = c("P-factor", "Externalising", "Internalising", "Attention")) +
  theme(legend.text = element_text(size = 12),
        legend.title = element_text(size = 13))

ggsave(paste0(plt_out, 'confs_FC_mean_accuracy_test_Pearson_r_factors&sum_scores_JUST_FOR_LEGEND.png'), plt, width = 3, height = 3.5)


pallet2 <- c("#e0afca", "#CC79A7", "#e69e66", "#D55E00", "#66c5ab", "#009E73", "#66aad1", "#0072B2")


d_mean <- data.frame("p" = mean(df$test_Pearson_r[df$Factor == "P-Factor"]),
                     "total problems sum score" = d$test_Pearson_r[d$beh ==  'cbcl_scr_syn_totprob_t'],
                     "externalising factors"    = mean(df$test_Pearson_r[df$Factor == "Externalising"]),
                     "externalising sum score"  = d$test_Pearson_r[d$beh == 'cbcl_scr_syn_external_t'],
                     "internalising factors"    = mean(df$test_Pearson_r[df$Factor == "Internalising"]),
                     "internalising sum score"  = d$test_Pearson_r[d$beh == 'cbcl_scr_syn_internal_t'],
                     "attention factors"        = mean(df$test_Pearson_r[df$Factor ==     "Attention"]),
                     "attention sum score"      = d$test_Pearson_r[d$beh == 'cbcl_scr_syn_attention_t'])
d_sd <- data.frame("p" = sd(df$test_Pearson_r[df$Factor == "P-Factor"]),
                   "total problems sum score" = 0,
                   "externalising factors"    = sd(df$test_Pearson_r[df$Factor == "Externalising"]),
                   "externalising sum score"  = 0,
                   "internalising factors"    = sd(df$test_Pearson_r[df$Factor == "Internalising"]),
                   "internalising sum score"  = 0,
                   "attention factors"        = sd(df$test_Pearson_r[df$Factor ==     "Attention"]),
                   "attention sum score"      = 0)
df_mean <- data.frame("mean_accuracy" = t(d_mean),"sd_accuracy" = t(d_sd), "Factor" = colnames(d_mean))


df_mean$Factor <- factor(
  df_mean$Factor,
  levels = c("total.problems.sum.score",
             "p",
             "externalising.sum.score",
             "externalising.factors",
             "internalising.sum.score",
             "internalising.factors",
             "attention.sum.score",
             "attention.factors"
  ))


plt = ggplot(data=NULL, aes(Factor,mean_accuracy, fill = Factor)) +
  #geom_col(data=df_mean) +
  geom_bar(data=df_mean, stat = "identity", position = 'dodge', alpha = 0.85, color = 'black', width = .85) +
  geom_errorbar(data=df_mean, aes(ymin = mean_accuracy - 1*sd_accuracy, ymax = mean_accuracy + 1*sd_accuracy), width = 0.2, position = position_dodge(0.75)) +
  ylab('Mean Accuracy') +
  theme_classic() + 
  scale_y_continuous(limits = c(0,0.17), breaks = c(0,0.08,0.16), expand = c(0, 0)) + 
  scale_x_discrete(
    #labels = c('Sum score','Factor', 'Sum score','Factor', 'Sum score','Factor', 'Sum score', 'Factor')) +
    labels = c('Total Problems','P-Factors','Extern. Sum Score','Extern. Factors','Intern. Sum Score','Intern. Factors','Atten. Sum Score', 'Atten. Factors')) +
  scale_fill_manual(values = pallet2, name = 'Factor') +
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 13),
        legend.position = "none",
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

#ggsave(paste0(plt_out, 'confs_CT_mean_accuracy_test_Pearson_r_factors&sum_scores.png'), plt, width = 3, height = 3.5)
ggsave(paste0(plt_out, 'confs_CT_mean_accuracy_test_Pearson_r_factors&sum_scores.png'), plt, width = 3, height = 4)



#R2
d_mean <- data.frame("p" = mean(df$test_R2[df$Factor == "P-Factor"]),
                     "total problems sum score" = d$test_R2[d$beh ==  'cbcl_scr_syn_totprob_t'],
                     "externalising factors"    = mean(df$test_R2[df$Factor == "Externalising"]),
                     "externalising sum score"  = d$test_R2[d$beh == 'cbcl_scr_syn_external_t'],
                     "internalising factors"    = mean(df$test_R2[df$Factor == "Internalising"]),
                     "internalising sum score"  = d$test_R2[d$beh == 'cbcl_scr_syn_internal_t'],
                     "attention factors"        = mean(df$test_R2[df$Factor ==     "Attention"]),
                     "attention sum score"      = d$test_R2[d$beh == 'cbcl_scr_syn_attention_t'])
d_sd <- data.frame("p" = sd(df$test_R2[df$Factor == "P-Factor"]),
                   "total problems sum score" = 0,
                   "externalising factors"    = sd(df$test_R2[df$Factor == "Externalising"]),
                   "externalising sum score"  = 0,
                   "internalising factors"    = sd(df$test_R2[df$Factor == "Internalising"]),
                   "internalising sum score"  = 0,
                   "attention factors"        = sd(df$test_R2[df$Factor ==     "Attention"]),
                   "attention sum score"      = 0)
df_mean <- data.frame("mean_accuracy" = t(d_mean),"sd_accuracy" = t(d_sd), "Factor" = colnames(d_mean))

df_mean


df_mean$Factor <- factor(
  df_mean$Factor,
  levels = c("total.problems.sum.score",
             "p",
             "externalising.sum.score",
             "externalising.factors",
             "internalising.sum.score",
             "internalising.factors",
             "attention.sum.score",
             "attention.factors"
  ))



plt = ggplot(data=NULL, aes(Factor,mean_accuracy, fill = Factor)) +
  #geom_col(data=df_mean) +
  geom_bar(data=df_mean, stat = "identity", position = 'dodge', alpha = 0.85, color = 'black', width = .85) +
  geom_errorbar(data=df_mean, aes(ymin = mean_accuracy - 1*sd_accuracy, ymax = mean_accuracy + 1*sd_accuracy), width = 0.2, position = position_dodge(0.75)) +
  ylab('Mean Accuracy (R2)') +
  theme_classic() + 
  scale_y_continuous(limits = c(-0.028,0.022), breaks = c(-0.02,-0.01,0,0.01,0.02), expand = c(0, 0)) + 
  scale_x_discrete(
    #labels = c('Sum score','Factor', 'Sum score','Factor', 'Sum score','Factor', 'Sum score', 'Factor')) +
    labels = c('Total Problems','P-Factors','Extern. Sum Score','Extern. Factors','Intern. Sum Score','Intern. Factors','Atten. Sum Score', 'Atten. Factors')) +
  scale_fill_manual(values = pallet2, name = 'Factor') +
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 13),
        legend.position = "none",
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

#ggsave(paste0(plt_out, 'confs_CT_mean_accuracy_test_R2_factors&sum_scores.png'), plt, width = 3, height = 3.5)
ggsave(paste0(plt_out, 'confs_CT_mean_accuracy_test_R2_factors&sum_scores.png'), plt, width = 3, height = 4)





#### With cbcl summary scores
cbcl_only <- d[d$Factor == 'cbcl',]
cbcl_only$beh <- str_replace(cbcl_only$beh, "cbcl_scr_syn_", "")
cbcl_only$beh <- str_replace(cbcl_only$beh, "_t", "")

names = c("Anx. Dep","With. Dep","Somatic","Social","Thought",
          "Attention","Rule break.","Aggres.","Intern.",
          "Extern.","Tot. problems")

cbcl_only$beh
names
cbcl_only$beh = names
#cbcl_only = cbcl_only[c(9,10,11),]
#cbcl_only$beh = c("Internalising","Externalising","Total Problems")

plt = ggplot() +
  geom_line(data=cbcl_only, aes(Reliability_r_corrected,test_Pearson_r), stat = "smooth", method = 'lm', se = FALSE, color='orangered4', alpha = 0.5, size = 1)  +
  #geom_point(data=~subset(df, sig == "1"), aes(Reliability_r_corrected,test_Pearson_r), colour = 'skyblue4', size = 2, alpha = 0.4) +
  #geom_point(data=~subset(df, sig == "0"), aes(Reliability_r_corrected,test_Pearson_r), colour = 'skyblue4', size = 2, shape = 1, alpha = 0.4) +
  geom_point(data=~subset(cbcl_only, sig == "1"), aes(Reliability_r_corrected,test_Pearson_r), colour='orangered4', size = 2) +
  geom_point(data=~subset(cbcl_only, sig == "0"), aes(Reliability_r_corrected,test_Pearson_r), colour='orangered4', size = 2, shape = 1) +
  theme_classic() +
  ylab('Accuracy r(pred,observed)') + xlab('Test-retest Reliability (r)') +
  scale_x_continuous(limits = c(0.47,0.8), breaks = c(0.4,0.5,0.6,0.7,0.8)) +
  scale_y_continuous(limits = c(-0.01,0.11), breaks = c(0,0.02,0.04,0.06,0.08,0.10))+
  geom_text_repel(data=cbcl_only, aes(Reliability_r_corrected,test_Pearson_r,label=beh), max.overlaps = 20) + 
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 13),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13))

ggsave(paste0(plt_out, 'confs_CT_cor_rel_test_Pearson_r_cbcl.png'), plt, width = 3.2, height = 3)



cbcl_only$X = 'Sum scores'
df2 <- df[,-c(31:37)]
df2$X = 'Factor'
df2 <- rbind(df2,cbcl_only)

df2$X <- factor(
  df2$X,
  levels = c(
    "Sum scores",
    "Factor"
  ))

model_colors <- c(
  "Factor" = 'skyblue4',
  "Sum scores" = 'orangered4')

model_colors <- c(
  "Factor" = 'gray25', #'skyblue4',
  "Sum scores" = 'gray85') #'orangered4')

plt = ggplot(data=df2, aes(X,test_Pearson_r)) +
  geom_boxplot(aes(fill = X), outliers = FALSE, alpha = 0.7) +
  # geom_point(aes(color = X), position = 'jitter') +
  geom_point(data=~subset(df2, sig == "1",), aes(color = X), size = 1, position = 'jitter', colour = 'black') +
  geom_point(data=~subset(df2, sig == "0"), aes(color = X), size = 1, shape = 1, position = 'jitter', colour = 'black') +
  theme_classic() +
  ylab('Accuracy r(pred,observed)') + xlab('') +
  scale_y_continuous(limits = c(-0.03,0.17), breaks = c(0,0.04,0.08,0.12,0.16)) +
  scale_colour_manual(values = model_colors, name = "Data") +
  scale_fill_manual(values = model_colors, name = "Data") +
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 13),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

#ggsave(paste0(plt_out, 'confs_CT_test_Pearson_r_cbcl_vs_factors.png'), plt, width = 3.7, height = 3.6)
ggsave(paste0(plt_out, 'confs_CT_test_Pearson_r_cbcl_vs_factors.pdf'), plt, width = 3.7, height = 3.6)


plt = ggplot(data=df2, aes(X,test_R2)) +
  geom_boxplot(aes(fill = X), outliers = FALSE, alpha = 0.7) +
  geom_point(aes(color = X), size = 1, position = 'jitter', colour = 'black') +
  #geom_point(data=~subset(df2, sig == "1",), aes(color = X), size = 1, position = 'jitter', colour = 'black') +
  #geom_point(data=~subset(df2, sig == "0"), aes(color = X), size = 1, shape = 1, position = 'jitter', colour = 'black') +
  theme_classic() +
  ylab('Accuracy (R2)') + xlab('') +
  scale_y_continuous(limits = c(-0.028,0.022), breaks = c(-0.02,-0.01,0,0.01,0.02))+
  scale_colour_manual(values = model_colors, name = "Data") +
  scale_fill_manual(values = model_colors, name = "Data") +
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 13),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
  
ggsave(paste0(plt_out, 'confs_CT_test_R2_cbcl_vs_factors.png'), plt, width = 3.7, height = 3.6)
  


