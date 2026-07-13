
# Plot results

#library(ggrepel)
library(tidyverse)
library(RColorBrewer)



pallet <- c("#0072B2", "#009E73", "#D55E00", "#F0E442", "#CC79A7")

old_path = '/home/mgell/Work/pfactor_reliability/'
new_path = '/Users/mgell/Work/pfactor/'

plt_out = paste0(new_path,"plots/prediction/")

# Prediction accuracy FC
df <- read_csv(paste0(new_path,
                      'res/collected/ridgeCV_zscore_group_2Fold_confound_removal_wcategorical_averaged-source_HCP2016FreeSurferSubcortical_abcd_baselineYear1Arm1_rest_3517_zscored-beh_abcd_cbcl_grps_model_fits_baseline_all_behs.csv'))
# df <- read_csv(paste0(new_path,
#                       'res/collected/ridgeCV_zscore_group_2Fold_confound_removal_wcategorical_averaged-source_HCP2016FreeSurferSubcortical_abcd_baselineYear1Arm1_rest_3517_zscored-beh_abcd_cbcl_grps_model_fits_followup_all_behs.csv'))

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

rel_p$Factor[rel_p$beh == 'AD_DTZDP'] <- 'Internalising'
rel_p$Factor[rel_p$beh == 'AG_DTZDP'] <- 'Externalising'

con_p <- read_csv(paste0(new_path,'res/abcd_pfactor_consistency.csv'))
colnames(con_p)[1] <- c('beh')

#df$Factor <- as.factor(df$Factor)

# factor_short <- rel_p$Factor_short
# rel_p <- rel_p %>% select(-Factor_short)

rel <- rbind(rel_cbcl,rel_p)

rel$beh == df$beh

df <- left_join(rel,df, by='beh')

d <- df

df <- df[df$Factor != 'cbcl',]
df <- left_join(df,con_p, by="beh")

plt <- ggplot(data=df, aes(Reliability_r_corrected,test_Pearson_r)) +
  #geom_point(data=df, aes(Reliability_r_corrected,test_Pearson_r,colour=Factor), size = 2) +
  geom_line(stat = "smooth", method = 'lm', se = FALSE, aes(colour = Factor), alpha = 0.5, size = 1)  +
  geom_point(aes(colour=Factor), size = 2, alpha = 1) + 
  theme_classic() +
  ylab('Accuracy r(pred,observed)') + xlab('Test-retest Reliability (r)') +
  scale_x_continuous(limits = c(0.35,0.8), breaks = c(0.4,0.5,0.6,0.7,0.8)) +
  scale_y_continuous(limits = c(-0.03,0.17), breaks = c(0,0.04,0.08,0.12,0.16))+
  scale_colour_manual(values = pallet, name = 'Factor') +
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 13),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13))

ggsave(paste0(plt_out, 'confs_FC_cor_rel_test_Pearson_r_followup.png'), plt, width = 4.5, height = 3)




### as bar plot
d_mean <- data.frame("p" = mean(df$test_Pearson_r[df$Factor == "P-Factor"]),
                     "externalising" = mean(df$test_Pearson_r[df$Factor == "Externalising"]),
                     "internalising" = mean(df$test_Pearson_r[df$Factor == "Internalising"]),
                     "attention" = mean(df$test_Pearson_r[df$Factor == "Attention"]),
                     "other" = mean(df$test_Pearson_r[df$Factor == "Other"]))
d_sd <- data.frame("p" = sd(df$test_Pearson_r[df$Factor == "P-Factor"]),
                   "externalising" = sd(df$test_Pearson_r[df$Factor == "Externalising"]),
                   "internalising" = sd(df$test_Pearson_r[df$Factor == "Internalising"]),
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
#        axis.text.x = element_text(angle = 90),
#        axis.title.x=element_blank())
#ggsave(paste0(plt_out, 'FC_mean_accuracy_test_Pearson_r.png'), plt, width = 1.6, height = 1.6)
#ggsave(paste0(plt_out, 'confs_FC_mean_accuracy_test_Pearson_r.png'), plt, width = 1.6, height = 1.6)
#ggsave(paste0(plt_out, 'confs_FC_fol_mean_accuracy_test_Pearson_r.png'), plt, width = 1.6, height = 1.6)



# # TESTING
# pallet2 <- c("#CC79A7", "#D55E00", "#009E73", "#0072B2", "#F0E442")
# 
# d_cbcl <- data.frame("p" = d$test_Pearson_r[d$beh == 'cbcl_scr_syn_totprob_t'],
#                      "internalising" = d$test_Pearson_r[d$beh == 'cbcl_scr_syn_internal_t'],
#                      "externalising" = d$test_Pearson_r[d$beh == 'cbcl_scr_syn_external_t'])
# 
# df_cbcl <- data.frame("mean_accuracy" = t(d_cbcl), "sd_accuracy" = 0, "Factor" = colnames(d_cbcl))
# 
# df_cbcl$Factor <- factor(
#   df_cbcl$Factor,
#   levels = c("p",
#              "externalising",
#              "internalising"
# ))
# 
# 
# df_mean$Factor <- factor(
#   df_mean$Factor,
#   levels = c("p",
#              "externalising",
#              "internalising",
#              "attention",
#              "other"))
# 
# plt = ggplot(data=NULL, aes(Factor,mean_accuracy, fill = Factor)) +
#   geom_col(data=df_mean) +
#   geom_bar(data=df_mean, stat = "identity", width = .75, position = 'dodge') +
#   geom_errorbar(data=df_mean, aes(ymin = mean_accuracy - 1*sd_accuracy, ymax = mean_accuracy + 1*sd_accuracy), width = 0.2, position = position_dodge(0.75)) +
#   geom_point(data=df_cbcl, colour = 'black', size = 2, alpha = 0.6) +
#   ylab('Mean Accuracy') +
#   theme_classic() + 
#   scale_y_continuous(limits = c(-0.02,0.17), breaks = c(0,0.08,0.16))+
#   scale_fill_manual(values = pallet2, name = 'Factor') +
#   theme(axis.text = element_text(size = 13),
#         axis.title = element_text(size = 13),
#         legend.position = "none",
#         axis.text.x = element_blank())
# 
# ggsave(paste0(plt_out, 'confs_FC_mean_accuracy_test_Pearson_r_with_cbcl_testing.png'), plt, width = 2.4, height = 2.8)


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

#ggsave(paste0(plt_out, 'confs_FC_mean_accuracy_test_Pearson_r_factors&sum_scores.png'), plt, width = 3, height = 3.5)
#ggsave(paste0(plt_out, 'confs_FC_mean_accuracy_test_Pearson_r_factors&sum_scores.png'), plt, width = 3, height = 4)
ggsave(paste0(plt_out, 'confs_FC_mean_accuracy_test_Pearson_r_factors&sum_scores.pdf'), plt, width = 3, height = 4)






# R2
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

#ggsave(paste0(plt_out, 'confs_FC_mean_accuracy_test_R2_factors&sum_scores.png'), plt, width = 3, height = 3.5)
ggsave(paste0(plt_out, 'confs_FC_mean_accuracy_test_R2_factors&sum_scores.png'), plt, width = 3, height = 4)






#### each model colored separately
colorscheme = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666", "#313695", "#A50026", "#000000")

df <- df[df$Factor != 'cbcl',]


df$Model <- 'Achenbach 8S'
df$Model[grepl('ACH2F', df$beh, fixed = TRUE)] <-  'Achenbach 2S'
df$Model[grepl('MOOR3S', df$beh, fixed = TRUE)] <- 'Moore 3S'
df$Model[grepl('MOOR4S', df$beh, fixed = TRUE)] <- 'Moore 4S'
df$Model[grepl('MCELRY', df$beh, fixed = TRUE)] <- 'McElroy'
df$Model[grepl('DTZGP', df$beh, fixed = TRUE)] <-  'Deutz GP'
df$Model[grepl('DTZDP', df$beh, fixed = TRUE)] <-  'Deutz DP'
df$Model[grepl('HALTGN', df$beh, fixed = TRUE)] <- 'Haltigan GP'
df$Model[grepl('CLRK2S', df$beh, fixed = TRUE)] <- 'Clark 2S'
df$Model[grepl('CLRK3S', df$beh, fixed = TRUE)] <- 'Clark 3S'
df$Model[grepl('CLRK4S', df$beh, fixed = TRUE)] <- 'Clark 4S'


plt <- ggplot() +
  geom_point(data=df, aes(Reliability_r_corrected,test_Pearson_r,colour=Model), size = 2) +
  theme_classic() +
  ylab('Accuracy r(pred,observed)') + xlab('Test-retest Reliability (r)') +
  scale_x_continuous(limits = c(0.35,0.8), breaks = c(0.4,0.5,0.6,0.7,0.8)) +
  scale_y_continuous(limits = c(-0.03,0.17), breaks = c(0,0.04,0.08,0.12,0.16))+
  scale_colour_manual(values = colorscheme)+
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 13),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13))

#ggsave(paste0(plt_out, 'FC_cor_rel_test_Pearson_r_models.png'), plt, width = 4.5, height = 3.5)
#ggsave(paste0(plt_out, 'confs_FC_cor_rel_test_Pearson_r_models.png'), plt, width = 4.5, height = 3.5)
#ggsave(paste0(plt_out, 'confs_FC_fol_cor_rel_test_Pearson_r_models.png'), plt, width = 4.5, height = 3.5)






#### With cbcl summary scores
cbcl_only <- d[d$Factor == 'cbcl',]
cbcl_only$beh <- str_replace(cbcl_only$beh, "cbcl_scr_syn_", "")
cbcl_only$beh <- str_replace(cbcl_only$beh, "_t", "")

cbcl_only$beh = c("Anx. Dep.","With. Dep.","Somatic","Social","Thought",
                  "Attention","Rule Break","Aggressive","Internalizing",
                  "Externalizing","Total Problems")
#cbcl_only = cbcl_only[c(9,10,11),]
#cbcl_only$beh = c("Internalising","Externalising","Total Problems")

plt = ggplot() +
  #geom_point(data=df, aes(Reliability_r_corrected,test_Pearson_r), colour = 'skyblue4', size = 2, alpha = 0.3) +
  geom_point(data=cbcl_only, aes(Reliability_r_corrected,test_Pearson_r), colour='orangered4', size = 2) +
  theme_classic() +
  ylab('Accuracy r(pred,observed)') + xlab('Test-retest Reliability (r)') +
  scale_x_continuous(limits = c(0.35,0.8), breaks = c(0.4,0.5,0.6,0.7,0.8)) +
  scale_y_continuous(limits = c(-0.03,0.17), breaks = c(0,0.04,0.08,0.12,0.16))+
  #geom_text_repel(data=cbcl_only, aes(Reliability_r_corrected,test_Pearson_r,label=beh), max.overlaps = 11) +
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 13),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13))

#ggsave(paste0(plt_out, 'FC_cor_rel_test_Pearson_r_cbcl.png'), plt, width = 3.2, height = 3)
ggsave(paste0(plt_out, 'confs_FC_cor_rel_test_Pearson_r_cbcl.png'), plt, width = 3, height = 3)
#ggsave(paste0(plt_out, 'confs_FC_fol_cor_rel_test_Pearson_r_cbcl.png'), plt, width = 3.2, height = 3)

dd = df %>% select(Reliability_r_corrected, test_Pearson_r)
dd$Score = 'Factor'
cbcl = cbcl_only %>% select(Reliability_r_corrected, test_Pearson_r)
cbcl$Score = 'Summary'
dd = rbind(dd, cbcl)

clr_scheme = c('skyblue4','orangered4')

plt = ggplot(dd, aes(Reliability_r_corrected,test_Pearson_r,colour = Score)) +
  geom_point(size = 2) +
  theme_classic() +
  ylab('Accuracy r(pred,observed)') + xlab('Test-retest Reliability (r)') +
  scale_x_continuous(limits = c(0.4,0.8), breaks = c(0.4,0.5,0.6,0.7,0.8)) +
  scale_y_continuous(limits = c(-0.03,0.17), breaks = c(0,0.04,0.08,0.12,0.16))+
  scale_colour_manual(values = clr_scheme) +
  theme(axis.text=element_text(size=12),
                  axis.title=element_text(size=12))
ggsave(paste0(plt_out, 'OHBM_confs_FC_cor_rel_test_Pearson_r_cbcl_FOR_LEGEND_ONLY.png'), plt, width = 3, height = 2.5)






cor.test(df$Reliability_r_corrected,df$FD)
cor.test(df$Reliability_r_corrected,df$Omega)
cor.test(df$Reliability_r_corrected,df$OmegaH)


