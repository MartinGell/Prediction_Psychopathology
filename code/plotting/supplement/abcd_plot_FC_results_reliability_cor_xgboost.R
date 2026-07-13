
# Plot results

library(ggrepel)
library(tidyverse)
library(RColorBrewer)


pallet <- c("#0072B2", "#009E73", "#D55E00", "#F0E442", "#CC79A7")

old_path = '/home/mgell/Work/pfactor_reliability/'
new_path = '/Users/mgell/Work/pfactor/'

plt_out = paste0(new_path,"plots/prediction/")

# Prediction accuracy FC
df <- read_csv(paste0(new_path,
                      'res/collected/xgboost_group_2Fold_confound_removal_wcategorical_averaged-source_HCP2016FreeSurferSubcortical_abcd_baselineYear1Arm1_rest_3517_zscored-beh_abcd_cbcl_grps_model_fits_baseline_all_behs.csv'))

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

#df$Factor <- as.factor(df$Factor)

# factor_short <- rel_p$Factor_short
# rel_p <- rel_p %>% select(-Factor_short)

rel <- rbind(rel_cbcl,rel_p)

df <- left_join(df, rel, by='beh')

df <- left_join(df,con_p, by="beh")

plt <- ggplot(data=df, aes(Reliability_r_corrected,test_Pearson_r)) +
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


ggsave(paste0(plt_out, 'XGBOOST_confs_FC_cor_rel_test_Pearson_r.png'), plt, width = 4.5, height = 3)




plt <- ggplot() +
  geom_point(data=df, aes(Reliability_r_corrected,test_R2,colour=Factor), size = 2) +
  theme_classic() +
  ylab('Accuracy (R2)') + xlab('Test-retest Reliability (r)') +
  scale_x_continuous(limits = c(0.35,0.8), breaks = c(0.4,0.5,0.6,0.7,0.8)) +
  scale_y_continuous(limits = c(-0.021,0.025), breaks = c(-0.01,0,0.01,0.02))+
  scale_colour_manual(values = pallet, name = 'Factor')+
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 13),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13))

ggsave(paste0(plt_out, 'XGBOOST_confs_FC_cor_rel_R2.png'), plt, width = 4.5, height = 3)









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

ggsave(paste0(plt_out, 'XGBOOST_FC_mean_accuracy_test_Pearson_r.png'), plt, width = 1.6, height = 1.6)






# R2
d_mean <- data.frame("p" = mean(df$test_R2[df$Factor == "P-Factor"]),
                     "internalising" = mean(df$test_R2[df$Factor == "Internalising"]),
                     "externalising" = mean(df$test_R2[df$Factor == "Externalising"]),
                     "attention" = mean(df$test_R2[df$Factor == "Attention"]),
                     "other" = mean(df$test_R2[df$Factor == "Other"]))
d_sd <- data.frame("p" = sd(df$test_R2[df$Factor == "P-Factor"]),
                   "internalising" = sd(df$test_R2[df$Factor == "Internalising"]),
                   "externalising" = sd(df$test_R2[df$Factor == "Externalising"]),
                   "attention" = sd(df$test_R2[df$Factor == "Attention"]),
                   "other" = sd(df$test_R2[df$Factor == "Other"]))
df_mean <- data.frame("mean_accuracy" = t(d_mean),"sd_accuracy" = t(d_sd), "Factor" = colnames(d_mean))

plt = ggplot(data=df_mean, aes(Factor,mean_accuracy, fill = Factor)) +
  geom_col() +
  geom_bar(stat = "identity", width = .75, position = 'dodge') +
  geom_errorbar(aes(ymin = mean_accuracy - 1*sd_accuracy, ymax = mean_accuracy + 1*sd_accuracy), width = 0.2, position = position_dodge(0.75)) +
  ylab('Mean Accuracy') +
  theme_classic() +
  scale_y_continuous(limits = c(-0.015,0.008), breaks = c(-0.012,-0.008,-0.004,0,0.004,0.008))+
  scale_fill_manual(values = pallet, name = 'Factor') +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 12))

ggsave(paste0(plt_out, 'XGBOOST_FC_mean_accuracy_test_R2.png'), plt, width = 1.6, height = 1.6)



