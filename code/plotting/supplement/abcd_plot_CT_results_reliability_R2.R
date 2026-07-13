
# Plot results

library(ggrepel)
library(tidyverse)
library(RColorBrewer)


pallet <- c("#0072B2", "#009E73", "#D55E00", "#F0E442", "#CC79A7")

plt_out = "/home/mgell/Work/pfactor_reliability/plots/prediction/"




# Prediction accuracy CT
#df <- read_csv('/home/mgell/Work/pfactor_reliability/res/collected/ridgeCV_zscore_group_2Fold_confound_removal_wcategorical_averaged-source_HCP2016FreeSurferSubcortical_abcd_baselineYear1Arm1_thickness_fsLR32k_6975_zscored-beh_abcd_cbcl_grps_model_fits_baseline_all_behs.csv')
df <- read_csv('/home/mgell/Work/pfactor_reliability/res/collected/ridgeCV_zscore_group_2Fold_confound_removal_wcategorical_averaged-source_HCP2016FreeSurferSubcortical_abcd_baselineYear1Arm1_thickness_fsLR32k_6975-beh_abcd_cbcl_grps_model_fits_baseline_all_behs_permutation.csv')

df <- df[-1,]

rel_cbcl <- read_csv('/home/mgell/Work/pfactor_reliability/data/abcd_sumscores_reliability.csv')
rel_cbcl <- rel_cbcl[rel_cbcl$Dataset == "ABCD(12/7250)",]
colnames(rel_cbcl)[1] <- c('beh')
rel_cbcl$full_name <- 'cbcl'
rel_cbcl$Factor <- 'cbcl'


rel_p <- read_csv('/home/mgell/Work/pfactor_reliability/data/all_pfactor_reliability.csv')
rel_p <- rel_p[rel_p$Dataset == "ABCD(12/7250)",]
rel_p <- rel_p %>% select(Factor_short,Reliability_r:Dataset,Factor)
colnames(rel_p)[1] <- c('beh')
colnames(rel_p)[8] <- c('full_name')

rel_p$Factor <- 'Other'
rel_p$Factor[grepl('P-factor', rel_p$full_name, fixed = TRUE)] <- 'P-Factor'
rel_p$Factor[grepl('Att', rel_p$full_name, fixed = TRUE)] <- 'Attention'
rel_p$Factor[grepl('Int', rel_p$full_name, fixed = TRUE)] <- 'Internalising'
rel_p$Factor[grepl('Ext', rel_p$full_name, fixed = TRUE)] <- 'Externalising'

con_p <- read_csv('/home/mgell/Work/pfactor_reliability/res/abcd_pfactor_consistency.csv')
colnames(con_p)[1] <- c('beh')

# factor_short <- rel_p$Factor_short
# rel_p <- rel_p %>% select(-Factor_short)

rel <- rbind(rel_cbcl,rel_p)

rel$beh == df$beh

df <- left_join(rel,df, by='beh')

d <- df

df <- df[df$Factor != 'cbcl',]
df <- left_join(df,con_p, by="beh")

plt <- ggplot() +
  geom_point(data=df, aes(Reliability_r_corrected,test_R2,colour=Factor), size = 2) +
  theme_classic() +
  ylab('Accuracy (R2)') + xlab('Test-retest Reliability (r)') +
  scale_x_continuous(limits = c(0.35,0.8), breaks = c(0.4,0.5,0.6,0.7,0.8)) +
  scale_y_continuous(limits = c(-0.028,0.004), breaks = c(-0.02,-0.01,0)) +
  scale_colour_manual(values = pallet, name = 'Factor') +
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 13),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13))

ggsave(paste0(plt_out, 'confs_CT_cor_rel_test_R2.png'), plt, width = 4.5, height = 3)
# plt = plt + theme(axis.text=element_text(size=12),
#                   axis.title=element_text(size=12)) +
#   scale_x_continuous(limits = c(0.35,0.8), breaks = c(0.4,0.6,0.8))
# ggsave(paste0(plt_out, 'OHBM_confs_TC_cor_rel_test_Pearson_r_sig.png'), plt, width = 3.5, height = 2.5)






### as bar plot
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
  scale_y_continuous(limits = c(-0.0165,0.005), breaks = c(-0.015,-0.01,-0.005,0,0.005))+
  scale_fill_manual(values = pallet, name = 'Factor') +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 12))
ggsave(paste0(plt_out, 'confs_CT_mean_accuracy_test_R2.png'), plt, width = 1.6, height = 1.6)




#### With cbcl summary scores
cbcl_only <- d[d$Factor == 'cbcl',]
cbcl_only$beh <- str_replace(cbcl_only$beh, "cbcl_scr_syn_", "")
cbcl_only$beh <- str_replace(cbcl_only$beh, "_t", "")

names = c("Anx_Dep","Withdraw_Dep","Somatic","social","Thought",
          "Attention","Rule_break","aggress","Internalizing",
          "Externalizing","Total_score")

plt = ggplot() +
  geom_point(data=cbcl_only, aes(Reliability_r_corrected,test_R2), colour='orangered4', size = 2) +
  theme_classic() +
  ylab('Accuracy (R2)') + xlab('Test-retest Reliability (r)') +
  scale_x_continuous(limits = c(0.35,0.8), breaks = c(0.4,0.5,0.6,0.7,0.8)) +
  scale_y_continuous(limits = c(-0.003,0.011), breaks = c(0,0.005,0.01))+
  geom_text_repel(data=cbcl_only, aes(Reliability_r_corrected,test_R2,label=beh), max.overlaps = 20)

ggsave(paste0(plt_out, 'confs_CT_cor_rel_test_R2_cbcl.png'), plt, width = 3.2, height = 3)
