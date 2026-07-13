
# Plot results

library(ggrepel)
library(tidyverse)
library(RColorBrewer)
library(scico)


pallet <- c("#0072B2", "#009E73", "#D55E00", "#F0E442", "#CC79A7")

plt_out = "/home/mgell/Work/pfactor_reliability/plots/prediction/"




# Prediction accuracy FC
df <- read_csv('/home/mgell/Work/pfactor_reliability/res/collected/ridgeCV_zscore_group_2Fold_confound_removal_wcategorical_averaged-source_HCP2016FreeSurferSubcortical_abcd_baselineYear1Arm1_rest_3517_zscored-beh_abcd_cbcl_grps_model_fits_followup_all_behs.csv')

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

sig <- read_csv('/home/mgell/Work/pfactor_reliability/res/ridgeCV_zscore_group_2Fold_confound_removal_wcategorical_averaged-source_HCP2016FreeSurferSubcortical_abcd_baselineYear1Arm1_rest_3517_zscored-beh_abcd_cbcl_grps_model_fits_followup_all_behs_signific.csv')
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
  geom_point(data = ~subset(., sig == "1"), size = 2, alpha = 1) + 
  geom_point(data = ~subset(., sig == "0"), size = 2, shape = 1) +
  theme_classic() +
  ylab('Accuracy r(pred,observed)') + xlab('Test-retest Reliability (r)') +
  scale_x_continuous(limits = c(0.35,0.8), breaks = c(0.4,0.5,0.6,0.7,0.8)) +
  scale_y_continuous(limits = c(-0.03,0.17), breaks = c(0,0.04,0.08,0.12,0.16))+
  scale_colour_manual(values = pallet, name = 'Factor') +
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 13),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13))

ggsave(paste0(plt_out, 'confs_fol_FC_cor_rel_test_Pearson_r.png'), plt, width = 4.5, height = 3)




plt <- ggplot(data=df, aes(Reliability_r_corrected,test_Pearson_r,colour=ECV_SG)) +
  #geom_point(data=df, aes(Reliability_r_corrected,test_Pearson_r,colour=Factor), size = 2) +
  geom_point(size = 2) +
  theme_classic() +
  ylab('Accuracy r(pred,observed)') + xlab('Test-retest Reliability (r)') +
  scale_x_continuous(limits = c(0.35,0.8), breaks = c(0.4,0.5,0.6,0.7,0.8)) +
  scale_y_continuous(limits = c(-0.03,0.17), breaks = c(0,0.04,0.08,0.12,0.16))+
  #scale_colour_continuous(type = "viridis") +
  scale_colour_scico(palette = "vik", midpoint = 0.3) +
  labs(color = "Expl. var.") +
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 13),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13))
ggsave(paste0(plt_out, 'confs_FC_cor_rel_test_Pearson_r_sig_expl_var.png'), plt, width = 4.5, height = 3.5)

# cor.test(df$Reliability_r_corrected,df$ECV_SG, method = "spearman")
# plot(df$Reliability_r_corrected,df$ECV_SG)
# 
# cor.test(df$test_R2,df$ECV_SG, method = "spearman")
# plot(df$test_R2,df$ECV_SG)




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
#        axis.text.x = element_text(angle = 90),
#        axis.title.x=element_blank())

ggsave(paste0(plt_out, 'confs_FC_fol_mean_accuracy_test_Pearson_r.png'), plt, width = 1.6, height = 1.6)





# All models separately
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
  geom_point(data=df, aes(ECV_SG,test_Pearson_r,colour=Model), size = 2) +
  theme_classic() +
  ylab('Accuracy r(pred,observed)') + xlab('Reliability (r)') +
  #scale_x_continuous(limits = c(0.35,0.8), breaks = c(0.4,0.5,0.6,0.7,0.8)) +
  scale_y_continuous(limits = c(-0.03,0.17), breaks = c(0,0.04,0.08,0.12,0.16))+
  ylab('Accuracy r(pred,observed)') + xlab('Explained variance in CBCL items')  +
  scale_colour_manual(values = colorscheme)

ggsave(paste0(plt_out, 'confs_FC_cor_ECV_test_Pearson_r_sig_presentation_models.png'), plt, width = 4.5, height = 3.5)






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
  geom_point(data=~subset(df, sig == "1"), aes(Reliability_r_corrected,test_Pearson_r), colour = 'skyblue4', size = 2, alpha = 0.4) +
  geom_point(data=~subset(df, sig == "0"), aes(Reliability_r_corrected,test_Pearson_r), colour = 'skyblue4', size = 2, shape = 1, alpha = 0.4) +
  geom_point(data=~subset(cbcl_only, sig == "1"), aes(Reliability_r_corrected,test_Pearson_r), colour='orangered4', size = 2) +
  geom_point(data=~subset(cbcl_only, sig == "0"), aes(Reliability_r_corrected,test_Pearson_r), colour='orangered4', size = 2, shape = 1) +
  theme_classic() +
  ylab('Accuracy r(pred,observed)') + xlab('Test-retest Reliability (r)') +
  scale_x_continuous(limits = c(0.4,0.8), breaks = c(0.4,0.5,0.6,0.7,0.8)) +
  scale_y_continuous(limits = c(-0.03,0.17), breaks = c(0,0.04,0.08,0.12,0.16))+
  geom_text_repel(data=cbcl_only, aes(Reliability_r_corrected,test_Pearson_r,label=beh), max.overlaps = 20) + 
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 13),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13))

ggsave(paste0(plt_out, 'confs_fol_FC_cor_rel_test_Pearson_r_cbcl.png'), plt, width = 3.2, height = 3)



# Get summary of ECV
d_mean <- data.frame("p" = mean(df$ECV_SG[df$Factor == "p-factor"]),
                     "specific" = mean(df$ECV_SG[df$Factor != "p-factor"]))
d_sd <- data.frame("p" = sd(df$ECV_SG[df$Factor == "p-factor"]),
                   "specific" = sd(df$ECV_SG[df$Factor != "p-factor"]))
d_min <- data.frame("p" = min(df$ECV_SG[df$Factor == "p-factor"]),
                    "specific" = min(df$ECV_SG[df$Factor != "p-factor"]))
d_max <- data.frame("p" = max(df$ECV_SG[df$Factor == "p-factor"]),
                    "specific" = max(df$ECV_SG[df$Factor != "p-factor"]))
df_sum <- data.frame("mean_accuracy" = t(d_mean),"sd_accuracy" = t(d_sd), "mind" = t(d_min), "max" = t(d_max), "Factor" = colnames(d_mean))


# check consistency for P:
summary(df[df$Factor == 'P-Factor',])
# and specific factors:
summary(df[df$Factor != 'P-Factor',])


# Correlate
cor.test(df$Reliability_r_corrected, df$FD, method = "spearman")
cor.test(df$Reliability_r_corrected, df$OmegaH, method = "spearman")
cor.test(df$Reliability_r_corrected, df$Omega, method = "spearman")

