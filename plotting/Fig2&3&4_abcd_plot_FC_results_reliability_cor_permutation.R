
# Plot results

library(tidyverse)
library(RColorBrewer)
library(ggrepel)

#library(scico)


pallet <- c("#0072B2", "#009E73", "#D55E00", "#F0E442", "#CC79A7")

old_path = '/home/mgell/Work/pfactor_reliability/'
new_path = '/Users/mgell/Work/pfactor/'

plt_out = paste0(new_path,"plots/prediction/")

# Prediction accuracy FC
df <- read_csv(paste0(new_path,
                      'res/collected/ridgeCV_zscore_group_2Fold_confound_removal_wcategorical_averaged-source_HCP2016FreeSurferSubcortical_abcd_baselineYear1Arm1_rest_3517_zscored-beh_abcd_cbcl_grps_model_fits_baseline_all_behs.csv'))

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

sig <- read_csv(paste0(new_path,
                       'res/ridgeCV_zscore_group_2Fold_confound_removal_wcategorical_averaged-source_HCP2016FreeSurferSubcortical_abcd_baselineYear1Arm1_rest_3517_zscored-beh_abcd_cbcl_grps_model_fits_baseline_all_behs_signific.csv'))
#df$Factor <- as.factor(df$Factor)

# factor_short <- rel_p$Factor_short
# rel_p <- rel_p %>% select(-Factor_short)

rel <- rbind(rel_cbcl,rel_p)

rel$beh == df$beh

df <- left_join(rel,df, by='beh')
df <- left_join(df, sig, by='beh')

d <- df

#write.csv(d, paste0(new_path, 'manuscript_tables/suppl_tables_data.csv'))

df <- df[df$Factor != 'cbcl',]
df <- left_join(df,con_p, by="beh")

plt <- ggplot(data=df, aes(Reliability_r_corrected,test_Pearson_r)) +
  #geom_point(data=df, aes(Reliability_r_corrected,test_Pearson_r,colour=Factor), size = 2) +
  geom_line(stat = "smooth", method = 'lm', se = FALSE, aes(colour = Factor), alpha = 0.5, size = 1)  +
  geom_point(data = ~subset(., sig == "1"), aes(colour=Factor), size = 2, alpha = 1) + 
  geom_point(data = ~subset(., sig == "0"), aes(colour=Factor), size = 2, shape = 1) +
  theme_classic() +
  ylab('Accuracy r(pred,observed)') + xlab('Test-retest Reliability (r)') +
  scale_x_continuous(limits = c(0.35,0.8), breaks = c(0.4,0.5,0.6,0.7,0.8)) +
  scale_y_continuous(limits = c(-0.03,0.17), breaks = c(0,0.04,0.08,0.12,0.16))+
  scale_colour_manual(values = pallet, name = 'Factor') +
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 13),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13))

#ggsave(paste0(plt_out, 'FC_cor_rel_test_Pearson_r.png'), plt, width = 4.5, height = 3)
ggsave(paste0(plt_out, 'confs_FC_cor_rel_test_Pearson_r_sig.png'), plt, width = 4.5, height = 3)
#ggsave(paste0(plt_out, 'confs_fol_FC_cor_rel_test_Pearson_r.png'), plt, width = 4.5, height = 3)
plt = plt + theme(axis.text=element_text(size=12),
                  axis.title=element_text(size=12)) +
  scale_x_continuous(limits = c(0.35,0.8), breaks = c(0.4,0.6,0.8))
ggsave(paste0(plt_out, 'OHBM_confs_FC_cor_rel_test_Pearson_r_sig.png'), plt, width = 3.5, height = 2.5)



plt <- ggplot(data=df, aes(ECV_SG,test_Pearson_r,colour=Factor)) +
  geom_line(stat = "smooth", method = 'lm', se = FALSE, aes(colour = Factor), alpha = 0.5, size = 1)  +
  geom_point(size = 2) +
  theme_classic() +
  scale_x_continuous(limits = c(0,0.81), breaks = c(0,0.2,0.4,0.6,0.8)) +
  scale_y_continuous(limits = c(-0.03,0.17), breaks = c(0,0.04,0.08,0.12,0.16))+
  ylab('Accuracy r(pred,observed)') + xlab('Explained variance in CBCL items')  +
  scale_colour_manual(values = pallet)+
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 13),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13))

ggsave(paste0(plt_out, 'confs_FC_cor_ECV_test_Pearson_r_sig.png'), plt, width = 4.5, height = 3)


# plt <- ggplot(data=df, aes(Reliability_r_corrected,test_Pearson_r,colour=ECV_SG)) +
#   #geom_point(data=df, aes(Reliability_r_corrected,test_Pearson_r,colour=Factor), size = 2) +
#   geom_point(size = 2) +
#   theme_classic() +
#   ylab('Accuracy r(pred,observed)') + xlab('Test-retest Reliability (r)') +
#   scale_x_continuous(limits = c(0.35,0.8), breaks = c(0.4,0.5,0.6,0.7,0.8)) +
#   scale_y_continuous(limits = c(-0.03,0.17), breaks = c(0,0.04,0.08,0.12,0.16))+
#   #scale_colour_continuous(type = "viridis") +
#   scale_colour_scico(palette = "vik", midpoint = 0.3) +
#   labs(color = "Expl. var.") +
#   theme(axis.text = element_text(size = 13),
#         axis.title = element_text(size = 13),
#         legend.text = element_text(size = 12),
#         legend.title = element_text(size = 13))
# ggsave(paste0(plt_out, 'confs_FC_cor_rel_test_Pearson_r_sig_expl_var.png'), plt, width = 4.5, height = 3.5)
# 
# # cor.test(df$Reliability_r_corrected,df$ECV_SG, method = "spearman")
# # plot(df$Reliability_r_corrected,df$ECV_SG)
# # 
# # cor.test(df$test_R2,df$ECV_SG, method = "spearman")
# # plot(df$test_R2,df$ECV_SG)

test = df %>% select(Factor, FD, beh, Reliability_r_corrected, test_Pearson_r)
test$category = 'P-Factor'
test$category[test$Factor != 'P-Factor'] = 'Specific.Factor'

test %>% group_by(category) %>% summarise(across(where(is.numeric), mean, na.rm = TRUE))
test %>% group_by(category) %>% summarise(across(where(is.numeric), min, na.rm = TRUE))
test %>% group_by(category) %>% summarise(across(where(is.numeric), max, na.rm = TRUE))
test[test$FD < 0.9,]

# R2
plt <- ggplot(data=df, aes(Reliability_r_corrected,test_R2)) +
  #geom_point(data=df, aes(Reliability_r_corrected,test_Pearson_r,colour=Factor), size = 2) +
  geom_line(stat = "smooth", method = 'lm', se = FALSE, aes(colour = Factor), alpha = 0.5, size = 1)  +
  # geom_point(data = ~subset(., sig == "1"), aes(colour=Factor), size = 2, alpha = 1) + 
  # geom_point(data = ~subset(., sig == "0"), aes(colour=Factor), size = 2, shape = 1) +
  geom_point(aes(colour=Factor), size = 2) +
  theme_classic() +
  ylab('Accuracy (R2)') + xlab('Test-retest Reliability (r)') +
  scale_x_continuous(limits = c(0.35,0.8), breaks = c(0.4,0.5,0.6,0.7,0.8)) +
  scale_y_continuous(limits = c(-0.02,0.023), breaks = c(-0.01,0,0.01,0.02))+
  scale_colour_manual(values = pallet, name = 'Factor') +
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 13),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13))

ggsave(paste0(plt_out, 'confs_FC_cor_rel_test_R2.png'), plt, width = 4.5, height = 3)


plt <- ggplot(data=df, aes(ECV_SG,test_R2,colour=Factor)) +
  geom_line(stat = "smooth", method = 'lm', se = FALSE, aes(colour = Factor), alpha = 0.5, size = 1)  +
  geom_point(size = 2) +
  theme_classic() +
  scale_x_continuous(limits = c(0,0.81), breaks = c(0,0.2,0.4,0.6,0.8)) +
  scale_y_continuous(limits = c(-0.02,0.023), breaks = c(-0.01,0,0.01,0.02))+
  ylab('Accuracy (R2)') + xlab('Explained variance in CBCL items')  +
  scale_colour_manual(values = pallet)+
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 13),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13))

ggsave(paste0(plt_out, 'confs_FC_cor_ECV_test_R2.png'), plt, width = 4.5, height = 3)




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
  scale_colour_manual(values = colorscheme)+
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 13),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13))

ggsave(paste0(plt_out, 'confs_FC_cor_ECV_test_Pearson_r_sig_presentation_models.png'), plt, width = 4.5, height = 3.5)


DF = df[df$Model != 'Achenbach 8S',]

DF$Model <- factor(
  DF$Model,
  levels = c(
    "Clark 2S",
    "Clark 3S",
    "Clark 4S",
    "Moore 4S",
    "Moore 3S",
    "Deutz DP",
    "Haltigan GP",
    "McElroy",
    "Deutz GP",
    "Achenbach 2S"
  )
)


plt <- ggplot(data=DF, aes(Model,test_Pearson_r)) +
#plt <- ggplot(data=DF, aes(Model,test_Pearson_r, colour = Model)) +
  stat_summary(data = subset(DF, sig %in% c(1, "1")),
               aes(group = 1), fun = mean, geom = "line",
               colour = "gray30", linewidth = 0.9, alpha = 0.5) +
  #stat_summary(fun = mean, geom = "point",
  #             colour = "gray30", size = 2, alpha = 0.7) +
  geom_point(aes(shape = Factor), size = 2, colour = "#529ea3") +
  theme_classic() +
  ylab('Accuracy r(pred,observed)') +
  scale_y_continuous(limits = c(-0.03,0.17), breaks = c(0,0.04,0.08,0.12,0.16)) +
  scale_colour_manual(values = colorscheme)+
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 13),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13))

#ggsave(paste0(plt_out, 'confs_FC_cor_test_Pearson_r_ordered_models.pdf'), plt, width = 4.5, height = 3.5)
ggsave(paste0(plt_out, 'confs_FC_cor_test_Pearson_r_ordered_models.pdf'), plt, width = 5, height = 3.5)
ggsave(paste0(plt_out, 'confs_FC_cor_test_Pearson_r_ordered_models.pdf'), plt, width = 5.1, height = 3.5)

#ggsave(paste0(plt_out, 'confs_FC_cor_test_Pearson_r_ordered_models_legend.png'), plt, width = 4.5, height = 5)


#plt <- ggplot(data=DF, aes(Model,ECV_SG, colour = Model)) +
plt <- ggplot(data=DF, aes(Model,ECV_SG)) +
  # stat_summary(data = subset(DF, sig %in% c(1, "1")),
  #              aes(group = 1), fun = mean, geom = "line",
  #              colour = "gray30", linewidth = 0.9, alpha = 0.5) +
  #stat_summary(fun = mean, geom = "point",
  #             colour = "gray30", size = 2, alpha = 0.7) +
  geom_point(aes(shape = Factor), size = 2, colour = "#529ea3") +
  theme_classic() +
  ylab('Explained variance in CBCL items') +
  scale_colour_manual(values = colorscheme)+
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 13),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13))

#ggsave(paste0(plt_out, 'confs_FC_ECV_SG_ordered_models.pdf'), plt, width = 4.5, height = 3.5)
ggsave(paste0(plt_out, 'confs_FC_ECV_SG_ordered_models.pdf'), plt, width = 5, height = 3.5)



# load item number
factor_items <- read_csv(paste0(new_path,"/data/abcd_item_number_per_factor.csv"))
colnames(factor_items)[2] = c('beh')

DF2 <- left_join(DF, factor_items, by='beh')
DF2 <- DF2 %>% select(Model,full_name,Factor,test_Pearson_r,test_R2,Reliability_r_corrected,sig,ECV_SG,item_number)
DF2 <- subset(DF2, sig %in% c(1, "1"))

x = DF2 %>% 
  group_by(Model) %>%
  summarise(Mean_pred = mean(test_Pearson_r), Mean_items = mean(item_number), Mean_pred_R2 = mean(test_R2))

m  <- lm(Mean_pred ~ Mean_items, data = x, na.action = na.exclude)
b  <- unname(coef(m)["Mean_items"])
mu <- mean(x$Mean_items, na.rm = TRUE)

x$test_Pearson_r = x$Mean_pred - b * (x$Mean_items - mu)

plt <- ggplot(data=NULL, aes(Model,test_Pearson_r)) +
  stat_summary(data = subset(DF, sig %in% c(1, "1")),
               aes(group = 1), fun = mean, geom = "line",
               colour = "black", linewidth = 0.9, alpha = 0.6) +
  geom_line(data = x, aes(group = 1), colour = "gray40", linetype = 2, linewidth = 0.9, alpha = 0.6) +
  geom_point(data=DF, aes(shape = Factor), size = 2, colour = "#529ea3") +
  theme_classic() +
  ylab('Accuracy r(pred,observed)') +
  scale_y_continuous(limits = c(-0.03,0.17), breaks = c(0,0.04,0.08,0.12,0.16)) +
  scale_colour_manual(values = colorscheme)+
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 13),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13))

ggsave(paste0(plt_out, 'confs_FC_cor_test_Pearson_r_ordered_models.pdf'), plt, width = 5, height = 3.5)


# plt <- ggplot(data=DF, aes(Model,Reliability_r_corrected, colour = Model)) +
#   # stat_summary(data = subset(DF, sig %in% c(1, "1")),
#   #              aes(group = 1), fun = mean, geom = "line",
#   #              colour = "gray30", linewidth = 0.9, alpha = 0.5) +
#   #stat_summary(fun = mean, geom = "point",
#   #             colour = "gray30", size = 2, alpha = 0.7) +
#   geom_point(aes(shape = Factor), size = 2) +
#   theme_classic() +
#   ylab('Reliability (r)') +
#   scale_y_continuous(limits = c(0.35,0.8), breaks = c(0.4,0.5,0.6,0.7,0.8)) +
#   scale_colour_manual(values = colorscheme)+
#   theme(axis.text = element_text(size = 13),
#         axis.title = element_text(size = 13),
#         axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
#         legend.text = element_text(size = 12),
#         legend.title = element_text(size = 13))
# 
# ggsave(paste0(plt_out, 'confs_FC_reliability_r_ordered_models.pdf'), plt, width = 4.5, height = 3.5)


# DF = df[df$Factor %in% c('P-Factor', 'Externalising'),]#, 'Internalising'),]
# DF = DF[DF$beh != 'P_ACH8',]
# DF = DF[DF$beh != 'P_DTZDP',]
# #DF$beh = str_replace(DF$beh, )
# 
# pfactor = DF[DF$Factor == 'P-Factor',]
# extern = DF[DF$Factor == 'Externalising',]
# pfactor$Model == extern$Model
# 
# dat = data.frame('pfactor_accuracy' = pfactor$test_Pearson_r, 
#                  'pfactor_var_expl' = pfactor$ECV_SG,
#                  'externalising_var_expl' = extern$ECV_SG)
# 
# lm(pfactor_accuracy ~ 1 + pfactor_var_expl + externalising_var_expl + pfactor_var_expl:externalising_var_expl, data = dat)
# 
# 
# dat = data.frame('accuracy_ratio_p_extern' = pfactor$test_Pearson_r / extern$test_Pearson_r, 
#                  'pfactor_var_expl' = pfactor$ECV_SG,
#                  'extern_var_expl' = extern$ECV_SG)
# 
# lm(accuracy_ratio_p_extern ~ 1 + pfactor_var_expl + extern_var_expl + pfactor_var_expl:extern_var_expl, data = dat)


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

sum_score_col = 'orangered4' # '#5da3a3' 

plt = ggplot() +
  geom_line(data=cbcl_only, aes(Reliability_r_corrected,test_Pearson_r), stat = "smooth", method = 'lm', se = FALSE, color=sum_score_col, alpha = 0.5, size = 1)  +
  #geom_point(data=~subset(df, sig == "1"), aes(Reliability_r_corrected,test_Pearson_r), colour = 'skyblue4', size = 2, alpha = 0.4) +
  #geom_point(data=~subset(df, sig == "0"), aes(Reliability_r_corrected,test_Pearson_r), colour = 'skyblue4', size = 2, shape = 1, alpha = 0.4) +
  geom_point(data=~subset(cbcl_only, sig == "1"), aes(Reliability_r_corrected,test_Pearson_r), colour=sum_score_col, size = 2) +
  geom_point(data=~subset(cbcl_only, sig == "0"), aes(Reliability_r_corrected,test_Pearson_r), colour=sum_score_col, size = 2, shape = 1) +
  theme_classic() +
  ylab('Accuracy r(pred,observed)') + xlab('Test-retest Reliability (r)') +
  scale_x_continuous(limits = c(0.47,0.8), breaks = c(0.4,0.5,0.6,0.7,0.8)) +
  scale_y_continuous(limits = c(-0.03,0.17), breaks = c(0,0.04,0.08,0.12,0.16))+
  geom_text_repel(data=cbcl_only, aes(Reliability_r_corrected,test_Pearson_r,label=beh), max.overlaps = 20) + 
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 13),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13))

ggsave(paste0(plt_out, 'confs_FC_cor_rel_test_Pearson_r_cbcl.png'), plt, width = 3.2, height = 3)


# R2
plt = ggplot() +
  geom_line(data=cbcl_only, aes(Reliability_r_corrected,test_R2), stat = "smooth", method = 'lm', se = FALSE, color=sum_score_col, alpha = 0.5, size = 1)  +
  geom_point(data=cbcl_only, aes(Reliability_r_corrected,test_R2), colour=sum_score_col, size = 2) +
  theme_classic() +
  ylab('Accuracy (R2)') + xlab('Test-retest Reliability (r)') +
  scale_x_continuous(limits = c(0.47,0.8), breaks = c(0.4,0.5,0.6,0.7,0.8)) +
  scale_y_continuous(limits = c(-0.02,0.023), breaks = c(-0.01,0,0.01,0.02))+
  geom_text_repel(data=cbcl_only, aes(Reliability_r_corrected,test_R2,label=beh), max.overlaps = 20) + 
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 13),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13))

ggsave(paste0(plt_out, 'confs_FC_cor_rel_test_R2_cbcl.png'), plt, width = 3.2, height = 3)

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

# model_colors <- c(
#   "Factor" = 'skyblue4', #'skyblue4',
#   "Sum scores" = 'skyblue2') #'orangered4')
# 
# model_colors <- c(
#   "Factor" = '#304a4a', #'skyblue4',
#   "Sum scores" = '#5da3a3') #'orangered4')
# 
# model_colors <- c(
#   "Factor" = '#554646', #'skyblue4',
#   "Sum scores" = '#bf9b9b') #'orangered4')

model_colors <- c(
  "Factor" = 'gray25', #'skyblue4',
  "Sum scores" = 'gray85') #'orangered4')

plt = ggplot(data=df2, aes(X,test_Pearson_r)) +
  geom_boxplot(aes(fill = X), outliers = FALSE, alpha = 0.7) +
  # geom_point(aes(color = X), position = 'jitter') +
  geom_point(data=~subset(df2, sig == "1",), aes(color = X), size = 1, position = 'jitter', color = 'black') +
  geom_point(data=~subset(df2, sig == "0"), aes(color = X), size = 1, shape = 1, position = 'jitter', color = 'black') +
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

#ggsave(paste0(plt_out, 'confs_FC_test_Pearson_r_cbcl_vs_factors.png'), plt, width = 3.7, height = 3.6)
ggsave(paste0(plt_out, 'confs_FC_test_Pearson_r_cbcl_vs_factors.pdf'), plt, width = 3.7, height = 3.6)



plt = ggplot(data=df2, aes(X,test_R2)) +
  geom_boxplot(aes(fill = X), outliers = FALSE, alpha = 0.7) +
  geom_point(aes(color = X), size = 1, position = 'jitter', color = 'black') +
  #geom_point(data=~subset(df2, sig == "1",), aes(color = X), size = 1, position = 'jitter', color = 'black') +
  #geom_point(data=~subset(df2, sig == "0"), aes(color = X), size = 1, shape = 1, position = 'jitter', color = 'black') +
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

ggsave(paste0(plt_out, 'confs_FC_test_R2_cbcl_vs_factors.png'), plt, width = 3.7, height = 3.6)




# TESTING
df2$Factor <- factor(
  df2$Factor,
  levels = c(
    "cbcl",
    "P-Factor",
    "Externalising",
    "Internalising",
    "Attention",
    "Other"
  )
)

model_colors <- c(
  "cbcl" = "#7570B3",
  "P-Factor" = "#CC79A7",
  "Externalising" = "#009E73",
  "Internalising" = "#D55E00",
  "Attention" = "#0072B2",
  "Other" = "#F0E442")

plt = ggplot(data=df2, aes(Factor,test_Pearson_r)) +
  geom_boxplot(aes(fill = Factor), outliers = FALSE, alpha = 0.7) +
  # geom_point(aes(color = X), position = 'jitter') +
  geom_point(data=~subset(df2, sig == "1",), aes(color = Factor), size = 1, position = 'jitter') +
  geom_point(data=~subset(df2, sig == "0"), aes(color = Factor), size = 1, shape = 1, position = 'jitter') +
  theme_classic() +
  ylab('Accuracy r(pred,observed)') + xlab('') +
  scale_y_continuous(limits = c(-0.03,0.17), breaks = c(0,0.04,0.08,0.12,0.16)) +
  scale_colour_manual(values = model_colors, name = "Data") +
  scale_fill_manual(values = model_colors, name = "Data") +
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 13),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13))

ggsave(paste0(plt_out, 'confs_FC_test_Pearson_r_cbcl_vs_factors_all_testing_plots.png'), plt, width = 4.5, height = 3.2)




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

