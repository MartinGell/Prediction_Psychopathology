
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
                      'res/collected/ridgeCV_zscore_group_2Fold_confound_removal_wcategorical_averaged-source_HCP2016FreeSurferSubcortical_abcd_baselineYear1Arm1_rest_3517_zscored-beh_abcd_cbcl_grps_model_fits_baseline_all_behs.csv'))

df <- df[-1,]
df <- df[!grepl('cbcl', df$beh, fixed = TRUE),]


con_p <- read_csv(paste0(new_path,'res/abcd_pfactor_consistency.csv'))
colnames(con_p)[1] <- c('beh')


rel_p <- read_csv(paste0(new_path,'data/all_pfactor_reliability.csv'))
rel_p <- rel_p[rel_p$Dataset == "ABCD(12/7250)",]
rel_p <- rel_p %>% select(Factor_short,Reliability_r:Dataset,Factor)
colnames(rel_p)[1] <- c('beh')
colnames(rel_p)[8] <- c('full_name')

rel_p$Factor <- 'other'
rel_p$Factor[grepl('P-factor', rel_p$full_name, fixed = TRUE)] <- 'p-factor'
rel_p$Factor[grepl('Att', rel_p$full_name, fixed = TRUE)] <- 'attention'
rel_p$Factor[grepl('Int', rel_p$full_name, fixed = TRUE)] <- 'internalising'
rel_p$Factor[grepl('Ext', rel_p$full_name, fixed = TRUE)] <- 'externalising'

sig <- read_csv(paste0(new_path,
                       'res/ridgeCV_zscore_group_2Fold_confound_removal_wcategorical_averaged-source_HCP2016FreeSurferSubcortical_abcd_baselineYear1Arm1_rest_3517_zscored-beh_abcd_cbcl_grps_model_fits_baseline_all_behs_signific.csv'))


# put together
rel_p$beh == df$beh
rel_p$beh == con_p$beh

rel <- left_join(rel_p, con_p, by=c("beh"))
df <- left_join(df, sig, by=c("beh"))
df <- left_join(rel, df, by=c("beh"))

# df$r <- rel_p$rel
# df$Factor <- rel_p$Factor

d <- df



# ALL CONSISTENCY WITH PREDICTION ACCURACY

plt <- ggplot(data=df, aes(FD,test_Pearson_r,colour=Factor)) +
  geom_line(stat = "smooth", method = 'lm', se = FALSE, aes(colour = Factor), alpha = 0.5, size = 1)  +
  geom_point(data = ~subset(., sig == "1"), size = 2, alpha = 1) + 
  geom_point(data = ~subset(., sig == "0"), size = 2, shape = 1) +
  theme_classic() +
  scale_x_continuous(limits = c(0.83,1.0), breaks = c(0.85,0.9,0.95,1.0)) +
  scale_y_continuous(limits = c(-0.03,0.17), breaks = c(0,0.04,0.08,0.12,0.16))+
  ylab('Accuracy r(pred,observed)') + xlab('Factor determinacy') +
  scale_colour_manual(values = pallet, name = 'Factor') +
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 13),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13))
  #annotate('text', x = 0.7, y = 0.02, label=text,  size=5, parse = TRUE) +
  #annotate('text', x = 0.705, y = 0.00, label=text2,  size=5, parse = TRUE)

#ggsave(paste0(plt_out, 'abcd_FC_FD_test_Pearson_r.png'), plt, width = 4.5, height = 3)
ggsave(paste0(plt_out, 'confs_abcd_FC_FD_test_Pearson_r.png'), plt, width = 4.5, height = 3)



plt <- ggplot(data=df, aes(Omega,test_Pearson_r,colour=Factor)) +
  geom_line(stat = "smooth", method = 'lm', se = FALSE, aes(colour = Factor), alpha = 0.5, size = 1)  +
  geom_point(data = ~subset(., sig == "1"), size = 2, alpha = 1) + 
  geom_point(data = ~subset(., sig == "0"), size = 2, shape = 1) +
  theme_classic() +
  #scale_x_continuous(limits = c(-0.1,0.9), breaks = c(0,0.2,0.4,0.6,0.8)) +
  scale_x_continuous(limits = c(0.7,1.0), breaks = c(0.7,0.8,0.9,1.0)) +
  scale_y_continuous(limits = c(-0.03,0.17), breaks = c(0,0.04,0.08,0.12,0.16))+
  ylab('Accuracy r(pred,observed)') + xlab('Omega')  +
  scale_colour_manual(values = pallet, name = 'Factor')+
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 13),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13))
#annotate('text', x = 0.7, y = 0.02, label=text,  size=5, parse = TRUE) +
#annotate('text', x = 0.705, y = 0.00, label=text2,  size=5, parse = TRUE)

#ggsave(paste0(plt_out, 'abcd_FC_Omega_test_Pearson_r.png'), plt, width = 4.5, height = 3)
ggsave(paste0(plt_out, 'confs_abcd_FC_Omega_test_Pearson_r.png'), plt, width = 4.5, height = 3)




plt <- ggplot(data=df, aes(OmegaH,test_Pearson_r,colour=Factor)) +
  geom_line(stat = "smooth", method = 'lm', se = FALSE, aes(colour = Factor), alpha = 0.5, size = 1)  +
  geom_point(data = ~subset(., sig == "1"), size = 2, alpha = 1) + 
  geom_point(data = ~subset(., sig == "0"), size = 2, shape = 1) +
  theme_classic() +
  #scale_x_continuous(limits = c(-0.1,0.9), breaks = c(0,0.2,0.4,0.6,0.8)) +
  scale_x_continuous(limits = c(0,1.0), breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  scale_y_continuous(limits = c(-0.03,0.17), breaks = c(0,0.04,0.08,0.12,0.16))+
  ylab('Accuracy r(pred,observed)') + xlab('OmegaH')  +
  scale_colour_manual(values = pallet, name = 'Factor')+
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 13),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13))
#annotate('text', x = 0.7, y = 0.02, label=text,  size=5, parse = TRUE) +
#annotate('text', x = 0.705, y = 0.00, label=text2,  size=5, parse = TRUE)

ggsave(paste0(plt_out, 'confs_abcd_FC_OmegaH_test_Pearson_r.png'), plt, width = 4.5, height = 3)




plt <- ggplot(data=df, aes(ECV_SG,test_Pearson_r,colour=Factor)) +
  #geom_smooth(data=df, aes(ECV_SG,r), method = lm, se = FALSE, colour = 'lightgray', size = 1.5, alpha = 0.4) +
  geom_point(data = ~subset(., sig == "1"), size = 2, alpha = 1) + 
  geom_point(data = ~subset(., sig == "0"), size = 2, shape = 1) +
  theme_classic() +
  scale_x_continuous(limits = c(-0.1,0.85), breaks = c(0,0.2,0.4,0.6,0.8)) +
  scale_y_continuous(limits = c(-0.03,0.17), breaks = c(0,0.04,0.08,0.12,0.16))+
  ylab('Accuracy r(pred,observed)') + xlab('CBCL Variance Explained by Factor') +
  scale_colour_manual(values = pallet, name = 'Factor') +
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 13),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13))
#annotate('text', x = 0.7, y = 0.02, label=text,  size=5, parse = TRUE) +
#annotate('text', x = 0.705, y = 0.00, label=text2,  size=5, parse = TRUE)

#ggsave(paste0(plt_out, 'abcd_FC_ECV_SG_test_Pearson_r.png'), plt, width = 4.5, height = 3)
ggsave(paste0(plt_out, 'confs_abcd_FC_ECV_SG_test_Pearson_r.png'), plt, width = 4.5, height = 3)


colorscheme = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666", "#313695", "#A50026", "#000000")

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


plt <- ggplot(data=df, aes(ECV_SG,test_Pearson_r,colour=Model)) +
  #geom_smooth(data=df, aes(ECV_SG,r), method = lm, se = FALSE, colour = 'lightgray', size = 1.5, alpha = 0.4) +
  geom_point(data = ~subset(., sig == "1"), size = 2, alpha = 1) + 
  geom_point(data = ~subset(., sig == "0"), size = 2, shape = 1) +
  theme_classic() +
  scale_x_continuous(limits = c(-0.1,0.85), breaks = c(0,0.2,0.4,0.6,0.8)) +
  scale_y_continuous(limits = c(-0.03,0.17), breaks = c(0,0.04,0.08,0.12,0.16))+
  ylab('Accuracy r(pred,observed)') + xlab('CBCL Variance Expl. by Factor') +
  scale_colour_manual(values = colorscheme, name = 'Factor') +
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 13),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13))
#annotate('text', x = 0.7, y = 0.02, label=text,  size=5, parse = TRUE) +
#annotate('text', x = 0.705, y = 0.00, label=text2,  size=5, parse = TRUE)

#ggsave(paste0(plt_out, 'abcd_FC_ECV_SG_test_Pearson_r.png'), plt, width = 4.5, height = 3)
ggsave(paste0(plt_out, 'confs_abcd_FC_ECV_SG_test_Pearson_r_allmodels.png'), plt, width = 4.5, height = 3)






# # ABCD full sample
# d = read_csv('/home/mgell/Work/pfactor_reliability/res/abcd_cbcl_model_fits.csv')
# 
# T1 <- d[,2:50]
# T2 <- d[,51:99]
# 
# cormat <- cor(T1,T2,use = "pairwise.complete.obs")
# rel <- cormat[row(cormat)==col(cormat)]
# 
# df$reliability <- rel
# 

# ALL CONSISTENCY WITH TEST-RETEST

plt <- ggplot() +
  #geom_smooth(data=df, aes(rel,ECV_SG), method = lm, se = FALSE, colour = 'lightgray', linewidth = 1.5, alpha = 0.4) +
  geom_point(data=df, aes(Reliability_r_corrected,ECV_SG,colour=Factor), size = 2) +
  theme_classic() +
  scale_x_continuous(limits = c(0.35,0.8), breaks = c(0.4,0.5,0.6,0.7,0.8)) +
  scale_y_continuous(limits = c(-0.01,0.85), breaks = c(0,0.2,0.4,0.6,0.8))+
  ylab('CBCL Variance Expl. by Factor') + xlab('Test-retest Reliability (r)') +
  scale_colour_manual(values = pallet, name = 'Factor') +
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 13),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13))
#annotate('text', x = 0.7, y = 0.02, label=text,  size=5, parse = TRUE) +
#annotate('text', x = 0.705, y = 0.00, label=text2,  size=5, parse = TRUE)

#ggsave(paste0(plt_out, 'abcd_FC_ECV_SG_rel.png'), plt, width = 4.5, height = 3)
ggsave(paste0(plt_out, 'confs_abcd_FC_ECV_SG_rel.png'), plt, width = 4.5, height = 3)


# plt <- plt + theme(legend.position = "none")
#ggsave(paste0(plt_out, 'abcd_ECV_SG_rel_no_leg.png'), plt, width = 3, height = 3)



plt <- ggplot() +
  geom_point(data=df, aes(Reliability_r_corrected,FD,colour=Factor), size = 2) +
  theme_classic() +
  scale_x_continuous(limits = c(0.35,0.8), breaks = c(0.4,0.5,0.6,0.7,0.8)) +
  scale_y_continuous(limits = c(0.83,1.0), breaks = c(0.85,0.9,0.95,1.0)) +
  ylab('Factor determinacy') + xlab('Test-retest Reliability (r)') +
  scale_colour_manual(values = pallet, name = 'Factor')
#annotate('text', x = 0.7, y = 0.02, label=text,  size=5, parse = TRUE) +
#annotate('text', x = 0.705, y = 0.00, label=text2,  size=5, parse = TRUE)

#ggsave(paste0(plt_out, 'abcd_FC_FD_rel.png'), plt, width = 4.5, height = 3)
ggsave(paste0(plt_out, 'confs_abcd_FC_FD_rel.png'), plt, width = 4.5, height = 3)



plt <- ggplot() +
  geom_point(data=df, aes(Reliability_r_corrected,Omega,colour=Factor), size = 2) +
  theme_classic() +
  scale_x_continuous(limits = c(0.35,0.8), breaks = c(0.4,0.5,0.6,0.7,0.8)) +
  #scale_y_continuous(limits = c(-0.1,0.9), breaks = c(0,0.2,0.4,0.6,0.8)) +
  scale_y_continuous(limits = c(0.7,1.0), breaks = c(0.7,0.8,0.9,1.0)) +
  ylab('Omega') + xlab('Test-retest Reliability (r)') +
  scale_colour_manual(values = pallet, name = 'Factor')
#annotate('text', x = 0.7, y = 0.02, label=text,  size=5, parse = TRUE) +
#annotate('text', x = 0.705, y = 0.00, label=text2,  size=5, parse = TRUE)

#ggsave(paste0(plt_out, 'abcd_FC_Omega_rel.png'), plt, width = 4.5, height = 3)
ggsave(paste0(plt_out, 'confs_abcd_FC_Omega_rel.png'), plt, width = 4.5, height = 3)

#cor.test(df$ECV_SG,df$reliability)



# ALL CONSISTENCY AGAINST EACH OTHER

plt <- ggplot() +
  geom_point(data=df, aes(ECV_SG,Omega,colour=Factor), size = 2) +
  theme_classic() +
  scale_x_continuous(limits = c(-0.01,0.8), breaks = c(0,0.2,0.4,0.6,0.8))+
  #scale_y_continuous(limits = c(-0.1,0.9), breaks = c(0,0.2,0.4,0.6,0.8)) +
  scale_y_continuous(limits = c(0.7,1.0), breaks = c(0.7,0.8,0.9,1.0)) +
  ylab('Omega') + xlab('CBCL Variance Explained by Factor') +
  scale_colour_manual(values = pallet, name = 'Factor')
#annotate('text', x = 0.7, y = 0.02, label=text,  size=5, parse = TRUE) +
#annotate('text', x = 0.705, y = 0.00, label=text2,  size=5, parse = TRUE)

#ggsave(paste0(plt_out, 'abcd_FC_Omega_rel.png'), plt, width = 4.5, height = 3)
ggsave(paste0(plt_out, 'confs_abcd_FC_Omega_rel.png'), plt, width = 4.5, height = 3)



plt <- ggplot() +
  geom_point(data=df, aes(ECV_SG,FD,colour=Factor), size = 2) +
  theme_classic() +
  scale_x_continuous(limits = c(-0.01,0.8), breaks = c(0,0.2,0.4,0.6,0.8))+
  scale_y_continuous(limits = c(0.83,1.0), breaks = c(0.85,0.9,0.95,1.0)) +
  ylab('Factor determinacy') + xlab('CBCL Variance Explained by Factor') +
  scale_colour_manual(values = pallet, name = 'Factor')
#annotate('text', x = 0.7, y = 0.02, label=text,  size=5, parse = TRUE) +
#annotate('text', x = 0.705, y = 0.00, label=text2,  size=5, parse = TRUE)

#ggsave(paste0(plt_out, 'abcd_FC_FD_rel.png'), plt, width = 4.5, height = 3)
ggsave(paste0(plt_out, 'confs_abcd_FC_FD_rel.png'), plt, width = 4.5, height = 3)



plt <- ggplot() +
  geom_point(data=df, aes(Omega,FD,colour=Factor), size = 2) +
  theme_classic() +
  #scale_x_continuous(limits = c(-0.1,0.9), breaks = c(0,0.2,0.4,0.6,0.8)) +
  scale_x_continuous(limits = c(0.7,1.0), breaks = c(0.7,0.8,0.9,1.0)) +
  scale_y_continuous(limits = c(0.83,1.0), breaks = c(0.85,0.9,0.95,1.0)) +
  ylab('Omega') + xlab('Factor determinacy') +
  scale_colour_manual(values = pallet, name = 'Factor')
#annotate('text', x = 0.7, y = 0.02, label=text,  size=5, parse = TRUE) +
#annotate('text', x = 0.705, y = 0.00, label=text2,  size=5, parse = TRUE)

#ggsave(paste0(plt_out, 'abcd_FC_FD_rel.png'), plt, width = 4.5, height = 3)
ggsave(paste0(plt_out, 'confs_abcd_FC_Omega_FD.png'), plt, width = 4.5, height = 3)


# 
# 
# # Null model
# set.seed(123456)           # Set seed
# 
# all_cors = list()
# 
# for (i in seq(1,1000)) {
#   r_rand <- sample(df$r)
#   null_cor <- cor(df$rel,r_rand)
#   all_cors[i] <- null_cor
# }
# 
# empirical = cor(df$r,df$rel)
# 
# all_cors <- do.call(rbind.data.frame, all_cors)
# colnames(all_cors) <- 'null_cors'
# 
# plt1 <- ggplot(all_cors, aes(null_cors)) + geom_histogram(fill="skyblue3", alpha=0.5, bins = 50) +
#   #scale_y_continuous(expand=c(0,0), limits = c(0,610), breaks = seq(0,600,100))+
#   theme_classic()+
#   theme(legend.position="none") + 
#   geom_segment(size= 1,
#                aes(x = empirical, y = 0, 
#                    xend = empirical, 
#                    yend = 30, colour= "red")) +
#   #geom_segment(size= 0.5, aes(x = sort_perm[950], y = 0, xend = sort_perm[950], yend = 60), colour = "gray60") +
#   xlab('r') + 
#   ylab('count')
# 
# ggsave(paste0(plt_out, 'null_model_cor_ef_rel.png'), plt1, width = 2, height = 2)
# 

plt <- ggplot(data=df, aes(OmegaH,test_Pearson_r,colour=ECV_SG)) +
  #geom_point(data=df, aes(Reliability_r_corrected,test_Pearson_r,colour=Factor), size = 2) +
  geom_point(size = 2) +
  theme_classic() +
  ylab('Accuracy r(pred,observed)') + xlab('Omega') +
  scale_x_continuous(limits = c(0,1.0), breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  scale_y_continuous(limits = c(-0.03,0.17), breaks = c(0,0.04,0.08,0.12,0.16))+
  #scale_colour_continuous(type = "viridis") +
  scale_colour_scico(palette = "vik", midpoint = 0.3) +
  labs(color = "Expl. var.") +
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 13),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13))
ggsave(paste0(plt_out, 'confs_FC_cor_rel_test_Pearson_r_sig_expl_var.png'), plt, width = 4.5, height = 3.5)



