
# Plot results

library(ggrepel)
library(tidyverse)
library(RColorBrewer)


pallet <- c("#0072B2", "#009E73", "#D55E00", "#F0E442", "#CC79A7")

plt_out = "/home/mgell/Work/pfactor_reliability/plots/prediction/"




# Prediction accuracy FC
df1 <- read_csv('/home/mgell/Work/pfactor_reliability/res/collected/ridgeCV_zscore_group_2Fold_confound_removal_wcategorical_averaged-source_HCP2016FreeSurferSubcortical_abcd_baselineYear1Arm1_rest_3517_zscored-beh_abcd_cbcl_grps_model_fits_baseline_all_behs.csv')
df1 <- df1[-c(1,13:67),]

df2 <- read_csv('/home/mgell/Work/pfactor_reliability/res/collected/ridgeCV_zscore_confound_removal_wcategorical_averaged-source_HCP2016FreeSurferSubcortical_abcd_baselineYear1Arm1_rest_3517_zscored-beh_ABCD_CBCL_factor_scores_baseline_all_behs.csv')
df2 <- df2[-1,]

cor.test(df1$test_Pearson_r,df2$test_Pearson_r)
cor.test(df1$test_R2,df2$test_R2)

plot(df1$test_Pearson_r,df2$test_Pearson_r)
plot(df1$test_R2,df2$test_R2)

