
library(tidyverse)
library(corrplot)
library(psych)
library(lme4)


# Paths
old_path = '/home/mgell/Work/pfactor_reliability/'
new_path = '/Users/mgell/Work/pfactor/'

plt_out = paste0(new_path,"plots/")

# Data
df1 = read_csv(paste0(new_path,'res/ABCD_CBCL_factor_scores_baseline.csv'))
df2 = read_csv(paste0(new_path,'res/ABCD_CBCL_factor_scores_followup.csv'))

df1$retest_interval <- df2$interview_age - df1$interview_age

df2 <- df2[df1$retest_interval > 1,]
df1 <- df1[df1$retest_interval > 1,] # 1 sub with retest interval of 1 - likely a mistake?


df1_factors <- df1[13:61]
df1_summary <- df1[62:72]

df2_factors <- df2[13:61]
df2_summary <- df2[62:72]


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

df1_factors <- df1_factors %>%
  rename(!!!setNames(column_mapping_df$old_name,as.character(column_mapping_df$new_name))) %>%
  select(c(everything(), column_mapping_df$new_name))
df2_factors <- df2_factors %>%
  rename(!!!setNames(column_mapping_df$old_name,as.character(column_mapping_df$new_name))) %>%
  select(c(everything(), column_mapping_df$new_name))




cormat <- cor(df1_factors, df1_factors, use = "pairwise.complete.obs", method = "spearman")
#cormat <- cor(df2_factors, df2_factors, use = "pairwise.complete.obs")#,method = "spearman")

corrplot(cormat, method = 'shade', tl.col = 'black', tl.cex = 0.8)



colnames(df1_summary) <- c("CBCL_Anx_Dep","CBCL_Withdraw_Dep","CBCL_Somatic","CBCL_social","CBCL_Thought",
                           "CBCL_Attention","CBCL_Rule_break","CBCL_aggress","CBCL_Internalizing",
                           "CBCL_Externalizing","CBCL_Total_score")

cormat <- cor(df1_summary, df1_summary, use = "pairwise.complete.obs",method = "spearman")
#cormat <- cor(df2_summary, df2_summary, use = "pairwise.complete.obs")#,method = "spearman")

corrplot(cormat, method = 'shade', tl.col = 'black', tl.cex = 0.8)




cormat <- cor(df1_summary, df1_factors, use = "pairwise.complete.obs", method = "spearman")
#cormat <- cor(df2_factors, df2_summary, use = "pairwise.complete.obs")#,method = "spearman")

corrplot(cormat, method = 'shade', tl.col = 'black', tl.cex = 0.8, tl.srt = 65)
