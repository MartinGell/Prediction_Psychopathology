
# Imports
import numpy as np
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt


# Paths
old_path = '/home/mgell/Work/pfactor_reliability/'
new_path = '/Users/mgell/Work/pfactor/'

p_values = np.loadtxt(f'{new_path}res/spin/spin_cortical_all_factors_vs_sums_pmat.txt')
corr_matrix = np.loadtxt(f'{new_path}res/spin/spin_cortical_all_factors_vs_sums_cmat.txt')

# Replace diagonal elements with NaN for clarity
np.fill_diagonal(corr_matrix, np.nan)
np.fill_diagonal(p_values, np.nan)

# Labels
# The order is: ['P','E','I','cbcl_scr_syn_totprob','cbcl_scr_syn_external','cbcl_scr_syn_internal', 'extern_cor_factors','intern_cor_factors']
labels = ["P-Factors", "Extern. Bifactors", "Intern. Bifactors", "Total Problems", "Externalising", "Internalising", "Extern. Cor. Factors", "Intern. Cor. Factors"]
corr_matrix = pd.DataFrame(corr_matrix, columns=labels, index=labels)
p_values = pd.DataFrame(p_values, columns=labels, index=labels)

# reorder
new_order = ["Total Problems", "Externalising", "Internalising",
             "Extern. Cor. Factors", "Intern. Cor. Factors",
             "P-Factors", "Extern. Bifactors", "Intern. Bifactors"]

# reorder rows and columns
corr_matrix = corr_matrix.loc[new_order, new_order]
p_values = p_values.loc[new_order, new_order]

# Mask for upper triangle
mask = np.triu(np.ones_like(corr_matrix, dtype=bool))

# Function to convert p-values to significance stars
def p_to_star(p):
    if np.isnan(p):
        return ""
    elif p < 0.001:
        return "***"
    elif p < 0.01:
        return "**"
    elif p < 0.05:
        return "*"
    else:
        return ""

# Create annotation matrix as strings
annot_matrix = corr_matrix.copy().astype(str)
for i in range(corr_matrix.shape[0]):
    for j in range(corr_matrix.shape[1]):
        if mask[i,j]:
            annot_matrix.iat[i,j] = ""
        else:
            stars = p_to_star(p_values.iat[i,j])
            annot_matrix.iat[i,j] = f"{stars}\n{corr_matrix.iat[i,j]:.2f}"  # stars above correlation

fig, ax = plt.subplots(figsize=(10, 8))

# Plot heatmap
sns.heatmap(corr_matrix, mask=mask, annot=annot_matrix, fmt="", cmap="coolwarm",
            vmin=-1, vmax=1, ax=ax,
            annot_kws={"size": 16, "fontname": "Arial", "weight": "bold"},
            cbar_kws={"shrink": 0.8})

# Set labels
ax.set_xticklabels(new_order, rotation=45, ha="right", fontsize=18, fontname="Arial")
ax.set_yticklabels(new_order, rotation=0, fontsize=18, fontname="Arial")

# Move x-axis labels to top
ax.xaxis.set_ticks_position('bottom')

# Save
plt.savefig(f"{new_path}res/spin/heatmap_weights_all_factors.pdf", format="pdf", bbox_inches="tight")
#plt.savefig(f"{new_path}/plots/heatmap_weights.pdf", format="pdf", bbox_inches="tight")
plt.show()
