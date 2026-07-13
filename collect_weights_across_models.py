
# codna env to use: FC_stability

# Imports
import pandas as pd
import numpy as np
import os
import glob
import matplotlib.pyplot as plt

from nilearn import connectome
from scipy.stats import pearsonr, spearmanr
from pathlib import Path
#from netneurotools import stats # old doesnt work anymore with update
from neuromaps.nulls.spins import gen_spinsamples

# if using brainsmash
#from scipy.spatial.distance import squareform, pdist
#from netneurotools.networks import match_length_degree_distribution
#from brainsmash.workbench.geo import volume


# function for spin test using neuromaps (used to be netneurotools) 
def corr_spin(x, y, spins, nspins):
    """
    version of corr_spin applied to matrices
    """
    n_nodes = len(x)
    mask = np.triu(np.ones(n_nodes), 1) > 0
    rho, _ = spearmanr(x[mask], y[mask])
    null = np.zeros((nspins,))

    # null correlation
    for i in range(nspins):
        ynull = y[np.ix_(spins[:, i], spins[:, i])]
        null[i], _ = spearmanr(x[mask], ynull[mask])

    pval = (1 + sum(abs((null - np.mean(null))) >
                    abs((rho - np.mean(null))))) / (nspins + 1)
    return rho, pval




### GLOBAL PARAMS ###
nspins = 10000
#####################


# PATHS
wd = os.getcwd()
wd = Path(os.path.dirname(wd))
wd = Path(os.path.dirname(wd))

data = wd / 'res' / 'collected_weights'

out = wd / 'res' / 'spin'
out.mkdir(parents=True, exist_ok=True)

out_plt = wd / 'res' / 'FC_weight_plots'
out_plt.mkdir(parents=True, exist_ok=True)

# coordinates for spins
coords = pd.read_table(f'{wd}/input/atlas/sphere_HCP.txt', sep='\s+', header=None)
hemiid = np.concatenate([np.repeat(0,180), np.repeat(1,180)])

# Network ordering
indsort = np.loadtxt(f'{wd}/input/atlas/cortex_subcortex_community_order.txt', dtype=int) - 1 
indsort.shape = (len(indsort),1)


### START WITH THE BIFACTORS AND SUM SCORES ###

# Input parameters BIFACTORS and SUM SCORES
transform = '_haufe' # haufe or ''
pipe = 'ridgeCV_zscore_group_2Fold_confound_removal_wcategorical'
feature = 'rest_3517'
session = 'baseline'    # baseline or followup

# Which models
models = ['P','E','I','cbcl_scr_syn_totprob','cbcl_scr_syn_external','cbcl_scr_syn_internal']

all_mean_weights = []

# do it
for model in models:
    print(model)
    
    path2data = wd / f"res/weights{transform}"

    if model == 'A': # attention naming is a little different
        globber = os.path.join(f"{path2data}", f"*{pipe}*{feature}*abcd_cbcl_grps_model_fits_{session}_{model}_*")
        files = glob.glob(globber)
        globber = os.path.join(f"{path2data}", f"*{pipe}*{feature}*abcd_cbcl_grps_model_fits_{session}_{model}T*")
        files += glob.glob(globber)
    else:
        globber = os.path.join(f"{path2data}", f"*{pipe}*{feature}*abcd_cbcl_grps_model_fits_{session}_{model}*")
        files = glob.glob(globber)
    #print(files)

    all_files = []
    for file in files:
        print(file)

        f = pd.read_csv(file, index_col=0).mean(axis=1)
        all_files.append(f)

    df_connectomes = pd.concat(all_files, axis=1)

    # Correlate within model group
    print('Correlating individual models...')
    all_model_corrs = df_connectomes.corr()

    # save => needs to be tested
    pattern = r'_baseline_(.*?)\-rseed'
    model_names = [match.group(1) for f in files if (match := re.search(pattern, f))]

    all_model_corrs.columns = model_names
    all_model_corrs.index = model_names
    file_to_save = f"{data}/res/{pipe}_{feature}_{session}_correlation_between_weights{transform}_all_models_{model}.csv"
    #all_model_corrs.to_csv(file_to_save, index=False)
    #print(f'saved {file_to_save}')

    # mean across all models within category
    m_weights = df_connectomes.mean(axis=1)
    all_mean_weights.append(pd.Series(m_weights)) #for now cat do as spatial nulls for cortical impossible

    # calculate node values (diag is nan so col average is ok)
    m_weights_mat = connectome.vec_to_sym_matrix(m_weights,diagonal=np.repeat(np.nan,379))
    roi_means = np.nanmean(m_weights_mat, axis = 0)
    
    # SAVE PLOT of weights
    #display = plotting.plot_matrix(m_weights_mat)
    m_weights_mat_sorted = m_weights_mat[indsort,indsort.T]

    # DEVIDE BY SD FOR VISUALISATION
    m_weights_mat_sorted = m_weights_mat_sorted / np.nanstd(m_weights_mat_sorted)
    
    cmap_custom = plt.cm.RdBu_r
    max_abs_val = np.nanmax(np.abs(m_weights_mat_sorted))

    plt.figure(figsize=(7, 7))
    plt.imshow(m_weights_mat_sorted, origin='lower', cmap=cmap_custom, vmin=-max_abs_val, vmax=max_abs_val)
    cbar = plt.colorbar(fraction=0.046)
    ticks = [-max_abs_val, -0.5 * max_abs_val, 0, 0.5 * max_abs_val, max_abs_val]
    cbar.set_ticks(ticks)
    #cbar.ax.set_yticklabels(['{:.2g}'.format(t) for t in ticks]) # Format tick labels to two significant digits
    cbar.ax.set_yticklabels(['{:.2g}'.format(t) for t in ticks], fontsize=20, fontdict={'fontname': 'Arial'}) # Format tick labels to two significant digits
    cbar.ax.tick_params(labelsize=20)
    plt.xticks(fontname='Arial')
    plt.yticks(fontname='Arial')
    plt.tick_params(labelsize=20)

    file2save = file.split('/')[7].split('.')[0].split(f'_{model}_')[0]
    file2save = f"{out_plt}/{file2save}_mean_weights{transform}_all_models_{model}.pdf"
    print(f'saving: {file2save}')
    plt.savefig(f'{file2save}', format = 'pdf', dpi=250)
    plt.close()
    #display.figure.clear()


### ADD CORRELATED FACTORS ###

# Correlated factors params
transform = '_haufe' # haufe or ''
pipe = 'ridgeCV_zscore_group_2Fold_confound_removal_wcategorical'
feature = 'rest_ALL_subs'
session = 'cor_factors_baseline'    # baseline or followup or cor_factors_baseline

# Which models
models = ['E','I']

# do it
for model in models:
    print(model)

    path2data = wd / f"res/weights{transform}"

    globber = os.path.join(path2data, f"*{pipe}*{feature}*abcd_cbcl_grps_model_fits_{session}_{model}*")
    files = glob.glob(globber)
#print(files)

    all_files = []
    for file in files:
        print(file)

        f = pd.read_csv(file, index_col=0).mean(axis=1)
        all_files.append(f)

    df_connectomes = pd.concat(all_files, axis=1)

    # Correlate within model group
    print('Correlating individual models...')
    all_model_corrs = df_connectomes.corr()

    # save => needs to be tested
    pattern = r'_baseline_(.*?)\-rseed'
    model_names = [match.group(1) for f in files if (match := re.search(pattern, f))]

    all_model_corrs.columns = model_names
    all_model_corrs.index = model_names
    file_to_save = f"{data}/res/{pipe}_{feature}_{session}_correlation_between_weights{transform}_all_models_{model}_cor_factors.csv"
    #all_model_corrs.to_csv(file_to_save, index=False)
    #print(f'saved {file_to_save}')

    # mean across all models within category
    m_weights = df_connectomes.mean(axis=1)
    all_mean_weights.append(pd.Series(m_weights)) #for now cat do as spatial nulls for cortical impossible

    # calculate node values (diag is nan so col average is ok)
    m_weights_mat = connectome.vec_to_sym_matrix(m_weights,diagonal=np.repeat(np.nan,379))
    roi_means = np.nanmean(m_weights_mat, axis = 0)
    
    # SAVE PLOT of weights
    #display = plotting.plot_matrix(m_weights_mat)
    m_weights_mat_sorted = m_weights_mat[indsort,indsort.T]

    # DEVIDE BY SD FOR VISUALISATION
    m_weights_mat_sorted = m_weights_mat_sorted / np.nanstd(m_weights_mat_sorted)
    
    cmap_custom = plt.cm.RdBu_r
    max_abs_val = np.nanmax(np.abs(m_weights_mat_sorted))

    plt.figure(figsize=(7, 7))
    plt.imshow(m_weights_mat_sorted, origin='lower', cmap=cmap_custom, vmin=-max_abs_val, vmax=max_abs_val)
    cbar = plt.colorbar(fraction=0.046)
    ticks = [-max_abs_val, -0.5 * max_abs_val, 0, 0.5 * max_abs_val, max_abs_val]
    cbar.set_ticks(ticks)
    #cbar.ax.set_yticklabels(['{:.2g}'.format(t) for t in ticks]) # Format tick labels to two significant digits
    cbar.ax.set_yticklabels(['{:.2g}'.format(t) for t in ticks], fontsize=20, fontdict={'fontname': 'Arial'}) # Format tick labels to two significant digits
    cbar.ax.tick_params(labelsize=20)
    plt.xticks(fontname='Arial')
    plt.yticks(fontname='Arial')
    plt.tick_params(labelsize=20)

    file2save = file.split('/')[7].split('.')[0].split(f'_{model}_')[0]
    file2save = f"{out_plt}/{file2save}_mean_weights{transform}_all_models_{model}.pdf"
    print(f'saving: {file2save}')
    plt.savefig(f'{file2save}', format = 'pdf', dpi=250)
    plt.close()
    #display.figure.clear()


models = ['P','E','I','cbcl_scr_syn_totprob','cbcl_scr_syn_external','cbcl_scr_syn_internal', 'extern_cor_factors','intern_cor_factors']

print('Correlating model averages...')
df_model_means = pd.concat(all_mean_weights, axis=1)
model_corrs = df_model_means.corr('spearman')
df_model_means.columns = models

N = df_model_means.shape[1]

# Initialize correlation and p-value matrices
corr_matrix = np.zeros((N, N))
pval_matrix = np.zeros((N, N))

# Set the number of permutations
#eu_distance = squareform(pdist(coords, metric = "euclidean"))
#n_nodes = len(coords)

# Loop over the upper triangle of the matrix
for i in range(N):
    for j in range(i, N):
                
        if i == j:
            corr_matrix[i, j] = 1
            pval_matrix[i, j] = np.nan
            continue
        else:

            print(f'Doing spins for correlation of {df_model_means.iloc[:,i].name} and {df_model_means.iloc[:,j].name}')
            
            # Compute the observed correlation
            # r_obs, _ = pearsonr(df_model_means[i], df_model_means[j])
            # corr_matrix[i, j] = r_obs

            model_i = connectome.vec_to_sym_matrix(df_model_means.iloc[:,i],diagonal=np.repeat(np.nan,379))
            model_j = connectome.vec_to_sym_matrix(df_model_means.iloc[:,j],diagonal=np.repeat(np.nan,379))

            model_i_cortical = model_i[0:360,0:360]
            model_j_cortical = model_j[0:360,0:360]

            np.fill_diagonal(model_i_cortical, 1)
            np.fill_diagonal(model_j_cortical, 1)

            # Perform permutation test
            # calculate nulls
            #spins = stats.gen_spinsamples(coords.to_numpy(), hemiid, n_rotate=nspins, method='hungarian')
            spins = gen_spinsamples(coords.to_numpy(), hemiid, n_rotate=nspins, method='hungarian')
            r, p = corr_spin(model_i_cortical, model_j_cortical, spins, nspins)

            corr_matrix[i, j] = r
            pval_matrix[i, j] = p

            # # for Brainsmash
            # Path(output_dir).mkdir(parents=True, exist_ok=True)
            # filenames = volume(coords, output_dir)

            # for k in range(nspins):
            #     fc_rewired, _ = match_length_degree_distribution(emp, eu_distance, 10, nnodes*20)

            #     # null[k] = np.mean(dis_sim[mask] # for plotting - gets upper tri????
            #     #                 [np.where(fc_rewired[mask] == 1)]) \
            #     #     - np.mean(dis_sim[mask]
            #     #             [np.where(fc_rewired[mask] == 0)])


            # # Compute the p-value
            # pval = np.sum(np.abs(permuted_corrs) >= np.abs(r_obs)) / n_permutations
            # pval_matrix[i, j] = pval


# Mirror the upper triangle to the lower triangle to complete the symmetric matrices
corr_matrix += np.triu(corr_matrix, k=1).T
pval_matrix += np.triu(pval_matrix, k=1).T

#np.savetxt(f'{out}/spin_cortical_all_factors_vs_sums_cmat.txt', corr_matrix)
#np.savetxt(f'{out}/spin_cortical_all_factors_vs_sums_pmat.txt', pval_matrix)

