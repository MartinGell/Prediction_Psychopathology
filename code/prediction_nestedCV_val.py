#%%
# Imports
import os

os.environ["MKL_NUM_THREADS"] = "1"
os.environ["OMP_NUM_THREADS"] = "1"
os.environ["OPENBLAS_NUM_THREADS"] = "1"

import sys
import numpy as np
import pandas as pd
#import datatable as dt
import matplotlib.pyplot as plt

from pathlib import Path
from sklearn import metrics
from scipy.stats import zscore
from threadpoolctl import threadpool_limits

from func.utils import align_on_id, cor_true_pred_pearson, cor_true_pred_spearman, mean_predictions, final_estimator, haufe_transform
from sklearn.base import clone
from func.models import model_choice
from sklearn.model_selection import ShuffleSplit, cross_validate, learning_curve, train_test_split, RepeatedKFold, KFold, GridSearchCV, GroupShuffleSplit, GroupKFold, permutation_test_score


"""
This script is used to predict behavioural scores from functional connectivity (or other).
There is a number of options that can be tweaked, e.g., confound removal, z-scoring of features,
haufe transformation of weights, etc. in the script parameters section below.
The script is designed to be run from the command line with appropriate arguments.

It requires the following command line arguments in this exact order (no checking is done):
    1. input features: name of the table with input features (e.g., functional connectivity matrix).
    2. input phenotype: name of the table with input phenotypes (e.g., behavioural scores).
    3. phenotype to predict: The specific column name of the phenotype in the input phenotype table (e.g., P-factor).
    4. model: The machine learning model to use for prediction - available options in func/models.py
       (e.g., ridgeCV_zscore_group_2Fold_confound_removal_wcategorical,
       xgboost_group_2Fold_confound_removal_wcategorical, etc.).

It outputs the prediction results, including cross-validation scores and model weights (if requested), to the specified output directory.

Example usage (paths relative to project working directory):
    python prediction_nestedCV_val.py Example_Schaefer400x17_data.csv Example_factors_with_grouping.csv P_CLRK ridgeCV_zscore_group_2Fold_confound_removal_wcategorical

Author: Martin Gell
Date: 2022-09-15

Edited: 2024-06-10 (Martin Gell)

Updated: 2026-10-02 (Martin Gell)
"""




### Set params ###
FC_file = sys.argv[1]
beh_file = sys.argv[2]
beh = sys.argv[3]
pipe = sys.argv[4]

k_inner = 5             # k folds for hyperparam search
k_outer = 10            # k folds for CV
n_outer = 5             # n repeats for CV
rs = 123456             # random state: int for reproducibility or None

predict = True          # predict or just subsample?
subsample = False       # Subsample data and compute learning curves?
remove_confounds = True # Remove confounds?
confounds = ['interview_age', 'sex', 'parental_education', 'meanFD']
categorical = ['sex']   # of which categorical?

zscr = False             # zscore features

val_split = False       # Split data to train and held out validation?
val_split_size = 0.2    # Size of validation held out sample

manual_val_split = False # use a grouping variable to split off validation

grouping = 'matched_group'     # column that should be used to split the dataset

# Feature weights transformation
haufe_inversion = True  # haufe-transform weights of the model refit on all of X (excluding held-out validation)
haufe_per_fold = True   # also save haufe patterns of each CV fold model (e.g. for split-half reliability)

permute = False          # do permutation testing?
perm = 1000              # number or permutations if doings permutation testing

#res_folder = 'permutation' #sys.argv[7] #'disc_rep'    # save results separately to ...
#designator = 'test'    # string designation of output file

if subsample:
    #subsample_Ns = np.array([195,295,395]) # these are only train + 55 test makes 250, 350 and 450
    subsample_Ns = np.geomspace(250,4450,7).astype('int')
    n_sample = 100      # number of samples to draw from data
    k_sample = 0.1      # fraction of data to use as test set
    res_folder = 'subsamples'
    print(f'\nSubsampling: {subsample_Ns}, each {n_sample} times with {k_sample*100}% left out\n')

score_pearson = metrics.make_scorer(cor_true_pred_pearson, greater_is_better=True)
score_spearman = metrics.make_scorer(cor_true_pred_spearman, greater_is_better=True)
mean_prediction = metrics.make_scorer(mean_predictions, greater_is_better=True)

scoring = {"RMSE": "neg_root_mean_squared_error", 
            "MAE": "neg_mean_absolute_error", 
            "R2": "r2", 
            "Pearson_r": score_pearson, "Rho": score_spearman, 
            "mean_pred": mean_prediction
}


#%%
# paths
wd = os.getcwd()
wd = Path(os.path.dirname(wd))

out_dir = wd / 'res' 
if 'res_folder' in locals():
    out_dir = out_dir / res_folder
    out_dir.mkdir(parents=True, exist_ok=True)

# load behavioural measures
path2beh = wd / 'input' / 'pheno' / beh_file
tab_all = pd.read_csv(path2beh) # beh data
tab_all = tab_all.dropna(subset = [beh]) # drop nans if there are in beh of interest
print(f'\nUsing {beh}')
print(f'Behaviour data shape: {tab_all.shape}')

# check if confounds are columns in phenotype file
if remove_confounds:
    missing = [c for c in confounds if c not in tab_all.columns]
    if missing:
        raise ValueError(f'Confounds {missing} not found in {beh_file}; add them to the behaviour file.')

# load data and define leave out set
# table of subs (rows) by regions (columns)
path2FC = wd / 'input' / FC_file

print(f'\nUsing {path2FC}')
if path2FC.suffix == '.jay':
    features_all = dt.fread(path2FC)
    FCs_all = features_all.to_pandas()
else:
    FCs_all = pd.read_csv(path2FC)
print(f'Feature data shape: {FCs_all.shape}')

print(f'Dropping Nans, FC data shape: {FCs_all.shape}')
FCs_all = FCs_all.dropna()
print(f'NEW FC data shape: {FCs_all.shape}')

# Filter FC subs based on behaviour subs
tab, FCs = align_on_id(tab_all, FCs_all, target = beh, pheno_id="src_subject_id", features_id="src_subject_id")

# set up X and y for prediction
target = tab.loc[:, [beh]]
FCs.pop(FCs.keys()[0])
print('\nFCs after removing subjects:')
print(FCs.head())

# optionaly zscore FCs rowwise -> no data leakage
src_fc = ''.join(FC_file.split('.')[0:-1])
if zscr:
    print('\nZscoring FCs...')
    FCs = FCs.apply(lambda V: zscore(V), axis=1, result_type='broadcast')
    print('zscored')
    src_fc = f'{src_fc}_zscored'
else:
    print('\nNot zscoring!')

# set up for confound removal
if remove_confounds:
    print('\nSetting up for confound removal...')
    FCs.reset_index(inplace=True, drop=True)

    for conf_i in confounds:
        print(f'Adding {conf_i}')
        conf2remove = tab[f'{conf_i}']
        conf2remove.reset_index(inplace=True, drop=True)
        #FCs = FCs.append(tab[f'{conf_i}'], ignore_index=True)
        FCs[f'{conf_i}'] = conf2remove

    # build the model AFTER confounds are appended: the ColumnTransformer column lists are fixed once run
    nested, model, grid = model_choice(pipe,X=FCs,confound=confounds,cat_columns=categorical)
    print(f'Running prediction with {model}')
    if hasattr(model[0], 'transformers'):
        print(f"Last continuous columns: {model[0].transformers[0][2][-4:]}, categorical columns: {model[0].transformers[1][2]}")
else:
    print('\nNot removing confounds!')
    nested, model, grid = model_choice(pipe)
    print(f'Running prediction with {model}')


#FCs[tab_all.isna().any(axis=1)]

#%%
# remove hold out data
if val_split:
    print(f'\nSplitting into train and validation sets uisng {val_split_size} as validation...')
    X, X_val, y, y_val = train_test_split(FCs, target, test_size=val_split_size, random_state=rs)
    print(f'train size: {X.shape}')
    print(f'test size:  {X_val.shape}')
else:
    X = FCs
    y = target

# CV set up
inner_cv = KFold(n_splits=k_inner, shuffle=True, random_state=rs)
outer_cv = RepeatedKFold(n_splits=k_outer, n_repeats=n_outer, random_state=rs)

# extra stuff for naming files
beh_f = beh_file.split('.')[0]
beh_f = beh_f.split('/')


# PREDICTION
# Only predict if requested
if predict:
    print('\nRunning Prediction...')
    # Run CV 
    if nested == 1: # Nested CV with parameter optimization
        print('Using nested CV!')
        print(f'Hyperparam search with {n_outer}x{k_outer}x{k_inner} over:')
        print(f'{grid}')
        grid_search = GridSearchCV(estimator=model, param_grid=grid, n_jobs=1,
            cv=inner_cv, scoring="neg_root_mean_squared_error", verbose=3)    
        with threadpool_limits(limits=1):
            scores = cross_validate(grid_search, X, np.ravel(y), scoring=scoring, cv=outer_cv,
                return_train_score=True, return_estimator=True, return_indices=True, verbose=3, n_jobs=1)
        # train/test indices per fold, used for per-fold haufe
        cv_indices = scores.pop('indices') 
        # results
        cv_res = pd.DataFrame(scores)
        for i in cv_res.loc[:,'estimator']: print(i.best_estimator_)             # put to utils?
        #
    elif nested == 0: # non-nested CV
        print('Using vanilla CV!')
        print(f'CV with {n_outer}x{k_outer}:')
        with threadpool_limits(limits=1):
            scores = cross_validate(model, X, np.ravel(y), scoring=scoring, cv=outer_cv,
                return_train_score=True, return_estimator=True, return_indices=True,verbose=3, n_jobs=1)
        # train/test indices per fold, used for per-fold haufe
        cv_indices = scores.pop('indices') 
        # results
        cv_res = pd.DataFrame(scores)
        if pipe == 'ridgeCV':                                                   # put to utils?
            for i in scores['estimator']: print(i.alpha_)
        elif pipe == 'ridgeCV_zscore':                                          # put to utils?
            for i in scores['estimator']: print(i[1].alpha_)                       # put to utils?
        elif pipe.__contains__('confound'):
            for i in scores['estimator']: print(i[2].alpha_)    
        #
    elif nested == 99:
        print('Using stratified vanilla CV!')
        splits = (n_outer*k_outer)
        train_size = 0.7
        group = 'Family_ID'
        print(f'CV with {splits} splits of {round((1-train_size)*100)}% of groups out')
        print(f'Grouping variable: {group}\n')
        outer_cv = GroupShuffleSplit(n_splits=splits, train_size=train_size, random_state=rs)
        with threadpool_limits(limits=1):
            scores = cross_validate(model, X, np.ravel(y), groups=tab[group], scoring=scoring, cv=outer_cv,
                return_train_score=True, return_estimator=True, return_indices=True, verbose=3, n_jobs=1)
        # train/test indices per fold, used for per-fold haufe
        cv_indices = scores.pop('indices')
        # results
        cv_res = pd.DataFrame(scores)
        if pipe == 'ridgeCV': # put to utils?
            for i in scores['estimator']: print(i.alpha_)
        elif pipe.__contains__('confound'):
            for i in scores['estimator']: print(i[2].alpha_)
        else:
            for i in scores['estimator']: print(i[1].alpha_)
        #
    elif nested == 2: # two-fold prediction for ABCD
        print('\nUsing stratified nested 2-fold CV!')
        outer_cv = GroupKFold(n_splits=2)  # 2-fold cross-validation using GroupKFold with groups replaced GroupSchuffleSplit
        
        # check group sizes
        train_index, test_index = enumerate(outer_cv.split(X, y, tab[grouping]))
        print(f'grp 1 N:{len(train_index[1][0])}, grp 2 N:{len(test_index[1][0])}')

        if pipe.startswith('xgboost'):
            print(f'Hyperparam search over:')
            print(f'{grid}')
            grid_search = GridSearchCV(estimator=model, param_grid=grid, n_jobs=1,
            cv=inner_cv, scoring="neg_root_mean_squared_error", verbose=3) # on Juseless n_jobs=None
            with threadpool_limits(limits=1):
                scores = cross_validate(grid_search, X, np.ravel(y), groups=tab[grouping], scoring=scoring, cv=outer_cv,
                    return_train_score=True, return_estimator=True, return_indices=True, verbose=3, n_jobs=1)
            cv_res = pd.DataFrame(scores)
            cv_indices = scores.pop('indices')

            # After cross-validation
            for i, grid_search in enumerate(scores['estimator']):
                if hasattr(grid_search, "best_params_"):  # Ensure it's a GridSearchCV object
                    print(f"Outer fold {i + 1} best parameters: {grid_search.best_params_}")
                else:
                    print(f"Outer fold {i + 1} is not a GridSearchCV object.")
        else:
            print('\nRunning prediction with empirical results...')
            with threadpool_limits(limits=1):
                scores = cross_validate(model, X, np.ravel(y), groups=tab[grouping], scoring=scoring, cv=outer_cv,
                    return_train_score=True, return_estimator=True, return_indices=True, verbose=3, n_jobs=1)
            cv_res = pd.DataFrame(scores)
            cv_indices = scores.pop('indices')

    # DONE WITH PREDICTION 
    # Print results
    mean_accuracy = cv_res.drop(columns = 'estimator').mean()
    print(f'Overall MEAN accuracy:')
    print(mean_accuracy)

    # SAVE
    # CV results
    out_file = out_dir / 'cv'
    out_file.mkdir(parents=True, exist_ok=True)
    out_file = out_file / f"pipe_{pipe}-source_{src_fc}-beh_{beh_f[len(beh_f)-1]}_{beh}-rseed_{rs}-cv_res.csv"
    print(f'saving: {out_file}')
    cv_res.to_csv(out_file, index=False)

    # Averaged CV results
    out_file = out_dir / 'mean_accuracy'
    out_file.mkdir(parents=True, exist_ok=True)
    out_file = out_file / f"pipe_{pipe}_averaged-source_{src_fc}-beh_{beh_f[len(beh_f)-1]}_{beh}-rseed_{rs}-cv_res.csv"
    print(f'saving averaged accuracy: {out_file}')
    mean_accuracy.to_frame().transpose().to_csv(out_file, index=False)


    # WEIGHTS
    if haufe_inversion:
        print('\nRunning haufe transformation...')
        # use a fitted CV model, skip models without linear weights as that would error out
        cv_final_estimator = final_estimator(scores['estimator'][0]) if predict else None
        if predict and not hasattr(cv_final_estimator, 'coef_'): # this feels awkward, but works?
            print(f'\n{type(cv_final_estimator).__name__} has no linear weights (coef_), skipping haufe transformation!!\n')
        else:
            w_dir = out_dir / 'weights_haufe'
            w_dir.mkdir(parents=True, exist_ok=True)
            f_base = f"pipe_{pipe}-source_{src_fc}-beh_{beh_f[len(beh_f)-1]}_{beh}-rseed_{rs}"
            var_yhat_all = {}

            # refit the same estimator as in CV (incl. any GridSearchCV wrapper) on the full data
            # as the model variable may not have hyperparm search defined until in the CV block (e.g. RidgeCV() vs Ridge())
            full_model = clone(scores['estimator'][0]) if predict else clone(model)
            print(f'Refitting on full data: {X.shape}')
            with threadpool_limits(limits=1):
                full_model.fit(X, np.ravel(y))

            if hasattr(final_estimator(full_model), 'alpha_'):
                print(f'full data alpha: {final_estimator(full_model).alpha_}')

            # Do the actual haufe transform
            haufe, var_yhat_all['full_model_refit'] = haufe_transform(full_model, X, y, label='full data', divide_by_var_yhat=False)

            # Save haufe weights (i.e. A)
            if haufe is not None:
                w_file = w_dir / f"{f_base}-weights.csv"
                print(f'saving: {w_file}')
                haufe.to_frame(f'haufe_full_model_refit').to_csv(w_file)

            # Save haufe per CV fold? 
            if haufe_per_fold and haufe is not None:
                if not predict:
                    print('\nhaufe_per_fold needs predict = True to use a fitted model from CV, skipping!!\n')
                else:
                    haufe_folds = {}
                    for fold, (fold_model, train_idx, test_idx) in enumerate(zip(scores['estimator'], cv_indices['train'], cv_indices['test'])):
                        print(f'\nTransforming weights of fold {fold} with haufe transformation')
                        X_train = X.iloc[train_idx]
                        haufe_folds[f'haufe_fold_{fold}'], var_yhat_all[f'fold_{fold}'] = haufe_transform(
                            fold_model, X_train, np.ravel(y)[train_idx], label=f'train group {fold}', divide_by_var_yhat=False)
                        #
                        # recompute the test score to check against the CV results
                        y_true = np.ravel(y)[test_idx]
                        with threadpool_limits(limits=1):
                            y_pred = fold_model.predict(X.iloc[test_idx])
                        print(f'predicted in test group {fold} :{len(y_true)}')
                        print(f'r(y_pred, y_true) i.e. TEST score: {np.corrcoef(y_pred, y_true)}')

                    # correlation between the full refit and average fold's pattern
                    print(f'\nCorrelation between full refit and average fold pattern:') 
                    print(f'{np.corrcoef(pd.DataFrame(haufe_folds).mean(axis=1), haufe)[0, 1]}\n')

                    # Save per-fold haufe weights
                    # Mean across coefficients? No, save both so you can check both for consistency/split-half reliability?
                    w_file = w_dir / "per_fold_maps" / f"{f_base}-weights_folds.csv"
                    w_file.parent.mkdir(parents=True, exist_ok=True)
                    print(f'saving: {w_file}')
                    pd.DataFrame(haufe_folds).to_csv(w_file)

            # Save var(y_hat) per model, so you can rescale patterns later
            if haufe is not None: # this has to be here as predict = False + nonlinear weight model like xgboost will throw error
                w_file = w_dir / f"{f_base}-var_yhat.csv"
                print(f'saving: {w_file}')
                pd.Series(var_yhat_all, name='var_yhat_full_model_refit').to_csv(w_file, index_label='model')
        
    print('\nFINISHED WITH PREDICTION\n')

#%%

# Permutation testing adapted for 2-fold
if permute:
    if "group_2Fold" not in pipe:
        raise ValueError("You are probably using the wrong pipe argument - this permutation is set to group 2-fold CV.")
    
    print('\nUsing stratified nested 2-fold CV with permutation!')
    outer_cv = GroupKFold(n_splits=2)  # 2-fold cross-validation using GroupKFold with groups replaced GroupSchuffleSplit

    # check group sizes
    train_index, test_index = enumerate(outer_cv.split(X, y, tab[grouping]))
    print(f'grp 1 N:{len(train_index[1][0])}, grp 2 N:{len(test_index[1][0])}')

    print('\nFirst running prediction with empirical results...')
    scores = cross_validate(model, X, np.ravel(y), groups=tab[grouping], scoring=scoring, cv=outer_cv,
        return_train_score=True, return_estimator=True, verbose=3, n_jobs=1)
    cv_res = pd.DataFrame(scores)

    # Print results
    mean_accuracy = cv_res.drop(columns = 'estimator').mean()
    print(f'Overall MEAN accuracy:')
    print(mean_accuracy)

    print(f'\nRunning {perm} permutations...')
    score_empirical, perm_scores, pval = permutation_test_score(model, X, np.ravel(y), scoring=score_pearson, cv=outer_cv, 
                                                                groups=tab[grouping], n_permutations=perm, verbose=5)
    print(f"Score on original data: {score_empirical:.2f} (p-value: {pval:.3f})")

    # save perm results
    mean_accuracy['pvalue'] = pval
    cv_res = pd.DataFrame(perm_scores)
    cv_res.rename(columns={0:'perm_score'}, inplace=True)

    # SAVE
    # CV results
    out_file = out_dir / 'permutation' / 'permutation_scores'
    out_file.mkdir(parents=True, exist_ok=True)
    out_file = out_file / f"pipe_{pipe}-source_{src_fc}-beh_{beh_f[len(beh_f)-1]}_{beh}-rseed_{rs}-cv_res.csv"
    print(f'saving: {out_file}')
    cv_res.to_csv(out_file, index=False)

    # Averaged CV results
    out_file = out_dir / 'permutation' / 'mean_accuracy'
    out_file.mkdir(parents=True, exist_ok=True)
    out_file = out_file / f"pipe_{pipe}_averaged-source_{src_fc}-beh_{beh_f[len(beh_f)-1]}_{beh}-rseed_{rs}-cv_res.csv"
    print(f'saving averaged accuracy: {out_file}')
    mean_accuracy.to_frame().transpose().to_csv(out_file, index=False)

    print('\nFINISHED WITH PERMUTATION\n')


