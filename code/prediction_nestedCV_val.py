#%%
# Imports
import os
import sys
import numpy as np
import pandas as pd
#import datatable as dt
import matplotlib.pyplot as plt

from pathlib import Path
from sklearn import metrics
from scipy.stats import zscore

from func.utils import align_on_id, cor_true_pred_pearson, cor_true_pred_spearman, prep_confs, mean_predictions
from func.models import model_choice
from sklearn.model_selection import ShuffleSplit, cross_validate, learning_curve, train_test_split, RepeatedKFold, KFold, GridSearchCV, GroupShuffleSplit, GroupKFold, permutation_test_score


### Set params ###
# FC_file = sys.argv[1]
# beh_file = sys.argv[2]
# beh = sys.argv[3]
# pipe = sys.argv[4]

k_inner = 5             # k folds for hyperparam search
k_outer = 10            # k folds for CV
n_outer = 5             # n repeats for CV
rs = 123456             # random state: int for reproducibility or None

predict = True          # predict or just subsample?
subsample = False       # Subsample data and compute learning curves?
remove_confounds = True # Remove confounds?
confs_in_file = False   # False = confs in beh file, otherwise it loads them from empirical data
confounds = ['sex']
categorical = ['sex']   # of which categorical?

zscr = True             # zscore features

external_validation = False
#val_FC_file = 'HCP2016FreeSurferSubcortical_abcd_baselineYear1Arm1_rest_3435.jay' #sys.argv[5] #'HCP2016FreeSurferSubcortical_abcd_baselineYear1Arm1_rest_3517.jay' #sys.argv[5]
#val_beh_file = 'OLD/abcd_cbcl_grp1_3434_model_fits.csv' #sys.argv[6] #'abcd_cbcl_grp2_3517_model_fits.csv' #sys.argv[6]
val_beh = beh

val_split = False       # Split data to train and held out validation?
val_split_size = 0.2    # Size of validation held out sample

manual_val_split = False # use a grouping variable to split off validation

grouping = 'matched_group'     # column that should be used to split the dataset
haufe_inversion = True

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

# attach confounds to tab_all if not there already before filtering
if remove_confounds:
    if confs_in_file:
        tab_all = prep_confs(tab_all, wd, FC_file)

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
    nested, model, grid = model_choice(pipe,X=FCs,confound=confounds,cat_columns=categorical)
    FCs.reset_index(inplace=True, drop=True)
    print(f'Running prediction with {model}')

    for conf_i in confounds:
        print(f'Adding {conf_i}')
        conf2remove = tab[f'{conf_i}']
        conf2remove.reset_index(inplace=True, drop=True)
        #FCs = FCs.append(tab[f'{conf_i}'], ignore_index=True)
        FCs[f'{conf_i}'] = conf2remove
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
        scores = cross_validate(grid_search, X, np.ravel(y), scoring=scoring, cv=outer_cv,
            return_train_score=True, return_estimator=True, verbose=3, n_jobs=1)
        # results
        cv_res = pd.DataFrame(scores)
        for i in cv_res.loc[:,'estimator']: print(i.best_estimator_)             # put to utils?

        # Print results
        mean_accuracy = cv_res.mean()
        print(f'Overall MEAN accuracy:')
        print(mean_accuracy)

        sd_accuracy = cv_res.std()
        print(f'Overall SD accuracy:')
        print(sd_accuracy)
        #
    elif nested == 0: # non-nested CV
        print('Using vanilla CV!')
        print(f'CV with {n_outer}x{k_outer}:')
        scores = cross_validate(model, X, np.ravel(y), scoring=scoring, cv=outer_cv,
            return_train_score=True, return_estimator=True, verbose=3, n_jobs=1)
        # results
        cv_res = pd.DataFrame(scores)
        if pipe == 'ridgeCV':                                                   # put to utils?
            for i in scores['estimator']: print(i.alpha_)
        elif pipe == 'ridgeCV_zscore':                                          # put to utils?
            for i in scores['estimator']: print(i[1].alpha_)                       # put to utils?
        elif pipe.__contains__('confound'):
            for i in scores['estimator']: print(i[2].alpha_)    

        # Print results
        mean_accuracy = cv_res.mean()
        print(f'Overall MEAN accuracy:')
        print(mean_accuracy)

        sd_accuracy = cv_res.std()
        print(f'Overall SD accuracy:')
        print(sd_accuracy)
        #
    elif nested == 99:
        print('Using stratified vanilla CV!')
        splits = (n_outer*k_outer)
        train_size = 0.7
        group = 'Family_ID'
        print(f'CV with {splits} splits of {round((1-train_size)*100)}% of groups out')
        print(f'Grouping variable: {group}\n')
        outer_cv = GroupShuffleSplit(n_splits=splits, train_size=train_size, random_state=rs)
        scores = cross_validate(model, X, np.ravel(y), groups=tab[group], scoring=scoring, cv=outer_cv,
            return_train_score=True, return_estimator=True, verbose=3, n_jobs=1)
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
            scores = cross_validate(grid_search, X, np.ravel(y), groups=tab[grouping], scoring=scoring, cv=outer_cv,
            return_train_score=True, return_estimator=True, verbose=3, n_jobs=1)
            cv_res = pd.DataFrame(scores)

            # After cross-validation
            for i, grid_search in enumerate(scores['estimator']):
                if hasattr(grid_search, "best_params_"):  # Ensure it's a GridSearchCV object
                    print(f"Outer fold {i + 1} best parameters: {grid_search.best_params_}")
                else:
                    print(f"Outer fold {i + 1} is not a GridSearchCV object.")
        else:
            print('\nRunning prediction with empirical results...')
            scores = cross_validate(model, X, np.ravel(y), groups=tab[grouping], scoring=scoring, cv=outer_cv,
                return_train_score=True, return_estimator=True, verbose=3, n_jobs=1)
            cv_res = pd.DataFrame(scores)

        weights = []

        if pipe.endswith('_zscore_group_2Fold'):
            for i in scores['estimator']: 
                print(i[1].alpha_)
                weights.append(pd.Series(i[1].coef_))
        elif pipe.endswith('_confound_removal_wcategorical'):
            for i in scores['estimator']: 
                print(i[2].alpha_)
                weights.append(pd.Series(i[2].coef_))
                #
            if haufe_inversion:
                grp = 0
                haufe = []
                # extract haufe inverted weights
                for best_model in scores['estimator']:
                    print('Transforming weights with haufe transformation')
                    y_grp = np.ravel(y)
                    y_grp = y_grp[train_index[1][grp]]
                    X_grp = X.iloc[train_index[1][grp]]
                    print(f'predicting in train group {grp} :{len(y_grp)}')
                    y_hat = best_model.predict(X_grp)
                    print('Verify:')
                    print(f'predicted in:, {y_hat.shape}. Fold size: {len(y_grp)}')
                    print(f'r(y_hat, y) i.e. train score: {np.corrcoef(y_hat,y_grp)}')
                    # now haufe transform
                    #test code for single edge: Xpq = X_grp['0']; cov_mat = np.cov(np.vstack((zscore(Xpq),y_hat)))
                    X_grp.pop(X_grp.keys()[71631])  # drop the confound column for haufe transform -> needs to be fixed, currently hard coded
                    X_normalized = X_grp.apply(lambda V: zscore(V), axis=0, result_type='broadcast') #zscore(X_grp, axis=0)
                    stacked_matrix = np.vstack((X_normalized.T, y_hat)).T
                    # calculate the covariance matrix
                    #cov_matrix = np.cov(stacked_matrix, rowvar=False)
                    # extract upper triangle for covariances between each column of X and y_hat
                    #covariances = cov_matrix[:-1, -1] this crashed sometimes
                    _y = stacked_matrix[:, -1]
                    covariances = stacked_matrix[:, :-1].T @ (_y - _y.mean()) / (stacked_matrix.shape[0] - 1)
                    haufe.append(pd.Series(covariances))
                    grp+= 1     
        else:
            try:
                for i in scores['estimator']: 
                    print(i.alpha_)
                    weights.append(pd.Series(i.coef_))
            except:
                print('NOT PRINTING alphas')
        
        # should you mean across coefficients? No, save both so you can check both for consistency?
        if weights:
            w_file = out_dir / 'weights'
            w_file.mkdir(parents=True, exist_ok=True)
            w_file = w_file / f"pipe_{pipe}-source_{src_fc}-beh_{beh_f[len(beh_f)-1]}_{beh}-rseed_{rs}-weights.csv"
            print(f'saving: {w_file}')
            weights = pd.concat(weights, axis=1)
            print(weights.shape)
            weights.to_csv(w_file)
        if haufe_inversion:
            w_file = out_dir / 'weights_haufe'
            w_file.mkdir(parents=True, exist_ok=True)
            w_file = w_file / f"pipe_{pipe}-source_{src_fc}-beh_{beh_f[len(beh_f)-1]}_{beh}-rseed_{rs}-weights.csv"
            print(f'saving: {w_file}')
            haufe = pd.concat(haufe, axis=1)
            print(haufe.shape)
            haufe.to_csv(w_file)

        # Print results
        mean_accuracy = cv_res.drop(columns = 'estimator').mean()
        print(f'Overall MEAN accuracy:')
        print(mean_accuracy)

    ## SAVE
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



#%%

###
# NOTE THAT THIS SECTION HAS NOT BEEN TESTED FULLY and is not part of the main pipeline
###

if val_split:
    #print('\nWARNING: TAKING FIRST ESTIMATOR FROM ALL CVs.')
    #print('IF DIFFERENT WILL IMPACT FOLLOWING RESULTS.')
    #params = cv_res.iloc[0,2].get_params

    # Predict on validation data
    # Now fit entire training set and predict leave out set
    #print(f"Fitting model with all training data: {params}")
    model.fit(X, np.ravel(y))
    # predict on validation data
    print("NOTE: THIS SECTION HAS NOT BEEN TESTED FULLY")
    print("evaluating on left out validation data")
    y_pred = model.predict(X_val)

    # validation results to be saved
    val_r = np.corrcoef(y_pred,np.ravel(y_val))
    val_r2 = model.score(X_val,y_val)
    val_MAE = metrics.mean_absolute_error(y_val,y_pred)
    val_rMSE = metrics.mean_squared_error(y_val,y_pred)
    val_res = {
        "r":[val_r[0,1]],
        "R2":[val_r2],
        "MAE":[val_MAE],
        "RMSE":[val_rMSE**(1/2)]
    }
    val_res = pd.DataFrame(val_res)
    print(f"on validation r(predicted,observed) = {val_r2}")

    # Save validation results
    out_file = out_dir / 'validation'
    out_file.mkdir(parents=True, exist_ok=True)
    out_file = out_file / f"pipe_{pipe}_averaged-source_{src_fc}-beh_{beh_f[len(beh_f)-1]}_{beh}-rseed_{rs}-validation_res.csv"
    print(f'saving averaged accuracy: {out_file}')
    val_res.to_csv(out_file, index=False)

    out_file = out_dir / 'predicted'
    out_file.mkdir(parents=True, exist_ok=True)
    out_file = out_file / f"pipe_{pipe}_averaged-source_{src_fc}-beh_{beh_f[len(beh_f)-1]}_{beh}-rseed_{rs}predicted_res.csv"
    print(f'saving averaged accuracy: {out_file}')
    np.savetxt(out_file, y_pred, delimiter=',')


if subsample:
    print('\nComputing learning curve...')
    # cv
    outer_cv = ShuffleSplit(n_splits=n_sample, test_size=k_sample, random_state=rs)
    if nested == 1:
        print('Using nested CV!')
        grid_search = GridSearchCV(estimator=model, param_grid=grid, n_jobs=1,
            cv=inner_cv, scoring="neg_root_mean_squared_error", verbose=3) # on Juseless n_jobs=None
        scores = learning_curve(grid_search, X, np.ravel(y), train_sizes=subsample_Ns, return_times=True, shuffle=True,
            cv=outer_cv, scoring='r2', verbose=3, n_jobs=1, random_state=rs)
    elif nested == 0:
        print('Using vanilla CV!')
        scores = learning_curve(model, X, np.ravel(y), train_sizes=subsample_Ns, return_times=True, shuffle=True,
            cv=outer_cv, scoring='r2', verbose=3, n_jobs=1, random_state=rs)

    # results
    train_size, train_scores, test_scores = scores[:3]
    sample_test_res = pd.DataFrame(np.transpose(test_scores), columns=train_size)
    sample_train_res = pd.DataFrame(np.transpose(train_scores), columns=train_size)

    print(f'Overall MEAN accuracy:')
    print(sample_test_res.mean())

    print(f'Overall SD accuracy:')
    print(sample_test_res.std())
    
    # save train
    out_file = out_dir / 'learning_curve'
    out_file.mkdir(parents=True, exist_ok=True)
    out_file = out_file / f"train-pipe_{pipe}-source_{''.join(FC_file.split('.')[0:-1])}-beh_{beh_f[len(beh_f)-1]}_{beh}-rseed_{rs}-cv_res.csv"
    print(f'saving: {out_file}')
    sample_train_res.to_csv(out_file, index=False)

    # save test
    out_file = out_dir / 'learning_curve'
    out_file = out_file / f"test-pipe_{pipe}-source_{''.join(FC_file.split('.')[0:-1])}-beh_{beh_f[len(beh_f)-1]}_{beh}-rseed_{rs}-cv_res.csv"
    print(f'saving: {out_file}')
    sample_test_res.to_csv(out_file, index=False)

    #cv_res = pd.DataFrame(scores)
    #print(f'Best hyperparams from nested CV {n_outer}x{k_outer}x{k_inner}:')
    #for i in cv_res.loc[:,'estimator']: print(i.best_estimator_)
