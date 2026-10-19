
# imports
import numpy as np
import pandas as pd
from scipy import stats
from sklearn.pipeline import Pipeline
from threadpoolctl import threadpool_limits

__all__ = ['heuristic_C','filter_outliers','sort_files','transform2SD','cor_true_pred_pearson','cor_true_pred_pearson','mean_predictions','median_predictions','sd_predictions','test_cor_true_pred','prep_confs']


def heuristic_C(data_df=None):
    """
    Function from Vera/Kaustubh

    Calculate the heuristic C for linearSVR (Joachims 2002).

    Returns
    -------
    C : float
        Theoretically calculated hyperparameter C for a linear SVM.
    """

    if data_df is None:
        raise Exception('No data was provided.')

    C = 1/np.mean(np.sqrt((data_df**2).sum(axis=1)))
    print(f'Using C = {C} based on heuritic (C = 1/mean(sqrt(rowSums(data^2))))')
    # Formular Kaustubh: C = 1/mean(sqrt(rowSums(data^2)))

    return C


def filter_outliers(tab, beh, SD=3):
    print(f'\nFiltering outliers using {SD} SD...')
    tab['z'] = stats.zscore(tab.loc[:,[beh]])
    outliers = tab.loc[abs(tab['z']) > SD]
    tab = tab.loc[abs(tab['z']) < SD]
    if not outliers.empty:
        print(f'Removed {len(outliers)} outliers!')
        print(f'{outliers}')
    else:
        print('No outliers found!')
    
    return tab


def align_on_id(
    tab: pd.DataFrame,
    features: pd.DataFrame,
    target: str,
    pheno_id: str,
    features_id: str,
    id_dtype=str,
    verify: bool = True,
    verbose: bool = True,
):
    """
    Align two dataframes on subject IDs via an inner match, returning (tab_aligned, fcs_aligned)
    with identical subject membership and row order.
    """
    # copies + type normalize
    tab = tab.copy()
    features = features.copy()
    tab[pheno_id] = tab[pheno_id].astype(id_dtype)
    features[features_id] = features[features_id].astype(id_dtype)
    if id_dtype is str:
        tab[pheno_id] = tab[pheno_id].str.strip()
        features[features_id] = features[features_id].str.strip()

    # drop missing ids
    print(f'Behaviour data shape: {tab.shape}')
    tab = tab.dropna(subset=[target])
    print(f'Dropping Nans, new Behaviour data shape: {tab.shape}')

    print(f'Feature data shape: {features.shape}')
    features = features.dropna()
    print(f'Dropping Nans, new feature data shape: {features.shape}')

    # # drop duplicates within each frame
    # tab = tab.drop_duplicates(subset=[pheno_id], keep=keep)
    # features = features.drop_duplicates(subset=[features_id], keep=keep)

    if verify:
        if tab[pheno_id].duplicated().any():
            raise ValueError("Duplicate IDs in 'tab'; resolve before aligning.")
        if features[features_id].duplicated().any():
            raise ValueError("Duplicate IDs in 'features'; resolve before aligning.")

    # set index and sort for deterministic order
    tab_i = tab.set_index(pheno_id).sort_index()
    fcs_i = features.set_index(features_id).sort_index()

    # inner subject set
    ids = tab_i.index.intersection(fcs_i.index)

    # reindex both to the same ordered IDs
    tab_aln = tab_i.reindex(ids)
    fcs_aln = fcs_i.reindex(ids)

    if verify and not tab_aln.index.equals(fcs_aln.index):
        raise RuntimeError("Post-align indices differ unexpectedly.")

    if verbose:
        print("\nSorting and filtering...")
        print(f"Subjects in common: {len(ids)}")
        print(f"NEW Behaviour (tab) shape: {tab_aln.shape}")
        print(f"NEW FC (features) shape: {fcs_aln.shape}")

    return tab_aln.reset_index(), fcs_aln.reset_index()


def transform2SD(tab, beh, type):
    # e.g. call as tab, beh = transform2SD(tab, beh, 'outlier') # this way beh gets replaced by label
    if beh == 'nih_tlbx_agecsc_dominant':
        print('Splitting based on NIH norms: mean 100, sd 15')

        if type == 'split2bins':
            print(f'Using: {type}')
            label = f'{beh}_SD'
            tab[label] = 2
            tab[label] = np.where(tab[beh] >  100, tab[label] +1, tab[label])
            tab[label] = np.where(tab[beh] >= 115, tab[label] +1, tab[label])
            tab[label] = np.where(tab[beh] <= 100, tab[label] -1, tab[label])
            tab[label] = np.where(tab[beh] <=  85, tab[label] -1, tab[label])
            #plt.hist(tab[label],bins=5,histtype = 'bar',rwidth=0.8)
        elif type == 'outlier':
            print(f'Using: {type}')
            label = f'{beh}_SD'
            tab[label] = 1
            tab[label] = np.where(tab[beh] > 115, tab[label] +1, tab[label])
            tab[label] = np.where(tab[beh] <  85, tab[label] -1, tab[label])
    #label = f'{tab}_SD'
    #tab[label] = stats.zscore(tab.loc[:,[beh]])
    #outliers = tab.loc[abs(tab['z']) < 1]
    #tab = tab.loc[abs(tab['z']) > 1]


    print(beh)
    print(tab.shape)
    print(tab[beh].to_numpy())

    return tab, label


def cor_true_pred_pearson(y_true, y_pred):
    # >>> x = [1,2,3,4,5]
    # >>> y = [1,2,3,4,5]
    # >>> cor_true_pred_pearson(x,y)
    # 1.0
    # >>> y = [2,3,4,5,6]
    # >>> cor_true_pred_pearson(x,y)
    # 1.0
    # >>> y = [-1,-2,-3,-4,-5]
    # >>> cor_true_pred_pearson(x,y)
    # -1.0
    # >>> y = [1,2,3,4,10]
    # >>> cor_true_pred_pearson(x,y)
    # 0.8944271909999159
    # >>> y = [1,2,3,4,np.nan]
    # >>> cor_true_pred_pearson(x,y)
    # Traceback (most recent call last):
    # ... ValueError: array must not contain infs or NaNs
    cor, p = stats.pearsonr(y_true, y_pred)
    return cor

def cor_true_pred_spearman(y_true, y_pred):
    # >>> x = [1,2,3,4,5]
    # >>> y = [1,2,3,4,5]
    # >>> cor_true_pred_spearman(x,y)
    # 0.9999999999999999
    # >>> y = [2,3,4,5,6]
    # >>> cor_true_pred_spearman(x,y)
    # 0.9999999999999999
    # >>> y = [1,2,3,4,10]
    # >>> cor_true_pred_spearman(x,y)
    # 0.9999999999999999
    # >>> y = [-1,-2,-3,-4,-5]
    # >>> cor_true_pred_spearman(x,y)
    # -0.9999999999999999
    # >>> y = [-1,-2,-3,-4,np.nan]
    # >>> cor_true_pred_spearman(x,y)
    # nan
    # x = [1,2,3,4,5,6]
    # >>> y = [-1,-2,-3,4,5,6]
    # >>> cor_true_pred_spearman(x,y)
    # 0.7714285714285715
    # >>> y = [3,2,1,4,5,6]
    # >>> cor_true_pred_spearman(x,y)
    # 0.7714285714285715
    cor, p = stats.spearmanr(y_true, y_pred)
    return cor

def mean_predictions(y_true, y_pred):
    mean_pred = np.mean(y_pred)
    return mean_pred

def median_predictions(y_true, y_pred):
    median_pred = np.median(y_pred)
    return median_pred
    
def sd_predictions(y_true, y_pred):
    sd_pred = np.std(y_pred)
    return sd_pred

def test_cor_true_pred(y_true, y_pred):
    r, p = stats.spearmanr(y_true, y_pred)
    rho, p = stats.pearsonr(y_true, y_pred)
    cor = {'rho': rho, 'r': r}
    return cor


def final_estimator(model):
    """Last step of a fitted model, unwrapping GridSearchCV and Pipeline if present."""
    if hasattr(model, 'best_estimator_'):
        model = model.best_estimator_
    return model[-1] if isinstance(model, Pipeline) else model


def haufe_transform(fitted_model, X, y, label='', divide_by_var_yhat=False):
    """
    Haufe et al. (2014, NeuroImage) activation pattern of a fitted linear model with a single target:

        A = Cov(Z) w = cov(Z_i, y_hat)                                  (divide_by_var_yhat=False)
        A = Cov(Z) w / var(y_hat) = cov(Z_i, y_hat) / var(y_hat)        (divide_by_var_yhat=True)

    where Z are the features as the final estimator saw them (i.e. X), passed through all
    pipeline steps.

    Parameters
    ----------
    fitted_model : fitted estimator, Pipeline or GridSearchCV with a linear final estimator (coef_)
    X : pandas.DataFrame | np.ndarray
        Data the model was fitted on
    y : array
        Target the model was fitted on, only for sanity checks.
    label : str
        Name of the data used, only for printing.
    divide_by_var_yhat : bool
        For testing mostly
        False (default): A = cov(Z_i, y_hat)
        True: A = cov(Z_i, y_hat) / var(y_hat)

    Returns
    -------
    A : pandas.Series | None
        Haufe pattern, one value per feature seen by the final estimator. None if the model has no coef_.
    var_yhat : float | None
    """

    with threadpool_limits(limits=1):
        if hasattr(fitted_model, 'best_estimator_'):  # in case of GridSearchCV wrapped pipeline
            fitted_model = fitted_model.best_estimator_
        if not hasattr(final_estimator(fitted_model), 'coef_'):
            print(f'{type(final_estimator(fitted_model)).__name__} has no linear weights (coef_), skipping haufe transformation')
            return None, None

        y = np.ravel(y)
        print(f'predicting in {label} :{len(y)}')
        y_hat = fitted_model.predict(X)
        # This is only the training score, not used for model evaluation. Use CV accuracy as the performance!
        print('Verify:')
        print(f'predicted in:, {y_hat.shape}. Fold size: {len(y)}')
        print(f'r(y_hat, y) i.e. TRAIN score: {np.corrcoef(y_hat, y)}')

        # features the final estimator used
        if isinstance(fitted_model, Pipeline) and len(fitted_model) > 1:
            Z = np.asarray(fitted_model[:-1].transform(X), dtype=float)
        else:  # in case there was no preprocessing
            Z = np.array(X, dtype=float)
        print(f'features seen by final estimator: {Z.shape}, n coefs: {final_estimator(fitted_model).coef_.shape}')

        # covariance
        Z -= Z.mean(axis=0)  # center in place to avoid memory issues
        var_yhat = y_hat.var(ddof=1)
        A = Z.T @ (y_hat - y_hat.mean()) / (Z.shape[0] - 1)
        if divide_by_var_yhat:
            A /= var_yhat
    #
    return pd.Series(A), var_yhat



