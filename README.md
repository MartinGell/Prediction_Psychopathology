# Prediction of Psychopathology factors

This code is based on a prediction pipeline from https://github.com/MartinGell/Prediction_Reliability with minor changes and additions.

Currently 2 fold CV is implemented as two separate fit/predict routines for which the final score is averaged.

To do:
permutation test - can re-implement 2 fold CV using group argument - need to verify that it always produces the same split.
