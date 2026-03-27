This project aimed to predict the outcome of professional tennis matches using various machine learning methods.
Data was sourced from TML Database Github Repository linked here: https://github.com/Tennismylife/TML-Database.
The database contains matches from 1968 to the present day.
The bulk of this project involved feature engineering and implimenting machine learning models, notably Logistic Regression, XGBoost, and Nerual Networks.

If you would like to read the full report, please view it here: [View Report (PDF)](Sports%20Analytics%20Final%20Report%20V2.pdf)


Code Files Dictionary:

The code files are a mix of Python and R code.


NN_exploration.qmd - Initial exploration of using Neural Networks for predictiosn.


aus_open_2026_sim.Rmd - Attempt to simulate 2026 Australian Open. Difficult to impliment with model because it requires recalculation of features each round which is extremely time consuming.


combine_tennis_csvs.qmd - Combining all data together.


feats_calc_for_aus_sim.qmd - Performing feature calculatations on new data for testing.


feature_engin_take_2.qmd - Updated feautre calc functions to iron out some kinks. Contains most of the feature engineering functions for this project. Note that these functions are slow. Some more work could be put in to optimize for more speed.


h2h_tennis_to_crc.py - Script sent to supercomputer, as this function takes a long time to run.


nn_tuning_for_crc.py - Script sent to super computer for tuning Neural Network.


tennis_functions.R - file for functions I created in R to easily recalculate features.


tennis_functions.py = file for functions I created in Python to easily recalculate features.


tennis_proj.Rmd - Initial file for start of the project. Loading data, cleaning, calculating a few features, doubling dataset. Project is moved to Python after this.


tennis_proj_ml.qmd - This file contains the machine learning training and prediction part of the project. Implimenting multiple models, namely Logistic Regression, XGBoost, and Neural Networks, as well as tuning for the latter two. Compares models performance on training and validation data, as well as test performance on new data.


time_and_rest_crc.py - Script sent to supercomputer to calculate features, as this function takes a long time to run.


win_streak_crc.py - Script sent to supercomputer to calculate features, as this function takes a long time to run.


xgb_rand_tuning.py - Script sent to supercomputer for tuning XGBoost models.

