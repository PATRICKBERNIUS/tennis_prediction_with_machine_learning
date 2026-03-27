# Tennis Prediction with Machine Learning

This project aimed to predict the outcome of professional tennis matches using various 
machine learning methods. Data was sourced from the 
[TML Database GitHub Repository](https://github.com/Tennismylife/TML-Database), 
containing matches from 1968 to the present day. The bulk of this project involved 
feature engineering and implementing machine learning models, notably Logistic 
Regression, XGBoost, and Neural Networks.

View the full report here: **[View Full Report (PDF)](your-link-here)**

---

## Code Files Dictionary

The code files are a mix of Python and R.

### Python Files

| File | Description |
|---|---|
| `tennis_functions.py` | Core functions for recalculating features in Python |
| `h2h_tennis_to_crc.py` | Head-to-head feature calculation — sent to supercomputer due to long runtime |
| `time_and_rest_crc.py` | Time and rest feature calculation — sent to supercomputer due to long runtime |
| `win_streak_crc.py` | Win streak feature calculation — sent to supercomputer due to long runtime |
| `nn_tuning_for_crc.py` | Neural Network hyperparameter tuning — sent to supercomputer |
| `xgb_rand_tuning.py` | XGBoost random hyperparameter tuning — sent to supercomputer |

### R Files

| File | Description |
|---|---|
| `tennis_functions.R` | Core functions for recalculating features in R |
| `tennis_proj.Rmd` | Initial project file — data loading, cleaning, feature engineering, and dataset preparation. Project transitions to Python after this point. |

### Quarto Notebooks (.qmd)

| File | Description |
|---|---|
| `combine_tennis_csvs.qmd` | Combines all raw data sources into a single dataset |
| `feature_engin_take_2.qmd` | Primary feature engineering notebook — contains most feature calculation functions. Note: functions are slow and could be optimised further. |
| `feats_calc_for_aus_sim.qmd` | Feature calculations on new data for simulation testing |
| `NN_exploration.qmd` | Initial exploration of Neural Networks for match prediction |
| `tennis_proj_ml.qmd` | Main ML notebook — trains and evaluates Logistic Regression, XGBoost, and Neural Networks. Includes hyperparameter tuning and comparison of model performance on training, validation, and test sets. |

### Simulation

| File | Description |
|---|---|
| `aus_open_2026_sim.Rmd` | Attempted simulation of the 2026 Australian Open. Challenging to implement as features must be recalculated after each round, making it highly time-consuming. |