from sklearn.model_selection import RandomizedSearchCV
from sklearn.model_selection import TimeSeriesSplit
from xgboost import XGBClassifier
import pandas as pd
import numpy as np



dat = dat = pd.read_csv("it_would_just_be_so_awesome_if_this_worked.csv")

mod_rand = XGBClassifier()

params = {
 "learning_rate" : [0.05, 0.10 ,0.15, 0.20, 0.25, 0.30],
 "max_depth" : [ 3, 4, 5, 6, 8, 10, 12, 15],
 "min_child_weight" : [ 1, 3, 5, 7 ],
 "gamma": [ 0.0, 0.1, 0.2 , 0.3, 0.4 ],
 "colsample_bytree" : [ 0.3, 0.4, 0.5 , 0.7, 1.0 ],
 'subsample': [0.6, 0.8, 1.0],
 "n_estimators": [100, 200, 300, 500]
}


rand_search = RandomizedSearchCV(mod_rand, param_distributions=params, n_iter=200, scoring='roc_auc', n_jobs=-1, cv=TimeSeriesSplit(n_splits=10), verbose=3)


dat = dat.drop(columns=['Unnamed: 0.1', 'Unnamed: 0'])

features = dat.drop(columns=['outcome', 'match_id', 'minutes', 'date'])

train_data = dat[dat['year'] < 2023]
test_data = dat[dat['year'] >= 2023]

# train data
x_train = train_data.drop(columns=['outcome', 'match_id', 'minutes', 'date']).select_dtypes(include=[np.number])
y_train = train_data['outcome']

# test data
x_test = test_data.drop(columns=['outcome', 'match_id', 'minutes', 'date']).select_dtypes(include=[np.number])
y_test = test_data['outcome']




rand_search.fit(x_train, y_train)

best_params = rand_search.best_params_

results_df = pd.DataFrame(rand_search.cv_results_)
results_df.to_csv("rand_search_results_3.csv", index=False)