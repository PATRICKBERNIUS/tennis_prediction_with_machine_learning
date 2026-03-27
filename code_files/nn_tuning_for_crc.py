import pandas as pd
import numpy as np
from sklearn.preprocessing import StandardScaler
from sklearn.impute import SimpleImputer
import tensorflow as tf
from tensorflow import keras
from keras.models import Sequential # Model building
from keras.layers import *


dat = pd.read_csv("it_would_just_be_so_awesome_if_this_worked.csv")

dat = dat.sort_values(["date", "round_order"])
dat = dat.drop(columns=['Unnamed: 0.1', 'Unnamed: 0'])

features = dat.drop(columns=['outcome', 'match_id', 'minutes'])

train_data = dat['year'] < 2022
val_data = dat['year'] == 2022
test_data = dat['year'] >= 2023

x_train = features[train_data].select_dtypes(include=[np.number])
y_train = dat.loc[train_data, 'outcome']

x_val = features[val_data].select_dtypes(include=[np.number])
y_val = dat.loc[val_data, 'outcome']

x_test = features[test_data].select_dtypes(include=[np.number])
y_test = dat.loc[test_data, 'outcome']

#NN needs scaled data
scaler = StandardScaler()

#mean imputation to handle null values
imputer = SimpleImputer(strategy='mean')
x_train_imputed = imputer.fit_transform(x_train)
x_val_imputed = imputer.transform(x_val)
x_test_imputed = imputer.transform(x_test)


scaler = StandardScaler()
x_train_scaled = scaler.fit_transform(x_train_imputed)
x_val_scaled = scaler.transform(x_val_imputed)
x_test_scaled = scaler.transform(x_test_imputed)




def model_nn(firstLayerFilters, numLayers, dropoutValue):
    # Create model object
    model = Sequential()

    current_filters = firstLayerFilters

    for i in range(numLayers):
        if i == 0:

            # Add the first layer with dropout
            model.add(Dense(current_filters, activation='relu',
                            input_shape=(x_train_scaled.shape[1],)))
        else:
            model.add(Dense(current_filters, activation='relu'))

        model.add(BatchNormalization())
        model.add(Dropout(dropoutValue))

        current_filters = max(32, current_filters // 2)

    model.add(Dense(1, activation='sigmoid'))


    model.compile(loss='binary_crossentropy',
                  optimizer=keras.optimizers.Adam(learning_rate=0.001),
                  metrics=['AUC'])

    # Return model
    return model




search_space = {
    'num_filters': [64, 128, 256, 512, 1024],
    'num_layers': [2, 3, 4, 5, 6],
    'dropout_val': [0.0, 0.1, 0.2, 0.3, 0.4, 0.5],
    'batch_size': [128, 256, 512, 1024]
}




results = []
num_trials = 200

for run in range(num_trials):

    params = {
        'num_filters': int(np.random.choice(search_space['num_filters'])),
        'num_layers': int(np.random.choice(search_space['num_layers'])),
        'dropout_val': float(np.random.choice(search_space['dropout_val'])),
        'batch_size': int(np.random.choice(search_space['batch_size']))
    }

    model = model_nn(
        params['num_filters'],
        params['num_layers'],
        params['dropout_val']
    )


    shist = model.fit(
        x_train_scaled, y_train,
        epochs=50,
        batch_size=params['batch_size'],
        validation_data=(x_val_scaled, y_val),
        callbacks=[
            keras.callbacks.EarlyStopping(monitor='val_auc', patience=5, restore_best_weights=True, mode='max'), #stops training if no improvement after 5 epochs
            keras.callbacks.ReduceLROnPlateau(monitor='val_loss', factor=0.5, patience=3) #reduce learning rate if no improvement after 3 epochs
        ],
        verbose=0
    )

    val_auc = max(shist.history['val_auc'])


    #store results
    results.append({
        **params,
        'val_auc': val_auc,
        'epochs_trained' : len(shist.history['loss'])
    })

results_df = pd.DataFrame(results)
results_df = results_df.sort_values('val_auc', ascending=False)
results_df.to_cvs("nn_tuned_results.csv", index=False)
