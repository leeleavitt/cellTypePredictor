import pandas as pd
import os
import numpy as np
import py.main
from py.main import featureMaker
import tensorflow as tf
from tensorflow.keras.models import Sequential
from tensorflow.keras.layers import Dense
from tensorflow.keras.layers import LSTM
print("Num GPUs Available: ", len(tf.config.experimental.list_physical_devices('GPU')))

modelName = 'R3J'
traceFileName = "../trainingData/cell_types_R3J/R3J_7238.csv"
#traceFileName2 = "../trainingData/cell_types_AMCK/AMCK_7238.csv"

labelFileName = "../trainingData/cell_types_label.csv"

# Prepare the traces for the LSTM
traces = pd.read_csv(traceFileName, index_col=0)
traces = np.asarray(traces)

tracesIndex = traces.shape[0]
tracesIndex = np.random.permutation(range(tracesIndex))
tracesNp = traces[tracesIndex,...]

tracesFeature = featureMaker(tracesNp, 10)
print("Shape of feature is :")
print(tracesFeature.shape)
#tracesFeature = tracesNp[..., np.newaxis]

# Prepare the labels
labels = pd.read_csv(labelFileName, index_col=0)
labels = labels.iloc[tracesIndex,]
labels.iloc[:,0] = labels.iloc[:,0] - 1
labels = labels.iloc[:,0].astype('category')
labels = np.asarray(labels)
uniqueLabels = len(set(labels))

#Create Train and Validation Set
val = int(np.ceil(tracesFeature.shape[0]*.2))
trainSize = tracesFeature.shape[0] - val 

x_train  = tracesFeature[:trainSize,...]
y_train = labels[:trainSize]

x_test = tracesFeature[trainSize:,...]
y_test = labels[trainSize:]

# Now DO what we need for the import to LSTM
BATCH_SIZE = 200
BUFFER_SIZE = 2000
LSTMINPUT = x_train.shape[1]
LSTMOUTPUT = uniqueLabels
train = tf.data.Dataset.from_tensor_slices((x_train, y_train))
train = train.shuffle(BUFFER_SIZE).batch(BATCH_SIZE).repeat()

test = tf.data.Dataset.from_tensor_slices((x_test, y_test))
test = test.batch(BATCH_SIZE).repeat()

model = Sequential()
model.add(tf.keras.layers.Conv1D(
    filters=10, 
    kernel_size=5, 
    padding='same', 
    activation='relu', 
    input_shape = tracesFeature.shape[1:]
))
model.add(tf.keras.layers.MaxPooling1D(pool_size=2))

model.add(tf.keras.layers.Conv1D(
    filters=36, 
    kernel_size=3, 
    padding='same', 
    activation='relu', 
))
model.add(tf.keras.layers.MaxPooling1D(pool_size=2))

model.add(tf.keras.layers.LSTM( 
    LSTMOUTPUT, 
    dropout = 0.2,
    recurrent_dropout=0.2,
))
model.add(tf.keras.layers.Dense(LSTMOUTPUT, activation='softmax'))


model.compile(optimizer='adam',
            loss='sparse_categorical_crossentropy',
            metrics=['acc'])


model.summary()

EVALUATION_INTERVAL = 400
EPOCHS = 200

es_callback = tf.keras.callbacks.EarlyStopping(monitor='val_loss', patience=5)
history = model.fit(train, epochs = EPOCHS, 
                    steps_per_epoch=EVALUATION_INTERVAL,
                    validation_data = test,
                    validation_steps=50,
                    callbacks=[es_callback])

# history = model.fit(x_train, y_train, epochs=25, 
#                     validation_data=(x_test,y_test ))

print("This is the loss vs Accuracy for" + '.')
py.main.plot_train_history(history, modelName)     

model.save('../models/'+modelName+'.h5')


import importlib
import matplotlib.pyplot as plt
importlib.reload(py.main)

num_rows = 30
num_cols = 3
num_images = num_rows*num_cols
testTraceProbs = model.predict(x_test)
traceToView = traces[trainSize:,...]

np.arange(len(traces)).shape

plt.figure(figsize=(2*2*num_cols, 2*num_rows))
for i in range(num_images):
  plt.subplot(num_rows, 2*num_cols, 2*i+1)
  py.main.plot_trace(i, testTraceProbs[i], y_test, traceToView)
  plt.subplot(num_rows, 2*num_cols, 2*i+2)
  py.main.plot_value_array(i, testTraceProbs[i], y_test)
plt.tight_layout()
plt.show()
