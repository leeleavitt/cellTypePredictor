import pandas as pd
import numpy as np
import py.main
from py.main import featureMaker
import tensorflow as tf
from tensorflow.keras.models import Sequential
from tensorflow.keras.layers import LSTM, Dense, Conv1D, MaxPooling1D, Flatten
print("Num GPUs Available: ", len(tf.config.experimental.list_physical_devices('GPU')))

modelName = 'amck'
traceFileName = "../trainingData/cell_types_AMCK/AMCK_7238.csv"
labelFileName = '../trainingData/cell_types_label.csv'

#traceFileName = "./trainingData/cell_types_R3J/R3J_7238.csv"
# Prepare the traces for the LSTM
traces = pd.read_csv(traceFileName, index_col=0)
tracesIndex = traces.index
tracesIndex = np.random.permutation(len(tracesIndex))
traces = traces.iloc[tracesIndex,]
tracesNp = np.asarray(traces)
tracesFeature = featureMaker(tracesNp, 10)
#tracesFeature = tracesNp[...,np.newaxis]

# Prepare the labels
labels = pd.read_csv(labelFileName, index_col=0)
labels = labels.iloc[tracesIndex,]
labels.iloc[:,0] = labels.iloc[:,0] - 1
labels = labels.iloc[:,0].astype('category')
labels = np.asarray(labels)
uniqueLabels = len(set(labels))

#Create Train and Validation Set
val = int(np.ceil(tracesFeature.shape[0]*.3))
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
# model.add(Conv1D(
#         filters=64, 
#         kernel_size=6, 
#         padding='same', 
#         activation='relu', 
#         input_shape = tracesFeature.shape[1:]))

# model.add(MaxPooling1D(pool_size=2))
model.add(LSTM(
        32,
        # dropout = 0.4,
        # recurrent_dropout=0.4
        input_shape = tracesFeature.shape[1:]
        ))
model.add(Dense(LSTMOUTPUT, activation='softmax'))

model.compile(optimizer='adam',
            loss='sparse_categorical_crossentropy',
            metrics=['acc'])

model.summary()

EVALUATION_INTERVAL = 200
EPOCHS = 100
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

model.save('../models/'+ modelName + '.h5')
