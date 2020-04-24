import pandas as pd
import numpy as np
import py.main
from py.main import featureMaker
import tensorflow as tf
from tensorflow.keras.models import Sequential
from tensorflow.keras.layers import Dense
from tensorflow.keras.layers import LSTM
print("Num GPUs Available: ", len(tf.config.experimental.list_physical_devices('GPU')))

traceFileName = "./trainingData/cell_types_AMCK/AMCK_7238.csv"
#traceFileName = "./trainingData/cell_types_R3J/R3J_7238.csv"
# Prepare the traces for the LSTM
traces = pd.read_csv(traceFileName, index_col=0)
tracesIndex = traces.index
tracesIndex = tracesIndex[np.random.permutation(len(tracesIndex))]
traces = traces.loc[tracesIndex,]
tracesNp = np.asarray(traces)
tracesFeature = featureMaker(tracesNp, 5)

# Prepare the labels
labelFileName = './trainingData/cell_types_AMCK/AMCK_7238_label.csv'
labels = pd.read_csv(labelFileName, index_col=0)
labels = labels.loc[tracesIndex,]
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

model = Sequential([
    tf.keras.layers.LSTM(
        LSTMINPUT, 
        input_shape = tracesFeature.shape[-2:], 
        dropout = 0.2
    ),
    tf.keras.layers.Dense(LSTMOUTPUT, activation='sigmoid')
])

model.compile(optimizer='adam',
            loss='sparse_categorical_crossentropy',
            metrics=['acc'])

model.summary()

EVALUATION_INTERVAL = 500
EPOCHS = 60

history = model.fit(train, epochs = EPOCHS, 
                    steps_per_epoch=EVALUATION_INTERVAL,
                    validation_data = test,
                    validation_steps=50)

# history = model.fit(x_train, y_train, epochs=25, 
#                     validation_data=(x_test,y_test ))

print("This is the loss vs Accuracy for" + '.')
py.main.plot_train_history(history, '.'+"_lstm.experiment2")     

x_test.shape
x_test[1,:,1]

test
model.predict(x_train).shape
