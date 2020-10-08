import pandas as pd
import numpy as np
from py.main import featureMaker
from python_pharmer import featureMaker2
import py
import tensorflow as tf
from tensorflow.keras.models import Sequential
from tensorflow.keras.layers import LSTM, Dense, Conv1D, MaxPooling1D, Flatten
print("Num GPUs Available: ", len(tf.config.experimental.list_physical_devices('GPU')))

featureWindows = 12
modelName = 'kdr'
traceFileName = "./features.csv"
labelFileName = "./labels.csv"

# Prepare the traces for the LSTM
traces = pd.read_csv(traceFileName, index_col=0)
tracesIndex = np.random.permutation(len(traces.index))
traces = traces.iloc[tracesIndex,]
tracesNp = np.asarray(traces)
tracesFeature = featureMaker2(tracesNp, featureWindows)

# Prepare the labels
labels = pd.read_csv(labelFileName, index_col=0)
labels = labels.iloc[tracesIndex,]
labels = labels.iloc[:,0].astype('category')
labelsReal = np.asarray(labels)

# Create Train and Validation Set
val = int(np.ceil(tracesFeature.shape[0]*.3))
trainSize = tracesFeature.shape[0] - val 

x_train  = tracesFeature[:trainSize,...]
y_train = labelsReal[:trainSize]

x_test = tracesFeature[trainSize:,...]
y_test = labelsReal[trainSize:]

# ## Augmented data
# # Add the augmented data to the train data
# # Load the augmented data
# augTraceFileName = "./augfeatures.csv"
# augLabelFileName = "./auglabels.csv"

# # Load augmented data
# traces = pd.read_csv(augTraceFileName, index_col=0)
# tracesIndex = np.random.permutation(len(traces.index))
# traces = traces.iloc[tracesIndex,]
# tracesNp = np.asarray(traces)
# augTracesFeature = featureMaker2(tracesNp, featureWindows)

# # Prepare the labels
# labels = pd.read_csv(augLabelFileName, index_col=0)
# labels = labels.iloc[tracesIndex,]
# labels = labels.iloc[:,0].astype('category')
# augLabels = np.asarray(labels)

# # now add the new data
# x_train  = np.concatenate((augTracesFeature, x_train))
# y_train = np.concatenate((augLabels, y_train))

# # mix it up
# randOrder = np.random.permutation(x_train.shape[0])
# x_train = x_train[randOrder,...]
# y_train = y_train[randOrder,...]


# Now DO what we need for the import to LSTM
BATCH_SIZE = 100
BUFFER_SIZE = 1000
LSTMINPUT = x_train.shape[1]
LSTMOUTPUT = len(set(y_train))
train = tf.data.Dataset.from_tensor_slices((x_train, y_train))
train = train.shuffle(BUFFER_SIZE).batch(BATCH_SIZE).repeat()

test = tf.data.Dataset.from_tensor_slices((x_test, y_test))
test = test.batch(BATCH_SIZE).repeat()

model = Sequential()
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
es_callback = tf.keras.callbacks.EarlyStopping(monitor='val_loss', patience=10)

history = model.fit(train, epochs = EPOCHS, 
                steps_per_epoch=EVALUATION_INTERVAL,
                validation_data = test,
                validation_steps=50,
                callbacks=[es_callback])


print("This is the loss vs Accuracy for" + '.')
py.main.plot_train_history(history, modelName)     

model.save('./'+ modelName + '.h5')
