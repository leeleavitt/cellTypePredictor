import pandas as pd
import numpy as np
from py.main import featureMaker
import py
import tensorflow as tf
from tensorflow.keras.models import Sequential
from tensorflow.keras.layers import LSTM, Dense, Conv1D, MaxPooling1D, Flatten
print("Num GPUs Available: ", len(tf.config.experimental.list_physical_devices('GPU')))

# modelName = ['aitc', 'menth', 'caps', 'k40']
# traceFileName = ["../../trainingData/cellTypeData/r3j/traces.csv"]
# labelFileName = ["../../trainingData/cellTypeData/r3j/labels.csv"]

modelName = ['lung']
traceFileName = ["./features.csv"]
#traceFileName = ["../../trainingData/cellTypeData/r3j/traces.csv"]
labelFileName = ["./labels.csv"]
#labelFileName = ["../../trainingData/cellTypeData/r3j/labels.csv"]
# Prepare the traces for the LSTM

for i in range(len(modelName)):
        traces = pd.read_csv(traceFileName[i], index_col=0)
        tracesIndex = traces.index
        tracesIndex = np.random.permutation(len(tracesIndex))
        traces = traces.iloc[tracesIndex,]
        tracesNp = np.asarray(traces)
        tracesFeature = featureMaker(tracesNp, 20)
        #tracesFeature = tracesNp[...,np.newaxis]

        # Prepare the labels
        labels = pd.read_csv(labelFileName[i], index_col=0)
        labels = labels.iloc[tracesIndex,]
        labels.iloc[:,0] = labels.iloc[:,0]
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
        BATCH_SIZE = 100
        BUFFER_SIZE = 1000
        LSTMINPUT = x_train.shape[1]
        LSTMOUTPUT = uniqueLabels
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
        es_callback = tf.keras.callbacks.EarlyStopping(monitor='val_loss', patience=5)

        history = model.fit(train, epochs = EPOCHS, 
                        steps_per_epoch=EVALUATION_INTERVAL,
                        validation_data = test,
                        validation_steps=50,
                        callbacks=[es_callback])


        # history = model.fit(x_train, y_train, epochs=25, 
        #                     validation_data=(x_test,y_test ))

        print("This is the loss vs Accuracy for" + '.')
        py.main.plot_train_history(history, modelName[i])     

        model.save('./'+ modelName[i] + '.h5')
