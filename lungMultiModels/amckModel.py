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
modelName = 'lungMulti'
traceFileName = "./features.csv"
labelFileName = "./labels.csv"

# Prepare the traces for the LSTM
traces = pd.read_csv(traceFileName, index_col=0)
tracesIndex = np.random.permutation(len(traces.index))
traces = traces.iloc[tracesIndex,]
tracesNp = np.asarray(traces)
features = featureMaker2(tracesNp, featureWindows)

# Prepare the labels
labels = pd.read_csv(labelFileName, index_col=0)
labels = labels.iloc[tracesIndex,0].astype('category')
labels = np.asarray(labels)

# Create Train and Validation Set

# The validation set should be balanced, so this is how i accomplish it

# What is the smallest label?
labelSumm = np.unique(labels, return_counts=True)
smallLabLogic = labelSumm[1] == np.min(labelSumm[1])
smallLab = labelSumm[0][smallLabLogic][0]
smallLabAmount = labelSumm[1][smallLabLogic][0]
testAmount = int(np.ceil(smallLabAmount * 0.3))

smallLogic = labels == smallLab
largeLogic = labels != smallLab

# TestData
testSmallLabels = labels[smallLogic][:testAmount]
testSmallFeatures = features[smallLogic][:testAmount,...]
testLargeLabels = labels[largeLogic][:testAmount]
testLargeFeatures = features[largeLogic][:testAmount,...]

testFeatures = np.concatenate((testSmallFeatures, testLargeFeatures))
testLabels = np.concatenate((testSmallLabels, testLargeLabels))
testShuff = np.random.permutation(testFeatures.shape[0])

testFeatures = testFeatures[testShuff, ...]
testLabels = testLabels[testShuff, ...]

#TrainData
trainSmallLabels = labels[smallLogic][(testAmount+1):]
trainSmallFeatures = features[smallLogic][(testAmount+1):, ...]

trainLargeLabels = labels[largeLogic][(testAmount+1):]
trainLargeFeatures = features[largeLogic][(testAmount+1):, ...]

# Undersample the large labels
trainLargeShrink = int(trainLargeLabels.shape[0] * .5)
trainLargeLabels = trainLargeLabels[:trainLargeShrink]
trainLargeFeatures = trainLargeFeatures[:trainLargeShrink]

# Now i will oversample the data for the smaller logic
#Oversample the small features
trainFeatures = np.concatenate((trainSmallFeatures, trainSmallFeatures, trainLargeFeatures))
trainLabels = np.concatenate((trainSmallLabels, trainSmallLabels, trainLargeLabels))
trainShuff = np.random.permutation(trainFeatures.shape[0])

trainFeatures = trainFeatures[trainShuff, ...]
trainLabels = trainLabels[trainShuff, ...]


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
train = tf.data.Dataset.from_tensor_slices((trainFeatures, trainLabels))
train = train.shuffle(BUFFER_SIZE).batch(BATCH_SIZE).repeat()

test = tf.data.Dataset.from_tensor_slices((testFeatures, testLabels))
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
es_callback = tf.keras.callbacks.EarlyStopping(monitor='val_loss', patience=30)

history = model.fit(train, epochs = EPOCHS, 
                steps_per_epoch=EVALUATION_INTERVAL,
                validation_data = test,
                validation_steps=50,
                callbacks=[es_callback])


print("This is the loss vs Accuracy for" + '.')
py.main.plot_train_history(history, modelName)     

model.save('./'+ modelName + '.h5')
