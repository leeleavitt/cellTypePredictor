import pandas as pd
import numpy as np
import py.main
from py.main import imageLoader
import tensorflow as tf
from tensorflow.keras.models import Sequential
from tensorflow.keras.layers import Dense, Activation, Dropout, Flatten, Conv2D, MaxPooling2D
from tensorflow.keras.layers import BatchNormalization

print("Num GPUs Available: ", len(tf.config.experimental.list_physical_devices('GPU')))

imageFileName = "../trainingData/cell_types_img2_123/7238_41_41_3_cell_types.npy"
labelFileName = "../trainingData/gfp.bin_cy5.bin_label.csv"

#traceFileName = "./trainingData/cell_types_R3J/R3J_7238.csv"
# Prepare the image for the LSTM
image = imageLoader(imageFileName)
imageIndex = image.shape[0]
imageIndex = np.random.permutation(range(imageIndex))
imageFeature = image[imageIndex,...]

# Prepare the labels
labels = pd.read_csv(labelFileName, index_col=0)
labels.shape
labels = labels.iloc[imageIndex,]
labels.iloc[:,0] = labels.iloc[:,0] - 1
labels = labels.iloc[:,0].astype('category')
labels = np.asarray(labels)
uniqueLabels = set(labels)

#Create Train and Validation Set
val = int(np.ceil(imageFeature.shape[0]*.4))
trainSize = imageFeature.shape[0] - val 

x_train  = imageFeature[:trainSize,...]
y_train = labels[:trainSize]

x_test = imageFeature[trainSize:,...]
y_test = labels[trainSize:]

# Now DO what we need for the import to LSTM
BATCH_SIZE = 200
BUFFER_SIZE = 2000
LSTMINPUT = x_train.shape[1]
LSTMOUTPUT = len(uniqueLabels)
train = tf.data.Dataset.from_tensor_slices((x_train, y_train))
train = train.shuffle(BUFFER_SIZE).batch(BATCH_SIZE).repeat()

test = tf.data.Dataset.from_tensor_slices((x_test, y_test))
test = test.batch(BATCH_SIZE).repeat()

# model = Sequential()
#     input_shape = imageFeature.shape[1:],
#     filters = 32,
#     kernel_size = (3,3),
#     padding='valid'
# ))
# model.add(Activation('relu'))
# model.add(MaxPooling2D(
#     pool_size = (2,2), 
#     strides = (2,2),
#     padding = 'valid'
# ))
# model.add(Conv2D(64, (3, 3), activation='relu'))
# model.add(MaxPooling2D(pool_size=(2, 2)))
# model.add(Dropout(0.25))
# model.add(Dropout(0.25))
# model.add(Flatten())
# model.add(Dense(128))
# model.add(Activation('relu'))
# model.add(Dropout(0.4))
# model.add(BatchNormalization())

# model.add(Dense(len(uniqueLabels)))
# model.add(Activation('softmax'))

model = Sequential()
# model.add(Conv2D(32, kernel_size=(3, 3),
#                  activation='relu',
#                  input_shape=imageFeature.shape[1:]))
# model.add(Conv2D(64, (3, 3), activation='relu'))
# model.add(MaxPooling2D(pool_size=(2, 2)))
# model.add(Dropout(0.25))
model.add(Flatten(input_shape=imageFeature.shape[1:]))
model.add(Dense(128*4, activation='relu'))
model.add(Dropout(0.5))
model.add(Dense(128*2, activation='relu'))
model.add(Dropout(0.5))
model.add(Dense(128, activation='relu'))
model.add(Dropout(0.5))
model.add(Dense(len(uniqueLabels), activation='softmax'))
model.summary()


model.compile(optimizer='adam',
            loss='sparse_categorical_crossentropy',
            metrics=['acc'])


EVALUATION_INTERVAL = 400
EPOCHS = 10

history = model.fit(train, epochs = EPOCHS, 
                    steps_per_epoch=EVALUATION_INTERVAL,
                    validation_data = test,
                    validation_steps=50)

# history = model.fit(x_train, y_train, epochs=25, 
#                     validation_data=(x_test,y_test ))

print("This is the loss vs Accuracy for" + '.')
py.main.plot_train_history(history, '.'+"_lstm.experiment2")     

model.save('../models/'+'label.h5')
