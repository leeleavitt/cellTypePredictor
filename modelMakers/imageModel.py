import pandas as pd
import numpy as np
import py.main
from py.main import imageLoader
import tensorflow as tf
from scipy.stats import describe
from tensorflow.keras.models import Sequential
from tensorflow.keras.layers import Dense, Activation, Dropout, Flatten, Conv2D, MaxPooling2D
from tensorflow.keras.layers import BatchNormalization
from tensorflow.keras.regularizers import l1

print("Num GPUs Available: ", len(tf.config.experimental.list_physical_devices('GPU')))

modelName = 'label'
imageFileName1 = "../trainingData/cell_types_img1_123/7238_41_41_3_cell_types.npy"
labelFileName1 = '../trainingData/cell_types_label.csv'


#imageFileName2 = "../trainingData/binaryData_gfp.bin_cy5.bin_img2_123/8127_41_41_3_gfp.bin_cy5.bin.npy"
#labelFileName2 = "../trainingData/binaryData_gfp.bin_cy5.bin_img2_123/gfp.bin_cy5.bin_8127_label.csv"


#traceFileName = "./trainingData/cell_types_R3J/R3J_7238.csv"
# Prepare the image for the LSTM
image1 = imageLoader(imageFileName1)
imageIndex1 = image1.shape[0]
imageIndex1 = np.random.permutation(range(imageIndex1))
imageFeature = image1[imageIndex1,...]

# image2 = imageLoader(imageFileName2)
# imageIndex2 = image2.shape[0]
# imageIndex2 = np.random.permutation(range(imageIndex2))
# imageFeature2 = image2[imageIndex2,...]

#imageFeature = np.concatenate((imageFeature1, imageFeature2), axis = 0)

# Prepare the labels
labels = pd.read_csv(labelFileName1, index_col=0)
labels = labels.iloc[imageIndex1,]
labels.iloc[:,0] = labels.iloc[:,0] - 1
labels = labels.iloc[:,0].astype('category')
labels = np.asarray(labels, dtype = 'uint8')

# labels = pd.read_csv(labelFileName2, index_col=0)
# labels = labels.iloc[imageIndex2,]
# labels.iloc[:,0] = labels.iloc[:,0] - 1
# labels = labels.iloc[:,0].astype('category')
# labels2 = np.asarray(labels)

#labels = np.concatenate((labels1, labels2), axis = 0)

uniqueLabels = len(np.unique(labels))
denseSize = imageFeature[1,...].flatten().shape[0]


#Create Train and Validation Set
val = int(np.ceil(imageFeature.shape[0]*.2))
trainSize = imageFeature.shape[0] - val 

x_train  = imageFeature[:trainSize,...]
y_train = labels[:trainSize]

x_test = imageFeature[trainSize:,...]
y_test = labels[trainSize:]

# Now DO what we need for the import to LSTM
BATCH_SIZE = 200
BUFFER_SIZE = 20000
LSTMINPUT = x_train.shape[1]
train = tf.data.Dataset.from_tensor_slices((x_train, y_train))
train = train.shuffle(BUFFER_SIZE).batch(BATCH_SIZE).repeat()

test = tf.data.Dataset.from_tensor_slices((x_test, y_test))
test = test.batch(BATCH_SIZE).repeat()

# model = Sequential()
# model.add(Conv2D(
#     input_shape = imageFeature.shape[1:],
#     filters = 32,
#     kernel_size = (3,3),
#     padding='valid',
#     activity_regularizer=l1(0.0001)
# ))
# model.add(Activation('relu'))
# model.add(MaxPooling2D(
#     pool_size = (2,2), 
#     strides = (2,2),
#     padding = 'valid'
# ))
# model.add(Conv2D(64, (3, 3), activation='relu', activity_regularizer=l1(0.0001)))
# model.add(MaxPooling2D(pool_size=(2, 2)))
# model.add(Dropout(0.4))
# model.add(Flatten())
# model.add(Dense(128,activity_regularizer=l1(0.0001)))
# model.add(Activation('relu'))
# model.add(Dropout(0.4))
# model.add(BatchNormalization())
# model.add(Dense(len(uniqueLabels)))
# model.add(Activation('softmax'))

model = Sequential()
model.add(Conv2D(
    input_shape = imageFeature.shape[1:],
    filters = 8,
    kernel_size = (3,3),
    padding='valid',
    # activation = 'relu',
    activity_regularizer=l1(0.0001)
))
model.add(MaxPooling2D(
    pool_size = (2,2), 
    strides = 2,
    padding = 'valid'
))
model.add(Dropout(0.4))

model.add(
    Conv2D(16, (3, 3), 
    activation='relu', 
    activity_regularizer=l1(0.0001)
))
model.add(MaxPooling2D(pool_size=(2, 2)))
model.add(Dropout(0.4))

model.add(
    Conv2D(32, (3, 3), 
    activation='relu', 
    activity_regularizer=l1(0.0001)
))
model.add(MaxPooling2D(pool_size=(2, 2)))
model.add(Dropout(0.4))

model.add(Flatten())
model.add(Dense(64, activity_regularizer=l1(0.0001), activation = 'relu'))
model.add(Dropout(0.4))

# model.add(Dense(228, activity_regularizer=l1(0.0001), activation = 'relu'))
# model.add(Dropout(0.4))

model.add(Dense(uniqueLabels, activation = 'softmax'))

model.summary()

model.compile(optimizer='adam',
            loss='sparse_categorical_crossentropy',
            metrics=['acc'])

EVALUATION_INTERVAL = 400
EPOCHS = 200

es_callback = tf.keras.callbacks.EarlyStopping(
    monitor='val_loss', 
    patience=5
)

history = model.fit(train, epochs = EPOCHS, 
                    steps_per_epoch=EVALUATION_INTERVAL,
                    validation_data = test,
                    validation_steps=50,
                    callbacks=[es_callback])

# history = model.fit(x_train, y_train, epochs=25, 
#                     validation_data=(x_test,y_test ))

print("This is the loss vs Accuracy for" + './history/')
py.main.plot_train_history(history, modelName)
model.save('../models/'+modelName+'.h5')



# model = Sequential()
# model.add(Conv2D(32, kernel_size=(3, 3),
#                  activation='relu',
#                  input_shape=imageFeature.shape[1:],
#                  activity_regularizer=l1(0.0001)))
# model.add(Conv2D(64, (3, 3), activation='relu'))
# model.add(MaxPooling2D(pool_size=(2, 2)))
# model.add(Dropout(0.5))
# model.add(Flatten())
# model.add(Dense(128*4, activation='relu'))
# model.add(Dropout(0.8))
# model.add(Dense(128*2, activation='relu'))
# model.add(Dropout(0.8))
# model.add(Dense(256, activation='relu', activity_regularizer=l1(0.0001)))
# model.add(Dropout(0.5))
# model.add(Dense(128, activation='relu', activity_regularizer=l1(0.0001)))
# model.add(Dropout(0.5))
# model.add(Dense(128, activation='relu', activity_regularizer=l1(0.0001)))
# model.add(Dense(len(uniqueLabels)))