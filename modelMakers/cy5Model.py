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
import matplotlib.pyplot as plt
from skimage import transform
import random

print("Num GPUs Available: ", len(tf.config.experimental.list_physical_devices('GPU')))

modelName = 'cy5_2'
imageFileName = "../trainingData/cy5.bin_img3_1/19076_41_41_1_cy5.bin.npy"
labelFileName = "../trainingData/cy5.bin_img3_1/cy5.bin_19076_label.csv"

#traceFileName = "./trainingData/cell_types_R3J/R3J_7238.csv"
# Prepare the image for the LSTM
image = imageLoader(imageFileName)
imageIndex = image.shape[0]
imageIndex = np.random.permutation(range(imageIndex))
imageFeature = image[imageIndex,...]

# Prepare the labels
labels = pd.read_csv(labelFileName, index_col=0)
labels = labels.iloc[imageIndex,]
#labels.iloc[:,0] = labels.iloc[:,0] - 1
#labels = labels.iloc[:,0].astype('category')
labels = np.asarray(labels, dtype='uint8')

np.asmatrix(np.unique(labels, return_counts=True))

uniqueLabels = np.unique(labels)

#Create Train and Validation Set
val = int(np.ceil(imageFeature.shape[0]*.3))
trainSize = imageFeature.shape[0] - val 

x_train  = imageFeature[:trainSize,...]
y_train = np.squeeze(labels[:trainSize])

# Augment the training set to buff up the samples of 1's
oneIndexs = np.argwhere(y_train == 1).flatten()
xTrainOnes = x_train[oneIndexs]

xTrainOnesFlipud = np.empty(xTrainOnes.shape)
for i in range(xTrainOnes.shape[0]):
    xTrainOnesFlipud[i] = np.flipud(xTrainOnes[i])

xTrainOnesFliplr = np.empty(xTrainOnes.shape)
for i in range(xTrainOnes.shape[0]):
    xTrainOnesFliplr[i] = np.fliplr(xTrainOnes[i])

xTrainOnesRot1 = np.empty(xTrainOnes.shape)
randRot = random.uniform(-20,20)
for i in range(xTrainOnes.shape[0]):
    xTrainOnesRot1[i] = transform.rotate(xTrainOnes[i], randRot)

# xTrainOnesRot2 = np.empty(xTrainOnes.shape)
# randRot = random.uniform(-20,20)
# for i in range(xTrainOnes.shape[0]):
#     xTrainOnesRot2[i] = transform.rotate(xTrainOnes[i], randRot)

# xTrainOnesRot3 = np.empty(xTrainOnes.shape)
# randRot = random.uniform(-20,20)
# for i in range(xTrainOnes.shape[0]):
#     xTrainOnesRot3[i] = transform.rotate(xTrainOnes[i], randRot)

# xTrainOnesRot4 = np.empty(xTrainOnes.shape)
# randRot = random.uniform(-20,20)
# for i in range(xTrainOnes.shape[0]):
#     xTrainOnesRot4[i] = transform.rotate(xTrainOnes[i], randRot)

fig, ((ax1, ax2), (ax3, ax4)) = plt.subplots(2,2)
fig.suptitle("Image Augmentations")

ax1.imshow(np.squeeze(xTrainOnes[50]))
ax1.set_title("Original")

ax2.imshow(np.squeeze(xTrainOnesFliplr[50]))
ax2.set_title('horizontal Flip')

ax3.imshow(np.squeeze(xTrainOnesFlipud[50]))
ax3.set_title('Vertical Flip')

ax4.imshow(np.squeeze(xTrainOnesRot1[50]))
ax4.set_title('Rotate on axis')

moreXtrainOnes = np.concatenate((
    xTrainOnes, 
    xTrainOnesFliplr, 
    xTrainOnesFlipud,
    xTrainOnesRot1))
    # xTrainOnesRot2,
    # xTrainOnesRot3,
    # xTrainOnesRot4))

moreYTrain = np.repeat(1, moreXtrainOnes.shape[0])

x_train = np.concatenate((x_train,moreXtrainOnes))
y_train = np.concatenate((y_train, moreYTrain))

randTrain = np.random.permutation(range(x_train.shape[0]))
x_train = x_train[randTrain]
y_train = y_train[randTrain]

x_test = imageFeature[trainSize:,...]
y_test = labels[trainSize:]

# Now DO what we need for the import to LSTM
BATCH_SIZE = 200
BUFFER_SIZE = 10000
LSTMINPUT = x_train.shape[1]
LSTMOUTPUT = len(uniqueLabels)
train = tf.data.Dataset.from_tensor_slices((x_train, y_train))
train = train.shuffle(BUFFER_SIZE).batch(BATCH_SIZE).repeat()

test = tf.data.Dataset.from_tensor_slices((x_test, y_test))
test = test.batch(BATCH_SIZE).repeat()

model = Sequential()
model.add(Conv2D(
    input_shape = imageFeature.shape[1:],
    filters = 8,
    kernel_size = (3,3),
    padding='valid',
    # activation = 'relu',
    activity_regularizer=l1(0.0001)
))
# model.add(MaxPooling2D(
#     pool_size = (2,2), 
#     strides = 2,
#     padding = 'valid'
# ))
model.add(Dropout(0.4))

model.add(
    Conv2D(16, (3, 3), 
    activation='relu', 
    activity_regularizer=l1(0.0001)
))
# model.add(MaxPooling2D(pool_size=(2, 2)))
model.add(Dropout(0.4))

model.add(
    Conv2D(32, (3, 3), 
    activation='relu', 
    activity_regularizer=l1(0.0001)
))
# model.add(MaxPooling2D(pool_size=(2, 2)))
model.add(Dropout(0.4))

model.add(Flatten())

model.add(Dense(200, activity_regularizer=l1(0.0001), activation = 'relu'))
model.add(Dropout(0.4))

model.add(Dense(64, activity_regularizer=l1(0.0001), activation = 'relu'))
model.add(Dropout(0.4))

model.add(Dense(len(np.unique(y_train)), activation = 'softmax'))

model.summary()

model.compile(optimizer='adam',
            loss='sparse_categorical_crossentropy',
            metrics=['acc'])

EVALUATION_INTERVAL = 300
EPOCHS = 200

es_callback = tf.keras.callbacks.EarlyStopping(monitor='val_loss', patience=5)

history = model.fit(train, epochs = EPOCHS, 
                    steps_per_epoch=EVALUATION_INTERVAL,
                    validation_data = test,
                    validation_steps=150,
                    callbacks=[es_callback])

# history = model.fit(x_train, y_train, epochs=25, 
#                     validation_data=(x_test,y_test ))

print("This is the loss vs Accuracy for" + '.')
py.main.plot_train_history(history, modelName)     

model.save('../models/' + modelName + '.h5')

#probability_model = tf.keras.Sequential([model, tf.keras.layers.Softmax()])

testImgProbs = model.predict(x_test)
# Plot the first X test images, their predicted labels, and the true labels.
# Color correct predictions in blue and incorrect predictions in red.
num_rows = 20
num_cols = 3
num_images = num_rows*num_cols
plt.figure(figsize=(2*2*num_cols, 2*num_rows))
for i in range(num_images):
  plt.subplot(num_rows, 2*num_cols, 2*i+1)
  py.main.plot_image(i, testImgProbs[i], y_test, np.squeeze(x_test))
  plt.subplot(num_rows, 2*num_cols, 2*i+2)
  py.main.plot_value_array(i, testImgProbs[i], y_test)
plt.tight_layout()
plt.show()
