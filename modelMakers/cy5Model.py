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

print("Num GPUs Available: ", len(tf.config.experimental.list_physical_devices('GPU')))

modelName = 'cy5'
imageFileName = "../trainingData/cy5.bin_img3_1/13227_41_41_1_cy5.bin.npy"
labelFileName = "../trainingData/cy5.bin_img3_1/cy5.bin_13227_label.csv"

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
labels = labels.iloc[:,0].astype('category')
labels = np.asarray(labels, dtype='uint8')

uniqueLabels = np.unique(labels)

#Create Train and Validation Set
val = int(np.ceil(imageFeature.shape[0]*.2))
trainSize = imageFeature.shape[0] - val 

x_train  = imageFeature[:trainSize,...]
y_train = labels[:trainSize]

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
    filters = 32,
    kernel_size = (3,3),
    padding='valid',
))
model.add(Activation('relu'))
model.add(MaxPooling2D(
    pool_size = (2,2), 
    strides = (2,2),
    padding = 'valid'
))
model.add(Conv2D(64, (3, 3), activation='relu', activity_regularizer=l1(0.0001)))
model.add(MaxPooling2D(pool_size=(2, 2)))
model.add(Dropout(0.4))
model.add(Flatten())
model.add(Dense(128,activity_regularizer=l1(0.0001)))
model.add(Activation('relu'))
model.add(Dropout(0.4))
#model.add(BatchNormalization())
model.add(Dense(2, activation = 'softmax'))

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
                    validation_steps=50,
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
  py.main.plot_image(i, testImgProbs[i], y_test, x_test)
  plt.subplot(num_rows, 2*num_cols, 2*i+2)
  py.main.plot_value_array(i, testImgProbs[i], y_test)
plt.tight_layout()
plt.show()
