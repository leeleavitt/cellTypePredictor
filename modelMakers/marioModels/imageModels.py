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

modelName = ['gfp', 'ib4', 'image']

imageFileName = [
"../../trainingData/cellTypeData/cell_types_img4_2/13465_41_41_1_cell_types.npy",
"../../trainingData/cellTypeData/cell_types_img3_1/13465_41_41_1_cell_types.npy", 
"../../trainingData/cellTypeData/cell_types_img1_123/13465_41_41_3_cell_types.npy"]

labelFileName = [
"../../trainingData/cellTypeData/cell_types_img4_2/cell_types_13465_label.csv",
"../../trainingData/cellTypeData/cell_types_img3_1/cell_types_13465_label.csv", 
"../../trainingData/cellTypeData/cell_types_img1_123/cell_types_13465_label.csv"]

# Prepare the image for the LSTM
for i in range(len(modelName)):
  
  # prepare images
  image = imageLoader(imageFileName[i])
  imageIndex = image.shape[0]
  randIndex = np.random.permutation(range(imageIndex))
  imageFeature = image[randIndex,...]

  # Prepare the labels
  labels = pd.read_csv(labelFileName[i], index_col=0)
  labels = labels.iloc[:,0]
  labels = np.asarray(labels)
  labels = labels[randIndex] - 1

  uniqueLabels = np.unique(labels, return_counts = True)

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
      strides = 2,
      padding = 'valid'
  ))
  model.add(
    Conv2D(64, (3, 3), 
    activation='relu', 
    activity_regularizer=l1(0.0001)
  ))
  model.add(MaxPooling2D(pool_size=(2, 2)))
  model.add(Dropout(0.4))
  model.add(Flatten())
  model.add(Dense(128,activity_regularizer=l1(0.0001)))
  model.add(Activation('relu'))
  model.add(Dropout(0.4))
  model.add(BatchNormalization())
  model.add(Dense(len(uniqueLabels[0]), activation = 'softmax'))

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
  py.main.plot_train_history(history, modelName[i])     


  model.save('./models/'+modelName[i]+'.h5')

  # #probability_model = tf.keras.Sequential([model, tf.keras.layers.Softmax()])

  # testImgProbs = model.predict(x_test)
  # # Plot the first X test images, their predicted labels, and the true labels.
  # # Color correct predictions in blue and incorrect predictions in red.
  # num_rows = 20
  # num_cols = 3
  # num_images = num_rows*num_cols
  # plt.figure(figsize=(2*2*num_cols, 2*num_rows))
  # for i in range(num_images):
  #   plt.subplot(num_rows, 2*num_cols, 2*i+1)
  #   py.main.plot_image(i, testImgProbs[i], y_test, x_test)
  #   plt.subplot(num_rows, 2*num_cols, 2*i+2)
  #   py.main.plot_value_array(i, testImgProbs[i], y_test)
  # plt.tight_layout()
  # plt.show()
