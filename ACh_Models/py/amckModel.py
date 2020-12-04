import pandas as pd
import numpy as np
from python_pharmer import featureMaker2
import tensorflow as tf
from tensorflow.keras.models import Sequential
from tensorflow.keras.layers import LSTM, Dense, Conv1D, MaxPooling1D, Flatten

import numpy as np
from scipy import stats
import matplotlib.pyplot as plt 


print("Num GPUs Available: ", len(tf.config.experimental.list_physical_devices('GPU')))

# Function to plot the Loss and Histories
def plot_train_history(history, title):
  loss = history.history['loss']
  val_loss = history.history['val_loss']
  epochs = range(len(loss))
  
  #Loss Plotter
  fig, axs = plt.subplots(2)
  fig.suptitle(title)
  axs[0].plot(epochs, loss, 'b', label='Training loss')
  axs[0].plot(epochs, val_loss, 'r', label='Validation loss')
  axs[0].set_title('Loss')
  axs[0].xaxis.set_visible(False)
  axs[0].legend()

  trainAcc = history.history['acc']
  valAcc = history.history['val_acc']

  axs[1].plot(epochs, trainAcc, 'b', label='Training Accuracy')
  axs[1].plot(epochs, valAcc, 'r', label='Validation Accuracy')
  axs[1].set_title('Accuracy')
  axs[1].legend()

  fig.show()
  figName = './' + title + '.png'
  print(figName)
  fig.savefig(figName, bbox_inches='tight')

def plot_value_array(i, predictions_array, true_label):
  predictions_array, true_label = predictions_array, true_label[i]
  plt.grid(False)
  plt.xticks(range(len(predictions_array)))
  plt.yticks([])
  thisplot = plt.bar(range(len(predictions_array)), predictions_array, color="#777777")
  plt.ylim([0, 1])
  predicted_label = np.argmax(predictions_array)

  thisplot[predicted_label].set_color('red')
  thisplot[true_label].set_color('blue')

def plot_image(i, predictions_array, true_label, img):
  predictions_array, true_label, img = predictions_array, true_label[i], img[i]
  plt.grid(False)
  plt.xticks([])
  plt.yticks([])

  plt.imshow(img.reshape(img.shape[0:2]))

  predicted_label = np.argmax(predictions_array)
  if predicted_label == true_label:
    color = 'blue'
  else:
    color = 'red'

  plt.xlabel("{} {:2.0f}% ({})".format(predicted_label,
                                100*np.max(predictions_array),
                                true_label),
                                color=color)

def plot_trace(i, predictions_array, true_label, trace):
  predictions_array, true_label, trace = predictions_array, true_label[i], trace[i]
  plt.grid(False)
  plt.xticks([])
  plt.yticks([])

  plt.plot(np.arange(len(trace)), trace )

  predicted_label = np.argmax(predictions_array)
  if predicted_label == true_label:
    color = 'blue'
  else:
    color = 'red'

  plt.xlabel("{} {:2.0f}% ({})".format(predicted_label,
                                100*np.max(predictions_array),
                                true_label),
                                color=color)


def valMaker(dat):
        # Create Train and Validation Set
        valSize = int(np.ceil(dat['features'].shape[0]*.3))
        trainSize = dat['features'].shape[0] - valSize 

        val = {
                'x_train' : dat['features'][:trainSize,...],
                'y_train' : dat['labels'][:trainSize],
                'x_test' : dat['features'][trainSize:,...],
                'y_test' : dat['labels'][trainSize:]
        }
        
        return val


# Function to import the labeled data, It preshuffles it
# the output of this function is a dictionary with 'features' and 'labels'
# overSample, if true the smaller label will be doubled.
# underSample, if true the larger label will be removed to be equal to the smaller label
def labeledDataMaker(folder, shuffle = True, featureWindows = 12):
        # create the foldernames
        traceFileName = folder + '/features.csv'
        labelFileName = folder + '/labels.csv'

        # loading the features
        features = np.asarray(pd.read_csv(traceFileName, index_col=0))
        if(featureWindows > 0 ):
                features = featureMaker2(features, featureWindows)
        
        # load the labels
        labels = pd.read_csv(labelFileName, index_col=0)
        labels = labels.iloc[:,0].astype('category')
        labels = np.asarray(labels)

        labeledData = {
                "features" : features,
                "labels" : labels
        }
        
        # shuffle it up
        randIndex = np.random.permutation(labeledData['features'].shape[0])
        labeledData['features'] = labeledData['features'][randIndex,...]
        labeledData['labels'] = labeledData['labels'][randIndex,...]

        summ = np.unique(labeledData['labels'], return_counts=True)

        print("The data is size ", labeledData['features'].shape[0])
        print(
                summ[0][0] , 
                ":", 
                round(summ[1][0]/labeledData['features'].shape[0]*100, 0), 
                "% :",  
                summ[1][0]
        )

        print(
                summ[0][1] , 
                ":", 
                round(summ[1][1]/labeledData['features'].shape[0]*100, 0),
                "% :",
                summ[1][1]
        )

        return labeledData

# Function to under or over sample the data. The input is a labeledData dictionary and adds to it.
# overSample, if > 0 smaller label will be duplicated that percentage
# underSample, if true the larger label will be removed the percentage
def sampler(labeledData, overSample = 1, underSample = .2):
        summ = np.unique(labeledData['labels'], return_counts=True)

        if any((overSample > 0, underSample > 0)):
                minLabLoc = np.where(summ[1] == np.min(summ[1]))[0][0]
                majorLabLoc = np.where(summ[1] == np.max(summ[1]))[0][0]

                sampleVals = (overSample, underSample)
                sampNames = ('overSample', 'underSample')

                for i in range(0,len(sampleVals)):
                        if sampleVals[i] > 0:
                                lab = summ[0][minLabLoc] 
                                labLogic = labeledData['labels'] == lab
                                
                                smallLabData = {
                                        'features' : labeledData['features'][labLogic,...],
                                        'labels' : labeledData['labels'][labLogic,...]
                                }
                                
                                # Here we are calculating the amount of data to duplicate
                                toDuplicate = int(smallLabData['features'].shape[0] * sampleVals[i])
                                print('The numbers of values added for', sampNames[i], 'was', toDuplicate)
                                
                                labeledData['features'] = np.concatenate(
                                        (labeledData['features'], labeledData['features'][0:toDuplicate,...]), 
                                0)

                                labeledData['labels'] = np.concatenate(
                                        (labeledData['labels'], labeledData['labels'][0:toDuplicate,...]), 
                                0)


        print("After sampling methods", labeledData['features'].shape[0])
        print(
                summ[0][0] , 
                ":", 
                round(summ[1][0]/labeledData['features'].shape[0]*100, 0), 
                "% :",  
                summ[1][0]
        )

        print(
                summ[0][1] , 
                ":", 
                round(summ[1][1]/labeledData['features'].shape[0]*100, 0),
                "% :",
                summ[1][1]
        )

        # shuffle it up
        randIndex = np.random.permutation(labeledData['features'].shape[0])
        labeledData['features'] = labeledData['features'][randIndex,...]
        labeledData['labels'] = labeledData['labels'][randIndex,...]

        return labeledData

# Function to split the labled data into train and test
# output is a dictionary of two labeledData dictionaries. 
def valSplitter(labeledData, split = 0.1):
        randIndex = np.random.permutation(labeledData['features'].shape[0])
        labeledData['features'] = labeledData['features'][randIndex,...]
        labeledData['labels'] = labeledData['labels'][randIndex,...]

        #create the testData
        testSize = int(labeledData['labels'].shape[0] * split)

        test = {
                'features' : labeledData['features'][0:testSize,...],
                'labels' : labeledData['labels'][0:testSize,...]
        }

        #create the trainData
        train = {
                'features' : labeledData['features'][ (testSize+1) : ,...],
                'labels' : labeledData['labels'][ (testSize+1) : ,...]
        }

        splitData = {
                'train': train,
                'test':test
        }

        return splitData

# here i load the data from the data folder
labeledData = labeledDataMaker("../data/200917.31.f.p1 dx332 k  dose response")

kdrData = labeledDataMaker("../data/200917.31.f.p1 dx332 k  dose response")
achData = labeledDataMaker("../data/achResponses")

# Now i need to create a validation set 

kdrVal = valMaker(kdrData)
achVal = valMaker(achData)


kdrVal['x_train']
x_train = np.concatenate((kdrVal['x_train'], achVal['x_train']), axis=0)
y_train = np.concatenate((kdrVal['y_train'], achVal['y_train']), axis=0)

x_test = np.concatenate((kdrVal['x_test'], achVal['x_test']), axis=0)
y_test = np.concatenate((kdrVal['y_test'], achVal['y_test']), axis=0)




# Now DO what we need for the import to LSTM
BATCH_SIZE = 200
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
        input_shape = x_train.shape[1:]
        ))
model.add(Dense(LSTMOUTPUT, activation='softmax'))

model.compile(optimizer='adam',
        loss='sparse_categorical_crossentropy',
        metrics=['acc'])

model.summary()

EVALUATION_INTERVAL = 200
EPOCHS = 200
#es_callback = tf.keras.callbacks.EarlyStopping(monitor='val_loss', patience=10)

history = model.fit(train, epochs = EPOCHS, 
                steps_per_epoch=EVALUATION_INTERVAL,
                validation_data = test,
                validation_steps=50)
                #callbacks=[es_callback])


print("This is the loss vs Accuracy for" + '.')
plot_train_history(history, modelName)     

model.save('./'+ modelName + '.h5')
