import numpy as np
from scipy import stats
import matplotlib.pyplot as plt 

# Function to load up and unzip the flat image!\
def imageLoader(fileName):
    fileName = fileName
    dimensions = fileName.split('/')
    dimensions.reverse()
    dimensions = dimensions[0].split('_')[:4]
    dimensions = [int(i) for i in dimensions] 
    images = np.load(fileName)
    images = images.reshape(dimensions, order = 'F')
    return images

# Input to this function is
# 1: traces to calculate features
# 2: Size of Steps between the times
def featureMaker(traces, steps = 10):
    # First initialize the empty data frame to fill up with the new 
    samples = traces.shape[0]
    timesteps = int(traces.shape[1] / steps)
    features = 4
    featureFrame = np.empty([samples, timesteps, features])


    rangeToCalc = np.arange(0, traces.shape[1]+1, steps)

    for i in range(len(rangeToCalc)-1):
        meanFeat = traces[:,rangeToCalc[i]:rangeToCalc[i+1]].mean(axis=1)
        stdFeat = traces[:,rangeToCalc[i]:rangeToCalc[i+1]].std(axis=1)
        semFeat = stats.sem(traces[:,rangeToCalc[i]:rangeToCalc[i+1]], axis=1)
        derivFeat = np.mean(np.gradient(traces[:,rangeToCalc[i]:rangeToCalc[i+1]], axis=1), axis=1)

        featureFrame[:, i, 0] = meanFeat
        featureFrame[:, i, 1] = stdFeat
        featureFrame[:, i, 2] = semFeat
        featureFrame[:, i, 3] = derivFeat
    
    return featureFrame

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
  axs[0].legend()

  trainAcc = history.history['acc']
  valAcc = history.history['val_acc']

  axs[1].plot(epochs, trainAcc, 'b', label='Training Accuracy')
  axs[1].plot(epochs, valAcc, 'r', label='Validation Accuracy')
  axs[1].set_title('Accuracy')
  axs[1].legend()

  fig.show()

  #fig.savefig(title+'.png', bbox_inches='tight')
