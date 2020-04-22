import numpy as np

def imageLoader(fileName):
    fileName = fileName
    dimensions = fileName.split('/')
    dimensions.reverse()
    dimensions = dimensions[0].split('_')[:4]
    dimensions = [int(i) for i in dimensions] 
    images = np.load(fileName)
    images = images.reshape(dimensions, order = 'F')
    return images
